#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.java.shell :refer [sh]]
         '[babashka.process :refer [process]]
         '[clojure.java.io :as io])

(defn get-duration-ms
  "Queries ffprobe to get the exact duration of a wav file in milliseconds."
  [filename]
  (let [{:keys [exit out err]} (sh "ffprobe" "-v" "error"
                                   "-show_entries" "format=duration"
                                   "-of" "default=noprint_wrappers=1:nokey=1"
                                   filename)]
    (if (zero? exit)
      (-> out str/trim Double/parseDouble (* 1000) Math/round)
      (do
        (binding [*out* *err*]
          (println "Error reading duration for:" filename err))
        (System/exit 1)))))

(defn id-telemetry?
  "Returns true when a parsed log line is an ID telemetry event."
  [event telemetry-type]
  (and (= event "TELEMETRY")
       (str/starts-with? (or telemetry-type "") "ID(")))

(defn parse-cw-id-timestamps
  "Parses an AllStar log file to find TXKEY timestamps of CW ID-only
  transmissions (MAIN transmissions with no active RX and only ID
  telemetry between TXKEY and TXUNKEY)."
  [log-file]
  (with-open [rdr (io/reader log-file)]
    (let [id-only (atom #{})
          active-rx (atom #{})
          curr (atom nil)]
      (doseq [line (line-seq rdr)]
        (let [trimmed (str/trim line)]
          (when-not (str/blank? trimmed)
            (let [[ts event arg1 arg2] (str/split trimmed #",")]
              (case event
                "STARTUP"
                (do
                  (reset! active-rx #{})
                  (reset! curr nil))

                "RXKEY"
                (do
                  (swap! active-rx conj arg1)
                  (when @curr
                    (swap! curr assoc :has-other true)))

                "RXUNKEY"
                (do
                  (swap! active-rx disj arg1)
                  (when @curr
                    (swap! curr assoc :has-other true)))

                "TXKEY"
                (if (= arg1 "MAIN")
                  (reset! curr {:tx ts
                                :has-id false
                                :has-other (boolean (seq @active-rx))})
                  (when @curr
                    (swap! curr assoc :has-other true)))

                "TXUNKEY"
                (if (= arg1 "MAIN")
                  (when-let [{:keys [tx has-id has-other]} @curr]
                    (when (and has-id (not has-other))
                      (swap! id-only conj tx))
                    (reset! curr nil))
                  (when @curr
                    (swap! curr assoc :has-other true)))

                (when @curr
                  (if (id-telemetry? event arg2)
                    (swap! curr assoc :has-id true)
                    (swap! curr assoc :has-other true))))))))
      @id-only)))

(defn find-log-file
  "Finds the matching <YYYYMMDD>.txt log file in the same directory as the
  audio file."
  [audio-file]
  (let [f (io/file audio-file)
        name (.getName f)
        parent (.getParentFile f)]
    (when (>= (count name) 8)
      (let [day-prefix (subs name 0 8)
            candidate (if parent
                        (io/file parent (str day-prefix ".txt"))
                        (io/file (str day-prefix ".txt")))]
        (when (.exists candidate)
          candidate)))))

(defn filter-cw-id-files
  "Filters out wav files that correspond to CW ID-only transmissions."
  [files]
  (let [log-cache (atom {})
        get-ids (fn [log-f]
                  (let [p (.getAbsolutePath log-f)]
                    (if-let [ids (get @log-cache p)]
                      ids
                      (let [parsed (parse-cw-id-timestamps log-f)]
                        (swap! log-cache assoc p parsed)
                        parsed))))
        cw-id? (fn [file-path]
                 (let [base (-> (io/file file-path)
                                .getName
                                (str/replace #"(?i)\.wav$" ""))]
                   (if-let [log-f (find-log-file file-path)]
                     (contains? (get-ids log-f) base)
                     false)))]
    (remove cw-id? files)))

(defn filename->iso-title
  "Parses a filename into an ISO datetime formatted string if possible,
  otherwise returns the base filename."
  [filename]
  (let [base (-> (io/file filename)
                 .getName
                 (str/replace #"(?i)\.wav$" ""))]
    (if-let [[_ y m d h min s ms]
             (re-matches #"^(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})(\d*)$"
                         base)]
      (if (seq ms)
        (str y "-" m "-" d "T" h ":" min ":" s "." ms)
        (str y "-" m "-" d "T" h ":" min ":" s))
      base)))

(defn generate-metadata-str [files]
  (let [file-durations (pmap (fn [f]
                               {:filename f
                                :duration-ms (get-duration-ms f)})
                             files)
        initial-state {:current-time 0 :chapters []}
        final-state (reduce
                     (fn [{:keys [current-time chapters]} {:keys [filename
                                                                  duration-ms]}]
                       (let [end-time (+ current-time duration-ms)
                             title (filename->iso-title filename)]
                         {:current-time end-time
                          :chapters (conj chapters
                                          (str "[CHAPTER]\n"
                                               "TIMEBASE=1/1000\n"
                                               "START=" current-time "\n"
                                               "END=" end-time "\n"
                                               "title=" title "\n"))}))
                     initial-state
                     file-durations)]
    (str ";FFMETADATA1\n\n" (str/join "\n" (:chapters final-state)))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [args *command-line-args*]
    (when (empty? args)
      (println "Usage: bb listen-allstar.bb <input1.wav> <input2.wav> ...")
      (System/exit 1))

    (let [filtered-args (filter-cw-id-files args)
          skipped-count (- (count args) (count filtered-args))]
      (when (pos? skipped-count)
        (println (str "Filtered out " skipped-count " CW ID file(s). "
                      (count filtered-args) " file(s) remaining.")))
      (when (empty? filtered-args)
        (println "No files remaining after filtering.")
        (System/exit 0))

      (let [inputs-file (io/file "ffmpeg_inputs.txt")
            metadata-file (io/file "ffmpeg_metadata.txt")
            output-file "output.m4a"]

        (println "Generating chapter metadata map...")
        (spit metadata-file (generate-metadata-str filtered-args))

        (println "Creating input manifest...")
        (let [manifest-lines (map (fn [f]
                                    (str "file '"
                                         (.getAbsolutePath (io/file f))
                                         "'"))
                                  filtered-args)]
          (spit inputs-file (str/join "\n" manifest-lines)))

        (println "Running FFmpeg concatenation and encoding...")
        (let [{:keys [exit err]} (sh "ffmpeg" "-y"
                                     "-threads" "0"
                                     "-f" "concat" "-safe" "0"
                                     "-i" (str inputs-file)
                                     "-i" (str metadata-file)
                                     "-map_metadata" "1"
                                     "-vn"
                                     "-c:a" "aac" "-b:a" "128k"
                                     output-file)]
          ;; Clean up temporary manifest files
          (.delete inputs-file)
          (.delete metadata-file)

          (when (not (zero? exit))
            (binding [*out* *err*]
              (println "FFmpeg failed:\n" err))
            (System/exit 1))

          (println "Success! Output generated at:" output-file))

        (println "
Playing with mpv...

Space to pause.
PgUp/PgDn to skip chapters.
Left/Right skip 5 seconds.
Up/Down skip 60 seconds.
? for help in mpv.

")

        @(process
          ["mpv"
           (str "--term-status-msg=Chapter: ${chapter-metadata/title} | "
                "Time: ${time-pos} / ${playtime-remaining}")
           "output.m4a"]
          {:inherit true})))))

