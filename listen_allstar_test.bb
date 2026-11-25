#!/usr/bin/env bb

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.test :as t])

(load-file
 (str (io/file (or (.getParentFile (io/file *file*))
                   (io/file "."))
               "listen-allstar.bb")))

(defn parse-log-str
  "Parses an AllStar log string and returns CW ID-only TXKEY timestamps."
  [s]
  (parse-cw-id-timestamps (java.io.StringReader. s)))

(t/deftest pure-cw-id-only-test
  (let [log (str/join
             "\n"
             ["2026112508424364,TXKEY,MAIN"
              "2026112508424414,TELEMETRY,63047,ID(I)"
              "2026112508424710,TXUNKEY,MAIN"])]
    (t/is (= #{"2026112508424364"} (parse-log-str log)))))

(t/deftest voice-rx-before-txkey-with-id-test
  (let [log (str/join
             "\n"
             ["2026112508560390,RXKEY,53085"
              "2026112508560390,TXKEY,MAIN"
              "2026112508560952,RXUNKEY,53085"
              "2026112508560952,RXKEY,53085"
              "2026112508560976,RXUNKEY,53085"
              (str "2026112508561053,TELEMETRY,63047,"
                   "LINKUNKEY(T),(700,1100,150,2048)")
              "2026112508561169,TELEMETRY,63047,ID(I)"
              "2026112508561466,TXUNKEY,MAIN"])]
    (t/is (= #{} (parse-log-str log)))))

(t/deftest short-voice-rx-during-cw-id-test
  (let [log (str/join
             "\n"
             ["2026111711103685,TXKEY,MAIN"
              "2026111711103735,TELEMETRY,63047,ID(I)"
              "2026111711103864,RXKEY,53085"
              "2026111711104064,RXUNKEY,53085"
              (str "2026111711104131,TELEMETRY,63047,"
                   "LINKUNKEY(T),(700,1100,150,2048)")
              "2026111711104197,TXUNKEY,MAIN"])]
    (t/is (= #{} (parse-log-str log)))))

(t/deftest id-with-other-telemetry-or-dtmf-test
  (let [log (str/join
             "\n"
             ["2026011009070371,TXKEY,MAIN"
              "2026011009070400,DTMF(M),MAIN,P"
              "2026011009070400,DTMF,MAIN,P"
              "2026011009070572,TELEMETRY,63047,UNKEY(T),(660,880,150,2048)"
              "2026011009070832,TELEMETRY,63047,LOCALPLAY"
              "2026011009072035,TELEMETRY,63047,ID(I)"
              "2026011009072335,TXUNKEY,MAIN"
              "2026021621552868,TXKEY,MAIN"
              "2026021621552868,LINKDISC,55553,nil"
              "2026021621553068,TELEMETRY,63047,COMPLETE(T)"
              "2026021621553318,TELEMETRY,63047,REMDISC"
              "2026021621553994,TELEMETRY,63047,ID(I)"
              "2026021621554290,TXUNKEY,MAIN"])]
    (t/is (= #{} (parse-log-str log)))))

(t/deftest nested-and-standalone-non-main-txkey-test
  (let [log (str/join
             "\n"
             ["2025112420053170,TXKEY,MAIN"
              "2025112420053172,TXKEY,53085"
              "2025112420053200,TELEMETRY,63047,ID(I)"
              "2025112420053300,TXUNKEY,53085"
              "2025112420053400,TXUNKEY,MAIN"
              "2025112420053564,TXKEY,53085"
              "2025112420053600,TELEMETRY,63047,ID(I)"
              "2025112420053700,TXUNKEY,53085"
              "2025112420060000,TXKEY,MAIN"
              "2025112420060050,TELEMETRY,63047,ID(I)"
              "2025112420060340,TXUNKEY,MAIN"])]
    (t/is (= #{"2025112420060000"} (parse-log-str log)))))

(t/deftest startup-resets-stale-active-rx-test
  (let [log (str/join
             "\n"
             ["2026010515111362,RXKEY,53085"
              "2026010515122750,STARTUP"
              "2026010515122760,FREQ,,FM,100.0,100.0,SIMPLEX,LOW,0,0"
              "2026010515200000,TXKEY,MAIN"
              "2026010515200050,TELEMETRY,63047,ID(I)"
              "2026010515200340,TXUNKEY,MAIN"])]
    (t/is (= #{"2026010515200000"} (parse-log-str log)))))

(t/deftest blank-lines-and-non-id-transmissions-test
  (let [log (str/join
             "\n"
             [""
              "   "
              "2026112508585986,TXKEY,MAIN"
              (str "2026112508590251,TELEMETRY,63047,"
                   "LINKUNKEY(T),(700,1100,150,2048)")
              "2026112508590316,TXUNKEY,MAIN"
              ""
              "2026112508424364,TXKEY,MAIN"
              "2026112508424414,TELEMETRY,63047,ID(I)"
              "2026112508424710,TXUNKEY,MAIN"])]
    (t/is (= #{"2026112508424364"} (parse-log-str log)))))

(t/deftest filter-cw-id-files-end-to-end-test
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "allstar-test-"
                          (make-array
                           java.nio.file.attribute.FileAttribute 0)))]
    (try
      (let [log-file (io/file tmp-dir "20261125.txt")
            id-wav (str (io/file tmp-dir "2026112508424364.WAV"))
            voice-id-wav (str (io/file tmp-dir "2026112508560390.WAV"))
            voice-wav (str (io/file tmp-dir "2026112508585986.WAV"))
            log-content (str/join
                         "\n"
                         ["2026112508424364,TXKEY,MAIN"
                          "2026112508424414,TELEMETRY,63047,ID(I)"
                          "2026112508424710,TXUNKEY,MAIN"
                          "2026112508560390,RXKEY,53085"
                          "2026112508560390,TXKEY,MAIN"
                          "2026112508560952,RXUNKEY,53085"
                          "2026112508561053,TELEMETRY,63047,LINKUNKEY(T)"
                          "2026112508561169,TELEMETRY,63047,ID(I)"
                          "2026112508561466,TXUNKEY,MAIN"
                          "2026112508585986,RXKEY,53085"
                          "2026112508585986,TXKEY,MAIN"
                          "2026112508590150,RXUNKEY,53085"
                          "2026112508590251,TELEMETRY,63047,LINKUNKEY(T)"
                          "2026112508590316,TXUNKEY,MAIN"])]
        (spit log-file log-content)
        (doseq [p [id-wav voice-id-wav voice-wav]]
          (spit p ""))
        (t/is (= [voice-id-wav voice-wav]
                 (vec (filter-cw-id-files [id-wav voice-id-wav voice-wav])))))
      (finally
        (doseq [f (.listFiles tmp-dir)]
          (.delete f))
        (.delete tmp-dir)))))

(let [{:keys [fail error]} (t/run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
