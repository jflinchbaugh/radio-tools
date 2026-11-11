(ns index
  (:require [hf-power-analysis.core :as hf]
            [clojure.string :as str]
            [scicloj.plotje.api :as pj]
            [scicloj.kindly.v4.kind :as kind]))

;; # HF QRP Power & Propagation Analysis: Doing More With Less
;;
;; Exploration and power analysis of amateur radio QSOs from log `kd3ee`.
;; Demonstrates how extreme low power (milliwatts up to 5W QRP) yields
;; extraordinary distance (DX) across global HF propagation paths, taking into
;; account transmit power, seasonal variations (month of year), and diurnal
;; cycles (time of day / UTC hour).

(def adi-file "kd3ee.439943.20261021142140.adi")

(def month-labels
  {"01" "01-Jan" "02" "02-Feb" "03" "03-Mar"
   "04" "04-Apr" "05" "05-May" "06" "06-Jun"
   "07" "07-Jul" "08" "08-Aug" "09" "09-Sep"
   "10" "10-Oct" "11" "11-Nov" "12" "12-Dec"})

(defn time-block
  "Group UTC hours into diurnal propagation groups."
  [h]
  (cond
    (nil? h) nil
    (<= 0 h 5) "00-05 UTC (Night)"
    (<= 6 h 11) "06-11 UTC (Dawn)"
    (<= 12 h 17) "12-17 UTC (Day)"
    :else "18-23 UTC (Evening/Grayline)"))

(def raw-qsos
  (->> adi-file
       slurp
       str/split-lines
       (map str/trim)
       (partition-by (partial re-matches hf/end-pattern))
       (remove (comp (partial re-matches hf/end-pattern) first))
       (map str/join)
       (filter hf/qso?)
       (map hf/parse-adi-qso)))

;; ## Data Quality: Dropped Bogus & Corrupted Records
;;
;; Raw log imports can contain corruption (such as software GPS glitches,
;; uncalculated distances, or missing transmit power fields).
;; Below is the review summary and sample table of records flagged and excluded.

(def classified-qsos
  (map (fn [q]
         (assoc q :bogus-reason (hf/bogus-reason q)))
       raw-qsos))

(def dropped-qsos
  (filter :bogus-reason classified-qsos))

;; ### Summary of Dropped Records by Reason

(kind/table
 (->> dropped-qsos
      (group-by :bogus-reason)
      (map (fn [[reason group]]
             {:reason reason
              :count (count group)
              :modes (str/join ", " (sort (distinct (keep :mode group))))
              :bands (str/join ", " (sort (distinct (keep :band group))))}))
      (sort-by :count)
      reverse))

;; ### Samples of Dropped Records Grouped by Reason

(kind/fragment
 (for [[reason group] (sort-by (comp count val) > (group-by :bogus-reason dropped-qsos))]
   (kind/fragment
    [(kind/md (str "#### " reason " (" (count group) " total)"))
     (kind/table
      (->> group
           (take 5)
           (map (fn [q]
                  {:call (or (:call q) "-")
                   :country (or (:country q) "-")
                   :state (or (:state q) "-")
                   :band (or (:band q) "-")
                   :mode (or (:mode q) "-")
                   :tx-pwr (or (:tx-pwr q) "<missing>")
                   :distance (or (:distance q) "<missing>")
                   :rst-rcvd (or (:rst-rcvd q) "-")
                   :lat (or (:lat q) "-")
                   :lon (or (:lon q) "-")}))))])))

;; ## Validated Clean QSOs
;;
;; Remaining records with verified positive power, physical distance,
;; and sound coordinates.

(def qsos
  (->> classified-qsos
       (remove :bogus-reason)
       (map (fn [q]
              (let [pwr (parse-double (:tx-pwr q))
                    dist (parse-double (:distance q))
                    rst (try (parse-double (:rst-rcvd q))
                             (catch Exception _ nil))
                    d-str (:qso-date q)
                    t-str (:time-on q)
                    m-code (when (and d-str (>= (count d-str) 6))
                             (subs d-str 4 6))
                    hour (when (and t-str (>= (count t-str) 2))
                           (try (Integer/parseInt (subs t-str 0 2))
                                (catch Exception _ nil)))]
                (assoc q
                       :tx-pwr pwr
                       :distance dist
                       :rst-rcvd-num rst
                       :month (month-labels m-code)
                       :utc-hour hour
                       :time-block (time-block hour)
                       :km-per-watt (/ dist pwr)))))))

;; ## QRP Overview
;;
;; Amateur radio QRP operation is traditionally defined as 5 Watts or less.
;; Below is the overall breakdown of transmit power across all verified contacts.

(kind/table
 (->> (group-by :tx-pwr qsos)
      (map (fn [[pwr group]]
             (let [dists (map :distance group)
                   rsts (keep :rst-rcvd-num group)]
               {:tx-power (format "%.1f W" pwr)
                :pwr-num pwr
                :qso-count (count group)
                :max-dist-km (int (apply max dists))
                :mean-dist-km (int (/ (reduce + dists) (count dists)))
                :max-km-per-watt (int (/ (apply max dists) pwr))
                :mean-rst-db (when (seq rsts)
                               (format "%.1f dB"
                                       (double (/ (reduce + rsts)
                                                  (count rsts)))))})))
      (sort-by :pwr-num)
      (map #(dissoc % :pwr-num))))

;; ## Transmit Power vs. Distance Achieved
;;
;; Distribution of distance achieved (in km) grouped by transmit power.
;; Notice that 1W and 2W easily span thousands of kilometers into Europe
;; and across oceans.

(-> (map (fn [q]
           {:power (format "%.1f W" (:tx-pwr q))
            :distance (:distance q)})
         qsos)
    (pj/pose :power :distance)
    (pj/lay-boxplot)
    (pj/options {:title "Contact Distance by Transmit Power"}))

;; ## Efficiency: Kilometers per Watt
;;
;; Kilometers per watt is the gold standard metric for QRP operators.
;; Running ultra-low power yields thousands of km per single watt.

(-> (map (fn [q]
           {:power (format "%.1f W" (:tx-pwr q))
            :km-per-watt (:km-per-watt q)})
         qsos)
    (pj/pose :power :km-per-watt)
    (pj/lay-boxplot)
    (pj/options {:title "Propagation Efficiency (km per Watt)"}))

;; ## Top 15 Most Efficient DX Contacts
;;
;; The top contacts achieving the highest distance per watt ratio.

(kind/table
 (->> qsos
      (sort-by :km-per-watt)
      reverse
      (take 15)
      (map (fn [q]
             {:call (:call q)
              :country (:country q)
              :band (:band q)
              :mode (:mode q)
              :month (:month q)
              :utc-hour (:utc-hour q)
              :tx-power (format "%.1f W" (:tx-pwr q))
              :distance-km (int (:distance q))
              :km-per-watt (int (:km-per-watt q))
              :rst-rcvd (:rst-rcvd q)}))))

;; ## Seasonal Variation: Propagation by Month of Year
;;
;; Ionospheric layers and maximum usable frequencies (MUF) change across
;; solstices and equinoxes. Here we observe monthly contact counts, average power
;; used, and reach.

(kind/table
 (->> (remove (comp nil? :month) qsos)
      (group-by :month)
      (map (fn [[m grp]]
             (let [dists (map :distance grp)
                   pwrs (map :tx-pwr grp)
                   effs (map :km-per-watt grp)]
               {:month m
                :contacts (count grp)
                :avg-power (format "%.1f W" (double (/ (reduce + pwrs)
                                                       (count pwrs))))
                :mean-dist-km (int (/ (reduce + dists) (count dists)))
                :max-dist-km (int (apply max dists))
                :mean-km-per-watt (int (/ (reduce + effs) (count effs)))
                :max-km-per-watt (int (apply max effs))})))
      (sort-by :month)))

;; ## Monthly Distance Distribution
;;
;; Notice the spring and autumn equinox months (e.g., March and September)
;; providing exceptional long-path and trans-equatorial DX openings on tiny power.

(-> (->> qsos
         (remove (comp nil? :month))
         (map (fn [q]
                {:month (:month q)
                 :distance (:distance q)})))
    (pj/pose :month :distance)
    (pj/lay-boxplot)
    (pj/options {:title "Contact Distance by Month"}))

;; ## Diurnal Variation: Time of Day (UTC Hour)
;;
;; Solar radiation charges the ionosphere during daylight and dissipates at night.
;; Contacts grouped by diurnal propagation windows show when DX happens.

(kind/table
 (->> (remove (comp nil? :time-block) qsos)
      (group-by :time-block)
      (map (fn [[blk grp]]
             (let [dists (map :distance grp)
                   pwrs (map :tx-pwr grp)
                   effs (map :km-per-watt grp)]
               {:time-window blk
                :contacts (count grp)
                :avg-power (format "%.1f W" (double (/ (reduce + pwrs)
                                                       (count pwrs))))
                :mean-dist-km (int (/ (reduce + dists) (count dists)))
                :max-dist-km (int (apply max dists))
                :mean-km-per-watt (int (/ (reduce + effs) (count effs)))
                :max-km-per-watt (int (apply max effs))})))
      (sort-by :time-window)))

;; ## Distance vs. UTC Hour
;;
;; Plotting contact distance against the hour of day reveals peak DX activity
;; during late afternoon, grayline, and evening hours (18:00 - 23:00 UTC).

(-> (->> qsos
         (remove (comp nil? :utc-hour))
         (map (fn [q]
                {:hour (:utc-hour q)
                 :distance (:distance q)})))
    (pj/pose :hour :distance)
    (pj/lay-point)
    (pj/options {:title "Contact Distance by UTC Hour"}))

;; ## Signal Reports (SNR) vs. Distance on 1 Watt & 2 Watts
;;
;; Even at only 1 to 2 Watts of RF output, received signal reports (FT8 SNR in dB)
;; remain solid across thousands of kilometers.

(let [low-pwr-qsos (->> qsos
                        (filter (fn [q]
                                  (and (<= (:tx-pwr q) 2.0)
                                       (:rst-rcvd-num q)
                                       (#{"FT8" "FT4"} (:mode q)))))
                        (map (fn [q]
                               {:distance (:distance q)
                                :snr-db (:rst-rcvd-num q)
                                :power (format "%.1f W" (:tx-pwr q))})))]
  (-> low-pwr-qsos
      (pj/pose :distance :snr-db)
      (pj/lay-point)
      (pj/options {:title "Received SNR (dB) vs Distance at <= 2W (Digital)"})))

;; ## QRP Band Performance Summary
;;
;; Breakdown of contacts by HF band under <= 5W QRP operation.

(kind/table
 (->> (filter (fn [q] (<= (:tx-pwr q) 5.0)) qsos)
      (group-by :band)
      (map (fn [[band group]]
             (let [dists (map :distance group)
                   pwrs (map :tx-pwr group)]
               {:band band
                :contacts (count group)
                :min-pwr (format "%.1f W" (apply min pwrs))
                :avg-pwr (format "%.1f W" (double (/ (reduce + pwrs)
                                                     (count pwrs))))
                :max-dist-km (int (apply max dists))
                :mean-dist-km (int (/ (reduce + dists) (count dists)))})))
      (sort-by :contacts)
      reverse))
