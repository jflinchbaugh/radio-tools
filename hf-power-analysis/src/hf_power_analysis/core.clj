(ns hf-power-analysis.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-adi-qso
  "Parse a single raw ADIF QSO record string into a Clojure map."
  [s]
  (-> s
      (str/split #"[<]")
      (->>
       (map str/trim)
       (remove empty?)
       (map (fn [p] (str/split p #":\d+>")))
       (filter (fn [x] (= 2 (count x))))
       (map (fn [[k v]]
              {(keyword (str/replace k #"_" "-")) v}))
       (into {}))))

(defn qso?
  "Predicate checking if record contains a callsign."
  [s]
  (re-matches #"(?m).*<call.*" s))

(def end-pattern (re-pattern #"<eo[hr]>"))

(defn parse-deg
  "Convert ADIF lat/lon string (e.g. 'N040 03.700' or 'W076 22.600') to
  signed decimal degrees."
  [s]
  (when-let [[_ hem deg min] (and s (re-matches #"([NSEW])(\d{3})\s+(\d{2}\.\d+)" s))]
    (let [val (+ (Double/parseDouble deg) (/ (Double/parseDouble min) 60.0))]
      (if (#{"S" "W"} hem) (- val) val))))

(defn bogus-reason
  "Returns a string explaining why a QSO is bogus or corrupted,
  or nil if the QSO appears valid."
  [q]
  (let [pwr-raw (:tx-pwr q)
        dist-raw (:distance q)
        pwr (try (parse-double pwr-raw) (catch Exception _ nil))
        dist (try (parse-double dist-raw) (catch Exception _ nil))
        lat (parse-deg (:lat q))]
    (cond
      (or (nil? pwr-raw) (str/blank? pwr-raw))
      "Missing transmit power"

      (nil? pwr)
      "Non-numeric transmit power"

      (<= pwr 0.0)
      "Non-positive transmit power"

      (> pwr 1500.0)
      "Transmit power exceeds legal amateur limit (1500W)"

      (or (nil? dist-raw) (str/blank? dist-raw))
      "Missing distance"

      (nil? dist)
      "Non-numeric distance"

      (<= dist 0.0)
      "Non-positive distance (Null Island / uncalculated)"

      (> dist 20015.0)
      "Distance exceeds Earth antipodal limit (20,015 km)"

      (and lat (< lat -80.0))
      "Corrupted polar coordinate artifact (lat < -80)"

      :else nil)))

(defn bogus-qso?
  "Predicate checking if a QSO record is bogus or corrupted."
  [q]
  (boolean (bogus-reason q)))
