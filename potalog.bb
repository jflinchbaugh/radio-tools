#!/usr/bin/env bb

(ns pota-log
  (:require [clojure.string :as str]))

(def log-file "/home/john/.local/share/WSJT-X - pota/wsjtx_log.adi")

(defn parse-adi-qso [s]
  (->
    s
    (str/split #"[<]")
    (->>
      (map str/trim)
      (remove empty?)
      (map (fn [p]
             (-> p
               (str/split #":\d+>"))))
      (filter #(= 2 (count %)))
      (map
        (fn [[k v]]
          {(keyword (str/replace k #"_" "-")) v}))
      (into {}))))

(defn qso? [s]
  (re-matches #"<call.*" s))

(let [qsos (->>
             log-file
             slurp
             str/split-lines
             (filter qso?)
             (map parse-adi-qso)
             )]
  (println "File: " log-file)
  (println qsos)

  (->>
    qsos
    (map (juxt :call :band :mode))
    sort
    (map cons (drop 1 (range)))
    (map (partial str/join "\t"))
    (map println )
    doall))

