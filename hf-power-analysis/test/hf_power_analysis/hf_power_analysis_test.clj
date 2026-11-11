(ns hf-power-analysis.hf-power-analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [hf-power-analysis.core :as core]))

(deftest bogus-qso-test
  (testing "valid QSO passes validation"
    (let [q {:tx-pwr "2" :distance "784" :lat "N033 55.535" :lon "W080 44.086"}]
      (is (nil? (core/bogus-reason q)))
      (is (false? (core/bogus-qso? q)))))

  (testing "missing power is identified"
    (let [q {:distance "1335"}]
      (is (= "Missing transmit power" (core/bogus-reason q)))
      (is (true? (core/bogus-qso? q)))))

  (testing "non-positive distance is identified"
    (let [q {:tx-pwr "2" :distance "0"}]
      (is (= "Non-positive distance (Null Island / uncalculated)"
             (core/bogus-reason q)))))

  (testing "corrupted polar coordinates are identified"
    (let [q {:tx-pwr "2" :distance "14464" :lat "S089 58.750" :lon "W179 57.500"}]
      (is (= "Corrupted polar coordinate artifact (lat < -80)"
             (core/bogus-reason q)))
      (is (true? (core/bogus-qso? q))))))
