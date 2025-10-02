(ns keytone.keytone-test
  (:require [clojure.test :refer :all]
            [keytone.core :refer :all]))

(deftest get-tones-test
  (testing "get-tones"
    (let [inst {:meta {:name "test/inst"}
                :description "Test instrument"
                :header {:category "cat"
                         :sub-category "subcat"
                         :id "id"
                         :name "name"}}
          raw-tones [["cat" "subcat" "id" "name"]
                     ["Piano" "Electric Piano" "001" "80s FM1"]
                     ["" "" "002" "80s FM2"]
                     ["Other" "Sound FX" "901" "Thunder"]
                     ["Other" "" "902" "Motorbike"]
                     ["Other" "Drums" "910" "Jazz kit"]]
          ;; invoke test function
          [cats subcats tones] (get-tones inst raw-tones)]
      (is (= cats ["Piano" "Other"]))
      (is (= subcats {"Piano" '([0 "Electric Piano"])
                      "Other" '([1 "Sound FX"] [2 "Drums"])}))
      (is (= tones {"Piano" {"Electric Piano" [["001" "80s FM1"] ["002" "80s FM2"]]}
                    "Other" {"Sound FX" [["901" "Thunder"] ["902" "Motorbike"]]
                             "Drums" [["910" "Jazz kit"]]}})))))
