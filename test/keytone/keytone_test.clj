(ns keytone.keytone-test
  (:require [clojure.test :refer :all]
            [keytone.keytone :refer :all]))

(deftest create-tone-map-test
  (testing "create-tone-map"
    (let [tone-list [["Piano" "Electric Piano" "001" "80s FM1"]]
          ]
      (is (= (create-tone-map tone-list)
             {:cats ["Piano"], :subcats {"Piano" '([0 "Electric Piano"])}, :map {"Piano" {"Electric Piano" '(["001" "80s FM1"])}}})))))
