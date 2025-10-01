(ns user.dummy
  (:require [malli.core :as m]
            [malli.instrument :as mi]
            [malli.util :as mu]
            [malli.dev.pretty :as pretty]
            [keytone.core :as k]))

;; dummy data

(defn validate! [x schema]
  (when-not (m/validate x schema)
    (throw (ex-info "Schema validation failed"
                    {:reason (pretty/explain x schema)}))))

;; (defn broken-validate! [schema x]
;;   (when-not (m/validate schema x)
;;     (throw (ex-info (pretty/format-explain (m/explain schema x))
;;                     {:value x :schema schema}))))

(def inst {:meta {:name "dummy/inst"}
           :description "Dummy instrument"
           :header {:category "cat"
                    :sub-category "subcat"
                    :id "id"
                    :name "name"}})
(validate! k/InstrumentSpec inst)

(def tones1
  "Dummy tones, deliberately out of order."
  [["name" "id" "subcat" "cat"]
   ["piano1" "001" "piano" "keyboard"]
   ["piano2" "002" "" ""]
   ["harpsichord" "003" "mallet" ""]
   ["clav" "004" "" ""]
   ["trumpet" "010" "brass" "woodwind"]
   ["horn" "011" "" ""]
   ["piccolo" "020" "piccolo" ""]
   ])

(def layout1 {:meta {:name "dummy/layout1"}
              :unit "pt"
              :page {:width 800
                     :height 470
                     :margin 4}
              :grid {
                     :rows 27
                     :cols 6
                     :row-gutter 10
                     :column-gutter-large 18
                     :column-gutter-small 6}})
(validate! k/LayoutSpec layout1)

(def style1 {:meta {:name "dummy/style1"}
             :text {
                    :font "Roboto"
                    :size 9
                    :unit "pt"}
             :fill ["#FFB3BA" ; soft pink
                    "#BAE1FF" ; baby blue
                    "#BAFFC9" ; mint green
                    ]
             :stroke "FF0000"})
(validate! k/StyleSpec style1)
