(ns keytone.main
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [keytone.core :as k]
            [keytone.files :as kf]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty])
  (:gen-class))

(defn -main
  "Create PDF cheatsheet for each pair of instrument and layout."
  [& args]
  (mi/collect! {:ns ['keytone.core 'keytone.files]})
  (mi/instrument! {:report (pretty/thrower)})
  (doseq [instrument (kf/get-instrument-specs)
          layout (kf/get-layout-specs)
          style (kf/get-style-specs)]
    (let [[cats subcats tones] (k/get-tones instrument)
          groups (mapcat (fn [cat] (k/split-columns cat (subcats cat) (tones cat) (get-in layout [:grid :rows]))) cats)
          pages (map vec (partition (get-in layout [:grid :cols]) groups))
          formatted-pages (map #(k/format-page layout style %) pages)]
      (k/create-cheatsheet (kf/build-path ".typ" (k/spec-name instrument) (k/spec-name layout) (k/spec-name style))
                           layout
                           style
                           formatted-pages))))
