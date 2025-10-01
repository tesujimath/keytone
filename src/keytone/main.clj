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
    (let [raw-tones (kf/read-tones-csv (k/spec-name instrument))
          [cats subcats tones] (k/get-tones instrument raw-tones)
          grid (layout :grid)
          columns (k/partition-into-columns (grid :rows) cats subcats tones)
          pages (k/partition-columns-into-pages (grid :rows) (grid :cols) columns)
          formatted-pages (map #(k/format-page layout style %) pages)]
      (kf/create-cheatsheet (k/spec-name instrument)
                            layout
                            style
                            formatted-pages))))
