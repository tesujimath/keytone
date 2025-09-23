(ns kbd-tones-cheatsheet.core
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:gen-class))

(defn yaml-files [dir-path]
  "Return all YAML files in directory."
  (->> (.listFiles (io/file dir-path))
       (filter #(.isFile %))
       (filter #(-> % .getName (.endsWith ".yaml")))))

(defn base-name [file-name]
  (let [last-dot (.lastIndexOf file-name ".")]
    (if (neg? last-dot)
      file-name
      (subs file-name 0 last-dot))
    ))

(defn get-sheet-spec [sheet-file out-dir]
  "Get the sheet spec, with relative paths adjusted for its location, and out-path inserted."
  (let [sheet-dir (.getParent sheet-file)
        sheet-basename (base-name (.getName sheet-file))
        sheet (yaml/from-file sheet-file)
        adjusted-sheet (update-in sheet [:tones :path] #(->> % (io/file sheet-dir) .getPath))
        ]
    (assoc adjusted-sheet :out-path (->> (io/file out-dir (str sheet-basename ".pdf"))
                                         .getPath))
    ))

(defn get-tones [{:keys [path header]}]
  "Return the tones as a vector of maps, with blank entries in the CSV propagated from last non-blank value in that column."
  (with-open [reader (io/reader path)]
    (let [rows (csv/read-csv reader)
          ]
      (mapv identity rows)
      )))

(defn create-cheatsheet [spec]
  )

(defn -main
  "Create PDF cheatsheet for each sheet descriptor."
  [& args]
  (let [sheets-dir (io/file "sheets")
        out-dir "pdf"
        sheet-specs (map #(get-sheet-spec % out-dir) (yaml-files sheets-dir))]
    (doseq [sheet-spec sheet-specs]
      (println sheet-spec))))
