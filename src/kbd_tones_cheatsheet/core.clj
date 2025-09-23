(ns kbd-tones-cheatsheet.core
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml])
  (:require [clojure.java.io :as io])
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
  (let [sheet (yaml/from-file sheet-file)
        name (base-name (.getName sheet-file))
        out-path (->> (io/file out-dir (str name ".pdf"))
                      .getPath)]
    (assoc sheet :out-path out-path)
    ))

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
