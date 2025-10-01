(ns keytone.files
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [yaml.core :as yaml]
            [keytone.core :as k])
  (:gen-class))

(defn yaml-files
  "Return all YAML files in directory."
  {:malli/schema [:-> k/File [:seqable k/File]]}
  [dir-path]
  (->> (.listFiles (io/file dir-path))
       (filter #(.isFile %))
       (filter #(-> % .getName (.endsWith ".yaml")))))

(defn base-name
  "Return base name of file without extension."
  [file-name]
  (let [last-dot (.lastIndexOf file-name ".")]
    (if (neg? last-dot)
      file-name
      (subs file-name 0 last-dot))
    ))

(defn get-spec
  "Get the spec as a map from a YAML file, inserting its name and dir as metadata."
  {:malli/schema [:-> k/File k/Spec]}
  [yaml-file]
  (let [name (base-name (.getName yaml-file))
        dir (.getParent yaml-file)
        spec (yaml/from-file yaml-file)
        annotated-spec (assoc spec :meta {:name name})]
    annotated-spec
    ))

(defn get-instrument-spec
  "Read a YAML file as an instrument spec."
  {:malli/schema [:-> k/File k/InstrumentSpec]}
  [yaml-file]
  (get-spec yaml-file))

(defn get-layout-spec
  "Read a YAML file as a layout spec."
  {:malli/schema [:-> k/File k/LayoutSpec]}
  [yaml-file]
  (get-spec yaml-file))

(defn get-style-spec
  "Read a YAML file as a style spec."
  {:malli/schema [:-> k/File k/StyleSpec]}
  [yaml-file]
  (get-spec yaml-file))

(defn get-typed-specs [reader resource-dir]
  "Get all instrument specs from filesystem."
  (map reader (yaml-files (io/file "resources" resource-dir))))

(defn get-instrument-specs []
  "Get all instrument specs from filesystem."
  (get-typed-specs get-instrument-spec "instruments"))

(defn get-layout-specs []
  "Get all layout specs from filesystem."
  (get-typed-specs get-layout-spec "layouts"))

(defn get-style-specs []
  "Get all style specs from filesystem."
  (get-typed-specs get-style-spec "styles"))

(defn read-tones-csv
  "Read the raw tones CSV file returning its rows."
  {:malli/schema [:-> :string [:vector [:vector :string]]]}
  [instrument-name]
  (let [tones-file (io/file "resources/instruments" (str instrument-name ".csv"))]
    (with-open [reader (io/reader tones-file)]
      (vec (csv/read-csv reader)))))

(defn create-cheatsheet
  "Write out cheatsheet to paths specified in `build`,"
  {:malli/schema [:=> [:cat :string k/LayoutSpec k/StyleSpec [:sequential :string]] :nil]}
  [instrument-name layout style formatted-pages]
  (let [out-file (io/file "build" (format "%s.%s.%s.typ"  instrument-name (k/spec-name layout) (k/spec-name style)))]
    (k/ensure-dir (.getParent out-file))
    (with-open [w (io/writer out-file)]
      (k/write-cheatsheet w layout style formatted-pages))))
