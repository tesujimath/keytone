(ns keytone.files
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [keytone.core :as k])
  (:gen-class))

(defn get-instrument-specs []
  "Get all instrument specs from filesystem."
  (map k/get-instrument-spec (k/yaml-files (io/file "resources/instruments"))))

(defn get-layout-specs []
  "Get all layout specs from filesystem."
  (map k/get-layout-spec (k/yaml-files (io/file "resources/layouts"))))

(defn get-style-specs []
  "Get all style specs from filesystem."
  (map k/get-style-spec (k/yaml-files (io/file "resources/styles"))))

(defn build-path [ext & components]
  "Return path as a File in build dir comprising components with extension"
  {:malli/schema [:=> [:cat :string] k/File]}
  (io/file "build" (format "%s%s" (str/join "." components) ext)))
