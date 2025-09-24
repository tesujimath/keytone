(ns kbd-tones-cheatsheet.core
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str])
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

(defn get-sheet-spec [sheet-file]
  "Get the sheet spec, with relative paths adjusted for its location, and out-path inserted."
  (let [sheet-dir (.getParent sheet-file)
        sheet-basename (base-name (.getName sheet-file))
        spec (yaml/from-file sheet-file)
        ;; adjust the tones path relative to sheet file
        path-adjusted-spec (update-in spec [:tones :path] #(->> % (io/file sheet-dir) .getPath))
        named-spec (assoc path-adjusted-spec :name sheet-basename)]
    named-spec
    ))

(defn build-spec [build-dir sheet-spec]
  (let [name (sheet-spec :name)
        out-path (fn [ext]
                   (->> (io/file build-dir (str name ext)) .getPath))]
    {:typst {:out-path (out-path ".typ")}
     :pdf {:out-path (out-path ".pdf")}
     }))

(defn get-field-indices [field-names header]
  (let [header-indices (into {} (map-indexed (fn [i v] [v i]) header))]
    (map #(get header-indices %) field-names)))


(defn nonblank-or-default [l defaults]
  "Take non-blank items from l, otherwise corresponding defaults."
  (map (fn [x d] (if (empty? x) d x)) l defaults))

(defn create-tone-map [tone-list]
  "Create a tone map from the list, with entries reversed."
  (reduce (fn [m [cat subcat id name]]
            (update-in m [cat subcat] #(conj % [id name])))
          (array-map)
          tone-list))

(defn get-tones [{:keys [path header]}]
  "Return the tones as a vector of maps, with blank entries in the CSV propagated from last non-blank value in that column."
  (with-open [reader (io/reader path)]
    (let [rows (csv/read-csv reader)
          field-names (map #(get header %) [:category :sub-category :id :name])
          blank-fields (map (fn [_] "") field-names)
          field-indices (get-field-indices field-names (first rows))
          field-getters (map #(fn [row] (get row %)) field-indices)
          row-mapper (fn [row] (map #(% row) field-getters))
          mapped-rows (map row-mapper (rest rows))
          defaulted-mapped-rows  (first (reduce (fn [[rows defaults] row]
                                                  (let [merged (nonblank-or-default row defaults)]
                                                    [(conj rows merged) merged]))
                                                ['() blank-fields] mapped-rows))
          ]
      (create-tone-map defaulted-mapped-rows)
      )))

(defn split-columns [cat subcats size]
  "From a map of sub-categories create a list of group lists of same size, with last filled with nil.
   Subcategory names appear inline in the lists occupying a slot."
  (let [linear-tones (mapcat #(conj (get subcats %) %) (keys subcats))]
    ;; TODO nil insertion to eliminate widows
    (map (fn [col] [cat (vec col)]) (partition size size (repeat nil) linear-tones))))

(defn split-pages [column-maps size]
  "From a list of column maps create page maps"

  )

(defn format-header-cell [[cat count]]
  (format "grid.cell(colspan: %d, align: center, [*%s*])," (* count 2) cat))

(defn format-body-cell [x]
  (cond
    (nil? x) "grid.cell(colspan: 2, []),"
    (string? x) (format "grid.cell(colspan: 2, align: center, [*%s*])," x)
    :else (let [[id name] x]
            (format "[%s],[%s]," id name)
            )))

(defn format-page [spec cols]
  (let [col-subcats (map first cols)
        subcat-freqs (frequencies col-subcats)
        subcats-with-counts (map (fn [c] [c (subcat-freqs c)]) (distinct col-subcats))
        body-rows (apply map vector (map #(nth % 1) cols))
        header (apply str (map format-header-cell subcats-with-counts))
        body (map #(apply str %) (map (fn [row] (map format-body-cell row)) body-rows))
        grid-begin (format "#pagebreak(weak: true)\n#grid(columns: %d, row-gutter: %s, column-gutter: %s,\n"
                           (* 2 (count cols))
                           (spec :row-gutter)
                           (spec :column-gutter))
        grid-end "\n)\n"
        ]
    (str grid-begin (str/join "\n" (conj body header)) grid-end)
    ))

(defn create-cheatsheet [build-dir spec formatted-pages]
  (let [typst (spec :typst)
        build (build-spec build-dir spec)]
    (with-open [out-f (io/writer (get-in build [:typst :out-path] ))]
      (let [header (format "#set text(font: \"%s\", size: %s)

#set page(
  paper: \"%s\",
  flipped: %s,
  margin: %s,
)
"
                           (typst :font)
                           (typst :font-size)
                           (typst :paper)
                           (typst :flipped)
                           (typst :margin)
                           )
            trailer ""]
        (.write out-f header)
        (doseq [page formatted-pages]
          (.write out-f page))
        (.write out-f trailer)))))

(defn -main
  "Create PDF cheatsheet for each sheet descriptor."
  [& args]
  (let [sheets-dir (io/file "sheets")
        build-dir "build"
        specs (map get-sheet-spec (yaml-files sheets-dir))]
    (doseq [spec specs]
      (let [tones (get-tones (spec :tones))
            groups (mapcat (fn [cat] (split-columns cat (tones cat) (get-in spec [:layout :rows]))) (reverse (keys tones)))
            pages (map vec (partition (get-in spec [:layout :cols]) groups))
            formatted-pages (map #(format-page (spec :typst) %) pages)
            ]
           ;(println spec)
           ;(println tones)
        ;(pp/pprint pages)
                                        ;(pp/pprint h)
        (create-cheatsheet build-dir spec formatted-pages)
        ))))
