(ns keytone.keytone
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

(defn get-spec [yaml-file]
  "Get the spec as a map from a YAML file, inserting its name as an extra field at the toplevel."
  (let [name (base-name (.getName yaml-file))
        spec (yaml/from-file yaml-file)
        named-spec (assoc spec :name name)]
    named-spec
    ))

(defn build-spec [build-dir instrument-name layout-name]
  (let [out-path (fn [ext]
                   (->> (io/file build-dir (format "%s.%s%s" instrument-name layout-name ext)) .getPath))]
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
  "Create a tone map from the list."
  (let [cats (vec (distinct (map #(nth % 0) tone-list)))
        cat-subcats (distinct (map (fn [row] [(nth row 0) (nth row 1)]) tone-list))
        cat-subcat-unordered-list (reduce (fn [m [cat subcat]]
                                            (update m cat #(conj % subcat))) {} cat-subcats)
        cat-subcat-list (into {} (map (fn [[k v]] [k (reverse v)]) cat-subcat-unordered-list))
        tone-map (reduce (fn [m [cat subcat id name]]
                           (update-in m [cat subcat] #(conj % [id name])))
                         {}
                         (reverse tone-list))
        ]
    ;;(pp/pprint cat-subcat-list)
    { :cats cats :subcats cat-subcat-list :map tone-map}))

(defn get-tones [tones-file {:keys [header]}]
  "Return the tones as a vector of maps, with blank entries in the CSV propagated from last non-blank value in that column."
  (with-open [reader (io/reader tones-file)]
    (let [rows (csv/read-csv reader)
          field-names (map #(get header %) [:category :sub-category :id :name])
          blank-fields (map (fn [_] "") field-names)
          field-indices (get-field-indices field-names (first rows))
          field-getters (map #(fn [row] (get row %)) field-indices)
          row-mapper (fn [row] (map #(% row) field-getters))
          mapped-rows (map row-mapper (rest rows))
          defaulted-mapped-rows (reverse (first (reduce (fn [[rows defaults] row]
                                                          (let [merged (nonblank-or-default row defaults)]
                                                            [(conj rows merged) merged]))
                                                        ['() blank-fields] mapped-rows)))
          ]
      ;;(pp/pprint field-names)
      (create-tone-map defaulted-mapped-rows)
      )))

(defn split-columns [cat subcats subcat-tones size]
  "From a map of sub-categories create a list of group lists of same size, with last filled with nil.
   Subcategory names appear inline in the lists occupying a slot."
  (let [linear-tones (mapcat #(conj (get subcat-tones %) %) subcats)]
    ;; TODO nil insertion to eliminate widows
    (map (fn [col] [cat (vec col)]) (partition size size (repeat nil) linear-tones))))

(defn split-pages [column-maps size]
  "From a list of column maps create page maps"

  )

(defn format-header-cell [[cat count] layout]
  (let [zero-inset "0pt"
        large-inset (get-in layout [:grid :column-gutter-large])]
    (format "grid.cell(colspan:%d,align:center,inset:(left:%s,right:%s),[*%s*])," (* count 2) large-inset large-inset cat)))

(defn format-body-cell [x prepad-row prepad-col postpad-col layout]
  (let [zero-inset "0pt"
        top-inset (if prepad-row (get-in layout [:grid :row-gutter]) zero-inset)
        large-inset (get-in layout [:grid :column-gutter-large])
        small-inset (get-in layout [:grid :column-gutter-small])
        id-left-inset large-inset
        name-right-inset (if postpad-col large-inset zero-inset)]
    (cond
      (nil? x) "grid.cell(colspan: 2, []),"
      (string? x) (format "grid.cell(colspan: 2, align: center, inset:(top:%s,left:%s,right:%s),[*%s*])," top-inset large-inset large-inset x)
      :else (let [[id name] x]
              (format "grid.cell(inset:(top:%s,left:%s,right:%s),[%s]),grid.cell(inset:(top:%s,right:%s),[%s]),"
                      top-inset id-left-inset (get-in layout [:grid :column-gutter-small]) id
                      top-inset name-right-inset name)))))

(defn format-body-row [i row vlines layout]
  (let [first-row (zero? i)
        vlines (set vlines)]
    (map-indexed #(let [prepad-col (vlines %1)
                        postpad-col (vlines (+ %1 1))]
                    (format-body-cell %2 first-row prepad-col postpad-col layout)) row)))

(defn format-page [layout cols]
  (let [col-cats (map first cols)
        cat-freqs (frequencies col-cats)
        cats-with-counts (map (fn [c] [c (cat-freqs c)]) (distinct col-cats))
        vlines (rest (reverse (second (reduce (fn [[total xs] [_ freq]] [(+ total freq) (conj xs total)]) [0 ()]  cats-with-counts))))
        body-rows (apply map vector (map #(nth % 1) cols))
        header (str (apply str (map #(format-header-cell % layout) cats-with-counts)) "grid.hline(),")
        body (map #(apply str %) (map-indexed #(format-body-row %1 %2 vlines layout) body-rows))
        grid-begin (str (format "#pagebreak(weak: true)\n#grid(columns: %d, row-gutter: %s,\n"
                               (* 2 (count cols))
                               (get-in layout [:grid :row-gutter]))
                        (apply str (map #(format "grid.vline(x: %d),\n" (* 2 %)) vlines)))
        grid-end "\n)\n"
        ]
    (str grid-begin (str/join "\n" (conj body header)) grid-end)
    ))

(defn ensure-dir [path]
  "Ensure directory exists"
  (let [f (io/file path)]
    (.mkdirs f)
    f))

(defn create-cheatsheet [build layout formatted-pages]
  (let [text (layout :text)
        page(layout :page)]
    (with-open [out-f (io/writer (get-in build [:typst :out-path] ))]
      (let [header (format "#set text(font: \"%s\", size: %s)

#set page(
  width: %s,
  height: %s,
  margin: %s,
)
"
                           (text :font)
                           (text :size)
                           (page :width)
                           (page :height)
                           (page :margin)
                           )
            trailer ""]
        (.write out-f header)
        (doseq [page formatted-pages]
          (.write out-f page))
        (.write out-f trailer)))))

(defn -main
  "Create PDF cheatsheet for each pair of instrument and layout."
  [& args]
  (let [build-dir "build"
        instruments (map get-spec (yaml-files (io/file "resources/instruments")))
        layouts (map get-spec (yaml-files (io/file "resources/layouts")))]
    (doseq [instrument instruments
            layout layouts]
      (let [csv-file (io/file "resources/instruments" (str (instrument :name) ".csv"))
            tones (get-tones csv-file instrument)
            groups (mapcat (fn [cat] (split-columns cat (get-in tones [:subcats cat]) (get-in tones [:map cat]) (get-in layout [:grid :rows]))) (tones :cats))
            pages (map vec (partition (get-in layout [:grid :cols]) groups))
            formatted-pages (map #(format-page layout %) pages)
            build (build-spec build-dir (instrument :name) (layout :name))]
        ;;(pp/pprint csv-file)
                                        ;(pp/pprint h)
        ;;(pp/pprint tones)
        ;;(pp/pprint pages)
        (ensure-dir build-dir)
        (create-cheatsheet build layout formatted-pages)
        ))))
