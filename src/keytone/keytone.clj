(ns keytone.keytone
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.instrument :as mi]
            [malli.util :as mu]
            [malli.dev.pretty :as pretty])
  (:gen-class))

;; a schema for a java.io.File
(def File [:fn {:error/message "expected java.io.File"} #(instance? java.io.File %)])

(defn ensure-dir
  "Ensure directory exists"
  {:malli/schema [:-> :string File]}
  [path]
  (let [f (io/file path)]
    (.mkdirs f)
    f))

(defn dump-edn
  "Dump data as EDN into named file in debug dir."
  [name data]
  (ensure-dir "debug")
  (let [w (clojure.java.io/writer (io/file "debug" (format "%s.edn" name)))]
    (pp/pprint data w)))

(defn yaml-files
  "Return all YAML files in directory."
  {:malli/schema [:-> File [:seqable File]]}
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

(def Spec
  "Base schema for any spec."
  [:map
   [:meta [:map
           [:name :string]
           [:dir :string]]]])

(defn get-spec
  "Get the spec as a map from a YAML file, inserting its name and dir as metadata."
  {:malli/schema [:-> File Spec]}
  [yaml-file]
  (let [name (base-name (.getName yaml-file))
        dir (.getParent yaml-file)
        spec (yaml/from-file yaml-file)
        annotated-spec (assoc spec :meta {:name name, :dir dir})]
    annotated-spec
    ))

;;(meta #'get-spec)

(defn spec-name
  "Get the name of the spec from its metadata."
  {:malli/schema [:-> Spec :string]}
  [s]
  (get-in s [:meta :name]))

(def InstrumentSpec
  "A spec for an instrument."
  (mu/merge
   Spec
   [:map
    [:description :string]
    [:header [:map
              [:category :string]
              [:sub-category :string]
              [:id :string]
              [:name :string]]]]))

(defn get-instrument-spec
  "Read a YAML file as an instrument spec."
  {:malli/schema [:-> File InstrumentSpec]}
  [yaml-file]
  (get-spec yaml-file))

(def LayoutSpec
  "A spec for an layout."
  (mu/merge
   Spec
   [:map
    [:page [:map
            [:width :string]
            [:height :string]
            [:margin :string]]]
    [:grid [:map
            [:rows :int]
            [:cols :int]
            [:row-gutter :int]
            [:column-gutter-large :int]
            [:column-gutter-small :int]]]]))

(defn get-layout-spec
  "Read a YAML file as a layout spec."
  {:malli/schema [:-> File LayoutSpec]}
  [yaml-file]
  (get-spec yaml-file))

(def StyleSpec
  "A spec for a style."
  (mu/merge
   Spec
   [:map
    [:text [:map
            [:font :string]
            [:size :int]]]
    [:fill [:vector :string]]]))

(defn get-style-spec
  "Read a YAML file as a style spec."
  {:malli/schema [:-> File StyleSpec]}
  [yaml-file]
  (get-spec yaml-file))

(defn build-paths
  "Return paths for output files."
  [build-dir instrument-name layout-name style-name]
  (let [out-path (fn [ext]
                   (->> (io/file build-dir (format "%s.%s.%s%s" instrument-name layout-name style-name ext)) .getPath))]
    {:typst {:out-path (out-path ".typ")}
     :pdf {:out-path (out-path ".pdf")}
     }))

(defn get-field-indices
  "Return indices of field-names in header, assumed to be present."
  [field-names header]
  (let [header-indices (into {} (map-indexed (fn [i v] [v i]) header))]
    (map #(get header-indices %) field-names)))


(defn nonblank-or-default
  "Take non-blank items from l, otherwise corresponding defaults."
  [l defaults]
  (map (fn [x d] (if (empty? x) d x)) l defaults))

(def Cats
  "All category names"
  [:vector :string])

(def Subcats
  "Map of sub-category by category name, where the sub-categories appear with their index in the overall list of all sub-categories."
  [:map-of :string
   [:sequential [:tuple :int :string]]] ;; [subcat-index subcat-name]
  )

(def Tones
  "Map returned by `get-tones`."
  [:map-of :string ; cat name
   [:map-of :string ; subcat name
    [:sequential [:tuple :string :string]]]]) ; [tone-id tone-name]

(defn get-tones
  "Return the cats, subcats, and tones, with blank entries in the CSV propagated from last non-blank value in that column."
  {:malli/schema [:-> InstrumentSpec [:tuple Cats Subcats Tones]]}
  [instrument]
  (let [tones-file (io/file "resources/instruments" (str (spec-name instrument) ".csv"))]
    (with-open [reader (io/reader tones-file)]
      (let [rows (csv/read-csv reader)
            field-names (map #(get-in instrument [:header %]) [:category :sub-category :id :name])
            blank-fields (map (fn [_] "") field-names)
            field-indices (get-field-indices field-names (first rows))
            field-getters (map #(fn [row] (get row %)) field-indices)
            row-mapper (fn [row] (map #(% row) field-getters))
            mapped-rows (map row-mapper (rest rows))
            tone-list (reverse (first (reduce (fn [[rows defaults] row]
                                                            (let [merged (nonblank-or-default row defaults)]
                                                              [(conj rows merged) merged]))
                                                          ['() blank-fields] mapped-rows)))
            cats (vec (distinct (map #(nth % 0) tone-list)))
            cat-subcats (map-indexed #(vector %1 %2) (distinct (map (fn [row] [(nth row 0) (nth row 1)]) tone-list)))
            reversed-subcats (reduce (fn [m [i-subcat [cat subcat]]]
                                                (update m cat #(conj % [i-subcat subcat]))) {} cat-subcats)
            subcats (into {} (map (fn [[k v]] [k (reverse v)]) reversed-subcats))
            tones (reduce (fn [m [cat subcat id name]]
                               (update-in m [cat subcat] #(conj % [id name])))
                             {}
                             (reverse tone-list))
            ]
        (dump-edn "cats" cats)
        (dump-edn "subcats" subcats)
        (dump-edn "tones" tones)
        [cats subcats tones]
        ))))

(defn split-columns
  "From a map of sub-categories create a list of group lists of same size, with last filled with nil.
   Subcategory names appear inline in the lists occupying a slot."
  [cat subcats subcat-tones size]
  (let [safe-cat (-> cat
                     (str/replace " " "-")
                     (str/replace "/" "-"))
        dummy1 (dump-edn (format "subcats.%s" safe-cat) subcats)
        dummy2 (dump-edn (format "subcat-tones.%s" safe-cat) subcat-tones)
        linear-tones (mapcat (fn [[i-subcat subcat]] (conj (map #(vector i-subcat %)
                                                                (get subcat-tones subcat))
                                                           [i-subcat subcat]))
                             subcats)]
    (dump-edn (format "subcat-tones.%s" safe-cat)
              subcat-tones)
    (dump-edn (format "linear-tones.%s" safe-cat)
              linear-tones)
    ;; TODO nil insertion to eliminate widows
    (map (fn [col] [cat (vec col)]) (partition size size (repeat nil) linear-tones))))

(defn pt
  "Return size in points."
  [n] (format "%dpt" n))

(defn halve
  "Return the halves of the given int, such that the sum is the original, so for odd numbers they differ."
  [n f]
  (let [top (quot n 2)
        bottom (- n top)
        ]
    [(f top) (f bottom)]))

(defn format-header-cell
  "Return header cell formatted as a typst cell."
  [[cat count] layout]
  (let [zero-inset "0pt"
        row-gutter (get-in layout [:grid :row-gutter])
        [top bottom] (halve row-gutter pt)
        large-inset (pt (get-in layout [:grid :column-gutter-large]))]
    (format "grid.cell(colspan:%d,align:center,inset:(top:%s,bottom:%s,left:%s,right:%s),[*%s*])," (* count 2) top bottom large-inset large-inset cat)))

(defn format-body-cell
  "Return body cell formatted as a typst cell."
  [maybe-cell prepad-row prepad-col postpad-col layout style]
  (let [zero-inset "0pt"
        row-gutter (get-in layout [:grid :row-gutter])
        [top bottom] (halve row-gutter pt)
        [left right] (halve (get-in layout [:grid :column-gutter-large]) pt)
        small (pt (get-in layout [:grid :column-gutter-small]))]
    (if (nil? maybe-cell) "grid.cell(colspan:2,fill:none,[]),"
        (let [[i-tone cell] maybe-cell
              fill (style :fill)
              rgb (format "rgb(\"%s\")" (fill (mod i-tone (count fill))))]
          (if (string? cell)
            (format "grid.cell(colspan: 2, align: center, inset:(top:%s,bottom:%s,left:%s,right:%s),fill:%s,[*%s*]),"
                    top bottom left right rgb cell)
            (let [[id name] cell]
              (format "grid.cell(inset:(top:%s,bottom:%s,left:%s,right:%s),fill:%s,[%s]),grid.cell(inset:(top:%s,bottom:%s,right:%s),fill:%s,[%s]),"
                      top bottom left small rgb id
                      top bottom right rgb name)))))))

(defn format-body-row
  "Return indexed seqable of formatted body cells in row."
  [i-row row vlines layout style]
  (let [first-row (zero? i-row)
        vlines (set vlines)]
    (map-indexed #(let [prepad-col (vlines %1)
                        postpad-col (vlines (+ %1 1))]
                    (format-body-cell %2 first-row prepad-col postpad-col layout style)) row)))

(defn format-page
  "Return whole page formatted."
  [layout style cols]
  (let [col-cats (map first cols)
        cat-freqs (frequencies col-cats)
        cats-with-counts (map (fn [c] [c (cat-freqs c)]) (distinct col-cats))
        vlines (rest (reverse (second (reduce (fn [[total xs] [_ freq]] [(+ total freq) (conj xs total)]) [0 ()]  cats-with-counts))))
        body-rows (apply map vector (map #(nth % 1) cols))
        dummy (dump-edn "body-rows" body-rows)
        header (str (apply str (map #(format-header-cell % layout) cats-with-counts)) "grid.hline(),")
        body (map #(apply str %) (map-indexed #(format-body-row %1 %2 vlines layout style) body-rows))
        grid-begin (str (format "#pagebreak(weak:true)\n#grid(columns:%d,\n"
                                (* 2 (count cols)))
                        (apply str (map #(format "grid.vline(x: %d),\n" (* 2 %)) vlines)))
        grid-end "\n)\n"
        ]
    (str grid-begin (str/join "\n" (conj body header)) grid-end)
    ))

(defn create-cheatsheet
  "Write out cheatsheet to paths specified in `build`,"
  [build layout style formatted-pages]
  (let [text (style :text)
        page (layout :page)]
    (with-open [out-f (io/writer (get-in build [:typst :out-path] ))]
      (let [header (format "#set text(font: \"%s\", size: %s)

#set page(
  width: %s,
  height: %s,
  margin: %s,
)
"
                           (text :font)
                           (pt (text :size))
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
        instruments (map get-instrument-spec (yaml-files (io/file "resources/instruments")))
        layouts (map get-layout-spec (yaml-files (io/file "resources/layouts")))
        styles (map get-style-spec (yaml-files (io/file "resources/styles")))]
    (doseq [instrument instruments
            layout layouts
            style styles]
      (dump-edn (format "instrument.%s" (spec-name instrument)) instrument)
      (dump-edn (format "style.%s" (spec-name style)) style)
      (let [[cats subcats tones] (get-tones instrument)
            groups (mapcat (fn [cat] (split-columns cat (subcats cat) (tones cat) (get-in layout [:grid :rows]))) cats)
            pages (map vec (partition (get-in layout [:grid :cols]) groups))
            formatted-pages (map #(format-page layout style %) pages)
            build (build-paths build-dir (spec-name instrument) (spec-name layout) (spec-name style))]
        (ensure-dir build-dir)
        (create-cheatsheet build layout style formatted-pages)
        ))))

(mi/collect!)
(mi/instrument! {:report (pretty/thrower)})
