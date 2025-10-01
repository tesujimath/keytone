(ns keytone.core
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.instrument :as mi]
            [malli.util :as mu]
            [malli.dev.pretty :as pretty])
  (:gen-class))

;; a schema for a java.io.File
;; TODO should be in files namespace, but what about ensure-dir for debugging?
(def File [:fn {:error/message "expected java.io.File"} #(instance? java.io.File %)])

;; a schema for a java.io.Writer
(def Writer [:fn {:error/message "expected java.io.Writer"} #(instance? java.io.Writer %)])

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

(def Spec
  "Base schema for any spec."
  [:map
   [:meta [:map
           [:name :string]]]])

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

(def LayoutSpec
  "A spec for an layout."
  (mu/merge
   Spec
   [:map
    [:unit :string]
    [:page [:map
            [:width :int]
            [:height :int]
            [:margin :int]]]
    [:grid [:map
            [:rows :int]
            [:cols :int]
            [:row-gutter :int]
            [:column-gutter-large :int]
            [:column-gutter-small :int]]]]))

(def StyleSpec
  "A spec for a style."
  (mu/merge
   Spec
   [:map
    [:text [:map
            [:font :string]
            [:size :int]
            [:unit :string]]]
    [:fill [:sequential :string]]]))

(defn get-field-indices
  "Return indices of field-names in header, assumed to be present."
  [field-names header]
  (let [header-indices (into {} (map-indexed (fn [i v] [v i]) header))]
    (map #(get header-indices %) field-names)))


(defn nonblank-or-default
  "Take non-blank items from l, otherwise corresponding defaults."
  [l defaults]
  (map (fn [x d] (if (empty? x) d x)) l defaults))

(def Cat
  "A category name."
  :string)

(def Cats
  "Category names"
  [:sequential :string])

(def Colour
  "Unbounded colour index used to generate actual colours by `mod`"
  :int)

(def ColouredSubcat
  "Sub-category with its colour."
  [:tuple Colour :string])  ; [index name]

(def ColouredSubcats
  "List of sub-categories each with its index."
  [:sequential ColouredSubcat])

(def ColouredSubcatsByCat
  "Map of sub-category by category name, where the sub-categories appear with their index in the overall list of all sub-categories."
  [:map-of :string ColouredSubcats])

(def Tone
  "A tone with its id."
  [:tuple :string :string]) ; [id name]

(def ColouredTone
  "A tone/id with colour."
  [:tuple Colour Tone]) ; [id name]

(def Tones
  "A list of tones."
  [:sequential Tone])

(def TonesBySubcat
  "Map of list of tones for each subcat."
  [:map-of :string Tones])

(def TonesBySubcatByCat
  "Map returned by `get-tones`, indexed by cat."
  [:map-of :string TonesBySubcat])

(def ColouredTonesOrSubcatsOrNil
  "List of optional coloured tone or coloured subcat."
  [:sequential [:maybe [:or ColouredTone ColouredSubcat]]])

(defn get-tones
  "Return the cats, subcats, and tones from the raw tones list, with blank entries in the CSV propagated from last non-blank value in that column."
  {:malli/schema [:=> [:cat InstrumentSpec [:vector [:vector :string]]] [:tuple Cats ColouredSubcatsByCat TonesBySubcatByCat]]}
  [instrument raw-tones]
  (let [field-names (map #(get-in instrument [:header %]) [:category :sub-category :id :name])
        blank-fields (map (fn [_] "") field-names)
        field-indices (get-field-indices field-names (first raw-tones))
        field-getters (map #(fn [row] (get row %)) field-indices)
        row-mapper (fn [row] (map #(% row) field-getters))
        mapped-rows (map row-mapper (rest raw-tones))
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
                      (reverse tone-list))]
    (dump-edn "cats" cats)
    (dump-edn "subcats" subcats)
    (dump-edn "tones" tones)
    [cats subcats tones]
    ))

(def Column
  "A column comprising its category and the list of coloured sub-categories or tones, filled with nil."
  [:tuple :string ColouredTonesOrSubcatsOrNil])

(defn partition-cat-into-columns
  "From a map of sub-categories create a list of group lists of same size, with last filled with nil.
   Subcategory names appear inline in the lists occupying a slot."
  {:malli/schema [:=> [:cat :int :string ColouredSubcats TonesBySubcat] [:sequential Column]]}
  [size cat subcats tones]
  (let [safe-cat (-> cat
                     (str/replace " " "-")
                     (str/replace "/" "-"))
        dummy1 (dump-edn (format "subcats.%s" safe-cat) subcats)
        dummy2 (dump-edn (format "tones.%s" safe-cat) tones)
        linear-tones (mapcat (fn [[i-subcat subcat]] (conj (map #(vector i-subcat %)
                                                                (get tones subcat))
                                                           [i-subcat subcat]))
                             subcats)
        columns (map (fn [col] [cat (vec col)]) (partition size size (repeat nil) linear-tones))
        ;; TODO nil insertion to eliminate widows
        ]
    (dump-edn (format "tones.%s" safe-cat)
              tones)
    (dump-edn (format "linear-tones.%s" safe-cat)
              linear-tones)
    (dump-edn (format "columns.%s" safe-cat)
              columns)
    columns))

(defn partition-into-columns
  "Collate all columns by partitioning for each category."
  {:malli/schema [:=> [:cat :int Cats ColouredSubcatsByCat TonesBySubcatByCat] [:sequential Column]]}
  [size cats subcats tones]
  (let [columns (mapcat (fn [cat] (partition-cat-into-columns size cat (subcats cat) (tones cat))) cats)]
    (dump-edn "columns" columns)
    columns))

(def Page
  "A page's width of columns."
  [:vector Column])

(defn partition-columns-into-pages
  "Partition according to the page width in columns."
  {:malli/schema [:=> [:cat :int :int [:sequential Column]] [:sequential Page]]}
  [rows cols columns]
  (let [empty-column [nil (vec (repeat rows nil))]
        ;; need to include empty columns for the partition, but then we immediately filter them out
        pages (map #(vec (filter (comp some? first) %)) (partition cols cols (repeat empty-column) columns))]
    (dump-edn "pages" pages)
    pages))

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
  {:malli/schema [:=> [:cat LayoutSpec StyleSpec Page] :string]}
  [layout style page]
  (let [page-cats (map first page)
        cat-freqs (frequencies page-cats)
        cats-with-counts (map (fn [c] [c (cat-freqs c)]) (distinct page-cats))
        vlines (rest (reverse (second (reduce (fn [[total xs] [_ freq]] [(+ total freq) (conj xs total)]) [0 ()]  cats-with-counts))))
        body-rows (apply map vector (map #(nth % 1) page))
        dummy (dump-edn "body-rows" body-rows)
        header (str (apply str (map #(format-header-cell % layout) cats-with-counts)) "grid.hline(),")
        body (map #(apply str %) (map-indexed #(format-body-row %1 %2 vlines layout style) body-rows))
        grid-begin (str (format "#pagebreak(weak:true)\n#grid(columns:%d,\n"
                                (* 2 (count page)))
                        (apply str (map #(format "grid.vline(x: %d),\n" (* 2 %)) vlines)))
        grid-end "\n)\n"
        ]
    (str grid-begin (str/join "\n" (conj body header)) grid-end)
    ))

(defn write-cheatsheet
  "Write out cheatsheet to paths specified in `build`,"
  {:malli/schema [:=> [:cat Writer LayoutSpec StyleSpec [:sequential :string]] :nil]}
  [w layout style formatted-pages]
  (let [text (style :text)
        layout-unit (layout :unit)
        page (layout :page)
        header (format "#set text(font: \"%s\", size: %d%s)

#set page(
  width: %d%s,
  height: %d%s,
  margin: %d%s,
)
"
                       (text :font)
                       (text :size) (text :unit)
                       (page :width) layout-unit
                       (page :height) layout-unit
                       (page :margin) layout-unit
                       )
        trailer ""]
    (.write w header)
    (doseq [page formatted-pages]
      (.write w page))
    (.write w trailer)))
