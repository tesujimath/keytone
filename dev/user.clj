(ns user
  (:require [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [keytone.core :as k]
            [keytone.files :as kf]
            [keytone.main :as km]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]
            [user.dummy :as dummy]))

(def keytone-namespaces ['keytone.core 'keytone.files])

(defn instrument-keytone! []
  (mi/collect! {:ns keytone-namespaces})
  (mi/instrument! {:report (pretty/thrower)})
  (println "🔧 Malli instrumentation enabled for keytone.*"))

(defn unstrument-keytone! []
  (mi/unstrument!)
  (println "⏹ Malli instrumentation disabled"))

(defn reset-keytone! []
  (println "Reloading...")
  (doseq [ns keytone-namespaces]
    (require ns :reload))
  (instrument-keytone!))

;; Auto-run when REPL starts
(instrument-keytone!)

;; dummy data
(def dummy)
