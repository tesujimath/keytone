(ns user
  (:require [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [keytone.core :as k]
            [keytone.main :as km]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]))

(defn instrument-core! []
  (mi/collect! {:ns ['keytone.core]})
  (mi/instrument! {:report (pretty/thrower)})
  (println "🔧 Malli instrumentation enabled for keytone.core"))

(defn unstrument-core! []
  (mi/unstrument!)
  (println "⏹ Malli instrumentation disabled"))

(defn reset-core! []
  (println "Reloading...")
  (require 'keytone.core :reload)
  (instrument-core!))

;; Auto-run when REPL starts
(instrument-core!)
