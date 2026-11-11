(ns dev
  (:require [scicloj.clay.v2.api :as clay]))

(defn start-clay!
  "Start Clay live reload server pointing to notebooks/index.clj."
  ([]
   (start-clay! {}))
  ([opts]
   (clay/start!
     (merge {:source-path "notebooks/index.clj"}
            opts))))

(defn make-clay!
  "Render the notebook."
  ([]
   (make-clay! {}))
  ([opts]
   (clay/make!
     (merge {:source-path "notebooks/index.clj"}
            opts))))

(comment
  ;; Start Clay server and file watcher
  (start-clay!)

  ;; Trigger a make / render
  (make-clay!)
  )
