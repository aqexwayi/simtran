(ns simtran.db
  (:require [monger core collection util query])
  (:use [monger operators])
  (:import [com.mongodb WriteConcern])
  (:import [java.util Date])
  )

(def db-config {:host "localhost"
                :port 27017
                :name "testpid"})

(comment
  (defn connect! []
    (try
      (do 
        (monger.core/connect! db-config)
        (monger.core/set-db! (monger.core/get-db (:name db-config)))
        true)
      (catch Exception _ false)))

  (defn disconnect! []
    (monger.core/disconnect!))

  (defn read! []
    (let [t (Date.)
          t- (Date. (- (.getTime t) 60000))
          ds (monger.collection/find-maps "DATAOUT" {:_id {$gt t-}})
          data (last ds)]
      (get data :U 0.0)
      ))

  (defn write! [u c]
    (monger.collection/insert "DATAIN" {:_id (Date.) 
                                        :U u
                                        :C c})))
(defn connect! []
  ())

(defn disconnect! []
  ())

(defn read! []
  1.0)

(defn write! [u c]
  (println "u=" u ",c=" c))

