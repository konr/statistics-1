(ns statistics-1.labs.lab02
  (:require
   [clojure.repl :refer :all]
   [clojure.pprint :refer :all]
   [incanter.core :refer :all]
   [incanter.stats :refer :all]
   [incanter.charts :refer :all]
   [incanter.io :as io]
   [statistics-1.utils :refer :all]
   [midje.sweet :refer :all]
   [incanter.charts :as c]))



(def db (io/read-dataset (clojure.java.io/resource "data-02.txt") :delim \tab :header true))
(def groups ($group-by :condition db))
(def control (get groups {:condition "control"}))
(def concussed (get groups {:condition "concussed"}))



(facts "on the dataset"
       (dim db) => [40 14]
       (nrow db) => 40
       (ncol db) => 14
       ;; FIX redefine .setVisible (view db) => (partial instance? javax.swing.JFrame)
       (col-names db) => [:subject :condition :verbal_memory_baseline :visual_memory_baseline
                          :visual-motor_speed_baseline :reaction_time_baseline
                          :impulse_control_baseline :total_symptom_baseline
                          :verbal_memory_retest :visual_memory_retest
                          :visual-motor_speed_retest :reaction_time_retest
                          :impulse_control_retest :total_symptom_retest]

       ;; Values and updating the database
       (class db) => incanter.core.Dataset
       (->> db ($ :subject) first class) => java.lang.Long
       (->> (transform-col db :subject str) ($ :subject) first class) => java.lang.String


       ;; Mean and simple statistics
       (mean ($ :verbal_memory_baseline db)) => 89.75
       (sd ($ :verbal_memory_baseline db)) => (roughly 6.44 0.01)
       (rich-summary db) => (has every? :col)
       (rich-summary db) => (has every? :col)

       ;; (pretty-summary db :condition)
       ;; (view-6-things concussed) => irrelevant
       )
