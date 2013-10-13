(ns statistics-1.labs.lab02
  (:require
   [clojure.repl :refer :all]
   [clojure.pprint :refer :all]
   [incanter.core :refer :all]
   [incanter.stats :refer :all]
   [incanter.charts :refer :all]
   [incanter.io :as io]
   [midje.sweet :refer :all]
   [incanter.charts :as c]))



(def labels
  {:subject "Subject number"
   :condition "Condition"
   :verbal_memory_baseline "Verbal memory"
   :visual_memory_baseline "Visual memory"
   :visual-motor_speed_baseline "Visual motor speed"
   :reaction_time_baseline "Reaction time"
   :impulse_control_baseline "Impulse control"
   :total_symptom_baseline "Total symptom"
   :verbal_memory_retest "Verbal memory (retest)"
   :visual_memory_retest "Visual memory (retest)"
   :visual-motor_speed_retest "Visual motor speed (retest)"
   :reaction_time_retest "Reaction time (retest)"
   :impulse_control_retest "Impulse control (retest)"
   :total_symptom_retest "Total symptom (retest)"})

(defmacro forf [defs loop finally]
  (list finally (list 'for defs loop)))

(defn map-vals [fun target]
  (forf [[k v] target]
        {k (fun v)}
        (partial into {})))

(defn map-keys [fun target]
  (forf [[k v] target]
        {(fun k) v}
        (partial into {})))

(def db (io/read-dataset (clojure.java.io/resource "data-02.txt") :delim \tab :header true))
(def groups ($group-by :condition db))
(def control (get groups {:condition "control"}))
(def concussed (get groups {:condition "concussed"}))


(defn median-absolute-deviation [data]
  (let [m (median data)]
    (median (map #(abs (- m %)) data))))


(defn rich-summary [ds]
  (let [summary-fn (fn [col ds]
                     (let [$$-all ($ col ds)
                           $$ (remove nil? $$-all)
                           $min (reduce min $$)
                           $max (reduce max $$)
                           $n (count $$-all)
                           $sd (sd $$)]
                       {:col col
                        :n $n
                        :mean (mean $$)
                        :sd $sd
                        :median (median $$)
                        ;; :trimmed FIX trimmed mean
                        :mad (median-absolute-deviation $$)
                        :min $min
                        :max $max
                        :range (- $max $min)
                        :skew (skewness $$)
                        :kurtosis (kurtosis $$)
                        :se (/ $sd (sqrt $n))
                        :is-numeric true}))]
    (with-redefs [incanter.stats/numeric-col-summarizer summary-fn] (vec (summary ds)))))

(defn pretty-summary
  ([db group-by]
     (doseq [[k v] ($group-by group-by db)]
       (pretty-summary v)))
  ([db]
     (->> (rich-summary db)
          (map (partial map-vals #(if (float? %) (format "%.2f" %) %)))
          (print-table [:col :n :mean :sd :median :mad
                        :min :max :range :skew :kurtosis :se :is-numeric]))))

(defn view-charts [charts]
  (let [N (count charts)
        y (int (Math/floor (sqrt N)))
        x (int (Math/ceil (/ N y)))
        width  (* x 500)
        height (* y 400)]
    (let [frame (javax.swing.JFrame.)]
      (.setLayout frame (java.awt.GridLayout. y x))
      (doseq [chart charts]
        (.add frame (org.jfree.chart.ChartPanel. chart)))
      (doto frame
        (.setSize width height)
        (.setVisible true)))))

(defn view-histograms [cols ds]
  (forf [x cols]
        (histogram ($ x ds) :title (or (get labels x) (name x))
                   :x-label (name x))
        view-charts))

(defn view-6-things [ds]
  (view-histograms [:verbal_memory_baseline :visual_memory_baseline
                    :visual-motor_speed_baseline :reaction_time_baseline
                    :impulse_control_baseline :total_symptom_baseline]
                   ds))


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
