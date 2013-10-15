(ns statistics-1.utils
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

(def six-things
  [:verbal_memory_baseline :visual_memory_baseline
   :visual-motor_speed_baseline :reaction_time_baseline
   :impulse_control_baseline :total_symptom_baseline])

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

(defn prettify [data & columns]
  (let [data (map (partial map-vals #(if (float? %) (format "%.2f" %) %)) data)] 
    (if columns (print-table columns data)
        (print-table data))))

(defn pretty-summary
  ([db group-by]
     (doseq [[k v] ($group-by group-by db)]
       (pretty-summary v [:col :n :mean :sd :median :mad
                          :min :max :range :skew :kurtosis :se :is-numeric])))
  ([db]
     (prettify (rich-summary db))))

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
  (view-histograms six-things ds))

(defn eg [field ds]
  (-> field ($ ds) rand-nth ))


(defn cor [ds fields]
  (let [fields (or fields (filter #(number? (eg % ds)) (col-names ds)))]
   (for [x fields]
     (forf [y fields]
           {y correlation ($ x ds) ($ y ds))}
           #(assoc (into {} %) :col x)))) )
