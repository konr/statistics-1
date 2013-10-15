(ns statistics-1.labs.lab03
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



(def impact (io/read-dataset (clojure.java.io/resource "data-02.txt") :delim \tab :header true))


;; (cor impact)


(def groups ($group-by :condition impact))
(def control   (get groups {:condition "control"}))
(def concussed (get groups {:condition "concussed"}))


#_(prettify (cor control nil))
#_(prettify (cor concussed nil))

(def verbal-impair
  ($map - [:verbal_memory_baseline :verbal_memory_retest] concussed))

(def visual-impair
  ($map - [:visual_memory_baseline :visual_memory_retest] concussed))

(def memory-impair
  (map (comp (partial * 0.5) +) verbal-impair visual-impair))

(correlation memory-impair ($ :impulse_control_baseline concussed))

(defn scatter-plot-duo []
  (let [x ($ :visual_memory_baseline impact)
       y ($ :verbal_memory_baseline impact)]
   (doto (scatter-plot x y 
                       :x-label "Visual memory"
                       :y-label "Verbal memory")
     (add-lines x (:fitted (linear-model y x))) 
     view)))

;; Stopped at 16'00 Lab 3
