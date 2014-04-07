;; Copyright 2013, 2014 BigML
;; Licensed under the Apache License, Version 2.0
;; http://www.apache.org/licenses/LICENSE-2.0

(ns bigml.sampling.simple
  "Provides simple random sampling. The original population is kept in
   memory but the resulting sample set is produced as a lazy
   sequence."
  (:require (bigml.sampling [random :as random]
                            [util :as util])))

(defn- with-replacement [coll rnd]
  (when-not (empty? coll)
    (repeatedly #(nth coll (random/next-int! rnd (count coll))))))

(defn- without-replacement [coll rnd]
  (when-not (empty? coll)
    (let [index (random/next-int! rnd (count coll))]
      (cons (nth coll index)
            (-> (subvec (assoc coll index (first coll)) 1)
                (without-replacement rnd)
                (lazy-seq))))))

(def ^:private branch-factor 8)

(defn- make-tree [coll weigh]
  (if (> (count coll) branch-factor)
    (let [branch-size (Math/ceil (/ (count coll) branch-factor))
          children (doall (map #(make-tree % weigh)
                               (partition-all branch-size coll)))]
      {:total (reduce + (map :total children))
       :children children})
    {:total (reduce + (map weigh coll))
     :items coll}))

(declare pop-item)

(defn- process-parent [roll weigh [prev-total result item] child]
  (let [new-total (+ prev-total (:total child))]
    (if (and (nil? item)
             (> new-total roll prev-total))
      (let [[new-child item] (pop-item weigh child (- roll prev-total))]
        [(+ prev-total (:total new-child))
         (if (zero? (:total new-child))
           result
           (cons new-child result))
         item])
      [new-total (cons child result) item])))

(defn- process-leaf [roll weigh [prev-total result item] candidate-item]
  (let [new-total (+ prev-total (weigh candidate-item))]
    (if (and (nil? item)
             (> new-total roll prev-total))
      [prev-total result candidate-item]
      [new-total (cons candidate-item result) item])))

(defn- pop-item [weigh {:keys [total children items]} roll]
  (if children
    (let [[new-total new-children item]
          (reduce (partial process-parent roll weigh) [0] children)]
      [{:total new-total :children new-children} item])
    (let [[new-total new-items item]
          (reduce (partial process-leaf roll weigh) [0] items)]
      [{:total new-total :items new-items} item])))

(defn- weighted-without-replacement [tree weigh rnd]
  (when (pos? (:total tree))
    (let [[new-tree item]
          (pop-item weigh tree (random/next-double! rnd (:total tree)))]
      (cons item (lazy-seq (weighted-without-replacement new-tree weigh rnd))))))

;; Alias Method
;; Vose, Michael. (1991) "A Linear Algorithm For Generating Random Numbers with a Given Distribution".  IEEE Transactions on Software Engineering 17 (9): 972-976
(defn- make-alias-vector [coll weigh]
  (let [
        weights (->> coll (map #(vector % (weigh %))) (filter (comp (every-pred (complement nil?) pos?) second)))
        [total bin-count] (reduce (fn [[total bin-count] [_ v]] [(+ total v) (inc bin-count)]) [0 1] weights)
        bin-size (/ total bin-count)
        {initial-small-values true
         initial-large-values false} (group-by (comp (partial > bin-size) second) weights)
        ]
      (loop [alias-vector []
             [[sk sv] & ss :as small-values] initial-small-values
             [[lk lv] & ls :as large-values] initial-large-values]
        (cond
         (not (empty? small-values))
         (let [alias-vector_ (conj alias-vector {:pivot (/ sv bin-size) :left sk :right lk})
               lv_ (- lv (- bin-size sv))]
           (cond
            (empty? ls) (vec (concat alias-vector_ (repeat (- bin-count (count alias-vector_)) {:just lk}))) 
            (< lv_ bin-size) (recur alias-vector_ (conj ss [lk lv_]) ls)
            :else (recur alias-vector_ ss (conj ls [lk lv_]))))
         (empty? large-values)
         alias-vector
         (empty? ls)
         (vec (concat alias-vector (repeat (- bin-count (count alias-vector)) {:just lk})))
         :else
         (let [lv_ (- lv bin-size)
               alias-vector_ (conj alias-vector {:just lk})]
           (cond
            (zero? lv_) (recur alias-vector_ ss ls)
            (< lv_ bin-size) (recur alias-vector_ (conj ss [lk lv_]) ls)
            :else (recur alias-vector_ ss (conj ls [lk lv_]))))))))

(defn- weighted-with-replacement [coll weigh rnd]
  (let [alias-vector (make-alias-vector coll weigh)
        bins (count alias-vector)]
    (repeatedly
     #(let [x (random/next-double! rnd bins)
            n (Math/floor x)
            v (nth alias-vector n)]
        (if-let [k (:just v)]
          k
          (let [{p :pivot sk :left lk :right} v]
            (if (< (- x n) p)
              sk
              lk)))))))

(defn sample
  "Returns a lazy sequence of samples from the collection.  The
   collection is kept in memory.

   Options:
    :replace - True to sample with replacement, defaults to false.
    :seed - A seed for the random number generator, defaults to nil.
    :generator - The random number generator to be used, options
                 are :lcg (linear congruential) or :twister (Marsenne
                 twister), default is :lcg.
    :weigh - A function that returns a non-negative weight for an
             item.  When nil, no sampling weights are applied.
             Defaults to nil."
  [coll & opts]
  (if-not (vector? coll)
    (apply sample (vec coll) opts)
    (let [{:keys [replace weigh]} opts
          weigh (util/validated-weigh weigh)
          rnd (apply random/create opts)]
      (cond (and replace weigh)
            (weighted-with-replacement coll weigh rnd)
            weigh
            (weighted-without-replacement (make-tree coll weigh) weigh rnd)
            replace
            (with-replacement coll rnd)
            :else
            (without-replacement coll rnd)))))
