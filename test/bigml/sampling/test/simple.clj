;; Copyright 2013, 2014 BigML
;; Licensed under the Apache License, Version 2.0
;; http://www.apache.org/licenses/LICENSE-2.0

(ns bigml.sampling.test.simple
  (:use clojure.test
        bigml.sampling.test.util)
  (:require (bigml.sampling [simple :as simple]
                            [random :as random])))

(deftest make-alias-vector-sad-paths
  (is (= [] (#'simple/make-alias-vector [] {})))
  (is (= [] (#'simple/make-alias-vector [:a] {:a 0})))
  (is (= [] (#'simple/make-alias-vector [:a] {:a -1})))
  (is (= [] (#'simple/make-alias-vector [:a] {})))
  (is (= [] (#'simple/make-alias-vector [:a :b] {})))
  (is (= [] (#'simple/make-alias-vector [:a :b] {:a 0 :b -1})))
  (is (= [] (#'simple/make-alias-vector [:a :b :c] {:a 0 :b -1}))))

(defn- unmake-alias-vector
  [alias-vector]
  (let [bin-size (/ 1 (count alias-vector))] 
    (reduce
     (fn [weigh v]
       (if-let [lk (:just v)]
         (update-in weigh [lk] #(+ bin-size (or % 0)))
         (let [{p :pivot sk :left lk :right} v]
           (-> weigh
               (update-in [sk] #(+ (* p bin-size) (or % 0)))
               (update-in [lk] #(+ (* (- 1 p) bin-size) (or % 0))))))) {} alias-vector)))

(deftest unmake-alias-vector-simple
  (let [coll [:a :b]
        weigh {:a 1 :b 1}
        weigh_ (unmake-alias-vector (#'simple/make-alias-vector coll weigh))]
    (is (=  (set coll) (set (keys weigh_))))
    (is (= 1 (apply + (vals weigh_))))
    (is (= (weigh_ :a) (weigh_ :b)))))

(defn- make-weighted-data [& {:keys [seed]}]
  (let [rnd (random/create :seed seed)]
    (map vector (range) (repeatedly #(Math/abs (.nextGaussian rnd))))))

(deftest unmake-alias-vector-random 
  (let [weigh (into {} (take 50 (make-weighted-data :seed :foo)))
        coll (keys weigh)
        total (apply + (vals weigh))
        weigh_ (unmake-alias-vector (#'simple/make-alias-vector coll weigh))
        ]
    (is (= (set coll) (set (keys weigh_))))
    (is (about-eq 1 (apply + (vals weigh_)) 10e-14))
    (is (every? #(about-eq (get weigh %) (* total (get weigh_ %)) 10e-14) coll))))

(deftest sample
  (is (about-eq (reduce + (take 500 (simple/sample (range 1000))))
                250000 25000))
  (is (about-eq (reduce + (take 500 (simple/sample (range 1000) :replace true)))
                250000 25000))
  (let [[v1 v2] (vals (frequencies (take 1000 (simple/sample [0 1] :replace true))))]
    (is (about-eq v1 v2 150))))

(deftest regression
  (is (= (take 10 (simple/sample (range 20) :seed :foo))
         '(7 3 9 6 10 4 2 8 5 13)))
  (is (= (take 10 (simple/sample (range 20) :seed 7))
         '(16 13 17 12 9 4 18 7 14 19)))
  (is (= (take 10 (simple/sample (range 20) :seed 7 :replace true))
         '(16 4 5 4 0 14 8 9 10 14))))

(deftest weighted-regression
  (let [data (take 10 (make-weighted-data :seed :foo))]
    (is (= (map first (simple/sample data :seed :bar :weigh second))
           '(9 0 8 3 1 4 7 6 5 2)))
    (is (= (take 10 (map first (simple/sample data
                                            :seed :bar
                                            :weigh second
                                            :replace true)))
           '(9 3 9 1 5 7 1 0 1 5)))))

(deftest twister-regression
  (is (= (take 10 (simple/sample (range 20)
                               :seed 7
                               :generator :twister))
         '(5 9 6 3 10 17 12 18 8 2)))
  (is (= (take 10 (simple/sample (range 20) :seed 7
                               :generator :twister
                               :replace true))
         '(5 8 4 0 7 17 9 17 0 6))))

(deftest zero-weight
  (is (= {:heads 100}
         (->> (simple/sample [:heads :tails]
                             :replace true
                             :weigh {:heads 1 :tails 0})
              (take 100)
              (frequencies)))))

