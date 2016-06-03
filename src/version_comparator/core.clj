(ns version-comparator.core
  (require [clojure.string :as st]
           [clojure.data :as d]
           [clojure.set :as s]))

(def index-map [:major :minor :step :addon])

;; Util functions

(defn- standard
  [vv]
  (<= (count vv) 4))

(defn- third
  [vect]
  (get vect 2))

(defn- update-vals [m vals f]
  (if (not (nil? m))
    (reduce #(update-in % [%2] f) m vals)))

(defn- zeroize-map
  [m]
  (update-vals m (into [] (keys m)) (fn [_] (identity "0"))))

(defn- not-nil?
  [arg]
  (not (nil? arg)))

(defn- clean-keys
  [m]
  (let [f (filter not-nil? m)]
    (apply sorted-map (interleave f (repeatedly (count f) #(identity "0"))))))

(defn- key-set
  [m]
  (into (sorted-set) (keys m)))

(defn- comp-key
  [k]
  (st/includes? k "comp"))

(defn- get-comp-index
  [k]
  (read-string (st/replace (name k) "comp" "")))

(defn- find-first
  [pred coll]
  (first (filter pred coll)))

(defn- get-result
  [coll]
  (if (every? zero? coll)
    0
    (find-first #(not (zero? %)) coll)))

;; Comparators

  (defn compare-keys
    [k1 k2]
    (cond
      (and (= k1 :major) (= k2 :minor)) 1
      (and (= k1 :major) (= k2 :step)) 1
      (and (= k1 :major) (comp-key k2)) 1
      (and (= k1 :major) (= k2 :addon)) 1

      (and (= k1 :minor) (= k2 :major)) -1
      (and (= k1 :minor) (= k2 :step)) 1
      (and (= k1 :minor) (comp-key k2)) 1
      (and (= k1 :minor) (= k2 :addon)) 1

      (and (= k1 :step) (= k2 :major)) -1
      (and (= k1 :step) (= k2 :minor)) -1
      (and (= k1 :step) (comp-key k2)) 1
      (and (= k1 :step) (= k2 :addon)) 1

      (and (comp-key k1) (= k2 :major)) -1
      (and (comp-key k1) (= k2 :minor)) -1
      (and (comp-key k1) (= k2 :step)) -1
      (and (comp-key k1) (= k2 :addon)) 1

      (and (comp-key k1) (comp-key k2)) (compare (get-comp-index k2) (get-comp-index k1))

      (and (= k1 :addon) (= k2 :major)) -1
      (and (= k1 :addon) (= k2 :minor)) -1
      (and (= k1 :addon) (= k2 :step)) -1
      (and (= k1 :addon) (comp-key k2)) -1
      :else 0))

  (def key-comparator (comparator compare-keys))

  ;; Main functions

  (defn parse-version-string
    [v]
    (vec (filter
           (fn [arg] (and
                       (not (empty? arg))
                       (not (nil? arg))))
           (st/split v #"\.|-"))))

  (defn std-to-version-map
    [vv]
    (into (sorted-map-by key-comparator)
          (map-indexed (fn [ind item] {(nth index-map ind) item}) vv)))

  (defn nstd-to-version-map
    [vv]
    (let [dcount (dec (count vv))]
      (-> (sorted-map-by key-comparator)
        (assoc :major (first vv))
        (assoc :minor (second vv))
        (assoc :step (third vv))
        (conj (apply conj (for [i (range 3 dcount)]
                            {(keyword (str "comp" (- dcount i))) (get vv i)})))
        (assoc :addon (last vv)))))

  (defn to-version-map
    [version-string]
    (let [vv (parse-version-string version-string)]
      (if (standard vv)
        (std-to-version-map vv)
        (nstd-to-version-map vv))))

  ;; Uses clojure.data/diff but doesn't work on large
  ;; vectors with keywords.
  (defn equalize-version-maps-1
    [m1 m2]
    (let [m1size (count m1)
          m2size (count m2)]
      (if (= m1size m2size)
        (vector m1 m2)
        (let [[f s _] (d/diff (keys m1) (keys m2))]
          [(conj m1 (clean-keys s))
           (conj m2 (clean-keys f))]))))

  (defn equalize-version-maps
    [m1 m2]
    (let [m1size (count m1)
          m2size (count m2)]
      (if (= m1size m2size)
        (vector m1 m2)
        (if (> m1size m2size)
          [m1 (conj m2 (clean-keys (s/difference (key-set m1) (key-set m2))))]
          [(conj m1 (clean-keys (s/difference (key-set m2) (key-set m1)))) m2]))))

  (defn compare-versions
    [v1 v2]
    (let [[vm1 vm2 :as both] (equalize-version-maps (to-version-map v1) (to-version-map v2))
          m1 (reverse vm1)
          m2 (reverse vm2)]
      (get-result (for [i (range (count m1))]
        (compare (read-string (second (nth m1 i))) (read-string (second (nth m2 i))))))))
