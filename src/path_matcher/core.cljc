(ns path-matcher.core)

(defn match-all? [el]
  (= :* el))

(defn match-all-pattern? [pattern]
  (= [:*] pattern))

(defn pattern->subpatterns [pattern]
  (->> pattern
       dedupe
       (partition-by match-all?)
       (mapv vec)))

(defn get-specificity [subpatterns]
  (let [starts-with-match-all? (match-all-pattern? (first subpatterns))
        match-all-count (count (filter match-all-pattern? subpatterns))
        res (loop [rem-patterns subpatterns
                   specificity [0]]
              (if (not (seq rem-patterns))
                (reduce + specificity)
                (let [current-pattern (first rem-patterns)]
                  (if (match-all-pattern? current-pattern)
                    (recur
                     (rest rem-patterns)
                     (conj (vec (butlast specificity)) (* 0.9 (last specificity))))
                    (recur
                     (rest rem-patterns)
                     (conj specificity (count current-pattern)))))))
        res' (- res (/ match-all-count 100))]
    (if starts-with-match-all?
      (* 0.9 res')
      res')))

(defn starts-with? [v sv]
  (let [c-v (count v)
        c-sv (count sv)]
    (if (> c-sv c-v)
      false
      (= (subvec v 0 c-sv) sv))))

(defn pattern-matches? [path pattern]
  (let [subpatterns (pattern->subpatterns pattern)
        specificity (get-specificity subpatterns)
        result
        (loop [rem-path path
               rem-patterns subpatterns]
          (let [current-pattern (first rem-patterns)
                next-pattern (second rem-patterns)
                matching-all? (match-all-pattern? current-pattern)]
            (cond
              (and (seq rem-path) (not (seq rem-patterns)))
              false

              (and (not (seq rem-path)) (seq rem-patterns))
              false

              (and (not (seq rem-path)) (not (seq rem-patterns)))
              true

              (and matching-all? (nil? next-pattern)) 
              true

              (and matching-all? (starts-with? rem-path next-pattern))
              (recur (subvec rem-path (count next-pattern))
                     (subvec rem-patterns 2))

              (starts-with? rem-path current-pattern)
              (recur (subvec rem-path (count current-pattern))
                     (subvec rem-patterns 1))

              matching-all?
              (recur (subvec rem-path 1)
                     rem-patterns)
              
              :else false)))]
    (if result
      {:specificity specificity :matches? true}
      {:matches? false})))

(defn matches-with-specificity? [path pattern]
  (cond
    (= path pattern) {:specificity ##Inf :matches? true}
    (match-all-pattern? pattern) {:specificity -1 :matches? true}
    :else (pattern-matches? path pattern)))

(defn matches? [path pattern]
  (:matches? (matches-with-specificity? path pattern)))

(defn get-matching [matcher-map path]
  (let [res (reduce-kv 
             (fn [acc k v]
               (let [r (matches-with-specificity? path k)]
                 (if (:matches? r)
                   (conj acc (assoc r :value v))
                   acc)))
             []
             matcher-map)]
    (->> res
         (sort-by :specificity)
         reverse
         (mapv :value))))
