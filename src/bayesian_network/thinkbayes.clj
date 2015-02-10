(ns bayesian-network.thinkbayes)

(defn set-map-values [m key-x value-y]
  (conj m (hash-map key-x value-y)))


(defn set-array-values
  ([array-a position-x]
  (set-array-values array-a position-x 0))

 ([array-a position-x value-y]
  (assoc array-a position-x value-y)))

(defn multiply-array-probability [array-a position-x multuplay-factor]
  (set-array-values array-a position-x (* (array-a position-x) multuplay-factor)))

(defn total-array-sum [array-a]
  (reduce + array-a))

(defn probabily-array [array-a position-x]
  (if (array-a position-x) (array-a position-x) 0))

(defn normalize-array [array-a fraction-f]
  (let [ff (or fraction-f 1.0)
        total (total-array-sum array-a)
        factor (/ ff total)]
    (map (partial * factor) array-a)
    ) )
