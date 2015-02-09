(ns bayesian-network.thinkbayes)

(defn setMapValues [m x y]
  (conj m (hash-map x y)))


(defn setValues
  ([a x]
  (setValues a x 0))

 ([a x y]
  (assoc a x y)))

(defn multProb [a x fact]
  (setValues a x (* (a x) fact)))

(defn totalSum [a]
  (reduce * a))

(defn Prob [a x]
  (if (a x) (a x) 0))

(defn Normalize [a f]
  (let [ff (or f 1.0)
        total (totalSum a)
        factor (/ ff total)]
    (map (partial * factor) a)
  ))
