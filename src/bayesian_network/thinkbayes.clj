(ns bayesian-network.thinkbayes)

(defn add-map-values [m key-x value-y]
  (conj m (hash-map key-x value-y)))

(defn set-map-values
  ([m key-x]
   (set-map-values m key-x 0))

  ([m key-x value-y]
   (assoc m key-x value-y)))

(defn multiply-map-probability [m key-x multiply-factor]
  (set-map-values m key-x (* (key-x m) multiply-factor)))


(defn total-map-sum [m]
  (reduce + (vals m)))

(defn probabily-map [m key-x]
  (key-x m))

(defn normalize-map
  ([m] (normalize-map m nil))

  ([m fraction]
  (let [ff (or fraction 1.0)
        total (total-map-sum m)
         factor (/ ff total)]
    (reduce #(update-in % [%2] (partial * factor)) m (keys m))
    )))


(defn likelihod [m array-key]
  (array-key m)
  )

(defn update-map [m l key-x]
 (normalize-map (#(update-in % [%2] (partial * (likelihod %3 %2))) m key-x l))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;    Array attempt    ;;;;;;;;;;;;;;;;;;;;;

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
