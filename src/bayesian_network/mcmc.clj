(ns bayesian-network.mcmc)

;(defn gibbs [limit add-up x prior likelihood posterior]

;(if (add-up < limit)
;  posterior)

;(recur limit (inc add-up)))


(defn get-sample
  ([m-seq r-value]
   (get-sample m-seq r-value 0 nil))


  ([m-seq r-value sum x-key]

  (if (> sum r-value)
    x-key
  (recur (rest m-seq) r-value (+ sum ((first m-seq) 1)) ((first m-seq) 0)))))


(defn sampling-posterior
  [prior-map likelihood-map num-samples]




  (loop [i 1
         cur_0 (get-sample prior-map (/ (rand-int 100) 100))
         cur_1 (get-sample prior-map (/ (rand-int 100) 100))
         res-samples []]

    (if (> i num-samples)
      res-samples
      (if (< (rand-int 1) (- (cur_1 likelihood-map) (cur_0 likelihood-map)))
        (recur (inc i)
               cur_1
             (get-sample prior-map (/ (rand-int 100) 100))
             (conj res-samples cur_1))
         (recur (inc i)
               cur_0
             (get-sample prior-map (/ (rand-int 100) 100))
             (conj res-samples cur_0))
         )
      )
    )
  )
