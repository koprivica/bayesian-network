(ns bayesian-network.proba-protokola)

(defprotocol Matrix
  "Protocol for working on 2D structures"
  (lookup [matrix i j])
  (update [matrix i j value])
  (rows [matrix])
  (cols [matrix])
  (dims [matrix]))

(defn markova-treca-fija [x]

  (println x)
  )

;= Extending protocol to Vectors

(extend-protocol Matrix
  clojure.lang.IPersistentVector

  (lookup [vov i j]
    (get-in vov [i j]))
  (update [vov i j value]
    (assoc-in vov [i j] value ))
  (rows [vov]
    (seq vov))
  (cols [vov]
    (apply map vector vov))
  (dims [vov]
    [(count vov) (count (first vov))]))

(defn vov
  "Create a vector of h w-item vectors."
  [h w]
  (vec (repeat h (vec (repeat w nil)))))
