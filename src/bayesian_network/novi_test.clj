(ns bayesian-network.novi-test
 ; (:use [incanter core charts stats])
  )

(defrecord Node [^String node-name node-parents node-output])

(defrecord InputNode [^String node-name node-state nod-probability] )

(defprotocol NodeDriving
  "Protocol for managing nodes."
  (probability [this forebears distribution])
  (node-state [this forebears distribution]))


(extend-protocol NodeDriving
  Node
  (probability [this]
    (let [par
     (loop [sek seq(this)]
      (if (and sek sek)
        (if (= (:node-output (first (first sek))) par


      )
    )

  )
