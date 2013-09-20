(ns bayesian-network.node-protocol
 ; (:use [incanter core charts stats])
  )
; Node hase name, possiable states and parents
(defrecord Node [node-name node-parents node-probabilies])

; Node with out parents -> input node
(defrecord InputNode [node-name node-probabilies])


; Protocol with node methods
(defprotocol NodeDriving

  (node-probability [node network-state] "This method returns probability of node being in some state")

  (node-current-state [node] "This method returns state of node") ;not sure that i need this
  )

; implementation of Protocol
(extend-protocol NodeDriving
  Node
  (node-probability [node network-state]

     ((conj       ; merging node name with names of his parents,
                  ;result set is key for node-probabilities map - > value of that map is probability
        (into
           #{}
           (map #(% network-state) (seq (:node-parents node))))
        ((:node-name node) network-state))

     node-probabilies)
    )

  (node-current-state [node])

  InputNode
  (node-probability [node network-state]
    (((:node-name node) network-state)     ;key, state of a node
                (:node-probabilies node))  ;map of possible probabilities, result map value is node probability
    )

  (node-current-state [node])
 )
