(ns bayesian-network.novi-test
 ; (:use [incanter core charts stats])
  )
; Node hase name, possiable states and parents
(defrecord Node [node-name node-parents node-probabilies])

; Node with out parents -> input node
(defrecord InputNode [node-name node-probabilies])


; Protocol with node methods
(defprotocol NodeDriving
  (node-probability [node] "This method returns probability of node being in some state")
  (node-current-state [node] "This method returns state of node") ;not sure that i need this
  )

; implementation of Protocol
(extend-protocol NodeDriving
  Node
  (node-probability [node network-state]
    (into #{} (:node-name node)
     (network-state) node-probabilies)

    )
  (node-current-state [node])


  InputNode
  (node-probability [node])
  (node-current-state [node])

  )

(def node1 (InputNode. "A" [{:m 0.5} {:f 0.5}]))
(def node2 (Node. "B" #{node1}))
(def node3 (Node. "C" #{node1 node2}
