(ns bayesian-network.novi-test
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
    ((conj       ; merging node name with names of his parents, result set is key for node-probabilities map - > value of that map is probability
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

(def node1 (InputNode. "A" {:m 0.5 :f 0.5}))
(def node2 (Node. "B" #{"A"} {#{:m :d} 0.75 #{:m :nd} 0.25 #{:f :d} 0.25 #{:f :nd} 0.75}))
(def node3 (Node. "C" #{"A" "B"} {#{:m :d :r} 0.2, #{:m :d :nr} 0.8, #{:m :nd :r} 0.3, #{:m :nd :nr} 0.7, #{:f :d :r} 0.2, #{:f :d :nr} 0.8, #{:f :nd :r} 0.3, #{:f :nd :nr} 0.7}))
