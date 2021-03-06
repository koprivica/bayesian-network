(ns bayesian-network.core
  (:require ; [bayesian-network.node-protocol :refer [->Node ->InputNode]]
            [bayesian-network.node-protocol :refer
              :all]   ;SUPER chatch, dealing with import record problem
  ; [bayesian-network.node-protocol]
            )
  (gen-class)
 ; (:import [bayesian-network.node-protocol Node] [bayesian-network.node-protocol InputNode])
  )

 ;  calculating probability function
 (defn network-probability [network network-state]
   (loop [nodes network product 1.0]
      (if (next nodes)
         (recur (next nodes)
                (* product (node-probability (first nodes) network-state)))
       product
         )))



 ; EXAMPLE
 ; defining 3 nodes

(def node1 (->InputNode "A" {:m 0.5 :f 0.5})) ; (->Record) is catch that i found on net, textbook examples are not applicable here
(def node2 (->Node "B" #{"A"} {#{:m :d} 0.75 #{:m :nd} 0.25 #{:f :d} 0.25 #{:f :nd} 0.75}))
(def node3 (->Node "C" #{"A" "B"} {#{:m :d :r} 0.2, #{:m :d :nr} 0.8, #{:m :nd :r} 0.3, #{:m :nd :nr} 0.7, #{:f :d :r} 0.2, #{:f :d :nr} 0.8, #{:f :nd :r} 0.3, #{:f :nd :nr} 0.7}))

  ; define network
(def bayes-network #{node1 node2 node3})

  ; concreate state of network: male, drugged, recovered
(def some-state {"A" :m "B" :d "C" :r})

 ;  calculating probability for given (bayes) network and given state (some-state)
(def probability1 (network-probability bayes-network some-state))
