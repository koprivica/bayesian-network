(ns bayesian-network.test-belief
  (:require (incanter [core :refer :all]
             [charts :refer :all]
             [stats :refer :all])))

(Comment        OVO JE SKUP SVIH MOGUCIH STANJA MREZE            )
(def GGG {:stanje :m})
(def DDD {:stanje :d})
(def RRR {:stanje :r})
(def skup [[{:stanje :m} {:stanje :d} {:stanje :r}]
          [{:Stanje :M} {:stanje :d} {:stanje :nr}]
          [{:stanje :m} {:stanje :nd} {:stanje :r}]
          [{:stanje :m} {:stanje :nd} {:stanje :nr}]
          [{:stanje :f} {:stanje :d} {:stanje :r}]
          [{:stanje :f} {:stanje :d} {:stanje :nr}]
          [{:stanje :f} {:stanje :nd} {:stanje :r}]
          [{:stanje :f} {:stanje :nd} {:stanje :nr}]
          ])

(comment        VEROVATNOCE ZA CVOR 1            )
(defn G1 [u] (let [m1 (first u)] (cond
               (=  (:stanje m1) :m) 0.5
               (= (:stanje m1) :f) 0.5
               :else 0)))

(comment        VEROVATNOCE ZA CVOR 2            )
(defn D1 [u] (let [m1 (first u) m2 (second u)]
  (cond
    (= (:stanje m1) :m)

                (cond
                           (=  (:stanje m2) :d) 0.75
                           (= (:stanje m2) :nd) 0.25
                           :else 0
                 )

  (= (:stanje m1) :f)
                (cond
                           (=  (:stanje m2) :d) 0.25
                           (= (:stanje m2) :nd) 0.75
                           :else 0
                 )

   )
  )
)

(comment        VEROVATNOCE ZA CVOR 3            )
(defn R1 [u] (let [m1 (first u) m2 (second u) m3 (nth u 2)]
  (cond
    (= (:stanje m1) :m)

                (cond
                           (=  (:stanje m2) :d)
                               (cond
                                 (=  (:stanje m3) :r) 0.6
                                 (=  (:stanje m3) :nr) 0.4
                                 :else 0
                               )

                           (= (:stanje m2) :nd)
                               (cond
                                 (=  (:stanje m3) :r) 0.7
                                 (=  (:stanje m3) :nr) 0.3
                                 :else 0
                               )
                  :else 0
                 )

  (= (:stanje m1) :f)
                (cond
                           (=  (:stanje m2) :d)
                               (cond
                                 (=  (:stanje m3) :r) 0.2
                                 (=  (:stanje m3) :nr) 0.8
                                 :else 0
                               )

                           (= (:stanje m2) :nd)
                               (cond
                                 (=  (:stanje m3) :r) 0.3
                                 (=  (:stanje m3) :nr) 0.7
                                 :else 0
                               )
                  :else 0
                 )

   )
  )
)

(comment        RACUNANJE VEROVATNOCE DA MREZA BUDE U NEKOM STANJU MNOZENJEM VEROVATNOCA CVOROVA           )
(defn Belif [s]
  (if (keyword? (first (first (first (seq s)))))

        (* (G1 (seq s)) (D1 (seq s)) (R1 (seq s)))

        (doseq [x (seq s)]
          (* (G1 (seq x)) (D1 (seq x)) (R1 (seq x)))
          )
  )
)

(comment        OVO JE U TESTIRANJU JER POKUSAVAM DA NACRTAM GRAFIK – NE RADI )
;(defn draw-histogram [s]
 ; (

 ;   view (histogram (sample-normal s))
;  )
; )

;(defn draw-pie [rasp]
 ; (view (pie-chart (vec (keys (rasp skup))) (vec (vals (rasp skup)))))
;)

(comment        RACUNANJE RASPODELE VEROVATNOCA MREZE  - STARO      )
 (defn raspodela1 [sekvenca]
                 (loop [my-map {}
                        sek sekvenca]
                   (if (and sek sek)
                     (recur (assoc my-map (vec (first sek)) (Belif (first sek)))
                            (next sek))
                     my-map)))

 (comment        RACUNANJE RASPODELE VEROVATNOCA MREZE        )
 (defn raspodela [sekvenca]
                                (loop [my-map {}
                                       sek sekvenca]
                                  (if (and sek sek)
                                    (recur (assoc my-map ((fn ([sekvenca-mapi] (loop [moj-vek [] mape sekvenca-mapi]
                                                                                (if (and mape mape)
                                                                                  (recur (conj moj-vek (vals (first mape))) (next mape))
                                                                                  moj-vek))))  (first sek)) (Belif (first sek)))
                                           (next sek))
                                    my-map)))
