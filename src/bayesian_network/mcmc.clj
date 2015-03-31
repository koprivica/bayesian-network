(ns bayesian-network.mcmc)
(require '(incanter core stats charts io) '[clojure.math.numeric-tower :as numeric-tower])



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

(defn mcmc-samling
  "Returns an array with MCMC samples from given data.
  Arguments are as follows:

  prior-fun : Function that generates Theta samples from prior distribution
  - Returns Theta -> sample from prior distribution
  (Currently we are using Incanter library)

  likelihood-fun : (This argument is only pasted to algorithm)
  Function that generates samples from likelihood distribution based on
  Current Theta and Proposed Theta.

  algorithm : Function that represent MCMC algorithm.
     - Takes as arguments Likelihood Function, Current Theta and Proposed Theta.
     - Returns double

  num-samples : Number of required samples "

  [prior-fun likelihood-fun algorithm num-samples]

  (loop [i 1
         cur-theta (prior-fun 1)        ; sampling onece, this can be written better
         prop-theta (prior-fun 1)
         res-samples []]

    (if (> i num-samples)
      res-samples
      ;"Theta test. If algorithm returns 1 Proposed Theta is selected,
      ;              else Current Theta is selected"
      (if (= 1 (algorithm likelihood-fun cur-theta prop-theta))
        (recur (inc i)
               prop-theta
               (prior-fun 1)
               (conj res-samples prop-theta))
        (recur (inc i)
               cur-theta
               (prior-fun 1)
               (conj res-samples cur-theta))
        ))))


(defn metropolis-hastings
  "Metropolis-Hastings Algorithm for MCMC"
  [likelihood-fun cur-theta prop-theta]


  (if (< (rand)
         (min
          1
           (/ (likelihood-fun prop-theta) (likelihood-fun cur-theta))))
    1
    0))


(defn gibbs-sampling
  "Gibbs sampling algorithm for bivariate MCMC.
  Arguments are as follows:
  start-1 : Theta-1 starting value
  start-2 : Theta-2 starting value
  likelihood-fin : funcion that return sample from marginal functions of boath Thetas
  num-samples : number of samples required
  num-thin : number of warming loops before sample is selected"
  [start-1 start-2 likelihood-fun num-samples num-thin]
  (loop [i 1          ;number of samples loops
         Theta-1 start-1        ; Theta number must be perserved for next sample loop so will be extracted from inner loop result
         Theta-2 start-2
         res-samples []]
    (if (> i num-samples)
      res-samples
      (let [thin-result (loop [j 1
                               Prop-Theta-1 Theta-1
                               Prop-Theta-2 Theta-2]
                                (if (> j num-thin)
                                  [Prop-Theta-1 Prop-Theta-2] ;if thin loop is ended -> return array with theta results
                                  (let [new-Prop-Theta-1 (likelihood-fun nil Prop-Theta-2)
                                        new-Prop-Theta-2 (likelihood-fun new-Prop-Theta-1 nil)]
                                    (recur (inc j) new-Prop-Theta-1 new-Prop-Theta-2))))]
        (recur (inc i) (thin-result 0) (thin-result 1) (conj res-samples thin-result))))))



(defn likelihood-example1              ;univariate example
  "g(Theta|y) = 0.8 * e^(-1/2 * Theta^2) + 0.2 * 1/2 e^(-1/(2*2^2) * (Theta-3)^2) "
  [Theta]
  (+ (* 0.8 (numeric-tower/expt Math/E (* (- (/ 1 2)) (numeric-tower/expt Theta 2))))
     (* 0.1 (numeric-tower/expt Math/E (* (- (/ 1 (* 2 (numeric-tower/expt 2 2)))) (numeric-tower/expt (- Theta 3) 2))))))


(defn likelihood-example2              ; bivariate example
  "g(Theta-1,Theta-2) ~ e^(-1/(2(1-0.9^2) * (Theta-1^2 - 2*0.9*Theta-1*Theta-2+Theta-2^2) "
  [[Theta-1 Theta-2]]
  (numeric-tower/expt Math/E (* (- (/ 1 (* 2 (- 1 (numeric-tower/expt 0.9 2))))) (+ (numeric-tower/expt Theta-1 2) (- (* 2 0.9 Theta-1 Theta-2)) (numeric-tower/expt Theta-2 2)))))

(defn likelihood-example-gibbs1
  "Theta1 = Gamma ->  shape 3, rate 1/(4+Theta2^2)
  Theta2 = Normal -> mean 1/(1+Theta1), sd 1/sqrt(1+Theta1)"
   [Theta-1 Theta-2]
   (if (nil? Theta-1)
     (incanter.stats/sample-gamma 1 :shape 3 :rate (/ 1 (+ 1 (numeric-tower/expt Theta-2 2))))
     (incanter.stats/sample-normal 1 :mean (/ 1 (+ 1 Theta-1)) :sd (/ 1 (numeric-tower/sqrt (+ 1 Theta-1))))))




(defn prior-example-uni
  "univariate normal distribution example"
  [n]
  (incanter.stats/sample-normal n))


(defn prior-example-mult
  "bivariate normal distribution example"
  [n]
  (let [dist (incanter.stats/sample-mvn n :sigma (incanter.core/identity-matrix 2))]
        (vector (incanter.core/sel dist :cols 0 :rows 0) (incanter.core/sel dist :cols 1 :rows 0))))
