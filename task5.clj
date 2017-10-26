(ns task5
    (require [clojure.test :as test])    
)

(defn integrateAndCalc [nextIntegrate step gridNth f x]
    (if (< x step)
        (/ (* (+ (f x) (f 0)) x) 2.)
        (let [n (/ x step)
              prev (gridNth (- n 1))]
            (+
                (/ (* (+ (f x) (f prev)) step) 2.)
                (nextIntegrate nextIntegrate step gridNth f prev)
            )
        )
    )
)

(defn integrate [step f] 
    (def grid (map (fn [x] (* x step)) (range)))
    (def gridNth (memoize (partial nth grid)))
    (def mI (memoize integrateAndCalc))
    (partial mI mI step gridNth f)
)

(defn f [x] (* 2 x))

(def fi (integrate 0.001 f))

; warming-up
(->>
    (take 100 (range))
    (map #(/ % 10.))
    (map fi)
    (reduce +)
)

(println (fi 6))

(time (fi 10))