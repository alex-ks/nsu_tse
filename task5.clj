(ns task5
    (require [clojure.test :as test])    
)

(defn integrateAndCalc [nextIntegrate step f x]
    (if (< (- x step) 0)
        (/ (* (+ (f x) (f 0)) step) 2.)
        (+
            (/
                (*
                    (+
                        (f x)
                        (f (- x step))
                    )
                    step
                )
                2.0
            )
            (nextIntegrate nextIntegrate step f (- x step))
        )
    )
)

(defn integrate [step f] 
    (def mI (memoize integrateAndCalc))
    (partial mI mI step f)
)

(defn f [x] (* 2 x))

(println (integrateAndCalc integrateAndCalc 0.01 f 6))

(def fi (integrate 0.01 f))

(time 
    (->>
        (take 10 (range))
;        (reverse)
        (map fi)
        (reduce +)
    )
)