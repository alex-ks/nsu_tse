(ns task5
    (require [clojure.test :as test])    
)

(defn integrateAndCalc [step f x]
    (->>
        (take (/ x step) (range))
        (map (fn [i] (* i step)))
        (partition 2 1)
        (map 
            (fn [pair] 
                (/ 
                    (* 
                        (+ (f (first pair)) (f (last pair))) 
                        step
                    ) 
                2.)
            )
        )
        (reduce +)
    )    
)

(defn integrate [step f] 
    (def fm (memoize f))
    (partial integrateAndCalc step fm)
)

(defn f [x] (* 2 x))

(def fi (integrate 0.001 f))

(time 
    (->>
        (take 40 (range))
        (reverse)
        (map fi)
        (reduce +)
    )
)