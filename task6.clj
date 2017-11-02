(ns task6
    (require [clojure.test :as test])
)

(defn trapeziaSeries [step f]
    (->>
        (range)
        (map (fn [i] (* i step)))
        (partition 2 1)
        (map (fn [pair] (+ (f (first pair)) (f (last pair)))))
        (map (fn [val] (* val step)))
        (map (fn [val] (/ val 2.)))
    )
)

(defn integralSeries [step f]
    (let [trapezia (trapeziaSeries step f)]
        (->>
            (range)
            (map 
                (fn [i] 
                    (->>
                        (take i trapezia)
                        (reduce +)
                    )
                )
            )
        )    
    )
)

(defn getIntegralResult [step f x]
    (->> 
        (trapeziaSeries step f)
        (take (/ x step))
        (reduce +)
    )
)

(defn integrate [step f]
    (partial getIntegralResult step f)
)

(defn getIntegralResult' [step f x]
    (nth (integralSeries step f) (/ x step))
)

(defn integrate' [step f]
    (partial getIntegralResult' step f)
)

(time (getIntegralResult 0.01 (fn [x] (* x 2.)) 9))
(time (getIntegralResult' 0.01 (fn [x] (* x 2.)) 9))