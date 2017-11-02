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

(defn calcSeries [seq]
    (->>
        (iterate 
            (fn [pair]
                (let [sum (first pair)
                    tail (last pair)]
                    (list
                        (+ sum (first tail))
                        (rest tail)
                    )
                )
            )
            (list 0 seq)
        )
        (map (fn [pair] (first pair)))
    )
)

(defn integralSeries [step f]
    (calcSeries (trapeziaSeries step f))
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

(println (time (getIntegralResult 0.01 (fn [x] (* x 2.)) 20)))
(println (time (getIntegralResult' 0.01 (fn [x] (* x 2.)) 20)))