(ns task6
    (require [clojure.test :as test])
)

(defn trapeziaSequence [step f]
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
    (calcSeries (trapeziaSequence step f))
)

(defn integrate [step f]
    (fn [x] (nth (integralSeries step f) (/ x step)))
)

(def square (integrate 0.01 (fn [x] (* x 2.))))

(println (time (square 20)))
