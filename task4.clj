(ns task4)

(defn task4
    [alphabet, n]
    (reduce 
        (fn [accum current]
            (mapcat
                (fn [s]
                    (->>
                        (filter (fn [c] (not= c (last s))) alphabet)
                        (map (fn [c] (str s c)))
                    )
                )
                accum
            )
        )
        (map (fn [c] (.toString c)) alphabet)
        (take (- n 1) (range))
    )
)

(println 
    (task4 '(\a \b \c) 3)
)