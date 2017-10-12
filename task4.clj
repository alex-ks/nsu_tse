(ns task4)

(defn task4
    [alphabet, n]
    (reduce 
        (fn [accum current]
            (mapcat
                (fn [s]
                    (map 
                        (fn [c] (str s c))
                        (filter 
                            (fn [c] (not= c (last s)))
                            alphabet)))
                accum))
        (map (fn [c] (.toString c)) alphabet)
        (take (- n 1) (range))))

(println (task4 '(\a \b \c) 3))