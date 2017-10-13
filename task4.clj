(ns task4
    (require [clojure.test :as test])    
)

(defn task4
    [alphabet, n]
    (reduce 
        (fn [accum current]
            (mapcat
                (fn [s]
                    (->>
                        (filter 
                            (fn [c] (not= c (last s))) 
                            alphabet
                        )
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
    (task4 '(\a \b \c \d) 1)
)

(test/deftest task4-test
    (test/testing "Testing task 4"
        (test/is 
            (= 
                (task4 '(\a \b \c) 3)
                '("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")
            )
        )
        (test/is
            (=
                (task4 '() 10)
                '()
            )
        )
        (test/is
            (let 
                [alphabet '(\a \b \c \d)]
                (=
                    (task4 alphabet 1)
                    (map (fn [c] (.toString c)) alphabet)    
                )
            )
        )
    )
)

(test/run-tests 'task4)