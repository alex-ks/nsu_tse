(ns task7
    (require [clojure.test :as test]))

(defn third [lst]
    (nth lst 2))

(defn const! [val]
    {:pre [(or (= val true) (= val false))]}
    (list ::const val))

(defn const!? [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::const))

(defn var! [name]
    {:pre [(keyword? name)]}
    (list ::var name))

(defn var!? [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::var))

(defn abstract-not [expr-check expr]
    {:pre [(expr-check expr)]}
    (list ::not expr))

(defn not!? [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::not))

(defn abstract-binary [symbol expr-check expr1 expr2]
    {:pre [(keyword? symbol)
           (expr-check expr1)
           (expr-check expr2)]}
    (list symbol expr1 expr2))

(defn abstract-binary? [symbol expr]
    {:pre [(keyword? symbol)
           (list? expr)]}
    (= (first expr) symbol))

(def or!? (partial abstract-binary? ::or))
(def and!? (partial abstract-binary? ::and))
(def ->!? (partial abstract-binary? ::->))

(defn expr? [expr]
    (cond
        (const!? expr) true
        (var!? expr) true
        (not!? expr) (expr? (second expr))
        (or (or!? expr) (and!? expr) (->!? expr)) (and (expr? (second expr)) (expr? (third expr)))
        :else false))

(def not! (partial abstract-not expr?))
(def or! (partial abstract-binary ::or expr?))
(def and! (partial abstract-binary ::and expr?))
(def ->! (partial abstract-binary ::-> expr?))

(defn normalize-impl [expr]
    {:pre [(expr? expr)]}
    (cond 
        (->!? expr)
            (or! 
                (not! (normalize-impl (second expr)))
                (normalize-impl (third expr)))
        (or 
            (and!? expr)
            (or!? expr))
            (list
                (first (expr))
                (normalize-impl (second expr))
                (normalize-impl (third expr)))
        (not!? expr)
            (not! (normalize-impl (second expr)))
        :else
            expr))

(defn impl-normalized? [expr]
    {:pre [(expr? expr)]}
    (cond 
        (->!? expr)
            false
        (or (const!? expr) (var!? expr))
            true
        :else
            (every? impl-normalized? (rest expr))))

(defn normalize-de-morgan [expr]
    {:pre [(impl-normalized? expr)]}
    (cond 
        (not!? expr)
            (let [inner (second expr)]
                (cond
                    (and!? inner)
                        (or! 
                            (not! (normalize-de-morgan (second inner)))
                            (not! (normalize-de-morgan (third inner))))
                    (or!? inner)
                        (and!
                            (not! (normalize-de-morgan (second inner)))
                            (not! (normalize-de-morgan (third inner))))
                    (not!? inner)
                        (normalize-de-morgan (second inner)) ; remove double not by chance
                    :else ; constant or var, normalization has no sense
                        expr))
        (or (var!? expr) (const!? expr))
            expr
        :else
            (cons
                (first expr)
                (map normalize-de-morgan (rest expr)))))

(defn unsafe-normalize-distribution [inner-check inner-op outer-op next-normalize expr]
    (let [fst (second expr)
          snd (third expr)]
        (cond
            (inner-check snd)
                (inner-op (outer-op fst (second snd)) (outer-op fst (third snd)))
            (inner-check fst)
                (inner-op (outer-op (second fst) snd) (outer-op (third fst) snd))
            :else
                (outer-op (next-normalize fst) (next-normalize snd))))
)

(defn normalize-distribution [expr]
    {:pre [(impl-normalized? expr)]}
    (cond
        (or (var!? expr) (const!? expr))
            expr
        (and!? expr)
            (unsafe-normalize-distribution or!? or! and! normalize-distribution expr)
        (or!? expr)
            (unsafe-normalize-distribution and!? and! or! normalize-distribution expr)
        :else
            (cons
                (first expr)
                (map normalize-distribution (rest expr)))))

(defn unsafe-absorption [inner-check inner-op outer-op next-normalize expr]
    (let [fst (second expr)
          snd (third expr)]
        (cond
            (inner-check snd)
                (if (or (= fst (second snd)) (= fst (third snd)))
                    (next-normalize fst)
                    (outer-op (next-normalize fst) (next-normalize snd)))
            (inner-check fst)
                (if (or (= snd (second fst)) (= fst (third fst)))
                    (next-normalize snd)
                    (outer-op (next-normalize fst) (next-normalize snd)))
            :else
                (outer-op (next-normalize fst) (next-normalize snd)))))

(defn normalize-absorption [expr]
    {:pre [(impl-normalized? expr)]}
    (cond
        (or (var!? expr) (const!? expr))
            expr
        (and!? expr)
            (unsafe-absorption or!? or! and! normalize-absorption expr)
        (or!? expr)
            (unsafe-absorption and!? and! or! normalize-absorption expr)
        :else
            (cons
                (first expr)
                (map normalize-absorption (rest expr)))))

(test/deftest task7-test
    (test/testing "Testing task 7"
        (test/is 
            (let [x (var! :x)]
                (var!? x)))
        (test/is
            (not (const!? (var! :x))))
        (test/is
            (let [expr (and! (or! (var! :x) (var! :y)) (->! (var! :x) (not! (var! :y))))]
                (expr? expr)))
        (test/is (thrown? AssertionError (or! (var! :x) 2)))
        (test/is 
            (= (const! true) (const! true)))
        (test/is 
            (let [ea (and! (or! (var! :x) (const! true)) (var! :y))
                  eb (and! (or! (var! :x) (const! true)) (var! :y))]
                (= ea eb)))
        (test/is
            (let [expr (and! (or! (var! :x) (const! true)) (var! :y))
                  result (or! (and! (var! :x) (var! :y)) (and! (const! true) (var! :y)))]
                (= (normalize-distribution expr) result)))
        (test/is
            (let [expr (or! (and! (var! :x) (const! true)) (var! :y))
                  result (and! (or! (var! :x) (var! :y)) (or! (const! true) (var! :y)))]
                (= (normalize-distribution expr) result)))
        (test/is
            (let [expr (or! (or! (var! :x) (const! true)) (var! :y))]
                (= (normalize-distribution expr) expr)))
        (test/is
            (let [expr (or! (and! (var! :x) (const! true)) (var! :x))
                  result (var! :x)]
                (= (normalize-absorption expr) result)))
        (test/is
            (let [expr (or! (or! (var! :x) (const! true)) (var! :x))]
                (= (normalize-absorption expr) expr)))
    ))

(test/run-tests 'task7)
                
