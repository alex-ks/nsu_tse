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
    (if (->!? expr)
        false
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
        (test/is (thrown? AssertionError (or! (var! :x) 2)))))

(test/run-tests 'task7)
                
