(ns symdiff.core)

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2] (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2] ['+ a1 a2])

(defn make-product [m1 m2] ['* m1 m2])

(defn symbol-eq? [s1 s2] (= (resolve s1) (resolve s2)))

(defn sum? [x] (and (list? x) (symbol-eq? (first x) '+)))

(defn addend [s] (get s 1))

(defn augend [s] (get s 2))

(defn product? [x] (and (list? x) (symbol-eq? (first x) '*)))

(defn multiplier [p] (get p 1))

(defn multiplicand [p] (get p 2))

(defn deriv [exp var]
  (let [exp (into [] exp)]
    (cond
      (number? exp) 0
      (variable? exp)
        (if (same-variable? exp var) 1 0)
      (sum? exp)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var))
      (product? exp)
        (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
      :else (throw (ex-info "unknown expression type -- DERIV " {:expression exp})))))
