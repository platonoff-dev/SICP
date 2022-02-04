(ns clj-sicp.newton
  (:gen-class))

(import java.lang.Math)

(defn average [x y]
  (/ (+ x y) 2))

(defn sqr [x]
  (* x x))

(defn good-enough? [guess x]
  (< (Math/abs (- (sqr guess) x)) 0.0001))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-newton [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-newton (improve guess x) x)))

(defn sqrt [x]
  (sqrt-newton 1.0 x))

(defn -main [& args]
  (println (sqrt 4)))
