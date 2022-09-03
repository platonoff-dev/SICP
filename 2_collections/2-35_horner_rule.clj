;; 2.35

(defn accumulate [f init-value collection]
  (if (empty? collection)
    init-value
    (f (first collection) (accumulate f init-value (rest collection)))))


(defn horner-eval [x coefficients] 
  (accumulate (fn [this-coeff higher-term] (+ (* higher-term x) this-coeff))
          0
          coefficients))
  
(print (horner-eval 2 [1, 3, 0, 5, 0, 1]))
