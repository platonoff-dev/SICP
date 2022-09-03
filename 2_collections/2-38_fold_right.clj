(defn fold-right [f initial collection]
  (defn iter [result collection]
    (if (empty? collection)
      result
      (iter (f result (first collection)) (rest collection))))
  (iter initial collection))

(defn fold-left [f init-value collection]
  (if (empty? collection)
    init-value
    (f (first collection) (fold-left f init-value (rest collection)))))

(println (fold-right list nil [1 2 3]))
(println (fold-left list nil [1 2 3]))