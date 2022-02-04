(defn accumulate [f init-value collection]
  (if (empty? collection)
    init-value
    (f (first collection) (accumulate f init-value (rest collection)))))

(defn accumulate-n [f init collection]
  (if (empty? (first collection))
    []
    (cons 
     (accumulate f init (map first collection))
     (accumulate-n f init (map rest collection)))))

(print (accumulate-n + 0 [[1, 2], [3, 4], [5, 6]]))
