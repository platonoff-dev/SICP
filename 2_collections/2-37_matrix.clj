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

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [line] (dot-product line v)) m))

(defn transpose [m]
  (accumulate-n cons [] m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [line] (do (println cols) (println line) (matrix-*-vector cols line))) m)))

(println (matrix-*-matrix [[1 2] [3 4]] [[5 6] [7 8]]))
