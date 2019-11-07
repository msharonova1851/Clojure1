(defn getStr [array result n]
  (if (= 0 n)
    result
    (recur array (mapcat
                   (fn [prev]
                     (map (fn [l] (cons l prev)) (filter (fn [x] (not= x (first prev))) array)))
                   result
                   ) (dec n))))

(println (getStr `(:a :b :C) `(()) 3))