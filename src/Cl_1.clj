(defn getStr
  ([array n] (filter #(not= % nil) (getStr array [] n)))
  ([array result n]
   (flatten (map
              (fn [item]
                (when (not= item (peek result))
                  (if (= (dec n) 0)
                    (clojure.string/join "" (conj result item))
                    (getStr array (conj result item) (dec n))
                    )
                  )
                )
              (clojure.string/split (clojure.string/join ", " array) #", "))
            )
   )
  )

(println (getStr [\a {} 5] 3))
(println (getStr [\a \b \c \d] 2))