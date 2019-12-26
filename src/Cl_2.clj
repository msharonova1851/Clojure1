(defn func [x] (* x x x))

(defn sum
  ([f step] sum f step 0 step)
  ([f x acc step]
   (let [x (* (/ x step) step)
     x_acc (*(/ (+ (f x) (f (- x step))) 2) step)]
    (lazy-seq (cons (+ acc x_acc) (sum f (+ x step) (+ acc x_acc) step)
                       )))))

(defn integral [f step]
  (let [sum_seq (sum f step)]
    (fn [x] (nth sum_seq (dec (Math/floor (/ x step))) step))
    ))

(println 20)
(println (time ((integral func 1) 1000)))

(println 20)
(println (time ((integral func 1) 1000)))

(println 20)
(println (time ((integral func 1) 999)))

(println 20)
(println (time ((integral func 1) 1001)))