(defn expr_with_type [type exprs]
  (cons type exprs))

;; val is true or false
(defn expr_const [val]
  {:pre [(or (true? val) (false? val))]}
  (list :const val))

;; name is keyword
(defn expr_var [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn expr_or [& exprs]
  (let [size (count exprs)]
    (if (> size 0)
      (if (= size 1)
        (first exprs)
        (cons :or exprs)))))

(defn expr_and [& exprs]
  (let [size (count exprs)]
    (if (> size 0)
      (if (= size 1)
        (first exprs)
        (cons :and exprs)))))

;; EL -> ER
(defn expr_impl [el er]
  (list :impl el er))

(defn expr_not [expr]
  (list :not expr))



(defn constant_value [expr]
  (second expr))

(defn variable_name [expr]
  (second expr))

(defn expr_type [expr]
  (first expr))

(defn expr_args [expr]
  (if (or (= (expr_type expr) :var)
          (= (expr_type expr) :const))
    (list expr)
    (rest expr)))

(defn expr_arg_nth [expr n]
  (nth expr n))

(defn variable? [expr]
  (= (first expr) :var))

(defn constant? [expr]
  (= (first expr) :const))

(defn same_variables? [vl vr]
  (and
    (variable? vl)
    (variable? vr)
    (= (variable_name vl)
       (variable_name vr))))

(defn expr_or? [expr]
  (= (first expr) :or))

(defn expr_and? [expr]
  (= (first expr) :and))

(defn expr_impl? [expr]
  (= (first expr) :impl))

(defn expr_not? [expr]
  (= (first expr) :not))

(defn atom? [expr]
    (variable? expr))

(defn literal? [expr]
  (or
    (atom? expr)
    (and (expr_not? expr)
         (atom? (expr_arg_nth expr 1)))))

(defn consist_of_literal? [expr]
  (every? literal? (expr_args expr)))

(defn elementary_conjuction? [expr]
  (or (literal? expr) (and
                        (expr_and? expr)
                        (consist_of_literal? expr)
                        (= (expr_args expr) (distinct (expr_args expr))))))

(defn dnf? [expr]
  (or
    (elementary_conjuction? expr)
    (and
      (expr_or? expr)
      (every? elementary_conjuction? (expr_args expr)))))



(defn replace_implications [expr]
  (if (expr_impl? expr)
    (expr_or
      (expr_not (replace_implications (expr_arg_nth expr 1)))
      (replace_implications (expr_arg_nth expr 2)))
    (if (atom? expr)
      expr
      (expr_with_type (expr_type expr) (map replace_implications (expr_args expr))))))

(defmulti replace_nots
          (fn [expr]
            (if (not (expr_not? expr))
              :default
              (expr_type (expr_arg_nth expr 1)))))

(defmethod replace_nots :default [expr]
  (if (atom? expr)
    expr
    (expr_with_type (expr_type expr) (map replace_nots (expr_args expr)))))

(defmethod replace_nots :const [expr]
  (expr_const (not (constant_value expr))))

(defmethod replace_nots :var [expr]
  expr)

(defmethod replace_nots :not [expr]
  (replace_nots (expr_arg_nth (expr_arg_nth expr 1) 1)))

(defmethod replace_nots :or [expr]
  (let [not_arg (expr_arg_nth expr 1)]
    (apply expr_and
           (map
             (fn [sub_expr]
               (replace_nots (expr_not sub_expr)))
             (expr_args not_arg)))))

(defmethod replace_nots :and [expr]
  (let [not_arg (expr_arg_nth expr 1)]
    (apply expr_or
           (map
             (fn [sub_expr]
               (replace_nots (expr_not sub_expr)))
             (expr_args not_arg)))))

(defn distribution [expr]
  (if (literal? expr)
    expr
    (if (expr_and? expr)
      (let [disjunctions (filter (fn [x]
                                   (expr_or? x))
                                 (expr_args expr))
            not_disjunctions (filter
                               (fn [x]
                                 (not (expr_or? x)))
                               (expr_args expr))]
        (apply expr_or
               (map (fn [and_args]
                      (apply expr_and and_args))
                    (reduce (fn [ands_arg_lists disjunction]
                              (reduce (fn [acc disjunction_arg]
                                        (concat acc (map (fn [and_arg_list]
                                                           (conj and_arg_list disjunction_arg))
                                                         ands_arg_lists)))
                                      []
                                      (expr_args disjunction))
                              )
                            [not_disjunctions]
                            disjunctions)
                    )))
      (apply expr_or (map distribution (expr_args expr))))))

(defn remove_duplicates_in_ands [expr]
  (apply expr_or
         (map
           (fn [sub_expr]
             (if (expr_and? sub_expr)
               (apply expr_and (distinct (expr_args sub_expr)))
               (if (literal? expr)
                 expr
                 (remove_duplicates_in_ands sub_expr)))
             )
           (expr_args expr))))

(defn expand_nested_and [expr]
  (if (not (expr_and? expr))
    expr
    (apply expr_and (reduce (fn [args cur]
                              (if (expr_and? cur)
                                (concat args (expr_args cur))
                                (concat args (list cur))))
                            []
                            (expr_args expr)))))

(defn expand_nested_or [expr]
  (if (not (expr_or? expr))
    expr
    (apply expr_or (reduce (fn [args cur]
                             (if (expr_or? cur)
                               (concat args (expr_args cur))
                               (concat args (list cur))))
                           []
                           (expr_args expr)))))

(defn dnf [expr]
  (if (dnf? expr)
    expr
    (expand_nested_or
      (expand_nested_and
        (remove_duplicates_in_ands
          (distribution
            (replace_nots
              (replace_implications expr))))))))

(defmulti evaluate
          (fn [expr values]
            (expr_type expr)))

(defn subevaluate [expr values]
  (map
    (fn [subexpr]
      (evaluate subexpr values))
    (expr_args expr)))

(defmethod evaluate :const [expr values]
  (constant_value expr))

(defmethod evaluate :var [expr values]
  (let [var_name (variable_name expr)]
    (if (contains? values var_name)
      (get values var_name)
      (throw (Exception. (format "variable %s isn't set" var_name))))))

(defmethod evaluate :not [expr values]
  (not (evaluate (expr_arg_nth expr 1) values)))

(defmethod evaluate :or [expr values]
  (let [x (subevaluate expr values)]
    (not (not-any? true? x))))

(defmethod evaluate :and [expr values]
  (every? true? (subevaluate expr values)))

(defmethod evaluate :impl [expr values]
  (or
    (not (evaluate (expr_arg_nth expr 1) values))
    (evaluate (expr_arg_nth expr 2) values)))




(let [or_func (expr_or (expr_var :a) (expr_var :b))]
  (println "or function")
  (println (evaluate or_func {:a true :b true}))
  (println))

(println "and function")
(let [and_func (expr_and (expr_var :a) (expr_var :b))]
  (println (not (evaluate and_func {:a false :b false})))
  (println))

(println "not function")
(let [not_func (expr_not (expr_var :a))]
  (println (evaluate not_func {:a false}))
  (println (not (evaluate not_func {:a false})))
  (println))

(println "implication function")
(let [impl_func (expr_impl (expr_var :a) (expr_var :b))]
  (println (evaluate impl_func {:a true :b true}))
  (println (evaluate impl_func {:a true :b false}))
  (println))

(println "dnf function")
(println " dnf implications")
(println (dnf? (dnf (expr_impl (expr_var :a) (expr_var :b)))))
(println)

(println  "dnf nested_or")
(println (dnf? (dnf (expr_or (expr_var :x) (expr_or (expr_var :y) (expr_var :z))))))
(println)

(println  "dnf nested_and")
(println (dnf? (dnf (expr_and (expr_var :x) (expr_and (expr_var :y) (expr_var :z))))))
(println)

(println  "dnf duplicates_in_ands, example: '(a and a and b) or (not(b) and not(b) and a)'")
(println (dnf? (dnf (expr_or
                      (expr_and (expr_var :a) (expr_var :a) (expr_var :b))
                      (expr_and (expr_not (expr_var :b)) (expr_not (expr_var :b)) (expr_var :a))))))
(println)

(println  "dnf distribution, example: 'a and (b or c) = (a and b) or (a and c)'")
(println (dnf? (dnf (expr_and (expr_var :a) (expr_or (expr_var :b) (expr_var :c)) (expr_var :d)))))
(println)

(println  "dnf replace_nots")
(println (dnf? (dnf (expr_not (expr_or (expr_not (expr_var :a)) (expr_var :b))))))
(println)

(println  "dnf replace_implications")
(println (dnf? (dnf (expr_impl (expr_var :a) (expr_var :b)))))
(println)