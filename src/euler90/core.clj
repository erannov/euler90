(ns euler90.core
  (:gen-class))

(def squares
  (list '(0 1) '(0 4) '(0 9) '(1 6) '(2 5) '(3 6) '(4 9) '(6 4) '(8 1)))

(def digits (range 0 10))

(defn eq
  [digit]
  (if (or (= digit 6)(= digit 9))
    (fn[y](or (= y 6)(= y 9)))
    (fn[y](= digit y))))

(defn add-to-lists
  "adds element to list of lists"
  [element lst]
  (map
    (fn [x] (cons element x))
    lst))

(defn lister
  [coll n]
  (if (= n 1)
    (map (fn [x] (list x)) coll)
    (list coll)))

(defn combs-aranger
  [coll n]
  (map
    (fn [n] (list (nth coll n) (nthrest coll (inc n))))
    (range 0 (inc (- (count coll) n)))))

(defn combinations
  [coll n]
  (if (or (= 1 n) (= n (count coll)))
    (lister coll n )
    (reduce
      (fn[prev,x](concat
                   prev
                   (add-to-lists
                     (first x)
                     (combinations (second x) (- n 1)))))
      '()
      (combs-aranger coll n))))

(defn valid-cubes?
  [cubes]
  (defn validate
    [cubes]
    (reduce
      (fn [a b] (and a b))
      (map
        (fn[sq](or
                 (and (some (eq (first sq)) (first cubes)) (some (eq (second sq)) (second cubes)))
                 (and (some (eq (first sq)) (second cubes)) (some (eq (second sq)) (first cubes)))))
        squares )))
  (if (validate cubes)
    true
    false))


(defn count-valid-cubes
  [cube-pairs]
  (count
    (filter
      valid-cubes?
      cube-pairs)))


(defn make-pairs
  [e1 l2]
  (map
    (fn [x] (list e1 x))
    l2))


(defn valids-counter
  []
  (defn iter
    [lst res]
    (if (empty? lst)
      res
      (iter (rest lst)
            (+
             res
             (count-valid-cubes (make-pairs (first lst)(rest lst)))))))
  (iter (combinations digits 6) 0))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (time(valids-counter))))

