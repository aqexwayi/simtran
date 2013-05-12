(ns simtran.rk4v)

(defmacro addV [& args]
  `(mapv + ~@args))

(defmacro subV [& args]
  `(mapv - ~@args))

(defn mulSV [s V]
  (map #(* s %) V))

(defn applyV [F X]
  (map #(% X) F))

(defn rk4V [dXdt dt X]
  (let [X1 X
        D1 (applyV dXdt X1)
        X2 (addV X (mulSV (/ dt 2.0) D1))
        D2 (applyV dXdt X2)
        X3 (addV X (mulSV (/ dt 2.0) D2))
        D3 (applyV dXdt X3)
        X4 (addV X (mulSV dt D3))
        D4 (applyV dXdt X4)
        X+ (addV X (mulSV (/ dt 6.0) (addV D1 D2 D2 D3 D3 D4)))
        ]
    X+))
