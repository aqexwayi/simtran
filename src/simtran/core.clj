(ns simtran.core
  (:use [simtran rk4v db])
  (:import [java.util.concurrent Executors TimeUnit])
  (:gen-class))

(def A (atom [1.0 0.2 0.01])) ;; n=2
(def B (atom [0.0 0.0 0.01]))
(def U (atom 1.0))
(def X (atom [0.0 0.0]))

(defn dot-product [X1 X2]
  (let [X_ (map * X1 X2)]
    (reduce + X_)))

(defn build-nth-dX [idx] ;; idx=[1..n]
  (if (< idx (dec (count @A))) 
    (fn [X]
      (nth X idx))
    (fn [X]
      (+ (* -1.0 (dot-product (reverse (rest @A)) X)) 
         @U))))

(defn build-nth-coff [idx] ;; idx=[n..1]
  (- (nth @B idx) (* (nth @A idx) (first @B))))

(defn step []
  (let [lst (rest (range (count @A))) ;; [1..n]
        X+ (rk4V (map build-nth-dX lst) 1.0 @X)
        K (map build-nth-coff (reverse lst))
        _ (reset! X X+)
        c (+ (dot-product K X+) (* (first @B) @U))
        ]
    c))

;; zeta=1.0 omega=0.1
;; [a0 a1 a2]=[1.0 0.2 0.01]
;; [b0 b1 b2]=[0.0 0.0 0.01]
;; u=1.0
;; X=[0.0 0.0]
(defn step2 []
  (let [f1 second
        f2 (fn [[x1 x2]]
             (+ (* -0.01 x1) (* -0.2 x2) @U))
        X+ (rk4V [f1 f2] 1.0 @X)
        _ (reset! X X+)
        c (* 0.01 (first X+))]
    c))

(def simu-thread 
  (proxy [Runnable] []
    (run []
      (let [u (read!)
            _ (reset! U u)
            c (step)]
        (write! u c)))))

(defn start-sim-object []
  (let [scheduler (Executors/newScheduledThreadPool 1)]
    (do
      (println "sim object started ...")
      (println "A=" @A)
      (println "B=" @B)
      (.scheduleAtFixedRate scheduler
                            simu-thread
                            1000
                            1000
                            TimeUnit/MILLISECONDS))))

(defn parse-args [args]
  (let [num (read-string (first args))
        den (read-string (second args))
        n (dec (count den))
        num+ (reverse (take (inc n) (concat (reverse num) (repeat 0))))
        f #(/ % (double (first den)))
        b (map f num+)
        a (map f den)   ;; a0 == 1.0
        ]
    [b,a]))

(defn -main [& args]
  (if (connect!)
    (case (count args)
      0 (do 
          (reset! A [1.0 0.2 0.01])
          (reset! B [0.0 0.0 0.01])
          (start-sim-object))
      2 (let [[b a] (parse-args args)]
          (do
            (reset! A a)
            (reset! B b) 
            (start-sim-object)))
      (println "usage: java -jar simtran.jar [1] [1,2,1]")
      )
    (println "can't connect to database ...")))

