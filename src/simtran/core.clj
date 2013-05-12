(ns simtran.core
  (:use [simtran rk4v db])
  (:import [java.util.concurrent Executors TimeUnit])
  (:gen-class))

(def A (atom ()))
(def B (atom ()))
(def U (atom 0.0))
(def X (atom [0.0]))

(defn dot-product [X1 X2]
  (let [X_ (map * X1 X2)]
    (reduce + X_)))

(defn build-nth-dX [idx] ;; idx [1,n]
  (if (< idx (dec (count @A))) 
    (fn [X]
      (nth X idx))
    (fn [X]
      (+ (* -1.0 (dot-product (reverse (rest @A)) X)) 
         @U))))

(defn step [X]
  (rk4V (map build-nth-dX (rest (range (count @A)))) 
        1.0 
        X))

(defn build-nth-coff [idx] ;; idx [1,n]
  (- (nth @B idx) (* (nth @A idx) (first @B))))

(def scheduler (Executors/newScheduledThreadPool 1))

(def simu-thread 
  (proxy [Runnable] []
    (run []
      (let [u (read!)
            _ (reset! U u)
            X+ (step @X)
            _ (println "X+ = " X+)
            K (map build-nth-coff (reverse (rest (range (count @A)))))
            _ (println "K=" K)
            c (+ (dot-product K X+)
                 (* (first @B) u))]
        (do 
          (reset! X X+)
          (write! u c)
          )))))

(defn -main [& args]
  (if (connect!)
    (if (not= (count args) 2)
      (println "usage: java -jar simtran.jar [1] [1,2,1]")
      (let [num (read-string (first args))
            den (read-string (second args))
            n (count den)
            num+ (reverse (take n (concat (reverse num) (repeat 0))))
            f #(/ % (double (first den)))
            a (map f num+)  ;; a0 == 1.0
            b (map f den)]
        
        (do
          (reset! A a)
          (reset! B b)
          (println a)
          (println b)
          (println "simtran started ...")
          (.scheduleAtFixedRate scheduler
                                simu-thread
                                1000
                                1000
                                TimeUnit/MILLISECONDS))))
    (println "can't connect to database ...")))

