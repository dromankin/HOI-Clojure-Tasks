(defn parallel-filter
  ([pred coll batch]
   (let [n 3
         batches (map doall (partition-all batch coll))
         fut #(doall (filter pred %))
         returns (map #(future (fut %)) batches)

         step (fn step [[x & xs :as vs] fs]
                (if-let [s (seq fs)]
                  (lazy-seq
                    (lazy-cat (deref x) (step xs (rest s))))
                  (apply concat  (map deref vs))
                  ))]
     (step returns (drop n returns)))))


(require '[clojure.test :refer [deftest is]])


(deftest parallel-filter-test
  (is (= [2 4 6 8 10] 
         (parallel-filter even? (range 1 11) 10)))
  
  (is (empty? (parallel-filter even? [] 10)))
  
  (is (= [0 2 4 6 8] 
         (take 5 (parallel-filter even? (range) 10))))
  
) 

(defn demonstrate-efficiency []
  (let [large-data (range 1000000)
        pred (fn [x] (Thread/sleep 0.1) (> (mod x 17) 8))] 
    
    
    (println "Serial")
    (time (doall (filter pred large-data)))
    
    (println "\nParallel (chunk-size: 1000):")
    (time (doall (parallel-filter pred large-data :chunk-size 1000)))
    
    (println "\nParallel(chunk-size: 5000):")
    (time (doall (parallel-filter pred large-data :chunk-size 5000))))
    
    (System/exit 0)
    )


(deftest ^:performance c3-timing-demo
  (let [xs  (range 1 5000000)
        p   (fn [n]
              (cond
                (< n 2) false
                (= n 2) true
                (even? n) false
                :else (let [lim (long (Math/sqrt n))]
                        (not-any? #(zero? (mod n %))
                                  (range 3 (inc lim) 2)))))]
    (println "Serial (filter):")
    (time (count (doall (filter p xs))))
    (println "Parallel (pfilter-blocked, 4096):")
    (time (count (doall (parallel-filter p xs 4096))))))


(println (doall (take 10 (parallel-filter even? (range 1 60) 10))))
(parallel-filter-test)
;;(demonstrate-efficiency)
(c3-timing-demo)