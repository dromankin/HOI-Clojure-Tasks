(defn parallel-filter [pred coll & {:keys [chunk-size] 
                                   :or {chunk-size 1000}}]
  (letfn [(process-chunk [chunk]
            (doall (filter pred chunk)))
          
          (split-into-chunks [s]
            (lazy-seq
              (when-let [s (seq s)]
                (let [chunk (take chunk-size s)
                      rest-coll (drop chunk-size s)]
                  (cons chunk (split-into-chunks rest-coll))))))
          
          (process-parallel [chunks]
            (lazy-seq
              (when-let [chunks (seq chunks)]
                (let [chunks-to-process (take 4 chunks)  
                      futures (doall (map #(future (process-chunk %)) 
                                         chunks-to-process))
                      results (mapcat deref futures)]
                  (lazy-cat results 
                           (process-parallel (drop (count chunks-to-process) chunks)))))))]
    
    (if (empty? coll)
      '()
      (process-parallel (split-into-chunks coll)))))


(require '[clojure.test :refer [deftest is]])


(deftest parallel-filter-test
  (is (= [2 4 6 8 10] 
         (parallel-filter even? (range 1 11) :chunk-size 3)))
  
  (is (empty? (parallel-filter even? [])))
  
  (is (= [0 2 4 6 8] 
         (take 5 (parallel-filter even? (range) :chunk-size 10))))
  
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
    
    (System/exit 0))

(parallel-filter-test)
(demonstrate-efficiency)