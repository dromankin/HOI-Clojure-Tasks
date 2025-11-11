(defn primes []
  (letfn [(sieve [table n]
            (if-let [factors (get table n)]
              (recur (reduce (fn [t p]
                               (update t (+ n p) conj p))
                             (dissoc table n)
                             factors)
                     (inc n))
              (lazy-seq (cons n (sieve (assoc table (* n n) [n]) (inc n))))))]
    (sieve {} 2)))

(println (take 100 (primes)))


(require '[clojure.test :refer [deftest is]])

(deftest primes-test
  (is (= [2 3 5 7 11 13 17 19 23 29] 
         (take 10 (primes))))
  (is (every? #(empty? (filter (fn [x] (zero? (mod % x))) 
                              (range 2 (int (Math/sqrt %)))))
              (take 100 (primes)))))

(defn run-all-tests []
  (primes-test)
)

(run-all-tests)