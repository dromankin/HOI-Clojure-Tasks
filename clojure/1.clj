(defn generate-strings [alphabet n]
  (let [step (fn [strings]
               (mapcat (fn [s]
                         (map (fn [c]
                                (str s c))
                              (remove (fn [x] (= (str (last s)) (str x))) alphabet)))
                       strings))]
    (if (<= n 0)
      [""]
      (reduce (fn [acc _] (step acc)) alphabet (range 1 n)))))

(println(generate-strings ["a" "b" "c"] 4))
