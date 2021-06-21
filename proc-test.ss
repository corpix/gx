(import :corpix/gerbilstd/test
	:corpix/gerbilstd/proc)

(deftest "proc"
  ("->"
   (check (-> 1) => 1)
   (check (-> 1 (+ 1)) => 2)
   (check (-> 1 (+ 1) (* 2)) => 4)
   (check (-> 1 (- 2)) => -1))
  ("->>"
   (check (->> 1) => 1)
   (check (->> 1 (+ 1)) => 2)
   (check (->> 1 (+ 1) (* 2)) => 4)
   (check (->> 1 (- 2)) => 1)))
