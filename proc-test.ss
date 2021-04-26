(import :corpix/gerbilstd/test
	:corpix/gerbilstd/proc)

(deftest "proc"
  ("->"
   (check-equal? (-> 1) 1)
   (check-equal? (-> 1 (+ 1)) 2)
   (check-equal? (-> 1 (+ 1) (* 2)) 4)))
