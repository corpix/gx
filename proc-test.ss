(import :std/test
	"./proc.ss")
(export proc-test)

(def proc-test
  (test-suite "test proc"
    (test-case "->"
      (check-equal? (-> 1) 1)
      (check-equal? (-> 1 (+ 1)) 2)
      (check-equal? (-> 1 (+ 1) (* 2)) 4))))

(run-test-suite! proc-test)
