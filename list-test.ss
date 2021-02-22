(import :std/test
	"./list.ss")
(export list-test)

(def list-test
  (test-suite "test list"
    (test-case "split-by"
      (check-equal? (split-by '(1 2 3 4 5 6) 1)
		    '((1) (2) (3) (4) (5) (6)))
      (check-equal? (split-by '(1 2 3 4 5 6) 3)
		    '((1 2 3) (4 5 6))))
    (test-case "pairs"
      (check-equal? (pairs '(1 2 3 4))
		    '((1 2) (3 4))))))

(run-test-suite! list-test)
