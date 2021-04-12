(import	:std/test)
(export (import: :std/test)
	*test-suites*
	deftest
	test!)

(def *test-suites* (make-parameter (box '())))

(defrules deftest ()
  ((_ desc body ...)
   (let ((test-suites (*test-suites*)))
     (box-set! test-suites
	       (cons (test-suite desc (~deftest-aux body ...))
		     (unbox test-suites))))))

(defrules ~deftest-aux ()
  ((_ (case body ...))
   (test-case case body ...))
  ((_ body ...)
   (begin (~deftest-aux body) ...)))

(def (test!)
  (displayln)
  (apply run-tests! (reverse (unbox (*test-suites*))))
  (displayln)
  (test-report-summary!))
