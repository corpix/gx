(import	:std/test
	:gerbil/gambit/threads)
(export (import: :std/test)
	make-tests
	*tests*
	deftest
	test!)

(def (make-tests . xs) (box xs))

(def *tests* (make-parameter (make-tests)))

(defrules deftest ()
  ((_ desc body ...)
   (let ((tests (*tests*)))
     (box-set! tests
	       (cons (test-suite desc (~deftest-aux body ...))
		     (unbox tests))))))

(defrules ~deftest-aux ()
  ((_ (case body ...))
   (test-case case body ...))
  ((_ body ...)
   (begin (~deftest-aux body) ...)))

(def (test!)
  (displayln)
  (with-exception-stack-trace
   (lambda ()
     (apply run-tests! (reverse (unbox (*tests*))))))
  (displayln)
  (test-report-summary!))
