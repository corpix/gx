(import (for-syntax :std/sugar
		    :std/stxutil))
(export cond*
	let-cond
	let*-cond
	struct-set!)

;; cond for multiple choices
(defrules cond* (else =>)
  ((cond* (else result0 results ...))
   (list (begin result0 results ...)))
  ((cond* (test => result))
   (let ((t test))
     (if t (list (result t)) (list))))
  ((cond* (test => result) clause0 clauses ...)
   (let ((t test)
	 (remaining (cond* clause0 clauses ...)))
     (if t (cons (result t) remaining) remaining)))
  ((cond* (test))
   (let ((t test))
     (if t (list test) (list))))
  ((cond* (test) clause0 clauses ...)
   (let ((t test)
	 (remaining (cond* clause0 clauses ...)))
     (if t (cons t remaining) remaining)))
  ((cond* (test result0 results ...))
   (if test (list (begin result0 results ...)) (list)))
  ((cond* (test result0 results ...)
	  clause0 clauses ...)
   (let ((remaining (cond* clause0 clauses ...)))
     (if test
       (cons (begin result0 results ...) remaining)
       remaining))))

;; (cond* (else 1))
;; (cond* (1 => (lambda (x) x)))
;; (cond* (#f => (lambda (x) x)))
;; (cond* (666 => (lambda (x) (list 'the-doom-is-here x)))
;;        (#f 'nevermind)
;;        ((eq? 2 2) 'second-sym)
;;        ((eq? 1 1) 'first-sym)
;;        (2)
;;        (else 1)) ;; => ((the-doom-is-here 666) second-sym first-sym 2 1)

(defrules ~let-cond-aux (else =>)
  ((~let-cond-aux _ (else result0 results ...))
   (begin result0 results ...))
  ((~let-cond-aux let-variant (binding test => result))
   (let-variant binding
     (let ((t test))
       (if t (result t)))))
  ((~let-cond-aux let-variant (binding test => result) clause0 clauses ...)
   (let-variant binding
     (let ((t test))
       (if t
	 (result t)
	 (~let-cond-aux clause0 clauses ...)))))
  ((~let-cond-aux let-variant binding (test))
   (let-variant binding test))
  ((~let-cond-aux let-variant (binding test) clause0 clauses ...)
   (let-variant binding
     (or test (~let-cond-aux clause0 clauses ...))))
  ((~let-cond-aux let-variant (binding test result0 results ...))
   (let-variant binding
     (if test (begin result0 results ...))))
  ((~let-cond-aux let-variant (binding test result0 results ...)
		  clause0 clauses ...)
   (let ((values match? result)
	 (let-variant binding
	   (if test
	     (values #t (begin result0 results ...))
	     (values #f #!void))))
     (if match? result
	 (~let-cond-aux let clause0 clauses ...)))))

(defrules let-cond ()
  ((let-cond body ...)
   (~let-cond-aux let body ...)))

(defrules let*-cond ()
  ((let*-cond body ...)
   (~let-cond-aux let* body ...)))

;; (let*-cond (((a 0) (t (> a 1))) t a)
;; 	   ((b 10) (> b 5) b)
;; 	   (else 'baaaakaaa))

;;

(defsyntax (struct-set! stx)
  (syntax-case stx ()
    ((_ struct type key value)
     (with-syntax ((field-set! (format-id #'stx "~a-~a-set!" #'type #'key)))
       (syntax (field-set! struct value))))))

;; (defstruct x (y z))
;; (def xx (x 1 2))
;; (x-y xx) ;; 1
;; (x-z xx) ;; 2
;; (struct-set! xx x y 666)
;; (struct-set! xx x z 777)
;; (x-y xx) ;; 666
;; (x-z xx) ;; 777