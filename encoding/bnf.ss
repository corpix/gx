(import :std/sugar
	:std/format
	:std/error
	:gerbil/gambit/threads
	:corpix/gerbilstd/string
	:corpix/gerbilstd/u8vector)
(export parser-error parser-error? parser-error-state parser-error-buf parser-error-rule
	buffer-not-exhausted-error buffer-not-exhausted-error? buffer-not-exhausted-error-state buffer-not-exhausted-error-buf
	(struct-out parser-node)
	(struct-out parser-state)
	defbnf)

(def empty-vec (vector))

(defstruct (parser-error <error>) (state buf rule))

(def make-parser-error*
  (case-lambda
    ((state buf rule)
     (make-parser-error*
      (format "Failed to parse ~s with rule ~a at ~a."
	      (if buf (utf8->string buf) "")
	      'body
	      (parser-state-pos state))
      '() #f
      state buf rule))
    ((message irritants trace state buf rule)
     (parser-error message irritants trace state buf rule))))

(defstruct (buffer-not-exhausted-error <error>) (state buf))

(def make-buffer-not-exhausted-error*
  (case-lambda
    ((state buf)
     (make-buffer-not-exhausted-error*
      (format "Input buffer is not exhausted, last position: ~a, data left in buffer: ~s."
	      (parser-state-pos state)
	      (if buf (utf8->string buf) ""))
      '() #f
      state buf))
    ((message irritants trace state buf)
     (buffer-not-exhausted-error message irritants trace state buf))))

;;

(defstruct parser-node (name rule match child)
  transparent: #t)

;;

(defstruct parser-state (buf pos depth)
  constructor: :init!)

(defmethod {:init! parser-state}
  (lambda (self buf)
    (set! (parser-state-buf   self) (*->u8vector buf))
    (set! (parser-state-pos   self) 0)
    (set! (parser-state-depth self) 0)))

(defmethod {load! parser-state}
  (lambda (self state-copy)
    (set! (parser-state-buf   self) (parser-state-buf   state-copy))
    (set! (parser-state-pos   self) (parser-state-pos   state-copy))
    (set! (parser-state-depth self) (parser-state-depth state-copy))))

(defmethod {match! parser-state}
  (lambda (self name rule do-match)
    (let ((buf   (parser-state-buf self))
	  (match (do-match self)))
      (and match
	   (parser-node
	    name rule
	    (subu8vector buf 0
			 (- (u8vector-length buf)
			    (u8vector-length (parser-state-buf self))))
	    match)))))

(defmethod {commit! parser-state}
  (lambda (self n)
    (begin0 (subu8vector (parser-state-buf self) 0 n)
      (set! (parser-state-buf self)
	(subu8vector (parser-state-buf self)
		     n (u8vector-length (parser-state-buf self))))
      (set! (parser-state-pos self)
	(+ (parser-state-pos self) n)))))

;; ~bnf-rule-aux expands a rule and returns a lambda
;; which receives `state` and returns (vector ...) or #f.
;; it is ok for vector to be empty.
;; results should not be wrapped into `parser-node`.
(defsyntax (~bnf-rule-aux stx)
  (syntax-case stx (or and * + lambda)
    ((_ (or rule ...))
     ;; TODO: static rules
     (syntax (lambda (state) (or ((~bnf-rule-aux rule) state) ...))))
    ((_ (and rule ...))
     (syntax
      (let* ((matchers (list (~bnf-rule-aux rule) ...))
	     (do-match (lambda (state)
			 (let loop ((acc (vector)) (ms matchers))
			   (if (pair? ms)
			     (let ((s (utf8->string (parser-state-buf state)))
				   (match ((car ms) state)))
			       (and match (loop
					   (vector-append acc (vector match))
					   (cdr ms))))
			     acc)))))
	(lambda (state)
	  (let* ((state-copy (struct-copy state))
		 (match (do-match state-copy)))
	    (begin0 match
	      (and match {load! state state-copy})))))))
    ((_ (* rule))
     (syntax
      (let ((do-match (~bnf-rule-aux rule)))
	(lambda (state)
	  (let loop ((acc (vector)))
	    (let ((match (do-match state)))
	      (if match
		(loop (vector-append acc (vector match)))
		acc)))))))
    ((_ (+ rule))
     (syntax
      (let ((do-match (~bnf-rule-aux rule)))
	(lambda (state)
	  (let loop ((acc (vector)) (n 0))
	    (let ((match (do-match state)))
	      (if match
		(loop (vector-append acc (vector match)) (+ n 1))
		(and (> n 0) acc))))))))
    ((_ (lambda (state) body ...))
     (syntax (lambda (state) body ...)))

    ((_ rule)
     (let ((rule-val (syntax->datum (syntax rule))))
       (with-syntax
	   ((transformer
	     (cond
	      ((string? rule-val)
	       (syntax (let ((r (*->u8vector rule)))
			 (lambda (state)
			   (and (>= (u8vector-length (parser-state-buf state))
				    (u8vector-length r))
				(let ((sub (subu8vector (parser-state-buf state)
							0 (u8vector-length r))))
				  (and (equal? sub r)
				       {commit! state (u8vector-length r)}
				       empty-vec)))))))
	      ((symbol? rule-val) (syntax rule))
	      (else (error "Expected symbol or string, got" rule-val)))))
	 (syntax transformer))))))

;; ~bnf-parse-aux expands all rules providen and
;; returns lambda which consumes an input to provide
;; the `parser-node` or raise an exception in case:
;; - there was no match in `input`
;; - input buffer is not drained (not all data was read)
(defsyntax (~bnf-parse-aux stx)
  (syntax-case stx ()
    ((_ ((var val) ...) body)
     (syntax
      (let* ((var  (lambda (state) {match! state 'var 'val (~bnf-rule-aux val)})) ...
	     (proc (lambda (state) {match! state #f 'body  (~bnf-rule-aux body)})))
	(lambda (input)
	  (let* ((state (make-parser-state input))
		 (child (proc state))
		 (buf   (and child (parser-state-buf state))))
	    ;; TODO: this is an extension point, we probably want to have
	    ;; an implementation which would not fail with exceptions
	    ;; of such kind (non exhausting)
	    (begin0 child
	      (unless child
		(raise (make-parser-error* state buf 'body)))
	      (when (> (u8vector-length buf) 0)
		(raise (make-buffer-not-exhausted-error* state buf)))))))))))

;; defbnf defines a parser with `name`
;; which is represented by a lambda
;; using `rules` and `body` for parsing
;; input passed to a lambda as first argument.
(defsyntax (defbnf stx)
  (syntax-case stx ()
    ((_ name rules body)
     (syntax (def name (~bnf-parse-aux rules body))))))
