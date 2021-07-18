(import (for-syntax :std/sugar
		    :std/stxutil)
	:corpix/gx/iter
	:corpix/gx/proc
	:corpix/gx/type
	:corpix/gx/test)
(export defstruct*
	defstruct*/zero
	make-struct*
	struct-fields
	struct->alist
	struct-assoc
	struct-ref
	struct-set!)

(defrules defstruct* ()
  ((_ name fields meta ...)
   (defstruct name fields meta ... transparent: #t)))

(defrules defstruct*/zero ()
  ((_ name xs ...)
   (begin ;; TODO: tag as a separate type? (to check in make-record*)
     (defstruct* name xs ... constructor: :init!)
     (defmethod {:init! name}
       (lambda (self) (struct-instance-init! self))))))

(def (struct-fields t (default '()))
  (or (assgetq fields: (type-descriptor-plist t)) default))

;; (defstruct point (x y z))
;; (def p (point 666 777 888))

;; (type-descriptor-fields point::t) ;; => 3
;; (struct-fields point::t)

;; (struct-field-ref point::t p (struct-field-offset point::t 0)) ;; => 666

;; (check (begin
;; 	 (def xx (x 1 2))
;; 	 (let ((original (cdr (struct->list xx))))
;; 	   (struct-set! x xx y 666)
;; 	   (struct-set! x xx z 777)
;; 	   (cons (cdr (struct->list xx)) original)))
;;        => (list (list 666 777) 1 2))

(def (struct->alist t struct)
  (map cons
       (struct-fields t)
       (cdr (struct->list struct))))

(def (alist->struct t alist)
  (let ((field-values (map (lambda (key)
			     (let* ((fields (struct-fields t))
				    (field (memq key fields))
				    (value (and field (assoc (car field) alist)))
				    (value (and value (cadr value))))
			       value))
			   (struct-fields t))))
    (displayln (list (struct-fields t) field-values))
    ;; WTF is going on?
    (apply make-struct-instance (cons t field-values))))

;; (defstruct point (x y z) transparent: #t)
;; (check (alist->struct point::t '((y 777) (x 666) (z 888)))
;;        => (point 666 777 888))

(def (struct-assoc t key struct)
  (assoc key (struct->alist t struct)))

;; FIXME:
;; user@localhost ~/p/s/g/c/example > cat example.ss
;; (def (struct-field-offset-proc) struct-field-offset)
;; user@localhost ~/p/s/g/c/example > gxi -e '(import :std/make
;;                                                    :gerbil/gambit/misc)
;;                                                    (make srcdir: "."
;;                                                          verbose: 0
;;                                                          optimize: #t
;;                                                          `("example"))'
;; *** ERROR IN gx#core-expand-ref% -- Syntax Error
;; *** ERROR IN "example.ss"@1.33
;; --- Syntax Error: Reference to unbound identifier
;; ... form:   (%#ref struct-field-offset)
;; ... detail: struct-field-offset at "example.ss"@1.33

;; user@localhost ~/p/s/g/c/example (master) [70]> gxi
;; Gerbil v0.16-152-g808929ae on Gambit v4.9.3-1234-g6acd87df
;; > struct-field-offset
;; #<procedure #4 struct-field-offset>
;; >
;; *** EOF again to exit

(def (struct-ref t struct key (default #f))
  ;; could be improved (?) with this piece of code
  ;; if compilation issue mentioned upwards
  ;;
  ;; (def (struct-ref t struct key (default #f))
  ;;   (let* ((field-descriptors (for/collect
  ;; 				((field  (struct-fields t))
  ;; 				 (number (in-naturals (type-descriptor-fields t))))
  ;; 			      (cons field number)))
  ;; 	 (field-descriptor (assoc key field-descriptors)))
  ;;     (or (and field-descriptor
  ;; 	     (->> field-descriptor
  ;; 		  (cdr)
  ;; 		  (struct-field-offset t)
  ;; 		  (struct-field-ref t struct)))
  ;; 	default)))
  (let ((field (struct-assoc t key struct)))
    (if field (cdr field) default)))

;; FIXME: should be lambda
(defsyntax (struct-set! stx)
  (syntax-case stx ()
    ((_ type struct key value)
     (with-syntax ((mutator (format-id #'stx "~a-~a-set!" #'type #'key)))
       (syntax (mutator struct value))))))

(defsyntax (make-struct* stx)
  (syntax-case stx ()
    ((_ t (key value) ...)
     (syntax (alist->struct t (list (list (quote key) value) ...))))))

;; (defstruct point (x y z) transparent: #t)
;; (check (make-struct* point::t (y 777) (x 666) (z 888))
;;        => (point 666 777 888))