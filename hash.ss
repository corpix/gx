(import :std/sugar
	:std/format)
(export hash-path-ref
	make-hash
	make-hash-eq
	make-hash-eqv)

(def undefined (gensym))

(def (hash-path-ref table refs (default undefined))
  (cond
   ((eq? refs '()) table)
   ((= (length refs) 1)
    (if (eq? default undefined)
      (hash-ref table (car refs))
      (hash-ref table (car refs) default)))
   ((hash-key? table (car refs))
    (hash-path-ref
     (if (eq? default undefined)
       (hash-ref table (car refs))
       (hash-ref table (car refs) default))
     (cdr refs) default))
   (else (if (eq? default undefined)
           (error (format "no value found for key: ~a"
			  (car refs)))
           default))))

      (let ((table    (make-hash-table))
	    (subtable (make-hash-table)))
	(hash-put! subtable 'bar 'baz)
	(hash-put! table 'foo subtable)
	(hash-path-ref table '(foo bar)))

;;

(defrule (make-hash expr ...)
  (~hash-table make-hash-table expr ...))

(defrule (make-hash-eq expr ...)
  (~hash-table make-hash-table-eq expr ...))

(defrule (make-hash-eqv expr ...)
  (~hash-table make-hash-table-eqv expr ...))

(defsyntax (~hash-table stx)
  (syntax-case stx ()
    ((_ make-ht expr ...)
     (with-syntax* ((size (stx-length #'(expr ...))))
       #'(let (ht (make-ht size: size))
           (apply hash-put! ht expr) ... ht)))))