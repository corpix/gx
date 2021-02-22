(import :std/format)
(export hash-path-ref)

(def default-not-supplied (gensym))

(def (hash-path-ref table refs (default default-not-supplied))
  (cond
   ((eq? refs '()) table)
   ((= (length refs) 1)
    (if (eq? default default-not-supplied)
      (hash-ref table (car refs))
      (hash-ref table (car refs) default)))
   ((hash-key? table (car refs))
    (hash-path-ref
     (if (eq? default default-not-supplied)
       (hash-ref table (car refs))
       (hash-ref table (car refs) default))
     (cdr refs) default))
   (else (if (eq? default default-not-supplied)
           (error (format "no value found for key: ~a"
			  (car refs)))
           default))))

      (let ((table    (make-hash-table))
	    (subtable (make-hash-table)))
	(hash-put! subtable 'bar 'baz)
	(hash-put! table 'foo subtable)
	(hash-path-ref table '(foo bar)))
