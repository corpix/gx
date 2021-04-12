(import	:corpix/gerbilstd/test
	:corpix/gerbilstd/hash)

(deftest "test hash"
  ("hash-path-ref"
   (let ((table    (make-hash-table))
	 (subtable (make-hash-table)))
     (hash-put! subtable 'bar 'baz)
     (hash-put! table 'foo subtable)
     (check-eq? (hash-path-ref table '(foo bar)) 'baz))
   (let (table (make-hash-table))
     (check-eq? (hash-path-ref table '(foo) 'not-presented) 'not-presented)
     (check-exception
      (hash-path-ref table '(foo))
      (lambda (e) (exception? e))))))
