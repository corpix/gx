(import :corpix/gx/test
	:corpix/gx/exception
	:corpix/gx/vector)

(deftest "u8vector"
  ("u8vector-index"
    (check-equal? (u8vector-index (u8vector 1 2 3) 3)
		  2)
    (check-equal? (u8vector-index (u8vector 1 2 3) 10)
		  #f)
    (check-equal? (u8vector-index (u8vector 1 2 3) 1 1)
		  #f)
    (check-equal? (u8vector-index (u8vector 1 2 3) 3 0 1)
		  #f)
    (check-equal? (u8vector-index (u8vector 1 2 3) 3 0)
		  2)
    (check-eq? (try (u8vector-index (u8vector 1 2 3) 10 0 10)
		    (catch (range-exception? exn) #t))
	       #t))
  ("u8vector-contains-any"
    (check-equal? (u8vector-contains-any (u8vector 1 2 3) (u8vector 6 5 4))
		  #f)
    (check-equal? (u8vector-contains-any (u8vector) (u8vector))
		  #f)
    (check-equal? (u8vector-contains-any (u8vector 1 2) (u8vector 1))
		  (cons 0 1))
    (check-equal? (u8vector-contains-any (u8vector 1 2) (u8vector 2))
		  (cons 1 2))
    (check-equal? (u8vector-contains-any (u8vector 1 2 2) (u8vector 2))
		  (cons 1 2))
    (check-equal? (u8vector-contains-any (u8vector 1 2 2 1) (u8vector 2 1))
		  (cons 1 2))))
