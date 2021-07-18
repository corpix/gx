(import	:corpix/gx/test
	:corpix/gx/list)

(deftest "list"
  ("split-by"
   (check-equal? (split-by '(1 2 3 4 5 6) 1)
		 '((1) (2) (3) (4) (5) (6)))
   (check-equal? (split-by '(1 2 3 4 5 6) 3)
		 '((1 2 3) (4 5 6))))
  ("pairs"
   (check-equal? (pairs '(1 2 3 4))
		 '((1 2) (3 4)))))
