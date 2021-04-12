(import :corpix/gerbilstd/test
	:corpix/gerbilstd/string)

(deftest "test string"
  ("*->string"
   (check-equal? (*->string foo:)
		 "foo")
   (check-equal? (*->string 'foo)
		 "foo")
   (check-equal? (*->string 666)
		 "666"))
  ("string-contains-any"
   (check-equal? (string-contains-any "" "f")
		 #f)
   (check-equal? (string-contains-any "" "")
		 #f)
   (check-equal? (string-contains-any "xf" "x")
		 (cons 0 #\x))
   (check-equal? (string-contains-any "xf" "f")
		 (cons 1 #\f))
   (check-equal? (string-contains-any "xff" "f")
		 (cons 1 #\f))
   (check-equal? (string-contains-any "xffx" "fx")
		 (cons 1 #\f))))
