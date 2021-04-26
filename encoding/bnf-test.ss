(import	:corpix/gerbilstd/test
	:corpix/gerbilstd/encoding/bnf)

(deftest "encoding/bnf"
  ("terminals"
   (defbnf terminals-test
     ((t "test")
      (y "you")
      (t/y (or t y)))
     t/y)

   (check-equal?
    (terminals-test "test")
    (parser-node
     #f 't/y #u8(116 101 115 116)
     (parser-node
      't/y '(or t y) #u8(116 101 115 116)
      (parser-node 't "test" #u8(116 101 115 116) (vector)))))
   (check-equal?
    (terminals-test #u8(116 101 115 116))
    (parser-node
     #f 't/y #u8(116 101 115 116)
     (parser-node
      't/y '(or t y) #u8(116 101 115 116)
      (parser-node 't "test" #u8(116 101 115 116) (vector)))))
   (check-equal?
    (terminals-test "you")
    (parser-node
     #f 't/y #u8(121 111 117)
     (parser-node
      't/y '(or t y) #u8(121 111 117)
      (parser-node 'y "you" #u8(121 111 117) (vector)))))
   (check-equal?
    (terminals-test #u8(121 111 117))
    (parser-node
     #f 't/y #u8(121 111 117)
     (parser-node
      't/y '(or t y) #u8(121 111 117)
      (parser-node 'y "you" #u8(121 111 117) (vector)))))

   (check-exception (terminals-test "testyou") buffer-not-exhausted-error?)
   (check-exception (terminals-test "y") parser-error?)
   (check-exception (terminals-test "") parser-error?))
  ("booleans"
   (defbnf booleans-test
     ((t "test")
      (y "you")
      (ty (or t y))
      (ty&ty (and ty ty)))
     ty&ty)
   (check-equal?
    (booleans-test "testyou")
    (parser-node
     #f 'ty&ty #u8(116 101 115 116 121 111 117)
     (parser-node
      'ty&ty '(and ty ty) #u8(116 101 115 116 121 111 117)
      (vector
       (parser-node 'ty '(or t y) #u8(116 101 115 116)
		    (parser-node 't "test" #u8(116 101 115 116) (vector)))
       (parser-node 'ty '(or t y) #u8(121 111 117)
		    (parser-node 'y "you" #u8(121 111 117) (vector)))))))
   (check-equal?
    (booleans-test "youtest")
    (parser-node
     #f 'ty&ty #u8(121 111 117 116 101 115 116)
     (parser-node
      'ty&ty '(and ty ty) #u8(121 111 117 116 101 115 116)
      (vector
       (parser-node 'ty '(or t y) #u8(121 111 117)
		    (parser-node 'y "you" #u8(121 111 117) (vector)))
       (parser-node 'ty '(or t y) #u8(116 101 115 116)
		    (parser-node 't "test" #u8(116 101 115 116) (vector)))))))

   (check-equal?
    (booleans-test "testtest")
    (parser-node
     #f 'ty&ty #u8(116 101 115 116 116 101 115 116)
     (parser-node
      'ty&ty '(and ty ty) #u8(116 101 115 116 116 101 115 116)
      (vector
       (parser-node 'ty '(or t y) #u8(116 101 115 116)
		    (parser-node 't "test" #u8(116 101 115 116) (vector)))
       (parser-node 'ty '(or t y) #u8(116 101 115 116)
		    (parser-node 't "test" #u8(116 101 115 116) (vector)))))))
   (check-equal?
    (booleans-test "youyou")
    (parser-node
     #f 'ty&ty #u8(121 111 117 121 111 117)
     (parser-node
      'ty&ty '(and ty ty) #u8(121 111 117 121 111 117)
      (vector
       (parser-node 'ty '(or t y) #u8(121 111 117)
		    (parser-node 'y "you" #u8(121 111 117) (vector)))
       (parser-node 'ty '(or t y) #u8(121 111 117)
		    (parser-node 'y "you" #u8(121 111 117) (vector)))))))
   (check-exception (booleans-test "testyoutest") buffer-not-exhausted-error?)
   (check-exception (booleans-test "y") parser-error?)
   (check-exception (booleans-test "") parser-error?))
  ("repeat zero or more"
   (defbnf repeat*-test
     ((t "test")
      (t* (* t)))
     t*)

   (check-equal?
    (repeat*-test "")
    (parser-node #f 't* #u8() (parser-node 't* '(* t) #u8() (vector))))
   (check-equal?
    (repeat*-test "test")
    (parser-node
     #f 't* #u8(116 101 115 116)
     (parser-node
      't* '(* t) #u8(116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))))))
   (check-equal?
    (repeat*-test "testtest")
    (parser-node
     #f 't* #u8(116 101 115 116 116 101 115 116)
     (parser-node
      't* '(* t) #u8(116 101 115 116 116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector))))))
   (check-equal?
    (repeat*-test "testtesttest")
    (parser-node
     #f 't* #u8(116 101 115 116 116 101 115 116 116 101 115 116)
     (parser-node
      't* '(* t) #u8(116 101 115 116 116 101 115 116 116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector)))))))
  ("repeat one and more"
   (defbnf repeat+-test
     ((t "test")
      (t+ (+ t)))
     t+)

   (check-equal?
    (repeat+-test "test")
    (parser-node
     #f 't+ #u8(116 101 115 116)
     (parser-node
      't+ '(+ t) #u8(116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))))))
   (check-equal?
    (repeat+-test "testtest")
    (parser-node
     #f 't+ #u8(116 101 115 116 116 101 115 116)
     (parser-node
      't+ '(+ t) #u8(116 101 115 116 116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector))))))
   (check-equal?
    (repeat+-test "testtesttest")
    (parser-node
     #f 't+ #u8(116 101 115 116 116 101 115 116 116 101 115 116)
     (parser-node
      't+ '(+ t) #u8(116 101 115 116 116 101 115 116 116 101 115 116)
      (vector (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector))
	      (parser-node 't "test" #u8(116 101 115 116) (vector))))))))