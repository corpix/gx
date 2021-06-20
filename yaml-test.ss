(import :std/sugar
	:corpix/gerbilstd/yaml
	:corpix/gerbilstd/test)

(deftest "yaml"
  ("yaml-load-string"
   (check (yaml-load-string "foo: bar")
	  => (list (hash ("foo" "bar"))))
   (parameterize ((*yaml-key-format* string->symbol))
     (check (yaml-load-string "foo: bar")
	    => (list (hash (foo "bar")))))
   (parameterize ((*yaml-key-format* string->keyword))
     (check (yaml-load-string "foo: bar")
	    => (list (hash (foo: "bar"))))))
  ("yaml-dump-string"
   (displayln (yaml-dump-string (hash (foo: (list))
				      (bar: "1"))))
   (displayln (yaml-dump-string "hello"))))
