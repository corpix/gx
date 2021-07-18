(import :std/sugar
	:corpix/gx/yaml
	:corpix/gx/test)

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
   (check (yaml-dump-string (hash (foo: (list))
				  (bar: "1")))
	  => "---\nbar: 1\nfoo: []\n")
   (check (yaml-dump-string "hello")
	  => "--- hello\n")))
