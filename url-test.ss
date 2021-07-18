(import :std/format
	:corpix/gx/test
	:corpix/gx/url)

(deftest "net/url"
  ("string->url"
   (check-equal? (string->url "http://localhost")
		 (make-url* scheme: "http" host: "localhost"))
   (check-equal? (string->url "http://localhost/test")
		 (make-url* scheme: "http" host: "localhost" path: "/test"))
   (check-equal? (string->url "https://localhost")
		 (make-url* scheme: "https" host: "localhost"))
   (check-equal? (string->url "https://localhost:44344")
		 (make-url* scheme: "https" host: "localhost" port: 44344))
   (check-equal? (string->url "https://localhost/test")
		 (make-url* scheme: "https" host: "localhost" path: "/test"))
   (check-equal? (string->url "https://localhost:5566/?foo=bar")
		 (make-url* scheme: "https" host: "localhost" port: 5566
			    path: "/" query: (form-url-decode "foo=bar")))
   (check-equal? (string->url "https://localhost/test?foo=bar")
		 (make-url* scheme: "https" host: "localhost"
			    path: "/test" query: (form-url-decode "foo=bar")))
   (check-equal? (string->url "https://localhost:6655/test?foo=bar")
		 (make-url* scheme: "https" host: "localhost" port: 6655
			    path: "/test" query: (form-url-decode "foo=bar")))
   (check-equal? (string->url "https://localhost/test?foo=bar&baz=qux")
		 (make-url* scheme: "https" host: "localhost"
			    path: "/test" query: (form-url-decode "foo=bar&baz=qux"))))
  ("url->string"
   (check-equal? (url->string (make-url* scheme: "http" host: "localhost"))
		 "http://localhost")
   (check-equal? (url->string (make-url* scheme: "http" host: "localhost" path: "/test"))
		 "http://localhost/test")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost"))
		 "https://localhost")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost" port: 44344))
		 "https://localhost:44344")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost" path: "/test"))
		 "https://localhost/test")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost" port: 5566
					 path: "/" query: (form-url-decode "foo=bar")))
		 "https://localhost:5566/?foo=bar")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost"
					 path: "/test" query: (form-url-decode "foo=bar")))
		 "https://localhost/test?foo=bar")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost" port: 6655
					 path: "/test" query: (form-url-decode "foo=bar")))
		 "https://localhost:6655/test?foo=bar")
   (check-equal? (url->string (make-url* scheme: "https" host: "localhost"
					 path: "/test" query: (form-url-decode "foo=bar&baz=qux")))
		 "https://localhost/test?foo=bar&baz=qux"))
  ("url-scheme->port"
   (check-eqv? (url-scheme->port "http") 80)
   (check-eqv? (url-scheme->port "https") 443)
   (check-exception (url-scheme->port "unknown")
		    (lambda (e) (exception? e)))
   (check-eqv? (url-scheme->port "unknown" 'default)
	       'default)
   (parameterize ((*url-scheme-port* (make-hash-table)))
     (hash-put! (*url-scheme-port*) "some" 666)
     (check-eqv? (url-scheme->port "some") 666)))
  ("url->path"
   (check-equal? (url->path (string->url "http://example.com"))
		 "/")
   (check-equal? (url->path (string->url "http://example.com/"))
		 "/")
   (check-equal? (url->path (string->url "http://example.com/test"))
		 "/test")
   (check-equal? (url->path (string->url "http://example.com/?foo=bar"))
		 "/?foo=bar")
   (check-equal? (url->path (string->url "http://example.com/test?foo=bar"))
		 "/test?foo=bar")
   (check-equal? (url->path (string->url "http://example.com/test?foo=bar&baz=qux"))
		 "/test?foo=bar&baz=qux"))
  ("url-http?"
   (check-eq? (url-http? (string->url "http://localhost"))  #t)
   (check-eq? (url-http? (string->url "https://localhost")) #f))
  ("url-https?"
   (check-eq? (url-https? (string->url "https://localhost")) #t)
   (check-eq? (url-https? (string->url "http://localhost"))  #f)))
