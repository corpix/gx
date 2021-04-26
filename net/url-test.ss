(import :std/format
	:corpix/gerbilstd/test
	:corpix/gerbilstd/net/url)

(deftest "net/url"
  ("url-http?"
   (check-eq? (url-http? (url +scheme-http+ "localhost" 80 "/" #f))
	      #t)
   (check-eq? (url-http? (url +scheme-https+ "localhost" 443 "/" #f))
	      #f))
  ("url-https?"
   (check-eq? (url-https? (url +scheme-http+ "localhost" 80 "/" #f))
	      #f)
   (check-eq? (url-https? (url +scheme-https+ "localhost" 443 "/" #f))
	      #t))
  ("string->url"
   (check-equal? (string->url "http://localhost")
		 (url +scheme-http+ "localhost" 80 "/" #f))
   (check-equal? (string->url "http://localhost/test")
		 (url +scheme-http+ "localhost" 80 "/test" #f))
   (check-equal? (string->url "https://localhost")
		 (url +scheme-https+ "localhost" 443 "/" #f))
   (check-equal? (string->url "https://localhost:44344")
		 (url +scheme-https+ "localhost" 44344 "/" #f))
   (check-equal? (string->url "https://localhost/test")
		 (url +scheme-https+ "localhost" 443 "/test" #f))
   (check-equal? (string->url "https://localhost:5566/?foo=bar")
		 (url +scheme-https+ "localhost" 5566 "/"
		      '(("foo" . "bar"))))
   (check-equal? (string->url "https://localhost/test?foo=bar")
		 (url +scheme-https+ "localhost" 443 "/test"
		      '(("foo" . "bar"))))
   (check-equal? (string->url "https://localhost:6655/test?foo=bar")
		 (url +scheme-https+ "localhost" 6655 "/test"
		      '(("foo" . "bar"))))
   (check-equal? (string->url "https://localhost/test?foo=bar&baz=qux")
		 (url +scheme-https+ "localhost" 443 "/test"
		      '(("foo" . "bar")
			("baz" . "qux")))))
  ("url->string"
   (check-equal? (url->string (url +scheme-http+ "localhost" 80 "/" #f))
		 "http://localhost")
   (check-equal? (url->string (url +scheme-http+ "localhost" 80 "/test" #f))
		 "http://localhost/test")
   (check-equal? (url->string (url +scheme-https+ "localhost" 443 "/" #f))
		 "https://localhost")
   (check-equal? (url->string (url +scheme-https+ "localhost" 44344 "/" #f))
		 "https://localhost:44344")
   (check-equal? (url->string (url +scheme-https+ "localhost" 443 "/test" #f))
		 "https://localhost/test")
   (check-equal? (url->string (url +scheme-https+ "localhost" 5566 "/"
				   '(("foo" . "bar"))))
		 "https://localhost:5566/?foo=bar")
   (check-equal? (url->string (url +scheme-https+ "localhost" 443 "/test"
				   '(("foo" . "bar"))))
		 "https://localhost/test?foo=bar")
   (check-equal? (url->string (url +scheme-https+ "localhost" 6655 "/test"
				   '(("foo" . "bar"))))
		 "https://localhost:6655/test?foo=bar")
   (check-equal? (url->string (url +scheme-https+ "localhost" 443 "/test"
				   '(("foo" . "bar")
				     ("baz" . "qux"))))
		 "https://localhost/test?foo=bar&baz=qux"))
  ("url->path"
   (check-equal? (url->path (url +scheme-http+ "localhost" 80 "/" #f))
		 "/")
   (check-equal? (url->path (url +scheme-http+ "localhost" 80 "/test" #f))
		 "/test")
   (check-equal? (url->path (url +scheme-https+ "localhost" 5566 "/"
				 '(("foo" . "bar"))))
		 "/?foo=bar")
   (check-equal? (url->path (url +scheme-https+ "localhost" 443 "/test"
				 '(("foo" . "bar"))))
		 "/test?foo=bar")
   (check-equal? (url->path (url +scheme-https+ "localhost" 443 "/test"
				 '(("foo" . "bar")
				   ("baz" . "qux"))))
		 "/test?foo=bar&baz=qux")))
