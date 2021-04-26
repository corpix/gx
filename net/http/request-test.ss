(import :std/text/utf8
	:std/format
	:std/event
	:std/misc/barrier
	:gerbil/gambit
	:corpix/gerbilstd/test
	:corpix/gerbilstd/net/url
	:corpix/gerbilstd/net/http/request)

(deftest "net/http/request"
  ("http-get"
   (let* ((url "http://localhost")
	  ((values client server) (open-u8vector-pipe
				   (list buffering: #t)
				   (list buffering: #t)))
	  (make-connection (lambda (request-url)
			     (begin0 server
			       (check-equal? (url->string request-url)
					     (url->string (string->url url)))))))
     (parameterize ((*make-http-connection* make-connection))
       (let* ((data (string->utf8 "hello world"))
	      (res  (u8vector-append (string->utf8
				      (format "HTTP/1.1 200 OK\r\nContent-Length: ~a\r\n\r\n"
					      (u8vector-length data)))
				     data)))
	 (write-subu8vector res 0 (u8vector-length res) client)
	 (check-equal? (request-content (http-get url)) data)
	 (check-equal? (get-output-u8vector client) #u8())))))

  ("http-get without content-length"
   (let* ((url "http://localhost")
	  (data (string->utf8 "hello world"))
	  ((values client server) (open-u8vector-pipe))
	  (make-connection (lambda (request-url)
			     (begin0 server
			       (check-equal? (url->string request-url)
					     (url->string (string->url url))))))
	  (res-u8 (u8vector-append (string->utf8 "HTTP/1.1 200 OK\r\n\r\n") data))
	  (req-u8 (string->utf8 "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"))
	  (content #f)
	  (b (make-barrier 2)))
     (parameterize ((*make-http-connection* make-connection))
       (spawn (lambda ()
		(set! content (request-content (http-get url)))
		(close-port server)
		(barrier-post! b)))
       (spawn (lambda ()
		(write-subu8vector res-u8 0 (u8vector-length res-u8) client)
		(force-output client)
		(close-port client)
		(barrier-post! b)))
       (barrier-wait! b)
       (check-equal? content data)
       (check-equal? (utf8->string (get-output-u8vector client)) "")
       (check-equal? (utf8->string (get-output-u8vector server))
		     (utf8->string req-u8)))))

  ("http-post"
   (let* ((url "http://localhost")
	  (data (string->utf8 "hello world"))
	  ((values client server) (open-u8vector-pipe))
	  (make-connection (lambda (request-url)
			     (begin0 server
			       (check-equal? (url->string request-url)
					     (url->string (string->url url))))))
	  (res-u8 (string->utf8 "HTTP/1.1 200 OK\r\n\r\n"))
	  (req-u8 (u8vector-append
		   (string->utf8 "POST / HTTP/1.1\r\nContent-Length: 11\r\nHost: localhost\r\n\r\n")
		   data))
	  (content #f)
	  (b (make-barrier 2)))
     (parameterize ((*make-http-connection* make-connection))
       (spawn (lambda ()
		(set! content (request-content (http-post url body: data)))
		(close-port server)
		(barrier-post! b)))
       (spawn (lambda ()
		(write-subu8vector res-u8 0 (u8vector-length res-u8) client)
		(force-output client)
		(close-port client)
		(barrier-post! b)))
       (barrier-wait! b)
       (check-equal? content #u8())
       (check-equal? (utf8->string (get-output-u8vector client)) "")
       (check-equal? (utf8->string (get-output-u8vector server))
		     (utf8->string req-u8)))))

  ("http-post port injection"
   (let* ((url "http://localhost")
	  (data (string->utf8 "hello world"))
	  ((values client server) (open-u8vector-pipe))
	  (res-u8 (string->utf8 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"))
	  (req-u8 (u8vector-append
		   (string->utf8 "POST / HTTP/1.1\r\nContent-Length: 11\r\nHost: localhost\r\n\r\n")
		   data))
	  (content #f)
	  (b (make-barrier 2)))
     (spawn (lambda ()
	      (set! content (request-content (http-post url
							body: data
							port: server)))
	      (close-port server)
	      (barrier-post! b)))
     (spawn (lambda ()
	      (write-subu8vector res-u8 0 (u8vector-length res-u8) client)
	      (force-output client)
	      (close-port client)
	      (barrier-post! b)))
     (barrier-wait! b)
     (check-equal? content #u8())
     (check-equal? (utf8->string (get-output-u8vector client)) "")
     (check-equal? (utf8->string (get-output-u8vector server))
		   (utf8->string req-u8))))

  ("http-post port reuse"
   (let* ((url "http://localhost")
	  (data (string->utf8 "hello world"))
	  ((values client server) (open-u8vector-pipe))
	  (res-u8 (string->utf8 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"))
	  (req-u8 (u8vector-append
		   (string->utf8 "POST / HTTP/1.1\r\nContent-Length: 11\r\nHost: localhost\r\n\r\n")
		   data))
	  (content #f)
	  (b (make-barrier 2)))
     (spawn (lambda ()
	      (set! content (request-content (http-post url
							body: data
							port: server)))
	      (set! content (u8vector-append
			     content
			     (request-content (http-post url
							 body: data
							 port: server))))
	      (close-port server)
	      (barrier-post! b)))
     (spawn (lambda ()
	      (write-subu8vector res-u8 0 (u8vector-length res-u8) client)
	      (write-subu8vector res-u8 0 (u8vector-length res-u8) client)
	      (force-output client)
	      (close-port client)
	      (barrier-post! b)))
     (barrier-wait! b)
     (check-equal? content #u8())
     (check-equal? (utf8->string (get-output-u8vector client)) "")
     (check-equal? (utf8->string (get-output-u8vector server))
		   (utf8->string (u8vector-append req-u8 req-u8))))))
