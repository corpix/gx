(import :std/text/utf8
	:std/format
	:gerbil/gambit/threads
	:corpix/gerbilstd/test
	:corpix/gerbilstd/net/url
	:corpix/gerbilstd/net/http/request)

(parameterize ((*tests* (make-tests)))
  (deftest "test request"
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
	   (close-output-port client)
	   (check-equal? (request-content (http-get url)) data)
	   (check-equal? (get-output-u8vector client) #u8())))))
    ("http-get without content-length"
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
		(res  (u8vector-append (string->utf8 "HTTP/1.1 200 OK\r\n\r\n") data)))
	   (write-subu8vector res 0 (u8vector-length res) client)
	   (close-output-port client)
	   (check-equal? (request-content (http-get url)) data)
	   (check-equal? (get-output-u8vector client) #u8()))))))
  (test!))2
