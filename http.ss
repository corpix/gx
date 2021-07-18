(import :gerbil/gambit/ports
        :std/error
        :std/format
        :std/net/uri
        :std/srfi/13
        :std/sugar
	:std/pregexp
	:std/stxutil
        :std/text/base64
        :std/text/json
        :std/text/utf8
        :std/text/zlib
	:std/misc/uuid
        :std/misc/list-builder
	:std/misc/ports
	:corpix/gx/url
	(prefix-in :corpix/gx/multipart multipart-))
(export
  http-get http-head http-post http-put http-delete http-options
  http-request
  http-headers-host
  http-headers-auth
  http-headers-cookies
  http-headers-cons http-headers-cons!
  with-request
  request? request request-port request-url request-history request-status request-status-text request-headers request-body
  request-encoding request-encoding-set!
  request-content
  request-text
  request-json
  request-cookies
  request-close
  request-id
  *make-request-id*
  make-http-connection
  *make-http-connection*)

;;

(defconst +header-host+              "Host")
(defconst +header-content-length+    "Content-Length")
(defconst +header-content-encoding+  "Content-Encoding")
(defconst +header-content-type+      "Content-Type")
(defconst +header-location+          "Location")
(defconst +header-user-agent+        "User-Agent")
(defconst +header-connection+        "Connection")
(defconst +header-accept+            "Accept")
(defconst +header-accept-encoding+   "Accept-Encoding")
(defconst +header-authorization+     "Authorization")
(defconst +header-cookie+            "Cookie")
(defconst +header-set-cookie+        "Set-Cookie")
(defconst +header-transfer-encoding+ "Transfer-Encoding")

(defconst +auth-type-basic+ "Basic")

(def cr (char->integer #\return))
(def lf (char->integer #\newline))
(def crlf (u8vector cr lf))

;;

(def (http-get url
               redirect: (redirect #f)
               headers:  (headers  #f)
               cookies:  (cookies  #f)
               query:    (query    #f)
	       port:     (port     #f)
	       auth:     (auth     #f))
  (http-request 'GET url
		redirect:  redirect
		headers:   headers
		cookies:   cookies
		form:      #f
		multipart: #f
		query:     query
		body:      #f
		port:      port
		auth:      auth))

(def (http-head url
                redirect: (redirect #f)
                headers:  (headers  #f)
                cookies:  (cookies  #f)
                query:    (query    #f)
		port:     (port     #f)
		auth:     (auth     #f))
  (http-request 'HEAD url
		redirect:  redirect
		headers:   headers
		cookies:   cookies
		form:      #f
		multipart: #f
		query:     query
		body:      #f
		port:      port
		auth:      auth))

(def (http-post url
                redirect:  (redirect  #f)
                headers:   (headers   #f)
                cookies:   (cookies   #f)
		form:      (form      #f)
		multipart: (multipart #f)
                query:     (query     #f)
                body:      (body      #f)
		port:      (port      #f)
		auth:      (auth      #f))
  (http-request 'POST url
		redirect:  redirect
		headers:   headers
		cookies:   cookies
		form:      form
		multipart: multipart
		query:     query
		body:      body
		port:      port
		auth:      auth))

(def (http-put url
               redirect:  (redirect  #f)
               headers:   (headers   #f)
               cookies:   (cookies   #f)
	       form:      (form      #f)
	       multipart: (multipart #f)
               query:     (query     #f)
               body:      (body      #f)
	       port:      (port      #f)
	       auth:      (auth      #f))
  (http-request 'PUT url
		redirect:  redirect
		headers:   headers
		cookies:   cookies
		form:      form
		multipart: multipart
		query:     query
		body:      body
		port:      port
		auth:      auth))

(def (http-delete url
                  redirect:  (redirect  #f)
                  headers:   (headers   #f)
                  cookies:   (cookies   #f)
		  form:      (form      #f)
		  multipart: (multipart #f)
                  query:     (query     #f)
		  body:      (body      #f)
		  port:      (port      #f)
                  auth:      (auth      #f))
  (http-request 'DELETE url
		redirect:  redirect
		headers:   headers
		cookies:   cookies
		form:      form
		multipart: multipart
		query:     query
		body:      body
		port:      port
		auth:      auth))

(def (http-options url
                   headers:  (headers #f)
                   cookies:  (cookies #f)
                   query:    (query   #f)
		   port:     (port    #f)
		   auth:     (auth    #f))
  (http-request 'OPTIONS url
		headers:   headers
		cookies:   cookies
		query:     query
		port:      port
		auth:      auth))

;;

(def (http-request method url
		   redirect:  (redirect  #f)
		   headers:   (headers   #f)
		   cookies:   (cookies   #f)
		   form:      (form      #f)
		   multipart: (multipart #f)
		   query:     (query     #f)
		   body:      (body      #f)
		   port:      (port      #f)
		   auth:      (auth      #f)
		   history:   (history   '()))
  (let* ((url (if (url? url) url (string->url url)))
	 (url (if query
		(begin0 url (url-query-set! url query))
		url))
	 (multipart (and multipart
			 (if (multipart-form? multipart) multipart
			     (multipart-form
			      (multipart-random-boundary)
			      multipart))))
         (body
	  (let ((choice (or body multipart form)))
	    (cond
	     ((not             choice) #f)
	     ((input-port?     choice) choice)
	     ((u8vector?       choice) choice)
	     ((string?         choice) (string->utf8 choice))
	     ;; TODO: could we do streaming multipart form here?
	     ;; protocol ready for this, but server implementations are not :(
	     ;; Go's server fails with: multipart: NextPart: EOF
	     ;; go-1.15.6/share/go/src/net/http/server.go:1801
	     ((multipart-form? choice) (multipart-form->u8vector choice))
	     (else
	      (error "unexpected request body type" choice)))))
	 (headers
	  (http-headers-cons
	   (let loop ((content-type   #f)
		      (content-length #f)
		      (acc (list (http-headers-host url))))
	     (cond ((and (not content-length)
			 (u8vector? body))
		    (loop content-type
			  #t
			  (cons (cons +header-content-length+
				      (u8vector-length body))
				acc)))
		   ((and (not content-type)
			 (multipart-form? multipart))
		    (loop #t
			  content-length
			  (cons
			   (cons +header-content-type+
				 (multipart-content-type (multipart-form-boundary multipart)))
			   acc)))
		   (else acc)))
	   (or headers '())))
         (sock (or port (make-http-connection url)))
         (req  (make-request sock url history)))
    (http-request-write sock method (url->path url) headers body)
    (force-output sock)
    (http-request-read-response! req)
    (cond
     ((and redirect
           (memv (request-status req) '(301 302 303 307)) ;; TODO: constants
           (memq method '(GET HEAD))
           (assoc +header-location+ (request-headers req)))
      => (match <>
           ([_ . new-url]
            (if (member new-url history)
              (error "redirect loop detected in response from" url)
              (begin
                (request-close req)
                (http-request method new-url
			      headers:  headers
			      body:     body
			      history: (cons url history)
			      redirect: #t))))))
     (else req))))

(defsyntax (with-request stx)
  (syntax-case stx ()
    ((_ (name ctr) body ...)
     (syntax
      (let ((name ctr))
	(try body ...
	     (finally (request-close name))))))))

;;

(defstruct request (port url history status status-text headers body encoding)
  constructor: :init!)

(defmethod {:init! request}
  (lambda (self port url history)
    (struct-instance-init! self port url history)))

;;

(def (header-string-e str)
  (cond
   ((string? str)   str)
   ((symbol? str)  (symbol->string str))
   ((keyword? str) (keyword->string str))
   (else
    (error "bad header key" str))))

;;

(def *http-user-agent* (make-parameter "Mozilla/5.0 (compatible; gerbil-http/1.0)"))

(def *http-base-headers*
  (make-parameter
   [[+header-user-agent+      . (*http-user-agent*)]
    [+header-connection+      . "close"]
    [+header-accept+          . "*/*"]
    [+header-accept-encoding+ . "gzip, deflate, identity"]]))

(def (make-http-headers headers cookies auth)
  (http-headers-cons! headers
		      (chain (*http-base-headers*)
			(http-headers-cons <> (http-headers-cookies (or cookies '())))
			(http-headers-cons <> (http-headers-auth auth)))))

(def (http-headers-host url)
  (let ((scheme (url-scheme url)))
    (cons +header-host+
	  (cond
	   ((or (equal? "http" scheme)
		(equal? "https" scheme))
	    (url-host url))
	   (else (format "~a:~a"
			 (url-host url)
			 (url-scheme->port scheme)))))))

(def (http-headers-auth auth)
  (def (basic-auth-header user password)
    (let ((credentials (u8vector->base64-string
                        (with-output-to-u8vector
                         (lambda ()
                           (display user)
                           (display ":")
                           (display password))))))
      (cons +header-authorization+
	    (string-append +auth-type-basic+ " " credentials))))
  (match auth
    ([basic: user password] [(basic-auth-header user password)])
    (#f                     '())
    (_                       (error "unknown auth value" auth))))

(def (http-headers-cookies cookies)
  (def (fold-e cookie str)
    (with ([key . value] cookie)
      (if (string-empty? str)
        (format "~a=~a" key value)
        (format "~a; ~a=~a" str key value))))
  (if (null? cookies) '()
      (list (cons +header-cookie+ (foldr fold-e cookies "")))))

(def (http-headers-cons new-headers headers)
  (def (fold-e header headers)
    (with ([key . value] header)
      (let ((key (string-titlecase (header-string-e key))))
        (if (assoc key headers)
          headers
          (let ((value (format "~a" value)))
            (cons (cons key value) headers))))))
  (foldr fold-e headers new-headers))

(def (http-headers-cons! new-headers headers)
  (def (fold-e header headers)
    (with ([key . value] header)
      (let ((key   (string-titlecase (header-string-e key)))
            (value (format "~a" value)))
        (cond
         ((assoc key headers)
	  => (lambda (pair)
               (set! (cdr pair) value) headers))
         (else
          (cons (cons key value) headers))))))
  (foldr fold-e headers new-headers))

;;

(def (http-request-write port method target headers body)
  (def crlf-len (u8vector-length crlf))
  (fprintf port "~a ~a HTTP/1.1" method target)
  (write-subu8vector crlf 0 crlf-len port)
  (for-each (match <>
	      ([key . val]
	       (fprintf port "~a: ~a" key val)
	       (write-subu8vector crlf 0 crlf-len port)))
            headers)
  (write-subu8vector crlf 0 crlf-len port)
  (when body
    (cond
     ((input-port?     body) (copy-port body port))
     ((u8vector?       body) (write-subu8vector body 0 (u8vector-length body) port))
     ((multipart-form? body) (multipart-write! port body))))
  (force-output port))

(def status-line-rx
  (pregexp "([0-9]{3})\\s+(.*)"))

(def header-line-rx
  (pregexp "([^:]+):\\s*(.*)?"))

(def (http-request-read-response! req)
  (let* ((port (request-port req))
         (status-line (read-response-line port)))
    (match (pregexp-match status-line-rx status-line)
      ([_ status status-text]
       (set! (request-status req)
         (string->number status))
       (set! (request-status-text req)
         (string-trim-both status-text))
       (let ((root [#f]))
         (let lp ((tl root))
           (let ((next (read-response-line port)))
             (if (string-empty? next)
               (set! (request-headers req)
                 (cdr root))
               (match (pregexp-match header-line-rx next)
                 ([_ key value]
                  (let ((tl* [(cons (string-titlecase key) (string-trim-both value))]))
                    (set! (cdr tl) tl*)
                    (lp tl*)))
                 (else
                  (raise-io-error 'http-request-read-response!
                                  "Malformed header" port next))))))))
      (else
       (raise-io-error 'http-request-read-response!
                       "malformed status line" port status-line)))))

(def (http-request-read-body port headers)
  (def (length-e headers)
    (cond
     ((assget +header-content-length+ headers)
      => string->number)
     (else #f)))

  (cond
   ((assget +header-transfer-encoding+ headers)
    => (lambda (tenc)
         (if (not (equal? "identity" tenc))
           (http-request-read-chunked-body port)
           (http-request-read-simple-body port (length-e headers)))))
   (else
    (http-request-read-simple-body port (length-e headers)))))

(def (http-request-read-chunked-body port)
  (let ((root [#f]))
    (let lp ((tl root))
      (let* ((line (read-response-line port))
             (clen (string->number (car (string-split line #\space)) 16)))
        (if (fxzero? clen)
          (append-u8vectors (cdr root))
          (let* ((chunk (make-u8vector clen))
                 (rd    (read-subu8vector chunk 0 clen port)))
            (when (##fx< rd clen)
              (raise-io-error 'http-request-read-body
                              "error reading chunk; premature end of port"))
            (read-response-line port) ;; read chunk trailing CRLF
            (let ((tl* [chunk]))
              (set! (cdr tl) tl*)
              (lp tl*))))))))

(def (http-request-read-simple-body port length)
  (def (read/length port length)
    (let* ((data (make-u8vector length))
           (rd (read-subu8vector data 0 length port)))
      (if (##fx< rd length)
        (begin
          (u8vector-shrink! data rd)
          data)
        data)))

  (def (read/end port)
    (let ((root [#f]))
      (let lp ((tl root))
        (let* ((buflen 4096)
               (buf (make-u8vector buflen))
               (rd  (read-subu8vector buf 0 buflen port)))
          (cond
           ((##fxzero? rd)
            (append-u8vectors (cdr root)))
           ((##fx< rd buflen)
            (u8vector-shrink! buf rd)
            (set! (cdr tl) [buf])
            (append-u8vectors (cdr root)))
           (else
            (let ((tl* [buf]))
              (set! (cdr tl) tl*)
              (lp tl*))))))))

  (if length
    (read/length port length)
    (read/end port)))

(def (read-response-line port)
  (let ((root [#f]))
    (let lp ((tl root))
      (let ((next (read-u8 port)))
        (cond
         ((eof-object? next)
          (raise-io-error 'request-read-response-line
                          "Incomplete response; connection closed" port))
         ((eq? next cr)
          (let ((next (read-u8 port)))
            (cond
             ((eof-object? next)
              (raise-io-error 'request-read-response-line
                              "Incomplete response; connection closed" port))
             ((eq? next lf)
              (utf8->string (list->u8vector (cdr root))))
             (else
              (let ((tl* [cr next]))
                (set! (cdr tl) tl*)
                (lp (cdr tl*)))))))
         (else
          (let ((tl* [next]))
            (set! (cdr tl) tl*)
            (lp tl*))))))))

(def (request-close req)
  (alet (port (request-port req))
    (with-catch void (cut close-port port))
    (set! (request-port req) #f)))

(defmethod {destroy request}
  request-close)

(def (request-content req)
  (cond
   ((request-body req) => values)
   ((request-port req)
    => (lambda (port)
         (let ((headers (request-headers req)))
           (try
            (let* ((body (http-request-read-body port headers))
                   (body
                    (and body (cond
			       ((assoc +header-content-encoding+ headers)
				=> (lambda (enc)
				     (case (cdr enc)
				       (("gzip" "deflate") (uncompress body))
				       (("identity")        body)
				       (else
					(error "Unsupported content encoding" enc)))))
			       (else body)))))
              (begin0 body (set! (request-body req) body)))
            (finally
             (close-port port)
             (set! (request-port req) #f))))))
   (else #f)))

(def (request-text req)
  (def (get-text enc)
    (if (eq? enc 'UTF-8)
      (utf8->string (request-content req))
      (bytes->string (request-content req) enc)))
  (cond
   ((request-encoding req) => get-text)
   (else
    (get-text 'UTF-8))))

(def (request-json req)
  (string->json-object (request-text req)))

(def (request-cookies req)
  (with-list-builder
   (push!)
   (let lp ((rest (request-headers req)))
     (match rest
       ([hd . rest]
	(match hd
	  ([+header-set-cookie+ . cookie]
	   (push! cookie)
	   (lp rest))
	  (else
	   (lp rest))))
       (else #!void)))))
;;

(def *make-request-id*
  (make-parameter (lambda () (uuid->string (random-uuid)))))

(def (request-id)
  ((*make-request-id*)))

;;

(def tls-context (delay (make-tls-context)))

(def *make-http-connection*
  (make-parameter
   (lambda (url)
     (let ((url (if (url? url) url (string->url url))))
       (open-tcp-client
	(let ((options (list
			server-address: (url-host url)
			port-number: (or (url-port url)
					 (url-scheme->port (url-scheme url)))
			eol-encoding: 'cr-lf)))
	  (if (url-https? url)
	    (cons* tls-context: (force tls-context) options)
	    options)))))))

(def (make-http-connection url)
  ((*make-http-connection*) url))