(import	:gerbil/gambit
	:std/format
	:std/net/uri
	:corpix/gerbilstd/encoding/hex
	:corpix/gerbilstd/string)
(export
  url-default-scheme-port
  *url-scheme-port*

  (struct-out url)
  make-url*

  url-escape?
  url-escaped?
  url-escape
  url-unescape

  ;; url-trim-anchor

  ;; url-consume-scheme
  ;; url-consume-user
  ;; url-consume-host
  ;; url-consume-path
  ;; url-consume-query

  ;; url-produce-scheme
  ;; url-produce-user
  ;; url-produce-host
  ;; url-produce-path
  ;; url-produce-query

  string->url
  url->string

  url-scheme->port
  url->path
  url-http?
  url-https?)

;; inspired by Go url parser
;; see: https://golang.org/src/net/url/url.go

(defstruct url
  (scheme? scheme
	   user? user
	   host? host
	   port? port
	   path? path
	   query? query)
  transparent: #t)

(def (make-url* scheme: (scheme #f)
		user:   (user   #f)
		host:   (host   #f)
		port:   (port   #f)
		path:   (path   #f)
		query:  (query  #f))
  (url
   (and scheme #t) (or scheme "")
   (and user   #t) (or user (list))
   (and host   #t) (or host "")
   (and port   #t) (or port #f)
   (and path   #t) (or path "")
   (and query  #t) (or query "")))

;;

(def url-default-scheme-port
  (let* ((t (make-hash-table))
	 (put! (lambda (k v)
		 (begin0 t
		   (hash-put! t k v)))))
    (put! "http"  80)
    (put! "https" 443)))

(def *url-scheme-port*
  (make-parameter url-default-scheme-port))

;;

(def url-escape-modes
  '(path path-segment host zone user query-component fragment))

;; https://tools.ietf.org/html/rfc3986
(def (url-escape? n (mode #f))
  ;; see: https://golang.org/src/net/url/url.go?#L100
  (cond
   ((or (and (>= n #x61) (<= n #x7a))		    ;; a-z
	(and (>= n #x30) (<= n #x39))		    ;; 0-9
	(and (>= n #x41) (<= n #x5a)))		    ;; A-Z
    #f)						    ;;
   ((and (or (eq? 'fragment mode) (eq? 'path mode)) ;;
	 (or (= #x21 n)				    ;; !
	     (= #x22 n)				    ;; "
	     (= #x24 n)				    ;; $
	     (and (>= n #x26) (<= n #x2c))	    ;; &'()*+,
	     (= #x3a n)				    ;; :
	     (= #x3b n)				    ;; ;
	     (= #x3d n)				    ;; =
	     (= #x40 n)				    ;; @
    	     (= #x5b n)				    ;; [
	     (= #x5d n)))			    ;; ]
    #f)						    ;;
   ((and (or (eq? 'host mode) (eq? 'zone mode))	    ;;
	 (or (= #x21 n)				    ;; !
	     (= #x22 n)				    ;; "
	     (= #x24 n)				    ;; $
	     (and (>= n #x26) (<= n #x2c))	    ;; &'()*+,
	     (and (>= n #x3a) (<= n #x3d))	    ;; :;<>=
	     (= #x5b n)				    ;; [
	     (= #x5d n)))			    ;; ]
    #f)						    ;;
   ((or (= #x2d n)				    ;; -
	(= #x2e n)				    ;; .
	(= #x5f n)				    ;; _
	(= #x7e n))				    ;; ~
    #f)						    ;;
   ((or (= #x24 n)				    ;; $
	(= #x26 n)				    ;; &
	(= #x2b n)				    ;; +
	(= #x2c n)				    ;; ,
	(= #x2f n)				    ;; /
	(= #x3a n)				    ;; :
	(= #x3b n)				    ;; ;
	(= #x3d n)				    ;; =
	(= #x3f n)				    ;; ?
	(= #x40 n))				    ;; @
    (cond					    ;;
     ((eq? 'path mode)				    ;;
      (= #x3f n))				    ;; ?
     ((eq? 'path-segment mode)			    ;;
      (or (= #x2c n)				    ;; ,
	  (= #x2f n)				    ;; /
	  (= #x3b n)				    ;; ;
	  (= #x3f n)))				    ;; ?
     ((eq? 'user mode)				    ;;
      (or (= #x2f n)				    ;; /
	  (= #x3a n)				    ;; :
	  (= #x3f n)				    ;; ?
	  (= #x40 n)))				    ;; @
     ((eq? 'query-component mode) #t)		    ;;
     ((eq? 'fragment mode) #f)			    ;;
     (else #t)))				    ;;
   (else #t)))

(def (url-escaped? u (mode #f))
  (let loop ((l (string-length u)) (n 0))
    (if (>= n l) #t
	(let ((c (char->integer (string-ref u n))))
	  (and (or (= #x25 c) (not (url-escape? c mode)))
	       (loop l (+ 1 n)))))))

(def (url-escape u (mode #f))
  (def len (string-length u))
  (defvalues (space-count hex-count)
    (let loop ((n 0) (space 0) (hex 0))
      (if (= n len)
	(values space hex)
	(let ((c (char->integer (string-ref u n))))
	  (if (url-escape? c mode)
	    (if (and (= #x20 c) (eq? 'query-component mode)) ;; space
	      (loop (+ 1 n) (+ 1 space) hex)
	      (loop (+ 1 n) space (+ 1 hex)))
	    (loop (+ 1 n) space hex))))))
  (if (and (= 0 space-count) (= 0 hex-count)) u
      (utf8->string
       (let loop ((buf (make-u8vector (+ len (* 2 hex-count))))
		  (n 0) (i 0))
	 (if (= n len) buf
	     (let ((c (char->integer (string-ref u n))))
	       (cond ((and (= #x20 c) (eq? 'query-component mode)) ;; space
		      (u8vector-set! buf i #x2b) ;; +
		      (loop buf (+ 1 n) (+ 1 i)))
		     ((url-escape? c mode)
		      (u8vector-set! buf i #x25) ;; %
		      (u8vector-set! buf (+ 1 i) (hex-upper (arithmetic-shift c -4)))
		      (u8vector-set! buf (+ 2 i) (hex-upper (bitwise-and c 15)))
		      (loop buf (+ 1 n) (+ 3 i)))
		     (else
		      (u8vector-set! buf i c)
		      (loop buf (+ 1 n) (+ 1 i))))))))))

(def (url-unescape u (mode #f))
  (def len (string-length u))
  (defvalues (i plus?)
    (let loop ((n 0) (i 0) (plus? #f))
      (if (>= n len)
	(values i plus?)
	(let ((c (char->integer (string-ref u n))))
	  (cond
	   ((= #x25 c)			;; %
	    (when (or (>=  (+ 2 n) len) ;; incomplete hex byte
		      (not (hex-char? (string-ref u (+ 1 n))))
		      (not (hex-char? (string-ref u (+ 2 n)))))
	      (error "escape error at:"
		(substring/shared u (if (> (+ 3 n) len) (+ 3 n) n) len)))
	    (case mode
	      ((host)
	       ;; https://tools.ietf.org/html/rfc3986#page-21
	       ;; https://tools.ietf.org/html/rfc6874#section-2
	       ;; first says %-encoding can only be used for non-ASCII bytes
	       ;; second says %25 could be used to escape percent sign in IPv6 addr
	       (when (not (equal? "%25" (substring/shared u n (+ 3 n))))
		 (error "escape error at:" (substring/shared u n len))))
	      ((zone)
	       (let ((nibble (bitwise-ior
			      (arithmetic-shift (unhex (string-ref u (+ 1 n))) 4)
			      (unhex (string-ref u (+ 2 n))))))
		 (when (and (not (equal? "%25" (substring/shared u n (+ 3 n))))
			    (= #x20 nibble)
			    (url-escape? nibble 'host))
		   (error "escape error at:" (substring/shared u n len)))))
	      (else (loop (+ 3 n) (+ 1 i) plus?))))
	   ((= #x2b c) ;; +
	    (loop (+ 1 n) i (eq? 'query-component mode)))
	   ((and (or (eq? 'host mode)
		     (eq? 'zone mode))
		 (> #x80 c)
		 (url-escape? c mode))
	    (error "invalid host error at:" c (url-escape? c mode) (> #x80 c) (substring/shared u n len)))
	   (else (loop (+ 1 n) i plus?)))))))
  (if (and (eq? 0 i) (not plus?)) u
      (utf8->string
       (let loop ((buf (make-u8vector (- (string-length u) (* 2 i))))
		  (n 0) (i 0))
	 (if (= n len) buf
	     (let ((c (char->integer (string-ref u n))))
	       (cond
		((eq? #x25 c) ;; %
		 (u8vector-set! buf i (bitwise-ior
				       (arithmetic-shift (unhex (string-ref u (+ 1 n))) 4)
				       (unhex (string-ref u (+ 2 n)))))
		 (loop buf (+ 3 n) (+ 1 i)))
		((eq? #x2b c) ;; +
		 (u8vector-set! buf i (if (eq? 'query-component mode) #x20 #x2b)) ;; space +
		 (loop buf (+ 1 n) (+ 1 i)))
		(else
		 (u8vector-set! buf i c)
		 (loop buf (+ 1 n) (+ 1 i))))))))))

;;

(def (url-trim-anchor u)
  (if (= 0 (string-length u))
    u (car (string-split u #\#))))

;;

(def (url-consume-scheme u)
  (def len (string-length u))
  (let loop ((n 0))
    (if (= n len)
      (values #f "" u)
      (let ((c (char->integer (string-ref u n))))
	(cond ((or (and (>= c #x61) (<= c #x7a))  ;; a-z
		   (and (>= c #x41) (<= c #x5a))) ;; A-Z
	       (loop (+ 1 n)))
	      ((or (and (>= c #x30) (<= c #x39))     ;; 0-9
		   (= #x2b c) (= #x2d c) (= #x2e c)) ;; +-.
	       (if (= 0 n)
		 (values #f "" u)
		 (loop (+ 1 n))))
	      ((= #x3a c) ;; :
	       (let ((rest (substring/shared u (+ 1 n) len)))
		 (cond ((string-prefix? "//" rest) ;; drop // prefix
			(values (> n 0)
				(substring/shared u 0 n)
				(substring/shared rest 2)))
		       (else (values #f "" u)))))
	      ;; invalid character, no valid scheme
	      (else (values #f "" u)))))))

(def (url-consume-user u)
  (let* ((delim (string-rindex
		 (substring/shared
		  u 0 (or (string-index u #\/)
			  (string-length u)))
		 #\@)))
    (if delim
      (let* ((user (substring/shared u 0 delim))
	     (len  (string-length user)))
	(let loop ((n 0))
	  (if (= n len)
	    (let ((info (string-split user #\:)))
	      (values #t
		      (cons (url-unescape (car info) 'user)
			    (and (pair? (cdr info))
				 (url-unescape (cadr info) 'user)))
		      (substring/shared u
					(+ 1 delim)
					(string-length u))))
	    (let ((c (char->integer (string-ref user n))))
	      (if (or (and (>= c #x61) (<= c #x7a)) ;; a-z
		      (and (>= c #x41) (<= c #x5a)) ;; A-Z
		      (and (>= c #x30) (<= c #x39)) ;; 0-9
		      (and (>= c #x24) (<= c #x2e)) ;; $%&'()*+,-.
		      (= #x21 c)		    ;; !
		      (= #x3a c)		    ;; :
		      (= #x3b c)		    ;; ;
		      (= #x3d c)		    ;; =
		      (= #x40 c)		    ;; @
		      (= #x5f c)		    ;; _
		      (= #x7e c))		    ;; ~
		(loop (+ 1 n))
		(values #f (list) u))))))
      (values #f (list) u))))

(def (url-consume-host u)
  (let ((slash (string-index u #\/)))
    (if (and slash (= 0 slash))
      (values #f "" u)
      (let* ((end (or slash (string-length u)))
	     (uu (substring/shared u 0 end)))
	(let ((open-bracket (string-index uu #\[)) ;; ipv6
	      (close-bracket (string-index-right uu #\])))
	  (if (and open-bracket (= 0 open-bracket) close-bracket)
	    ;; rfc 6874 defines that %25 (%-encoded percent) introduces
	    ;; the zone identifier, and the zone identifier can use basically
	    ;; any %-encoding it likes. that's different from the host, which
	    ;; can only %-encode non-ascii bytes
	    (let* ((address (substring/shared uu open-bracket (+ 1 close-bracket)))
		   (zone (string-contains address "%25")))
	      (values
	       #t
	       (if zone
		 (string-append/shared
		  (url-unescape (substring/shared address 0 zone) 'host)
		  (url-unescape (substring/shared address zone (+ 1 close-bracket)) 'zone))
		 (url-unescape address 'host))
	       (substring/shared u (+ 1 close-bracket) (string-length u))))
	    (let ((colon (string-index uu #\:)))
	      (values
	       #t
	       (url-unescape
		(if colon (substring/shared uu 0 colon) uu)
		'host)
	       (substring/shared u (or colon end))))))))))

(def (url-consume-port u)
  (let ((colon (string-index u #\:)))
    (if (and colon (= 0 colon))
      (let* ((end (or (string-index u #\/)
		      (string-length u)))
	     (port (substring/shared u 1 end))
	     (ok (let loop ((n 0) (l (string-length port)))
		   (if (>= n l)
		     (> l 0)
		     (let ((c (char->integer (string-ref port n))))
		       (and (<= #x30 c) (>= #x39 c) ;; 0-9
			    (loop (+ 1 n) l)))))))
	(if ok
	  (values
	   #t
	   (string->number port)
	   (substring/shared u end (string-length u)))
	  (values #f #f u)))
      (values #f #f u))))

(def (url-consume-path u)
  (let ((query (or (string-index u #\?) (string-length u)))
	(slash (string-index u #\/)))
    (if (and slash (= 0 slash))
      (values #t
	      (url-unescape (substring/shared u slash query) 'path)
	      (substring/shared u query))
      (values #f "" u))))

(def (url-consume-query u)
  (let ((query (string-index u #\?)))
    (cond
     ((and query (= 0 query))
      (values (> (string-length u) 1)
	      (form-url-decode (substring/shared u 1))
	      ""))
     (else (values #f "" "")))))

(def (string->url s)
  (let*-values (((scheme? scheme rest) (url-consume-scheme (url-trim-anchor s)))
		((user?   user   rest) (url-consume-user rest))
		((host?   host   rest) (url-consume-host rest))
		((port?   port   rest) (url-consume-port rest))
		((path?   path   rest) (url-consume-path rest))
		((query?  query  rest) (url-consume-query rest)))
    (url scheme? scheme
	 user?   user
	 host?   host
	 port?   port
	 path?   path
	 query?  query)))

;;

(def (url-produce-scheme u)
  (if (url-scheme? u)
    (string-append (url-scheme u) "://")
    ""))

(def (url-produce-user u)
  (if (url-user? u)
    (string-append (url-user u) "@")
    ""))

(def (url-produce-host u)
  (if (url-host? u)
    (url-escape (url-host u) 'host)
    ""))

(def (url-produce-port u)
  (if (url-port? u)
    (string-append ":" (number->string (url-port u)))
    ""))

(def (url-produce-path u)
  (if (url-path? u)
    (let ((path (url-path u)))
      (if (url-escaped? path 'path) path
	  (url-escape path 'path)))
    ""))

(def (url-produce-query u)
  (if (url-query? u)
    (string-append "?" (form-url-encode (url-query u)))
    ""))

;;

(def (url->string u)
  (string-append/shared
   (url-produce-scheme u)
   (url-produce-user u)
   (url-produce-host u)
   (url-produce-port u)
   (url-produce-path u)
   (url-produce-query u)))

;;

(def (url-scheme->port scheme (default #f))
  (or (hash-ref (*url-scheme-port*) scheme default)
      (error (format "unknown port number for scheme ~s" scheme))))

(def (url->path u)
  (string-append
   (if (url-path? u) (url-produce-path u) "/")
   (url-produce-query u)))

(def (url-http? url)
  (equal? (url-scheme url) "http"))

(def (url-https? url)
  (equal? (url-scheme url) "https"))
