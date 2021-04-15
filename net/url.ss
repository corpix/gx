(import :std/format
	:std/pregexp
	:std/net/uri)
(export
  (struct-out url) url-http? url-https? url->string string->url url->path
  +scheme-http+ +scheme-https+
  +scheme-http-port+ +scheme-https-port+)

(defconst +scheme-http+       "http")
(defconst +scheme-https+      "https")
(defconst +scheme-http-port+  80)
(defconst +scheme-https-port+ 443)

(def url-rx (pregexp "(?:(https?)://)?([^/:]+)(:[0-9]+)?(/[^\?]*)?(?:\\?)?(.+)?"))

;;

(defstruct url (scheme host port target query)
  transparent: #t)

(def (url-http? url)
  (equal? (url-scheme url)
	  +scheme-http+))
(def (url-https? url)
  (equal? (url-scheme url)
	  +scheme-https+))

(def (string->url s) ;; TODO: add query?
  (with ([_ scheme host port target query]
         (pregexp-match url-rx s))
    (url
     (or scheme +scheme-http+)
     host
     (cond
      (port (string->number (substring port 1 (string-length port))))
      ((equal? scheme +scheme-https+) +scheme-https-port+)
      (else +scheme-http-port+))
     (or target "/")
     (and query (form-url-decode query)))))

(def (url->string u)
  (let ((scheme (url-scheme u))
	(port   (url-port   u))
	(query  (url-query  u)))
    (string-append
     scheme
     "://"
     (url-host u)
     (if (or (and (eq? port +scheme-http-port+)
		  (equal? scheme +scheme-http+))
	     (and (eq? port +scheme-https-port+)
		  (equal? scheme +scheme-https+)))
       ""
       (format ":~a" (url-port u)))
     (let ((target (url-target u)))
       (if (and (equal? target "/")
		(not query))
	 ""
	 target))
     (if query
       (string-append "?" (form-url-encode query))
       ""))))

(def (url->path u)
  (let ((query (url-query u)))
    (string-append
     (url-target u)
     (if query (string-append "?" (form-url-encode query)) ""))))
