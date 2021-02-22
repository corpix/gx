(import :std/format
	:std/pregexp
	:std/net/uri)
(export
  url? url url-scheme url-host url-port url-target url-query
  url-host-set! url-port-set! url-query-set! url-scheme-set! url-target-set!
  url-http? url-https?
  url->string string->url url->path
  +scheme-http+ +scheme-https+
  +scheme-http-port+ +scheme-https-port+)

(defconst +scheme-http+       "http")
(defconst +scheme-https+      "https")
(defconst +scheme-http-port+  80)
(defconst +scheme-https-port+ 443)

(def url-rx (pregexp "(?:(https?)://)?([^/:]+)(:[0-9]+)?(/.*)?(?:\\?)?(.+)?"))

;;

(defstruct url (scheme host port target query))

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
	(query  (url-query u)))
    (string-append
     scheme
     "://"
     (url-host u)
     (if (or (equal? scheme +scheme-http+)
	     (equal? scheme +scheme-https+)) ""
	     (format ":~a" (url-port u)))
     (url-target u)
     (if query (form-url-encode query) ""))))

(def (url->path u)
  (let ((query (url-query u)))
    (string-append
     (url-target u)
     (if query (string-append "?" (form-url-encode query)) ""))))