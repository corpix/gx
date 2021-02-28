(import :gerbil/gambit
	:std/srfi/1
	:std/sugar
	:std/iter
	:std/text/json
	:std/text/utf8
	(for-syntax :std/iter
		    :std/format)
	:corpix/gerbilstd/scheme
	:corpix/gerbilstd/list
	:corpix/gerbilstd/string
	:corpix/gerbilstd/proc
	:corpix/gerbilstd/net/http/request)
(export
  *token*
  *url*
  *timeout*
  *poll-timeout*
  *log*

  get-me
  get-updates
  send-message
  send-photo

  dispatch
  for/telegram)

;;

(def *token*        (make-parameter ""))
(def *url*          (make-parameter "https://api.telegram.org"))
(def *timeout*      (make-parameter 5))
(def *poll-timeout* (make-parameter 120))
(def *log*          (make-parameter (lambda xs (displayln xs))))

;;

(def (uri method)
  (string-append (*url*)
		 "/bot" (*token*)
		 "/" method))

;;

(def (encode-query payload)
  (map (lambda (kv)
	 (cons (car kv)
	       (*->string (cdr kv))))
       payload))

(def (encode-json payload)
  (-> payload
      (list->hash-table)
      (json-object->string)
      (string->utf8)))

(def (decode-json request)
  (let (payload (request-json request))
    (unless (hash-ref payload 'ok)
      (error "server responded with an error: "
	(hash-ref payload 'error_code)
	(hash-ref payload 'description "")))
    (hash-ref payload 'result)))

;;

(begin-syntax
  (def (payload-syntax kwargs)
    (def pairs '())
    (for (arg (syntax->list kwargs))
      (let (argp (syntax-e arg))
	(cond
	 ((symbol? argp) ;; required argument
	  (with-syntax ((key (symbol->string argp))
			(value arg))
	    (set! pairs
	      (cons (syntax (cons key value))
		    pairs))))
	 ((pair? argp) ;; optional argument
	  (let* ((sym (car argp))
		 (name (syntax->datum sym)))
	    (with-syntax ((key (symbol->string name))
			  (value sym))
	      (set! pairs
		(cons (syntax (cons key value))
		      pairs))))))))
    (cons 'list pairs)))
;;(kwargs->alist-stx (syntax (foo: (bar 0) baz: (qux 1) ducks: oops)))
;; => (list (cons qux qux) (cons bar bar))

(defsyntax (defapimethod stx)
  (syntax-case stx ()
    ((_ (name http-method method-name)
	(kind arguments ...)
	(encode decode))
     (let ((request-kind (syntax->datum #'kind)))
       (with-syntax* ((headers (case request-kind
				 ((query multipart) (syntax #f))
				 ((json)            (syntax '(("Content-Type" . "application/json"))))))
		      (payload (payload-syntax #'(arguments ...)))
		      (request-key (symbol->keyword
				    (cond
				     ((eq? request-kind 'query)     'query)
				     ((eq? request-kind 'json)      'body)
				     ((eq? request-kind 'multipart) 'multipart)
				     (else (error (format "unsupported request kind: ~a" request-kind)))))))
	 (syntax (def (name arguments ...)
		   (displayln method-name)
		   (with-request (req (http-request http-method (uri method-name)
						    request-key (if encode (encode payload) payload)
						    headers: headers))
				 (decode req)))))))))

(defapimethod
  (get-me 'GET "getMe")
  (query)
  (encode-query decode-json))

(defapimethod
  (get-updates 'GET "getUpdates")
  (query limit:   (limit 100)
	 offset:  (offset 0)
	 timeout: (timeout (*poll-timeout*)))
  (encode-query decode-json))

(defapimethod
  (send-message 'POST "sendMessage")
  (json chat-id: chat_id
	text:    text)
  (encode-json decode-json))

(defapimethod
  (send-photo 'POST "sendPhoto")
  (multipart chat-id:    chat_id
	     photo:      photo
	     caption:    (caption ""))
  (#f decode-json))

;;

(defsyntax dispatch
  (syntax-rules (<>)
    ((dispatch <> case0 cases ...)
     (lambda (t) (dispatch t case0 cases ...)))
    ((dispatch table (key action) ...)
     (let ((t table))
       (and (hash-table? t)
	    (let ((results
		   (cond*
		    ((hash-ref t key #f) => action) ...)))
	      (if (pair? results)
		results #f)))))))

(defsyntax for/telegram
  (syntax-rules ()
    ((for/telegram sym body0 body ...)
     (let ((thunk (lambda (sym) body0 body ...))
	   (offset 0))
       (let loop ((updates (get-updates)))
	 (for ((update updates))
	   (unless (hash-table? update)
	     (error "expected update to be a hash-table, got:" update))
	   (set! offset (hash-ref update 'update_id))
	   (*log* (json-object->string update))
	   (thunk update))
	 (loop (get-updates offset: (+ 1 offset))))))))