(import :std/crypto
	:std/format
	:std/misc/ports
	:std/text/hex
	:std/text/utf8
	:gerbil/gambit
	:corpix/gerbilstd/u8vector
	:corpix/gerbilstd/string)
(export form? form form-boundary form-fields
	form->u8vector
	form-field? form-field form-field-parms form-field-mime form-field-payload
	validate-boundary random-boundary
	content-type
	write!)

;;

(defstruct form
  (boundary fields))

(defstruct form-field
  (parms mime payload))

;;

(def +cr+                  (char->integer #\return))
(def +lf+                  (char->integer #\newline))
(def +crlf+                (u8vector +cr+ +lf+))
(def +dash+                (char->integer #\-))
(def +colon+               (char->integer #\:))
(def +space+               (char->integer #\space))
(def +colon-space+         (u8vector +colon+ +space+))
(def +double-quote+        (char->integer #\"))
(def +magic+               (u8vector +dash+ +dash+))
(def +content-type+        (string->utf8 "Content-Type"))
(def +content-disposition+ (string->utf8 "Content-Disposition"))

(defconst +boundary-size+ 30)
(def +boundary-escape+
  (list->u8vector
   (map ;; rfc2045 tspecials
     char->integer
     '(#\( #\) #\< #\>
       #\@ #\, #\; #\:
       #\" #\/ #\[ #\]
       #\? #\= #\space))))

;;

(def (validate-boundary boundary)
  ;; modelled after https://golang.org/src/mime/multipart/writer.go#L45
  ;; rfc2046#section-5.1.1
  (def len (u8vector-length boundary))
  (cond
   ((or (= len 0) (> len 70))
    (error "invalid boundary length"))
   (else (let loop ((n 0))
	   (if (= n len)
	     #t ;; reached boundary end
	     (let (chr (integer->char (u8vector-ref boundary n)))
	       (cond
		((or (and (char-ci>=? chr #\A) (char-ci<=? chr #\Z))
		     (and (char>=?    chr #\0) (char<=?    chr #\9)))
		 (loop (+ n 1)))
		((memq chr '(#\' #\( #\)
			     #\+ #\_ #\,
			     #\- #\. #\/
			     #\: #\= #\?))
		 (loop (+ n 1)))
		((and (char-whitespace? chr)
		      (not (= (+ n 1) len)))
		 (loop (+ n 1)))
		(else (error (format "invalid boundary character at position ~a: ~s" n chr))))))))))

(def (random-boundary)
  (string->utf8 (hex-encode (random-bytes +boundary-size+))))

(def (content-type boundary)
  (let* ((escape? (u8vector-contains-any boundary +boundary-escape+))
	 (boundary (if escape?
		     (u8vector-append
		      (u8vector +double-quote+)
		      boundary
		      (u8vector +double-quote+))
		     boundary)))
    (string-append "multipart/form-data; boundary=" (utf8->string boundary))))

;;(content-type (string->utf8 "test"))

(def (content-disposition type (rest '()))
  ;; TODO: better support for disposition-parm rfc2183#page-3
  (let loop ((acc type) (parms rest))
    (cond
     ((pair? parms)
      (let (parm (car parms))
	(loop
	 (string-append acc "; "
			(car parm) "=" (format "~s" (cdr parm)))
	 (cdr parms))))
     (else acc))))

;;(content-disposition "form-data" '(("name" . "key1") ("foo" . "bar")))

(def (write! port form)
  (let* ((boundary  (form-boundary form))
	 (fields    (form-fields form))
	 (delimiter (u8vector-append +magic+ boundary)))
    (let loop ((rest fields))
      (cond
       ((pair? rest)
	(let* ((field   (car rest))
	       (name    (car field))
	       (value   (cdr field))
	       ((values parms mime payload)
		(cond
		 ((string? value) (values #f #f value))
		 ((port?   value) (values '(("filename" . "file"))
					  "application/octet-stream"
					  value))
		 ((form-field? value) (values (form-field-parms   value)
					      (form-field-mime    value)
					      (form-field-payload value)))
		 (else
		  (error (format "expected string/port/form-field, got: ~a"
				 value))))))

	  (write-u8vector delimiter port)
	  (write-u8vector +crlf+ port)
	  ;;
	  (write-u8vector +content-disposition+ port)
	  (write-u8vector +colon-space+ port)
	  (write-u8vector (string->utf8
			   (content-disposition
			    "form-data"
			    (append (list (cons "name" name))
				    (or parms '()))))
			  port)
	  (write-u8vector +crlf+ port)
	  ;;
	  (when mime
	    (write-u8vector +content-type+ port)
	    (write-u8vector +colon-space+  port)
	    (write-u8vector (string->utf8 mime) port)
	    (write-u8vector +crlf+ port))
	  ;;
	  (write-u8vector +crlf+ port)
	  (cond
	   ((string? payload) (write-u8vector (string->utf8 payload) port))
	   ((port? payload)   (copy-port payload port))
	   (else (error "unexpected payload type: ~a" payload)))
	  (write-u8vector +crlf+ port))
	(loop (cdr rest)))))
    (write-u8vector delimiter port)
    (write-u8vector +magic+ port)
    (write-u8vector +crlf+ port)))

(def (form->u8vector form)
  (let ((out (open-output-bytes)))
    (write! out form)
    (get-output-bytes out)))

;; (def p (open-output-bytes))

;; (write!
;;  p
;;  (form
;;   (random-boundary)
;;   (list
;;    (cons "foo" "foo val")
;;    (cons "bar" "bar val")
;;    (cons "test-file" (open-input-bytes (u8vector 1 2 3)))
;;    (cons "test-file-with-params" (form-field
;; 				  '(("filename" . "untitled.png"))
;; 				  "image/png"
;; 				  (open-input-bytes (u8vector 1 2 3)))))))

;; (utf8->string (get-output-bytes p))