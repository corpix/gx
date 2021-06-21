(import :std/format
	:std/srfi/13
	:std/text/utf8)
(export (import: :std/format)
	(import: :std/srfi/13)
	(import: :std/text/utf8)
	char-control?
	*->string
	string-contains-any
	string-contains-control)

(def (char-control? c)
  (let ((n (char->integer c)))
    ;; n < space && n == 0x7f (127)
    (or (< n #x20) (= n #x7f))))

;;

(def (*->string s)
  (cond
   ((string? s)   s)
   ((u8vector? s) (utf8->string s))
   ((number? s)   (number->string s))
   ((symbol? s)   (symbol->string s))
   ((keyword? s)  (keyword->string s))))

(def (string-contains-any s chrs)
  (def chrs-len (string-length chrs))
  (cond
   ((= chrs-len 0) #f)
   (else (let loop ((n 0))
	   (if (< n chrs-len)
	     (let* ((chr (string-ref chrs n))
		    (i (string-index s chr)))
	       (if i
		 (cons i chr)
		 (loop (+ n 1))))
	     #f)))))

(def (string-contains-control s)
  (let loop ((n (- (string-length s) 1)))
    (and (>= n 0)
	 (or (char-control? (string-ref s n))
	     (loop (- n 1))))))