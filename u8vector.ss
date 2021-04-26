(import :std/text/utf8)
(export *->u8vector
	u8vector-index
	u8vector-contains-any)

(def (*->u8vector v)
  (cond
   ((string? v) (string->utf8 v))
   ((u8vector? v) v)
   ((char? v) (u8vector (char->integer v)))
   (else (error "unsupported value given:" v))))

(def (u8vector-index vec octet (start 0) (end (u8vector-length vec)))
  (let loop ((index start))
    (cond
     ((>= index end) #f)
     ((= octet (u8vector-ref vec index)) index)
     (else (loop (+ 1 index))))))

(def (u8vector-contains-any vec octets)
  (def len (u8vector-length octets))
  (cond
   ((= len 0) #f)
   (else (let loop ((n 0))
	   (if (< n len)
	     (let* ((octet (u8vector-ref octets n))
		    (i (u8vector-index vec octet)))
	       (if i
		 (cons i octet)
		 (loop (+ n 1))))
	     #f)))))
