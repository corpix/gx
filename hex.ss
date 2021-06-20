(import :std/text/hex)
(export (import: :std/text/hex)
	hex-upper
	hex-char?)

(defconst +hex+ "0123456789ABCDEF")

(def (hex-upper c)
  (char->integer (string-ref +hex+ c)))

(def (hex-char? c)
  (let ((n (char->integer c)))
    (or (and (>= n #x30) (<= n #x39)) ;; 0-9 or a-f or A-F
	(and (>= n #x61) (<= n #x66))
	(and (>= n #x41) (<= n #x46)))))
