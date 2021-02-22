(import :std/srfi/1)
(export split-by
	pairs)

(def (split-by lst n)
  (if (not (eq? lst '()))
    (cons (take lst n)
	  (split-by (drop lst n) n))
    '()))

(def (pairs lst)
  (split-by lst 2))
