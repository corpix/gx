(import :gerbil/expander)
(export (import :gerbil/expander))

(def (x) "foo" (+ 1 x))

(binding-id (resolve-identifier 'x))
(binding-key (resolve-identifier 'x))
