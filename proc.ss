(import (for-syntax :std/stxutil
		    :gerbil/expander/stx))
(export ->
	->>)

(defsyntax (-> stx)
  (syntax-case stx ()
    ((_ value) (syntax value))
    ((_ value (proc args ...) rest ...)
     (syntax (-> (proc value args ...) rest ...)))))

(defsyntax (->> stx)
  (syntax-case stx ()
    ((_ value) (syntax value))
    ((_ value (proc args ...) rest ...)
     (syntax (->> (proc args ... value) rest ...)))))
