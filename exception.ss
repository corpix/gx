(import :gerbil/gambit/exceptions
	(only-in :std/sugar try catch finally))
(export error-with-message?
	(import: :gerbil/gambit/exceptions)
	try
	catch
	finally)

(def (error-with-message? message)
  (lambda (e)
    (and (error-exception? e)
	 (equal? (error-exception-message e) message))))