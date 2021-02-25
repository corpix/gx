(import :gerbil/gambit/ports
	:gerbil/gambit/exceptions
	:std/crypto
	:std/sugar
	:std/text/utf8
	:std/text/hex)
(export #t)

(def (temporary-file-name entropy: (entropy 4) . prefix)
  (string-join
   (append prefix
	   (list (hex-encode (random-bytes entropy))))
   "."))

(def (delete-file-if-exists path)
  (try
   (delete-file path)
   (catch (no-such-file-or-directory-exception? e)
     (void))))

(def (call-with-temporary-file thunk
			       directory: (directory (current-directory))
			       prefix:    (prefix   '())
			       keep:      (keep?    #f))
  (let* ((name (apply temporary-file-name prefix))
	 (path (path-normalize (path-expand name directory)))
	 (port (open-file (list path: path
				create: #t))))
    (try
     (thunk path port)
     (finally (close-port port)
	      (or keep? (delete-file-if-exists path))))))