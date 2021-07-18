#!/usr/bin/env gxi
(import :std/make
        :gerbil/gambit/misc)

(def (build-spec)
  `(
    "bnf"
    "exception"
    "fs"
    "hash"
    "hex"
    "http"
    "list"
    "iter"
    "match"
    "multipart"
    "proc"
    "scheme"
    "struct"
    "string"
    "telegram"
    "test"
    "type"
    "time"
    "url"
    "vector"

    (gsc: "libyaml"
          "-cc-options" ,(cppflags "libyaml" "")
          "-ld-options" ,(ldflags "libyaml" "-lyaml"))
    (ssi: "libyaml")
    "yaml"))

(def (main . args)
  (def (build)
    (make srcdir: "."
	  verbose: 0
	  optimize: #t
	  (build-spec)))
  (match args
    ('("meta")    (write '("spec" "compile")) (newline))
    ('("spec")    (pretty-print build-spec))
    ('("compile") (build))
    ('()          (build))))
