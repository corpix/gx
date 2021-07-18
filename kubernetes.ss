(import :std/sugar
	:corpix/gx/hash
	:corpix/gx/proc
	:corpix/gx/yaml)
(export pod)

(def *api-version* (make-parameter 'v1))
(def *namespace* (make-parameter #f))

(defrules ~containers-aux (name image env)
  ((_ (name value))
   (list name: value))
  ((_ (image url))
   (list image: url))
  ((_ (env (key value) ...))
   (list env: (list (make-hash (list name: key)
			       (list value: value))
		    ...))))

(defrules ~labels-aux ()
  ((_ (label value)) (list label value)))

(defrules ~metadata-aux (namespace labels)
  ((_ (namespace value))
   (list namespace: value))
  ((_ (labels value ...))
   (list labels: (make-hash (~labels-aux value) ...))))

(defrules ~spec-aux (containers)
  ((_ (containers (value ...)))
   (list containers: (list (make-hash (~containers-aux value)
				      ...)))))

(defrules ~manifest-aux (api-version kind metadata spec)
  ((_ (api-version value))
   (list apiVersion: value))
  ((_ (kind value))
   (list kind: value))
  ((_ (metadata value ...))
   (list metadata: (make-hash (~metadata-aux value) ...)))
  ((_ (spec value ...))
   (list spec: (make-hash (~spec-aux value) ...))))

(defrules pod ()
  ((pod rest ...)
   (yaml-dump-string
    (make-hash (~manifest-aux (api-version (*api-version*)))
	       (~manifest-aux (kind 'Pod))
	       (~manifest-aux rest) ...))))

;;

(let ((user "nobody"))
  (map displayln
       (list
	(pod
	 (metadata (labels (foo: 'bar) (baz: 'qux)))
	 (spec (containers
		((name "shell")
		 (image 'nixos/nix:latest)
		 (env ('NIX_OPTS '--help)
		      ('USER user))))))
	(pod
	 (metadata (labels (foo: 'bar) (baz: 'qux)))
	 (spec (containers
		((name "shell")
		 (image 'nixos/nix:latest)
		 (env ('NIX_OPTS '--help)
		      ('USER user)))))))))
