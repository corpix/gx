(import	:corpix/gx/string
	:corpix/gx/list
	:corpix/gx/struct
	:corpix/gx/syntax
	:corpix/gx/match
	:corpix/gx/url
	:corpix/gx/http

	:corpix/gx/test

	:std/interactive)

(defstruct* ast:type (name arguments))
(defstruct* ast:funcall (name arguments))
(defstruct*/zero ast:column (name type null? default materialized alias compression ttl))

(defstruct* ast:placeholder (name))

(defstruct*/zero ast:create-table (if-not-exists? name cluster columns engine))

(defmethod {:emit ast:create-table}
  (lambda (ast) (flatten (map emit (cdr (struct->list ast))))))

(def (emit ast)
  (cond
   ((not ast) "")
   (else {:emit ast})))

;;

(defsyntax (defparser stx)
  "defines a parser macro matching `literal`s collecting values into `type` `key`s"
  (syntax-case stx (=>)
    ((_ consume (literal ...) => type ((sym ...) value => key) ...)
     (with-syntax ((type-constructor (format-id #'type "make-~a" #'type))
		   (consume-arguments (format-id #'consume "~a-arguments" #'consume))
		   (ellipsis #'(... ...)))
       (syntax (begin
		 (defrules consume ()
		   ((_ body ellipsis)
		    (let ((ast (type-constructor)))
		      (consume-arguments ast body ellipsis))))
		 (defrules consume-arguments (literal ...)
		   ((_ ast sym ... rest ellipsis)
		    (begin
		      (struct-set! type ast key value)
		      (consume-arguments ast rest ellipsis)))
		   ... ((_ ast) ast))))))))

;;

(defrules funcall (quote)
  ((_ (quote name)) 'name)
  ((_ (name argument ...)) (ast:funcall 'name (list (funcall argument) ...)))
  ((_ name) name))

(check (funcall (foo (bar 'baz)))
       => (ast:funcall 'foo (list (ast:funcall 'bar '(baz)))))

;;

(defrules type (quote)
  ((_ (quote name)) (ast:type 'name #f))
  ((_ (name argument ...)) (ast:type 'name (list (type argument) ...)))
  ((_ name) name))

(check (type (array (tuple 'string 'uint8 (fixed-string 13))))
       => (ast:type 'array
		    (list (ast:type 'tuple
				    (list (ast:type 'string #f)
					  (ast:type 'uint8 #f)
					  (ast:type 'fixed-string '(13)))))))

;;

(defrules placeholder (quote)
  ((_ (quote name)) (ast:placeholder 'name))
  ((_ (expr ...)) ((placeholder expr) ...))
  ((_ name) (ast:placeholder name)))

;;

(defparser ~create-table-column-aux
  (:name :null :not-null :default :materialized :alias :compression :ttl)
  => ast:column
  ((:name value) value => name)
  ((:type value) value => type)
  ((:null) #t => null?)
  ((:non-null) #f => null?)
  ((:default expr) (funcall expr) => default)
  ((:materialized expr) (funcall expr) => materialized)
  ((:alias expr) (funcall expr) => alias)
  ((:compression expr) (funcall expr) => compression)
  ((:ttl expr) (funcall expr) => ttl))

(defparser create-table (:if-not-exists :name :columns :engine) => ast:create-table
	   ((:if-not-exists) #t => if-not-exists?)
	   ((:name value) value => name)
	   ((:columns ((description ...) ...))
	    (list (~create-table-column-aux description ...) ...) => columns)
	   ((:engine value) value => engine))

;; FIXME: matches garbage for some reason, but should display "bad syntax" error
(check (create-table :name "foos"
		     :if-not-exists
		     :columns ((:name 'ts :type 'DateTime)))
       => (make-struct* ast:create-table::t
			(if-not-exists? #t)
			(name "foos")
			(cluster #f)
			(columns (list (make-struct* ast:column::t
						     (name 'ts)
						     (type 'DateTime)
						     (null? #f)
						     (default #f)
						     (materialized #f)
						     (alias #f)
						     (compression #f)
						     (ttl #f))))
			(engine #f)))

(check (create-table :name (placeholder 'name)
		     :if-not-exists
		     :columns ((:name 'ts :type 'DateTime)))
       => (make-struct* ast:create-table::t
			(if-not-exists? #t)
			(name (ast:placeholder 'name))
			(cluster #f)
			(columns (list (make-struct* ast:column::t
						     (name 'ts)
						     (type 'DateTime)
						     (null? #f)
						     (default #f)
						     (materialized #f)
						     (alias #f)
						     (compression #f)
						     (ttl #f))))
			(engine #f)))

;; (displayln (emit (create-table :name (placeholder 'name)
;; 			       :if-not-exists
;; 			       :columns ((:name 'col :type 'uint8 :ttl 1)))))

;;

;; (defrules ~insert-aux ()
;;   ((_ rest ...)
;;    `("INSERT INTO" ,@(~insert-arguments-aux rest ...))))

;; (defrules ~insert-arguments-aux (:name :columns :values)
;;   ((_ :name name rest ...)
;;    `(name ,@(~insert-arguments-aux rest ...)))
;;   ((_ :columns (column ...) rest ...)
;;    `("(" ,@(list 'column ...) ")"
;;      ,@(~insert-arguments-aux rest ...)))
;;   ((_ :values ((value ...) ...) rest ...)
;;    `("VALUES" ,@(list "(" (~funcall-aux value) ... ")") ...
;;      ,@(~insert-arguments-aux rest ...)))
;;   ((_) '()))

;; ;;

;; (clickhouse
;;  ;; (create-table :if-not-exists
;;  ;; 	       :name hello
;;  ;; 	       :columns ((timestamp date-time)
;;  ;; 			 (request-id uint)))
;;  (insert
;;   :name hello
;;   :columns (timestamp request-id)
;;   :values (((to-date-time 666) 777)
;; 	   ((to-date-time 888) 999))))
;; => CREATE TABLE IF NOT EXISTS hello ( timestamp NULL DEFAULT CODEC ( LZ4HC ( 10 ) ) )
