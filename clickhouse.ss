(import :std/db/dbi
	:std/format
	:corpix/gerbilstd/url
	:corpix/gerbilstd/http)
(export clickhouse-open)

(defstruct (clickhouse-connection connection) ()
  final: #t)
(defstruct (clickhouse-statement statement) (connection)
  final: #t)

(def (clickhouse-open url)
  (let* ((u (string->url url)))
    (make-clickhouse-connection
     (lambda (body) ;; TODO: pool of connections (see :std/db/conpool)
       (http-post u body: body)))))

(defmethod {:init! clickhouse-connection}
  connection:::init!)

(defmethod {close clickhouse-connection}
  void) ;; TODO: close pool & all connections

(defmethod {prepare clickhouse-connection}
  (lambda (self sql)
    (clickhouse-statement sql (connection-e self))))

(defmethod {finalize clickhouse-statement}
  void)

(defmethod {bind clickhouse-statement}
  void)

(defmethod {clear clickhouse-statement}
  void)

(defmethod {reset clickhouse-statement}
  void)

(defmethod {exec clickhouse-statement}
  (lambda (self . args)
    (request-content ((clickhouse-statement-connection self)
		      (statement-e self)))))

(defmethod {query-start clickhouse-statement}
  void)

(defmethod {query-fetch clickhouse-statement}
  void)

(defmethod {query-row clickhouse-statement}
  void)

(defmethod {query-fini clickhouse-statement}
  void)

(defmethod {columns clickhouse-statement}
  void)