(import :std/srfi/19
	:std/format)
(export +iso8601-format+
	+iso8601-nano-format+

	nanosecond
	microsecond
	millisecond
	second
	minute
	hour
	day
	week

	nanosecond-duration
	microsecond-duration
	millisecond-duration
	second-duration
	minute-duration
	hour-duration
	day-duration
	week-duration
	duration-unit
	string->duration

	unix-microseconds
	unix-milliseconds
	unix-seconds

	time->time-utc
	time->unix-nano

	time->date
	date->time

	time->string
	string->time

	(import: :std/srfi/19))

;;

(defconst +iso8601-format+      "~Y-~m-~dT~H:~M:~S~z")    ;; 2006-01-02T15:04:05-0700
(defconst +iso8601-nano-format+ "~Y-~m-~dT~H:~M:~S.~N~z") ;; 2006-01-02T15:04:05.999999999-0700

;;

(def nanosecond   1)
(def microsecond  1000)
(def millisecond (* 1000 1000))
(def second      (* 1000 1000 1000))
(def minute      (* 60 second))
(def hour        (* 60 minute))
(def day         (* 24 hour))
(def week        (* 7 day))

;;

(def (make-duration i unit)
  (let ((ns (* i unit))
	(s 0))
    (when (or (<= -999999999 ns)
	      (<= ns 999999999))
      (set! s  (quotient ns second))
      (set! ns (remainder ns second)))
    (make-time time-duration ns s)))

(def (nanosecond-duration i)  (make-duration i nanosecond))
(def (microsecond-duration i) (make-duration i microsecond))
(def (millisecond-duration i) (make-duration i millisecond))
(def (second-duration i)      (make-duration i second))
(def (minute-duration i)      (make-duration i minute))
(def (hour-duration i)        (make-duration i hour))
(def (day-duration i)         (make-duration i day))
(def (week-duration i)        (make-duration i week))

(def (duration-unit u)
  (case u
    ((w)  week)
    ((d)  day)
    ((h)  hour)
    ((m)  minute)
    ((s)  second)
    ((ms) millisecond)
    ((µs) microsecond)
    ((ns) nanosecond)
    (else (error (format "unexpected duration unit ~s" u)))))

;; TODO: implement duration->string
;; (def (duration->string)
;;   )

(def (string->duration s)
  (def signs  '(#\+ #\-))
  (def digits '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
  (def units  '(#\w #\d #\h #\m #\s #\n #\µ))
  ;;
  (def (flush acc amount unit)
    (if (and (pair? amount)
	     (pair? unit))
      (cons (cons amount unit) acc)
      acc))
  (def (tokenize s)
    (let loop ((input   (string->list s))
	       (n        0)
	       (acc    '())
	       (amount '())
	       (unit   '()))
      (if (pair? input)
	(let ((c (car input)))
	  (cond
	   ((memq c units)
	    (loop (cdr input) (+ n 1)
		  acc
		  amount
		  (cons (string->symbol (string c)) unit)))
	   ((memq c digits)
	    (let ((next-acc (flush acc amount unit)))
	      (unless (eq? acc next-acc)
		(set! acc next-acc)
		(set! amount '())
		(set! unit   '()))
	      (loop (cdr input) (+ n 1)
		    acc
		    (cons (string->number (string c)) amount)
		    unit)))
	   ((memq c signs)
	    (when (> n 0)
	      (error "unexpected duration sign at" (+ n 1)))
	    (loop (cdr input) (+ n 1)
		  (cons (string->symbol (string c)) acc)
		  amount
		  unit))
	   (else (error (format
			 "unexpected token ~s at ~a"
			 (string c) (+ n 1))))))
	(flush acc amount unit))))
  (def (normalize tree)
    (let loop ((acc  '())
	       (rest tree))
      (if (pair? rest)
	(let ((node (car rest)))
	  (loop
	   (cons (if (pair? node)
		   (let ((amount (car node))
			 (unit   (cdr node)))
		     (cons
		      (let normalize-amount ((acc 0) (m 1) (rest amount))
			(cond
			 ((pair? rest)
			  (normalize-amount (+ acc (* m (car rest)))
					    (* m 10)
					    (cdr rest)))
			 (else acc)))
		      (let normalize-unit ((acc "") (rest unit))
			(cond
			 ((pair? rest)
			  (normalize-unit (cons (symbol->string (car rest)) acc)
					  (cdr rest)))
			 (else (string->symbol (string-join acc "")))))))
		   node)
		 acc)
	   (cdr rest)))
	acc)))
  (def (fold tree)
    (if (pair? tree)
      (let ((sign (let ((fst (car tree)))
		    (if (and (symbol? fst) (eq? fst '-))
		      (begin0 -1 (set! tree (cdr tree)))
		      +1)))
	    (amount (let loop ((acc 0) (rest tree))
		      (if (pair? rest)
			(loop (+ acc (* (caar rest)
					(duration-unit (cdar rest))))
			      (cdr rest))
			acc))))
	(make-duration (* sign amount) nanosecond))
      (make-duration 0 nanosecond)))
  ;;
  (fold (normalize (tokenize s))))

;; (parse-duration "1m6s15ms10µs666ns")  ;; => #<time #77 type: time-duration nanosecond: 15010666 second: 66>
;; (parse-duration "-1m6s15ms10µs666ns") ;; => #<time #77 type: time-duration nanosecond: -15010666 second: -66>

;;

(def (unix-microseconds unix) (exact->inexact (/ unix microsecond)))
(def (unix-milliseconds unix) (exact->inexact (/ unix millisecond)))
(def (unix-seconds unix)      (exact->inexact (/ unix second)))

;;

(def (time->time-utc time)
  (case (time-type time)
    ((time-utc)        time)
    ((time-tai)       (time-tai->time-utc time))
    ((time-monotonic) (time-monotonic->time-utc time))
    (else (error "unsupported time type" time))))

(def (time->unix-nano time)
  (unless (member (time-type time) '(time-utc time-duration))
    (set! time (time->time-utc time)))
  (+ (time-nanosecond time)
     (* second (time-second time))))

;;

(def (time->date time . tz-offset)
  (apply
      (case (time-type time)
	((time-utc)       time-utc->date)
	((time-tai)       time-tai->date)
	((time-monotonic) time-monotonic->date)
	(else (error "unsupported time type" time)))
    time tz-offset))

(def (date->time date) (date->time-utc date))

;;

(def (time->string time . format-str)
  (apply date->string (time->date time) format-str))

(def (string->time str format-str)
  (date->time (string->date str format-str)))
