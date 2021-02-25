(import :std/srfi/19)
(export one-nanosecond
	one-microsecond
	one-millisecond
	one-second
	one-minute
	one-hour
	one-day
	one-week

	hour-duration
	day-duration

	current-unix-time

	time->time-utc
	time->unix

	unix-microseconds
	unix-milliseconds
	unix-seconds

	(import: :std/srfi/19))

;; TODO: see more https://github.com/fare/gerbil-utils/blob/master/timestamp.ss

(def one-nanosecond   1)
(def one-microsecond  1000)
(def one-millisecond (* 1000 1000))
(def one-second      (* 1000 1000 1000))
(def one-minute      (* 60 one-second))
(def one-hour        (* 60 one-minute))
(def one-day         (* 24 one-hour))
(def one-week        (* 7 one-day))

(def (minute-duration i)
  (make-time time-duration 0 (* 60 i)))
(def (hour-duration i)
  (make-time time-duration 0 (* 3600 i)))
(def (day-duration i)
  (make-time time-duration 0 (* 86400 i)))

(def (current-unix-time)
  (time->unix (date->time-utc (current-date))))

;;

(def (time->time-utc time)
  (case (time-type time)
    ((time-utc) time)
    ((time-tai) (time-tai->time-utc time))
    ((time-monotonic) (time-monotonic->time-utc time))
    (else (error "time cannot be converted to utc" time))))

(def (time->unix time)
  (unless (member (time-type time) '(time-utc time-duration))
    (set! time (time->time-utc time)))
  (+ (time-nanosecond time)
     (* one-second (time-second time))))

(def (unix-microseconds unix)
  (exact->inexact (/ unix one-microsecond)))
(def (unix-milliseconds unix)
  (exact->inexact (/ unix one-millisecond)))
(def (unix-seconds unix)
  (exact->inexact (/ unix one-second)))