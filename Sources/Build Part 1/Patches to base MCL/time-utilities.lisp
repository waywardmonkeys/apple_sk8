;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;

(in-package :cl-user)

;;;;;;;;;;
;;; Utilities
;;;;;;;;;;

;;; *** isn't there a system version?
(defun get-time-string (&key time no-time seconds no-date no-year)
  (unless time (setq time (get-universal-time)))
  (multiple-value-bind (secs minutes hours date month year) (decode-universal-time time)
    (format nil "~:[~2d-~2,'0d~@[-~a~]~;~3*~]~
                 ~:[  ~;~]~
                 ~:[~2d:~2,'0d~@[:~2,'0d~] ~am~;~3*~]"
            no-date
            month date (unless no-year (mod year 100))
            (or no-date no-time)
            no-time
            (if (> hours 12) (- hours 12) hours) minutes (when seconds secs)
            (if (>= hours 12) #\p #\a))))

(defun universal-time-string (&optional (num (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time num)
    (with-output-to-string (s)
      (format s "~d:~2,'0d:~2,'0d " hour minute second)
      (princ (nth day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                        "Saturday" "Sunday"))
             s)
      (format s ", ~d " date)
      (princ (nth month '("" "January" "February" "March" "April" "May" "June" "July"
                          "August" "September" "October" "November" "December"))
             s)
      (format s ", ~d" year))))

#|
	Change History (most recent last):
	1	2/25/94	sidney	Move some utility functions to the base build so they can be used by the file scanner
	2	6/18/94	chip	get-time-string now correctly associates "pm" with the noon hour (radar 1169127)
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
