(in-package :SK8Development)

(provide "DATETIME")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#|
The DateTime object is provided to allow convenient storage and type coercion of calender dates.
For example, the DateTime object can coerce text strings such as "September 21, 1968", 
"21 sept 68", and "9/21/68" into a value which can be compared to other DateTimes.
The DateTime object uses storage similar to the Macintosh's, so the range of allowable dates
begins on January 1, 1904, and ends in February, 2040.

DateTime Properties:
---------------

SecondsSince1904 -- this is an integer representing the number of seconds between 
Midnight, January 1, 1904, and the calendar date you are representing. The default value
is the number of seconds between jan 1 1904 and the time at which the new DateTime object was
made (according to the clock in your Macintosh).  The DateTime object is designed to be 
subclassed--the SK8 user should never want or need to change DateTime itself, only DateTime's
children.

DateTime Handlers:
--------------

DateString -- Getter keywords: numericForm, abbreviation, dayOfWeek
              Setter Keywords: none
Allows you to get a the string representation of the date, or to set the date based on a string.
If ShortForm is true, it returns a numeric string, for example "9/21/68". 
If DayOfWeek is true, the day of week is prepended to the string, separated by a comma. This 
works only when ShortFrom is false.  For example "Tuesday, February 2, 1993"
If Brevity is true, the string is in abbreviated form, e.g., "Feb 2, 1993" (or "Tue, Feb 2, 1993")
Note that if you have a child of DateTime, BeginVactaion, and you set the datestring of begin vacation
to "4 jun 1993", when you get the datestring of BeginVacation, the text will be formatted: 
"June 4, 1993".

Day -- sets or returns the day of month of the date, as an integer.  
"get the day of beginVacation" returns 4
"set the day of beginVacation to 11" changes beginvacation from june 4 to june 11

Month -- sets or returns the month of the year of the date, as an integer.
"get the month of beginVacation" returns 6
"set the month of beginVacation to 7" changes beginvacation from june 11 to july 11

Year --  sets or returns the year of the date, as an integer.
"get the year of beginVacation" returns 1993
"set the year of beginVacation to 1994" changes beginvacation from june 11, 1993 to july 11, 1994
N.B.: Don't set the year to 94 if you mean 1994.

The following are only getters:

DayOfWeek -- returns an integer between 1 and 7, representing the day of the week
             for a given date, Sun = 1, Sat = 7
set the datestring of beginvacation to "28 june 93"
get the dayofweek of beginvacation   ->returns 2, for Monday

DayName -- keyword: brevity (boolean). 
           similar to DayOfWeek, but it returns a string, e.g., "Monday" or "Mon"

DayOfYear -- returns an integer representing the number of days since the year has begun.
get the dayofyear of beginvacation  -> returns 178
get the dayofyear of newYearsDay    -> returns 1


Note that there is no DayOfMonth -- that's what Day is.

DaysBetween --pass it two DateTimes, it tells you how many days apart they are.
new DateTime with objectname "myBirthDay" with datestring "21 sept 68"
daysbetween mybirthday, Now -> returns 8900 
daysbetween Now, mybirthday -> returns -8900
daysbetween Now, Now -> returns 0

Monthname --  keyword: brevity (boolean). 
              returns a string corresponding to the month of the DateTime you passed
get the monthname of mybirthday  -> returns "September"
get the monthname of Now with brevity -> returns "Feb"

Monthsbetween --pass it two DateTimes, and it returns the number of months apart they are
                note that dec 31 and jan 1 are a month apart, although they are only a day apart
monthsbetween mybirthday, Now -> returns 293

DaysInMonth --tells you the number of days in a given month.  Gets leapyear right.
DaysinMonth Now -> returns 28


The Now Object
----------------
Now is a child of DateTime, whose value is bound to the clock in the Macintosh.  Now is designed 
to be used as a handy reference.  You can't set Now's values, you can only get them.

The Now Function
----------------
Now is a function that takes no arguments.   It returns the number of seconds since 1/1/1904, according
to your Mac.
get now()  -> returns 2811529348

DateTime as part of SK8Script
-------------------------
Children of DateTime may be coerced into strings (or text) or integers (or numbers) 
using classic SK8script coercion.
get Now as a string  -> returns "February 2, 1993"
get Now as a number  -> returns 2811528694 , the seconds since 1904

Children of DateTime may be compared with predicates such as =, <>, <, >, etc.  "A < B" means A is before B.
get mybirthday < Now  -> returns true. 

|#

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(defvar *date-scrap-cache*)

(def-load-pointers init-date-scrap-cache ()
  (setq *date-scrap-cache* (make-record :dateCacheRecord))
  (#_initDateCache *date-scrap-cache*))


(defun current-secs-since-1904 ()
  (%get-unsigned-long (%int-to-ptr 524)))


(defun numToLongDateTime (num LDT)
  (let ((hi (ash num -32))
        (lo (logand #xFFFFFFFF num)))
    (%put-long LDT hi)
    (%put-long LDT lo 4)))


(defun longDateTimeToNum (LDT)
  (logior (ash (%get-long LDT) 32)
          (%get-unsigned-long LDT 4)))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new Object
     :objectname "DateTime"
     :project SK8
     :properties '(secondsSince1904))

(define-handler initialize (DateTime original isNew initArgs)
  (declare (ignore isNew initArgs))
  (setf original (originalAncestor me original DateTime))
  (setf (secondsSince1904 me)
        (if (eq original DateTime)
          (* 86400 (floor (secondsSince1904 original) 86400))
          (secondsSince1904 original))))

(define-handler localVirtualProperties (DateTime)
  '(secondsSince1904 dateString DayName MonthName
    timeString day month year era hour minute
    seconds ticks ticksPerSecond dayOfWeek
    dayOfYear weekOfYear pm daysInMonth))

(define-handler secondsSince1904 (DateTime)
  (if (eq me DateTime)
    (current-secs-since-1904)
    (slot-value me 'secondsSince1904)))

(define-handler (setf secondsSince1904) (secs DateTime)
  (when (eq me DateTime)
    (raiseUnsettablePropertyError 'secondsSince1904 me))
  (setf (slot-value me 'secondsSince1904) (require-type secs integer)))

(define-handler writeObject (DateTime theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (when rereadably (write-string "the DateTime " theStream))
    (writeObject (dateString me :numericForm t) theStream t)))

(define-handler dateString (DateTime &key (numericForm nil) (abbreviation nil) (dayOfWeek nil))
  (let* ((the-itl1-rez (#_GetIntlResource 1))
         (old-suppressDay (rref the-itl1-rez intl1rec.suppressDay)))
    (if dayOfWeek
      (rset the-itl1-rez intl1rec.suppressDay 0)
      (rset the-itl1-rez intl1rec.suppressDay #xFF)) 
    (rlet  ((OSresult  :str255)
            (theDateTime :longDateTime))
      (numToLongDateTime (secondsSince1904 me) theDateTime)
      (if numericForm 
        (#_LongDateString theDateTime 0 OSresult (%null-ptr))
        (if abbreviation
          (#_LongDateString theDateTime #x200 OSresult the-itl1-rez)
          (#_LongDateString theDateTime #x100 OSresult the-itl1-rez)))
      (rset the-itl1-rez intl1rec.suppressDay old-suppressDay)
      (%get-string OSresult)
      )))

(define-handler (setf dateString) (str DateTime)
  (check-type str string)
  (when (zerop (length str))
    (sk8-error IncorrectArgumentsError
                 :handlername "set dateString"
                 :arguments (list str)))
  (rlet ((lengthused  :longint)
         (theDateTime :longDateTime)
         (theDateRec :longDateRec)
         (strptr  :str255))
    (let ((mylen (length str))
          (certainty))
      (%put-string strptr str)
      (numToLongDateTime (secondsSince1904 me) theDateTime)
      (#_LongSecs2Date theDateTime theDateRec)
      (setq certainty (#_String2Date (%inc-ptr strptr) mylen *date-scrap-cache* lengthused theDateRec))
      
      (when (or (eq certainty 0) (eq certainty 1)) 
        (#_LongDate2Secs theDateRec theDateTime)
        (setf (secondsSince1904 me) (LongDateTimeToNum theDateTime))))))


(define-handler dayName (DateTime)
  (let ((d (- (dayOfWeek me) 1)))
    (rref (#_GetIntlResource 1) (Intl1rec.days d) :storage :pointer)))

(define-handler monthName (DateTime)
  (let ((m (- (month me) 1)))
    (rref (#_GetIntlResource 1) (Intl1rec.months m) :storage :pointer)))

(define-handler timeString (DateTime &key (seconds nil))
  (rlet  ((OSresult  :str255)
          (theDateTime :longDateTime))
    (numToLongDateTime (secondsSince1904 me) theDateTime)
    (#_LongTimeString theDateTime seconds OSresult (%null-ptr))    
    (%get-string OSresult)))


(define-handler (setf timeString) (str DateTime)
  (check-type str string)
  (when (zerop (length str))
    (sk8-error IncorrectArgumentsError
                 :handlername "set timeString"
                 :arguments (list str)))
  (rlet ((lengthused  :longint)
         (theDateRec :longDateRec)
         (strptr  :str255)
         (theDateTime :longDateTime))
    (let ((mylen (length str))
          (certainty))
      (%put-string strptr str)
      (numToLongDateTime (secondsSince1904 me) theDateTime)
      (#_LongSecs2Date theDateTime theDateRec)
      (setq certainty (#_String2Time (%inc-ptr strptr) mylen *date-scrap-cache* lengthused theDateRec))
      (when (or (eq certainty 0) (eq certainty 1)) 
        (#_LongDate2Secs theDateRec theDateTime)
        (setf (secondsSince1904 me) (LongDateTimeToNum theDateTime))))))

(define-handler getDateTimeComponent :private (DateTime component)
                (getDateTimeComponentFromSeconds (secondsSince1904 me) component))

(define-handler setDateTimeComponent :private (DateTime component newValue)
                (setDateTimeComponentFromSeconds (secondsSince1904 me) component newValue))

(symbol-macrolet ((component-era         0)
                  (component-year        2)
                  (component-month       4)
                  (component-day         6)
                  (component-hour        8)
                  (component-minute     10)
                  (component-seconds    12)
                  (component-dayOfWeek  14)
                  (component-dayOfYear  16)
                  (component-weekOfYear 18)
                  (component-pm         20))
  
  (defun getDateTimeComponentFromSeconds (secondsSince1904 component)
    (rlet ((theDateRec  :LongDateRec)
           (theDateTime :LongDateTime))
      (numToLongDateTime secondsSince1904 theDateTime)
      (#_LongSecs2Date theDateTime theDateRec)
      (%get-word theDateRec component)))
  
  (defun setDateTimeComponentFromSeconds (secondsSince1904 component newValue)
    (rlet ((theDateRec  :LongDateRec)
           (theDateTime :LongDateTime))
      (numToLongDateTime secondsSince1904 theDateTime)
      (#_LongSecs2Date theDateTime theDateRec)
      (%put-word theDateRec newValue component)
      (cond
       ((= component component-month)
        (let* ((oldDay   (%get-word theDateRec component-day))
               (thisYear (%get-word theDateRec component-year))
               (lastDay  (days-in-month-of-year newValue thisYear)))
          (when (> oldDay lastDay)
            (%put-word theDateRec lastDay component-day))))
       ((= component component-year)
        (let* ((oldDay    (%get-word theDateRec component-day))
               (thisMonth (%get-word theDateRec component-year))
               (lastDay   (days-in-month-of-year thisMonth newValue)))
          (when (> oldDay lastDay)
            (%put-word theDateRec lastDay component-day)))))
      (#_LongDate2Secs theDateRec theDateTime)
      (LongDateTimeToNum theDateTime)))
  
  (define-handler era        (DateTime)                   (getDateTimeComponent me component-era))
  (define-handler year       (DateTime)                   (getDateTimeComponent me component-year))
  (define-handler month      (DateTime)                   (getDateTimeComponent me component-month))
  (define-handler day        (DateTime)                   (getDateTimeComponent me component-day))
  (define-handler hour       (DateTime)                   (getDateTimeComponent me component-hour))
  (define-handler minute     (DateTime)                   (getDateTimeComponent me component-minute))
  (define-handler seconds    (DateTime)                   (getDateTimeComponent me component-seconds))
  (define-handler dayOfWeek  (DateTime)                   (getDateTimeComponent me component-dayOfWeek))
  (define-handler dayOfYear  (DateTime)                   (getDateTimeComponent me component-dayOfYear))
  (define-handler weekOfYear (DateTime)                   (getDateTimeComponent me component-weekOfYear))
  (define-handler pm         (DateTime)         (let ((pm (getDateTimeComponent me component-pm)))
                                                      (cond
                                                       ((eq pm *undefined*)                   pm)
                                                       (t                   (%boolean-to-lisp pm)))))

  (define-handler (setf era       ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-era        newValue)))
  (define-handler (setf year      ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-year       newValue)))
  (define-handler (setf month     ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-month      newValue)))
  (define-handler (setf day       ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-day        newValue)))
  (define-handler (setf hour      ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-hour       newValue)))
  (define-handler (setf minute    ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-minute     newValue)))
  (define-handler (setf seconds   ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-seconds    newValue)))
#| These aren't this simple.  They need more work
  (define-handler (setf dayOfWeek ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-dayOfWeek  newValue)))
  (define-handler (setf dayOfYear ) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-dayOfYear  newValue)))
  (define-handler (setf weekOfYear) (newValue DateTime) (setf (secondsSince1904 me) (setDateTimeComponent me component-weekOfYear newValue)))
|#
  )

(define-handler (setf pm) (newValue DateTime)
  (when (xor (pm me) newValue)
    (if newValue
      (setf (hour me) (+ (hour me) 12))
      (setf (hour me) (- (hour me) 12))))
  newValue)

(define-handler ticks (DateTime)
  0)

(define-handler ticksPerSecond (DateTime)
  1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; some calculation handlers

(define-handler daysBetween (DateTime date2)
  (when (inheritsFrom date2 DateTime)
    (- (floor (secondsSince1904 date2) 86400)
       (floor (secondsSince1904 me)    86400))))

(define-handler yearAndMonth :private (DateTime)
  (rlet ((theDateRec :LongDateRec)
         (theDateTime :longDateTime))
    (numToLongDateTime (secondsSince1904 me) theDateTime)
    (#_LongSecs2Date theDateTime theDateRec)
    (values
     (%get-word theDateRec 2)  ;year
     (%get-word theDateRec 4)  ;month
     )))

(define-handler monthsBetween (DateTime date2)
  (when (inheritsFrom date2 DateTime)
    (multiple-value-bind (y1 m1) (yearAndMonth me)
      (multiple-value-bind (y2 m2) (yearAndMonth date2)
        (format t "~%~A~%~A" m1 m2)
        (+ (* 12 (- y2 y1))
           (- m2 m1))))))

(defun days-in-month-of-year (m Y)
  (case m
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (otherwise (if (and (= 0 (rem y 4))
                        (or (/= 0 (rem y 100))
                            (=  0 (rem y 400))))
                 29
                 28))))


(define-handler daysInMonth (DateTime)
  (multiple-value-bind (y m) (yearAndMonth me)
    (days-in-month-of-year m y)))

(define-handler objectAsNumber (DateTime &key type ((:project proj)))
  (declare (ignore proj))
  (cond
   ((null (secondsSince1904 me))
    (call-next-method))
   ((and type (neq type Integer))
    (objectAsNumber (secondsSince1904 me) :type type))
   (t
    (secondsSince1904 me))))

(define-handler objectAsString (DateTime &key ((:project proj)))
  (declare (ignore proj))
  (if (secondsSince1904 me)
    (dateString me)
    (call-next-method)))

(define-handler numberAs (DateTime obj &key ((:project proj)))
  (new me :project proj :secondsSince1904 (round obj)))

(define-sk8-function dateStringToAbsoluteTime nil (theString)
  (check-type theString string)
  (when (zerop (length theString))
    (sk8-error IncorrectArgumentsError
                 :handlername "set dateString"
                 :arguments (list theString)))
  (rlet ((dateCache :dateCacheRecord)
         (lengthused  :longint)
         (longDate :longDateRec)
         (secsfrom04 :longDateTime)
         (strptr  :str255))
    (%put-string strptr theString)
    ;;;(clear-record longDate longDateRec) this ain't working, so i use the workaround below
    (dotimes (i 7)
      (%put-long longDate 0 (* 4 i)))
    (#_initDateCache dateCache)
    (let ((certainty (#_String2Date (%inc-ptr strptr) (length theString) dateCache lengthused longDate)))
      (unless (or (eq certainty 0) (eq certainty 1))
        (sk8-error IncorrectArgumentsError
                   :handlername "set dateString"
                   :arguments (list theString))))
    (#_LongDate2Secs longdate secsfrom04)
    (%get-unsigned-long secsfrom04 4)))

(define-handler stringAs (DateTime obj &key ((:project proj)))
  (let ((secs (dateStringToAbsoluteTime obj)))
    (if secs
      (new me :project proj :secondsSince1904 secs)
      (call-next-method))))


(define-handler equalTo (DateTime other &key exactMatch)
  (declare (ignore exactMatch))
  (when (and other (inheritsfrom other DateTime) (secondsSince1904 other) (secondsSince1904 me))
    (= (secondsSince1904 me)
       (secondsSince1904 other))))

(define-handler greaterThan (DateTime other)
  (when (and other (inheritsfrom other DateTime) (secondsSince1904 other) (secondsSince1904 me))
    (> (secondsSince1904 me)
       (secondsSince1904 other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moon.lisp
;;;
;;; Two functions, lunarPhase and AgeOfMoon, that work with SK8's date object.
;;; By Alan Peterson
;;; May 28, 1993
;;; lunarPhase returns a number between 0 and 1.  
;;;           0 is a new moon, 1 is a full moon.
;;; ageOfMoon returns how far around the moon has gone since the last new moon
;;;           Values returned are in radians, from 0 to 2Pi
;;;           0 is new moon
;;;           Pi is fullmoon

;;; Based on the book Practical Astronomy with Your Calculator, Third Edition,
;;;   by Peter Duffett-Smith.  Cambridge University Press, 1988.
;;;   Thanks to John Miller of the Coca-Cola Company for pointing me in the 
;;;   direction of that book.   
;;;



(defun reduce-Angle (angle)
  (when (< angle 0)
    (setq angle (reduce-Angle (+ angle 360))))
  (when (>= angle 360)
    (setq angle (reduce-Angle (- angle 360))))
  angle)

(defun lunar-Phase-from-Mac-Seconds (secsSince1904)
  (let* ((piover180 (/ pi 180))
         (SecondsNaught 2713910400)  ;;seconds between 1904 and 1990
         (epsilon-g 279.403303)      ;;ecliptic longitude at epoe 1990.0 (section 46, table 6)
         (omega-g 282.768422)        ;;ecliptic longitude of perigee (section 46, table 6)
         (e 0.016713)                ;;eccentricity of [earth-sun] orbit (section 46, table 6)
         (l-naught  318.351648)      ;;Moon's mean longitude at the epoch (section 65, table 10)
         (P-naught 36.340410)        ;;Mean longitude of perigee at the epoch (section 65, table 10)
         (D (/ (- secsSince1904 SecondsNaught) 86400))               ;;section 46, step 1 & 2
         (N (reduce-angle (/ (* D 360) 365.242191)))                 ;;section 46, step 3
         (M-Sun (reduce-angle (- (+ N epsilon-g) omega-g)))          ;;section 46, step 4
         (E-c (/ (* (sin (* M-Sun piover180)) e 360) pi))            ;;section 46, step 5
         (lambda-Sun (reduce-Angle (+ N E-c epsilon-g)))             ;;section 46, step 6
         (l (reduce-Angle (+ l-naught (* D 13.1763966))))            ;;section 65, step 4
         (M-m (reduce-Angle (- l (* 0.1114041 D) P-naught)))         ;;section 65, step 5
         (E-v (* 1.2739 (sin (* piOver180 
                                (- (* 2 (- l lambda-sun)) 
                                   M-m)))))                          ;;section 65, step 7
         (A-e (* 0.1858 (sin (* piOver180 M-sun))))                  ;;section 65, step 8
         (A-3 (* 0.37 (sin (* piOver180 M-sun))))                    ;;section 65, step 8
         (M-m-prime (- (+ M-m E-v) A-e A-3))                         ;;section 65, step 9
         (EE-c (* 6.2886 (sin (* piOver180 M-m-prime))))             ;;section 65, step 10
         (A-4 (* 0.214 (sin (* 2 piOver180 M-m-prime))))
         (l-prime (- (+ l E-v EE-c A-4) A-e))
         (V (* 0.6583 (sin (* piOver180 2 (- l-prime lambda-sun))))) ;;section 65, step 13
         (l-prime-prime (+ V l-prime))
         (DD (reduce-Angle(- l-prime-prime lambda-sun)))             ;;section 67, step 2
         (F (* 0.5 (- 1 (cos (* piOver180 DD)))))
         )
    (values 
     (/ (round (* 1000000 F)) 1000000.0)  ;;lunar phase
     (* dd piOver180)                     ;;Age of moon, in radians
     )))


(define-handler lunarPhase (DateTime)
  (nth-value 0 (lunar-Phase-from-Mac-Seconds (secondsSince1904 me))))

(define-handler ageOfMoon (DateTime)
  (nth-value 1 (lunar-Phase-from-Mac-Seconds (secondsSince1904 me))))

#| Modification History

03-04-93 alan  added simple time functions (get hour, minute, sec).
02-22-93 ruben inheritsFrom? -> inheritsFrom
09-23-93 alan  changed SetAbsoluteTimeByCalender to account for setting month of "jan 31" to 2 -> "feb 28" not "mar 2";
               added days-In-Month-of-Year;
               changed define-SK8-handlers to defuns for internal stuff

|#

#|
	Change History (most recent last):
	2		6/10/93	Hernan	Introduced Alan's new moon phase stuff.
	8		8/31/93	hernan	Added the timeString handler.
	9		9/1/93	hernan	The great integrating build for d4!!!
	10		9/24/93	hernan	Alan Peterson makes guest appeareance to fix bug.
	11		11/22/93	hernan	Alan added intToTimeOfDay.
	12		12/21/93	sidney	Changes so files can be compiled
	13		1/11/94	hernan	self -> me
	14		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15		2/22/94	hernan	second -> seconds.
	16		2/25/94	chip	added project arg to objectAs; made printObject print literal form of date
	17		2/28/94	chip	made printObject only print literal form of date when printing rereadably
	18		3/3/94	Hernan	The great handler argument name renaming of 94!
	19		3/6/94	chip	fixed coercion handlers
	20		3/6/94	chip	print... --> write...
	21		6/10/94	chip	oops -- writeObject of Date was responding to its 'rereadably' arg wrong; fixed it
	22 	 8/31/94	chip    	fixed typo in Today's incrementByDays handler (radar #1184052)
	23 	 9/19/94	chip    	folded in Al's new version
	24 	 9/20/94	dy      	take out the 3 deftraps
	25 	 9/20/94	Hernan  	Adding the ticks handler of Now, which returns the
							number of ticks since the machine started running
							which is a number comparable to the eventTime().
	26 	 9/22/94	sidney  	declare date-scrap-cache in the correct place so as not to generate bogus warning messages
	27 	11/11/94	chip    	added 'exactMatch' arg to equalTo handler
	28 	 2/16/95	sidney  	readable argument names for initialize handler
	1  	 3/13/95	dy      	fix spelling error
	2  	 3/14/95	dy      	more cleanup and speedup.  Also, made pm settable and fixed bug when day is last day of the month and you set the month - it now works like when setting year
	3  	 3/21/95	Hernan  	1230547: fixed localVirtualProperties of DateTime
							and all handlers of DateTimeIndirect.
	4  	 3/21/95	dy      	oops dayOfYear was missing
	5  	 3/28/95	dy      	add dayOfYear to virtual properties
	6  	 3/28/95	dy      	remove some setters that don't work: dayOfWeek, dayOfYear, weekOfYear
	7  	 4/ 4/95	Hernan  	1235882: dateStringToAbsoluteTime is redefined from the
							B2 sources (for some reason, the function has been excised
							from B3 even though some handlers call it!)
	8  	 4/18/95	dy      	When DateTimeIndirect is not initialized correctly, return #undefined#
							for various properties instead of raising an error.
							Change writeObject to output "the DateTime blah" instead of "the Date blah".  Other minor fixes
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	11/15/96	Hernan  	Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
