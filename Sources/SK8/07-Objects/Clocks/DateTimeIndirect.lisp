;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "DATETIMEINDIRECT")

(Require "DATETIME" "objects;Clocks:DateTime")

(new DateTime
     :objectName "DateTimeIndirect"
     :project SK8
     :undisposable t)

(addProperty DateTimeIndirect 'getterFunction  :initialValue nil)
(addProperty DateTimeIndirect 'getterArguments :initialValue '())

(addProperty DateTimeIndirect 'setterFunction  :initialValue nil)
(addProperty DateTimeIndirect 'setterArguments :initialValue '())

(define-handler initialize (DateTimeIndirect original isNew initArgs)
  (when isNew
    (raiseInsufficientInitializersError
     original
     '(getterFunction
       getterArguments)
     :argumentsToCheck initArgs))
  (sk8-return-nothing))

(define-handler privateSecondsSince1904 :private (DateTimeIndirect)
                       (if (eq me DateTimeIndirect)
                         (call-next-method)
                         (let ((args (getterArguments me)))
                           (if args
                             (apply   (getterFunction me) (first args) (rest args))
                             (funcall (getterFunction me))))))

(define-handler secondsSince1904 (DateTimeIndirect)
  (or (ignore-errors (privateSecondsSince1904 me))
      *undefined*))

(define-handler (setf secondsSince1904) (newValue DateTimeIndirect)
  (if (eq me DateTimeIndirect)
    (call-next-method)
    (let ((setter (setterFunction me)))
      (if setter
        (apply   setter newValue (setterArguments me))
        (raiseUnsettablePropertyError 'secondsSince1904 me)))))

(define-handler writeObject (DateTimeIndirect theStream rereadably)
  (declare (ignore theStream rereadably))
  (ignore-errors (call-next-method)))

(define-handler getDateTimeComponent :private (DateTimeIndirect component)
                (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
                  (or (and secondsSince1904
                           (getDateTimeComponentFromSeconds (secondsSince1904 me) component))
                      *undefined*)))

(define-handler dateString (DateTimeIndirect &key (numericForm nil) (abbreviation nil) (dayOfWeek nil))
  (declare (ignore numericForm abbreviation dayOfWeek))
  (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
    (or (and secondsSince1904
             (call-next-method))
        *undefined*)))

(define-handler dayName (DateTimeIndirect)
  (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
    (or (and secondsSince1904
             (call-next-method))
        *undefined*)))

(define-handler monthName (DateTimeIndirect)
  (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
    (or (and secondsSince1904
             (call-next-method))
        *undefined*)))

(define-handler timeString (DateTimeIndirect &key (seconds nil))
  (declare (ignore seconds))
  (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
    (or (and secondsSince1904
             (call-next-method))
        *undefined*)))

(define-handler daysInMonth (DateTimeIndirect)
  (let ((secondsSince1904 (ignore-errors (privateSecondsSince1904 me))))
    (or (and secondsSince1904
             (call-next-method))
        *undefined*)))

#| TESTS

;;; error
(new DateTimeIndirect :project SK8)
(new DateTimeIndirect :project SK8
     :getterFunction 'T_GetMovieModificationTime
     :getterArguments (list (mediaData sk8::m1)))

|#

;;; _______________________________ 
;;; Now & Today
;;; _______________________________ 

(new DateTimeIndirect
     :objectname "Now"
     :project sk8
     :undisposable t
     :getterFunction #'current-secs-since-1904
     :getterArguments '())

(define-SK8-constant Today Now :register t :project SK8)


#|
	Change History (most recent last):
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
