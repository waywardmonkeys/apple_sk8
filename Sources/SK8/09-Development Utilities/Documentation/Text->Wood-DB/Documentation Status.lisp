;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

;;; This file defines a function that outputs to the listener the
;;; names of everything that has no documentation. The list is done
;;; by responsible engineer...

;;; This is an alist which is sorted by engineer.

(defvar *missing-docs* nil)

(defun nodocs? (str)
    (string= str ""))

(defun collect-missing-handlers (obj)
  (let ((result nil))
    (map-handler-doc-entries 
     #'(lambda (theFun)
         (when (nodocs? (handler_description theFun))
           (push (handler_name theFun) result)))
     obj)
    (when result
      (pushnew "Handlers" result))
    result
    ))

(defun collect-missing-properties (obj)
  (let ((result nil))
    (map-property-doc-entries
     #'(lambda (theProp)
         (when (and (nodocs? (property_description theProp))
                    (nodocs? (property_getter_description theProp))
                    (nodocs? (property_setter_description theProp)))
           (push (property_name theProp) result)))
     obj)
    (when result
      (pushnew "Properties" result))
    result
    ))

;;; Two types of strings:
;;; "(engineer: Brian, modified: 8-12-94)" 
;;; "(engineer: Adam)" 

(defun extract-owner (str)
  (let ((colonPos (position #\: str))
        commaPos)
    (when colonPos
      (setf commaPos (position #\, str :start colonPos))
      (if commaPos
        (subseq str (+ colonPos 2) commaPos)
        (subseq str (+ colonPos 2) (position #\) str :start colonPos))))))
  
(defun register-missing-docs (ownerStr things)
  (let ((ownerList (car (member ownerStr *missing-docs* :key #'car :test #'string-equal))))
    (if ownerList
      (nconc ownerList (list things))
      (push (list ownerStr things) *missing-docs*))))
  
(defun whine-about-object (theObject)
  (print (object_name theObject))
  (let ((docsMissing? nil)
        (thingsToDocument nil)
        missingHandlers missingProperties)
    ;;; Description missing?
    (when (nodocs? (object_description theObject))
      (setf docsMissing? t)
      (push (format nil "~a and its description." (object_name theObject)) thingsToDocument))
    ;;; Handlers missing?
    (when (setf missingHandlers (collect-missing-handlers theObject))
      (setf docsMissing? t)
      (setf thingsToDocument (append thingsToDocument (list missingHandlers))))
    ;;; Properties missing?
    (when (setf missingProperties (collect-missing-properties theObject))
      (setf docsMissing? t)
      (setf thingsToDocument (append thingsToDocument (list missingProperties))))
    ;; Ok, anything to do? If so figure out who the engineer is and assing whole
    ;; list to him.
    (when docsMissing?
      (unless (stringp (car thingsToDocument))
        (push (object_name theObject) thingsToDocument))
      (register-missing-docs (extract-owner (object_owner theObject))
                             thingsToDocument))))
  
(defun report-missing-docs ()
  (setf *missing-docs* nil)
  (with-help-file
    (map-object-doc-entries #'whine-about-object)))

(defun brief-report ()
  (terpri)
  (dolist (c *missing-docs*)
    (format t "Items missing documentation for ~a: ~a.~%"
            (car c) (1- (length c)))))

(defun print-with-indentation (theList &optional (indentation " "))
  (cond ((null theList) nil)
        ((atom theList) (format t "~%~a~a" indentation theList))
        (t (let ((newIndentation (concatenate 'string indentation indentation)))
             (dolist (c theList)
               (print-with-indentation c newIndentation))))))

;;; List format: (name obj1 -> objn thing1 -> thingn)
;;; objects are lists. Functions and constants are strings.

(defun report-on-object (objList)
  (format t "~%  ~a" (car objList))
  (dolist (c objlist)
    (when (listp c)
      (cond ((string-equal (car c) "Handlers")
             (format t "~%   Handlers: ")
             (dolist (aHandler (cdr c))
               (format t "~%     ~a" aHandler)))
            ((string-equal (car c) "Properties")
             (format t "~%   Properties: ")
             (dolist (aHandler (cdr c))
               (format t "~%     ~a" aHandler))))))
  (terpri))
             
(defun long-report ()
  (terpri)
  (dolist (c *missing-docs*)
    (format t "~%+++++++++++++++++++++++++++++++++")
    (format t "~%Missing Documentation for ~a:" (car c))
    (format t "~%+++++++++++++++++++++++++++++++++")
    (dolist (objOrGlobal (cdr c))
      (report-on-object objOrGlobal))
    (terpri)))

#|

(report-missing-docs)
(brief-report)
(long-Report)

|#
#|
	Change History (most recent last):
	1  	12/16/94	Hernan  	New file. Implements report of missing docs from
							the doc DB.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
