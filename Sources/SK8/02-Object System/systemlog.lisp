;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  THE SYSTEMLOG PROTOCOL:
;;;
;;;  There is one systemLog at a time, as given by SK8's systemLog property (which may be set to
;;;  whatever object you want, or FALSE to disable logging).
;;;
;;;  The systemLog object needs to have these two handlers:    insertInto  &  getAttention
;;;  For the textual representation of a logged object it should use logObjectString (or writeLogObject).
;;;
;;;  ANY object may be inserted into the systemLog.  If an object being logged wants to write into the
;;;  log something different than its objectString, it can specialize writeLogObject (analogous to
;;;  the writeObject handler, but doesn't have a 'rereadably' argument)
;;;

(define-handler #~writeLogObject (Object strm)
  (writeObject me strm t))

(define-SK8-function #~logObjectString nil (obj &key ((:project proj)))
  (with-output-to-string (strm)
    (when proj (setf (targetProject strm) proj))
    (writeLogObject obj strm)))

(define-SK8-function #~sendToLog nil (obj &key attention)
  (let ((sysLog (systemLog SK8)))
    (when sysLog
      (when (nth-value 1 (ignore-errors
                          (insertInto sysLog obj)
                          (when attention (getAttention sysLog))))
        (let* ((objStr (ignore-errors (objectString obj)))
               (logObjStr (ignore-errors (logObjectString obj)))
               (same? (equal logObjStr objStr)))
          (when (or objStr logObjStr)
            (messageToUser (concatenate 'string
                                        "Error inserting "
                                        (or objStr "a log object")
                                        (unless same? " (Ò")
                                        (unless same? logObjStr)
                                        (unless same? "Ó)")
                                        " into SK8's systemLog")
                           ; width height
                           ))))))
  obj)

(define-SK8-function #~canLogit nil ()
  (let ((sysLog (systemLog SK8)))
    (when (and sysLog (canDo 'insertInto sysLog))
      t)))


;;; Note:  No getAttention handlers are defined by default, but the symbol must still be declared in SK8
;;;      so that all projects have access to it
;;;
(SK8-declare-syms :SK8 :public #~SK8::getAttention)



#|
	Change History (most recent last):
	1	11/29/93	chip	new file
	2	12/13/93	chip	added some necessary SPECIAL declarations
	3	1/11/94	hernan	self -> me
	4	2/12/94	kleiman	logit -> sendToLog
	5	2/21/94	chip	explicitly declared printLogObject public (since it's defined with defmethod)
	6	2/25/94	chip	printLogObject now assumes targetProject for printing is given by the stream; logObjectString now takes a project arg
	7	3/2/94	kleiman	define-handler for Object patch
	8	3/6/94	chip	print... to write... & new writeObject api
	9	3/6/94	chip	added getAttention declaration
	10 	10/24/94	chip    	obsoleted applicableHandler
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
