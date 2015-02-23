;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETNEWNUMBERFROMUSER")

(require "GETANSWERFROMUSER" "objects;Dialogs:GetAnswerFromUser")

(define-handler getNewFromUser (number &key ((:project InProject) nil) 
                                          (message "Please enter a number:"))
  (declare (ignore inproject))
  (let* ((theString (getanswerfromuser message :textsize 12 :allowspaces t 
                                       :allowreturns nil :allowemptystrings t))
         (theNum (read-from-string theString nil nil)))
    (when (or (null theNum) (not (numberp theNum)))
      (error "A number was expected."))
    theNum))
 
(defmethod getNewFromUser ((me (eql Number)) &key ((:project InProject) nil) 
                              (message "Please enter a number:"))
  (declare (ignore inproject))
  (let* ((theString (getanswerfromuser message :textsize 12 :allowspaces t 
                                       :allowreturns nil :allowemptystrings t))
         (theNum (read-from-string theString nil nil)))
    (when (or (null theNum) (not (numberp theNum)))
      (error "A number was expected."))
    theNum))

(define-handler getFromUser (number &key (multipleValues nil) 
                                      ((:project InProject) nil)
                                      popUpMenu relativeactor)
  (getnewfromuser me :project inproject))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
