;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETNEWSYMBOLFROMUSER")

(require "GETANSWERFROMUSER" "objects;Dialogs:GetAnswerFromUser")


(define-handler getNewFromUser (Symbol &key ((:project InProject) nil) 
                                          (message "Please enter a symbol:"))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (let (returnedValue)
    (setf returnedValue (getanswerfromuser message :textsize 12 :defaultAnswer nil
                                           :allowSpaces t :allowReturns nil 
                                           :allowEmptyStrings nil))
    (setf returnedValue (intern-symbol returnedValue (SK8::package inproject)))
    returnedValue))

(defmethod getNewFromUser ((me (eql Symbol)) &key ((:project InProject) nil) 
                              (message "Please enter a symbol:"))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (let (returnedValue)
    (setf returnedValue (getanswerfromuser message :textsize 12 :defaultAnswer nil
                                           :allowSpaces t :allowReturns nil 
                                           :allowEmptyStrings nil))
    (setf returnedValue (intern-symbol returnedValue (SK8::package inproject)))
    returnedValue))

(define-handler getFromUser (symbol &key (multipleValues nil) 
                                      ((:project InProject) nil)
                                      popUpMenu relativeactor)
  (getnewfromuser me :project inproject))




#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
