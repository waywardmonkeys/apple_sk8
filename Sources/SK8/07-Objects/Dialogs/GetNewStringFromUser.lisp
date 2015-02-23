;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETNEWSTRINGFROMUSER")

(require "GETANSWERFROMUSER" "objects;Dialogs:GetAnswerFromUser")


(define-handler getNewFromUser (string &key ((:project InProject) nil) 
                                          (message "Please enter a string:"))
  (declare (ignore inproject))
  (getanswerfromuser message :textsize 12 :allowspaces t 
                     :allowreturns nil :allowemptystrings t))

(defmethod getNewFromUser ((me (eql string)) &key ((:project InProject) nil) 
                              (message "Please enter a string:"))
  (declare (ignore inproject))
  (getanswerfromuser message :textsize 12 :allowspaces t 
                     :allowreturns nil :allowemptystrings t))

(define-handler getFromUser (string &key (multipleValues nil) 
                                      ((:project InProject) nil) 
                                      popUpMenu relativeactor)
  (getnewfromuser me :project inproject))



#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
