;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  5-21-96   2:05 pm
                  SK8::UNDEFINED)


(provide "SK8SCRIPTRUNTIME")

(def-logical-directory "SK8Script;" (directory-namestring 
                                       (ccl::loading-file-source-file)))

(define-sk8-constant Undefined *undefined*) 

(cl-user::sk8-build-files "SK8Script;Collections:Load Collections")
(cl-user::sk8-build-files "SK8Script;Runtime:Load Runtime")
(cl-user::sk8-build-files "SK8Script;SKIL:Load SKIL Runtime")


#|
	Change History (most recent last):
	2  	 4/ 3/96	Brian   	
	3  	 4/22/96	Brian   	
	4  	 4/22/96	Brian   	
	5  	 5/21/96	Brian   	
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
