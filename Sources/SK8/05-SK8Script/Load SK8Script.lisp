;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "SK8SCRIPT")

;;;------------------------------------------------------------------------------------
;;Our Two Globals:
(defparameter skil::*Compile-With-Debugging* t)
(defparameter skil::*current-project* sk8::sk8)

;;;------------------------------------------------------------------------------------

(cl-user::sk8-build-files "SK8Script;Skil:Load SKIL Compiler")
(cl-user::sk8-build-files "SK8Script;SK8Script to Skil:Load Compiler")

(cl-user::sk8-build-files "SK8Script;Store:RAM Store"
                          "SK8Script;Store:Load Script File")

(cl-user::sk8-build-files "Sk8Script;Collections:WriteObject Collections")

#|
	Change History (most recent last):
	2  	 4/ 3/96	Brian   	
	3  	 4/ 3/96	Hernan  	Now loading the RAM store and load script file.
	4  	 4/22/96	Brian   	
	5  	 5/13/96	Hernan  	Loading writeObject for collections.
	6  	 5/21/96	Brian   	
	7  	 7/ 8/96	Brian   	turning of debugging stuff for now.
	8  	 8/ 5/96	Brian   	
	9  	10/15/96	sidney  	remove redundant definition of sk8script; logical directory
	10 	10/17/96	sidney  	remove dummy definition of sk8-error now that we've written the real one
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
