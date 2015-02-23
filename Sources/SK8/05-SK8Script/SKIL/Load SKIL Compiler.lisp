;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;;;   The SKIL Compiler

(cl-user::sk8-build-files "skil;Skil Compiler")
;;(cl-user::sk8-build-files "skil;ScriptX Skil Compiler")
(cl-user::sk8-build-files "skil;Type Inference")
(cl-user::sk8-build-files "skil;Java Skil Compiler")
(cl-user::sk8-build-files "skil;Java Indentor")

#|
	Change History (most recent last):
	1  	 4/ 3/96	Brian   	
	3  	 4/29/96	Brian   	Adding new ScriptX SKIL Compiler.
	4  	 7/11/96	Brian Roddy	Adding java skil compiler
	5  	 9/16/96	Brian   	
	6  	 9/19/96	Brian   	
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
