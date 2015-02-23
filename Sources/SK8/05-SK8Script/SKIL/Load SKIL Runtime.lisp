;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;;;   Runtime Functions for SK8Script

(defpackage :skil (:use :ccl :cl))

(def-logical-directory "skil;" (directory-namestring 
                                  (ccl::loading-file-source-file)))

(cl-user::sk8-build-files "skil;sk8script primitives.lisp")
(cl-user::sk8-build-files "skil;collection mappers.lisp")

#|
	Change History (most recent last):
	1  	 4/ 3/96	Brian   	
	2  	 4/22/96	Brian   	
	3  	 4/30/96	Brian   	
	5  	10/22/96	sidney  	compiler patch now totally obsolete
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
