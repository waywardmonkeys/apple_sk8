;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :sk8dev)

(def-logical-directory "sscompiler;" (directory-namestring 
                                         (ccl::loading-file-source-file)))


;; The core of the compiler

(cl-user::sk8-build-files "SSCompiler;Parser Basics.lisp")
(cl-user::sk8-build-files "SSCompiler;Lex.lisp")
(cl-user::sk8-build-files "SSCompiler;Expression Parser.lisp")
(cl-user::sk8-build-files "SSCompiler;Command Parser.lisp")
(cl-user::sk8-build-files "SSCompiler;Debugging.Lisp")
(cl-user::sk8-build-files "SSCompiler;Compiler Interfaces.lisp")




#|
	Change History (most recent last):
	1  	 3/12/96	Brian   	first time check in
	2  	 3/12/96	Brian   	fixing typo
	3  	 3/12/96	Brian   	removing loading of patches.
	2  	 4/15/96	Hernan  	
	3  	 5/20/96	Brian   	
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
