;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)



(def-logical-directory "SSRuntime;" (directory-namestring 
                                       (ccl::loading-file-source-file)))


(cl-user::sk8-build-files "SSRuntime;coercion")
(cl-user::sk8-build-files "SSRuntime;comparator-handlers")
(cl-user::sk8-build-files "SSRuntime;characters")
(cl-user::sk8-build-files "SSRuntime;library-functions")
(cl-user::sk8-build-files "SSRuntime;lists-&-sequences")
(cl-user::sk8-build-files "SSRuntime;math")
(cl-user::sk8-build-files "SSRuntime;operators")
(cl-user::sk8-build-files "SSRuntime;predicates")



#|
	Change History (most recent last):
	2  	 4/16/96	Brian   	getting rid of delegation
	3  	 4/16/96	Brian   	adding characters
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
