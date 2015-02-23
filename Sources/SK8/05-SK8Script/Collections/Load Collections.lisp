;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(def-logical-directory "collections;" (directory-namestring 
                                          (ccl::loading-file-source-file)))

(cl-user::sk8-build-files "collections;Collection Protocol")
(cl-user::sk8-build-files "collections;Aliased Protocol")
(cl-user::sk8-build-files "collections;Array Protocol")
(cl-user::sk8-build-files "collections;List Protocol")
(cl-user::sk8-build-files "collections;Table Protocol")
(cl-user::sk8-build-files "collections;Text Protocol")
(cl-user::sk8-build-files "collections;Vector Protocol")


#|
	Change History (most recent last):
	4  	 4/22/96	Brian   	
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
