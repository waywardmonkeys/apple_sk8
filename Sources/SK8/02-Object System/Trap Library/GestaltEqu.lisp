;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :SK8Dev)

(defun_X T_Gestalt nil (selector &key OSErr)
  "Returns the signed-long response.  If there was an error, raises an error.
Or, if OSErr keyword is non-nil, returns two values on error:
nil and the error result code."
  (rlet ((result :signed-long))
    (if OSErr
      (let ((osErr (#_Gestalt selector result)))
        (if (= osErr #$noErr)
          (values_X (%get-long result) osErr)
          (values_X nil osErr)))
      (progn
        (checking-toolbox-error (:OSErr) (#_Gestalt selector result))
        (%get-long result)))))

#|
	Change History (most recent last):
	1	3/2/94	dy	
	2	3/2/94	Yost	
	3  	 2/24/95	dy      	Add OSErr keyword arg to T_Gestalt
	4  	 2/24/95	dy      	oops.  use values_X
	5  	 2/26/95	dy      	new OSErr keyword arg to T_Getstalt
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
