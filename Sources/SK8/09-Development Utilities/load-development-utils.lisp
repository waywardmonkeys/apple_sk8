;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :cl-user)

(provide :load-development-utils)

(defparameter mf::*SK8-Dev-packages*
  (list (list (find-package :SK8)
              (find-package :SK8Dev)
              "SK8Development" 
              "SK8"
              #P"ccl:SK8;Preload;SK8PublicSyms.lisp"
              #P"ccl:SK8;Preload;SK8PrivateSyms.lisp"
              #P"ccl:SK8;Preload;SK8SpecialSyms.lisp")
        (list (find-package :UI)
              (find-package :UIDev)
              "UIDevelopment"
              "UI"
              #P"ccl:SK8;Preload;UIPublicSyms.lisp"  
              #P"ccl:SK8;Preload;UIPrivateSyms.lisp"
              #P"ccl:SK8;Preload;UISpecialSyms.lisp")))

;;; For now, it's all or nothing (as per the :development flag) -- in the future might want 
;;; to make the individual files selectable:

(when *development-p*
  (let ((*fasl-save-local-symbols* t)
        (*record-source-file* t)
        (*save-definitions* t)
        (*save-doc-strings* t)
        (*save-local-symbols* t)
        (*load-verbose* t))
    
    (sk8-build-files "SK8;09-Development Utilities:development-package-utils"
                     "SK8;09-Development Utilities:lisp-dev-utils"
                     "SK8;09-Development Utilities:General:fred-sk8script-commands"
                     "SK8;09-Development Utilities:General:Documentation Utilities"
                     "SK8;09-Development Utilities:Toolbox Lookup"
                     )

    ))


#|
	Change History (most recent last):
	2	7/6/93	chip	added "development-package-utils" to files loaded
	3	9/2/93	hernan	We no longer load the 2d utilities.
	4	12/14/93	sidney	Define *sk8-dev-packages* even when building a on-developer version
	5	12/14/93	kleiman	Correct a slip of the brain
	6	12/17/93	till	faslling
	7	12/21/93	sidney	Changes so files can be compiled
	8	2/20/94	sidney	new files for specials
	2  	 4/ 3/96	Hernan  	Now loading the SK8Script Fred utilities. Also cleaned up a
						little.
	3  	 4/29/96	Hernan  	Loading the toolbox assistant instant lookup file.
	4  	 5/ 3/96	Hernan  	Adding the documentation utilities.
	5  	 2/27/97	sidney  	remove no longer used file. add lisp dev mode stuff to here so release version does not have MCL listener
	6  	 2/27/97	Hernan  	
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
