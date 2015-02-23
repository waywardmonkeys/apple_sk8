(in-package :cl-user)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(provide "QUICKTIME")

(require "DYNAMICRENDERER" "objects;Effects:DynamicRenderer")
(require "INDIRECTTRANSLATOR" "objects;Import/Export:IndirectTranslator")
;; also require IndirectCollection.

;;; Load the other files!

(defvar *quicktime-files*
  '("macf-trap-library;Files"
    "macf-trap-library;Movies Traps"
    "macf-trap-library;GestaltEqu"
    "macf-trap-library;Components"
    "macf-trap-library;Script"
    "objects;Movies:ObjectID"
    "objects;Movies:QuickTime"
    "objects;Movies:QT-Media"
    "objects;Movies:QT-Track"
    "objects;Movies:QT-TimeBase"
    "objects;Movies:QT-CallBack"
    "objects;Movies:QT-TrackCollection"
    "objects;Movies:QT-Movie"
    "objects;Movies:QT-PictRenderer"
    "objects;Movies:QT-Renderer"
    "objects;Movies:QT-Translators"
    "objects;Movies:MovieRectangle"))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *qt-debugging* nil)
  #+DYost (setf *qt-debugging* :best))

#+DYost (insert-features :debug-initialize)

(with-debugging-features (*qt-debugging*)
  (apply #'sk8-build-files *quicktime-files*))



#|
	Change History (most recent last):
	2	6/25/93	kleiman	ownedRegion -> ownsRegion
	3	6/26/93	kleiman	mf::ownsRegion! -> mf::ownedRegion!
	9	9/24/93	kleiman	quicktimemovie no longer inherits from mixin
	10	10/1/93	kleiman	obsolete struct
	11	12/1/93	dy	Add keep-movies-playing function called from window-event
				DY
	12	12/3/93	kleiman	mac-pathname -> file
	13	12/21/93	sidney	Changes so files can be compiled
	14	1/20/94	dy	Changed the code to load the other files so it
				loads them from the same folder rather than from
				a specified path
	15	2/12/94	kleiman	renaming
	16	2/18/94	dy	changed name of *movies-are-playing-p* to *active-movies-without-controllers* and keep-movies-playing to movies-task
	17	2/18/94	dy	tidy up movies-initialize package
	19	2/25/94	dy	change the list of files we load and ignore the rest of the file
	20	2/25/94	dy	fix build code
	21	2/26/94	kleiman	fix paths and change package to cl-user
	22	3/8/94	dy	add QT-SimplePlayer and delete QT-ViewerWithoutController
	23	3/11/94	dy	remove old junk
	24	5/6/94	dy	load movieRectangle.lisp
	25	6/30/94	dy	build the new files, too
	26	6/30/94	dy	sourceserver madness
	27	7/19/94	dy	QT Renderer changes
	28 	 3/ 2/95	dy      	compile with debugging level if DYost feature is active
	29 	 3/ 6/95	dy      	add QT-TimeBase and QT_CallBack to the build
	30 	 3/28/95	dy      	#+DYost (insert-features :debug-initialize)
	3  	 6/25/96	sidney  	accidentally checked in some debugging stuff, revert
	4  	 9/ 4/96	Hernan  	Moved the traps to the object system folder.
	5  	 9/23/96	Hernan  	Loading the objectID code.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
