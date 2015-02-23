(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public
                  SK8::QuickTimeMovieToResourceTranslator
                  SK8::QuickTimeMovieTomoovResourceTranslator

                  SK8::QuickTimeMovieToFileTranslator
                  SK8::QuickTimeMovieToMooVFileTranslator
                  SK8::QuickTimeMovieToAiffFileTranslator

                  SK8::QuickTimeRendererToResourceTranslator
                  SK8::QuickTimeRendererTomoovResourceTranslator

                  SK8::QuickTimeRendererToFileTranslator
                  SK8::QuickTimeRendererToAiffFileTranslator
                  SK8::QuickTimeRendererToMooVFileTranslator
                  )


;;;==================================================================

(new Translator
     :objectName "QuickTimeMovieToResourceTranslator"
     :project sk8
     :internalObject QuickTimeMovie
     :finalObject QuickTimeMovie)

(define-handler externalType (QuickTimeMovieToResourceTranslator)
  nil)

(define-handler canImport (QuickTimeMovieToResourceTranslator source destination)
  (declare (ignore source))
  (if (eq me QuickTimeMovieToResourceTranslator)
    nil
    (if (and destination
             (eq (slot-value destination 'file)
                 :project))
      nil ; Can't do that yet
      (call-next-method))))

(define-handler importFromFile (QuickTimeMovieToResourceTranslator source destination)
  (when (eq (slot-value destination 'file) :project)
    (sk8-error GeneralProgrammaticError '("Can't import to a project.")))
  (setf (resourceid destination) nil) ;? why do we need to do this?
  (importFromDataFork destination source)
  destination)

(define-handler importFromResource (QuickTimeMovieToResourceTranslator source destination)
  (call-next-method)
  (importFromResource destination source)
  destination)

(define-handler importFromClipboard (QuickTimeMovieToResourceTranslator source destination)
  (declare (ignore source)) ;?
  (call-next-method)
  (importFromResource destination destination) ;?
  destination)

;;;------------------------------------------------------------------

(new QuickTimeMovieToResourceTranslator
     :objectName "QuickTimeMovieTomoovResourceTranslator"
     :project sk8)

(define-handler externalType (QuickTimeMovieTomoovResourceTranslator)
  "moov")

;;;==================================================================

(new Translator
     :objectName "QuickTimeMovieToFileTranslator"
     :project sk8
     :internalObject QuickTimeMovie)

(define-handler externalType (QuickTimeMovieToFileTranslator)
  nil)

(define-handler canImport (QuickTimeMovieToFileTranslator source destination)
  (if (eq me QuickTimeMovieToFileTranslator)
    nil
    (if (and destination
             (eq (slot-value destination 'file)
                 :project))
      nil ; Can't do that yet
      (and (call-next-method)
           (ignore-errors
            (let ((fRefNum (T_OpenMovieFile (appleFileSpecGCHandle source))))
;(pr 1)
              (unwind-protect
                (multiple-value-bind_X
                  (movie resIDResult dataRefWasChanged couldNotResolveDataReference)
                  (T_NewMovieFromFileGC fRefNum :notGC t)
                  (declare (ignore dataRefWasChanged couldNotResolveDataReference))
;(pr 2)
                  (prog1
                    (if (= resIDResult TC_movieInDataForkResID)
                      t
                      nil)
                    (T_DisposeMovie movie)
;(pr 3)
                    ))
                (T_CloseMovieFile fRefNum))))))))

;;; Asumes the file exists and is of the right macfiletype. Gets the handle and writes it to the resource file
;;; of the project. (If you want to read it from the data fork all the time, should use the translator for
;;; PictFileMedia).

(define-handler importFromFile (QuickTimeMovieToFileTranslator source destination)
  (when (eq (slot-value destination 'file) :project)
    (sk8-error GeneralProgrammaticError '("Can't import to a project.")))
  (setf (resourceid destination) nil) ;? why do we need to do this?
  (importFromDataFork destination source)
  destination)

;;;------------------------------------------------------------------

(new QuickTimeMovieToFileTranslator
     :objectName "QuickTimeMovieToMooVFileTranslator"
     :project sk8)

(define-handler externalType (QuickTimeMovieToMooVFileTranslator)
  "MooV")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(new QuickTimeMovieToFileTranslator
     :objectName "QuickTimeMovieToAiffFileTranslator"
     :project sk8)

(define-handler externalType (QuickTimeMovieToAiffFileTranslator)
  "AIFF")


;;;==================================================================

(new IndirectTranslator
     :objectName "QuickTimeRendererToResourceTranslator"
     :project sk8

     :internalObject  QuickTimeMovie
     :finalObject     QuickTimeRenderer)

(define-handler externalType (QuickTimeRendererToResourceTranslator)
  nil)

(define-handler canImport (QuickTimeRendererToResourceTranslator source destination)
  (declare (ignore source destination))
  (if (eq me QuickTimeRendererToResourceTranslator)
    nil
    (call-next-method)))

(define-handler sk8::import (QuickTimeRendererToResourceTranslator &key source destination)
  (declare (ignore source destination))
  (let ((theMovie (call-next-method)))
   (new QuickTimeRenderer :media theMovie :project (project theMovie))))


;;;------------------------------------------------------------------

(new QuickTimeRendererToResourceTranslator
     :objectName "QuickTimeRendererTomoovResourceTranslator"
     :project sk8

     :mediaTranslator QuickTimeMovieTomoovResourceTranslator)

(define-handler externalType (QuickTimeRendererTomoovResourceTranslator)
  "moov")

;;;==================================================================

(new IndirectTranslator
     :objectName "QuickTimeRendererToFileTranslator"
     :project sk8

     :internalObject  QuickTimeMovie
     :finalObject     QuickTimeRenderer)

(define-handler externalType (QuickTimeRendererToFileTranslator)
  nil)

(define-handler canImport (QuickTimeRendererToFileTranslator source destination)
  (declare (ignore source destination))
  (if (eq me QuickTimeRendererToFileTranslator)
    nil
    (call-next-method)))


(define-handler sk8::import (QuickTimeRendererToFileTranslator &key source destination)
  (declare (ignore source destination))
  (let ((theMovie (call-next-method)))
    (new QuickTimeRenderer :media theMovie :project (project theMovie))))

;;;------------------------------------------------------------------

(new QuickTimeRendererToFileTranslator
     :objectName "QuickTimeRendererToAiffFileTranslator"
     :project sk8

     :mediaTranslator QuickTimeMovieToAiffFileTranslator)

(define-handler externalType (QuickTimeRendererToAiffFileTranslator)
  "AIFF")

;;;------------------------------------------------------------------

(new QuickTimeRendererToFileTranslator
     :objectName "QuickTimeRendererToMooVFileTranslator"
     :project sk8

     :mediaTranslator QuickTimeMovieToMooVFileTranslator)

(define-handler externalType (QuickTimeRendererToMooVFileTranslator)
  "MooV")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



#|
	Change History (most recent last):
	1	5/12/94	dy	
	2	7/19/94	dy	QT Renderer changes
	3	8/10/94	dy	symbols with preserved case
	4  	 9/ 2/94	dy      	Fix bug 1182759 (not finished yet, probably)
	5  	 9/ 2/94	dy      	More on bug 1182766
	6  	 9/ 7/94	dy      	more attempts to fix
	7  	 9/ 8/94	sidney  	fixed apparent typo
	8  	 9/ 9/94	dy      	add finalObject, fix canImport
	9  	 9/13/94	dy      	
	10 	 9/13/94	dy      	implement qtrenderer import handlers
	11 	 9/14/94	dy      	closer
	12 	 9/30/94	dy      	remove debug printouts
	13 	10/ 1/94	sidney  	remove typo in removal of debug printouts
	14 	 3/16/95	rod     	fileSpecGCHandle -> appleFileSpecGCHandle
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
