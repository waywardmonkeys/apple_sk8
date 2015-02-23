(in-package :SK8Dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public
                  SK8::quickTimeVersion
                     )

(new Object :objectname "QuickTimeMedia"
     :undisposable t
     :project sk8)

(new Object :objectname "QuickTimeTrack"
     :undisposable t
     :project sk8)

#|
(defvar QTScratchGWorld nil)

(defun getQTScratchGWorld ()
  (or (and QTScratchGWorld (wptr QTScratchgWorld))
      (let ((theWindow (make-instance 'window :view-size (make-point 1 1) 
                                      :view-position (make-point -4000 -4000))))
        (setf QTScratchgWorld theWindow)
        (wptr theWindow))))
|#

(define-sk8-function quickTimeVersion nil ()
  (sk8-multival-bind (result error)
    (T_Gestalt #$gestaltQuicktime :OSErr t)
    (declare (ignore error))
    (when (or (not result)
              (< result #x1600000))
      (error "Please install QuickTime 1.6.2 or later and restart."))
    result))

;;; requireQuickTimeInitialized
;;;	Initializes QuickTime driver if QuickTime is available.
;;;	If not, it signals an error condition
;;;
(define-sk8-function requireQuickTimeInitialized nil ()
  ; (error "QuickTime is not working in this version of SK8.")
  (or gs:*movies-initialized*
      (progn
        (quickTimeVersion)
        (checking-Toolbox-Error (:OSErr) (#_EnterMovies))
        (initialize-movies-trap-null-ptr-values)
        (setq gs:*movies-initialized* t))))

;;; There used to be code here that arranged for #_ExitMovies to be called at quit time.
;;; Problem is, that all component connections that are related to the Movie Toolbox
;;; must be closed before ExitMovies is called.  This would require us to make sure that
;;; ExitMovies is called after the quit hook that terminates open component connections.
;;; It's simpler to rely on the fact (IM-QuickTime p. 2-83) that ExitMovies is called
;;; automatically when you quit, i.e. after all lisp code.

;;;? Move these somewhere earlier in the build

(new Media :objectname "QuickTimeMovie"
       :undisposable t
       :project SK8)


#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/26/94	kleiman	call initialize-movies-trap-nul-ptr-values from RequireMoviesInitialized
	3	3/2/94	dy	use T_Gestalt
	4	3/2/94	dy	require qt 1.6.2 or later
	5	3/3/94	sidney	SK8-Declare-syms in non-header file should not specify a category
	6	3/3/94	sidney	...but they should be in the correct format!
	7	3/11/94	dy	tweaking
	8	3/25/94	dy	don't do ExitMovies.  Leave that to the OS.
	9	3/29/94	dy	fix QT version number we require
	10	6/30/94	dy	move some object definitions here
	11	7/19/94	dy	QT Renderer changes
	12	8/11/94	dy	symbols with preserved case
	13 	 8/22/94	dy      	upper-lower case names
	14 	 8/22/94	dy      	move copyright back to top
	15 	 8/24/94	dy      	no changes?
	16 	 9/ 1/94	dy      	requireMoviesInitialized -> sk8 function
	17 	 9/ 6/94	dy      	attempt to get upper-lower-case symbols in the image
	18 	 9/15/94	dy      	#~movieNumber
	19 	 9/19/94	dy      	RequireMoviesInitialized -> requireQuickTimeInitialized
	20 	 9/27/94	dy      	NoValuexxx -> *undefined*
	21 	 9/28/94	chip    	AliasedCollection --> IndirectCollection
	22 	10/ 3/94	dy      	(error "QuickTime is not working in this version of SK8.")
	23 	10/ 5/94	dy      	re-enable QuickTime
	24 	10/ 6/94	dy      	move public symbols here
	25 	10/ 6/94	dy      	move symbols to preload
	26 	10/28/94	dy      	add some #~ symbols
	27 	11/21/94	dy      	use read-from-string to make the #~ stuff work
	28 	 1/31/95	dy      	add QTScratchGWorld and getQTScratchGWorld
	29 	 2/24/95	dy      	fix requireQuickTimeInitialized to print the right error if it's not installed at all
	30 	 3/ 8/95	dy      	remove my mixed-case symbol trick
	31 	 4/18/95	dy      	Added new quickTimeVersion function which is called by set resizingStyle of QuickTimeRenderer
							to determine if it should do the workaround for a QT 2.0 bug.
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
