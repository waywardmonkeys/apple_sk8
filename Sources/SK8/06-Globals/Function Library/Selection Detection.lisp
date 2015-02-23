;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "SELECTIONDETECTION")

#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
01-22-93 ruben id -> objectName

|#


(defun pointsToRegion (thePointsList region)

  ;;;  When the user just clicks, we add a few points to make the region
  ;;;  big enough to form a region
  (when (<= (length thePointsList) 4)
    (setf thePointsList (append thePointsList (list      (car thePointsList)  (+ 2 (cadr thePointsList))
                                            (+ 2 (car thePointsList)) (+ 2 (cadr thePointsList))
                                            (+ 2 (car thePointsList))       (cadr thePointsList)))))
  (#_OpenRgn)
  (#_MoveTo (gs:f.round (car thePointsList)) (gs:f.round (cadr thePointsList)))
  (doTimes (i (1- (/ (length thePointsList) 2)))
    (#_LineTo (gs:f.round (nth (* 2 (1+ i)) thePointsList))
     (gs:f.round (nth (1+ (* 2 (1+ i))) thePointsList))))
  (#_LineTo (gs:f.round (car thePointsList)) 
   (gs:f.round (cadr thePointsList)))
  (#_CloseRgn region)
  region)

(defun RegionInRegion (region1 region2)
  (gs:let+ ((region3 (:region)))
    (#_SectRgn region1 region2 region3)
    (if (#_EmptyRgn region3)
      nil
      (if (#_EqualRgn region2 region3)
        :contains
        (if (#_EqualRgn region1 region3)
          :containedBy
          :partial)))))

(defun ActorsInRegion (region &key (avoiding nil) (container nil))
  (unless (listp avoiding) (setf avoiding (list avoiding)))
  (gs:let+ ((userRegion (:region))
             theoffset
             thestruct
             (actorsToCheck (list (or container stage)))
             (contentsToCheck nil)
             containedActors TouchedActors
             (remainingTouches nil)
             (LowestContainedByActor nil)
             (TouchedActorList nil)
             (ContainedActorList nil))
    (loop
      (setf remainingtouches nil)
      (setf actorsToCheck (remove-if #'(lambda (x) (memq x avoiding)) actorsToCheck))
      (if actorsToCheck
        (progn
          (dolist (i actorstocheck)
            (setf containedActors nil)
            (setf TouchedActors nil)
            (setf theoffset (if (eq i stage) 0 (view-position (gs:node-window i))))
            (#_CopyRgn region userRegion)
            (if (neq i stage)
              (#_OffsetRgn userRegion (- (point-h theOffset)) (- (point-v theOffset))))
            (setf contentsToCheck (remove-if #'(lambda (x) (or (not (visible x)) (memq x avoiding)))
                                             (contents i)))
            (doList (obj contentsToCheck)
              ;;; The stage is a weird special case where our offset changes
              ;;; from actor to actor because the MCL view system is window relative
              ;;; we therefore offset from the top left corner of the window
              (when (eq i stage)
                (#_CopyRgn region userRegion)
                (sk8-multival-bind (ll tt rr bb) (boundsrect obj)
                  (declare (ignore bb rr))
                  (#_OffsetRgn userRegion (- (truncate ll)) (- (truncate tt)))))
              (setq thestruct obj)
              (gs:recompute-bounds-region obj (gs:node-flags thestruct))
              (case (regionInRegion userRegion (gs:node-boundsRegion thestruct))
                (:contains (progn
                             (push obj containedActors)
                             (push obj TouchedActors)))
                (:containedby (when (or (null LowestContainedByActor)
                                        (containedby obj LowestContainedByActor))
                                (setf LowestContainedByActor obj)
                                (push obj TouchedActors)))
                (:partial (push obj TouchedActors))
                (otherwise nil)))
            (when touchedactors (push touchedActors TouchedActorList))
            (setf remainingtouches (append touchedActors remainingtouches))
            (when containedactors (push containedactors ContainedActorList)))
          (setf actorstocheck remainingtouches))
        (return)))
    (sk8-multivals (reverse ContainedActorList) (reverse TouchedActorList) LowestContainedByActor)))
            

;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;
;  This function take a point list and a boundsrect (respectively) and go through all the actors on the stage to check for selection.
;  They return a set of values which can be used to generate a list of actors to be selected based on the style the user wishes to program,
;  therefore they return four special purpose values:
;   (1) clicked actor:  if the result was a click and not a box/lasso, this is the object that was clicked on.  
;   (2) Contained Actors:  This is an ordered list of lists.  Each sublist is a list of objects fully contained by the region
;                     Each list represents a level of containment in one actor.  Thus each list is a list of all the
;                     actors fully surrounded at a specific level in a specific actor.
;                     Example: The first list is the list of actors which are fully surrounded at the highest level.
;                             For every item in this first list there will be another list of all the items inside because
;                             all of these items will be surrounded.  There will also be lists of objects which are contained
;                             by the halo but are not as high up in the containment hierarchy as those in the first list, etc.
;   (3) TouchedActors:  This is an ordered list of lists in the same form as (2) except the objects in the lists are ones
;                   that are touched in any way (partially or fully) by the region defined.
;   (4) Lowest Fully Contained By Actor:  This is the deepest actor which fully contains the region.  (There can be at most one of these)
;
;   Note that all of these are pruned so as not return objects from sk8 or the ui, thus even though the stage theoretically contains every
;   region, it will not be returned by (4) because it is an object in sk8.
;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(define-sk8-function AnalyzeActorsForSelection nil (thePointsList &key (avoiding nil) 
                                                                       (container nil))
  (gs:let+ ((theRegion (:region))
            (clicked nil))
    (if (= (length thePointsList) 4)   ;;; Assume we are doing a rectangle...
      (let  ((ll (first thePointsList))
             (tt (second thePointsList))
             (rr (third thePointsList))
             (bb (fourth thePointsList)))
        (when (eql ll rr)
          (setf rr (+ 1 ll))
          (setf clicked t))
        (when (eql tt bb)
          (setf bb (+ 1 tt))
          (setf clicked t))
        (pointsToRegion (list ll tt rr tt rr bb ll bb) theRegion))
      (progn                       ;;; otherwise we are doing a polygon
        (if (= (length thePointsList) 2)
          (setf clicked t))
        (pointsToRegion thePointsList theRegion)))
    (sk8-multival-bind (ContainedActorList TouchedActorList LowestContainedByActor) (ActorsInRegion theRegion :avoiding avoiding :container container)
      (sk8-multivals (if clicked LowestContainedByActor nil) ContainedActorList TouchedActorList LowestContainedByActor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  These are sample functions that uses this analyzing function....
;;;;
;;;;  *******************A whole host of these things should be written in sk8script for the users perusal **************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-sk8-function FindSelectedActors nil (thePointsList &key (avoiding nil) (container nil))
  (sk8-multival-bind (ClickedActor ContainedActorList TouchedActorList LowestContainedByActor)
                     (AnalyzeActorsForSelection thePointsList :avoiding avoiding :container container)
    (declare (ignore TouchedActorList))
    (cond
     (clickedActor (list ClickedActor))
     (ContainedActorList (car ContainedActorList))
     (LowestContainedByActor (list LowestContainedByActor))
     (t nil))))

(define-sk8-function FindSelectedActorsGrow nil (thePointsList &key (avoiding nil) (container nil))
  (sk8-multival-bind (ClickedActor ContainedActorList TouchedActorList LowestContainedByActor)
                     (AnalyzeActorsForSelection thePointsList :avoiding avoiding :container container)
    (cond
     (clickedActor (list ClickedActor))
     (ContainedActorList (car ContainedActorList))
     (LowestContainedByActor (list LowestContainedByActor))
     (t (car touchedactorlist)))))

#|

;; (setCurrentProject foo)
(define-handler mousedown (bingham) 
  (ui::select (FindSelectedActors (getshapefromuser rectangle eventh eventv )))
  )
(define-handler mousedown (bingham) 
  (ui::select (Experimental-FindSelectedActors (getshapefromuser polygon eventh eventv :style 'freehand)))
  )
(define-handler mousedown (bingham) 
  (sk8-multival-bind (a b c d) (AnalyzeActorsInBox (getshapefromuser rectangle eventh eventv ))
    (format t "~%~a  ~%~a ~%~a ~%~a ~%" a b c d))
  )
(define-handler mousedown (bingham) 
  (sk8-multival-bind (a b c d) (Experimental-LassoedActors (getshapefromuser polygon eventh eventv :style 'freehand))
    (format t "~%~a  ~%~a ~%~a ~%~a ~%" a b c d))
  )
|#

#|
	Change History (most recent last):
	1	6/1/93	Brian Roddy	Used to be in utilities, now is for selection detection.  Not redesigned yet.
	2	6/2/93	Brian Roddy	Redesigned and added the two user visible
				functions: LassoedActors and BoxedActors.
				Users can use this as part of their selection
				tools.  Still could use some user testing...
	3	6/3/93	Brian Roddy	Totally redid this puppy again to have the two tiered process of first analyzing and then doing the style of lasso/boxing the user wants
	6	6/3/93	Brian Roddy	Comments and consolidated to just one function for both rectangles and pointlists...
	7	6/3/93	Brian Roddy	
	8	6/10/93	Brian Roddy	
	14	9/20/93	rod	Fixed selection bug by making us traverse only
				down visible (frontmost) actors for lowest
				contained by actor.
	15	10/1/93	rod	Removed Node References and Added With-fast-slots.
	16	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	17	10/13/93	rod	
	18	10/15/93	rod	
	19	11/5/93	rod	
	20	11/19/93	rod	
	21	1/31/94	rod	Adding the avoiding option...
	22	1/31/94	rod	removed a print
	23	3/21/94	rod	Making it so the default size of a region when 
				you click is 1, 1 rather than 2, 2.  I don't know why
				it used to be 2, 2 but now you can select line
				segments.
	24	6/27/94	Hernan	1170049: style options become symbols.
	25 	11/21/94	rod     	Making the container stuff work.
	26 	12/ 7/94	Hernan  	pointList -> thePointsList
	5  	 2/27/97	Hernan  	define-sk8-function->defun.
						4  11/25/96Hernan  Truncate floats before passing them to a trap expecting ints.
|# ;(do not edit past this line!!)
