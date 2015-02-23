;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  3-24-94   1:50 pm
                  SK8::HORIZONTALCENTER SK8::VERTICALCENTER)




(defun CalcLayoutParameters (actorList BoundsRect relativeActor physical)
  (let (MaxSize ll tt rr bb cont)
    (cond
     (BoundsRect
      (sk8-multival-setf (ll tt rr bb) BoundsRect)
      (unless physical
        (setf cont (container (car actorlist)))
        (sk8-multival-bind (l2 t2 r2 b2) (boundsrect cont :physical t)
          (incf ll l2)
          (incf tt t2)
          (incf rr l2)
          (incf bb t2)))
      )
     (relativeActor
      (sk8-multival-setf (ll tt rr bb) (boundsRect relativeActor :physical t)))
     (t
      (sk8-multival-setf (ll tt rr bb) (ui::actorsBounds actorList :physical t))))
    (let ((hh -1) (vv -1))
      (dolist (i actorlist)
        (sk8-multival-bind (hact vact) (size i)
          (if (> hact hh) (setf hh hact))
          (if (> vact vv) (setf vv vact))))
      (setf MaxSize (list HH VV)))
    (sk8-multivals MaxSize ll tt rr bb)))


(define-Sk8-Function tile nil
  (actorList &key rows columns (hSpacing 4) (vSpacing 4) relativeActor BoundsRect resizing physical)
  (unless hspacing (setf hspacing 4))
  (unless vSpacing (setf vSpacing 4))
  (when actorList
    (if resizing 
      (FitToBounds actorList :rows rows :columns columns :hspacing hspacing :vspacing vspacing :relativeActor relativeActor :boundsRect boundsRect)
      (sk8-multival-bind (MaxSize left top rr bb) (CalcLayoutParameters actorList BoundsRect relativeActor physical)
        (sk8-multival-bind (hh vv) MaxSize
          ;; compute the defaults
          (incf left (/ hh 2))
          (incf top (/ vv 2))
          (when relativeactor
            (incf left hSpacing)
            (incf top vSpacing))
          (incf hSpacing hh)
          (incf vSpacing vv)
          ;; compute the sizes!
          (let* ((x left)
                 (y top)
                 (count (length actorList)))
            (if (and rows columns (not (= (* rows columns) count)))
              (setf rows nil))
            (if (and rows
                     (< count rows))
              (setf rows count))
            (if (and columns
                     (< count columns))
              (setf columns count))
            (let*  ((xSize (if rows 
                             (ceiling count rows)
                             (if columns 
                               columns
                               (round (sqrt count)))))
                    obj)
              (dotimes (i count)
                (setf obj (nth i actorList))
                (setLocation obj x y :physical t)
                (setq x (+ x hSpacing))
                (if (= (mod (1+ i) xSize) 0)
                  (setq x left
                        y (+ y vSpacing))))
              (sk8-multivals (- left hspacing) (- top vspacing) (* hspacing (1+ xsize)) (+ y vspacing))
              )))))))

(define-Sk8-Function FitToBounds nil
  (actorList &key BoundsRect relativeActor rows columns 
             (hSpacing 4) (vSpacing 4) (setRowAndCol nil)
             physical)
  (unless hspacing (setf hspacing 4))
  (unless vSpacing (setf vSpacing 4))
  (let* ((numActors (length actorlist))
         hh vv
         left x y obj
         HRoom VRoom
         (currow 1) (curcol 1)
         hSize VSize)
    (sk8-multival-bind (MaxSize ll tt rr bb) (CalcLayoutParameters actorList BoundsRect relativeActor physical)
      ;; compute the defaults
      (setf hh (- rr ll)
            vv (- bb tt))
      (if (and (not rows) (not columns))
        (setf columns (ceiling (sqrt numActors))))
      (if (not columns)
        (setf columns (ceiling (/ numActors rows))))
      (if (or (not rows)
              (> numactors (* rows columns)))
        (setf rows (ceiling (/ numActors columns))))
      (setf HRoom (- hh (* hspacing (+ 1 columns))))
      (setf VRoom (- vv (* Vspacing (+ 1 rows))))
      (when (< HRoom columns)
        (setf hspacing 0)
        (setf HRoom hh))
      (when (< VRoom rows)
        (setf vspacing 0)
        (setf vroom vv))    
      (setf hsize (round HRoom columns))
      (setf Vsize (round VRoom rows))
      (setf left (+ ll hspacing (round hsize 2)))
      (setf x left)
      (setf y (+ tt vspacing (round vsize 2)))
      (dotimes (i numActors)
        (setf obj (nth i actorList))
        (setSize obj hsize vsize)
        (setLocation obj x y :physical t)
        (when setRowAndCol
          (setf (row obj) currow)
          (setf (col obj) curcol))
        (incf curcol)
        (setq x (+ x hSpacing hsize))
        (when (= (mod (1+ i) columns) 0)
          (setf curcol 1)
          (incf currow)
          (setq x left
                y (+ y vSpacing vsize))))
      )))


;;__________________________________________________________________
;; ALIGN
;; Aligns actors.

(define-Sk8-Function align nil (actorList &key (alignLocation nil) (resizing nil) physical
                                           relativeActor BoundsRect)
  (let (lll ttt rrr bbb temp1 temp2 )
    (when alignLocation
      (sk8-multival-bind (MaxSize ll tt rr bb) (CalcLayoutParameters actorList BoundsRect relativeActor physical)
        (doList (theObj actorList)
          ;; set the rect data
          (sk8-multival-setf (lll ttt rrr bbb) (boundsRect theObj :physical t))
          ;; hAlign
          (case alignLocation
            ((topleft centerleft bottomleft left) ;; HEW
             (progn
               (unless Resizing
                 (setq temp1 (- rrr lll) ;; the size of me
                       lll ll
                       rrr (+ ll temp1)))
               (setq lll ll)))
            ((center topCenter bottomCenter verticalCenter)
             (setq temp1 (/ (- rrr lll) 2) ;; 1/2 the size of me
                   temp2 (/ (+ ll rr) 2)   ;; the loc of the alignActor
                   lll (round (- temp2 temp1))
                   rrr (round (+ temp2 temp1))))
            ((topright centerright bottomRight right)
             (progn
               (unless Resizing
                 (setq temp1 (- rrr lll) ;; the size of me
                       rrr rr
                       lll (- rr temp1)))
               (setq rrr rr))))
          ;; vAlign
          (case alignLocation
            ((topleft topCenter topRight top)
             (progn
               (unless Resizing
                 (setq temp1 (- bbb ttt) ;; the size of me
                       ttt tt
                       bbb (+ tt temp1)))
               (setq ttt tt)))
            ((center CenterRight centerLeft horizontalCenter)
             (setq temp1 (/ (- bbb ttt) 2) ;; 1/2 the size of me
                   temp2 (/ (+ tt bb) 2)   ;; the loc of the alignActor
                   ttt (round (- temp2 temp1))
                   bbb (round (+ temp2 temp1))))
            ((bottomLeft bottomRight bottomCenter bottom)
             (progn
               (unless Resizing
                 (setq temp1 (- bbb ttt) ;; the size of me
                       bbb bb
                       ttt (- bb temp1)))
               (setq bbb bb))))
          ;; set it back
          (setBoundsRect theobj lll ttt rrr bbb :physical t)
          )))))

;;__________________________________________________________________
;; DISTRIBUTE
;; Distributes actors evenly. 

(define-Sk8-Function distribute nil (actorList &key
                                                 (how nil)
                                                 (sizeRelative nil)
                                                 relativeActor boundsRect physical
                                                 )
  (sk8-multival-bind (MaxSize ll tt rr bb) (CalcLayoutParameters actorList BoundsRect relativeActor physical)
    (cond 
     ;; linear - simple for now
     ((not sizeRelative)
      (incf ll (/ (car maxsize) 2))
      (incf tt (/ (cadr maxsize) 2))
      (decf rr (/ (car maxsize) 2))
      (decf bb (/ (cadr maxsize) 2))
      (let* ((spaces (1- (length actorList)))
             (dh (truncate (f- rr ll) spaces))
             (dv (truncate (f- bb tt) spaces))
             hLoc vLoc)
        (dolist (obj actorList)
          (sk8-multival-setf (hLoc vLoc) (location obj :physical t))
          (cond
           ((not how)
            (setq hLoc ll
                  vLoc tt))
           ((eq how 'h)
            (setq hLoc ll))
           ((eq how 'v)
            (setq vLoc tt)))
          (setLocation obj hLoc vLoc :physical t)
          (incf ll dh) (incf tt dv))))
     (t
      (let* ((spaces (max (1- (length actorList)) 1))
             (tHSize 0)
             (tVSize 0)
             left1 top1 right1 bottom1
             (hRoom (- rr ll))
             (vRoom (- bb tt))
             hSpace vSpace)
        ;; compute sizes
        (doList (obj actorList)
          (sk8-multival-setf (hSpace vSpace) (size obj :physical t))
          (setq tHsize (+ tHsize hSpace)
                tvSize (+ tvsize vSpace)))
        ;; compute the vars
        (setq hSpace (/ (- hRoom tHsize) spaces)
              vSpace (/ (- vRoom tVSize) spaces))
        ;; actually do the move
        (dolist (obj actorList)
          (sk8-multival-setf (left1 top1 right1 bottom1) (boundsRect obj :physical t))
          (cond
           ((not how)
            (setf (left obj :resizing nil :physical t) ll)
            (setf (top obj :resizing nil :physical t) tt))
           ((eq how 'h)
            (setf (left obj :resizing nil :physical t) ll))
           ((eq how 'v)
            (setf (top obj :resizing nil :physical t) tt)))
          (incf ll (+ (- right1 left1) hSpace))
          (incf tt (+ (- bottom1 top1) vSpace))
          ))))
    ))



#|
	Change History (most recent last):
	1	6/22/93	Brian Roddy	
	2	6/23/93	Brian Roddy	
	3	6/25/93	Brian Roddy	
	9	9/20/93	rod	Fixed zero case bug in distribute
	10	11/29/93	chip	Changes for more restrictive SK8-multival-setf/bind
	12	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	13	2/21/94	hernan	window -> sk8::window.
	14	2/25/94	hernan	Using symbols instead of keywords for options!!!
	15	3/5/94	rod	
	16	3/5/94	rod	
	17	3/5/94	rod	
	18	3/24/94	rod	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
