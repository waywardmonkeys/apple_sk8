;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(new imageRenderer :project ui :objectname "simpledocumentcolor")
(setf (media simpledocumentcolor) sk8:DocumentRSRC)

(new imageRenderer :project ui :objectname "TemporarySwatch")
(setf (media TemporarySwatch) sk8:graypatternrsrc)

;;;;;;;;;;;;;;;--------------------------------------------------------------------
;;;;;;;;;;;;;;;--------------------------------------------------------------------

(defparameter SwatchHashTable (make-hash-table :test #'eq))
(defparameter SwatchToDo nil)

(defun RemoveSwatchFromTable (self)
  (if (inheritsfrom self handler) (setf self (object self)))
  (let ((entry (gethash self SwatchHashTable)))
    (when (and self entry)
      (dolist (i (cdr entry)) 
        (when (eq (fillcolor i) (car entry))
          (setf (fillcolor i) white)))
      (when (inheritsfrom (car entry) swatchrenderer)
        (setf (actor (car entry)) nil)
        (when (and (media (car entry)) (is-a (media (car entry)) pixelmap) (mediaData (media (car entry))))
          (t_deactivategcosptr (mediaData (media (car entry))) :disposeNow t))
        (setf (media (car entry)) nil)
        (discard (car entry)))
      (remhash self SwatchHashTable)
      )))

(defun integrityCheckOnSwatchTable (self)
  (when self
    (if (inheritsfrom self handler) (setf self (object self)))
    (let* ((entry (gethash self SwatchHashTable))
           (swatch (car entry))
           (acts (cdr entry)))
      (if (not entry)
        (print (concatenate 'string "Integrity Check on Swatches Failed on: "
                            (objectstring self)))
        (if nil ;;;(or (disposed self) (disposed swatch))   IF WE ADD A DISPOSE WATCHER, THIS SHOULD CHECK THERE!!!!
          (RemoveSwatchFromTable self)
          (progn
            (setf acts (delete-duplicates acts :test #'eq))
            ;;;(setf acts (delete-if #'disposed acts))
            (if acts
              (setf (gethash self SwatchHashTable) (cons swatch acts))
              (RemoveSwatchFromTable self)
              )))))))

(defun LogOutSwatches (theactor swatches)
  (tickeventclock)
  (let (act entry)
    (unless (listp swatches) (setf swatches (list swatches)))
    (dolist (i swatches)
      (when (and (eq (baseparent i) swatchrenderer) (not (objectname i)))
        (setf act (actor i))
        (if (memq act swatchToDo) (setf swatchToDo (delete act swatchToDo)))
        (setf entry (gethash act SwatchHashTable))
        (when entry
          (setf (gethash act SwatchHashTable) (delete theactor entry)))
        (integrityCheckOnSwatchTable act))))
  )

(defun UpdateSwatchofActor (self)
  (tickeventclock)
  (if (inheritsfrom self handler) (setf self (object self)))
  (let ((entry (gethash self SwatchHashTable)))
    (when (and self entry (integrityCheckOnSwatchTable self))
      (MakeTheSwatch (car entry) self)
      (dolist (i (cdr entry))
        (lightforceredraw i)))
    (car entry)))

(new swatchRenderer :objectname "EmptyColor" :project ui)
(setf (backgroundRenderer EmptyColor) uidarkcolor)
(setf (text EmptyColor) "Empty")

(defun MakeTheSwatch (thecolor theobject)
  (if (inheritsfrom theobject handler) (setf theobject (object theobject)))
  (setf (backgroundRenderer thecolor) uidarkcolor)
  (setf (actor thecolor) theobject)
  (setf (text thecolor) (objectstring theobject :project (project theobject)))
  (cond
   ((and (inheritsfrom theobject actor)
         (neq theobject actor)
         )
    (multiple-value-bind (val1 err?) 
                         (ignore-errors (capturePicture thecolor theobject :size 50))
      (declare (ignore val1))
      (if err? (setf (media thecolor) simpledocumentcolor))
      )
    )
   ((inheritsfrom theobject media)
    (setf (media thecolor) simpledocumentcolor))
   ((and (inheritsfrom theobject renderer)
         (not (inheritsfrom theobject swatchrenderer)))
    (setf (media thecolor) theobject))
   (t
    (setf (media thecolor) simpledocumentcolor)))
  )

(defun GenerateSwatch (TheActor TheDestination)
  (tickeventclock)
  (let (newguy)
    (if theActor
      (progn
        (if (inheritsfrom theActor handler) (setf theActor (object theActor)))
        (let ((entry (gethash TheActor SwatchHashTable))
              newentry)
          (if entry
            (setf newentry (append entry (list theDestination))
                  newguy (updateSwatchOfActor theActor))
            (progn
              (setf newguy (recycled swatchRenderer :in ui))
              (setf (media newguy) TemporarySwatch)
              (setf (backgroundRenderer newguy) uidarkcolor)
              (setf (actor newguy) theactor)
              (Push theactor SwatchToDo)
              ;(MakeTheSwatch newguy theactor)
              (setf newEntry (list newguy theDestination))))
          (setf (gethash TheActor SwatchHashTable) newEntry)
          (integrityCheckOnSwatchTable theActor)
          ))
      (setf newguy EmptyColor))
    newGuy))

(defun DoASwatch ()
  (when SwatchToDo
    (UpdateSwatchofActor (pop SwatchToDo))))

;;;;;;;;;;;;;;;--------------------------------------------------------------------
;;;;;;;;;;;;;;;--------------------------------------------------------------------


;;MCL3.0
(define-handler setInform (ui objs propertyname)
  (updateEditors objs :propertyname propertyname))

;;MCL3.0
(defun updateEditors (items &key (propertyname nil))
  (process-run-function
   '(:name "Updating Editors" :background-p t)
   #'(lambda (items propertyname)
       (do-updateEditors items propertyname))
   items propertyname))

(defun do-updateEditors (items propertyname) 
  (sk8dev::do-sk8-window-actors (i)
    (when (and (eq (project i) ui)
               (or (eq i stageoverviewer)
                   (eq i systembrowser)
                   (is-a i objecteditor)
                   (is-a i renderereditorwindow)
                   ))
      (updateEditor i :propertyname propertyname :items items)))
  (let (swatchesToUpdate)
    (when (eq propertyName 'container)
      (let ((propname (propertyname undoablesetlog))
            (vallist (valuelist undoablesetlog)))
        (when (and propname vallist)
          (if (listp propname)
            (let ((n (position 'container propname)))
              (when n (setf items (append (nth n vallist) items))))
            (setf items (append vallist items))))))
    (when (eq propertyName 'objectname)
      (sk8dev::do-sk8-window-actors (i)
        (when (and (eq (project i) ui)
                   (is-a i uiscripteditor))
          (updateEditor i :propertyname propertyname :items items)))
      
      (when (container drawpalette)
        (let ((xx (inputobjects userpalette)))
          (if (intersection xx items :test #'eq)
            (setf (inputobjects userpalette) xx)))
        )
      (when (and (= (length (selecteditems selectionhalo)) 1)
                 (memq (car (selecteditems selectionhalo)) items))
        (let ((xx (selecteditems selectionhalo)))
          (mayberemove selectionhalo)
          (select xx)))
      )
    (when (or (eq propertyName 'boundsrect)
              (eq propertyName 'fillcolor)
              (eq propertyName 'framecolor)
              (eq propertyName 'framesize)
              (eq propertyName 'textcolor)
              (eq propertyName 'text)
              (eq propertyName 'size)
              (eq propertyName 'container)
              (eq propertyName 'location)
              (eq propertyName 'objectname)
              (eq propertyName 'items))
      (dolist (i items)
        (when (and (inheritsfrom i actor) (neq i actor))
          (setf swatchesToUpdate (nconc (cons i (sk8::containers i)) swatchesToUpdate)))
        )
      (dolist (j (delete-duplicates swatchesToUpdate))
        (updateSwatchofActor j)))))

#|
	Change History (most recent last):
	1	1/31/94	rod	
	2	1/31/94	rod	
	3	1/31/94	rod	
	4	1/31/94	rod	Added dispose updates and container updates
	5	2/3/94	rod	
	6	2/4/94	rod	
	7	2/14/94	rod	
	8	2/15/94	rod	
	9	2/15/94	rod	Set properties of non actor swatches.
	10	2/16/94	rod	Now do remhash to make sure references are
				gone.
	11	2/18/94	rod	
	12	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	13	2/22/94	rod	Handlers show the swatch of their object...
	14	2/22/94	rod	Uh now it should work with handlers.
	15	2/23/94	rod	
	16	2/28/94	rod	
	17	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	18	3/5/94	rod	
	19	3/7/94	rod	
	20	3/8/94	rod	
	21	3/8/94	rod	
	22	3/8/94	rod	
	23	3/9/94	rod	Doing Project Switching and Reference Clearing.
	24	3/26/94	rod	
	25	3/26/94	rod	Fixed multiple update problem.
	26	3/27/94	Brian	Mucking with default swatch size.
	27	3/29/94	rod	
	28	3/30/94	rod	WatchCursor stuff.
	29	4/1/94	rod	Forcing disposePtr when it is possible.
	30	4/1/94	rod	Making switching containers update the old
				container.
	31	4/1/94	rod	
	32	4/1/94	rod	Fixed bugs with updating on set container.
	33	4/3/94	sidney	Fixed a bug with the updating on set container stuff
	34	4/13/94	Hernan	Avoiding use of contents when possible.
	35	5/6/94	rod	
	36	8/1/94	rod	Fixing the update of the objectname of singly 
				selected item in the selection halo.
	37 	 9/21/94	rod     	
	38 	10/ 4/94	rod     	Making all of this lazy.
	39 	12/ 3/94	rod     	updates on set objectname.
	40 	12/ 5/94	rod     	
	41 	 2/ 1/95	rod     	Making sure swatch background color gets set.
	42 	 2/16/95	brian   	
	43 	 2/20/95	rod     	Changing getHandlerInfo to ensure objectname
							is correct.
	44 	 3/20/95	Hernan  	Making pixelMap behave like a real child of Media.
	45 	 4/11/95	rod     	Dealing with failed snapshots.
	3  	12/ 8/95	Brian   	
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 9/30/96	sidney  	changes to put sk8 windows in separate threads
	4  	10/11/96	Brian   	removing crossauthorable.
	5  	11/12/96	sidney  	name some anonymous threads
	6  	12/12/96	Hernan  	Need to recompile do-sk8-window-actors.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
