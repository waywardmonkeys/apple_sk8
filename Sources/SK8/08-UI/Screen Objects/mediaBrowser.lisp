;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :uidev)

#|

TO DO: 

1. Allow import without moving resources.
2. Fix the pickers.
3. Fix the labels.
4. Add progress bars.
5. Add translators.

|#

;;; ___________
;;; The Name Dialog for the creation of objects.
;;; ___________

(new dialogBox :objectname "MaybeNameDialog" :project ui
     :properties '((numberToCreate :value nil)))

(setf (fillcolor maybenamedialog) (fillcolor dialogbox))

(setf (size maybenameDialog) '(330 150))

(new dialogBoxLabel :objectname "NumObjsLabel" :project ui)
(setf (container numObjsLabel) maybenamedialog)
(setf (text numObjsLabel) "About to Make n Objects")
(setLocation numObjsLabel (/ (width maybenameDialog) 2) 12)
(setf (textcolor numObjsLabel) black)

(define-handler restore (numObjsLabel) 
  (call-next-method)
  ;; This is done to combat the workings of uicolorize.
  (setf (textColor me) black))

(new dialogBoxLabel :objectname "CreationProjLabel" :project ui)
(setf (container creationProjLabel) maybenamedialog)
(setf (text creationProjLabel) "in Project SK8")
(setLocation creationProjLabel (/ (width maybenameDialog) 2) 28)
(setf (textcolor creationProjLabel) black)

(define-handler restore (creationProjLabel) 
  (call-next-method)
  ;; This is done to combat the workings of uicolorize.
  (setf (textColor me) black))

(new rectangle :objectname "DialogDivider" :project ui)
(setf (container dialogDivider) maybenameDialog)
(setBoundsRect dialogDivider 10 40 (- (width maybenameDialog) 10) 42)

(new uiradioButton :objectname "MaybeName" :project ui)
(setf (checked MaybeName) t)
(setf (container maybename) maybenamedialog)
(setf (text maybename) "Provide Name for New Sk8 Object:")
(setf (textFont MaybeName) "Geneva")
(setf (textStyle MaybeName) '(bold)) ;; HEW
(setf (boundsRect maybename) '(10 46 262 62))
(setf (fillcolor maybename) (fillcolor dialogbox))

(new editText :project ui :textsize 12 :container maybenameDialog  :textstyle '(bold)
     :objectName "GetMediaName")
(setboundsrect getMediaName 50 64 300 82)

(define-handler (setf fillcolor) (theval getMediaName)
  (call-next-method white me))

(define-handler keydown (getMediaName thechar)
  (unless (checked MaybeName)
    (check MaybeName))
  (if (char= thechar #\space)
    (ed-beep)
    (call-next-method)))

(define-handler returnInField (getMediaName)
  nil)

(define-handler tabInField (getMediaName)
  nil)

;;; If the user starts typing we check the MaybeName radio button.

(define-handler keyDown (getMediaName theKey)
  (unless (checked MaybeName) 
    (check maybeName))
  (call-next-method))

(new uiradioButton :objectname "MaybeUseResNames" :project ui)
(setf (container MaybeUseResNames) maybenamedialog)
(setf (text MaybeUseResNames) "Use Resource Names")
(setf (textFont maybeuseresnames) "Geneva")
(setf (textStyle maybeuseResnames) '(bold))
(setboundsRect maybeuseresnames 10 86 190 102)
(setf (fillcolor maybeuseresnames) (fillcolor dialogbox))

(new uiradioButton :objectname "MaybeNoName" :project ui)
(setf (container MaybeNoName) maybenamedialog)
(setf (text MaybeNoName) "Do not Name")
(setf (textFont MaybeNoName) "Geneva")
(setf (textStyle MaybeNoName) '(bold))
(setboundsRect maybenoname 10 106 119 122)
(setf (checked maybenoname) t)
(setf (fillcolor maybenoname) (fillcolor dialogbox))

(new DialogBoxHighlightedButton :objectname "ImportMedia" :project ui)
(setf (text ImportMedia) "Create")
(setf (container importMedia) maybenamedialog)
(setboundsRect ImportMedia 249 121 319 145)

;;; Returns: name?, name, method

(define-handler mouseup (importMedia)
  (cond ((checked MaybeNoName) (exitModalState nil))
        ((checked MaybeUseResNames) (exitmodalState (list :resnames nil nil)))
        (t (let ((thename (text getMediaName)))
             (if theName
               (exitModalState (list :name theName (> (numbertoCreate maybenamedialog) 1)))
               (messageToUser "Please Enter the Name To Use."))))))

(define-handler enterInField (getMediaName)
  (mouseUp importMedia))

(define-handler returnInField (getMediaName)
  (mouseUp importMedia))

(new DialogBoxCancelButton :objectname "CancelMediaImport" :project ui)
(setf (container CancelMediaImport) maybenamedialog)
(setboundsRect cancelmediaimport 189 124 245 141)

(define-handler activate (maybeNameDialog)
  (call-next-method)
  (setf (keyTarget me) getMediaName)
  (selectAll getMediaName))

(defun maybenamedialog (numberToCreate typeToCreate creationProject)
  (setf (numberToCreate maybenameDialog) numberToCreate)
  (if (= numberToCreate 1)
    (setf (text maybename) "Provide Name for New Sk8 Object:")
    (setf (text maybeName) "Name Sequentially Starting With:"))
  (setf (text numObjsLabel) 
        (format nil "About to Make ~a ~a" numberToCreate (objectString typeToCreate)))
  (setf (text creationProjLabel) 
        (format nil "in Project ~a" (objectString creationProject)))
  ;; The real thing.
  (setf (location maybenamedialog) (mainmonitorcenter))
  (modalDialog maybenamedialog))

;;; _______________
;;; The MediaBrowser.
;;; _______________

(new UIsimpleWindow :objectname "MediaBrowser" :project ui)

(setf (width mediaBrowser) 500)
(setf (height mediaBrowser) 375)
(setf (left mediaBrowser :resizing nil) 6)
(setf (top mediaBrowser :resizing nil) 26)
(setf (sk8::menubar mediaBrowser) nil)
(setf (resizer mediaBrowser) nil)
(setf (zoombox mediaBrowser) nil)

(setf (text MediaBrowser) "Media Browser")

;;; (1) The files panel!

(new resourceChooser :objectName "MBResourceChooser" :project ui)
(setf (container mbResourcechooser) mediaBrowser)
(setBoundsRect mbResourcechooser 165 25 487 255)
(setf (fillcolor mbResourcechooser) UIMiddle)

(define-handler (setf displayType) (newType mbResourceChooser)
  (unless (eq newType 'resources)
    (setf (fillcolor mbResourceSampler) white))
  (call-next-method)
  ;; Fixing the makeMenu...
  (case newType
    (resources
     (setf (enabled mbMakePopUp) t)
     (setf (enabled mbImportAll) t)
     (setf (enabled mbImportButton) t))
    (restype
     (setf (enabled mbMakePopUp) t)
     (setf (enabled mbImportAll) t)
     (setf (enabled mbImportButton) nil))
    (otherwise
     (setf (enabled mbMakePopUp) nil)
     (setf (enabled mbImportAll) nil)
     (setf (enabled mbImportButton) nil))))
     
;;; More buttons and a rectangle separator...

(new rectangle :objectname "MBButtonSep" :project ui)
(setf (container mbButtonSep) mbResourceChooser)
(setBoundsRect mbButtonSep 227 128 318 130)

(new DialogBoxButton :objectName "MBImportButton" :project ui)
(setf (container mbImportButton) mbResourceChooser)
(setf (text mbImportButton) "Import")
(setBoundsRect mbImportButton 230 177 312 195)
(setf (enabled mbImportButton) nil)

(define-handler mouseup (mbImportButton)
  (when (eq (textColor me) black)
    ;; We are enabled. Let the import operation proceed.
    (if (eq (displayType mbResourceChooser) 'file)
      ;; Import from a file!
      (make-new-media-from-file)
      ;; Normal resource importing.
      (let ((thingsToMake (selectedItems (picker (fileList mbResourceChooser))))
            (theTranslator (mySK8Type mbMakePopUp))
            (targetProj (targetProject ui))
            (moveResources? (checked mbCopyMedia))
            (theFile (outputFile mbResourceChooser)))
        ;; Some error checking...
        (cond ((null targetProj) (error "No Project Available for Import."))
              ((null theTranslator) (error "No translators available."))
              ((null theFile) (error "No file is available to get media from."))
              (t ;; Everything is ok. Make the things.
               (make-new-media theFile thingsToMake theTranslator targetProj moveResources?)))))))

(new DialogBoxButton :objectName "MBImportAll" :project ui)
(setf (container mbImportAll) mbResourceChooser)
(setf (text mbImportAll) "Import All")
(setBoundsRect mbImportAll 230 200 312 218)
(setf (enabled mbImportAll) nil)

;;; ImportAll means different things in different situations:
;;; 1. When displayType = restype, imports all resources of the type selected.
;;; 2. When displayType = resources, imports everything (not just what is selected).

;;; Note that this does not work for files because we cannot choose what translator to use
;;; for each type of media present.

(define-handler mouseUp (mbImportAll)
  (when (memq (displayType mbResourceChooser) '(resources restype))
    ;; We are ready to import!
    (let ((theTranslator (mySK8Type mbMakePopUp))
          (targetProj (targetProject ui))
          (moveResources? (checked mbCopyMedia))
          (theFile (outputFile mbResourceChooser)))
      (cond ((or (null targetProj) (memq targetProj (list sk8 ui)))
             (messageToUser "No Project Available for Import."))
            ((null theTranslator) (messageToUser "No translators available."))
            ((null theFile) (messageToUser "No file is available to get media from."))
            (t ;; Everything is ok. Make the things.
             (if (eq (displayType mbResourceChooser) 'resources)
               ;; Items are ready to be taken.
               (make-new-media theFile (items (picker (fileList mbResourceChooser))) 
                               theTranslator targetProj moveResources?)
               ;; Get the items from the resource file.
               (let* ((thePicker (picker (fileList mbResourceChooser)))
                      (thingsToMake (macgetResourceHandleInfo theFile (first (selectedItems thePicker)))))
                 ;; Putting thingsToMake in the same representation as when resources are visible
                 ;; in the picker.
                 (setf thingsToMake (mapcar #'(lambda (x y) (list x y))
                                            (first thingsToMake) (cadr thingsToMake)))
                 (make-new-media theFile thingsToMake theTranslator targetProj moveResources?))))))))
               
(define-handler resized (mbResourceChooser)
  (sk8-multival-bind (hSize vSize) (size me)
    (setBoundsRect (FileList me) 10 35 (- hsize 105) (- vsize 10))
    (setBoundsRect (OpenButton me) (- hsize 94) (- vsize 90) (- hsize 5) (- vsize 64))
    (setBoundsRect (DeskTopButton me) (- hsize 90) (- vsize 129) (- hsize 9) (- vsize 111))
    (setLocation (Menu me) (/ (- hsize 95) 2) 20)))

(define-handler (setf currentDirectory) (theval mbResourceChooser)
  (unless (fileExists theVal)
    ;; If the directory is no longer found, revert to something that we know exists.
    (setf theVal SK8Directory))
  (setf (currentDirectory system) theval)
  (call-next-method theVal me))

(resized mbResourceChooser)

;;; Just in case the Ui acts up again!

(define-handler uiColorize ((picker (fileList mbResourcechooser)))
  (setf (highlightcolor me) uilightcolor))
(define-handler uiColorize ((fileList mbResourcechooser))
  )

(define-handler (setf fillcolor) (theval (picker (fileList mbResourcechooser)))
  (call-next-method white me))

(define-handler restore ((picker (fileList mbResourcechooser)))
  (call-next-method)
  (setf (textsize me) 9))

(define-handler selectionCompleted ((picker (fileList mbresourceChooser)))
  (let* ((mytype (displayType mbResourceChooser))
         (restype? (eq mytype 'restype))
         (filetype? (eq mytype 'file))
         (firstItem (first (selectedItems me))))
    (when restype?
      (makeMenu mbMakePopUp))
    (when (or (not filetype?) 
              (and firstItem 
                   (probe-file firstItem)))
      (call-next-method)
      (when (and filetype?
                 (null (importableObjects mbResourceChooser)))
        (unless (eq (fillcolor mbresourceSampler) White)
          (setf (fillcolor mbResourceSampler) white))
        ;; disable import and the menu.
        (setf (enabled mbmakepopup) nil
              (enabled mbImportButton) nil)))))

(define-handler enteringStage (mediaBrowser)
  (call-next-method)
  (setf (currentdirectory mbResourcechooser) (currentdirectory system))
  (setf (keytarget me) (picker (fileList mbResourcechooser)))  ;; Set the keytarget so users can start typing right away!
  )

(new label :objectname "MBPreviewTitle" :project ui)
(setf (text mbPreviewTitle) "Preview")
(setf (container mbPreviewTitle) mediaBrowser)
(setf (textColor mbPreviewTitle) black)
(setf (textFont mbPreviewTitle) espysansboldfont)
(setf (textsize mbPreviewTitle) 9)
(setf (textStyle mbPreviewTitle) '(plain))
(setLocation mbPreviewTitle 80 85)

(new Rectangle :objectname "MBResourceSampler" :project ui)
(setBoundsRect mbResourceSampler 20 100 150 210)
(setf (container mbresourceSampler) mediaBrowser)
(setf (framecolor mbresourceSampler) FrameScore)

(new UIcheckBox :objectName "MBShowPreview" :project ui)
(setf (container mbShowPreview) mediabrowser)
(setf (text mbShowPreview) "Show Preview")
(setf (checked mbShowPreview) nil)
(setlocation mbShowPreview 90 230)

(define-handler (setf checked) (boolean mbShowPreview)
  (call-next-method)
  ;; Throws away the imageRenderer that was being used. Mmm...
  (if boolean
    (when (outputhandle mbResourceChooser)
      (displaySample (outputHandle mbResourceChooser) mbResourceSampler))
    (progn 
      (setValue 'outputHandle mbResourceChooser nil)
      (setf (fillcolor mbResourceSampler) white))))

;;; Hooking up the sampler and the viewer. Those symbols!!!

(define-handler (setf outputHandle) (newHandle mbResourceChooser)
  (setf (slot-value me 'outputhandle) newHandle)
  (let ((theDisplayType (displayType mbResourceChooser)))
    (unless (eq theDisplayType 'restype)
      ;; Show the preview.
      (when (checked mbShowPreview)
        (displaySample newHandle mbResourceSampler)))
    ;; If we are showing files, we might have to update the import control panel.
    (when (eq (displayType mbResourceChooser) 'file)
      (makeMenu mbmakepopup)
      (setf (enabled mbmakepopup) t
            (enabled mbimportbutton) t))
    newHandle))

;;; (2) The Import panel!

(new rectangle :objectname "MBImportSep" :project ui)
(setf (container mbImportSep) mediaBrowser)
(setBoundsRect mbImportSep 20 280 480 282)

(new label :objectname "MBImportTitle" :project ui)
(setf (text mbImportTitle) "Import Media...")
(setf (container mbImportTitle) mediaBrowser)
(setf (textColor mbImportTitle) black)
(setf (textFont mbImportTitle) espysansboldfont)
(setf (textStyle mbImportTitle) '(plain))
(setf (textsize mbImportTitle) 9)
(setLocation mbImportTitle 75 300)

(new uiradioButton :objectName "MBCopyMedia" :project ui)
(setf (text mbCopyMedia) "Copying media to project file")
(setf (container mbCopyMedia) mediaBrowser)
(setLocation mbCopyMedia 150 325)
(setf (left mbCopyMedia :resizing nil) 60)
(setf (checked mbCopyMedia) t)

(new uiradioButton :objectName "MBCopyFileReference" :project ui)
(setf (text mbCopyFileReference) "Leaving media in original file")
(setf (container mbCopyFileReference) mediaBrowser)
(setLocation mbCopyFileReference 151 345)
(setf (left mbCopyFileReference :resizing nil) 60)

(new label :objectname "MBAsTitle" :project ui)
(setf (text mbAsTitle) "Import As:")
(setf (container mbAsTitle) mediaBrowser)
(setf (textColor mbAsTitle) black)
(setf (textFont mbAsTitle) espysansboldfont)
(setf (textStyle mbAsTitle) '(plain))
(setf (textsize mbAsTitle) 9)
(setLocation mbAsTitle 355 300)

;;; The mbMakeMenu.

(new menu :objectname "MBMakePopUp" :project ui
     :properties '((myHolder :value nil)
                   (mySk8Type :value nil)
                   (enabled :value nil)))

(define-handler (setf enabled) (boolean mbMakePopUp)
  (setf (slot-value me 'enabled) boolean)
  (if boolean
    (setf (textColor me) black)
    (setf (textColor me) Gray)))

(setf (enabled mbMakePopUp) nil)

(define-handler mousedown (mbMakePopUp)
  (when (enabled me)
    (call-next-method)))

(setf (container mbMakePopUp) mediaBrowser)
(setLocation mbMakePopUp 395 325)
(setf (text mbMakePopUp) "ImageRenderer")
(setf (textFont mbMakePopUp) EspysansFont)
(setf (textSize mbMakePopUp) 9)

(new menuItem :objectname "MBMakeMenuItem" :project ui
     :properties '((makeType :value nil)))

(define-handler menuSelect (mbMakeMenuItem)
  (setf (text (menu me)) (text me))
  (setf (mySk8Type (menu me)) (makeType me)))

(define-handler makeMenu (mbMakePopUp)
  ;; [1] Dispose current menu items.
  (dolist (c (menuItems me))
    (setf (menu c) nil))
  ;; [2] Make the new items.
  (let (theItem)
    (dolist (c (importableObjects mbResourceChooser :justTranslators t))
      (setf theItem (new mbMakeMenuItem :project ui :makeType c 
                         :text (objectString (finalObject c))))
      (setf (menu theItem) me)))
  ;; [3] Set the menu's text.
   (let ((theItems (menuitems me))
        highItem theTranslator)
    ;; Try to use an indirect translator here (this is what the user will most often want).
    (dolist (c theItems (setf highItem (first theItems)))
      (setf theTranslator (makeType c))
      (when (neq (internalObject theTranslator) (finalObject theTranslator))
        (setf highItem c)
        (return)))
    (setf (text me) (text highItem))
    (setf (mySk8Type me) (makeType highItem))))

(define-handler resized (mediaBrowser)
  (call-next-method)
  (if (and *MacStyleInterface* (not (= (v mbPreviewTitle) 65)))
    (dolist (i (contents me))
      (setf (v i) (- (v i) 20))))
  (if (and (not *MacStyleInterface*) (not (= (v mbPreviewTitle) 85)))
    (dolist (i (contents me))
      (unless (or (eq i (closeboxrect me)) (eq i (growboxrect me)))
        (setf (v i) (+ (v i) 20)))))
  )

;;; ______________________________
;;; IMPORTING DATA!!!
;;; ______________________________

;;; Find all things to be made.
;;; Run through them and make them.
;;; Options: how to name them? Preserve name? Sequential name?

;;; Note 1: we make whetever is showing in the sampler.

(defun new-objects-to-overviewer (inproj theResult)
  (when (eq (targetproject ui) inproj)
    (setf (container projectoverviewer) stage)
    (withactorlocked (projectoverviewer)
      (click POBPile)
      (setf (inputobjects (picker (picker POBPile))) 
            (append theresult (inputobjects (picker (picker POBPile)))))
      (setf (selecteditems (picker (picker POBPile))) theresult))))

(defun make-new-media (fromFile thingsToMake theTranslator inProj &optional moveResources?)
  (let* ((extObj (restype mbResourceChooser))
         (counter 1)
         (source (outputHandle mbResourceChooser))
         (mediaType (internalObject theTranslator))
         destination
         theResult naming name? theName sequential? newFile numToMake)
    ;; [1] Forget about all things not of the right type. After this we know that
    ;; everything is a valid thing.
    (unless thingsToMake 
      (setf thingsToMake (selectedItems (picker (fileList mbResourceChooser)))))
    (setf numToMake (length thingsToMake))
    ;; [2] If the resources are not to be moved, make a copy of the file in the current project
    ;; and use it for the import operation.
    (unless moveResources?
      (setf newFile (findFileInProject fromFile inProj)))
    ;; [3] Prepare the Source resource.
    ;; Make sure the resource to be used as the source is of the rigth type.
    (unless (string= (restype source) extObj)
      (setf source (getHolderOfType mbResourceChooser :theType extObj)))
    ;; Just in case: clear the handle.
    (unloadMedia source)
    ;; [4] Bring up name dialog to figure out what to do about names.
    (setf naming (maybenameDialog numToMake (internalObject theTranslator) inProj))
    (setf name? (first naming)
          theName (cadr naming)
          sequential? (caddr naming))
    ;; [5] Loop and create. Return the result as a list.
    (sk8::withlockedcursor animatedClock
      (without-interrupts 
       (dolist (c thingsToMake theResult)
         (tickEventClock)
         ;; [a] Setup the source.
         (setf (resourceId source) (cadr c))
         ;; [b] Make the media object.
         (setf destination (new mediaType :project inProj))
         ;; [c] if the resource is not to be moved, set the file of destination to fromFile.
         (unless moveResources?
           (setf (file destination) newFile))
         ;; [d] Call import to do the work and fill in the resource.
         ;; We bind again because in the case of 2 step creation the import operation does not
         ;; return the media (eg. QDPicture) but the derived object (eg. an ImageRenderer).
         (setf destination (sk8::import theTranslator :source source :destination destination))
         ;; [e] Name it if required.
         (when name?
           (cond ((and (eq name? :resnames) (first c))
                  (when (and (stringp (first c)) 
                             ;; This deals with repeated resource names.
                             (not (nameAlreadyInUse (first c) inProj)))
                    ;; Resources without names yield anonymous objects.
                    (setf (objectName destination) (sk8::removeSpaces (first c)))))
                 (t 
                  (if sequential?
                    (progn (setf (objectName destination) (format nil "~a~a" theName counter))
                           (incf counter))
                    (setf (objectName destination) theName)))))
         ;; [4] Put it in the result.
         (push destination theResult))
       ;; Update the resfile just in case.
       (when (sk8::swapfile inProj)
         (macUpdateResourceFile (sk8::swapfile inProj)))))
    ;; Return everything that was created.
    (new-objects-to-overviewer inproj theResult)
    theResult))

;;; Assumes the resource chooser is in file mode and the current file is importable.

(defun make-new-media-from-file ()
  (let* ((thefile (outputFile mbResourceChooser))
         (theTranslator (mySk8type mbmakepopup))
         (inproj (targetProject ui))
         (moveResources? (checked mbCopyMedia))
         theResult)
    ;; the file should already be loaded with the right pathname or the 
    ;; import button would not have been enabled.
    (if thetranslator
      (sk8::withLockedCursor animatedClock
        (let ((theNewObj (new (internalObject theTranslator) :project inproj)))
          ;; Make sure we have a file in the right project.
          (setf theFile (findFileInProject theFile inproj))
          (unless moveResources?
            (setf (file theNewObj) theFile))
          (sk8::import theTranslator :source theFile :destination thenewObj)
          (push theNewObj theResult)
          ;; Notify the ui.
          (new-objects-to-overviewer inproj theresult)
          ;; Return the result.
          theResult))
      (error "Although file ~a seems to be importable, did not find translator for it." 
             thefile))))


#|
	Change History (most recent last):
	1	10/29/93	hernan	New file. The new media browser. Uses the 
				media components.
	2	10/29/93	hernan	Do not put the mediaBrowser on the Stage.
	3	10/29/93	hernan	Making the buttons the right size.
	4	11/3/93	hernan	Lots of things.
	5	11/8/93	hernan	Improving all the time. Now lets you import things
				without moving them to your project.
	6	11/8/93	hernan	Making a dialog popup when media creation is done. Also added 4 restore methods to undo what UI
				colorize does to these guys.
	7	11/8/93	hernan	EnterInField moved to a place where importMedia
				has been created.
	8	11/10/93	hernan	Fixing problems when trying to name a new object
				with a resource number (no resource name).
	9	11/12/93	hernan	Lots of stuff mainly dealing with stopping the user
				from using the creation panel when no projects 
				are available.
	10	11/15/93	hernan	Fixing make-new-media so that the mediabrowser
				does not choke when you try to name 2 objects
				with the same name (since resource names can be
				repeated).
	11	11/15/93	hernan	Updating tag names after Brian's subversive renaming.
	12	11/19/93	hernan	Radically changed to use the new resourceChooser
				component.
	13	11/22/93	hernan	Making the what to make menu be built whenever
				the user goes into a resource type.
	14	11/22/93	hernan	Fixing the makemenu to recompute itme at the
				right times.
	15	11/22/93	hernan	Fixing make-new-media to stop depending on the
				holder to know the resource type.
	16	12/1/93	hernan	Making the originalSize checkbox be checked 
				when the resourceSampler's showOriginalSize is
				true.
	17	12/2/93	rod	modified prepareForStage to set the initial
				keytarget.  This way users can start typing as
				soon as it comes up.
	18	12/3/93	hernan	Deactivate will close the resource file.
	19	12/10/93	hernan	Only close the resource file when it was not
				open in the first place.
	20	12/17/93	till	#.'s be gone: restore, activateText, keydown
	21	1/10/94	hernan	Moving removeSpaces to the d2 utilities.
	23	1/14/94	rod	
	24	2/11/94	sidney	removespaces is now in SK8::removespaces since it was moved
	25	2/12/94	kleiman	name changes
	26	2/14/94	sidney	rename descendants to knowndescendants
	27	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	28	2/21/94	hernan	window -> sk8::window.
	29	2/25/94	hernan	Using symbols instead of keywords for options!!!
	30	2/26/94	rod	hilighter->highlighter
	31	2/28/94	hernan	Fixing leftover keyword stuff.
	32	2/28/94	hernan	Avoiding disposing things directly.
	33	3/2/94	Hernan	Porting to Fred 3.0
	34	3/3/94	Hernan	The great handler argument name renaming of 94!
	35	3/7/94	Hernan	Changing layout to make UI happy. Later we will
				add some functionality.
	36	3/7/94	Hernan	Fixing left over references to disposed objects
				originating in the layout changes to this file.
	37	3/7/94	Hernan	And more fixes to this object...
	38	3/8/94	Hernan	Fixing the name dialog to have the name button
				checked when you get in.
	39	3/8/94	Hernan	Fixing location of the radiobuttons.
	40	3/15/94	Hernan	Removing weird relocation of labels when the
				naming dialog comes up.
	41	3/17/94	Hernan	Adding a preview label to the preview box and
				fixing a bug with the original size checkbox.
	42	3/17/94	Hernan	Now refreshing the file list when the browser
				enters the stage. The idea is that although we 
				may already have the files in the picker, they 
				may have changed while the browser was closed.
	43	3/21/94	Hernan	Fixing import all button to pass the right thing to
				make-new-media.
	44	3/23/94	Hernan	Making media with the animated watch cursor.
	45	3/23/94	Hernan	After the media has been copied we update the 
				resource file (as a safeguard in case something
				bad happens before the file is written to disk).
	46	3/26/94	rod	
	47	3/29/94	Hernan	If we are making a media object without copying
				the resource, we need to put the id in the slot
				after the media gets created. It cannot be done
				from the handle at import time since the handle is
				no longer a resource.
	48		3/30/94	Hernan	Better yet, if the resource is not to be copied, the
							import function takes the id instead of the handle.
							Also changing "Create!" to "Create" and making
							sure return on the field activates the button.
	49		3/31/94	Hernan	Using withLockedCursor instead of withCursor.
	51		4/18/94	rod	Layout fixes...
	52		4/22/94	Hernan	Removing modal dialog that informs you that 
							media was actually made.
	53		5/16/94	till	Less consing for maybe-newResfile.  
							(mapKnownDescendants sweep).
	54		5/17/94	sidney	media resources go to swapfile of project, not file
	55		5/22/94	rod	Here begins the experimentation with a Mac look to the UI.
	56		6/1/94	rod	New layout
	57		6/1/94	Brian	
	58		6/3/94	rod	
	59		6/3/94	rod	
	60		6/23/94	sidney	support lazy creation of swapfile
	61		7/8/94	Hernan	New implementation of Translators.
	62		7/8/94	Hernan	Removing reference to deceased property.
	63		7/11/94	Hernan	Allowing preview of things in files.
	64		7/12/94	Hernan	Set outputhandle will not show a preview if the
							resourceChooser is showing resource types.
	65		7/13/94	Hernan	Fixing make-new-file-media to call import no
							matter what. This is required to make 2 step
							importing work with file media.
	66		7/15/94	Hernan	1174716: makeMenu now selects a high level 
							translator as the default.
	67		7/15/94	rod	Removing zoombox from window.
	68		8/22/94	rod	updating the currentdirectory of the system as 
							the directory is changed.
	69 	 8/24/94	Hernan  	1182780: making the import buttons enable and
							disable themselves properly.
	70 	 9/ 2/94	sidney  	espysans[bold] -> espysans[bold]Font
	71 	 9/ 5/94	rod     	
	72 	 9/ 9/94	dy      	String comparison of Ostypes should be case
							sensitive.
	73 	 9/ 9/94	dy      	disable Import button until appropriate
	74 	 9/12/94	Hernan  	1180502: capitalizing object names.
	75 	 9/20/94	Hernan  	1187352: making make-new-media-from-file
							do its thing while spinning the watch cursor.
	76 	10/ 3/94	Hernan  	handle -> mediaData.
	77 	11/22/94	Hernan  	1200350: making the show preview button show
							the preview when clicked on.
	78 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	79 	 3/ 8/95	till    	bug 1205997, selectionCompleted of mbResourceChooser
							needs to give up if the file doesn't exist.
	80 	 3/ 8/95	Hernan  	to focus the overviewer on the drop pile we have
							to click the button, not mouseDown on it.
	81 	 3/ 9/95	sidney  	when is a filelist not a filelist? when it is a list of restypes!
	82 	 4/14/95	Hernan  	1238718: set currentDirectory of mbResourceChooser reverts the
							directory to SK8Directory if the directory specified no
							longer exists.
	2  	 6/26/95	Hernan  	set checked of mbShowPreview clears the outputhandle
							when checked off.
	3  	 2/14/96	Brian   	
	2  	 9/20/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
