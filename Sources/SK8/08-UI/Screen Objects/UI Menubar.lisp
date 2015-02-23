;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  4-29-96   5:50 pm
                  UI::MBMEDIABROWSER UI::MBWINDOWCOLOR UI::PBMENUHELP UI::PBMENULOADSCRIPT UI::PBMENUSAVEASSTANDALONE
                  UI::PBMENUSAVELIBASTEXT UI::PBSEARCHER UI::SAVEASEDITOR)


;;__________________________________________________________________
;; Little menuitem we use in here

(new menuItem :objectName "BringUpMenuItem" :project ui
     :properties '(actor label))

(define-handler Update (BringUpMenuItem)
  (setf (text me) (label me))
  (setf (checkmark me) (eq (container (actor me)) stage)))

(define-handler menuSelect (BringUpMenuItem)
  (when (typep (actor me) 'sk8::object)
    (bringUp (actor me))))

;;__________________________________________________________________
;;__________________________________________________________________
;; THE MENUS
;;__________________________________________________________________
;;__________________________________________________________________
;; File menu
;; Open and close projects, browse, etc.

(new uiMenuActor :objectName "PBMenuProject" :project ui)
(setf (text PBMenuProject) "File")

;;__________________________________________________________________
;; NEW Project

(new menuItem :objectName "PBMenuNew" :project ui)
(setf (text PBMenuNew) "New...")
(setf (menu PBMenuNew) PBMenuProject)
(setf (commandkey PBMenuNew) #\N)

(define-handler menuSelect (PBMenuNew)
  
  (let ((proj (if (or (eq (targetproject ui) sk8)
                      (eq (targetproject ui) ui))
                sk8
                (if (YesOrNoDialog (format nil "Create a new SubProject of ~a or a new project in SK8?"
                                           (objectName (targetproject ui)))
                                   :notext (objectname (targetproject ui))
                                   :yestext "SK8")
                  sk8 (targetproject ui)))))
    (sk8::NewProjectDialog :project proj)
    ))

;;__________________________________________________________________
;; Load Project

(new menuItem :objectName "PBMenuLoad" :project ui)
(setf (text PBMenuLoad) "Open...")
(setf (menu PBMenuLoad) PBMenuProject)
(setf (commandkey PBMenuLoad) #\O)

(define-handler menuSelect (PBMenuLoad)
  (sk8::OpenProjectDialog)
  )

;;__________________________________________________________________
;; Close Project

(new menuItem :objectName "PBMenuClose" :project ui)
(setf (text PBMenuClose) "Close")
(setf (menu PBMenuClose) PBMenuProject)

(define-handler Update (PBMenuClose)
  (if (and (targetProject ui)
           (neq (targetProject ui) sk8))
    (progn
      (setf (enabled me) t)
      (setf (text me) (concatenate 'string "Close " (objectstring (targetproject ui) :project (targetproject ui)))))
    (progn
      (setf (enabled me) nil)
      (setf (text me) "Close"))
    ))

;; Ask the user if project file should be saved before closing the project or quitting SK8
(defun maybeSaveProject ()
  (let ((proj (targetProject ui)))
    (when (and (neq proj sk8::sk8)
               (neq proj ui)
               (sk8::file proj))
      (let ((curprojname (objectstring proj :project proj)))
        (unless (or *override-quit-questions*
                    (YesOrNodialog (concatenate 'string
                                                "Before closing, do you wish to save "
                                                curprojname
                                                "?")
                                   :cancel t :notext "Save" :yestext "No"))
          (menuSelect PBMenuSave :force t))))))

;; Ask about saving project files before quitting SK8
(defun safeCloseAllProjects ()
  (sk8::withLockedCursor watchCursor
    (let ((projects (remove sk8::sk8
                            (sk8::knownchildren project)))
          lastproj)
      (when (and (boundp 'sk8::ui) sk8::ui)  ;; some things don't do if the ui has been removed in a standalone app
        (setf (container projectoverviewer) nil)
        (setf (container pbmholder) nil)
        (setf projects (delete sk8::ui projects)))
      (setf lastproj (car (last projects)))
      (dolist (proj projects)
        (ignore-errors
         (when (sk8::file proj)
           (setf (targetProject ui) proj)
           (maybeSaveproject)))
        (unless (eq proj lastproj)
          (ignore-errors (closeproject proj))))
      ;; get rid of any other files in the temporary folder now that we know the real files are gone
      (when (eql ccl::*quitting* t) ;;*** KLUDGE ALERT!! This appears to be how to distinguish a simple quit from a buildstandalone, but it is only an empirical observation that may not work in the future
        (ps::delete-all-temporary-files))  ;; If we are saving a standalone app, these files may be used and deleted later, don't delete them now
      )))

(pushnew #'safeCloseAllProjects mf::*quit-functions*)

(defun doTheCloseProjectThing (curproj )
  (let ((curprojname (objectstring curproj :project curproj)))
    (setf (targetproject ui) curproj) ;; should already be like this, but just in case
    (maybeSaveProject)  ;;; This gives us the confirmation and option to save...
    (sk8::withLockedCursor animatedClock
      (gs:without-events
        
        (without-interrupts
         (sendToLog (concatenate 'string 
                                 "Closing project " 
                                 curprojname 
                                 "..."))
         (tickeventclock)
         (let ((cc (delete curproj (delete sk8 (delete ui (knowndescendants project))))))
           (unless cc (setf (container projectoverviewer) nil))
           (setf (targetProject ui) (if cc (car cc) sk8))
           (when (and (not cc) (container pbmholder))
             (if ss::!*lisp-dev-mode*
               (progn
                 (setf (sk8:menubar stage) nil)
                 (set-menubar *default-menubar*)
                 (gs:showmenubar :force t))
               (setf (sk8::menubar stage) projectbuildermenubar)))
           (tickeventclock)
           (clearReferences ui)
           (tickeventclock)
           (clearReferences projectoverviewer :objects (list curproj))
           (clearReferences drawpalette :objects (list curproj))
           (clearReferences messagebox :objects (list curproj))
           (clearReferences uiscripteditor :objects (list curproj))
           (clearReferences objecteditor :objects (list curproj))
           (closeProject curproj)
           (sendToLog "... Done!")))))))

(define-handler menuSelect (PBMenuClose)
  (let ((curproj (targetProject ui)))
    (when (and curproj
               (neq curproj sk8::sk8)
               (neq curproj ui))
      (sk8dev::Eval-In-New-Thread "Closing Project" `(doTheCloseProjectThing ,curproj)))))

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuProject)

;;__________________________________________________________________
;; Save Project

(new menuItem :objectName "PBMenuSave" :project ui)
(setf (text PBMenuSave) "Save")
(setf (menu PBMenuSave) PBMenuProject)
(setf (commandkey PBMenuSave) #\S)


(define-handler Update (PBMenuSave)
  (if (and (targetProject ui)
           (file (targetProject ui))
           )
    (progn
      (setf (enabled me) t)
      (setf (text me) (concatenate 'string "Save " (objectstring (targetproject ui) :project (targetproject ui)))))
    (progn
      (setf (enabled me) nil)
      (setf (text me) "Save"))
    ))

(defun closeSubprojectsBeforeSavingParent (proj)
  ;; first take care of any subprojects, which must be closed before this one can be saved
  (unwind-protect
    (mapc #'(lambda(subproj)
              (ignore-errors (setf (targetProject ui) subproj)
                             (maybeSaveproject)
                             (closeproject subproj)))
          (requiringprojects proj))
    ;; make sure parent proect is left as target, even if user canceled out of the save dialog
    (setf (targetProject ui) proj))
  )

(defun doTheSaveProjectThing (proj)
  (closeSubprojectsBeforeSavingParent proj)
  (sk8dev::withLockedCursor animatedClock
    ;; un-edit windows
    (sendToLog (concatenate 'string
                            "Saving " 
                            (objectString proj :project proj) 
                            "..."))
    ;; Save the preferences
    (savePreferences ui proj)  ;;;; note this puts the objects from the clears list into the pref object...
    ;; save the project
    (sk8::saveproject proj)
    (setf (sk8::preferences proj) nil)
    (when (eq (eventWindow) MessageBox)
      (setf (savedThisActivation MessageBox) t))
    ;; restore the windows
    (sendToLog "... Done!")
    ))

(define-handler menuSelect (PBMenuSave &key (force nil))
  (let ((proj (targetProject ui)))
    (when (and proj
               (neq proj sk8::sk8)
               (or (control-key-p)
                   (neq proj ui)))
      (when (or force 
                (yesornodialog (concatenate 'string "Save "
                                            (objectstring proj :project proj)
                                            "?"  
                                            (if (requiringprojects proj)
                                              "  Note that this project's subprojects will be closed before it is saved and you will be given a chance to save them first."
                                              ""))
                               :cancel nil))
        (if force
          (doTheSaveProjectThing proj) ;;; if force don't queue it!!!
          (sk8dev::Eval-In-New-Thread "Saving Project" `(doTheSaveProjectThing ,proj)))))))

;;__________________________________________________________________
;; Save As

(new menuItem :objectName "PBMenuSaveAs" :project ui)
(setf (text PBMenuSaveAs) "Save Copy As...")
(setf (menu PBMenuSaveAs) PBMenuProject)

(define-handler Update (PBMenuSaveAs)
  (setf (enabled me) (enabled PBMenuSave)))

(defun doTheSaveProjectAsThing (proj newfile)
  (closeSubprojectsBeforeSavingParent proj)
  (sk8dev::withLockedCursor animatedClock
    (sendToLog (concatenate 'string
                            "Saving " 
                            (objectString proj :project proj) 
                            "..."))
    ;; Save the preferences
    (savePreferences ui proj)
    ;; save the project
    (sk8::saveprojectas proj newfile)
    (setf (sk8::preferences proj) nil)
    (sk8dev::make-object-gcable newfile)
    (sendToLog "... Done!")
    ))

(define-handler menuSelect (PBMenuSaveAs)
  (let ((proj (targetProject ui))
        newfile)
    (when (or (null (requiringprojects proj))
              (yesornodialog (concatenate 'string "Save in another file "
                                          (objectstring proj :project proj)
                                          "?"  
                                          "  Note that this project's subprojects will be closed before it is saved and you will be given a chance to save them in their project files first.")
                             :cancel nil))
      (setf newfile (newfiledialog :title (concatenate 'string "Save "
                                                       (objectstring proj :project proj)
                                                       " in the file:")
                                   :project sk8::sk8))
      (when newfile
        (sk8dev::Eval-In-New-Thread "Saving Project As" `(doTheSaveProjectAsThing ,proj ,newfile))))))

;;__________________________________________________________________
;; Save As Text

(new menuItem :objectName "PBMenuSaveAsText" :project ui)
(setf (menu PBMenuSaveAsText) PBMenuProject)
(setf (text PBMenuSaveAsText) "Save As Text...")

(define-handler Update (PBMenuSaveAsText)
  (setf (enabled me) (enabled PBMenuSave))
  )

(defun doTheSaveAsTextThing (proj newfile)
  (closeSubprojectsBeforeSavingParent proj)
  (sk8dev::withLockedCursor animatedClock
    (sendToLog (concatenate 'string
                            "Saving text sources of " 
                            (objectString proj :project proj) 
                            "..."))
    (sk8::writesources proj (physicalname newfile))
    (sk8dev::make-object-gcable newfile)
    (sendToLog "... Done!")
    ))

(define-handler menuSelect (PBMenuSaveAsText)
  (let ((proj (targetProject ui))
        newfile)
    (when (or (null (requiringprojects proj))
              (yesornodialog (concatenate 'string "Save as text "
                                          (objectstring proj :project proj)
                                          "?"  
                                          "  Note that this project's subprojects will be closed before it is saved and you will be given a chance to save them in their project files first.")
                             :cancel nil))
      (setf newfile (newfiledialog :title (concatenate 'string "Save "
                                                       (objectstring proj :project proj)
                                                       " as text in the file:")
                                   :project sk8::sk8  ;; so temp file obj not saved with project
                                   ))
      (when newfile
        (sk8dev::Eval-In-New-Thread "Saving As Text" `(doTheSaveAsTextThing ,proj ,newfile))))))

;;__________________________________________________________________
;; Save As Standalone

(new menuItem :objectName "PBMenuSaveAsStandalone" :project ui)
(setf (menu PBMenuSaveAsStandalone) PBMenuProject)
(setf (text PBMenuSaveAsStandalone) "Save As StandAlone")

(define-handler Update (PBMenuSaveAsStandalone)
  (setf (enabled me) (enabled PBMenuSave))
  )

(defun doTheSaveAsStandAloneThing (proj)
  (closeSubprojectsBeforeSavingParent proj)
  (sk8dev::withLockedCursor animatedClock
    ;; un-edit windows
    (sendToLog (concatenate 'string
                            "Saving " 
                            (objectString proj :project proj) 
                            "..."))
    ;; Save the preferences
    (savePreferences ui proj)  ;;;; note this puts the objects from the clears list into the pref object...
    ;; save the project
    (sk8::saveproject proj :forStandAlone t)
    (setf (sk8::preferences proj) nil)
    (when (eq (eventWindow) MessageBox)
      (setf (savedThisActivation MessageBox) t))
    ;; restore the windows
    (sendToLog "... Done!")
    (messageToUser 
     (format nil "To build your application, drop the saved project file onto ~
                  the \"SK8 Runtime\" application.")
     :beep t)
    ))

(define-handler menuSelect (PBMenuSaveAsStandalone &key (force nil))
  (let ((proj (targetProject ui)))
    (when (and proj
               (neq proj sk8::sk8)
               (or (control-key-p)
                   (neq proj ui)))
      (when (or force 
                (yesornodialog (concatenate 'string "Save "
                                            (objectstring proj :project proj)
                                            "?"  
                                            (if (requiringprojects proj)
                                              "  Note that this project's subprojects will be closed before it is saved and you will be given a chance to save them first."
                                              ""))
                               :cancel nil))
        (if force
          (doTheSaveAsStandAloneThing proj) ;;; if force don't queue it!!!
          (sk8dev::Eval-In-New-Thread "Saving Standalone" `(doTheSaveAsStandAloneThing ,proj)))))))

;;__________________________________________________________________
;; COMPACT PROJECT STORE
;; Let's the user choose a project store file to compact...

(new menuItem :objectName "PBMenuCompactProject" :project ui)
(setf (text PBMenuCompactProject) "Compact Project Code")
(setf (menu PBMenuCompactProject) PBMenuProject)

(define-handler Update (PBMenuCompactProject)
  (if (and (targetProject ui)
           (neq (targetProject ui) sk8::sk8)
           (neq (targetProject ui) ui))
    (progn
      (setf (enabled me) t)
      (setf (text me) (concatenate 'string "Compact " 
                                   (objectstring (targetproject ui) :project (targetproject ui))
                                   "'s code"
                                   )))
    (progn
      (setf (enabled me) nil)
      (setf (text me) "Compact"))))

(defun doTheCompactProjectThing (curproj)
  (sk8::withLockedCursor animatedclock
    (sendToLog (concatenate 'string
                            "Compacting Project " 
                            (objectString curproj :project curproj) 
                            "..."))
    
    (sk8::compactProjectCode curproj)
    (sendToLog "... Done!")))

(define-handler menuSelect (PBMenuCompactProject)
  (let ((curproj (targetProject ui)))
    ;; un-edit windows
    ;; *** took out watch cursor (done by compactStore) and took out yes-no dialog
    (sk8dev::Eval-In-New-Thread "Compacting Project" `(doTheCompactProjectThing ,curproj))))

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuProject)

;;__________________________________________________________________
;; Load Script File
;; 

(new menuItem :objectName "PBMenuLoadScript" :project ui)
(setf (text PBMenuLoadScript) "Load Script File...")
(setf (menu PBMenuLoadScript) PBMenuProject)
(setf (enabled PBMenuLoadScript) t)

(define-handler menuSelect (PBMenuLoadScript)
  (declare (ignore me))
  (when (or (neq (targetproject ui) sk8) (yesornodialog "The active project is SK8.  Scripts loaded into the SK8 project can not be saved.  Do you wish to continue?" :cancel nil))
    (let ((xx (openfiledialog :macfiletype "TEXT" :project ui)))
      (when xx
        (setf xx (name xx))
        (sk8dev::Eval-In-New-Thread (concatenate 'string "Loading Script File " (file-namestring xx)) `(loadscriptfile ,xx :project ,(targetproject ui)))))))


;;__________________________________________________________________
;; Media Browser or THE IMPORTER...
;; 

(new menuItem :objectName "PBMenuImport" :project ui)
(setf (text PBMenuImport) "Import Media...")
(setf (menu PBMenuImport) PBMenuProject)
(setf (enabled PBMenuImport) t)

(define-handler menuSelect (PBMenuImport)
  (bringup MediaBrowser))

#|
;;__________________________________________________________________
;; Import Library
;; 

(new menuItem :objectName "PBMenuImportLibrary" :project ui)
(setf (text PBMenuImportLibrary) "Import/Export Library")
(setf (menu PBMenuImportLibrary) PBMenuProject)

(define-handler menuSelect (PBMenuImportLibrary)
  (bringup LibraryEditor))
|#

#|

;;; This is replace by the "Save for Standalone" menuitem. HERNAN

;;__________________________________________________________________
;; BUILD STANDALONE
;; Does its thing without regard to what projects are up.

(new menuItem :objectName "PBMenuBuild" :project ui)
(setf (text PBMenuBuild) "Build StandAlone...")
(setf (menu PBMenuBuild) PBMenuProject)
(setf (enabled PBMenuBuild) t)

(define-handler menuSelect (PBMenuBuild)
  (buildstandalonedialog))

|#

#|
;;__________________________________________________________________
;; Compact

(new menuItem :objectName "PBMenuCompact" :project ui)
(setf (text PBMenuCompact) "Compact Memory")
(setf (menu PBMenuCompact) PBMenuProject)

(define-handler menuSelect (PBMenuCompact)
  (gc)) ; Compacts SK8 via GC hook
|#
;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuProject)


;;__________________________________________________________________
;; QUIT
;; Quits SK8.  Bye!

(new menuItem :objectName "PBMenuQuit" :project ui)
(setf (text PBMenuQuit) "Quit")
(setf (menu PBMenuQuit) PBMenuProject)
(setf (commandkey PBMenuQuit) #\Q)

(define-handler menuSelect (PBMenuQuit)
  (quit))



;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;; EDIT menu
;; Open and close projects, browse, etc.

(new uiMenuActor :objectName "PBMenuEdit" :project ui)
(setf (text PBMenuEdit) "Edit")

;;__________________________________________________________________
;; Undo Action

(new menuItem :objectName "PBMenuUndo" :project ui)
(setf (text PBMenuUndo) "Undo")
(setf (menu PBMenuUndo) PBMenuEdit)
(setf (commandkey PBMenuUndo) #\Z)

(define-handler Update (PBMenuUndo)
  (let ((usl (and (propertyname UndoableSetLog)
                  (objectlist UndoableSetLog)))
        (ty (and (eventWindow)
                 (keytarget (eventWindow)))))
    (setf (enabled me) (or usl ty))
    (if (and ty (inheritsfrom ty editText))
      (setf (text me) "Undo Typing")
      (if usl
        (setf (text me) (concatenate 'string "Undo Setting of "
                                     (if (listp (propertyname Undoablesetlog))
                                       "Multiple Properties"
                                       (name (propertyname Undoablesetlog)))))
        (setf (text me) "Undo"))))
  )

(define-handler menuSelect (PBMenuUndo)
  (let* ((eventWindow (eventWindow))
         (usl (and (propertyname UndoableSetLog) (objectlist UndoableSetLog)))
         (ty (and eventWindow (keytarget eventWindow)))
         (sels (selecteditems selectionhalo)))
    (if (and ty (is-a ty editText))
      (undo (keytarget eventWindow))
      (if usl
        (undolastset)
        ))
    (when (and sels (not (selecteditems selectionhalo)))
      (select sels))))

#|
  (if (and (eventWindow)
           (keytarget (eventWindow)))
    (undo (keytarget (eventWindow)))
    (undolastset)))
|#

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuEdit)


;;__________________________________________________________________
;; Cut Action

(new menuItem :objectName "PBMenuCut" :project ui)
(setf (text PBMenuCut) "Cut")
(setf (commandkey PBMenuCut) #\X)
(setf (menu PBMenuCut) PBMenuEdit)

(setf (enabled PBMenuCut) t)

(define-handler menuSelect (PBMenuCut)
  (if (eq (container selectionhalo) stage)
    (sk8::cutSelectionToClipboard (keytarget selectionhalo))
    (if (and (eventWindow) (keytarget (eventWindow)))
      (sk8::cutSelectionToClipboard (keytarget (eventWindow)))
      ;; A Lisp window must be active...
      (when gs:*currentTla*
        (cut gs:*currentTla*)))))

;;__________________________________________________________________
;; Copy Action

(new menuItem :objectName "PBMenuCopy" :project ui)
(setf (text PBMenuCopy) "Copy")
(setf (commandkey PBMenuCopy) #\C)
(setf (menu PBMenuCopy) PBMenuEdit)

(setf (enabled PBMenuCopy) t)

(define-handler menuSelect (PBMenuCopy)
  (if (eq (container selectionhalo) stage)
    (sk8::copySelectionToClipboard (keytarget selectionhalo))
    (if (and (eventWindow) (keytarget (eventWindow)))
      (copySelectionToClipboard (keytarget (eventWindow)))
      (when gs:*currentTla*
        (copy gs:*currentTla*)))))

;;__________________________________________________________________
;; Paste Action

(new menuItem :objectName "PBMenuPaste" :project ui)
(setf (text PBMenuPaste) "Paste")
(setf (commandkey PBMenuPaste) #\V)
(setf (menu PBMenuPaste) PBMenuEdit)

(setf (enabled PBMenuPaste) t)

(define-handler menuSelect (PBMenuPaste)
  (if (eq (container selectionhalo) stage)
    (sk8::pasteClipboardToSelection (keytarget selectionhalo))
    (if (and (eventWindow) (keytarget (eventWindow)))
      (pasteClipboardToSelection (keytarget (eventWindow)))
      (when gs:*currentTla*
        (paste gs:*currentTla*)))))

;;__________________________________________________________________
;; Clear Action

(new menuItem :objectName "PBMenuClear" :project ui)
(setf (text PBMenuClear) "Clear")
(setf (menu PBMenuClear) PBMenuEdit)

(setf (enabled PBMenuClear) t)

(define-handler menuSelect (PBMenuClear)
  (if (eq (container selectionhalo) stage)
    (sk8::clearSelection (keytarget selectionhalo))
    (if (and (eventWindow) (keytarget (eventWindow)))
      (clearSelection (keytarget (eventWindow)))
      (when gs:*currentTla*
        (clear gs:*currentTla*)))))

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuEdit)

;;__________________________________________________________________
;; Select All

(new menuItem :objectName "PBMenuSelectAll" :project ui)
(setf (text PBMenuSelectAll) "Select All")
(setf (menu PBMenuSelectAll) PBMenuEdit)
(setf (commandkey PBMenuSelectAll) #\A)

(setf (enabled PBMenuSelectAll) t)

(define-handler menuSelect (PBMenuSelectAll)
  (when (eventWindow)
    (selectAll (keyTarget (eventWindow)))))

(define-handler menuSelect (PBMenuSelectAll)
  (if (and (eventWindow) (keytarget (eventWindow)))
      (selectAll (keyTarget (eventWindow)))
      ))
;;__________________________________________________________________
;; Close Window

(new menuItem :objectName "PBMenuCloseWindow" :project ui)
(setf (text PBMenuCloseWindow) "Close Window")
(setf (menu PBMenuCloseWindow) PBMenuEdit)
(setf (commandkey PBMenuCloseWindow) #\W)


(define-handler menuSelect (PBMenuCloseWindow)
  (when (eventWindow)
    (sk8::close (eventWindow))))

;;__________________________________________________________________

(new menuItem :objectName "MessageBoxSearcherMenuItem" :project ui)
(setf (text MessageBoxSearcherMenuItem) "Find...")
(setf (menu MessageBoxSearcherMenuItem) PBMenuEdit)
(setf (commandkey MessageBoxSearcherMenuItem) #\F)

(setf (enabled MessageBoxSearcherMenuItem) t)

(define-handler menuSelect (MessageBoxSearcherMenuItem)
  (bringup searcher))


;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuEdit)

;;__________________________________________________________________
;; Abort Event

(new menuItem :objectName "MessageBoxAbortEvent" :project ui)
(setf (text MessageBoxAbortEvent) "Abort")
(setf (menu MessageBoxAbortEvent) PBMenuEdit)
(setf (commandkey MessageBoxAbortEvent) #\.)

(define-handler menuSelect (MessageBoxAbortEvent)
  (abortEvent))

;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;; Tools menu

(new uiMenuActor :objectName "PBMenuTools" :project ui)
(setf (text PBMenuTools) "Tools")


;;__________________________________________________________________
;; Object Editor
;; Brings up Info Window

(new menuItem :objectName "PBMenuItemInfoWindow" :project ui)
(setf (menu PBMenuItemInfoWindow) PBMenuTools)
(setf (commandkey PBMenuItemInfoWindow) #\E)

(define-handler Update (PBMenuItemInfoWindow)
  (setf (text me) "Object Editor(s)"))

(define-handler menuSelect (PBMenuItemInfoWindow)
  (if (and (boundp 'InfoWindow) (eq (container InfoWindow) stage))
    (editobjects nil)
    (bringUp InfoWindow)))

;;__________________________________________________________________
;; ProjectEditor

(new BringUpMenuItem :objectName "PBMenuProjectEditor" :project ui)
(setf (label PBMenuProjectEditor) "Project Overviewer")
(setf (menu PBMenuProjectEditor) PBMenuTools)
;;; the setf (actor of this is in "Project Editor.lisp" due to dependencies...

;;__________________________________________________________________
;; StageOverviewer

(new BringUpMenuItem :objectName "PBMenuStageViewer" :project ui)
(setf (label PBMenuStageViewer) "Stage Monitor")
(setf (menu PBMenuStageViewer) PBMenuTools)
(setf (commandkey PBMenuStageViewer) #\G)
;;; the setf (actor of this is in "Stage Viewer" due to dependencies...

;;__________________________________________________________________
;; Inheritance Viewer

(new BringUpMenuItem :objectName "PBMenuInheritanceViewer" :project ui)
(setf (label PBMenuInheritanceViewer) "Inheritance Overviewer")
(setf (menu PBMenuInheritanceViewer) PBMenuTools)
(setf (commandkey PBMenuInheritanceViewer) #\I)
;;; the setf (actor of this is in "Inheritance Viewer" due to dependencies...

;;__________________________________________________________________
;; Sk8 Browser

(new BringUpMenuItem :objectName "PBMenuBrowser" :project ui)
(setf (label PBMenuBrowser) "System Browser")
(setf (actor PBMenuBrowser) systemBrowser)
(setf (menu PBMenuBrowser) PBMenuTools)
(setf (commandkey PBMenuBrowser) #\B)

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuTools)

;;;__________________________________________________________________
;; Sk8 Finder

(new BringUpMenuItem :objectName "PBSearcher" :project ui)
(setf (label PBSearcher) "Searcher")
(setf (actor PBSearcher) Searcher)
(setf (menu PBSearcher) PBMenuTools)

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuTools)

;;__________________________________________________________________
;; Draw Palette

(new BringUpMenuItem :objectName "PBMenuDrawPalette" :project ui)
(setf (label PBMenuDrawPalette) "Draw Palette")
(setf (menu PBMenuDrawPalette) PBMenuTools)
(setf (actor PBMenuDrawPalette) DrawPalette)
(setf (commandkey PBMenuDrawPalette) #\D)

;;__________________________________________________________________
;; Color Palette
;; 

(new BringUpMenuItem :objectName "MBColorPalette" :project ui)
(setf (label MBColorPalette) "Color Palette")
(setf (actor MBColorPalette) ColorPalette)
(setf (menu MBColorPalette) PBMenuTools)
(setf (commandkey MBColorPalette) #\R)

;;__________________________________________________________________
;; Media Browser
;; 

(new BringUpMenuItem :objectName "MBMediaBrowser" :project ui)
(setf (label MBMediaBrowser) "Media Browser")
(setf (actor MBMediaBrowser) MediaBrowser)
(setf (menu MBMediaBrowser) PBMenuTools)


;;__________________________________________________________________
;; Menu Builder
;; 

(new BringUpMenuItem :objectName "MBMenuEditor" :project ui)
(setf (label MBMenuEditor) "Menu Editor")
(setf (actor MBMenuEditor) MenuEditor)
(setf (menu MBMenuEditor) PBMenuTools)


(new menuspacer :project ui :menu PBMenuTools)

;;__________________________________________________________________
;; MessageBox
;; 

(new BringUpMenuItem :objectName "PBMenuMessageBox" :project ui)
(setf (label PBMenuMessageBox) "Message Box")
(setf (menu PBMenuMessageBox) PBMenuTools)
(setf (commandkey PBMenuMessageBox) #\M)

;;__________________________________________________________________
;; Handler Tracer
;; 

(new BringUpMenuItem :objectName "PBMenuHandlerTracer" :project ui)
(setf (label PBMenuHandlerTracer) "Handler Tracer")
(setf (menu PBMenuHandlerTracer) PBMenuTools)



#|
;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuTools)

;;__________________________________________________________________
;;__________________________________________________________________
;;______________Now the Draw Palette Stuff_______________________________
(new bringupmenuitem :objectname "BringupDPItem" :project ui)
(define-handler menuSelect (BringupDPItem)
  (sk8dev::withLockedCursor animatedClock
    (when (typep (actor me) 'sk8::object)
      (if (eq (container (actor me)) stage)
        (setf (container (actor me)) nil)
        (progn
          (unless (actortodraw (car (contents (toolsection (actor me)))))
            (setup (actor me)))
          (bringUp (actor me)))))))


;;__________________________________________________________________

(new BringupDPItem :objectName "PBMenuItemTools" :project ui)
(setf (label PBMenuItemTools) "Standard Drawing Palette")
(setf (actor PBMenuItemTools) StandardDrawPalette)
(setf (menu PBMenuItemTools) PBMenuTools)
(setf (commandkey PBMenuItemTools) #\T)

;;__________________________________________________________________
(new BringupDPItem :objectName "PBMenuItemBCTools" :project ui)
(setf (label PBMenuItemBCTools) "Browser Component Palette")
(setf (actor PBMenuItemBCTools) BrowserComponentPalette)
(setf (menu PBMenuItemBCTools) PBMenuTools)

;;__________________________________________________________________
(new BringupDPItem :objectName "PBMenuItemDialogTools" :project ui)
(setf (label PBMenuItemDialogTools) "Dialog Box Palette")
(setf (actor PBMenuItemDialogTools) DialogBoxPalette)
(setf (menu PBMenuItemDialogTools) PBMenuTools)

;;__________________________________________________________________
(new BringupDPItem :objectName "PBMenuItemDrawToolTools" :project ui)
(setf (label PBMenuItemDrawToolTools) "Drawing Tools Draw Palette")
(setf (actor PBMenuItemDrawToolTools) DrawingToolsDrawPalette)
(setf (menu PBMenuItemDrawToolTools) PBMenuTools)

|#


;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;; Workspace menu

(new uiMenuActor :objectName "PBMenuWorkspace" :project ui)
(setf (text PBMenuWorkspace) "Workspace")

(new menuitem :objectName "MBWindowBackgroundMenuItem" :project ui :menu PBMenuWorkspace)
(define-handler Update (MBWindowBackgroundMenuItem)
  (if (gs:findbackground)
    (setf (text me) "Uncover Stage")
    (setf (text me) "Cover Stage"))
  )
(define-handler menuSelect (MBWindowBackgroundMenuItem)
  (if (gs:findbackground)   ;;;** Need a SK8 version of these 3.
    (gs:deinstall-background)
    (gs:install-background)))


;;__________________________________________________________________
;; Port Mode

(new menuItem :objectName "PBMenuPortWiringMode" :project ui)
(setf (text PBMenuPortWiringMode) "Enter Port Wiring Mode")
(setf (menu PBMenuPortWiringMode) PBMenuWorkspace)
(setf (enabled PBMenuPortWiringMode) t)

(define-handler Update (PBMenuPortWiringMode)
  (if (memq PortEditLayerMode (CurrentEventModes))
    (setf (text me) "Exit Port Wiring Mode")
    (setf (text me) "Enter Port Wiring Mode")
    ))

(define-handler menuSelect (PBMenuPortWiringMode)
  (mousedown uipelbutton)
  (mapc #'addClearedObject (editlayer uipelbutton))
  )

;;__________________________________________________________________
(new menuspacer :project ui :menu PBMenuWorkspace)

;;__________________________________________________________________


(new menuitem :objectName "MBWindowColor" :project ui :menu PBMenuWorkspace)
(setf (text MBWindowColor) "Window Preferences...")

(define-handler menuSelect (MBWindowColor)
  (WindowPrefsDialog))


;;__________________________________________________________________
;; Edit Mode

(new menuItem :objectName "PBMenuEditMode" :project ui)
(setf (text PBMenuEditMode) "Selection Preferences...")
(setf (menu PBMenuEditMode) PBMenuWorkspace)

(define-handler Update (PBMenuEditMode)
  (call-next-method))


(define-handler menuSelect (PBMenuEditMode)
  (HaloPrefsDialog))


;;__________________________________________________________________
;; Hide SK8 Tools

(new menuItem :objectName "MessageBoxHidePB" :project ui)
(setf (text MessageBoxHidePB) "Hide Project Builder")
(setf (menu MessageBoxHidePB) PBMenuWorkspace)

(define-handler menuSelect (MessageBoxHidePB)
  (sk8dev::withLockedCursor animatedClock
    (setf (busy projectbuildermenubar) t)
    (setf (items BackToPB) (remove-if-not #'(lambda (x) (eq (project x) ui)) (contents stage)))
    (mapcar #'(lambda (x) (tickeventclock) (setf (container x) nil)) (items BackToPB))
    (setf (sk8::menubar BackToPB) (eq (sk8::menubar stage) projectbuildermenubar))
    (setf (wantsidle projectbuildermenubar) nil)
    (if (sk8::menubar BackToPB)
      (setf (sk8::menubar stage) nil))
    (sk8-multival-bind (ll tt rr bb) (boundsrect (mainmonitor system))
      (setBoundsrect BackToPB (- rr 170) (- bb 30) rr bb))
    (setf (container BacktoPB) stage)
    ))

(new rectangle :objectname "BackToPB" :project ui
     :properties '(items sk8::menubar))

(setf (text BackToPB) "Show Project Builder")
(setf (fillcolor BackToPB) UIMiddle)
(setf (framecolor BackToPB) uirectangleoutbevel)
(setf (framesize BackToPB) '(3 3))
(setf (size BackToPB) '(170 30) )
(setf (textsize BackToPB) 12)
(setf (floating BackToPB) t)

(define-handler click (BackToPB)
  (sk8dev::withLockedCursor animatedClock
    (setf (container BacktoPB) nil)
    (mapcar #'(lambda (x) (tickeventclock) (setf (container x) stage)) (items BackToPB))
    (setf (wantsidle projectbuildermenubar) t)
    (if (sk8::menubar BackToPB)
      (setf (sk8::menubar stage) projectbuildermenubar))
    (setf (items BackToPB) nil)
    (setf (sk8::menubar BackToPB) nil)
    (setf (busy projectbuildermenubar) nil)
    ))


;;__________________________________________________________________
;; Menubar Mover

(new menuItem :objectName "PBMenuMenubarMover" :project ui)
(setf (menu PBMenuMenubarMover) PBMenuWorkspace)

(define-handler Update (PBMenuMenubarMover)
  (if (eq ProjectBuildermenubar (sk8::menubar stage))
    (setf (text me) "Detach this Menubar")
    (setf (text me) "Reattach this Menubar")))


(define-handler menuSelect (PBMenuMenubarMover)
  (if (eq ProjectBuildermenubar (sk8::menubar stage))
    (sk8dev::Eval-In-New-Thread "Removing Project Menu Bar" '(setf (sk8::menubar stage) nil))
    (sk8dev::Eval-In-New-Thread "Adding Project Menu Bar" '(setf (sk8::menubar stage) projectbuildermenubar))
    ))

;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;; WINDOWS menu

(new uiMenuActor :objectName "PBMenuWindows" :project ui)
(addparent PBMenuWindows PickerMenu)
(setf (text PBMenuWindows) "Windows")

(define-handler menuSelect (PBMenuWindows)
  (let ((wr (selectedItem me)))
    (if (stringp wr)
      (messageToUser "There are too many windows on the stage to list here.  Only Project Builder windows are being shown."
                     :beep t)
      (progn
        (unless (container wr) (setf (container wr) stage))
        (BringToFront wr)
        ;;;(unless (visible wr) (undoableset 'visible wr t))
        )
      ))
  )

(define-handler createTextDisplayItem (PBMenuWindows targetItem)
  (let ((i targetitem))
    (cond
     ((inheritsFrom i scriptEditorWindow) (windowTitle i))
     ((eq i selectionhalo) "Selection Halo")
     ((eq (project i) ui) (text i))
     ((stringp i) i)
     (t (objectstring i :project (project i))))
    ))


(define-handler items (PBMenuWindows)
  (let ((cc (contents stage)))
    (setf cc (delete-if-not #'visible cc))
    (when (> (length cc) 30)
      (setf cc (delete-if-not #'(lambda (x) (eq (project x) ui)) cc))
      (setf cc (append cc (list "Other Windows"))))
    cc
    ))


;;__________________________________________________________________
;;__________________________________________________________________
;; THE PROJECT LIST
;; This viewer has a list of all the projects in it

(new uimenuactor :objectName "PBMenuProjects" :project UI
     :properties '(item))

(setf (text PBMenuProjects) "Projects")

(new menuItem :objectName "cppumenuItem" :project ui
     :properties '(item))

(define-handler menuSelect (cppumenuitem)
  (when (eq (menu me) PBMenuProjects) 
    (setf (targetProject ui) (item me))
    )
  (setf (item (menu me)) (item me))
  (call-next-method)
  (dolist (i (menuitems (menu me))) (setf (item i) nil)))

(defun makeProjectTree ()
  (let* ((projs (knownchildren project))
         (pairs (reverse (mapcar #'(lambda (x) (list x (project x))) projs))))
    (doMakeProjectTree (cdr pairs) sk8::sk8)))

(defun domakeProjectTree (pairs curroot)
  (let ((kids (mapcar #'(lambda (x) (if (eq (second x) curroot) (first x) nil))
                      pairs)))
    (setf kids (remove-duplicates kids))
    (setf kids (remove nil kids))
    (if kids
      (cons curroot (mapcar #'(lambda (x) (doMakeProjectTree pairs x))
                            kids))
      (list curroot))))

(defun domakemenu (me projtree level)
  (let (newitem (leader "") mytext
                (proj (first projtree)) )
    (setf newitem (new cppuMenuItem :project UI))
    (dotimes (i level) (setf leader (concatenate 'string "--" leader)))
    (setf mytext (concatenate 'string leader (objectString proj :project proj)))
    (setf (text newitem) mytext)
    (if (eq proj sk8::sk8) (setf (text newItem) "SK8        ")) 
    (if (eq proj ui) (setf (enabled newItem) nil)) 
    (if (and (eq me PBMenuProjects)
             (eq proj (targetProject ui)))
      (setf (checkmark newitem) t))
    (setf (menu newitem) me)
    (setf (item newitem) proj)
    (dolist (i (cdr projtree)) (domakemenu me i (1+ level)))))

(define-handler Update (PBMenuProjects)
  (call-next-method)
  (let ((projtree (makeProjectTree)))
    (dolist (i (menuItems me)) (if (inheritsFrom i cppumenuitem)
                                 (setf (menu i) nil)
                                 ;; (dispose i)
                                 ))
    (domakemenu me projtree 0)
    ))

(new uimenubaractor :objectName "ProjectBuilderMenubar" :project ui)
(setf (menus ProjectBuilderMenubar) (list PBMenuProject PBMenuEdit 
                                          PBMenuTools PBMenuWorkspace
                                          PBMenuWindows PBMenuProjects))
(addproperty ProjectBuilderMenubar 'busy)
;;__________________________________________________________________
;;__________________________________________________________________

;;;(setf (objectname PBMHolder) nil)
;;;(setf (objectname UIPELButton) nil)

(new uiSimpleWindow :objectname "PBMHolder" :project ui)
(setf (dofirstclick PBMHolder) t)
(setSize PBMHolder 355 37)
(setf (framesize projectbuildermenubar) '(0 0))
(setf (sk8::resizer PBMHolder) nil)
(setf (sk8::menubar PBMHolder) t)
(setf (zoombox PBMHolder) nil)
(setf (text pbmholder) "Project Builder Menubar")
;;(uicolorize pbmholder)


(define-handler sk8::close (pbmholder)
  (declare (ignore me))
  (unless ccl::*quitting*
    (when (yesornodialog "Do you wish to reattach this menubar to the stage?" :cancel nil)
      (menuselect PBMenuMenubarMover))))

(setf (container pbmholder) nil)
(setSize PBMHolder 315 38)

(new PortEditLayerButton :objectname "UIPELButton" :project ui)
(setf (container UIPELButton) PBMHolder)

(define-handler editedProject (UIPELButton)
  (targetProject ui))
(setBoundsrect UIPELButton 200 200 210 210)

(define-handler doubleclick (PBMHolder)
  )

(define-handler makeBoundsRegion (PBMHolder)
  (call-next-method)
  (unless *MacStyleInterface*
    (gs:let+ ((physRect (gs:recompute-physicalBoundsRect me))
              (ll (gs:rect-left physRect))
              (rr (gs:rect-right physRect))
              (bb (gs:rect-bottom physRect))
              (r (:rect))
              (temp (:region))
              (bounds (gs:node-boundsRegion me))
              (flags (gs:node-flags me)))
      ;; Rect 1
      (set-qd-rect r ll (- bb 4) rr bb)
      (#_rectRgn temp r)
      (#_diffrgn bounds temp bounds)
      (when (memq projectbuildermenubar (contents me))
        (dolist (i (menus projectbuildermenubar))
          (gs:let+ ((tempphys (gs:recompute-physicalBoundsRect i))
                    (ll1 (gs:rect-left tempphys))
                    (rr1 (gs:rect-right tempphys))
                    (bb1 (gs:rect-bottom tempphys)))
            (#_UnionRgn bounds (gs:node-boundsRegion i) bounds)
            (set-qd-rect r ll1 (- bb1 3) (+ ll1 2) bb1)
            (#_rectRgn temp r)
            (#_diffrgn bounds temp bounds)
            (set-qd-rect r ll1 (- bb1 2) (+ ll1 4) bb1)
            (#_rectRgn temp r)
            (#_diffrgn bounds temp bounds)
            (set-qd-rect r (- rr1 2) (- bb1 3) rr1 bb1)
            (#_rectRgn temp r)
            (#_diffrgn bounds temp bounds)
            (set-qd-rect r (- rr1 4) (- bb1 2) rr1 bb1)
            (#_rectRgn temp r)
            (#_diffrgn bounds temp bounds))))
      (set-qd-rect r ll (- bb 1) rr (+ bb 2))
      (#_rectRgn temp r)
      (#_diffrgn bounds temp bounds)
      (set-qd-rect r ll (- bb 5) (+ ll 1) bb)
      (#_rectRgn temp r)
      (#_diffrgn bounds temp bounds)
      (set-qd-rect r (- rr 1) (- bb 5) rr bb)
      (#_rectRgn temp r)
      (#_diffrgn bounds temp bounds)
      
      (gs:boundsDirty! flags 0))))

(setf (wantsidle ProjectBuilderMenubar) t)
(define-handler idle (ProjectBuilderMenubar)
  (doaswatch)
  ;;;;;DEAL WITH CONTROL KEY FOR SELECTION   !!!!!!!!
  (when (and (eq (currentTool DrawPalette) nil)
             (controlkeydown)
             (not (busy selectionhalo))
             (or (not (activemode)) (not (is-a (activemode) modaldialogmode)))
             ;;(> (- (get-internal-real-time) (exitedLastTime pickActorMode)) 1000)
             )
    (setf (enteredWithControlKey PickActorMode) t)
    (mousedown SelectTool)
    ))

;;; We use the textColor to control the color of the text. For the fillcolor, however we need to
;;; use both the fillcolor and menufillcolor since when the menus are in their actor form, the
;;; fillcolor and the menufillcolor are not the same (a pattern and a graytone).

(define-handler (setf sk8::onstage) (theval ProjectBuilderMenubar)
  (sk8::setValue 'sk8dev::onstage me theval)
  (unless (or gs:*no-ui-windows*  ;; kludge for when we buildstandalone without ui
              (busy me))
    (if theval
      (progn
        (setf (container PBMHolder) nil)
        (setf (fillcolor ProjectBuilderMenubar) white)
        (dolist (i (menus ProjectBuilderMenubar))
          (setf (textcolor i) black)
          (setf (menuFillcolor i) white)
          (setf (fillcolor i) black)))
      (progn
        (setf (fillcolor ProjectBuilderMenubar) transparent)
        (dolist (i (menus ProjectBuilderMenubar))
          (setf (textcolor i) black)
          (setf (fillcolor i) uimenucolor)
          (setf (menuFillcolor i) graytone90))
        (setf (container me) PBMHolder)
        (resized PBMHolder)
        (moveoffstage PBMHolder)
        (setf (container PBMHolder) stage)
        (setBoundsrect PBMHolder 0 0 1 0 :relative t)
        (play zoomupsound)
        (dotimes (i 10)
          (SendUpdateEvent PBMHolder)
          (setLocation PBMHolder (+ (* i i) 190) (- (* i 6) 10)))
        (SendUpdateEvent PBMHolder)
        (setlocation PBMHolder 2 -4 :relative t)
        (setlocation PBMHolder -2 4 :relative t)
        (setlocation PBMHolder -1 1 :relative t)
        (setlocation PBMHolder 1 -1 :relative t)
        (setlocation PBMHolder 0 1 :relative t)
        (setlocation PBMHolder 0 -1 :relative t)
        ))))

(define-handler resized (PBMHolder)
  (when (eq (container projectbuildermenubar) me)
    (call-next-method)
    (setf (fillcolor projectbuildermenubar) (if *macstyleinterface* white transparent))
    (setboundsrect projectbuildermenubar *BarLeft* *WindowTop* (width me) (+ *WindowTop* 20))
    (moveoffstage UIPELButton)))


#|
(dev-mode :force? t)
(sk8dev::onstage projectbuildermenubar)
(setf (sk8::menubar stage) nil)
(setf (sk8::menubar stage) simplemenubar)
(setf (sk8::menubar stage) projectbuildermenubar)

|#

#|
Change History (most recent last):
1	2/22/94	rod	
10	2/25/94	hernan	Got rid of objectnames on the menuspacers
16	2/28/94	hernan	Avoiding calling dispose directly.
17	2/28/94	hernan	Avoiding calling dispose directly.
19	3/2/94	Hernan	Porting to Fred 3.0
22	3/5/94	rod	Changing draw palette.  Adding library stuff.
	27	3/9/94	rod	Doing Project Switching and Reference Clearing.
	29	3/10/94	Brian	DPBigRect symbol wasn't declared
	34	3/15/94	rod	Fixed undo to be more general.
	39	3/17/94	Hernan	Fixing typo.
	42	3/21/94	Hernan	Fixing (setf onstage) to correctly set the menu's
				fillcolor and menufillcolor and textcolor.
				Also fixed menuSelect of the cut/paste items to 
				work when the selected window is a lisp window
				(this was required by the loadScript users).
	52	3/31/94	sidney	save and close projects properly when quitting
	53	3/31/94	sidney	whoops
	54	3/31/94	sidney	more fixing of save/close project behavior when quitting sk8
	65	5/17/94	sidney	close ui & sk8 before quitting to delete the swap files
	66	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	67	5/22/94	rod	OK Now I'm tuning the mac version.
	68	5/22/94	rod	OK Now I'm tuning the mac version.
	69	5/26/94	rod	
	70	5/26/94	rod	
	71	6/1/94	rod	Making window preferences dialog
	72	6/3/94	rod	
	73	6/6/94	kleiman	Problem 1165206: *override-quit-questions* added
				to maybeSaveProject to prevent it from asking questions.
	74	6/9/94	sidney	saveproject menu should call sk8::saveproject handler, not lower level
				ps::saveproject function, so all before and after methods can get run
				1167046: Set file of ui and sk8 projects to nil here, part of 1167046 changes
	75	6/12/94	rod	Adding a "Save As..." menuitem.
	76	6/12/94	rod	Making project switching behave.
	77	6/17/94	rod	Fixing objectstring
	78	6/17/94	rod	Fixing objectstring
	79	6/23/94	rod	Getting rid of tablePickerScroller.
	80	6/25/94	sidney	refer to menubar and menubox in their correct package
	81	6/28/94	rod	Making the tool menus not switch container, but
				rather always bring to front.
	82	7/11/94	rod	
	83	7/13/94	rod	
	84	7/13/94	rod	1174095
	85	7/18/94	sidney	1171984, 1173251: delete any old leftover temp files before quitting
	86	7/18/94	rod	Messing with tool menu  1173754
	87	7/18/94	rod	Adding menuitem stuff here.
	88	7/20/94	rod	
	89	7/21/94	rod	Fixing undo to keep selection.
	90	7/21/94	rod	Fixing command key
	91	7/29/94	rod	
	92	8/1/94	rod	1177762:  Load Script File only shows "TEXT" files.
	93	8/2/94	rod	Making sure that you cannot enter selection mode
				when in modaldialogmode.
	94	8/5/94	rod	
	95	8/5/94	rod	
	96 	 8/25/94	rod     	
	97 	 8/25/94	rod     	
	98 	 8/30/94	rod     	
	99 	 8/30/94	rod     	Making resized call-next-method
	100	 8/31/94	Hernan  	*eventWindow* -> *currentTla*.
	101	 9/ 9/94	sidney  	use watch cursor when saving all projects before quitting
	102	 9/12/94	rod     	
	103	 9/12/94	rod     	
	104	 9/22/94	sidney  	treat dev-mode as a special case in dealing with menubar when closing a project
	105	10/ 4/94	rod     	Lazy Swatch stuff...
	106	10/19/94	rod     	Removing libraries.
	107	10/27/94	rod     	
	108	11/ 4/94	kend    	Updated menu labels for "Save As Text..." and "Save Library As Text...
	109	11/ 4/94	rod     	Fixing menu items.  Commenting out save objects
							as text.
	110	11/ 5/94	rod     	Now it asks the user to save the project before
							it saves as text.
	111	11/ 8/94	rod     	
	112	11/13/94	sidney  	no longer have to save project before saving as text. remove dialog forcing that
	113	12/ 5/94	rod     	saving clears...
	114	12/22/94	rod     	Making close say the project name.
	115	12/22/94	rod     	Fixing loadscriptmenu item to deal with new 
							macdialogs returning false at cancel.
	116	 1/12/95	sidney  	don't create temporary file objects that get saved with a project as part of the saving process
	117	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	118	 2/10/95	sidney  	saveastext had bogus dependency on project file
	119	 2/14/95	rod     	moving setting clears to the prefereces...
	120	 2/21/95	sidney  	disable mouse clicks, etc. during close project
	121	 2/28/95	rod     	
	122	 3/ 1/95	sidney  	windows were grayed out during save as text
	123	 3/ 2/95	rod     	
	124	 3/ 2/95	rod     	
	125	 3/ 8/95	sidney  	keep windows from being grayed out during save project
	126	 3/13/95	sidney  	enqueue all of the long save/load operations that were getting hung up in the menu selection
	127	 3/16/95	rod     	sk8dev::Eval-In-New-Thread doesn't work for save with close.
	128	 3/22/95	rod     	Warning messages for save and save as text.
	129	 3/27/95	sidney  	kludge avoiding the fancy projectbuildermenubar whoosh during buildstandalone
	130	 3/28/95	sidney  	save, save as, save as text, must all close subprojects before saving
	131	 3/28/95	rod     	
	132	 3/29/95	sidney  	don't ask to save project when quitting standalone app. better bulletproofing, too.
	133	 4/13/95	rod     	Fixing cancel of save copy as...
	134	 4/17/95	sidney  	ensure cancelling a save of a project with subprojects leaves the project as the target
	2  	 6/23/95	Hernan  	These menus have to use eventWindow (instead of eventActor).
	3  	 7/10/95	Brian Roddy	changing eval-enqueues to eval-in-new-thread
	4  	 8/ 9/95	Brian   	currentWindow -> eventWindow
	6  	 9/21/95	Till    	Add Liposuction mode.
	8  	12/11/95	Brian   	fixing call to ss package
	9  	12/11/95	Brian   	Removing all references to the stackwatcher, as it is
						going away.  It's too slow and cumbersome.  It's not
						general.  No one uses it.  The new debugging API doesn't
						support it.  Good riddance.
	10 	 1/31/96	Brian   	Making quitting faster by making safecloseallprojects
						not close the last user project.
	11 	 1/31/96	Brian   	
	12 	 2/ 6/96	Hernan  	helpWindow no longer with us.
	13 	 2/14/96	Brian   	fixing population stuff
	14 	 2/14/96	Brian   	fixing calls to external packages.
	2  	 4/16/96	brian   	removing (nthitem call.
	3  	 4/25/96	Hernan  	Removed all references to liposuction. Also added
						a menu item to save a project for standalone build.
	4  	 4/25/96	Hernan  	Declaring some symbol...
	8  	 7/25/96	sidney  	suppress do you wish to reattach message when quitting sk8
	10 	 9/ 3/96	Hernan  	Fixed keyword arg in call to saveProject.
	11 	10/10/96	Hernan  	Replacing compactProject with compactProjectCode, a
						function that removes every old version of every handler
						and function saved with the project.
	12 	10/21/96	sidney  	delete files from temporary folder only on ordinary quit, not in buildstandalone
	13 	10/22/96	Brian   	removing sk8 to java menu item.
	14 	11/12/96	sidney  	added a name argument to eval-in-new-thread
	15 	 2/27/97	sidney  	code for developer mode item in ProjectMenubar moved to file loaded only in developer build
	16 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
