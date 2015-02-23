;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  3-19-96   6:12 pm
                  SK8::GETHOLDEROFTYPE SK8::IMPORTABLEOBJECTS)


(provide "RESOURCECHOOSER")

(require "FILECHOOSER" "objects;Browsers:FileChooser")
(require "MESSAGETOUSER" "objects;Dialogs:MessageToUserDialogBox")
(require "PIXELMAPTOPICTTRANSLATOR" "objects;Import/Export:PixelMapToPICTTranslator")

;;; _________________________________________________________________________
;;;                            (3) The ResourceChooser
;;; _________________________________________________________________________

;;; This is an extension of the file dialog. When we get to a file that has media,  we go into it.
;;; The next thing we see is a list of resourcetypes available for import into SK8. If we doubleclick
;;; on one of them we go into a list of resources of the type selected. The menu shows our whole
;;; path up the to resourceTypes level.

;;; The responsabilities of the resourceChooser are:
;;; (1) To always have the right thing in the outputhandle.  The "right thing" means: an object of the right type
;;;    which is filled in to show what it is being imported from (eg. something in the resource fork will not have a resource id).
;;; (2) To always point at the right file. That is, the outputFile property is always set to the right file being looked at. The
;;;    file used for this is always in the chooser's project.

(new fileChooser 
     :objectname "ResourceChooser" 
     :project sk8
     :properties '((DisplayType :value 'file) ;; file, restype or resources
                   (outputhandle :value nil)  ;; Slot for output. When a resource is selected it is set.
                   (viewBy :value 'name)      ;; :resid or :resname
                   (handleHolders :value nil) ;; Temp objects to hold resources.
                   (dummyFile :value nil)     ;; A temp file needed to check the macfiletype of pathnames.
                   (restype :value nil)))     ;; ResourceType currently being viewed.

(setf (private ResourceChooser) t)
(setf (private ResourceChooser :property 'displayType) t)
(setf (private ResourceChooser :property 'viewBy) t)
(setf (private ResourceChooser :property 'handleHolders) t)
(setf (private ResourceChooser :property 'DummyFile) t)
(setf (private ResourceChooser :property 'restype) t)

;;; Makes sure there is a file in the dummy slot. This file will be used for the outputFile property.

(defun getDummyFile (theChooser)
  (or (dummyFile theChooser)
      (setf (dummyFile theChooser) (new file :project (project theChooser)))))

;;; Installs the selected pathname in the file provided.

(defun updateFileFromChooser (theFile theChooser)
  (setf (osPathname theFile) (car (selectedItems (fileList theChooser)))))

;;; Returns the current resource type being looked at. In the case of file viewing this is the
;;; macFileType of the file currently selected.

(define-handler restype (resourceChooser)
  (case (displayType me)
    (resources (slot-value me 'restype))
    (restype (car (selectedItems (fileList me))))
    (file 
     (let ((theFile (getDummyFile me)))
       (updateFileFromChooser theFile me)
       (macFileType theFile)))))

(define-handler (setf outputFile) (newFile resourceChooser)
  (when (fileExists newFile)
    (let ((hasResources (macHasResources newFile)))
      (if hasResources
        (let ((typesAvailable (macSK8importableresourcetypesavailable newfile)))
          (if typesAvailable
            ;; :file -> :restype.
            (progn
              ;; Get on with it! Set the slot and whether it was open already.
              (setf (slot-value me 'outputfile) newFile)
              ;; Update the resourceChooser.
              (switch-to-restype me typesAvailable)
              (makeMenu (menu me))
              newFile)
            (messageToUser (format nil "No Media Found in ~a" (physicalName newFile :directory nil)))))
        (messageToUser (format nil "No Media Found in ~a" (physicalName newFile :directory nil)))))))

(define-handler (setf currentDirectory) (theFile resourceChooser)
  (declare (ignore-if-unused theFile))
  (switch-to-file me)
  (call-next-method))

;;; _____________________
;;; Functions to switch display type.
;;; _____________________

(defun switch-to-restype (theChooser &optional typesAvailable)
  ;; Compute the items if nothing was supplied.
  (unless typesAvailable
    (setf typesAvailable (macSK8importableresourcetypesavailable (outputFile theChooser))))
  ;; Set items and state.
  (setf (displayType theChooser) 'restype)
  (setf (restype theChooser) nil)
  (setf (selectionStyle (picker (fileList theChooser))) 'single)
  (setf (items (fileList theChooser)) typesAvailable)
  (unless (enabled (openButton theChooser))
    (setf (enabled (openButton theChooser)) t))
  (selectionCompleted (picker (fileList theChooser))))

(defun switch-to-resources (theChooser restype)
  (setf (restype theChooser) restype)
  (setf (displayType theChooser) 'resources)
  (setf (selectionStyle (picker (fileList theChooser))) 'discontiguous)
  (setf (enabled (openButton theChooser)) nil)
  ;; Compute the items if nothing was supplied.
  (sk8-multival-bind (names ids) (macGetResourceHandleInfo (outputFile theChooser) restype)
    (setf (items (fileList theChooser))
          (mapcar #'(lambda (theName theId) (list theName theId)) names ids)))
  (selectionCompleted (picker (fileList theChooser))))

(defun switch-to-file (thechooser)
  (setf (selectionStyle (picker (fileList theChooser))) 'single)
  (unless (enabled (openButton theChooser))
    (setf (enabled (openButton theChooser)) t))
  (setf (displayType theChooser) 'file))
          
;;; _____________________
;;; Pop Up MenuStuff
;;; _____________________

(new fileMenuItem :objectname "ResFileMenuItem" :project sk8)

(setf (private ResFileMenuItem) nil) ; in public api

(define-handler makemenu ((menu resourceChooser))
  (withActorLocked (me)
    ;; Disposes the rest and makes the directory stuff.
    (let* ((theChooser (container me))
           (curDir (currentDirectory theChooser))
           (newFiles (path-to-root curDir))
           (theProj (project me))
           (displayType (displayType theChooser))
           (theFile (outputFile theChooser))
           (aFileName (when theFile (physicalName theFile :directory nil)))
           theItem)
      ;; [1] Dispose current menuitems.
      (dolist (c (menuitems me))
        (setf (menu c) nil)
        ;; (dispose c)
        )
      ;; [2] Make the new items.
      (dolist (c newFiles)
        (setf theItem (new ResFileMenuItem :project theProj :theFile c
                           :text (physicalname c :directory nil)))
        (setf (menu theItem) me))
      ;; Any extra stuff to do?
      (when (and (neq displayType 'file) theFile)
        ;; [3] Yes! Add the file.
        (new menuSpacer :project theProj :menu me :layer 1)
        (new ResFileMenuItem :project theProj :theFile :restypes
             :text aFileName :menu me :layer 1)
        ;; Anything else?
        (when (eq displayType 'resources)
          ;; [4] Yes! Add resource type menu!
          (new menuSpacer :project theProj :menu me :layer 1)
          (new ResFileMenuItem :project theProj :theFile (restype theChooser)
               :text (format nil "~as in ~a" (restype theChooser) aFileName)
               :menu me :layer 1)))
      ;; [5] And set the menu's text!
      (setf (text me) (text (car (menuitems me)))))))
  
(define-handler menuSelect (ResFileMenuItem)
  (let ((theChooser (container (menu me))))
    (cond ((inheritsFrom (thefile me) file) (call-next-method))
          ;; If thefile = :restypes, set items to available restypes in the file.
          ((eq (theFile me) :restypes)
           (unless (eq (displaytype theChooser) 'restype)
             (switch-to-restype (container (menu me))))
           (makeMenu (menu theChooser)))
          ;; If the file is a string we know we are dealing with a resource type.
          ;; Refresh the resource list.
          ((stringp (theFile me))
           (switch-to-resources theChooser (theFile me))
           (makeMenu (menu theChooser))))))

;;; _____________________
;;; Picker Methods.
;;; _____________________

(define-handler commandKeyEvent ((picker (filelist resourceChooser)) theKey)
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displaytype 'file) (call-next-method))
          ((eq displayType 'restype) 
           (case theKey
             (#\UpArrow (menuSelect (third (menuItems (menu theChooser)))))
             (#\DownArrow (doubleClick me))
             (otherwise (call-next-method))))
          ((eq displayType 'resources)
           (case theKey
             (#\UpArrow (menuSelect (third (menuItems (menu theChooser)))))
             (#\downArrow (ed-beep))
             ((#\a #\A) 
              ;; Select all!
              (setf (selection me) (list 0 (1- (length (items me))))))
             (otherwise (call-next-method)))))))

(define-handler doubleclick ((picker (filelist resourceChooser)))
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displayType 'file) (call-next-method))
          ((eq displayType 'restype) 
           ;; Highlight the button.
           (setf (Highlight (openbutton theChooser)) t)
           (setf (Highlight (openButton theChooser)) nil)
           ;; Do the work.
           (switch-to-resources theChooser (car (selectedItems me)))
           (makeMenu (menu theChooser)))
          ;; Nothing to do in next case since SelectionCompleted does the work.
          (t nil))))

(define-handler createIcon ((picker (filelist resourceChooser)) theItem thestring theposition)
  (declare (ignore thePosition theString))
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displayType 'file) (call-next-method))
          ((eq displayType 'restype)
           (cond ((string-equal theItem "cicn") sk8::cicnRenderer)
                 ((string-equal theItem "curs") sk8::cursRenderer)
                 ((string-equal theItem "ppat") sk8::ppatRenderer)
                 ((string-equal theItem "pict") sk8::pictRenderer)
                 ((string-equal theItem "snd ") sk8::sndRenderer)
                 ((string-equal theItem "pat ") sk8::patRenderer)
                 ((string-equal theItem "crsr") sk8::cursRenderer)
                 (t nil)))
          (t nil))))

(define-handler createTextDisplayItem ((picker (filelist resourceChooser)) theItem)
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displayType 'file) (call-next-method))
          ((eq displayType 'restype) 
           (concatenate 'string theItem " [" 
                        (format nil "~a" (maccountResourcesOfType (outputFile theChooser) theItem)) "]"))
          (t (let ((theString (car theItem)))
               (if (stringp theString)
                 theString
                 ;; Believe it or not, this is faster that princ-to-string and its friends!
                 (format nil "~a" theString)))))))

(define-handler keyDown ((picker (filelist resourceChooser)) theKey)
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displaytype 'file) (call-next-method))
          ((eq displayType 'restype)
           (if (or (char= theKey #\Newline) (char= theKey #\Enter))
             (doubleClick me)
             (call-next-method)))
          ((eq displayType 'resources)
           (if (or (char= theKey #\Newline) (char= theKey #\Enter))
             (doubleClick me)
             (call-next-method))))))

;;; When we switch to resources, the handle holders gets a resource object of the right type.
;;; Then, this method just sets the properties of that resource object and passes it to the sampler
;;; which can choose to sample or not.

;;; Use handle holders to put new resources as we make them. Empty to start with.

(define-sk8-function getHolderOfType nil (theChooser &key theTranslator theType)
  ;; Get the right translator if not provided.
  (unless theTranslator
    (setf theTranslator (car (importTypes theType :justTranslators t))))
  ;; First check if there is an appropriate resource in the holders.
  (let ((theHolders (handleHolders theChooser))
        (targetObj (internalObject theTranslator))
        theResource)
    ;; KLUDGE: since pixelMap and QDPicture have the same resType, and we want
    ;; to use QDPicture in all these cases...
    (when (eq targetObj PixelMap) (setf targetObj QDPicture))
    (dolist (c theHolders)
      (when (inheritsFrom c targetObj)
        (return-from getHolderOfType c)))
    ;; No! Make a new holder and add it to the holders.
    (setf theResource (new targetObj :project (project thechooser)))
    (push theResource (handleHolders theChooser))
    theResource))
               
(define-handler selectionCompleted ((picker (fileList resourceChooser)))
  (let* ((theChooser (container (container me)))
         (displayType (displayType theChooser)))
    (cond ((eq displaytype 'file)
           (if (directory-pathname-p (car (selecteditems me)))
             (call-next-method)
             ;; We are looking at a file. If there is anything to import, get it
             ;; into the output handle.
             (let ((importableTypes (importableObjects theChooser))
                   (theHolder (outputhandle theChooser))
                   (theFile (getDummyFile theChooser)) ;; Filled in by importableObjects!
                   theTranslator)
               ;; Makes sure we are always pointing at the right thing.
               (setf (slot-value theChooser 'outputfile) theFile)
               (when importableTypes
                 (setf theTranslator (findImportTranslator theFile nil))
                 (when theTranslator
                   ;; Make sure the holder is of the right type.
                   (unless (and theHolder 
                                (string= (restype theHolder) (externalType theTranslator))
                                (inheritsFrom theHolder (internalObject theTranslator))
                                )
                     (setf theHolder (getHolderOfType theChooser :theTranslator theTranslator)))
                   (setf (file theHolder) theFile)
                   ;; Clear the handle to force the system to go get the new one.
                   (setf (resourceId theHolder) nil)
                   (unloadMedia theHolder)
                   (sk8::import theTranslator :source theFile :destination theHolder)
                   (setf (outputHandle theChooser) theHolder))))))
          ((eq displayType 'restype) 
           ;; This makes sure the right holder is placed in the output handle.
           (setf (outputhandle theChooser) (getHolderOfType theChooser :theType (restype theChooser)))
           (setf (file (outputHandle theChooser)) (outputFile thechooser))
           (call-next-method))
          (t 
           ;; Get the handle.
           (let* ((theSelection (car (selectedItems me)))
                  (theFile (outputFile theChooser))
                  (theHolder (outputHandle theChooser))
                  theType theTranslator)
             (when theSelection (setf theType (restype theChooser)))
             (when theType
               (setf theTranslator (car (importTypes theType :justTranslators t)))
               ;; KLUDGE! We do not want to import dummys a pixelMaps.
               (when (eq theTranslator sk8::PixelMapToPICTTranslator)
                 (setf theTranslator QDPictureToPICTTranslator))
               (when theTranslator
                 ;; Make sure the holder is of the right type.
                 (unless (and theHolder (inheritsFrom theHolder (internalObject theTranslator)))
                   ;; No? Make a holder of the right type and install it there.
                   (setf theHolder (getHolderOfType theChooser :theTranslator theTranslator)))
                 ;; Set the properties of the holder and set the output handle.
                 (setf (file theHolder) theFile)
                 ;; Clear the handle to force the system to go get the new one.
                 (setf (resourceId theHolder) (cadr theSelection))
                 (unloadMedia theHolder)
                 (sk8::import theTranslator :source theHolder :destination theHolder)
                 (setf (outputHandle theChooser) theHolder))))))))

;;; __________________
;;; Given the current state of the chooser, return all the things we could make from it.
;;; __________________

;;; This handler should be used by everyone to check the state of the chooser. Right after
;;; this is called, the dummy file has the current file installed in it.

(define-handler importableObjects (resourceChooser &key justTranslators)
  (let (source)
    ;; Get the thing to be imported.
    (case (displayType me)
      (file
       (let ((thePn (car (selectedItems (fileList me))))
             (theFile (getDummyFile me)))
         (when (and thePn (probe-file thePn))
           (setf (osPathname thefile) thePn)
           (setf source theFile))))
      (restype
       (let* ((theType (car (selecteditems (fileList me))))
              (theHolder (getHolderOfType me :theType theType)))
         (setf source theHolder)))
      (resources
       (setf source (outputhandle me))))
    ;; Ask translators if they can import it and return the types to be produced.
    (when source
      (if justTranslators 
        (importTranslatorsApplicable source)
        (mapcar #'internalObject (importTranslatorsApplicable source))))))

#|
	Change History (most recent last):
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	5  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
						4   7/12/96Brian
|# ;(do not edit past this line!!)
