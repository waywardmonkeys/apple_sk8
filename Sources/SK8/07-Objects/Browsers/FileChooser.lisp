;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  7-12-96   1:23 pm
                  SK8::DOCUMENTRENDERER SK8::FOLDERRENDERER)


(provide "FILECHOOSER")

(require "RECTANGLE")
(require "TEXTLIST" "objects;Pickers:TextList")
(require "ICONTEXTPICKER" "objects;Pickers:IconTextPicker")
(require "DIALOGBOX" "objects;Dialogs:DialogBox")

;;; _______________________________ 
;;; FileChooser
;;; _______________________________ 

(new rectangle :objectname "FileChooser" :project sk8
     :properties '((ActionText :inherit :value "Open")
                   (outputfile :value nil)
                   (MacFileType :value nil)
                   (currentDirectory :value nil)))

(setf (private FileChooser) nil) ; in public api

(addproperty fileChooser 'DisableFiles)
(setSize fileChooser 200 250)
(setFramesize fileChooser 0 0)

(define-handler (setf currentDirectory) (theDir fileChooser)
  (setf (slot-value me 'currentDirectory) theDir)
  (let ((pn (probe-file (OSPathname thedir))))
    (when pn
      (if (directory-pathname-p pn)
        (withActorLocked (me)
          (makeMenu (Menu me))
          (setf (items (FileList me)) 
                (withLockedCursor animatedClock
                  (files-internal pn t t (macFileType me))))
          (setf (selecteditems (FileList me)) 
                (if (createtextcolors (picker (filelist me)))
                  (list (car (remove-if-not #'directory-pathname-p (items (filelist me)))))
                  (car (items (filelist me))))))
        (withActorLocked (me)
          (setf (selecteditems (FileList me)) nil)
          (setf (items (FileList me)) nil))))))

(define-handler mouseEnter (fileChooser)
  (setf (cursor stage) cursorPointing))

(define-handler mouseleave (fileChooser)
  (setf (cursor stage) standardCursor))

;;; The file list! (its picker is a multiline picker -> to show the icons).

(new textList :objectname "FileList" :project sk8)

(setf (private FileList) nil) ; in public api

(setf (pickerPrototype fileList) iconTextPicker)

(setf (linespacing (picker fileList)) 2)
(setf (alphabeticalDisplay fileList) nil)
(setf (iconSize (picker fileList)) '(16 16))

(define-handler createTextColors ((picker fileList))
  (disablefiles (container (container me))))

(define-handler createtextcolor ((picker fileList) thelocation)
  (if (directory-pathname-p (nth thelocation (items me)))
    black
    gray))

;;; Make this more specialized and gnarley sometime later.

(define-handler createIcon ((picker fileList) theItem thestring theposition)
  (declare (ignore theString thePosition))
  (if (pathname-alias-p theItem)
    (if (member (mac-file-type theItem :dont-resolve)
                ;; from IM vol VI page 9-30.   
                '(:|faam| :|fadr| :|fact| :|faet| :|faex| :|srvr| :|flpy| :|fdrp| :|hdsk|
                  :|famn| :|drop| :|fapf| :|fapn| :|fash| :|fast| :|fasy| :|trsh|))
      sk8::folderRenderer
      sk8::documentRenderer)
    (if (directoryp theItem)
      sk8::folderRenderer
      sk8::documentRenderer)))

(defun fileNameFromString (theString)
  (let* ((len (length theString))
         (lastChar (elt theString (1- len)))
         startPos startpos2 endPos)
    (if (char= lastChar #\:)
      (setf endPos (1- len))
      (setf endPos len))
    (setf startPos (position #\: theString :from-end t :end endPos))
    (setf startPos2 (position #\; theString :from-end t :end endPos))
    (setf startPos
          (cond ((and (numberp startPos) (numberp startpos2))
                 (1+ (max startPos startPos2)))
                ((numberp startPos) (1+ startPos))
                ((numberp startPos2) (1+ startPos2))
                (t 0)))
    ;; Return the darn thing!
    (subseq theString startPos endPos)))
                 
(define-handler createTextDisplayItem ((picker fileList) theItem)
  (if (and (null (pathname-name theItem))
           (null (pathname-type theItem)))
    (car (last (pathname-directory theItem)))
    (pathname-name&type theItem)))

(setf (container fileList) fileChooser)
(hide (titlebar fileList))
(setBoundsRect fileList 20 50 180 230)
(tagpart fileChooser fileList 'FileList)

;;; This one can be specialized for filtering!

(define-handler getFiles (fileList thedir)
  (let ((thefiles (FileNames thedir))
        (TheMacFileType (MacFileType (container me))))
    (when TheMacFileType
      (if (eq TheMacFileType 'directory)
        (setf thefiles (directorynames thedir))
        (let* ((ourfiles (files thedir :directories nil))
               (files (copy-list ourfiles)))
          (setf files (remove-if-not #'(lambda (x) (string= (macfiletype x) TheMacFileType)) files))
          (setf thefiles (sort (append (directorynames thedir)
                                       (mapcar #'logicalname files))
                               #'string-lessp))
          ;; (mapcar #'dispose ourfiles)
          )))
    thefiles))

(define-handler fileAction (fileChooser)
  (setf (Highlight (openbutton me)) t)
  (let ((pn (car (selectedItems (picker (filelist me))))))
    (when pn 
      (let ((theFile (new file :project (project me) :OSPathname pn)))
        (if (directoryp pn)
          (setf (currentDirectory me) theFile)
          (setf (outputFile me) theFile)))))
  (setf (Highlight (openbutton me)) nil))

(define-handler selectioncompleted ((picker fileList))
  (let ((fc (container (container me))))
    (setf (text (openbutton fc)) 
          (if (and (selecteditems me)
                   (directory-pathname-p (car (selecteditems me))))
            "Open" (actionText fc)))))

(define-handler doubleclick ((picker fileList))
  (when (selecteditems me)
    (fileAction (container (container me)))))

(define-handler (setf SelectedItems) (items (picker fileList) &key start end upwards first prev next last (deselecting t))
  (declare (ignorable upwards deselecting))
  (if (createtextcolors me)
    (progn
      (cond (first nil)
            (last nil)
            (prev (setf start (findnextfolder me (car (selection* me)) -1)))
            (next (setf start (findnextfolder me (car (selection* me)) 1))))
      (if (or start end) (setf items (nth (or start end) (items me))))
      (unless (listp items) (setf items (list items)))
      (setf items (list (car items)))
      (let ((pos (position (car items) (items me))))
        (withactorlocked (me)
          (setf (selection* me) (and pos (list (min (1- (length (items me))) pos))))
          (showSelection me)
          (unless (and pos (directory-pathname-p (car items)))
            (setf (selection* me) nil)))))
    (call-next-method)))

(define-handler FindNextFolder ((picker fileList) curpos direction)
  (let* ((newpos curpos)
         (index curpos)
         (its (items me))
         (numits (length its)))
    (loop
      (incf index direction)
      (when (or (< index 0) (>= index numits)) (return))
      (when (directory-pathname-p (nth index its))
        (setf newpos index)
        (return)))
    newpos))

(define-handler keyDown ((picker fileList) theKey)
  (if (or (eq theKey #\return) (eq theKey #\Enter))  ;;;Need to check for enter as well as return!! -BJR
    (fileAction (container (container me)))
    (call-next-method)))

(define-handler commandKeyEvent ((picker fileList) theKey)
  (case theKey
    ;; up and down arrows.
    (#\upArrow (let* ((theChooser (container (container me)))
                      (newDir (sk8::directory (currentDirectory theChooser))))
                 (setf (currentDirectory thechooser) newDir)))
    (#\downArrow (let* ((theChooser (container (container me)))
                        (curFile (car (selectedItems me))))
                   (when (directory-pathname-p curFile)
                     (setf (currentDirectory thechooser)
                           (new File :project (project me) :ospathname curFile)))))
    (otherwise (call-next-method))))

(define-handler activateText ((picker fileList))
  (call-next-method)
  (setf (currentCommandKey system) me))

(define-handler deactivateText ((picker fileList))
  (call-next-method)
  (setf (currentCommandKey system) nil))

;;; The file pop up!

(new menu :objectname "FilePopUp" :project sk8)
(setf (textlocation FilePopUp) 'center) ; in public api

(setf (private FilePopUp) nil) ; in public api

(tagpart fileChooser filePopUp 'Menu)

(setf (container filePopUp) fileChooser)
(setLocation filePopUp 100 35)

(define-handler mousedown (FilePopUp)
  (setf (defaultmenuitem me) 1)
  (call-next-method))

;;; Given a directory, returns the path to the root.

(defun path-to-root (dir)
  (let ((proj (project dir)))
    (labels ((path-to-root-internal (pn)
               (if (or (null pn) (root-directory-pathname-p pn))
                 (list rootdirectory)
                 (cons (new file :osPathname pn :project proj)
                       (path-to-root-internal (pathname-parent-directory pn))))))
      (path-to-root-internal (translate-logical-pathname (ospathname dir))))))

(defun pathname-parent-directory (pn)
  (let ((pn-dir (butlast (pathname-directory pn))))
    (when (cdr pn-dir)
      (make-pathname :host (pathname-host pn)
                     :directory pn-dir))))

(new menuItem :objectname "FileMenuItem" :project sk8
     :properties '((theFile :value nil)))

(setf (private FileMenuItem) nil)  ; in public api

(define-handler file-name-for-menuitem (file)
  (let ((pn (OSPathname me)))
    (cond ((root-directory-pathname-p pn) "Desktop")
          ((directory-pathname-p pn)
           (or (car (last (pathname-directory pn)))
               (pathname-host pn)))
          (t (pathname-name&type pn)))))

(define-handler makemenu (filePopUp)
  (let ((newFiles (path-to-root (currentDirectory (container me))))
        (theProj (project me)))
    ;; Dispose current menuitems.
    (dolist (c (menuitems me)) (setf (menu c) nil))
    ;; Make the new items. 
    (dolist (c newFiles)
      (setf (menu (new fileMenuItem
                       :project theProj
                       :theFile c
                       :text (file-name-for-menuitem c))) me))
    ;; And set the menu's text.
    (setf (text me) (file-name-for-menuitem (car newFiles)))))

(define-handler menuSelect (fileMenuItem)
  (withActorLocked ((menu me))
    (setf (currentDirectory (container (menu me))) (theFile me))
    ))

;;;;-----Now some buttons.
;;;; Here is one for opening, and one for the desktop.
;;;; **** We should add an Eject button as well.

(new DialogBoxHighlightedButton :objectname "FCOpenButton" :project SK8)

(setf (private FCOpenButton) t)

(setf (container FCOpenButton) fileChooser)
(setf (text FCOpenButton) "Open")
(tagpart fileChooser FCOpenButton 'OpenButton)

;;; Making it go through the picker. Then the picker is the only
;;; one that need to worry about what to do when we are viewing different
;;; things (like in the resourceChooser).

(define-handler click (FCOpenButton)
  (doubleClick (picker (fileList (container me)))))

(new DialogBoxButton :objectname "FCDeskTopButton" :project SK8)

(setf (private FCDeskTopButton) t)

(setf (container FCDeskTopButton) fileChooser)
(setf (text FCDeskTopButton) "Desktop")
(tagpart fileChooser FCDeskTopButton 'DeskTopButton)
(define-handler click (FCDeskTopButton)
  (setf (currentDirectory (container me)) rootdirectory))

(define-handler resized (fileChooser)
  (sk8-multival-bind (hSize vSize) (size me)
    (setBoundsRect (FileList me) 10 35 (- hsize 10) (- vsize 40))
    (setBoundsRect (OpenButton me) (- hsize 90) (- vsize 35) (- hsize 10) (- vsize 10))
    (setBoundsRect (DeskTopButton me) (- hsize 190) (- vsize 32) (- hsize 100) (- vsize 13))
    (setLocation (Menu me) (/ hsize 2) 20)))

#|

;;; File notes!

(1) Need proper behavior for desktop.
(2) Need proper handling of hidden files.
(3) Might not want to create more than 1 file object with the same pathname.
(4) Need files to be GCable.
(5) A set of useful files (eg. SK8 directory).
(6) Remove those silly ":" at the end of directories. Maybe the print
function should just print "Directory X".

|#


#|
	Change History (most recent last):
	2  	 7/12/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
