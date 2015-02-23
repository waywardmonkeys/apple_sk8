;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "FILEDIALOGS")

(require "USERDIALOGS" "functions;User Dialogs")

(SK8-declare-syms :SK8 :public ; Updated 11-17-94   4:57 pm
                  SK8::BUILDSTANDALONECHECK2 SK8::CURRENTPROJECT)

(define-sk8-function OpenFileDialog nil (&key ((:project InProject) nil)
                                                 (directory nil)
                                                 (macFileType nil)
                                                 (Title "Open a File...")
                                                 (ButtonText "Open")
                                                 )
  (declare (ignore title))  ;;;************
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (MacChooseFileDialog :project inproject
                       :directory (if directory (ospathname directory))
                       :macFileType macFileType
                       :buttonString ButtonText))

;;;;-------------------------------------------------------------------------------------------------------------------------------------

;;(require-trap DialogRecord)
(defrecord DialogRecord 
  (window :windowrecord)
  (items :handle)
  (textH (:handle :terec))
  (editField :signed-integer)
  (editOpen :signed-integer)
  (aDefItem :signed-integer)
  )

(defrecord ActivateList
  (count :word)
  (data (:array :signed-word 3))
  )

(defrecord OpenFileInfoRecord
  (data :word)
  )

(defrecord newFileInfoRecord
  (data :Str255)
  )

(defpascal OpenProjectModalFilter (:word item :ptr dlg :ptr myDataPtr :word)
  (when (= item 11)
    (rset myDataPtr :OpenFileInfoRecord.data 1)
    (setf item 2))
  item
  )

(define-sk8-function OpenProjectDialog nil (&key (newbutton nil) 
                                                    (directory nil))
  (let (theFile)
    (if newbutton
      (withresourceFile ((or (file sk8) SK8ApplicationFile))
        (%stack-block ((myostype 4))
          (rlet ((theRpl :StandardFileReply)
                 (myfileinfo :OpenFileInfoRecord)
                 )
            (%put-ostype myostype #.(string-upcase (pathname-type *.fasl-pathname*)))
            (rset myfileinfo :OpenFileInfoRecord.data 0)
            (#_customGetFile (%null-ptr)
             1           ;;; 1
                    myostype
             theRpl 
             129 
             -1
             OpenProjectModalFilter 
             (%null-ptr)
             (%null-ptr)
             (%null-ptr)
             myfileinfo)
            (if (and (rref therpl :standardfilereply.sfgood) 
                     (= (rref myfileinfo :OpenFileInfoRecord.data) 0))
              (progn
                (setf thefile (new file :project sk8))
                (setf (ospathname thefile) (%path-from-fsspec (rref therpl :standardfilereply.sffile))))
              (when (= (rref myfileinfo :OpenFileInfoRecord.data) 1)
                (setf thefile nil)
                (eval-in-new-thread "New Project Dialog" `(newprojectdialog)))
              ))))
      (setf thefile (MacChooseFileDialog :project sk8
                                         :directory (if directory (ospathname directory))
                                         :macFileType #.(string-upcase (pathname-type *.fasl-pathname*))
                                         :buttonString "Open")))
    (when thefile
      (eval-in-new-thread "Opening Project" `(withLockedCursor animatedClock
                             ;; remove the sendtolog that was here. it broke when users called this
                             ;; function from within a standalone app built without the projectbuilder ui
                             ;;(sendToLog (format nil "Opening from ~a..." ,theFile))
                             (openProject ,theFile)
                             ;;(sendToLog "... Done!")
                             )))))
;;;;-------------------------------------------------------------------------------------------------------------------------------------


(define-sk8-function NewFileDialog nil (&key ((:project InProject) nil)
                                                (directory nil)
                                                (Title "Create New File:")
                                                (ButtonText "New"))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (MacChooseNewFileDialog :project InProject
                          :directory (if directory (ospathname directory))
                          :prompt title
                          :buttonString ButtonText))

;;;;;;;;;;-------------------------------------------------------------------------------------------------------------

(defun validsk8code (charcode firstchar?)
  (or (= charcode 8)
      (= charcode 9)
      (= charcode 13)
      (= charcode 3)
      (and (= charcode 46) (commandkeydown))
      (and (>= charcode 28) (<= charcode 31))
      (and (>= charcode 48) (<= charcode 57) (not firstchar?))
      (and (>= charcode 65) (<= charcode 90))
      (and (>= charcode 97) (<= charcode 122)))
  )


(defpascal NewProjectModalFilterYD (:ptr dlg :ptr theEvent :ptr itemHit :ptr myDataPtr :word )
  (let ((result 0)
        (returnItemHit (%get-word itemHit))
        (eventType (rref theEvent EventRecord.what))
        (currentEditField (+ (rref dlg DialogRecord.editField) 1))
        charCode
        ObjNameStr)
    (when (and (= eventType 3) (= currentEditField 13))
      (setf charCode (logand (rref theEvent EventRecord.message) #xff))
      (with-returned-pstrs ((ObjNameField255Str ""))
        (%stack-block ((itemTypePtr 2))
          (gs:let+ ((itemPtr (:pointer))
                    (itemRect (:Rect)))
            (#_GetDItem dlg 13 itemTypePtr itemPtr itemRect)
            (#_GetIText (%get-ptr itemPtr) ObjNameField255Str)
            (setf ObjNameStr (%get-string ObjNameField255Str))
            (unless (validsk8code charcode (string-equal objnamestr ""))
              (setf result -1))
            (ccl::with-pstr (objstring objnamestr) (rset myDataPtr :newFileInfoRecord.data objstring))
            ))))
    (when (>= returnItemHit #$sfHookCharOffset)
      (if (= (- returnItemHit #$sfHookCharOffset) 32)
        (#_SysBeep 1)	;;(#_SysBeep 1))
        ))
    result
    ))

(defpascal NewProjectDlgHook (:word item :ptr dlg :ptr myDataPtr :word)
  (let (ObjNameStr FileNameStr (newStr ""))
    (when (or (= item 13) (= item 15) )
      (with-returned-pstrs ((ObjNameField255Str "")
                            (FileNameField255Str ""))
        (%stack-block ((itemTypePtr 2))
          (gs:let+ ((itemPtr (:pointer))
                    (itemRect (:Rect)))
            (#_GetDItem dlg 13 itemTypePtr itemPtr itemRect)
            (#_GetIText (%get-ptr itemPtr) ObjNameField255Str)
            (setf ObjNameStr (%get-string ObjNameField255Str))
            (rset myDataPtr :newFileInfoRecord.data ObjNameField255Str)

            (#_GetDItem dlg 15 itemTypePtr itemPtr itemRect)
            (#_GetIText (%get-ptr itemPtr) FileNameField255Str)
            (setf FileNameStr (%get-string FileNameField255Str))
            
            (setf newstr objnamestr)
            (when (and (not (string-equal filenamestr ""))
                       (not (string-equal objnamestr "")))
              (setf newstr filenamestr))
            (ccl::with-pstr (new255str newstr)
              (#_GetDItem dlg 10 itemTypePtr itemPtr itemRect)
              (#_SetIText (%get-ptr itemPtr) new255str)
              )
            )))
      ))
  item
  )

(defun DoNewProjectThing (pathName projectName withinProj)
  (let (proj)
    (sk8dev::withLockedCursor animatedClock
      (sk8::withProject withinProj
        (sendToLog (concatenate 'string
                                "Creating project " projectName
                                " (a subproject of " (objectString withinProj) ")..."))
        (setq proj (new project :objectName projectName :project withinProj :filename (mcl-namestring-to-sk8-filename pathname))))
      (sendToLog "... Done!")
      proj
      )))

(define-sk8-function NewProjectDialog nil (&key ((:project InProject) sk8) 
                                                   (label2 "with Filename (if different):")
                                                   ) 
  (let (thepath thename)
    (withresourceFile ((or (file sk8) SK8ApplicationFile))
      (with-pstrs ((theStr label2)
                   (defltName ""))
        (rlet ((theRpl :StandardFileReply)
               (activateList :ActivateList)
               (myfileinfo :newFileInfoRecord))
          (rset   activateList Activatelist.count 3)
          (rarset activateList 13 Activatelist.data 0)
          (rarset activateList 15 Activatelist.data 1)
          (rarset activateList  7 Activatelist.data 2)
          (#_customPutFile theStr defltName theRpl 128 -1
           NewProjectDlgHook
           NewProjectModalFilterYD
           activateList
           (%null-ptr)
           myfileinfo)
          (when (rref therpl :standardfilereply.sfgood)
            (setf thepath (%path-from-fsspec (rref therpl :standardfilereply.sffile)))
            (setf thename (%get-string (rref myfileinfo :newFileInfoRecord.data)))
            ))))
    (when thepath
      (when (probe-file thepath) (delete-file thepath))
      (DoNewProjectThing thepath thename inproject)
      )))


;;;;_____________________________________________________________________________________________
;;;;

(define-sk8-function BuildStandaloneDialog nil (&key directory title)
  (declare (ignore title))
  (let ((thePath (NewFileDialog :project sk8
                                :directory directory
                                :title "Build Standalone:")))
    (when thepath
      (eval-in-new-thread "Building A Standalone App"
        `(sk8::buildStandalone
          (physicalName ,thepath)
          :uiwindows (yesornodialog "Do you wish to include the Project Builder in the Standalone?")
          )))))

#|
	Change History (most recent last):
	1	11/15/93	rod	
	2	11/15/93	rod	
	3	11/15/93	rod	
	4	11/15/93	rod	
	5	11/15/93	rod	
	6	11/15/93	rod	
	7	11/15/93	rod	
	8	11/16/93	rod	
	9	11/16/93	rod	
	10	11/17/93	kleiman	
	11	11/19/93	rod	
	12	11/23/93	rod	
	13	11/24/93	hernan	Called resized on the OpenProjectDialogBox.
	14	11/24/93	herbnan	Moving the resized call to the end of the file
				(has no effect where it is now!).
	15	11/29/93	rod	PrintMessage->Logit
	16	11/30/93	rod	
	17	11/30/93	rod	
	18	12/3/93	hernan	Making openProjectDialog finish the withCursor
				call before calling modalDialog. This is to blame
				for the watchCursor not going away when SK8
				comes up.
	19	12/5/93	rod	
	20	12/17/93	till	#.'s be gone: resized, (setf outputFile), deactivateText, activateText, click, keydown, click
	22	1/17/94	hernan	Adding the animatedClock to the open project stuff.
	23	2/12/94	kleiman	name changes
	24	2/17/94	kleiman	ps::openproject -> openProject
	25	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	26	2/21/94	hernan	window -> sk8::window.
	27	2/22/94	rod	
	28	2/25/94	hernan	Using symbols instead of keywords for options!!!
	29	2/26/94	rod	hilighter->highlighter
	30	2/26/94	rod	Getting rid of that stupid f%#$ing rectangle that
				appears at startup.
	31	2/28/94	rod	
	32	3/1/94	brian	It's all hernan's fault
	33	3/2/94	till	Frob callers of macFileType  (OpenProjectDialog)
	34	3/7/94	rod	
	35	3/11/94	till	Speed up file dialogs.  This involves changes to 
				                                openprojectdialog, openfiledialog, newfiledialog.
	36	3/14/94	rod	
	37	3/15/94	rod	
	38	3/15/94	rod	
	39	3/16/94	rod	
	40	3/18/94	rod	
	41	3/18/94	rod	
	42	3/22/94	rod	Making option to disable files.
	43	3/22/94	rod	
	44	3/23/94	sidney	save-sk8 -> sk8::save-sk8
	45	3/24/94	rod	Restrict file names to 31 chars.  Make sure 
				dialogs match picker size.
	46	3/24/94	rod	
	47	3/25/94	rod	
	48	3/26/94	rod	
	49	3/26/94	rod	
	50	3/29/94	rod	
	51	3/30/94	rod	WatchCursor stuff.
	52	3/31/94	Hernan	Typo in call to save-sk8.
	53	3/31/94	Hernan	Click of openButton now sends the physical path
				to save-sk8. (Hernan)
	54	4/4/94	kleiman	BuildStandaloneDialog translates SK8 pathname
				to an appropiate MCL pathname
	55	5/4/94	till	Fix click openbutton for NewFileDialog, 
				bug number 1160750.
	56	5/6/94	rod	Redoing buttons
	57	5/9/94	rod	
	58	6/3/94	rod	
	59	7/6/94	rod	
	60	7/19/94	kleiman	1175346 "Save Projects" checkbox added
	61	7/19/94	kleiman	
	62	8/6/94	sidney	1177239: File new dialog error while checking for name exists as directory
	63	8/22/94	rod	Updating the currentDirectory of the system.
	64	8/22/94	rod	
	65	9/9/94	till	Bug 1183792.  Better check for the existance of a directory with that filename in click of openButton of FileChooser.
	66 	 9/12/94	rod     	
	67 	 9/28/94	rod     	Fixing bug with saving a copy as.
	68 	10/ 4/94	sidney  	bulletproof an eval-enqueue in case things are being destroyed
	69 	11/14/94	rod     	
	70 	11/17/94	rod     	
	71 	11/17/94	dy      	fixing new project dialog.
	72 	11/18/94	till    	Fix a bug and a bunch of ugliness in click of openbutton of filechooser of newProjectDialogBox.
	73 	12/ 3/94	rod     	Fixing default keytarget of open file dialog.
	74 	12/ 3/94	rod     	Fixing cancel of newfiledialog.
	75 	12/ 3/94	rod     	
	76 	12/ 3/94	rod     	shift-tab in newprojectdialog.
	77 	12/ 3/94	rod     	error message fixes.
	78 	12/16/94	rod     	
	79 	12/16/94	rod     	
	80 	12/16/94	rod     	
	81 	 1/11/95	Hernan  	Passes the pathname to buildStandAlone (not the file).
	82 	 1/16/95	rod     	Fixing buildstandalone dialog to cancel properly.
	83 	 1/30/95	rod     	Fixing bug where clicking in new project dialog.
							Typing was the only event which updated the
							data.
	84 	 1/30/95	rod     	
	85 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	86 	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	87 	 3/ 2/95	rod     	
	88 	 3/13/95	sidney  	make open and new  project queued so they don't hold up everything when the menu item is selected
	89 	 3/14/95	rod     	leaving title keyword in openfiledialog in the
							hopes of later making it work.
	90 	 3/18/95	rod     	
	91 	 3/19/95	sidney  	remove option of building standalone with separate project file
	92 	 3/22/95	rod     	Fixing new project dialog.
	93 	 3/22/95	rod     	
	94 	 4/ 3/95	sidney  	1234485: don't send message to log from a user function
	2  	 7/10/95	Brian Roddy	getting rid of eval-enqueues
	3  	 7/10/95	Brian Roddy	fixing eval-enqueues
	4  	 9/21/95	Till    	Add Liposuction Mode.
	5  	 9/26/95	Till    	fix humorous typo
	6  	 9/26/95	Till    	The senator meant to include a comma...
	7  	 9/27/95	Till    	One more comma.
	8  	 2/ 7/96	sidney  	remove wood
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 4/25/96	Hernan  	Removed all references to liposuction.
	5  	 8/ 1/96	Hernan  	OpenProjectDialog should open a fasl or a pfasl depending
						on what Sk8 you are running.
	6  	10/21/96	sidney  	tweak the expression used to refer to fasl pathnames
	7  	11/12/96	sidney  	added a name argument to eval-in-new-thread
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
