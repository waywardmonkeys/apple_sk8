(in-package :sk8dev)

(provide "CLIPBOARD")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| TO DO

(1) project stuff: making things in other projects and the problems involved!


|#

;;; _______________________________ 
;;; Low level scrap functions. 
;;; _______________________________ 

;;; Returns whether something of the restype provided is available on the
;;; scrap.

(defun typeInScrap (resType)
  (%stack-block ((offsetLong 4))
    (plusp (#_getScrap (%null-ptr) restype offsetLong))))

;;; theHandle is a handle. TheType is a resType.

(define-sk8-function putScrap nil (theHandle theType)
  (without-interrupts
   (let ((size (T_GetHandleSize theHandle)))
     (with-dereferenced-handles
       ((thePtr theHandle))
       (#_PutScrap size theType thePtr)))))

(define-sk8-function getScrap nil (theType)
  (without-interrupts
   (let* ((theHandle (T_NewHandle 0)))
     (rlet ((junk :signed-long))
       (#_GetScrap theHandle theType junk))
     theHandle)))

(define-sk8-function zeroScrap nil ()
  (#_zeroScrap))

;;; Weird special case: Strings:

(defun string-to-scrap (theString)
  (without-interrupts
   (when theString
     (with-macptrs ((h (%get-ptr (%int-to-ptr 2740))))
       (ccl::%str-to-handle theString h nil)
       (with-pointers ((p h))
         ;;This is a little weird because _PutScrap can trigger a gc
         (#_PutScrap (T_GetHandleSize h) :text  p))
       (#_SetHandleSize :errchk h 0)))))

(defun scrap-to-string ()
  (without-interrupts 
   (let ((theString))
     (with-macptrs ((h (%get-ptr (%int-to-ptr 2740))))
       (%stack-block ((poffset 4))
         (let ((size (ccl::%imax 0 (#_GetScrap h "TEXT" poffset))))
           (with-pointers ((p h))
             (setf theString (ccl::%str-from-ptr p size)))
           (#_SetHandleSize :errchk h 0))))
     theString)))

;;; ______________
;;; The Clipboard!
;;; ______________

(new object :objectName "ClipBoard" :project sk8
     :prototype t
     :undisposable t
     :properties '((objectsOnHold :value nil)
                   (clipboardOpen :value t)
                   (incomingData  :value nil)
                   (thingsExported :value nil) ;; Things we wrote out to the system scrap.
                   (scrapCount :value -1)))

;;; This variable is set to t by addToClipboard to let us know that
;;; when we leave SK8 we should add our clipboard contents to the
;;; system's scrapbook.

(defvar *new-clipboard-contents-since-resume* nil)

;;; ______________
;;; Informational File Utilities.
;;; ______________

;;; Run through the media types checking all resource types.

(define-handler macResourceTypesAvailable (clipboard)
  (let ((result nil)
        thetype)
    (dolist (c (knownChildren media))
      (setf theType (restype c))
      (when (and theType (typeInScrap theType))
        (pushnew theType result)))
    ;; Special text: text.
    (when (typeInScrap "TEXT")
      (pushnew "TEXT" result))
    result))

;;; The intersection of the above and sk8importableResources.

(defun remove-intersection (set intersection)
  (cond ((null set) nil)
        ((member (car set) intersection :test #'string-equal)
         (cons (Car set) (remove-intersection (cdr set) intersection)))
        (t (remove-intersection (cdr set) intersection))))

(define-handler macSK8ImportableResourceTypesAvailable (clipboard)
  (let ((availableResources (macresourceTypesAvailable me))
        (resourcesOk nil)
        thetype)
    (mapKnownDescendants
     Translator
     #'(lambda (c)
         (setf theType (externalType c))
         (when (member theType availableResources :test #'string-equal)
           (pushnew theType resourcesOk)
           (setf availableResources (delete theType availableResources))
           (unless availableResources 
             (return-from macsk8ImportableResourceTypesAvailable nil)))))
    resourcesOk))

;;; ______________
;;; THE API!
;;; ______________

(define-handler clearClipboard (clipBoard)
  (setf (objectsOnHold me) nil))

;;; Adds the object to the clipboard...

(define-handler addToClipboard (Object theClipBoard &key (copy t) (overWrite t))
  (let ((newMember nil))
    ;; [0] Clear the incomingData field.
    (setf (incomingData theClipboard) nil)
    ;; Record the fact that we have changed the clipboard since we resumed.
    (setf *new-clipboard-contents-since-resume* t)
    ;; [1] Make a copy if required.
    (if copy
      (setf newMember (sk8::copy me))
      (setf newMember me))
    ;; [2] Replace the contents or add to clipboard.
    (if overwrite
      (progn (clearClipboard theClipBoard)
             (setf (objectsOnHold theClipBoard) 
                   (if (listp newMember)
                     newMember
                     (list newMember))))
      (pushnew newMember (objectsOnHold theClipBoard)))))

;;; These two need to deal with system stuff...

(define-handler objectsInClipboard (clipboard)
  (if (incomingData me)
    ;; Return all the things we could make from stuff in the scrap.
    (let ((result nil))
      (dolist (c (ImporttranslatorsApplicable me) result)
        (pushnew (internalObject c) result)))
    (objectsOnHold me)))

(define-handler typesInClipboard (clipboard)
  (if (incomingData me)
    (let ((translatorsApplicable (importTranslatorsApplicable me))
          result)
      (dolist (c translatorsApplicable result)
        (pushnew (finalObject c) result)))
    (remove-duplicates (mapcar #'baseParent (objectsOnHold me)))))

;;; Calls get-scrap on the first type on the list that is found in the scrap state.

(defun get-type-from-scrap (typeList)
  ;; Make sure *scrap-state* has the right thing in it.
  (ccl::get-external-scrap)
  ;; Now do it.
  (dolist (aType typeList)
    (when (member aType ccl::*scrap-state* :test #'string-equal)
      (return-from get-type-from-scrap (get-scrap (ccl::make-keyword aType))))))

;;; Thing is an object!

(define-handler getFromClipboard (clipboard thing inProject &key copy removal (everyone t))
  (if (incomingData me)
    ;; External Objects
    (let ((theTranslators (importtranslatorsApplicable me))
          theNewObject)
      (dolist (c theTranslators)
        (when (or (inheritsFrom thing (finalObject c)) 
                  ;; This is needed because String does not inherit from String.
                  (eq (finalObject c) thing))
          ;; Since import takes the media type, make sure that is what we pass.
          (if (inheritsFrom thing (internalObject c))
            (setf theNewObject (new thing :project inProject))
            (setf theNewObject (new (internalObject c) :project inProject)))
          (setf theNewObject (sk8::import c :source me :destination theNewObject))
          (setf (incomingData me) nil)
          (addToClipboard theNewObject me :overwrite t)
          (return)))
      ;; Now insert the newObject in the clipboard and return it!
      (when theNewObject
        (list theNewObject)))
    ;; SK8 Objects
    (let ((thingsToReturn nil))
         ;; [1] Figure out what we want to get from the clip.
      (if everyone
        ;; Get them all!
        (dolist (c (objectsOnHold me))
          (when (inheritsFrom c thing)
            (pushnew c thingsToReturn)))
        ;; Returns as soon as we find one.
        (dolist (c (objectsOnHold me))
          (when (inheritsFrom c thing)
            (pushnew c thingsToReturn)
            (return))))
      ;; Figure out what to return.
      (if removal
        ;; Remove what we found and return it!
        (dolist (c thingsToReturn thingsToReturn)
          (setf (objectsOnHold me) (remove c (objectsOnHold me))))
        (if copy
          (progn
            ;; Remove originals found.
            (dolist (c thingsToReturn thingsToReturn)
              (setf (objectsOnHold me) (remove c (objectsOnHold me))))
            ;; Add copies to the clipboard.
            (dolist (c thingsToReturn)
              (pushnew (sk8::copy c :project (or inProject (project c)))
                       (objectsOnHold me)))
            ;; Return the originals found.
            thingsToReturn)
          ;; Just return what we found.
          thingsToReturn)))))

;;; ______________
;;; The SK8Clipboard!
;;; ______________

(new clipboard :objectName "SK8Clipboard" :project sk8
     :undisposable t)

(addProperty sk8Clipboard 'eventInterests)
(setf (eventInterests sk8Clipboard) (makeEventInterestsHashTable))

(define-handler copySelectionToClipboard (string)
  (addToClipboard me SK8Clipboard))

;;; The Sk8Clipboard wants to get suspend and resume events from the system.

(addEventInterest Sk8Clipboard 'suspend System)
(addEventInterest Sk8Clipboard 'resume System)

;;; ______________
;;; Interface between our scrap and MCL's... 
;;; ______________

;;; Called when we are switching from a SK8 window to a Lisp window. If the text in the
;;; sk8Clipboard is not the text in the MCL scrap, it puts it there.

(defun sendTextToMclScrap ()
  (let ((theText (car (getFromClipboard sk8Clipboard String sk8))))
    (when (and theText (not (string-equal theText (get-scrap :text))))
      (put-scrap :text theText))))

;;; Called when we are swtiching from a Lisp window to a SK8 window. Again if the text in
;;; the clipboard is not the same we set it.

(defun getTextFromMclScrap ()
  (let ((theText (get-scrap :text))
        (textInClipboard (car (getFromClipboard sk8clipboard String sk8))))
    (if (string-equal theText textInClipboard)
      ;; Make sure we output this string to the outside world. 
      (setf *new-clipboard-contents-since-resume* t)
      ;; Get the text from the MCL scrap. 
      (addToClipboard theText sk8Clipboard :overwrite t :copy nil))))

;;; MCL cannot read from the external scrap!!! SK8 takes control of the whole thing.

(let ((ccl::*warn-if-redefine-kernel* nil))
  (defun ccl::put-external-scrap ()
    ))

;;; ______________
;;; Interface between our scrap and the world!
;;; ______________

(defconstant $scrapvars 2400)

(defmacro system-ScrapCount ()
  `(rref (ccl::%int-to-ptr $scrapvars) :scrapStuff.scrapCount))

(define-handler resume (sk8Clipboard)
  ;; Clear the variable that records whether we have put
  ;; a new thing in the clipboard since we resumed.
  (setf *new-clipboard-contents-since-resume* nil)
  ;; Record if anything on system scrap.
  (unless (= (system-scrapCount) (scrapCount sk8Clipboard))
    (setf (incomingData sk8Clipboard) t)
    (setf (thingsExported me) nil)
    (setf (scrapCount sk8Clipboard) (system-scrapCount)))
  ;; Dispatch to interested objects. 
  (dispatchToInterestedObjects me 'resume))

(define-handler suspend (Sk8Clipboard)
  (let ((scrapZeroed nil)
        theTranslator)
    ;; Send stuff from MCL to our clipboard.
    (unless (gs:sk8-window-p gs::*currentTla*)
      (getTextFromMCLScrap))
    (when *new-clipboard-contents-since-resume*
      ;; For each object, see if it can be exported. If so, do it!
      (dolist (c (objectsOnHold sk8Clipboard))
        (unless (memq c (thingsExported me))
          (setf theTranslator (findExportTranslator c me))
          (when theTranslator
            ;; Better clear the scrap to allow stuff to be written by MCL.
            (unless scrapZeroed
              (zeroScrap)
              (setf (thingsExported me) nil)
              (setf scrapZeroed t))
            (sk8::export theTranslator :source c :destination me)
            ;; Add it to the thingsExported.
            (unless (stringp c)
              (setf (thingsExported me) (cons c (thingsExported me))))
            ))))
    ;; Save the scrap count.
    (setf (scrapCount sk8Clipboard) (system-scrapCount))
    ;; Dispatch to interested objects.
    (dispatchToInterestedObjects me 'suspend)))
 

#|
	Change History (most recent last):
	1	9/2/93	hernan	New file!
	2	9/3/93	hernan	Scanned and added symbols to the preload files.
	3	9/3/93	hernan	Suspend and Resume now deactivate and activate
						the text of the keytarget of the current window.
	4	9/20/93	hernan	Suspend and resume now show and hide the
						background window.
	5	10/29/93	hernan	The clipboard now needs to do more work since
						import and export do less.
	6	10/29/93	hernan	Do not do anything weird on suspend/resume till
						I figure it all out.
	7	11/1/93	hernan	mf::inheritsFrom? -> inheritsFrom.
	8	11/8/93	hernan	Making the clipboard consistent with the new
						translators.
	9	11/10/93	hernan	Redefined the functions that move text accross 
						the SK8 scrap to the MCL scrap. Also added a few
						functions to the API.
	10	11/10/93	hernan	Fixing but that does not let you create stuff from
						the scrap unless you provide a project argument.
	11	11/12/93	hernan	Making suspend pass text to the MCL scrap if a 
						SK8 window was the current window.
	12	11/24/93	hernan	Fixing resume to update ccl::*scrap-count* so that
						we do not miss any changes to the system scrap.
	13	11/29/93	hernan	Removing specialization of addtoClipboard that
						removes an actor from its container when it goes
						into the clipboard.
						Also fixed getFromClipboard to handle returning
						text (using coercion) and to return the original
						stuff and leave copies in the clipboard on paste
						(this makes move operations work).
	14	11/30/93	hernan	Setting *events-on* to false on suspend and true
						on restore.
	15	1/11/94	hernan	self -> me
	16	1/31/94	hernan	Making suspend and resume check whether the 
						depth of any monitors has changed. If so, we have
						to update tlas to use the best depth available.
	17	2/14/94	sidney	rename children to knownchildren
	18	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	19	2/22/94	hernan	Fixing getFromClipboard's arglist.
	20	2/28/94	hernan	Requiring project be specified in all these functions.
	21	3/2/94	Hernan	Porting to Fred 3.0
	22	3/3/94	kleiman	addToClipboard defmethod -> define-handler
	23	3/3/94	kleiman	(monitor-depths) -> (mapcar #'colorDepth (monitors system))
	24	3/21/94	Hernan	Adding copySelectionToClipboard method for strings.
	25	4/15/94	Hernan	The project argument is now required in getFromClipboard.
	26	5/9/94	Hernan	Fixed getFromClipboard to make stuff out of the 
						external clipboard. get-from-scrap (all those 
						methods) should be obsoleted.
	27	6/6/94	chip	made getFromClipboard for "Text" COERCE TO STRING instead 
						of just getting the simpleObjectString
	28	6/7/94	Hernan	Making sure sk8importableResourceTypesAvailable
						does not return duplicates.
	29	6/10/94	Hernan	1159082: get-external-scrap does not need to be
						called unless the user actually cares about the
						contents of the clipboard. Thus, the user only
						pays when he wants something out of it.
	30	6/23/94	sidney	get rid of warning about mf::*events-on* needing to be declared
	31	6/27/94	Hernan	1169732: Removing weird special case in addToClipboard.
						Seemed to copy each item in a list if the thing to 
						add is a list of objects. Why? Why did it have to
						do that and behave differently from say copying
						an array? I do not know and no longer will it be 
						that way.
	32	7/1/94	Hernan	1168757: Saves the state of the *events-on* var
						before suspending to restore it properly at
						resume time.
	33	7/8/94	Hernan	New Translators are being implemented. The
						Clipboard adapts. And it is so cool...
	34	7/13/94	Hernan	1173846: Making the clipboard work with text.
	35	7/13/94	Hernan	The text translator now uses String as its 
						internal object.
	36	7/14/94	Hernan	Adding the Sk8Clipboard's interest in suspend and
						resume.
	37	7/18/94	Hernan	Fixing the clipboard to export stuff when suspend
						gets called.
	38	7/18/94	Hernan	Installed our very own scrap handling code totally
						separate from MCL's efforts (we event redefine
						ccl::put-external-scrap to do nothing since we do
						not want MCL to mess with us).
	39	7/20/94	Hernan	Getting rid of extranous beep.
	40	7/29/94	Hernan	1177238: objectsInClipboard is calling an obsolete
						function.
	41	7/29/94	Hernan	1177241: When getting from the clipboard something
						from the scrap, compare the thing wanted to the
						finalObject of the translators.
	42 	 8/31/94	Hernan  	*currentWindow* -> *currentTla*.
	43 	 9/12/94	Hernan  	1180502: capitalizing object names.
	44 	10/ 3/94	Hernan  	Resource methods will all start with "mac".
	45 	11/17/94	Hernan  	Added a variable to record whether the clipboard's
							contents were changed in this session. This stops
							SK8 from bashing the contents of the system clipboard
							when there is no need to do so.
	2  	 6/23/95	Hernan  	Making the SK8Clipboard broadcast suspend and resume events.
	6  	 2/27/97	Hernan  	define-system-handler->define-handler.
						5   5/ 2/96Hernan  getTextFromMCLScrap makes sure *new-clipboard-contents-since-resume* is t
						so that the text can be put out to the outside world.
|# ;(do not edit past this line!!)
