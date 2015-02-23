(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

;;; _______________________________ 
;;; Low Level Idle Machinery.
;;; _______________________________ 

(define-handler sk8::lowLevelIdle (object)
  )

;;; Dispatched lowLevelIdle to everything in the variable. 

(defun dispatch-lowLevelIdle ()
  (dolist (c *objects-that-want-lowLevelIdle*)
    (lowLevelIdle c)))

;;; If the object supports the lowLevelIdle event, it is added to the
;;; *objects-that-want-lowLevelIdle* variable.

(define-sk8-function registerForLowLevelIdle nil (theObject)
  (if (canDo 'lowLevelIdle theObject)
    (pushNew theObject *objects-that-want-lowLevelIdle*)
    (error "lowLevelIdle is undefined for ~a." (objectString theObject))))

;;; Removes the object from the *objects-that-want-lowLevelIdle*.

(define-sk8-function unregisterForLowLevelIdle nil (theObject)
  (when (memq theObject *objects-that-want-lowLevelIdle*)
    (setf *objects-that-want-lowLevelIdle* (delete theObject *objects-that-want-lowLevelIdle*))))

;;; If in the context of processing an event, this returns the event record.

(define-sk8-function currentEventRecord nil ()
  (declare (special ccl::*current-event*))
  (when (boundp 'ccl::*current-event*)
    ccl::*current-event*))

;;; _______________________________ 
;;; EventInterest Machinery.
;;; _______________________________ 

;;; Assumes the object in question has an eventInterests property that contains a hash table
;;; of objects keyed on event names (symbols).

;;; API: (functions)
;;; (0) makeEventInterestsHashTable() 
;;; (1) addEventInterest(interestedObject event eventDispatcher)
;;; (2) removeEventInterest(interestedObject event eventDispatcher)
;;; (3) dispatchToInterestedObjects(eventDispatcher event)
;;; (4) objectsInterestedInEvent(eventDispatched event)

(define-sk8-function makeEventInterestsHashTable nil ()
  (make-hash-table :test #'eq))

(define-sk8-function objectsInterestedInEvent nil (eventDispatcher event)
  (getHash event (eventInterests eventDispatcher)))

(define-sk8-function addEventInterest nil (interestedObject event eventDispatcher)
  (let* ((eventInterests (eventInterests eventdispatcher))
         (currentInterestedGuys (gethash event eventInterests)))
    (unless (memq interestedObject currentInterestedGuys)
      (setf (gethash event eventInterests) (cons interestedObject currentInterestedGuys)))))

(define-sk8-function removeEventInterest nil (interestedObject event eventDispatcher)
  (let* ((eventInterests (eventInterests eventdispatcher))
         (currentInterestedGuys (gethash event eventInterests)))
    (when (memq interestedObject currentInterestedGuys)
      (setf (gethash event eventInterests) (remove interestedObject currentInterestedGuys)))))

(define-sk8-function eventsDelegated nil (interestedObject eventDispatcher)
  (let* ((theHashTable (eventInterests eventDispatcher))
         (theKeys (hash-table-keys theHashTable))
         result)
    (dolist (aKey theKeys result)
      (when (memq interestedObject (gethash aKey theHashTable))
        (push aKey result)))))

(define-sk8-function dispatchToInterestedObjects nil (eventDispatcher event &rest args)
  (dolist (c (objectsInterestedInEvent eventDispatcher event))
    (when (project c) ;; ignore objects that we have tried to dispose
      (apply event c args))))

(defun hash-table-keys (hashTable)
  (let (result)
    (maphash #'(lambda (key value) 
                 (declare (ignore value))
                 (pushnew key result))
             hashTable)
    result))

(define-sk8-function clearProjectInterests nil (eventDispatcher theProj)
  (let* ((theTable (eventInterests eventDispatcher))
         (theKeys (hash-table-keys theTable))
         theList)
    (dolist (aKey theKeys)
      (setf theList (gethash aKey theTable))
      (setf theList (delete-if #'(lambda (x) (or (eq x theProj) (eq (project x) theProj))) theList))
      (if theList
        (setf (gethash aKey theTable) theList)
        (remhash aKey theTable)))))

;;; _______________________________ 
;;; And now, the System object.
;;; _______________________________ 

;;; Needed: ram size, rom version, canYouDo

(new object :objectName "System" :project sk8 :undisposable t
     :properties '((currentCommandKey :value nil)   ; the object which should get keyDown events on command keys
                   (eventListeners :value nil)      ; list of objects interested in system events
                   (clock :value nil)               ; System Clock (set by Orchestrator file in build)
                   (eventInterests :value nil)      ; The hash table of interests.
                   (currentDirectory :value nil)))  ; Added here by Don to work with Files...

(setf (eventInterests System) (makeEventInterestsHashTable))
                    
;;; _______________________________ 
;;; System Events (can be intercepted by users using event listeners).
;;; _______________________________ 

;;; resume -- called whenever the SK8 application resumes
;;;

(define-handler resume (System)
  (declare (special mf::*events-on*))
  ;; Switch events on. If they are on already it must mean that
  ;; however turned them off in the first time is done. If we are
  ;; to err we will do it on the side of turning things on.
  (unless mf::*events-on*
    (setf mf::*events-on* mf::*events-were-on*))
  ;; Dispatch to interested objects.
  (dispatchToInterestedObjects me 'resume))

;;; suspend -- called whenever the SK8 application is about to be suspended
;;;          that is, when the user is about to switch to another application
;;;

(define-system-handler suspend (System)
  (declare (special mf::*events-on*))
  ;; Dispatch event to the listeners.
  (dispatchToInterestedObjects me 'suspend)
  ;; Switch events off and remember whether they should be turned on again.
  (setf mf::*events-were-on* mf::*events-on*)
  (setf mf::*events-on* nil))

;;; ActiveApplication -- Returns true if SK8 is the frontmost Application.
;;;
(define-system-handler ActiveApplication (System)
  ccl::*foreground*)


;;; These defined so that the UI hears about it.

(define-handler createdNewProject (System Proj)
  (dispatchToInterestedObjects me 'createdNewProject proj))

(define-handler openedProject (System Proj)
  (dispatchToInterestedObjects me 'openedProject proj))

(define-handler setInform (System objs propertyName)
  (dispatchToInterestedObjects me 'setInform objs propertyName))

;;; _______________________________ 
;;; Informational
;;; _______________________________ 

;;; fonts -- returns a list of all fonts currently available in the system
;;;

(define-handler fonts (System)
  (declare (ignore me))
  (let ((result nil))
    (mapKnownDescendants
     font
     #'(lambda(fnt) (push fnt result)))
    result))

;;; textStyles -- returns a list of all textStyles currently available in the system
;;;

(define-handler textStyles (System)
  )

;;; processor -- a string specifying the current cpu
;;;

(define-handler processor (System)
  (cpu-number))

;;; name -- system name (Macintosh chooser name)
;;;

(define-handler name (System)
  (machine-instance))

;;; version -- system version (Macintosh operating system version)
;;;

(define-handler version (System)
  (cadr (memq :system-version *environs*)))

;;; maxColorDepth -- maximum color depth of system
;;;

(define-handler maxColorDepth (System)
  (cadr (memq :color-quickdraw *environs*)))
  
;;; printers -- returns list of all printer devices accessible to system
;;;

(define-handler printers (System)
  (knownchildren Printer))

;;; monitors -- returns list of all monitor devices connected to system
;;;

(define-handler monitors (System)
  (knownchildren Monitor))

(define-handler sk8:mainMonitor (system)
  (dolist (m (monitors me) (sk8-error GeneralProgrammaticError
                                      :strings '("This system no monitor!")))
    (when (mainMonitor m)
      (return m))))

(define-handler storageDevices (System)
  (knownchildren storageDevice))


;;; Returns the point in the center of the main monitor.

(define-sk8-function mainMonitorCenter nil ()
    (let ((main (dolist (m (monitors system))
                  (when (mainMonitor m)
                    (return m)))))
      (SK8-multival-bind (ll tt rr bb) (boundsRect main)
        (SK8-multivals (round (/ (+ ll rr) 2)) (round (/ (+ tt bb) 2))))))

;;; keyboards -- returns list of all keyboard devices connected to system
;;;

(define-handler keyboards (System)
  (knownchildren Keyboard))

#|
;;; modems -- returns list of all modem devices connected to system
;;;

(define-handler modems (System)
  (knownchildren Modem))
|#

;;; This used to be "ticks of Now"

(define-sk8-function systemTickCount nil ()
  (#_tickCount))


#|
	Change History (most recent last):
	2	6/9/93	kleiman	on-line documentation
	3	6/11/93	kleiman	more online documentation
	4	6/15/93	kleiman	documentation cosmetics
	5	6/25/93	kleiman	maximumDepth -> maximumColorDepth
	12	8/31/93	Brian Roddy	Adding property "currentDirectory
	13	9/3/93	hernan	Noting that resume and suspend are redefined in
				the clipboard file.
	14	11/1/93	kleiman	d4 store changes
	15	12/21/93	kleiman	take out documentation strings
	16	1/10/94	hernan	Fonts just became objects!!!
	17	2/14/94	sidney	rename descendants to knowndescendants, children to knownchildren
	18	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	19	3/3/94	kleiman	storageDevices
	20	3/24/94	kleiman	eventListeners property of SYSTEM
				systemEvent function
	21	3/24/94	kleiman	systemEvent defined as a handler on System
	22	3/25/94	kleiman	systemEvent applies event message to object
	23	7/14/94	Hernan	Adding the eventInterest machinery to be used
				by the System object and the Stage object.
	24	7/14/94	Hernan	Adding a function to remove from the event
				interests all objects from a project.
	25	7/14/94	Hernan	Giving all window related stuff in suspend and
				resume to the Stage.
	26	7/15/94	Hernan	Adding a function to check what an object is
				interested in.
	27	7/18/94	Hernan	Moving definition of *events-on* and 
				*events-were-on* to 02-system.
	28	7/18/94	Hernan	Ooops. Those vars should be in the mf package.
	29	8/4/94	rod	adding openedlibrary.
	30 	11/ 4/94	Hernan  	*events-were-on* now starts as true.
	31 	12/21/94	rod     	Adding predicate "ActiveApplication" to system.
	32 	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	33 	 3/27/95	sidney  	clearProjectInterests could die when passed a closed project or closed project's object
	34 	 3/28/95	Hernan  	1233197: resume of System only sets *events-on*
							if it is False when we return.
	35 	 4/12/95	rod     	Removing undefined devices.
	2  	 8/ 2/95	Hernan  	Adding lowLevelIdle API.
	3  	 8/ 4/95	Hernan  	Ooops. currentEventRecord was not referen the right
						variable. Now it is.
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 7/18/96	sidney  	move declare of event globals to globals file
	2  	11/26/96	Hernan  	Defining lowlevelidle to do nothing.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
