;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :macframes)

;;;
;;; MacFrames Constants
;;;

;;; *all-weak-hash-tables* -- list of all weak hash tables
;;;   This list is visited just before GC kicks in so that weak hash tables' caches
;;;   can be cleared: the MCL 2.0.1 garbage collector doesn't deal with weak hash
;;;   table caches as well as one would expect.
(defvar *all-weak-hash-tables* nil)


;;; *no-initarg* -- marker for lack of an initarg in an initarg list
;;;

(defvar *no-initarg* ccl::%special-marker%) ; a borrowing

;;;
;;; MacFrames Globals
;;;

;;; *object* -- bound to object object
;;;
(defvar *object* nil)

;;; *project* -- bound to project object
;;;

(defvar *Project* nil)

;;; *sk8-project* -- bound the the SK8 project object
;;;

(defvar *SK8-Project* nil)

;;; *object-class* -- the class assigned by boostrap to the object Object
;;;

(defvar *Object-class* nil)

;;; *project-class* -- the class assigned by boostrap to the object Project
;;;

(defvar *Project-class* nil)

;;; *sk8-class* -- the class assigned by bootstrap to the object SK8 (the SK8 project object)
;;;

(defvar *SK8-class* nil)



(defvar *Collection-object* nil)
(defvar *Collection-class* nil)
(defvar *Text-object* nil)
(defvar *Text-class* nil)
(defvar *String-object* (find-class 'string))


;;; See description and initialization in "01-object"
;;;
(defvar *object-pseudo-children-and-precedence-lists* nil)


;;; *closing-project* -- when T, a project is being closed
;;;   A closed project's store is not disposed!!!
;;;

(defvar *closing-project* nil)


;;; *deleting-object* -- when T, we are deleting an object,
;;;   so all children of the object MUST be deleted, too.
;;;

(defvar *deleting-object* nil)

;;; *sk8-bootstrap* -- non-nil when SK8 is bootrapping (build sequence only)
;;;

(defvar *sk8-bootstrap* nil)


;;; *saving-into-project-store* -- when non-nil, we're saving objects into a project store
;;;

(defvar mf::*saving-into-project-store* nil)


;;; *inheritance-relation-names* -- Names of all inheritance relations
;;;

(defvar *inheritance-relation-names* '(sk8::knownchildren sk8::parents))

;;; *mf-internal-format-slot-names* -- Slots which are privately used by MacFrames
;;;

(defvar *mf-internal-format-slot-names* '(sk8::sk8_id
                                                sk8::sk8_flags
                                                sk8::project))

;;; *mf-private-property-names* -- Slots which are available in most MacFrames objects
;;;

(defvar *mf-private-property-names*
  (append *mf-internal-format-slot-names*
          *inheritance-relation-names*))

;;; *valid-property-spec-elements* -- list of valid property spec keywords
;;;

(defvar *valid-property-spec-elements* '(:inherit :value :private))

;;; *special-new-initarg-keywords* -- initarg keywords that are neither real nor virtual properties of an object
;;;

(defvar *special-new-initarg-keywords* '(:objectName :properties :Project :otherParents :privateObject :skipInitialization))

;;; *last-object-oid* -- oid for SK8 objects
;;;    Each newly created SK8 object gets an oid which is monotonically increasing
;;;    as the SK8 session progresses. This id is used to sort SK8 objects
;;;    according to creation order. This is important for efficient re-loading of objects
;;;    when saved in a textual format.
;;;

(defvar *last-object-oid* 0)

;;; *property-characteristics* -- table contains pdata objects for properties
;;;

(defvar *property-characteristics* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;
;;; CONTROL GLOBALS
;;;;;;;;;;;;;;;;;;;

;;; *root-object-class-list* --List of CLOS classes, one of which any MacFrames IV object must belong to
;;;

(defvar  *root-object-class-list* (cons 'sk8::object nil))

;;; *root-object-class* -- Initialized to the class for the object Object
;;;

(defvar *root-object-class* nil)

;;; Should handler definitions be saved?
(defvar *save-handler-definitions* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MacFrames Object System Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *valid-macframes-handler-types* -- valid macframes handler types
;;;

(defvar *valid-macframes-handler-types* '(nil :before :after :singleton))

;;; *sk8-package* -- The SK8 project's package
;;;

(defvar *SK8-Package* (find-package :SK8))


(defvar *sk8dev-package* (find-package :sk8dev))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics System Globals
;;;;;;;;;;;;;;;;;;;;;;;

;;; *use-offscreen-pixmap* -- Use offscreen pixmaps for SK8 windows, unless otherwise specified
;;;

(defvar *use-offscreen-pixmap* t)

;;; *offscreen-browsers* -- Use offscreen pixmaps for SK8 User Interface browsers
;;;

(defvar *offscreen-browsers* t)

;;;  *save-internal-handler-definition* -- informs define-handler that the handler definition text
;;;                               should be saved by define-handler.
;;;  Note: typically, a MacFrames handler or SK8Script handler text editor will save
;;;  the handler or script's text via save-handler-definition or save-handler-script
;;;  However, in the case where a MacFrames handler is evaluated, e.g., in a FRED text buffer,
;;;  by a user (i.e., without the knowledge of SK8 browsers or editors), the responsibility
;;;  for saving the definition falls to define-handler.
;;;  *** Take this out as soon as SK8Script is up and running
;;;

(defvar *save-internal-handler-definition* nil)

;;;;;;;;;;;;;;;;;;;;;
;;; Event System Globals
;;;;;;;;;;;;;;;;;;;;;

;;; *EVENTS-ON*
;;; Specifies whether the SK8 Graphics event system should be running or not
;;;

(defvar *events-on* t)

;;; Remembers at suspend time whether the events are on.

(defvar *events-were-on* t)

;;; without-events -- should surround code that should not be affected
;;;  by the Macintosh event system

(defmacro sk8::without-events (&body body)
  `(let ((*events-on* nil))
     (declare (special *events-on*))
     ,@body))

;;;;;;;;;;;;;;;;;;;
;;; Macintosh Menubar
;;;;;;;;;;;;;;;;;;;

(defvar *savembarheight* (%get-word (%int-to-ptr #$mbarheight)))
(defvar *savegrayregion* nil)


;;;;;;;;;;;;
;;; Keyboard
;;;;;;;;;;;;

(defvar *keyMap* (make-record :keyMap))

;;;;;;;;;;;;
;;; Finder startup arguments
;;;;;;;;;;;;
(defvar *files-loaded-from-finder* nil)  ;; to contain a list of files the finder tells us to load


#|
	Change History (most recent last):
	2	6/2/93	kleiman	*deleting-object* special global indicates whether
				we are inside a delete-object operation, in which
				case undisposable children of a disposed object
				should be disposed anyway
	3	6/3/93	kleiman	*closing-project* default should be nil
	4	6/9/93	kleiman	*registry-pheap* new var keeps open pheap for
				documentation registry
	5	6/10/93	kleiman	removed class descriptor globals
	6	8/31/93	kleiman	removed definition of *mbarvisible*
	7	9/20/93	kleiman	new os
	8	9/22/93	kleiman	new d4 os
	9	9/27/93	kleiman	duplicate fixed
	10	9/29/93	kleiman	*macframes-readtable* obsoleted
	11	9/29/93	kleiman	nodes obsoleted
	12	10/1/93	kleiman	obsolete data struct
	13	10/11/93	kleiman	mf::*active-tracks* obsoleted
	14	11/19/93	kleiman	*event-clock-on* has to be defined BEFORE
				08-clock.lisp, so it's been moved to macframes
				globals.lisp and is now in the macframes package
	15	11/22/93	kleiman	obsoleted sk8_oid slot
	16	11/23/93	kleiman	obsoleted *load-project-object*
	17	11/24/93	kleiman	*sk8dev-package*
	18	12/21/93	kleiman	taken out *registry-pheap*
	19	1/7/94	sidney	the great mf/ss consolidation and reorg (ahh!)
	20	1/7/94	sidney	Add mf::*clocks-running*
	21	2/11/94	sidney	Unnamed objects that can be gc'd
	22	2/14/94	sidney	rename children to knownchildren
	24	2/25/94	kleiman	moved defvar *saving-into-project-store* from here to symbol-case-saving.lisp
	25	3/1/94	kleiman	sk8_name -> sk8_id
	26	3/1/94	kleiman	*all-weak-hash-tables*
	27	3/1/94	chip	new globals *Text-object* and *Text-class*
	28	3/1/94	chip	new global *IndirectText-class*
	29	3/1/94	chip	new global *Collection-object*
	30	3/2/94	chip	
	31	3/2/94	chip	new global *object-pseudo-children-and-precedence-lists*
	32	3/7/94	sidney	make *all-weak-hash-tables* a population
	33	3/8/94	till	Weak hash table fix.
	34	3/10/94	chip	obsoleted an unused -class* global
	35	3/16/94	sidney	new defvar, *files-loaded-from-finder
	36	3/18/94	chip	obsoleted *ActorCollection-class* & *EditText-class* globals
	37	3/18/94	chip	obsoleted *IndirectText-class* global
	38	3/28/94	dy	add *macptrs-to-terminate-on-quit*
	39	3/31/94	Hernan	Moving the *clocks-running* variable to MCLcmd
				where do-event is.
	40	6/29/94	chip	moved defvar *saving-into-project-store* back here (don't know what it's for though!!!)
	41 	12/26/94	sidney  	add :skipInitialization keyword to new to support save as text
	1  	 5/ 9/96	sidney  	New object system
	2  	 7/18/96	sidney  	event globals and macro go here, not after they are used in build
	3  	 9/30/96	sidney  	removed some unused ones
	4  	10/21/96	sidney  	removed more unused ones
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
