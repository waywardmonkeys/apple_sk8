;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(provide "APPLEEVENTS")

;;; This is a supported media type that is not in the kernel.

(require "COLORCURSOR" "objects;Media:ColorCursor")

(in-package :SK8DEV)

(SK8-declare-syms :SK8 :public ; Updated  4-22-96   7:47 pm
                  SK8::ACCESSTYPE SK8::ACCESSTYPETAG SK8::AEDESCRIPTOR SK8::AELIST SK8::AERECORD
                  SK8::AETARGET SK8::DATA SK8::OSTYPE SK8::OSTYPESTRING SK8::OSTYPESTRINGLIST
                  SK8::SYMBOLLIST  SK8::TYPETAG SK8::VIRTUALDESCRIPTORTYPE ;;SK8::TARGET 
                  SK8::AETARGET SK8::APPLEEVENT SK8::APPLEEVENTRECEIVER SK8::APPLICATIONNAME SK8::EVENTCLASS
                  SK8::EVENTID SK8::EXTRACTPARAMETER SK8::EXTRACTREPLYPARAMETER SK8::HANDLEAPPLEEVENT
                  SK8::INSERTPARAMETER SK8::INSERTREPLYPARAMETER SK8::SEND SK8::UPDATE-DISPATCH-METHOD)

(SK8-declare-syms :SK8 :private 
                  SK8::the-aedesc SK8::the-reply)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant traps::$typeEnumeration      :|enum|)
  (defconstant traps::$keyReturnIDAttr      :|rtid|)
  (defconstant traps::$typeQDPoint			  :|QDpt|)
  (defconstant traps::$typeQDRectangle		  :|qdrt|)
  ;; !! note: IAC manual says this is right, but some header files indicate it should be cRGB
  ;;(defconstant traps::$typeRGBColor			  :|RGB |)	
  ;; This is a special type we use just in this file to describe an AEDescriptor
  (defconstant traps::$typeDescriptor			  :|desc|)
  (defconstant traps::$typeTargetID			  :|targ|)
  )


(defun make-aedesc ()
  ;;  (make-record (:aedesc :storage :pointer))
  (newRecordGCPtr :aedesc))

;;; _______________________________ 
;;; Media support: passing bits in apple events.
;;; _______________________________ 

(defvar *ae-supported-media* 
  (list QDpicture ColorCursorRSRC ColorPattern BWPattern CursorRSRC SoundRSRC))

(defun restype->media (restype)
  (dolist (c *ae-supported-media*)
    (when (eq restype (ccl::make-keyword (restype c)))
      (return c))))

(defmacro supported-media-p (thing)
  `(find-if #'(lambda (x) (inheritsfrom ,thing x)) *ae-supported-media*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AETarget objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;; An AETarget contains the aedesc for the target object.
(new Object
     :project sk8
     :objectName "AETarget"
     :properties '(OSaetarget))

(setf (private AETarget :property 'OSaetarget) t)


;;; Instead of the above.
(define-handler getFromUser (AETarget &key (multipleValues nil) ((:project inproject) nil) popUpMenu relativeactor)
  (declare (ignore multipleValues inproject popUpMenu relativeactor))
  (setf (OSaetarget me) 
        (appleevents:choose-appleevent-target (make-aedesc))))

;;; Other ways to set the target.
(define-handler (setf applicationName) (value AETarget)
  (setf (OSaetarget me) (appleevents::create-named-process-target (make-aedesc) value))
  me)

;;; Gotta special-case for a bunch of special cases.
(define-handler applicationName (AETarget)
  (with-slots (OSaetarget) me
    (when OSaetarget
      (case (rref OSaetarget :aedesc.descriptortype)
        (#.#$keyProcessSerialNumber
         (with-dereferenced-handles ((h (rref OSaetarget :aedesc.datahandle)))
           (let ((ppcportrec (make-record :PPCPortRec))
                 (process-number (dpb (rref h :processserialnumber.highlongofpsn)
                                      (byte 32 32)
                                      (rref h :processserialnumber.lowlongofpsn))))
             (cond 
              ((= process-number #$kcurrentProcess)
               "ThisApplicationAETarget")
              ((zerop (#_getportnamefromprocessserialnumber ppcportrec h))
               (rref ppcportrec :ppcportrec.name :storage :pointer))))))
        (#.#$typeTargetID
         (with-dereferenced-handles ((h (rref OSaetarget :aedesc.datahandle)))
           (case (rref h :targetid.name.portkindselector :storage :pointer)
             (#.#$ppcByString (rref h :targetid.name.name :storage :pointer)))))))))

(define-handler (setf signature) (value AETarget)
  (setf (OSaetarget me) (appleevents::create-signature-target (make-aedesc) value)))

(define-handler signature (AETarget) )


(define-handler processSerialNumber (AETarget)
  (with-slots (OSaetarget) me
    (when OSaetarget
      (when #$keyProcessSerialNumber (rref OSaetarget :aedesc.descriptortype))
      (with-dereferenced-handles ((h (rref OSaetarget :aedesc.datahandle)))
        (dpb (rref h :processserialnumber.highlongofpsn)
             (byte 32 32)
             (rref h :processserialnumber.lowlongofpsn))))))


(define-handler (setf processSerialNumber) (value AETarget)
  (check-type value integer)
  (setf (OSaetarget me) (appleevents::create-psn-target 
                         (make-aedesc)
                         (ldb (byte 32 32) value)
                         (ldb (byte 32 0) value))))


;;; The AETarget for this very SK8 process.
(new AETarget
     :project SK8
     :objectName "ThisApplicationAETarget"
     :osaetarget (appleevents::create-self-target))

;;; Gotta get a new osaetarget for each new startup.
(define-handler restore (ThisApplicationAETarget)
  (setf (slot-value me 'osaetarget) (appleevents::create-self-target))
  t)

(define-handler preserve (ThisApplicationAETarget)
  (with-slots (osaetarget) me
    (when OSAETarget 
      (dispose-record osaetarget)
      (setf osaetarget nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AppleEvents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(SK8-declare-syms :SK8 :private SK8::the-aedesc SK8::the-reply)
(defvar *ae-return-id* 0)



;;; AppleEvents all require the datatype to be included in all parameter
;;; operations.  For most cases the type is completely obvious.  This routine
;;; is where we handle that.
;;;
;;; If you have more you want to add, this would be a good place for them.
;;;
;;; I need to add something for floating point numbers.

(defun guess-ae-type (val)
  (typecase val 
    (string #$typeChar)
    (null   #$typeNull)
    (fixnum #$typeLongInteger)
    (float  #$typeFloat)
    (list   #$typeAEList)
    (otherwise
     (cond
      ((is-a val AEDescriptor)
       (typeTag-symbol val))
      ((is-a val AETarget)   		#$typeTargetID)
      ((is-a val RGBColor)   		#$typeRGBColor)
      ((supported-media-p val)
       (ccl::make-keyword (restype val)))
      (t
       (error "Can't determine the type for ~s." val))))))


(defun set-parameter (aedesc keyword value &optional datatype)
  (cond 
   ((or (string= datatype "type") (string= datatype "enum"))
    (%stack-block ((data-ptr (record-length :ostype)))
      (%put-ostype data-ptr value)))
   ((string= datatype "TEXT")
    (with-cstrs ((data-ptr value))
      (ae-error
        (#_AEPutParamPtr aedesc keyword datatype data-ptr (length value)))))
   ((string= datatype "null")
    (%stack-block ((data-ptr (record-length :long)))
      (%put-long data-ptr 0)
      (ae-error (#_AEPutParamPtr aedesc keyword datatype data-ptr 0))))
   ((string= datatype "long")
    (%stack-block ((data-ptr (record-length :long)))
      (%put-long data-ptr value)
      (ae-error (#_AEPutParamPtr aedesc keyword datatype data-ptr (record-length :long)))))
   ((string= datatype "bool")
    (rlet ((data-ptr :boolean))
      (%put-boolean data-ptr value)
      (ae-error 
        (#_AEPutParamPtr aedesc keyword datatype data-ptr 1))))
   (t
    (error "We're not set up for datatype ~a" datatype))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Receiving AppleEvents

(new Object :project SK8
     :objectName "AppleEventReceiver"
     :properties '(eventClass eventID))


;;; I discovered the hard way that install-appleevent-handler is 
;;; very picky about ostypes being keywords.  
;;;
;;; This would be a good time to update the dispatch method.
(define-handler (setf eventClass) (value AppleEventReceiver)
  (setf (slot-value me 'eventClass) (assure-OSTypeString value))
  (when (eventID me)
    (update-dispatch-method me))
  value)

(define-handler (setf eventID) (value AppleEventReceiver)
  (setf (slot-value me 'eventID) (assure-OSTypeString value))
  (when (eventClass me)
    (update-dispatch-method me))
  value)

;;; The handler to write AppleEvent Hanndlers on.  A stub.
(define-handler handleAppleEvent (AppleEventReceiver theEvent)
  (declare (ignore me theevent))
  (error "handleAppleEvent needs to be defined"))



;;; Installs a call to the handleAppleEvent
;;;
;;; This needs to get called after changing the eventClass or eventID of an 
;;; AppleEventReceiver. It defines a method to dispatch on, and we gotta 
;;; have that method there.
;;;
(define-handler update-dispatch-method (AppleEventReceiver)
  (install-appleevent-handler 
   (intern (eventClass me) :keyword)
   (intern (eventID me) :keyword)
   #'(lambda (app aedesc reply refcon)
       (declare (ignore refcon app))
       (sk8::handleAppleEvent me (incoming-appleevent aedesc reply))))
  t)


;;; Memory conservation, see incoming-appleevent below.
;; Change these from defvar (in the original AppleEvent implementation)
;; to defparameter, so that they work properly after reloading a saved standalone
(defparameter *received-AETargets* nil)
(defparameter *ae-target-ptr* nil)

;;; For use by the sk8-appleevent-handler only.
;;;
;;; Returns an AppleEvent object given the incoming aedesc.
;;; AppleEvents include AETarget objects, so we attempt to include 
;;; an AETarget object that we've seen here before.  
;;; 
;;; Note the slightly hairy attempt to conserve space by reusing the target
;;; handler dealie.
(defun incoming-appleevent (aedesc reply)
  (unless *ae-target-ptr* (setq *ae-target-ptr* (make-aedesc)))
  (ae-error (#_AEGetAttributeDesc aedesc #$keyAddressAttr #$typeWildCard *ae-target-ptr*))
  (let ((target (car (member *ae-target-ptr* *received-AETargets* 
                             :key #'OSaetarget 
                             :test #'OSaetarget-eq))))
    (unless target
      (setq target (new AETarget :project sk8 :osaetarget *ae-target-ptr*))
      (push target *received-AETargets*)
      (setq *ae-target-ptr* nil))
    (new AppleEvent 
         :project  SK8
         :eventClass (symbol-name (ccl::ae-get-attribute-type aedesc #$keyEventClassAttr))
         :eventID    (symbol-name (ccl::ae-get-attribute-type aedesc #$keyEventIdAttr))
         :target     target
         :the-aedesc aedesc
         :the-reply  reply)))

;;; This is my best attempt to compare two aesdesc-targets to see if they 
;;; refer to the same target or not.  Semi-heroic use of real low level 
;;; stuff.  I know, it's ugly as all hell.
;;;   t   if these are the aetargets
;;;   nil if they're not or we're not sure.
(defun OSaetarget-eq (OSaetarget1 OSaetarget2)
  ;; Yeah, they *could* be bogus objects.
  (unless (or (ccl::bogus-thing-p OSaetarget1)
              (ccl::bogus-thing-p OSaetarget2))
    (or (eq OSaetarget1 OSaetarget2)
        (let ((aet1 (rref OSaetarget1 :aedesc.descriptortype))
              (aet2 (rref OSaetarget2 :aedesc.descriptortype)))
          (when (eq aet1 aet2)
            (case aet1
              (#.#$keyProcessSerialNumber
               ;; compare process serial numbers
               (flet ((process-serial-number (ae)
                        (with-dereferenced-handles ((h (rref ae :aedesc.datahandle)))
                          (dpb (rref h :processserialnumber.highlongofpsn)
                               (byte 32 32)
                               (rref h :processserialnumber.lowlongofpsn)))))
                 (= (process-serial-number OSaetarget1) (process-serial-number OSaetarget2))))
              (#.#$typeTargetID
               (with-dereferenced-handles ((h1 (rref OSaetarget1 :aedesc.datahandle))
                                           (h2 (rref OSaetarget2 :aedesc.datahandle)))
                 ;; We can really only grock ppcByString
                 (and (eq #$ppcByString
                          (rref h1 :targetid.name.portkindselector :storage :pointer))
                      (eq #$ppcByString
                          (rref h2 :targetid.name.portkindselector :storage :pointer))
                      ;; location strings the same?
                      (string= (rref h1 :targetid.name.name :storage :pointer)
                               (rref h2 :targetid.name.name :storage :pointer))
                      (let ((lks1 (rref h1 :targetid.location.locationkindselector :storage :pointer))
                            (lks2 (rref h2 :targetid.location.locationkindselector :storage :pointer)))
                        (when (eq lks1 lks2)
                          (case lks1
                            (#.#$ppcnolocation t)
                            (#.#$ppcNBPLocation
                             (string= (rref h1 :targetid.location.nbpentity.objstr :storage :pointer)
                                      (rref h2 :targetid.location.nbpentity.objstr :storage :pointer)))
                            (#.#$ppcNBPTypeLocation
                             (string= (rref h1 :targetid.location.nbpType :storage :pointer)
                                      (rref h2 :targetid.location.nbpType :storage :pointer)))))))))))))))


;;; For debugging.
#|

(defun describe-aetarget (OSaetarget)
  (if OSaetarget
    (let ((aedt (rref OSaetarget :aedesc.descriptortype)))
      (case aedt
        (#.#$keyProcessSerialNumber
         (with-dereferenced-handles ((h (rref OSaetarget :aedesc.datahandle)))
           (let ((ppcportrec (make-record :PPCPortRec))
                 (process-number (dpb (rref h :processserialnumber.highlongofpsn)
                                      (byte 32 32)
                                      (rref h :processserialnumber.lowlongofpsn))))
             (format t "Process Serial Number: ~d" process-number)
             (when (= process-number #$kcurrentProcess)
               (format t ", Current Process"))
             (when (zerop (#_getportnamefromprocessserialnumber ppcportrec h))
               (format t ", Program: ~a"
                       (rref ppcportrec :ppcportrec.name :storage :pointer))))))
        (#.#$typeTargetID
         (with-dereferenced-handles ((h (rref OSaetarget :aedesc.datahandle)))
           (case (rref h :targetid.name.portkindselector :storage :pointer)
             (#.#$ppcByCreatorAndType 
              (format t " <some bycreatorandtype>}"))
             (#.#$ppcByString
              (format t "Program: ~a," (rref h :targetid.name.name :storage :pointer))
              (case (rref h :targetid.location.locationkindselector :storage :pointer)
                (#.#$ppcnolocation 
                 (format t " Machine: local"))
                (#.#$ppcNBPLocation
                 (format t " Machine: ~a, Zone: ~a"
                         (rref h :targetid.location.nbpentity.objstr :storage :pointer)
                         (rref h :targetid.location.nbpentity.zonestr :storage :pointer)))
                (#.#$ppcNBPTypeLocation
                 (format t " Machine: ~a" 
                         (rref h :targetid.location.nbpType :storage :pointer))))))))
        (#.#$typeSessionID
         (format t " <some session id>"))
        (t 
         (format t "<Unexpected descriptor type>"))))
    (format t "none")))

|#


(defun get-parameter (aedesc keyword &optional type error-if-not-found value-if-not-found)
  (with-aedescs (buffer)
    (let ((err (#_aegetparamdesc aedesc keyword (or type #$typeWildCard) buffer)))
      (case err
        (#.#$errAEDescNotFound
         (when error-if-not-found
           (error "Descriptor ~a no found" keyword))
         value-if-not-found)
        (#.#$noErr
         (let ((return-type        (rref buffer aedesc.descriptortype))
               (return-data-handle (rref buffer aedesc.datahandle)))
           (with-dereferenced-handles ((ptr return-data-handle))
             (case return-type
               (#.#$typeChar 
                (ccl::%str-from-ptr ptr (T_GetHandleSize return-data-handle)))
               ((#.#$typeEnumeration #.#$typeType)
                (%get-ostype ptr))
               (#.#$typeLongInteger
                (%get-signed-long ptr))
               (#.#$typeShortInteger
                (%get-signed-word ptr))
               (t
                (error "Don't know how to deal with type ~a" return-type))))))
        (t
         (error (make-condition 'appleevent-error :oserr err)))))))


#|
;;; A new object called an AppleEventApplication, it includes the 
;;; CLOS class application.
(new Object :project sk8 :objectname "AppleEventApplication")
;;; (mf::add-class-to-object AppleEventApplication 'application)
|#



;-*- Mode: Lisp; Package: SK8DEV -*-
;;***************************************************************************
;;***************************************************************************
;; 
;; Title        : Apple Event Object Support Library for SK8
;; 
;;***************************************************************************
;;***************************************************************************
;; Author       : Chris DiGiano
;; Copyright    : This code is work derived from part of the SK8 system which
;;					 : is a copyright of Apple Computer, Inc. Pending an agreement
;;              : between the author and Apple, neither party currently has
;;              : full rights to this code.
;; Address      : University of Colorado at Boulder
;;              : Computer Science Department
;;              : CB #430
;;              : Boulder,Colorado 80303-430
;;              : digi@cs.colorado.edu
;;***************************************************************************
;; Filename     : SK8-AEOSL.lisp
;; Version      : 1.1a1
;; 
;; Abstract     : This module replaces the SK8 AppleEvent object with one built
;;					 : on top of a hierarchy of SK8 objects corresponding to the
;;              : the standard AppleEvent data structures for the MacOS.
;;              : In addition to this rewritten AppleEvent object, this module
;;              : provides an AEDescriptor, AEList, and AERecord. AELists,
;;              : AERecords, and AppleEvents can be assigned SK8 data of various
;;              : kinds which are automatically translated into appropriate
;;              : AppleEvent descriptors for the operating system. One can 
;; 				 : also at custom conversion routines in Lisp, or one can associate
;;					 : custom children of AEDescriptor which have their own custom
;;              : type identifier.
;;
;;              : Finally, this module adds the ability to extract the 
;;					 : signature from an AETarget.
;;
;; Credits      : Thanks to the SK8 team for the original AppleEvent 
;;					 : implementation and to Steve Ritter for the idea of 
;;  				 : an extensible conversion table.
;;
;; Bugs         : 
;; 
;; Todo         : with-std-ae-type should be done at a lower level!
;; 				 : Extracting an AERecord from an AERecord should by default
;;					 : return a SK8 Table.
;;					 : Consider whether insert/extract protection is also necessary
;;					 : for VirtualDescriptorTypes.
;;***************************************************************************



;; These symbols need to be provided to all descendants of the SK8 project for indicating the
;; reply mode to AppleEvents, but should not be declared by SK8-declare-syms, since this won't
;; attempt to import them into SK8DEV which already has some of them defined differently.
;;
(publishSymbol SK8 'SK8::none)
(publishSymbol SK8 'SK8::queue)
(publishSymbol SK8 'SK8::wait)



;; Hash tables are used to maintain a mapping between special custom types and the
;; functions which allow them to be stored and retrieved from Apple Events. 
;; A hash table entry consists of a fn, object tuple where the object is either
;; nil or a child of AEDescriptor. A nil object indicates that any caller to 
;; deinstall may get rid of the entry. On the other hand, if any owner object is
;; provided, only owners are allowed to remove entries. 
;; This is done to prevent a child of the object (who inherits the customType) from 
;; accidentally removing the entry when the child wants to have an even more
;; specific custom type.
;;
;; This idea borrowed from Steven Ritter, sritter@cmu.edu
;;
(defparameter *ae-extract-conversion-table* (make-hash-table :test #'eq))
(defparameter *ae-insert-conversion-table* (make-hash-table :test #'eq))

;; install-extract-conversion is used to store a way to convert a value returned from an
;; AppleEvent (via extract*) to a SK8 type. We use this for non-standard types.
;;
(defun install-extract-conversion (type function &optional owner)
  (setf (gethash type *ae-extract-conversion-table*) (list function owner)))

(defun deinstall-extract-conversion (type &optional requester)
  (let ((entry (gethash type *ae-extract-conversion-table*)))
    (when (and entry (or (null (second entry)) (eq (second entry) requester)))
      (remhash type *ae-extract-conversion-table*))))

(defun get-extract-conversion (type)
  (first (gethash type *ae-extract-conversion-table*)))

;; install-insert-conversion is the same, but it converts between a SK8 type
;; and an external type
;;
(defun install-insert-conversion (type function &optional owner)
  (setf (gethash type *ae-insert-conversion-table*) (list function owner)))

(defun deinstall-insert-conversion (type &optional requester)
  (let ((entry (gethash type *ae-insert-conversion-table*)))
    (when (and entry (or (null (second entry)) (eq (second entry) requester)))
      (remhash type *ae-insert-conversion-table*))))

(defun get-insert-conversion (type)
  (first (gethash type *ae-insert-conversion-table*)))



(defun valid-ostype (type &key (default nil))
  (cond
   ((is-a type OSTypeString)
    (intern type :keyword))
   ((is-a type AEDescriptor)
    (valid-ostype (typeTag type)))
   ((is-a type VirtualDescriptorType)
    (valid-ostype (typeTag type)))
   ((and (null type) default)
    (valid-ostype default))
   ((and type (symbolp type))
    (is-a (symbol-name type) OSTypeString)
    type)
   ((eq type String)
    #$typeChar)
   ((or (eq type Integer) (eq type BigInteger) (eq type SmallInteger))
    #$typeLongInteger)
   ((eq type Real)
    #$typeFloat)
   ((eq type List)
    #$typeAEList)
   ((eq type EnumeratedType)            ; !! kind of a hack
    #$typeEnumerated)
   ((eq type PointList)
    #$typeQDPoint)
   ((eq type BoundsRectList)
    #$typeQDRectangle)
   ((restype->media type)
    (ccl::make-keyword (restype (restype->media type))))   
   ((eq type RGBColor)
    #$typeRGBColor)))

(defun ensure-valid-ostype (type &rest key-args &key default)
  (declare (ignore default))
  (or (apply #'valid-ostype type key-args) 
      (error "Can't make ~s a valid ostype." type)))

;;
;; OSTypeString
;;
;; A virtual type used to identify four character OS types.

(new VirtualType
     :objectName "OSTypeString"
     :project SK8)

(define-handler typeSatisfied (OSTypeString obj)
  (and (stringp obj) (= 4 (length obj))))

;;
;; OSTypeStringList
;;

(new VirtualType 
     :objectName "OSTypeStringList"
     :project SK8)

(define-handler typeSatisfied (OSTypeStringList obj)
  (when (is-a obj List)
    (cond
     ((is-a (first obj) OSTypeString)
      (typeSatisfied me (rest obj)))
     ((null obj)
      t) 
     (t
      nil))))


;;
;; ForeignMemory
;;

(new VirtualType 
     :objectName "ForeignMemory"
     :project SK8)

(define-handler typeSatisfied (ForeignMemory obj)
  (macptrp obj))






;;
;; VirtualDescriptorType
;;

(new VirtualType
     :objectName "VirtualDescriptorType"
     :properties '(accessType typeTag)
     :project SK8)

(define-system-handler install-extract-insert (VirtualDescriptorType 
                                                   type-tag
                                                   access-type
                                                   &key old-type-tag)
  (when (and type-tag access-type)
    (when old-type-tag
      (deinstall-extract-conversion (intern old-type-tag :keyword))
      (deinstall-insert-conversion (intern old-type-tag :keyword)))
    (setq type-tag (intern type-tag :keyword)
          access-type (intern access-type :keyword)))
  (install-extract-conversion 
   type-tag
   #'(lambda (aedesc data-ptr data-size proj)
       (get-from-ae-storage 
        aedesc
        access-type
        data-ptr
        data-size
        proj)))
  ;; To insert a virtual desc, we must package up the descriptor with the virtual type tag
  ;; using a standard access type.
  (install-insert-conversion 
   type-tag
   #'(lambda (desc accessor access-method value type)
       (declare (ignore type))
       (if (eq access-method 'whole-descriptor)
         ;; If we're replacing a descriptor en masse, simply recurse with its primitive storage type
         (put-into-ae-storage desc access-type access-method value type-tag)
         ;; Otherwise, we insert the value into a temporary descriptor with the proper type tag
         ;; and then pass the temporary descriptor into ae-put-descriptor.
         (with-aedescs (special-tagged-desc) 
           (put-into-ae-storage special-tagged-desc 
                                access-type
                                'whole-descriptor
                                value
                                type-tag)
           (ae-put-descriptor desc accessor access-method special-tagged-desc))))))

(define-handler (setf accessType) (value VirtualDescriptorType)
  (unless (valid-ostype value)
    ;; !! need a more appropriate virtual type here
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType OSTypeString
               :ownerObject me
               :propertyName 'accessType))
  ;; Since callers can pass SK8 types such as Integer, we must first
  ;; convert it its proper typestring.
  (let ((type-string (symbol-name (valid-ostype value))))
    (install-extract-insert me (typeTag me) type-string)
    (setValue 'accessType me type-string)
    type-string))

(define-handler (setf typeTag) (value VirtualDescriptorType)
  (unless (is-a value OSTypeString)
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType OSTypeString
               :ownerObject me
               :propertyName 'typeTag))
  (install-extract-insert me value (accessType me) :old-type-tag (typeTag me))
  (setValue 'typeTag me value)
  value)

;; Since virtual don't do standard type checking,
;; we simulate it here in case someone wants to know
;; if an object is a child of VirtualDescriptorType
;;
(define-handler typeSatisfied (VirtualDescriptorType obj)
  (and (is-a obj Object)
       (contains (parents obj) me)))


;;
;; SymbolList
;;

(new VirtualType
     :objectName "SymbolList"
     :project SK8)

(define-handler typeSatisfied (SymbolList obj)
  (when (is-a obj List)
    (cond
     ((null obj)
      t)
     ((is-a (first obj) Symbol)
      (typeSatisfied me (rest obj))) 
     (t
      nil))))


;;
;; AEDescriptor
;;

(new Object
     :project sk8
     :objectName "AEDescriptor"
     :properties '(aedesc
                   accessType))     ; optionally specifies the way to put/get data

(define-handler initialize (AEDescriptor original isNew initArgs) 
  (declare (ignore isNew))
  ;; !! Wow, we had two call-next-methods here. I guess this one should go.
;;  (call-next-method)
  ;; To remain backwards compatible, an AEDescriptor can be given 
  ;; an appleevent descriptor through the initarg the-aedesc.
  ;; But, SK8 callers can't use symbols with dashes in them.
  ;; So, we provide the initarg 'aedesc' as the official and public
  ;; way to assign an aedesc. A public way is important, since
  ;; we sometimes need to create new AEDescriptors or descendants
  ;; for public projects.
  (let ((aedesc-initArg (or (initializerArgument initArgs 'SK8::the-aedesc)
                            (initializerArgument initArgs 'aedesc))))
    (make-aedesc-property me aedesc-initArg initArgs)
    (init-aedesc-property me original (aedesc me) aedesc-initArg initArgs)
    (call-next-method)))

(define-handler addedMeAsParent (AEDescriptor child oldParents)
  (declare (ignore oldParents))
  (make-aedesc-property child nil nil)
  (init-aedesc-property child me (aedesc child) nil nil))

;; Creates a descriptor record and sets our aedesc property to it
;; if the caller has provided a value for the-aedesc, this is used instead.
;;
(define-system-handler make-aedesc-property (AEDescriptor aedesc-initArg initArgs)
  (declare (ignore initArgs))
  (setf (aedesc me) (or aedesc-initArg (make-aedesc))))

;; Establishes the contents of aedesc, if necessary.
;;
(define-system-handler init-aedesc-property (AEDescriptor 
                                                 original
                                                 my-aedesc
                                                 aedesc-initArg
                                                 initArgs) 
  (unless aedesc-initArg
    ;; We need to extract a bunch of initargs here, so we can order things properly
    (let ((data-initArg (initializerArgument initArgs 'data :use t))
          (typeTag-initArg (initializerArgument initArgs 'typeTag :use t))
          (accessType-initArg (initializerArgument initArgs 'accessType :use t)))
      (cond
       (data-initArg
        ;; Ensure the type and accessType are established prior to setting the data        
        (when typeTag-initArg
          (setf (typeTag me :coercion nil) typeTag-initArg))
        (when accessType-initArg
          (setf (accessType me) accessType-initArg))
        (if typeTag-initArg
          (setf (data me :preserveTypeTag t) data-initArg)
          (setf (data me) data-initArg)))
       ;; If there's a prototype, duplicate all the data in the original.
       ;; This includes all key fields in an AERecord.
       (original
        ;; !! seems like when restoring a SK8 project the original may not have been restored yet,
        ;; so we force it here, before copying it.
        (unless (is-a (aedesc original) ForeignMemory)
          (restore original))
        (#_AEDuplicateDesc (aedesc original) my-aedesc)
        (when typeTag-initArg
          (setf (typeTag me :coercion nil) typeTag-initArg))
        (when accessType-initArg
          (setf (accessType me) accessType-initArg)))
       (t 
        (setf (data me) nil)
        (when typeTag-initArg 
          (setf (typeTag me :coercion nil) typeTag-initArg))
        (when accessType-initArg 
          (setf (accessType me) accessType-initArg)))))))


;; Provides quick access to the type tag as Lisp symbol.
;;
(define-handler typeTag-symbol (AEDescriptor)
  (rref (aedesc me) aedesc.descriptortype))

;; Returns the type tag as a string
;;
(define-handler typeTag (AEDescriptor)
  (symbol-name (typeTag-symbol me)))


;; Changes the type tag for this descriptor.
;;
(define-handler (setf typeTag) (value AEDescriptor &key (coercion t) errorIfCantCoerce) 
  (setq value (ensure-valid-ostype value))
  (if coercion
    (let* ((new-aedesc (make-aedesc))
           (err (#_AECoerceDesc (aedesc me) value new-aedesc)))
      (cond
       ((= err #$noErr)
        (setf (aedesc me) new-aedesc))
       ((and (= err #$errAECoercionFail) (null errorIfCantCoerce))
        (setf (rref (aedesc me) aedesc.descriptortype) value))
       (t 
        (ae-error err))))
    (setf (rref (aedesc me) aedesc.descriptortype) value))
  (symbol-name value))

;; Changes the type that will be used for storing and extracting data in this descriptor.
;;
(define-handler (setf accessType) (value AEDescriptor) 
  (unless (valid-ostype value)
    ;; !! need a more appropriate virtual type here
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType OSTypeString
               :ownerObject me
               :propertyName 'accessType))
  (setValue 'accessType me value))

;; Returns the descriptor from which data is to be gotten.
;; If the access-method is 'whole-descriptor, returns a copy of the original descriptor passed.
;; Otherwise, does the extract for the accessor based on the access-method.
;; !! note: making a copy of the descriptor is a bit wasteful, but it helps make descriptor
;; access simpler and more consistent. Besides, this should be a rare case, since most of
;; the time, we can expect descriptors to be embedded within other structures.
;;
(defun ae-get-descriptor (desc accessor access-method type buffer)
  (ecase access-method
    (whole-descriptor
     (#_AEDuplicateDesc desc buffer))
    (index
     (rlet ((key :ostype))
       (#_AEGetNthDesc desc accessor type key buffer)))
    (key
     (#_AEGetKeyDesc desc accessor type buffer))
    ;; !! kinda a hack here for getting the keyword
    ;; first, we call nth which fills the record 'key'
    ;; then, we clear the buffer, since we don't care about the actual value
    ;; finally, we store the 'key' in a descriptor record so it can be
    ;; processed normally upon return
    ((key-index)
     (rlet ((key :ostype))
       (ae-error (#_AEGetNthDesc desc accessor type key buffer))
       (ae-error (#_AEDisposeDesc buffer))
       (#_AECreateDesc #$typeKeyword key (record-length :ostype) buffer)))
    ((parameter-key nil)
     (#_AEGetParamDesc desc accessor type buffer))
    (attribute-key 
     (#_AEGetAttributeDesc desc accessor type buffer))))  

(defun create-descriptor-object (desc prototype proj)
  (let ((new-desc (make-aedesc)))
    (ae-error 
      (#_AEDuplicateDesc desc new-desc))
    (new prototype
         :aedesc new-desc
         :project proj)))

(defun create-color-object (red-component green-component blue-component proj)
  (or (find-if #'(lambda (Color) 
                   (and (= (foreRed Color) red-component)
                        (= (foreGreen Color) green-component)
                        (= (foreBlue Color) blue-component)
                        (not (is-a Color ComplexRGBColor)))) 
               (knownChildren RGBColor))
      (new RGBColor :foreRed red-component :foreGreen green-component :foreBlue blue-component
           :project proj)))

(defun extract-file-from-alias-desc (desc proj)
  (rlet ((file-spec :fsspec)
         (was-changed :boolean))
    (let ((err (#_resolvealias (%null-ptr) (rref desc :aedesc.datahandle) file-spec was-changed)))
      (if (= err #$noerr)
        (let ((path (%path-from-fsspec File-Spec)))
          (New File :physicalName (namestring path) :project proj))
        (error err)))))

;; Attempts to extract an standard internal data value from a descriptor given a pointer to
;; its data and the size of the data. The 'type' parameter specifies how the extraction should
;; take place. The 'proj' parameter indicates the project into which new objects should be 
;; created, if necessary.
;; Unknown descriptor types are automatically extracted as AEDescriptors, unless the 
;; errorIfNoExtractor key parameter is set to True.
;;
(defun get-from-ae-storage (desc type data-ptr data-size proj &key (errorIfNoExtractor nil))
  (case type
    (#.#$typeBoolean 
     (%get-boolean data-ptr))
    (#.#$typeChar 
     (ccl::%str-from-ptr data-ptr data-size))
    ((#.#$typeEnumerated #.#$typeType)
     (symbol-name (%get-ostype data-ptr)))
    (#.#$typeLongInteger
     (%get-signed-long data-ptr))
    (#.#$typeShortInteger
     (%get-signed-word data-ptr))
    (#.#$typeFloat
     (%copy-float data-ptr))
    (#.#$typeAEList
     (let ((result (list))
           (count (rlet ((count-ptr :signed-long))
                    (ae-error (#_AECountItems desc count-ptr))
                    (%get-signed-long count-ptr))))
       (dotimes (i count)
         (push (get-data-item desc (- count i) 'index :errorIfNotFound t :project proj)
               result))
       result))
    (#.#$typeAERecord
     (create-descriptor-object desc AERecord proj))
    (#.#$typeAppleEvent
     (create-descriptor-object desc AppleEvent proj))
    (#.#$typeKeyword 
     (symbol-name (%get-ostype data-ptr))) 
    (#.#$typeRGBColor
     (create-color-object (rref data-ptr :RGBColor.red)
                          (rref data-ptr :RGBColor.green)
                          (rref data-ptr :RGBColor.blue)
                          proj))
    (#.#$typeProcessSerialNumber
     (%get-signed-long data-ptr))
    (#.#$typeQDPoint
     (let ((point (ccl::%get-point data-ptr)))
       (list (point-h point) (point-v point))))
    (#.#$typeQDRectangle
     (list (rref data-ptr :rect.left) (rref data-ptr :rect.top)
           (rref data-ptr :rect.right) (rref data-ptr :rect.bottom)))
    (#.#$typeAlias (extract-file-from-alias-desc desc proj))
    (#.#$typeNull
     nil) 
    (t 
     (let ((mediafortype (restype->media type)))
       (if mediafortype
         (importfromptr mediafortype data-ptr data-size proj)
         (let ((func (get-extract-conversion type)))
           (cond
            (func
             (funcall func desc data-ptr data-size proj))
            (errorIfNoExtractor
             (error "Unrecognized type to get-from-ae-storage: ~s~%."
                    type))
            (t
             (create-descriptor-object desc AEDescriptor proj)))))))))


(defun maybe-convert-to-sk8 (value type proj)
  (cond
   ;; !! for now, just return a record
   ((is-a value AERecord)
    value)
   #| !! to do:
((and (is-a value AERecord) (neq type #$typeAERecord))
    (data value))
|#
   ;; note, this must come after the AERecord check
   ;; otherwise, it will pass the is-a AEList predicate
   ((and (listp value)
         (eq type #$typeAEList))
    (new AEList
         :data value
         :project proj))
   #|
((and (is-a value AEList)
         (neq type #$typeAEList))
    (data value))
|#
   
   (t
    value)))

;; Extracts a piece of data from a descriptor using the access method indicated
;; by the accessor and access-method arguments.
;; This function is based on a similarly strange function in the original 
;; implementation of SK8 AppleEvents.
;;
;; Here's how it works: 
;; First, we extract the data item as a descriptor using the function ae-get-descriptor.
;; This makes sense for items such as AppleEvent attributes and AERecord fields,
;; but not necessarily for when we're trying to extract, say, an integer from
;; an AEDescriptor. The reason this is done for all types, including AEDescriptors,
;; is that it gets the result into a standard form, a descriptor, in preparation
;; for translating the value into a SK8 type. The next step in this function, then,
;; is to identify the type and size of the data in this new descriptor
;; and pass all the information to get-from-ae-storage which does the 
;; translation to a SK8 value.
;;
;; The access-method parameter governs how the accessor is interpreted according
;; to the table below.
;; ACCESS-METHOD		ACCESSOR				USES
;; whole-descriptor		an ostype					Specifies the method for extracting the 
;;						contents of an AEDescriptor's data.
;; index					an integer					Specifies the position for the nth item
;;						of an AEList which is to be extracted
;; key					an ostype					Specifies the keyword for the data item
;;						of an AERecord which is to be extracted
;; attribute-key			an ostype					Specifies the keyword for the attribute
;;						of an AppleEvent which is to be extracted
;; parameter-key			an ostype					Specifies the keyword for the parameter
;;						of an AppleEvent which is to be extracted
;;
(defun get-data-item (desc accessor access-method &key
                              type
                              (errorIfNotFound t)
                              (valueIfNotFound nil)
                              ((:project p) (targetProject UI)))
  (when type
    (setq type (ensure-valid-ostype type)))
  (with-aedescs (buffer)
    (let* ((err (ae-get-descriptor desc
                                   accessor
                                   access-method
                                   #$typeWildCard
                                   buffer)))
      (case err
        (#.#$errAEDescNotFound
         (when errorIfNotFound
           (error "Descriptor ~a not found." accessor))
         valueIfNotFound)
        (#.#$noErr 
         (let ((data-handle (rref buffer aedesc.datahandle))
               (data-size 0))
           (unless (%null-ptr-p data-handle)
             (setq data-size (T_GetHandleSize data-handle)))
           (with-dereferenced-handles ((data-ptr data-handle))
             (maybe-convert-to-sk8 
              (get-from-ae-storage buffer 
                                   (if (eq access-method 'whole-descriptor)
                                     accessor
                                    (or type (rref buffer aedesc.descriptortype)))
                                   data-ptr
                                   data-size
                                   p)
              type
              p))))
        (otherwise
         (ae-error err))))))

(defmacro with-standard-ae-type (me  &body body)
  `(let ((saved-type (typeTag-symbol ,me))
         (dat (valid-ostype (accessType ,me))))
     (cond
      ((and dat (neq saved-type dat))
       (setf (typeTag me) dat)
       (unwind-protect
         (progn 
           ,@body)
         (setf (typeTag me) saved-type)))
      (t
       (progn
         ,@body)))))
#|
(defmacro with-standard-ae-type (me  &body body)
  `(let ((saved-type (customType ,me)))
     (if saved-type
       (unwind-protect 
         (progn
           (setf (rref (aedesc ,me) :aedesc.descriptortype) (ae-standard-type ,me))
           ,@body)
         (setf (rref (aedesc ,me) :aedesc.descriptortype) saved-type))
       (progn
         ,@body))))
|#

;; Extracts the contents of this AEDescriptor's data.
;; The keyword argument 'type' specifies how to extract the data. If no type 
;; parameter is provided, the accessType of the descriptor will be used.
;; As a last resort, the current type-tag for the descriptor will be used.
;; The keyword argument 'project' indicates the project to use in the
;; event the extraction produces a new SK8 object.
;;
(define-handler data (AEDescriptor &key type ((:project p)))
  (let ((storage-type (or (valid-ostype type)
                          (valid-ostype (accessType me))
                          (rref (aedesc me) aedesc.descriptortype))))
    (get-data-item (aedesc me)
                 storage-type
                 'whole-descriptor
                 :type storage-type         ; ignored
                 :project (or p (project me)))))
  
;; Installs a pointer to a block of data as an item in this descriptor.
;; If the access-method is 'whole-descriptor, the pointer data replaces the entire contents
;; of the descriptor.
;;
(defun ae-put-pointer (desc accessor access-method type-id data-ptr data-size)
  (ae-error
    (ecase access-method
      (whole-descriptor
       (#_AECreateDesc type-id data-ptr data-size desc))
      (index
       (#_AEPutPtr desc accessor type-id data-ptr data-size))
      (key
       (#_AEPutKeyPtr desc accessor type-id data-ptr data-size))
      (parameter-key
       (#_AEPutParamPtr desc accessor type-id data-ptr data-size))
      (attribute-key
       (#_AEPutAttributePtr desc accessor type-id data-ptr data-size)))))

;; Embeds an ae descriptor in this descriptor.
;;
(defun ae-put-descriptor (desc accessor access-method desc-to-put)
  (rref desc-to-put :aedesc.descriptortype)
  (ae-error
    (ecase access-method
      (whole-descriptor
       (error "Can't embed an AEDescriptor, AEList, AERecord, or AppleEvent inside an AEDescriptor."))
      (index
       (#_AEPutDesc desc accessor desc-to-put))
      (key
       (#_AEPutKeyDesc desc accessor desc-to-put))
      (parameter-key
       (#_AEPutParamDesc desc accessor desc-to-put))
      (attribute-key
       (#_AEPutAttributeDesc desc accessor desc-to-put)))))

;; Adds a piece of data to a descriptor using the access method indicated
;; by the accessor and access-method arguments.
;; This is the lower-level routine called by put-data-item. Unlike put-data-item,
;; this function does no type inferencing.
;; The access-method parameter governs how the accessor is interpreted according
;; to the table below.
;; ACCESS-METHOD		ACCESSOR				USES
;; whole-descriptor		an ostype					Specifies the method for replacing the 
;;						contents of an AEDescriptor's data.
;; index					an integer					Specifies the position for the nth item
;;						of an AEList which is to be replaced.
;; key					an ostype					Specifies the keyword for the data item
;;						of an AERecord which is to be replaced.
;; attribute-key			an ostype					Specifies the keyword for the attribute
;;						of an AppleEvent which is to be replaced.
;; parameter-key			an ostype					Specifies the keyword for the parameter
;;						of an AppleEvent which is to be replaced.
;;
(defun put-into-ae-storage (desc accessor access-method value type)
  (case (if (eq access-method 'whole-descriptor)
          accessor
          type)
    (#.#$typeBoolean
     (rlet ((data-ptr :boolean))
       (%put-boolean data-ptr value)
       (ae-put-pointer desc accessor access-method type data-ptr 1)))
    (#.#$typeChar
     (with-cstrs ((data-ptr value))
       (ae-put-pointer desc accessor access-method type data-ptr (length value))))
    (#.#$typeLongInteger
     (%stack-block ((data-ptr (record-length :long)))
       (%put-long data-ptr value)
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :long))))
    ((#.#$typeEnumerated #.#$typeType)
     (%stack-block ((data-ptr (record-length :ostype)))
       (%put-ostype data-ptr value)
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :ostype))))
    ;; !! float kludge based on MCL example file DefAppleEvents
    ;; We convert a string version of the float to an aedesc using AECoerceDesc
    ;; Unfortunately, we have to handle whole-descriptor putting differently from 
    ;; descriptor-item putting.
    (#.#$typeFloat
     (with-aedescs (string-desc float-desc)
       (put-into-ae-storage string-desc
                            #$typeChar
                            'whole-descriptor
                            (format nil "~f" value)
                            #$typeChar)
       (cond
        ((eq access-method 'whole-descriptor)
         (ae-error (#_AECoerceDesc string-desc #$typeFloat desc)))
        (t
         (ae-error (#_AECoerceDesc string-desc #$typeFloat float-desc))
         (ae-put-descriptor desc accessor access-method float-desc)))))
    ((#.#$typeDescriptor #.#$typeAEList #.#$typeAERecord #.#$typeAppleEvent)
     (ae-put-descriptor desc accessor access-method (aedesc value)))
    (#.#$typeRGBColor
     (rlet ((data-ptr :rgbcolor
                      :red (foreRed value)
                      :green (foreGreen value)
                      :blue (foreBlue value)))
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :RGBColor))))
    (#.#$typeProcessSerialNumber
     (%stack-block ((data-ptr (record-length :long)))
       (%put-long data-ptr value)
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :long))))
    (#.#$typeQDPoint
     (%stack-block ((data-ptr (record-length :point)))
       (ccl::%put-point data-ptr (make-point (round (first value)) 
                                             (round (second value)))) 
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :point))))
    (#.#$typeQDRectangle
     (rlet ((data-ptr :rect 
                      :topleft (make-point (round (first value)) 
                                           (round (second value)))
                      :bottomright (make-point (round (third value)) 
                                               (round (fourth value)))))
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :rect))))
    (#.#$typeTargetID
     (ae-put-descriptor desc accessor access-method (osaetarget value)))
    (#.#$typeNull
     (%stack-block ((data-ptr (record-length :long)))
       (%put-long data-ptr 0)
       (ae-put-pointer desc accessor access-method type data-ptr (record-length :long))))
    (#.#$typeWildCard
     (put-into-ae-storage desc accessor access-method value (guess-ae-type value))) 
    (t 
     (if (supported-media-p value)
       (sk8-multival-bind (data-ptr data-size) (exporttoptr value)
         (ae-put-pointer desc accessor access-method type data-ptr data-size))
       (let ((func (get-insert-conversion type)))
         (if func
           (funcall func desc accessor access-method value type)
           (error "Unrecognized type to put-into-ae-storage: ~s~%."
                  type)))))))


#| !! to do:
((and (is-a value Table) (or (eq type #$typeAERecord)
                               (eq type #$typeWildCard)
                               (null type)))
    (new AERecord
         :data value
         :project SK8))
|#
(defun maybe-convert-from-sk8 (value type)
  (cond
   ((and (or (is-a value List)
             (and (is-a value Vector) (not (is-a value String))))
         (or (eq type #$typeAEList)
             (eq type #$typeWildCard)
             (null type)))
    (new AEList
         :data (asType List value)
         :project SK8))

   (t
    value)))

;; Adds a piece of data to a descriptor using the access method indicated
;; by the accessor and access-method arguments.
;; First tries to determine the intended type from either an explicit 'type'
;; argument or else by guessing the intended type from the data argument itself.
;; Next this function passes control on to put-into-ae-storage.
;; See put-into-ae-storage for legal accessor and access-method parameters.
;;
(defun put-data-item (desc accessor access-method data &key type)
  (setq type (valid-ostype type))
  (let ((storage-type (cond
                       ; Use the given type as long as it's not wild card.
                       ((and type (neq type #$typeWildCard))
                        type)
                       ; Mark AEDesc's specially.
                       ((is-a data AEDescriptor)
                        #.#$typeDescriptor)    ;;###
                       ; If we don't know the type at this point, guess it.
                      (t       
                        (guess-ae-type data)))))
    (put-into-ae-storage desc
                         accessor
                         access-method
                         (maybe-convert-from-sk8 data storage-type)
                         storage-type))
  data)

;; Assigns the contents of the descriptor to a new value.
;; The keyword parameter 'type' can be used to explicity specify how it
;; should be stored, otherwise the type is guessed from the new value.
;; If the AEDescriptor has been assigned a accessType, this
;; will be used to specify the method for storing the new data.
;;
(define-handler (setf data) (value AEDescriptor &key type preserveTypeTag) 
  (let ((new-tag (cond
                  (preserveTypeTag
                   (typeTag-symbol me))
                  (type
                   (valid-ostype type))
                  (t
                   (guess-ae-type value)))))
    (put-into-ae-storage (aedesc me)
                         (or (valid-ostype type)
                             (valid-ostype (accessType me))
                             new-tag)
                         'whole-descriptor
                         value
                         new-tag)
    value))

(defun delete-data-item (desc accessor access-method &key 
                                 (errorIfNotFound t))
  
  (let ((err (if accessor
               (ecase access-method
                 ((parameter-key nil)
                  (#_AEDeleteParam desc accessor))
                 (key
                  (#_AEDeleteKeyDesc desc accessor))
                 (index
                  (#_AEDeleteItem desc accessor)))
               ;; !! dangerous
               (#_AEDisposeDesc desc))))
    (case err
      (#.#$errAEDescNotFound
       (when errorIfNotFound
         (error "Descriptor ~a not found." accessor)))
      (#.#$noErr 
       nil)
      (otherwise
       (ae-error err)))))

;; ensure that even the prototype has an aedesc
(make-aedesc-property AEDescriptor nil nil)
(init-aedesc-property AEDescriptor nil (aedesc AEDescriptor) nil nil)

;; Gotta get a new aedesc for each new startup.
;; !! Unfortunately, the old data is lost. An the object
;; reverts to its default empty state.
;;
(define-handler restore (AEDescriptor)
  (call-next-method)
  (unless (is-a (aedesc me) ForeignMemory)
    (make-aedesc-property me nil nil)
    (init-aedesc-property me nil (aedesc me) nil nil)))

;; Flushes the one memory handler associated with this object.
;;
(define-handler preserve (AEDescriptor)
  (call-next-method)
  (setf (aedesc me) nil))

;; Allows a bunch of descriptor changes to take place using a standard access method
;; without changing the coercing the type to a standard type for each and every
;; descriptor access call within the body.
;;
(defmacro |WITH ACCESSTYPETAG| ((descObject) &body body)
  (let ((savedtypevar (gensym)))
    `(let ((,savedtypevar (typeTag ,descObject)))
       (unwind-protect
         (progn
           (setf (typeTag ,descObject) (accessType ,descObject))
           ,@body)
         (setf (typeTag ,descObject) ,savedtypevar)))))


;;
;; AEList
;;

(new AEDescriptor
     :accessType (symbol-name #$typeAEList)
     :objectName "AEList"
     :project sk8)

(define-system-handler init-list-aedesc (AEList aedesc isRecord)
  (ae-error
    (#_AECreateList 
     (ccl::%null-ptr)                 ; no factoring
     0                                ; no factoring
     isRecord                         ; t for a AERecord, nil for an AEList
     aedesc)))

(define-system-handler init-aedesc-property (AEList 
                                                 original
                                                 my-aedesc
                                                 aedesc-initArg
                                                 initArgs)
  (unless aedesc-initArg
    (let ((data-initArg (initializerArgument initArgs 'data :use t)))
      ;; Fill the data, either from the keyarg or from the original.
      (cond
       (data-initArg
        (init-list-aedesc me my-aedesc nil)
        (setf (data me) data-initArg))
       (original
        ;; !! seems like when restoring a SK8 project the original may not have been restored yet,
        ;; so we force it here, before copying it.
        (unless (is-a (aedesc original) ForeignMemory)
          (restore original))
        (#_AEDuplicateDesc (aedesc original) my-aedesc))
       (t
        (init-list-aedesc me my-aedesc nil))))))

(define-handler data (AEList &key 
                              type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (let (result
        (count (countItems me)))
    (dotimes (i count)
      (push (extractNthItem me
                            (- count i)
                            :type type
                            :errorIfNotFound errorIfNotFound
                            :valueIfNotFound valueIfNotFound
                            :project (or p (project me)))
            result ))
    result))

(define-handler (setf data) (value AEList)
  (unless (listp value)
    (error "The data for an AEList must be a list."))
  (ae-error (#_AEDisposeDesc (aedesc me)))
  (make-aedesc-property me nil nil)
  (init-aedesc-property me nil (aedesc me) nil nil)
  (with-standard-ae-type me
    (dotimes (i (length value))
      (put-data-item (aedesc me) (1+ i) 'index (nth i value))))
  value)

(define-handler countItems (AEList)
  (with-standard-ae-type me
    (rlet ((count-ptr :signed-long))
      (ae-error (#_AECountItems (aedesc me) count-ptr))
      (%get-signed-long count-ptr))))

(define-handler extractNthItem (AEList n &key
                                          type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (with-standard-ae-type me
    (get-data-item (aedesc me) 
                   n
                   'index
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler extractNthKey (AEList n &key
                                         type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (with-standard-ae-type me
    (get-data-item (aedesc me)
                   n
                   'key-index
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler deleteNthItem (AEList n &key (errorIfNotFound t))
  (with-standard-ae-type me
    (delete-data-item (aedesc me)
                      n
                      'index
                      :errorIfNotFound errorIfNotFound)))

;; here's a new handler for getting the type of the nth item in the list
;;
(define-handler getDataType (AEList n)
  (with-aedescs (buffer)
    (with-standard-ae-type me
      (ae-error
        (ae-get-descriptor (aedesc me) n 'index #$typeWildCard buffer))
      (symbol-name (rref buffer aedesc.descriptortype)))))

;; ensure that even the prototype has a proper aedesc
(make-aedesc-property AEList nil nil)
(init-aedesc-property AEList nil (aedesc AEList) nil nil)


;;
;; AERecord
;;

(new AEList
     :accessType (symbol-name #$typeAERecord)
     :objectName "AERecord"
     :project sk8)

    #|
(let ((data-initArg (initializerArgument initArgs 'data :use t)))
|#
(define-handler init-aedesc-property (AERecord original my-aedesc aedesc-initArg initArgs)
  (declare (ignore initArgs)) 
  (unless aedesc-initArg
      ;; Fill the data, either from the keyarg or from the original.
      (cond
       (original
	;; !! seems like when restoring a SK8 project the original may not have been restored yet,
	;; so we force it here, before copying it.
        (unless (is-a (aedesc original) ForeignMemory)
          (restore original))
        (#_AEDuplicateDesc (aedesc original) my-aedesc))
       (t 
        (init-list-aedesc me my-aedesc t)))))

;; here's a new handler for getting the type of a keyword-specified record item
;;
(define-handler getDataType (AERecord key)
  (with-aedescs (buffer)
    (with-standard-ae-type me
      (ae-error
        (ae-get-descriptor (aedesc me) (ensure-valid-ostype key) 'key #$typeWildCard buffer))
      (symbol-name (rref buffer aedesc.descriptortype)))))

(define-handler extractData (AERecord key &key 
                                        type (errorIfNotFound t) valueIfNotFound
                                        ((:project p)))
  (with-standard-ae-type me
    (get-data-item (aedesc me)
                   key
                   'key
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler insertData (AERecord key value &key type)
  (with-standard-ae-type me
    (put-data-item (aedesc me)
                   key
                   'key
                   value
                   :type type)))

(define-handler deleteData (AERecord key &key (errorIfNotFound t))
  (with-standard-ae-type me
    (delete-data-item (aedesc me)
                      key
                      'key
                      :errorIfNotFound errorIfNotFound)))

(define-handler coerceToAppleEvent (AERecord target eventClass eventID &key reply)
  (let ((ae (if reply
              (new AppleEvent 
                   :target target
                   :eventClass eventClass
                   :eventID eventID
                   :reply reply
                   :project SK8)
              (new AppleEvent 
                   :target target
                   :eventClass eventClass
                   :eventID eventID
                   :project SK8))))
    (rlet ((the-keyword :ostype))
      (with-aedescs (buffer)
        (dotimes (i (countItems me))
          (ae-error 
            (#_AEGetNthDesc (aedesc me) (1+ i) #$typeWildCard the-keyword buffer))
          (#_AEPutParamDesc
           (aedesc ae) 
           (%get-ostype the-keyword)
           buffer))))
    ae))

;; ensure that even the prototype has a proper aedesc
(make-aedesc-property AERecord nil nil)
(init-aedesc-property AERecord nil (aedesc AERecord) nil nil)


;;
;; AppleEvent
;;

;; first clear the definition of the old AppleEvent object
(unless (eq AppleEvent sk8::undefined) (setf (objectName AppleEvent :force t) nil))

;; now make it a child of AERecord
(new AERecord
     :accessType (symbol-name #$typeAppleEvent)     
     :objectName "AppleEvent"
     :properties '(AETarget)
     :project sk8)

;; We tag here for two reasons: 1. Adds the property 'reply' to the AppleEvent object 
;; for maintaining an AERecord in which values can be returned from an event.
;; 2. Adds 'reply' to AppleEvent's creation relations which ensures that new
;; AppleEvents will automatically get new copies of a reply record.
;;
(tagPart AppleEvent (new AERecord :project (project AppleEvent)) 'reply)

;; Restores the AE's aeTarget and reply, when the event itself is being restored
;;
(define-handler restore (AppleEvent)
  (call-next-method)
  (when (and (aeTarget me) (not (is-a (osAETarget (aeTarget me)) ForeignMemory)))
    (restore (aeTarget me)))
  (when (and (reply me) (not (is-a (aedesc (reply me)) ForeignMemory)))
    (restore (reply me))))

(define-handler init-aedesc-property (AppleEvent original my-aedesc aedesc-initArg initArgs)
  (unless aedesc-initArg
    (cond
     (original
      ;; !! seems like when restoring a SK8 project the original may not have been restored yet,
      ;; so we force it here, before copying it.
      (unless (is-a (aedesc original) ForeignMemory)
        (restore original))
      (#_AEDuplicateDesc (aedesc original) my-aedesc))
     ;; Build the AppleEvent (unless it's already been supplied.)
     ((null aedesc-initArg)
      ;; !! hack to ensure ThisApplicationAETarget is valid
      (unless (is-a (osAETarget ThisApplicationAETarget) ForeignMemory)
        (restore ThisApplicationAETarget))
      (ae-error 
        (#_AECreateAppleEvent 
         #$typeWildCard 
         #$typeWildCard
         (osAETarget ThisApplicationAETarget)
         #$kAutoGenerateReturnID 
         #$kAnyTransactionID
         my-aedesc))
      ;; Update the aeTarget cache with the default target which just used.
      ;; Note: We don't call the setter handler, to avoid assigning the ae attribute twice.
      (setf (slot-value me 'aeTarget) ThisApplicationAETarget))))
  ;; Process an internal 'the-reply' parameter, if there is one. This is provided automatically
  ;; by SK8 when AppleEvents are received (I think).
  (let ((reply (initializerArgument initargs 'reply :use nil))
        (the-reply (initializerArgument initargs 'SK8::the-reply :use t)))
    ;; !! Somewhere in SK8, AE handlers are passed an AppleEvent object, but the
    ;; object is created with the-reply set to the aedesc for the reply AppleEvent object.
    ;; This means we must build an AERecord with the possibility that its the-aedesc
    ;; should be the-reply. So, we must allow the reply to be either an AERecord object
    ;; or an aedesc.
    (unless reply
      (cond
       ((is-a the-reply AERecord)
        (setf (reply me) the-reply))
       ((macptrp the-reply)
        (setf (reply me) (new AERecord :project (project me) :aedesc the-reply)))
       (the-reply
        (error "Bad ae reply parameter.")))))
  me)

(define-handler (setf eventClass) (value AppleEvent)
  (unless (is-a value OSTypeString)
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType OSTypeString
               :ownerObject me
               :propertyName 'eventClass))
  (insertAttribute me #$keyEventClassAttr value :type #$typeType))

(define-handler eventClass (AppleEvent)
  (extractAttribute me #$keyEventClassAttr))

(define-handler (setf eventID) (value AppleEvent)
  (unless (is-a value OSTypeString)
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType OSTypeString
               :ownerObject me
               :propertyName 'eventID))
  (insertAttribute me #$keyEventIDAttr value :type #$typeType))

(define-handler eventID (AppleEvent)
  (extractAttribute me #$keyEventIDAttr))
 
(define-handler (setf aeTarget) (value AppleEvent)
  (unless (is-a value AETarget)
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType AETarget
               :ownerObject me
               :propertyName 'aeTarget))
  (unless (macptrp (OSaetarget value))
    (error "Target, ~a, isn't fully initialized." value))
  (setValue 'AETarget me value)
  (insertAttribute me #$keyAddressAttr value))

;; Bcs of the documentation says one things and the implementation of the original
;; AEs does another, we must support two forms of targets keywords.
;; 'target' is assumed to be the inferior form.
;;
(define-handler SK8::target (AppleEvent)
  (aeTarget me))

(define-handler (setf SK8::target) (value AppleEvent)
  (setf (aeTarget me) value))

(define-handler (setf reply) (value AppleEvent)
  (unless (is-a value AERecord)
    (sk8-error PropertyTypeMismatchError
               :object value
               :expectedType AERecord
               :ownerObject me
               :propertyName 'reply))
  (setValue 'reply me value))

 
;; replyMode may be one of [none, queue, or wait]
;;

(define-handler send (AppleEvent &key replyMode dontRecord dontExecute)
  (setq replyMode (case replyMode
                    (sk8::wait        :wait-reply)
                    ((sk8::none nil)  :no-reply)
                    (sk8::queue       :queue-reply)
                    (t                (error "ReplyMode must be one of 'none', 'queue' or 'reply'"))))
  (with-slots (aedesc) me
    (when (eq replyMode :queue-reply)
      (ccl::ae-put-attribute-longinteger aedesc #$keyreturnIDAttr (incf *ae-return-id*))
      (install-queued-reply-handler aedesc #'(lambda (&rest r) (format t "~% ding! ~s" r))))
    (appleevents::send-appleevent (aedesc me) 
                                  (aedesc (reply me))
                                  :reply-mode replyMode
                                  :dont-record dontRecord
                                  :dont-execute dontExecute))
  t)

;;#'sk8::HandleAppleEventQueuedReply

;;;
;;; Important user functions for getting and setting the parameters of 
;;; an AppleEvent object.
;;;

(define-handler getDataType (AppleEvent key)
  (with-aedescs (buffer)
    (with-standard-ae-type me
      (ae-error
        (ae-get-descriptor (aedesc me) key 'parameter-key #$typeWildCard buffer))
      (symbol-name (rref buffer aedesc.descriptortype)))))

(define-handler extractParameter (AppleEvent key &key
                                                type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (with-standard-ae-type me
    (get-data-item (aedesc me)
                   key
                   'parameter-key
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler extractReplyParameter (AppleEvent key &key
                                                      type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (with-standard-ae-type me
    ; assume the reply is only a record
    (get-data-item (aedesc (reply me))
                   key
                   'key
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler extractAttribute (AppleEvent key &key
                                                type (errorIfNotFound t) valueIfNotFound ((:project p)))
  (with-standard-ae-type me
    ; get-as-attribute
    (get-data-item (aedesc me)
                   key
                   'attribute-key
                   :type type
                   :errorIfNotFound errorIfNotFound
                   :valueIfNotFound valueIfNotFound
                   :project (or p (project me)))))

(define-handler insertParameter (AppleEvent key value &key type)
  (with-standard-ae-type me
    (put-data-item (aedesc me)
                   key
                   'parameter-key
                   value 
                   :type type)))
                                         
(define-handler insertReplyParameter (AppleEvent key value &key type)
  (with-standard-ae-type (reply me)
    (put-data-item (aedesc (reply me))
                   key
                   'key
                   value
                   :type type)))
 
(define-handler insertAttribute (AppleEvent key value &key type)
  (with-standard-ae-type me
    (put-data-item (aedesc me)
                   key
                   'attribute-key
                   value
                   :type type)))
                                        
(define-handler deleteParameter (AppleEvent key &key (errorIfNotFound t))
  (with-standard-ae-type me
    (delete-data-item (aedesc me)
                      key
                      'parameter-key
                      :errorIfNotFound errorIfNotFound)))

(define-handler deleteReplyParameter (AppleEvent key &key (errorIfNotFound t))
  (with-standard-ae-type me
    (delete-data-item (aedesc (reply me))
                      key
                      'key
                      :errorIfNotFound errorIfNotFound)))


;; ensure that even the prototype has a proper aedesc
(make-aedesc-property AppleEvent nil nil)
(init-aedesc-property AppleEvent nil (aedesc AppleEvent) nil nil)


;;
;; Additions to AETarget
;;

;; For some reason this is undefined in the original file, so here it is:
;;
#|  Can't find process info rec in new mcl.
(define-handler signature (AETarget)
  (rlet ((psn :processSerialNumber 
              :highLongOfPSN (ldb (byte 32 32) (processSerialNumber me))
              :lowLongOfPSN (ldb (byte 32 0) (processSerialNumber me))))
    ;; Can't use rlet here, bcs the record itself contains pointers
    ;; to data space such as for the name of the process.
    ;; make-processInfoRec builds a rec w/ sufficient space
    (let ((pir (cl-user::make-processInfoRec)))
      (cl-user::get-process-info psn pir)
      (prog1
        (symbol-name (cl-user::get-process-signature pir))
        (cl-user::dispose-processInfoRec pir)))))
|#


;; Preserve was previously only defined for a single instance of AETarget,
;; ThisApplicationAETarget.
;; Here we provide a preserve handler for all AETargets. This ensures that
;; their target descriptor gets disposed and prevents problems later
;; when a project is restored.
;;
(define-handler preserve (AETarget)
  (with-slots (osaetarget) me
    (when OSAETarget 
      (dispose-record osaetarget)
      (setf osaetarget nil)))
  ;; Take me off the cache of targets, now that my descriptor is invalid
  (setf *received-AETargets* (remove me *received-AETargets*))
  (call-next-method))

;; Gets rid of the special case for ThisApplicationAETarget.
;;
(define-handler preserve (ThisApplicationAETarget)
  (call-next-method))


;;
;; globals routines
;;

(defun do-collect-params (desc key-list)
  (when key-list
    (cons (get-data-item desc (first key-list) 'key :errorIfNotFound nil)
          (do-collect-params desc (rest key-list)))))

;; Returns a list of values used in determining the prototype SK8 object
;; for the given descriptor to be extracted as.
;;
(defun collect-params (desc extractor-keys)
  (let ((saved-type (rref desc :aedesc.descriptortype)))
    (if (eq saved-type #$typeAERecord)
      (do-collect-params desc extractor-keys)
      ;; Temporarily set the aedesc's type to a record in order to extract parameters
      (with-aedescs (temp-desc)
        (let ((err (#_AECoerceDesc desc #$typeAERecord temp-desc)))
          (case err
            (#.#$noErr
             ;; now just collect the keys and convert back.
             (prog1
               (do-collect-params temp-desc extractor-keys)
               (ae-error (#_AECoerceDesc temp-desc saved-type desc))))
            (#.#$errAECoercionFail
             ;; must resort to brute force type-tag switch.
             (unwind-protect
               (progn
                 (setf (rref desc :aedesc.descriptortype) #$typeAERecord)
                 (do-collect-params desc extractor-keys))
               (setf (rref desc :aedesc.descriptortype) saved-type)))
            (otherwise 
             (ae-error err))))))))

;; Installs an extraction function which takes an aedesc, a type, and possibly a series
;; of values retrieved from the keys of the aedesc (assuming it is a record).
;; This function should return either an AEDescriptor or some other value
;; representative of the aedesc. If the result is an AEDescriptor, this is used
;; as a prototype object for creating a new AEDescriptor using the new aedesc, and
;; this new AEDescriptor is returned. Otherwise, the value is returned as is.
;;
(defun install-extraction-via-function (fromType fn &key extractorKeys)  
  (install-extract-conversion 
   fromType
   #'(lambda (aedesc data-ptr data-size proj)
       (declare (ignore data-ptr data-size))
       (let ((ret-val (apply fn 
                             aedesc
                             fromType
                             (collect-params aedesc
                                             extractorKeys))))
         (if (is-a ret-val AEDescriptor) 
           (let ((new-desc (make-aedesc)))
             (ae-error 
               (#_AEDuplicateDesc aedesc new-desc))
             (new ret-val :aedesc new-desc :project proj))
           ret-val)))))

(defun install-extraction-via-prototype (fromType proto)  
  (install-extract-conversion 
   fromType
   #'(lambda (aedesc data-ptr data-size proj)
       (declare (ignore data-ptr data-size))
       (let ((new-desc (make-aedesc)))
         (ae-error 
           (#_AEDuplicateDesc aedesc new-desc))
         (new proto
              :aedesc new-desc
              :project proj)))))

(define-sk8-function deinstallAEExtraction nil (fromType)
  (deinstall-extract-conversion (ensure-valid-ostype fromType)))

(define-sk8-function installAEExtraction nil (fromType extractor &key extractorKeys)
  (setq fromType (ensure-valid-ostype fromType))
  (cond
   ((and (symbolp extractor) (functional extractor))
    (deinstallAEExtraction fromType)
    (install-extraction-via-function fromType extractor :extractorKeys extractorKeys))
   ((is-a extractor AEDescriptor)
    (deinstallAEExtraction fromType)
    (install-extraction-via-prototype fromType extractor))
   (t
    (sk8-error TypeMismatchError
               :object extractor
               :expectedType '(SK8::Function AEDescriptor)))))

;; Installs an insertion function which takes a value, and a type.
;; This function should return either an AEDescriptor or a new value
;; representative of original. If the result is an AEDescriptor, the type tag of
;; the descriptor is used in place of the original type when performing the
;; insertion. Otherwise, the new value is inserted using standard type guessing.
;;
(defun install-insertion-via-function (inType fn)  
  (install-insert-conversion 
   inType
   #'(lambda (desc accessor access-method value type)
       (put-data-item desc 
                      accessor
                      access-method
                      (funcall fn value type)))))

;; Installs an insertion function which simply checks whether the value is a child
;; of the given prototype AEDescriptor and, if it is, inserts the descriptor as is.
;; Otherwise, an error is signalled. The prototype may also be a list of prototypes.
;;
(defun install-insertion-via-prototype (inType proto)  
  (install-insert-conversion 
   inType
   #'(lambda (desc accessor access-method value type)
       (declare (ignore type))
       (ensureType value proto)
       (put-into-ae-storage desc accessor access-method value #.#$typeDescriptor))))

(define-sk8-function deinstallAEInsertion nil (inType)
  (deinstall-insert-conversion (ensure-valid-ostype inType)))

(define-sk8-function installAEInsertion nil (inType insertor)
  (setq inType (ensure-valid-ostype inType))
  (cond
   ((and (symbolp insertor) (functional insertor))
    (deinstallAEInsertion inType)
    (install-insertion-via-function inType insertor))
   ((or (is-a insertor AEDescriptor)
        (and (is-a insertor List)
             (not (null insertor))
             (every #'(lambda (obj) 
                        (is-a obj AEDescriptor))
                    insertor)))
    (deinstallAEInsertion inType)
    (install-insertion-via-prototype inType insertor))
   (t
    (sk8-error TypeMismatchError
               :object insertor
               :expectedType '(SK8::Function AEDescriptor)))))

;; Here's a version of send-appleevent which only traps
;; ae-errors if a flag is set.
;; Evaluate this if you need to debug a broken appleevent handler.
;; Then call ignoreAENotHandledError.
;; To return to proper error handling, call unignoreAENotHandledError.
;;
(defparameter trap-ae-not-handled-error t)

(define-sk8-function ignoreAENotHandledError nil ()
  (setq trap-ae-not-handled-error nil))

(define-sk8-function unignoreAENotHandledError nil ()
  (setq trap-ae-not-handled-error t))

;; This version of send-appleevents adds two new keyword arguments: dont-record and dont-execute
;; Also, this version checks for a debugging flag trap-ae-not-handled-error.
;; !! Note: this debug flag doesn't seem to really help under SK8's normal condition handling system.
;;
(defun get-error-string (the-desc &optional (errorp t))
  (ccl::ae-get-parameter-char the-desc #$keyErrorString errorp))

(defun ccl::check-reply-error (reply)
  (let ((error-number nil))  ;;;(get-error-number reply nil)))  ;;get-error-number is dying...************ERROR CHECKING TURNED OFF
    (when (and error-number
               (not (= error-number #$noerr)))
      (error (make-condition 'appleevent-error
                             :oserr error-number
                             :error-string (get-error-string reply nil))))))

(defun ccl::send-appleevent (the-appleevent the-reply &key
                                                         (reply-mode :no-reply) (interact-mode nil)
                                                         (can-switch-layer nil) (dont-reconnect nil)
                                                         (want-receipt nil) (priority #$kAENormalPriority)
                                                         (dont-record nil) (dont-execute nil) 
                                                         (timeout #$kAEDefaultTimeout)
                                                         (idleproc appleevent-idle)
                                                         filterproc )
  (let ((mode (+ (ecase reply-mode
                   (:no-reply #$kAENoReply)
                   (:queue-reply #$kAEQueueReply)
                   (:wait-reply #$kAEWaitReply))
                 (ecase interact-mode
                   ((nil) 0)
                   (:never-interact #$kAENeverInteract)
                   (:can-interact #$kAECanInteract)
                   (:always-interact #$kAEAlwaysInteract))
                 (if can-switch-layer #$kAECanSwitchLayer 0)
                 (if dont-reconnect #$kAEDontReconnect 0)
                 (if want-receipt #$nreturnreceipt 0)
                 ;; here's handling for the extra key parameters -digi
                 (cond
                  (dont-record #$kAEDontRecord)
                  (dont-execute #$kAEDontExecute)
                  (t 0)))))
    (ae-error 
      (let ((ccl::*inside-aesend* t)    ; used to be in appleevent package
            (res (#_AESend the-appleevent the-reply mode priority 
                  timeout idleproc (or filterproc (%null-ptr)))))
        (if (or (eq res #$errAEWaitCanceled) ; be silent about aborts
                (and (eq res #$errAEEventNotHandled)
                     (null trap-ae-not-handled-error)))    ; !! here's the debug patch - digi
          #$NoErr
          res)))
    (when (eq reply-mode :wait-reply)
      (ccl::check-reply-error the-reply)
      the-reply)))


#| for reevaluating this file:

(defparameter *received-AETargets* nil)
(defparameter *ae-target-ptr* nil)
(setf (objectName SymbolList) nil)
(setf (objectName OSTypeString) nil)
(setf (objectName OSTypeStringList) nil)
(setf (objectName VirtualDescriptorType) nil)
(setf (objectName AEDescriptor) nil)
(setf (objectName AEList) nil)
(setf (objectName AERecord) nil)

|#

#|
	Change History (most recent last):
	1		10/1/95	digi		Created
	2		10/25/95	digi		0.8a release
	3		11/1/95	digi		Fixed bug in extractReplyParameter.
	4		11/17/95	digi		Changed get-data-item from a handler to a fn, to make it 
                           useful even without a SK8 AE Object.
	5		11/22/95	digi		Redefined send-appleevent to take :dont-execute
									and :dont-record keywords.
	6		11/27/95	digi		Changed the std behavior of extractData, so that if the
									item was a AEList, it would return a standard SK8 list.
									Also changed set-data-item, so it would accept either a 
									SK8 list or an AEList object.
									This required adding functions maybe-convert-to-sk8 and
									maybe-convert-from-sk8.
	7		12/21/95	digi		Changed defvars -> defparameters so saving standalone works
	8		1/3/96	digi		Added the ability to associate children of AEDescriptor
									with custom types, so that whenever a desc of such a type
									is extracted, a child of the custom SK8 object is made.
									Also added the notion of VirtualDescriptor types which
									use standard storage and extraction routines for primitive
									data types, but under a special type code.
	9		1/6/96	digi		Reorganized the function space to make inserting and 
									extracting more consistent which hopefully will make this
									module easier to debug and easier to pass on. (setf data)
									and insertData now call put-data-item then put-into-ae-storage
									then put-ae-pointer or put-ae-descriptor. data and extractData
									now call get-data-item then get-from-ae-storage.
	10		1/9/96	digi		Added inherited key properties for indicating virtual 
									properties tied to record keys which are explicitly 
									initialized.
	11		1/13/96	digi		0.9a release.
	12		1/14/96	digi		Fixed bug in countItems which prevented counting of
									AELists with customTypes.
	13		1/14/96	digi 		Added AEList & AERecord item deletion.
	14		1/16/96	digi		Allowed customType to be set to nil, which removed and
									Custom extraction, insertion routines. 
	15		1/16/96	digi		Fixed bug which prevented custom extaction/insertion routines
									from actually getting removed. Enhanced the hash table for custom
								   typing to prevent children of a custom-typed object from 
									accidentally erasing the mapping when establishing their own custom 
									type.
	16		1/16/97	digi		Changed the way keys were extracted for determiner fn's so
									that missing keys were passed to the determiner as nil.
	17		1/16/97	digi		Added the ability to pass AEDescriptors as values to insertData
									which enables callers to provide types implicitly (in the desc type)
									instead of explicitly with the type keyword parameter.
	18		1/16/97	digi		Fixed bizarre bug in maybe-convert-from-sk8 which converted a String
									when the type was typeWildCard to an AEList!
	19		1/17/96	digi		Added data inheritance for all descendants AEDescriptors including
									AERecords and AppleEvents whose key entries are all copied.
	20		1/23/96	digi		Removed &rest parameter from SK8 handlers, since this prevents them
									from showing up in the Object Editor. Thanks to Charlie Hill at Apple
								   for pointing out this problem.
	21		1/23/96	digi		Removed inheritedKey property. No longer needed now that all
									AEDescriptors now automatically pass on their data.
	22		1/23/96	digi		0.91a limited release.
	23		2/4/96	digi		Added restore handler for AEDescriptor so it would load properly from
									a saved application. This was not needed before change 19.
	24		2/4/96	digi		0.92a release
	25		2/8/96	digi		Attempted to fix strange behavior in standalones which caused a TypeMismatchError
									in init-aedesc-property upon loading. Added some safety checks to 
									init-aedesc-property and a preserve handler for AEDescriptor to ensure that the
									aedesc property is blown away before saving to a standalone. Neither of these
									changes would seem necessary.
	26		2/28/96	digi		0.93a release
	27		3/3/96	digi		Fixed bug in get-from-ae-storage which prevented new colors from being extracted
									properly
	28		3/3/96	digi		get-from-ae-storage now automatically extracts unknown descriptors as 
									AEDescriptors, unless the errorIfNoExtractor key parameter is set to True.
	29		3/3/96	digi		The customType property was causing problems for two reasons: 1. Since the
									the type was just a SK8 property and not part of the aedesc, it was possible
									to get the property and aedesc typetag out of sync. 2. Even when they were
									in sync, the customtype tag was not "blessed" by the Apple Event Manager. This
									meant that although, say, an object specifier was tagged "obj ", the AE Manager
									still did not recognize the specifier as such, since it hadn't been coerced
									using the AE Manager routine.
									The fix: the customType property was removed in favor of maintaining the 
									custom type in the aedesc typetag itself. A new property accessType was added
									which specifies the tag to use when modifying the descriptor. Changes to the
									AEDescriptor's type now call the official AE Manager coercion routine to 
									ensure "blessing."
	30		3/3/96	digi		installAEExtraction and installAEInsertion added to give users explicit
									control over extracting SK8 objects from AEDescriptors and inserting SK8 
									objects into AEDescriptors. This was previously done implicitly by
									setting the customType property.
	31		3/3/96	digi		AEDescriptor type -> typeTag to avoid confusion with type parameter to handlers
									which do non-destructive type conversions on the fly and don't necessarily
									affect a descriptor's type tag.
	32		3/18/96	digi		I think I identified the problem described in 25. Restoration from a saved
									SK8 image does not seem to occur in the order we would want. Descendants
									of AEDescriptor are restored prior to their originals. The causes problems
									when the descendant tries to duplicate the aedesc property of the parent, but
									that property hasn't been initialized yet. To get around this, we now check 
									for whether the parent's aedesc is of type ForeignMemory, if it isn't we go
									and restore the parent first. The attempted "fix" in 25 was removed.
	33		3/26/96	digi		Added 'with accessTypeTag' handler to allow efficient access to non-standard desc types.						
	34		3/26/96	digi		Fixed list extraction inefficiency.
	35		3/26/96	digi		VirtualDescriptorType typeID -> typeTag
									VirtualDescriptorType storage -> accessType
									Fixed a bug when inserting using a virtual descriptor type into AERecords.
	36		3/27/96	digi		Added getDataType handler for AEList.
	37 	3/27/96	digi		Eliminated determinerKeys and objectDeterminerFn properties from AERecord
									since installAEExtraction and installAEInsertion make them unnecessary.
	38 	3/27/96	digi		Allowed either a 'target' or 'aetarget' initializer argument to AppleEvent
									to allow backwards compatibility with old version of AppleEvents.lisp.
									Thanks to Charlie Hill for pointing out the problem.
	39		3/27/96	digi		1.0a1 release
	40		4/22/96	digi		Added the ability to extract file aliases. (As requested by Skot Posey)
	41		5/2/96	digi		Fixed color extractor to prevent ComplexRGBColor's from being extracted
									in place of Black.
	42		8/9/96	digi		Added floating point handling.
	43		8/9/96	digi		Properties of AppleEvents such as eventClass, eventID, and aeTarget can
									now be substituted in place, which eliminates the need to create new 
									AppleEvents every time new properties are desired.
	44		8/10/96	digi		The AppleEvent init-aedesc-property handler was simplified to take
									advantage of change #43 above.
	45		8/10/96	digi		The reply property is now tagged, so a new AERecord is automatically
									created when a new AppleEvent is made.		
	46		8/10/96	digi		Added a preserve handler for all AETargets to avoid problems with 
									restoring targets which are not ThisApplicationAETarget.		
	47		8/12/96	digi		1.1a1 release under the new name SK8-AEOSL							
	2  	 9/12/96	Brian   	
	3  	 9/16/96	Brian   	
	4  	 9/16/96	Brian   	Adding eval-when to make the constants work.
	5  	10/31/96	Brian   	Added ForeignMemory Type back in.
	6  	11/ 1/96	Brian   	Fixing signature of stub handleAppleEvent handler.
	7  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	8  	11/12/96	Brian   	Adding infrastructure for passing PICTs back
						and forth.  Now it can take a QDPicture as a
						parameter.
	9  	11/14/96	Hernan  	Added support for simple, handle based media.
	10 	11/26/96	Hernan  	Moving assure-ostypestring to File.lisp.
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
