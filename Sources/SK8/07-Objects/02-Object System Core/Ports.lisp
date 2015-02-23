;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




;;; Ports, man.

(in-package :SK8Development)


#|

NOTE: A port cannot propagate the special value :%unwired%, which is  
     reserved for internal use.

REMAINING THINGS TO BE DONE:

REMOVE PROPERTY SHOULD REMOVE A PORT
INHERITANCE: CREATE A DUPLICATE OF THE PARENTS PORT SAVE FOR THE PARENT'S PORT'S CONNECTIONS
FIX THE CAPS LOCK KEY

|#


;;; Hash table for all the objects with an output port connected to them.
;;; key: the object being ported
;;; value: list of ports
;;; This allows checking to see if a particular object is ported with a 
;;; single hash lookup.  Fastest way I can imagine.
(defvar *output-port-hash-table* (make-hash-table :weak :key :test #'eq))

;;; Hash table for input ports so they don't get GC'd.
(defvar *input-port-hash-table* (make-hash-table :weak :key :test #'eq))

;;; Gotta keep track of all weak hash tables.
(push *output-port-hash-table* mf::*all-weak-hash-tables*)
(push *input-port-hash-table*  mf::*all-weak-hash-tables*)

(defun clean-up-port-table (me)
  ;;; First we map through the objects and make sure they are valid...
  (maphash #'(lambda (obj ports) 
               (declare (ignore ports))
                 
               (unless (project obj) (remhash obj me))) me)
  ;;;Then we map through the remaining ports and make sure they are in fact ports...
  (maphash #'(lambda (obj ports) 
               (unless (every #'(lambda (obj) (and (project obj) (is-a obj port))) ports)
                 (setf (gethash obj me) (delete-if-not #'(lambda (obj) (and (project obj) (is-a obj port))) ports))))
           me)
  )

;;; The interfaces to the above hash-tables (and %object-has-ports? bit)...
;;;
(defun register-port-with-object (obj thePort portType)
    (let* ((portTable (if (eq portType 'input)
                      *input-port-hash-table*
                      *output-port-hash-table*))
         (entry (gethash obj portTable)))
    (unless (memq thePort entry)
      (setf (gethash obj portTable) (cons thePort entry))
      (MF::set-%object-has-ports? obj))))

(defun unregister-port-with-object (obj thePort portType)
  (let* ((portTable (if (eq portType 'input)
                      *input-port-hash-table*
                      *output-port-hash-table*))
         (entry (gethash obj portTable)))
    (when (memq thePort entry)
      (setq entry (delete thePort entry :test #'eq))
      (setf (gethash obj portTable) entry)
      (when (and (null entry)
                 (null (gethash obj (if (eq portType 'input)
                                      *output-port-hash-table*
                                      *input-port-hash-table*))))
        (MF::clear-%object-has-ports? obj)))))

(defun unregister-ports-with-object (obj portType &key test)
  (let* ((portTable (if (eq portType 'input)
                      *input-port-hash-table*
                      *output-port-hash-table*))
         (entry (gethash obj portTable)))
    (if (null test)
      (setq entry nil)
      (setq entry (delete-if test entry)))
    (setf (gethash obj portTable) entry)
    (when (and (null entry)
               (null (gethash obj (if (eq portType 'input)
                                    *output-port-hash-table*
                                    *input-port-hash-table*))))
      (MF::clear-%object-has-ports? obj))))

(defun object-output-ports (obj)
  (gethash obj *output-port-hash-table*))

(defun object-input-ports (obj)
  (gethash obj *input-port-hash-table*))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro do-for-all-output-ports ((portVar) &body body)
    (let* ((obj (gensym))
           (oports (gensym)))
      `(flet ((port-mapper-body-function (,obj ,oports)
                (declare (ignore ,obj))
                (dolist (,portVar ,oports)
                  ,@body)))
         (declare (dynamic-extent #'port-mapper-body-function))
         (maphash #'port-mapper-body-function *output-port-hash-table*)))))



;;; Used for testing for port feedback loops.
;;; Bound to the object starting the loop.
;;;
(defvar *port-feedback-history* nil)



;;; Replace instances of this macro with the real thing when
;;; we get type checking.
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro port-check-SK8-type (var type)
    `(unless (is-a ,var ,type)
       (SK8-error TypeMismatchError :object ,var :expectedType ,type))))



;;; InputPorts and OutputPorts are built on this.
;;; portObject is the object this Port is connected to. 
;;; portProperty, a symbol, is the property of the object this port is connected to.
;;; valuetype ...
;;; enabled is a hook to individually enable or disable a port.
;;; feedbackAction may be one of:
;;;   nil                         do nothing, let it loop.
;;;   stop                  stop any feedback loop.
;;;   stopAndWarn        stop a feedback loop and provide a warning.
;;;   warn                    warn when loop is detected
;;;   error                   error out on detected feedback loop
;;; The default value is stop cuz Brian thinks that what users most typically will want.
(new object 
     :objectname "Port"
     :project sk8
     :properties '(portObject 
                   portProperty 
                   (valueTypes     :value t)
                   (signature      :value t)
                   (enabled        :value t)
                   (feedbackAction :value stop)))

(new Port
     :objectname "InputPort"
     :project sk8)

;;; WiredTo is a list of input ports that we're wired to.
;;; Alternatively the value can be broadcast, in which case
;;; all input ports that are type compatible get triggered.
;;;
;;; triggerBefore is boolean (used to be called whenToTrigger)
(new Port
     :objectname "OutputPort"
     :project sk8
     :properties '((triggerBefore :value nil)
                   wiredTo))

;;; A word of explanation here.
;;; InputOutports can only be wired to other InputOutputPorts.
;;; Their wiring is stored in the property wiredTo as a list like:
;;;   (WireRing ioport1 ioport2 ioport3...)
;;;
;;; All InputOutputPorts that are connected together share a common WireRing
;;; list structure, so it's inexpensive to add a port to the ring and consistancy
;;; is assured.
;;;
;;; So for InputOutputPorts, the wiredTo slot name is somewhat of a misnomer.  Ie., 
;;; the symbol WireRing and the Port itself are included in the wiredTo list.  Such is life; 
;;; the reader handler compensates.
;;;
;;; Also, note that InputOutputPorts inherit from OutputPorts *first*, than InputPorts.
;;; 
;;; activateDisable is used internally to keep InputOutputPorts from trigger themselves.
(new OutputPort
     :objectname "InputOutputPort"
     :otherParents InputPort
     :properties '(activateDisable)
     :project sk8)


(define-handler initialize (Port original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (setf original (originalAncestor me original Port))
  (let* ((portType (portType me))
         (inputPort? (eq portType 'input))
         (portObject (getValue 'portObject me))
         (wiredTo (unless inputPort? (getValue 'wiredTo me))))
    (cond
     ;; If this port isn't associated with any new objects (it's just a simple instance/copy of the original),
     ;; just clear its associations
     ((and (eq (portObject original) portObject)
           (eq (portProperty original) (getValue 'portProperty me))
           (or inputPort? (equal (wiredTo original) wiredTo)))
      (setValue 'portObject me nil)
      (unless inputPort? (setValue 'wiredTo me nil)))
     ;; If it's an InputPort, just register it with its object
     (inputPort?
      (when portObject (register-port-with-object portObject me portType)))
     ;; If it's an OutputPort or InputOutputPort, install the hook method if necessary (and register it with its object)
     (t
      ;; All ports in a "wireRing" must share the same (eq) wireRing list, so make sure this is the case
      (when (and (listp wiredTo) (eq (first (the list wiredTo)) 'wireRing))
        (dolist (otherPort (cdr (the list wiredTo)))
          (unless (eq otherPort me)
            (if (eq (getValue 'wiredTo otherPort) wiredTo)
              (return)
              (setValue 'wiredTo otherPort wiredTo)))))
      (when portObject (install-outputty-port me))))))


(defun update-ports-from-parents (obj)
  (flet ((merge-ports (targetObject otherObject)
           (let ((existingInputPorts (object-input-ports targetObject))
                 (existingOutputPorts (object-output-ports targetObject))
                 portProperty portType portSignature)
             
             (dolist (otherPort (object-input-ports otherObject))
               (setq portProperty (portProperty otherPort)
                     portSignature (signature otherPort))
               (unless (dolist (existingPort existingInputPorts)
                         (when (and (eq portProperty (portProperty existingPort))
                                    (eq portSignature (signature existingPort)))
                           (return t)))
                 (addInputPort targetObject portProperty :signature portSignature :prototype otherPort)))
             
             (dolist (otherPort (object-output-ports otherObject))
               (setq portProperty (portProperty otherPort)
                     portType (portType otherPort)
                     portSignature (signature otherPort))
               (unless (dolist (existingPort existingOutputPorts)
                         (when (and (eq portProperty (portProperty existingPort))
                                    (eq portType (portType existingPort))
                                    (eq portSignature (signature existingPort)))
                           (return t)))
                 (if (eq portType 'OutputPort)
                   (addOutputPort targetObject portProperty :signature portSignature :prototype otherPort)
                   (addInputOutputPort targetObject portProperty :signature portSignature :prototype otherPort)))))))
    (mf::do-SK8-parents (parent obj)
      (when (logbitp mf::%flags-has-ports? (sk8::sk8_flags parent))
        (merge-ports obj parent)))))



(define-handler portType (Port)
  'Unknown)

(define-handler portType (InputPort)
  'Input)

(define-handler portType (OutputPort)
  'Output)

(define-handler portType (InputOutputPort)
  'InputOutput)


;;; To make the SK8Script read better.
(define-handler enable (Port)
  (setf (enabled me) t))

(define-handler disable (Port)
  (setf (enabled me) nil))

(define-handler disabled (Port)
  (not (enabled me)))

;;; Enforce the rules.
(define-handler (setf signature) :before (value Port)
  (declare (ignore value))
  (when (wiredTo me)
    (error
     "It's not permitted to change the signature of a Port after it's been wired")))



(define-handler (setf portObject) (newObject Port)
  (let ((oldObject (portObject me))
        (portType (portType me)))
    (when oldObject (unregister-port-with-object oldObject me portType))
    (when newObject (register-port-with-object newObject me portType))
    (setValue 'portObject me newObject)))



#| ; before cleanup, for reference...
(define-handler (setf portObject) (newObject OutputPort)
  (declare (ignore newObject))
  (delete-output-port-from-hash-table me)
  (call-next-method)
  (add-output-port-to-hash-table me))
|#


;;; Emits messages,
;;; and returns t or nil.
(define-handler feedback-check-okay? :private (Port)
  (case (feedbackAction me)
    ((stop)        nil)
    ((warn)        (warn "Port feedback loop detected by ~s" me)
                   t)
    ((stopAndWarn) (warn "Port feedback loop detected and stopped by ~s" me) 
                   nil)
    ((error)       (error "Port feedback loop at ~s" me))
    ((nil)         t)
    (t             (error "Bogus value of FeedbackAction"))))


;; support saving and loading of unnamed port objects
(define-handler sk8::helpfindmychild (Port)
  (let* ((myproj (project me))
         (myclass (class-of me))
         (myid (sk8::sk8_id me))
         (myportobject (getvalue 'portobject me))
         (myportproperty (getvalue 'portproperty me))
         (slots (mapcar #'car (class-direct-instance-slots myclass)))  ;; the local slot names
         (pars (sk8::parents me))
         (myprojname (sk8::sk8_id myproj)))
    (list 'port myprojname pars slots myid myportobject myportproperty)))

(define-handler sk8::pleaseFindMyChild (Port args)
  (if (and (listp args) (eq (pop args) 'port))
    (let* ((projname (pop args))
           (pars (pop args))
           (slots (pop args))
           (myid (pop args))
           (myportobject (pop args))
           (myportproperty (pop args)))
      (if (and myPortObject myPortProperty)
        (findPort myPortObject myPortProperty (portType (car pars)))
        ;; this is taken from find-unique-unnamed-object, but also looks for portobject and portproperty
        (let ((proj (and (symbolp projname) (boundp projname) (symbol-value projname))))
          (when (sk8::is-a proj sk8::project)
            (let ((parent-classes (mapcar #'class-of pars))
                  retobj)
              (SK8::mapProjectObjects
               proj
               #'(lambda (child)
                   (unless (mf::named-object? child)
                     (let* ((child-class (class-of child)))
                       (when (equal (class-direct-superclasses child-class) parent-classes)
                         (when (and (eq myportobject (getvalue 'portobject child))
                                    (eq myportproperty (getvalue 'portproperty child))
                                    (not (set-exclusive-or slots
                                                           (mapcar #'car (class-direct-instance-slots child-class))))
                                    ;; child is a valid candidate. use it if id matches too, else save one in case we never find an id match
                                    (if (eql myid (sk8::sk8_id child))
                                      (return-from sk8::pleasefindmychild child)
                                      (unless retobj
                                        (setf retobj child))))))))))
              retobj)))))
    (call-next-method)))

;;; Decides whether or not to run activateOutputPort, and does so if appropriate.
;;; Note that the let performs an automatic optimized unwind-protect.  And we only cons 
;;; when necessary.
(define-handler activate-output-port-preamble :private (OutputPort oldValue newValue)
  (and (enabled me)
       (portFilter me NewValue)
       (cond ((member me *port-feedback-history*)
              (when (feedback-check-okay? me)
                (activateOutputPort me oldValue newValue)))
             (t
              (let ((*port-feedback-history* (cons me *port-feedback-history*)))
                (activateOutputPort me oldValue newValue))))))
  
  
;;; Don't forget, advanced Port users can write before and after handlers on this.
(define-handler activateOutputPort (OutputPort oldValue newValue)
  (with-slots (wiredTo) me
    (if (eq wiredTo 'broadcast)
      (mapKnownDescendants InputPort
                           #'(lambda (obj)
                               (activate-input-port-preamble obj oldValue newValue me)))
      (dolist (p wiredTo)
        (activate-input-port-preamble p oldValue newValue me)))))
  

;;; Activate the next port in the ring.  Use activateDisable to stop.
(define-handler ActivateOutputPort (InputOutputPort oldValue newValue)
  (with-slots (wiredTo activateDisable) me
    (setq activateDisable t)
    (unwind-protect 
      (cond
       ((and (listp wiredTo)
             (eq (car wiredTo) 'wireRing))
        ;; Activate next port in the ring
        (activate-input-port-preamble (or (cadr (member me (cdr wiredTo))) (cadr wiredTo))
                                      oldValue newValue me))
       ((eq wiredTo 'broadcast)
        (mapKnownDescendants InputPort
                            #'(lambda (obj) 
                                (activate-input-port-preamble obj oldValue newValue me))))
       ((null wiredTo) nil)
       (t (error "Bogus value of wiredTo ~s" wiredTo)))
      (setq activateDisable nil))))

;;; Decides whether or not to run activateInputPort, and does so if appropriate.
(define-handler activate-input-port-preamble (InputPort OldValue NewValue fromPort)
  (with-slots (enabled) me
    (and enabled
         (portFilter me NewValue)
         (if (member me *port-feedback-history*)
           (when (feedback-check-okay? me)
             (activateInputPort me oldValue newValue fromPort))
           (activateInputPort me oldValue newValue fromPort)))))

(define-handler activateInputPort (InputPort OldValue NewValue fromPort)
  (declare (ignore OldValue fromPort))
  (with-slots (portProperty) me
    (funcall (setter portproperty) newValue (portObject me))))

(define-handler activateInputPort (InputOutputPort OldValue NewValue fromPort)
  (declare (ignore OldValue fromPort))
  (with-slots (activateDisable portProperty) me
    (unless activateDisable
      (funcall (setter portproperty) newValue (portObject me)))))

;;; valuetype may be:
;;;  nil                  always fails
;;;  t                    always succeeds, default.
;;;  (or par1 par2 ...)   succeeds if obj is a descendant of any of the parent objects.
;;;  (ior par1 par2 ...)  succeeds if obj is a descendant or equal to of any of the
;;;                       parent objects.
;;;  (and par1 par2 ...)  succeeds if obj is a descendant of all of the parent objects.
;;; Do a before or after method on this to perform other effects.
;;; Currently OldValue is not correct for :after methods.
;;; The user may want to write their own version of this.
(define-handler portFilter (Port value)
  (let ((valuetypes (valuetypes me)))
    (cond ((eq valuetypes t) t)
          ((eq valuetypes nil) nil)
          ((typep valuetypes 'list)
           (case (car valuetypes)
             (or  (some  #'(lambda (p) (inheritsfrom value p)) (cdr valuetypes)))
             (and (every #'(lambda (p) (inheritsfrom value p)) (cdr valuetypes)))
             (ior (some  #'(lambda (p) (or (eq p value) (inheritsfrom value p))) (cdr valuetypes)))
             (t   (error "Bad valuetypes"))))
          (t (error "Bad valuetypes")))))

;;; Creates a new port and attaches it to an object's property.
;;; Returns the port object.
(define-handler addOutputPort (object property 
                                         &key 
                                         ObjectName 
                                         triggerBefore
                                         (signature t)
                                         broadcasting 
                                         (prototype OutputPort))
  (let ((portt (new prototype
                    :project       (project me)
                    :wiredTo       (when broadcasting 'broadcast)
                    :triggerBefore triggerBefore
                    :signature     signature
                    :ObjectName    ObjectName)))
    (setf (slot-value portt 'portObject) me)
    (setf (slot-value portt 'portProperty) property)
    (install-outputty-port portt)
    portt))

(define-handler addInputOutputPort (object property
                                              &key ObjectName 
                                              triggerBefore
                                              (signature t)
                                              (prototype InputOutputPort))
  (let ((portt (new prototype 
                    :project (project me)
                    :signature signature
                    :ObjectName ObjectName
                    :portProperty property
                    :triggerBefore triggerBefore)))
    
    (setValue 'portObject portt me)
    (install-outputty-port portt)
    portt))


;;; Works for both OutputPorts and InputOuputPorts
;;; I'm planning on flushing the eval soon; more pressing matters demand 
;;; attention just now though...
(defun install-outputty-port (portt)
  (let* ((obj (portObject portt))
         (property (portProperty portt))
         (setter-name (CCL::setf-function-name property)))
    ;; Only build up the :around method if necessary
    (unless 
      (and setter-name
           (dolist (gfm (generic-function-methods (symbol-function setter-name)))
             (when (member :around (method-qualifiers gfm)) (return t))))
      (eval `(defmethod (setf ,property) :around (NewValue (obj Object) &key)
               (let ((OldValue (,property obj))
                     (ports (object-output-ports obj)))
                 (dolist (portt ports)
                   (and (eq (portProperty portt) ',property)
                        (triggerBefore portt)
                        (activate-output-port-preamble portt OldValue NewValue)))
                 (call-next-method)
                 (dolist (portt ports)
                   (and (eq (portProperty portt) ',property)
                        (not (triggerBefore portt))
                        (activate-output-port-preamble portt OldValue (,property obj))))))))
    (register-port-with-object obj portt 'output)
    t))


;;; Attach an existing OutputPort to a new place.
;;; If it's already connected attached somewhere, remove it first.
(define-handler attachPort (OutputPort &key ((:object obj)) property triggerBefore)
  (mf::ensure-real-SK8-object obj '("CanÕt attach ports to "))
  (check-type property symbol)
  (setf (triggerBefore me) triggerBefore)
  (setf (portObject me) obj)          ; note: this updates the hash table.
  (setf (portProperty me) property)        
  me)

(define-handler attachPort (InputPort &key ((:object obj)) property)
  (mf::ensure-real-SK8-object obj '("CanÕt attach ports to "))
  (check-type property symbol)
  (setf (portObject me) obj)
  (setf (portProperty me) property)
  me)


(define-handler addInputPort (object property 
                                       &key 
                                       ObjectName 
                                       (signature t)
                                       (prototype InputPort))
  (let ((portt (new prototype 
                    :project    (project me)
                    :signature     signature
                    :ObjectName    ObjectName)))
    (setf (slot-value portt 'portProperty)  property)
    (setf (portObject portt) me)
    portt))


;;; Returns a list of OutputPorts which are wired to this InputPort (OutputPorts just grab the property).
;;;
(define-handler wiredto (InputPort)
  (let ((result nil))
    (do-for-all-output-ports (oport)
      (when (memq me (getValue 'wiredTo oport))
        (push oport result)))
    result))

(define-handler wiredto (InputOutputPort)
  (remove me (cdr (getValue 'wiredTo me))))


(define-handler removePorts (Object &key property type)
  (labels ((removep (p)
             (and (eq (portObject p) me)
                  (or (null property) (eq (portProperty p) property))))
           (remove-output-ports ()
             (unregister-ports-with-object me 'output :test #'removep))
           (remove-input-ports ()
             (let ((iportsRemoved (remove-if-not #'removep (object-input-ports me))))
               (unregister-ports-with-object me 'input :test #'removep)
               (do-for-all-output-ports (oport)
                 (setValue 'wiredTo oport
                           (delete-if #'(lambda (wtport) (memq wtport iportsRemoved))
                                      (wiredTo oport)))))))
    (declare (dynamic-extent #'removep #'remove-output-ports #'remove-input-ports))
    (if type
      (case type
        (input  (remove-input-ports))
        (output (remove-output-ports))
        (t       (error "Type, ~a, can only be false, input or output" type)))
      (progn (remove-input-ports)
             (remove-output-ports)))
    t))



;;; Detaches the Port from whatever it's connected to.
;;; Note that setting the portObject frobs the hash table for us.
;;;
(define-handler detachPort (Port)
  (setf (portObject me) nil)
  (setf (portProperty me) nil)
  t)

(define-handler ports (object)
  (nconc (copy-list (object-input-ports me))
         (copy-list (object-output-ports me))))

(define-handler inputPorts (object)
  (copy-list (object-input-ports me)))

(define-handler outputPorts (object)
  (remove 'output (object-output-ports me)
          :key #'portType
          :test-not #'eq))

(define-handler inputOutputPorts (object)
  (remove 'inputoutput (object-output-ports me)
          :key #'portType
          :test-not #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wiring ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Wire these ports together.
;;; There are two seperate cases:
;;;   Unidirectional wires:
;;;     wirePorts (fromPort toPort1 toPort2 toPort3...)
;;;     connects wires from fromPort to each of the toPorts
;;;     fromPort must be an OutputPort, the toPorts must be InputPorts
;;;   InputOutputPorts:
;;;     wirePorts (IOPort1 IOPort2 IOPort3 ...)
;;;     connects a ring of IOPorts.
;;;
;;; Note that all error checking is done before any wiring (so the user doesn't 
;;; have to deal with a half-wired situation if there's an error.)
;;; 
;;; Also note that this doesn't unwire anything previously wired.
(define-handler wirePorts (OutputPort toPort &rest moreToPorts)
  (let ((success t)
        (toPorts (cons toPort moreToPorts)))
    (dolist (eachToPort ToPorts)
      (unless (ports-wireable-check me eachToPort :warnp)
        (setq success nil)
        (return)))
    (when success 
      (setf (wiredTo me) (union toPorts (wiredTo me)))
      t)))


;;; returns t if wirable, nil otherwise
;;; checks for broadcast port
;;; checks if already wired
;;; checks if signatures are compatible
(defun ports-wireable-check (output-port input-port warnp)
  (let ((success     t)
        (output-sig  (signature output-port))
        (input-sig   (signature input-port))
        (op-wired-to (wiredto output-port)))
    (unless (inheritsfrom input-port InputPort)
      (unless warnp (return-from ports-wireable-check nil))
      (setq success nil)
      (warn "~a should be an InputPort" input-port))
    (if (listp op-wired-to)
      (when (member input-port op-wired-to)
        (unless warnp (return-from ports-wireable-check nil))
        (setq success nil)
        (warn "~a is already wired to ~a" output-port input-port))
      (when (eq op-wired-to 'broadcast)
        (unless warnp (return-from ports-wireable-check nil))
        (setq success nil)
        (warn "~a is a broadcast port." output-port)))
    (unless (compatible-sigs-p output-sig input-sig)
      (unless warnp (return-from ports-wireable-check nil))
      (setq success nil)
      (warn "Port with signature ~a can't be wired to a Port with signature ~a"
            output-sig input-sig))
    success))


(define-handler wirePorts (InputOutputPort toPort &rest toPorts)
  (dolist (eachToPort (cons toPort toPorts))
    (unless (inheritsFrom eachToPort InputOutputPort)
      (error "~a should be an InputOutputPort" eachToPort))
    (unless (portsCompatible me eachToPort)
      (error "Ports ~a and ~a have incompatible signatures" me eachToPort)))
  (labels ((wire-two-ports-internal (port-a port-b)
             (with-slots ((wired-to-a wiredTo)) port-a
               (with-slots ((wired-to-b wiredTo)) port-b
                 (cond ((and (null wired-to-a) (null wired-to-b))
                        (let ((wr (list 'WireRing port-a port-b)))
                          (setf wired-to-a wr)
                          (setf wired-to-b wr)))
                       ((eq wired-to-a wired-to-b)
                        nil)
                       ((and wired-to-a wired-to-b)
                        (let ((r (nconc (cdr wired-to-a) (cdr wired-to-b))))
                          (setf (cdr wired-to-a) r)
                          (setf (cdr wired-to-b) r)))
                       (t 
                        (when wired-to-a (rotatef port-a port-b))
                        (push port-a (cdr wired-to-b))
                        (setf wired-to-a wired-to-b))))))
           (wire-ports-internal (port-a port-b more-ports)
             (if more-ports
               (wire-ports-internal 
                (wire-two-ports-internal port-a port-b)
                (car more-ports)
                (cdr more-ports))
               (wire-two-ports-internal port-a port-b))))
    (wire-ports-internal me toPort toPorts)))



;; t is compatible with everything except nil
;;; nil is compatible with nuthin'
;;; otherwise, if we're compatible if to-sig inherits from from-sig
(defun compatible-sigs-p (from-sig to-sig)
  (case from-sig
    ((t)   (not (null to-sig)))
    ((nil) nil)
    (t     (or (eq from-sig to-sig) (inheritsfrom to-sig from-sig)))))


(define-handler portsCompatible (OutputPort InPort)
  (compatible-sigs-p (signature me) (signature InPort)))


(define-handler portsCompatible (InputOutputPort otherIOPort)
  (let ((sig1 (signature me))
        (sig2 (signature otherIOPort)))
    (and (compatible-sigs-p sig1 sig2)
         (compatible-sigs-p sig2 sig1))))


(define-handler autoWire (Object otherObject)
  (let ((outs (outputPorts me))
        (ins  (inputPorts otherObject))
        (ios1 (inputOutputPorts me))
        (ios2 (inputOutputPorts otherObject)))
    (dolist (out outs)
      (dolist (in ins)
        (and  (ports-wireable-check out in nil)
              (portsCompatible out in)
              (push in (wiredto out)))))
    (dolist (io1 ios1)
      (dolist (io2 ios2)
        (when (portsCompatible io1 io2)
          (wirePorts io1 io2))))))


;;; Sever the connection.
(define-handler unwirePorts (OutputPort toPort)
  (port-check-SK8-type toPort InputPort)
  (let ((wtfp (wiredTo me)))  
    (cond ((eq wtfp 'broadcast)
           (warn "Port ~a is a broadcast port." me))
          ((member toPort wtfp)
           (setf (slot-value me 'wiredto) (delete toPort  (slot-value me 'wiredto)))
           t)
          (t
           (warn "Ports ~a and ~a aren't wired." me toPort)))))

(define-handler unwirePorts (InputOutputPort toPort)
  (unwirePort me)
  (unwirePort toPort))

;;; Severs all wires from this Port.
(define-handler unwirePort (OutputPort)
  (setf (wiredto me) nil)
  t)


(define-handler unwirePort (InputPort)
  (dolist (oport (wiredTo me))
    (unwirePort oport))
  t)

;;; Note what's happening here.  Remove me from the shared list, 
;;; then set the local version to nil.
(define-handler unwirePort (InputOutputPort)
  (let ((wiredTo (wiredTo me)))
    (setf (cdr wiredTo) (delete me (cdr wiredTo) :test #'eq))
    (setValue 'wiredTo me nil))
  t)

(define-handler initializeFromStore (InputPort)
  (call-next-method)
  (let ((portObject (portObject me)))
    (when portObject
      (register-port-with-object portObject me (portType me)))))

(define-handler initializeFromStore (OutputPort)
  (call-next-method)
  (let ((portObject (portObject me)))
    (when portObject
      (install-outputty-port me))))

(define-handler findPort (object property portType &key resultEnsured)
  (let ((resultPort nil))
    ;; Find an existing port.
    (dolist (aPort (ports me))
      (when (and (eq (portType aPort) portType)
                 (eq (portProperty aPort) property))
        (setf resultPort aPort)
        (return)))
    (if resultEnsured
      (or resultPort
          (case portType
            (input (addInputPort me property))
            (output (addOutputPort me property))
            (inputOutput (addInputOutputPort me property))))
      resultPort)))

;;; 2 ports which are nice to cyclical clients.

(new inputPort :objectName "SafeInputPort" :feedbackAction 'stop
     :project sk8 :undisposable t)
(new outputPort :objectName "SafeOutputPort" :feedbackAction 'stop
     :project sk8 :undisposable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing out code...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(new Object :project sk8 :objectname "Bart" :properties '(a b c d))
(new Bart :objectname "Bart2" :project sk8)

(new Object :project sk8 :objectname "Lisa" :properties '(a b c d))
(new Lisa :objectname "Lisa2" :project sk8)
(dispose Bart)
(dispose Lisa)

(setq op1 (addOutputPort Bart2 'a :objectname "Port1"))
(setq ip1 (addInputPort Lisa2 'b  :objectname "Port2"))

(wirePorts op1 ip1)

(dispose op1)
(dispose ip1)

(inputports bart2)
(inputports Lisa2)
(ports bart2)
(ports Lisa2)
(detachport op1)
(detachport ip1)
(inspect op1)
(inspect ip1)

(setf (a Bart2) 14)
(b Lisa2)

;; try out feedback...

(setq op2 (addOutputPort Lisa2 'b :objectname "Port3"))
(setq ip2 (addInputPort Bart2 'a :objectname "Port4"))
(wirePorts op2 ip2)

(dispose ip2)
(dispose op2)
(inspect op2)
(inspect ip2)

(setf (checkforfeedback port3) t)
(setf (erroronfeedback Port1) t)


(setf (a Bart2) 17)
(b lisa2)

;;; try out InputOutputPorts
(setq iop1 (addInputOutputPort Bart2 'c))
(setq iop2 (addInputOutputPort Lisa2 'c))
(wirePOrts iop1 iop2)
(setf (c Bart2) 17)
(c lisa2)
(setf (c lisa2) 22)
(c Bart2)


||# 



#|
	Change History (most recent last):
	6	8/31/93	Brian Roddy	Updated ports file with changes since d3
				Still needs to be able to work with inheritance. 
				(inherited objects should have a duplicate of the
				port without it's wired connections).  Also needs
				to work with the store.  Now D4 compatible.
	7	9/20/93	rod	Fixed setter method so it works properly with
				inheritance.  Problem case was when parent had
				a ported property of the same name.  The around method called the parents setter which then tried
				to propagate the value. -BJR
	8	9/22/93	kleiman	member -> memq
	9	11/12/93	rod	changed install-outputty-port so that ports use
				after methods rather than around methods.  See comments there for reasons
	10	11/22/93	rod	
	11	12/2/93	rod	
	12	12/5/93	rod	Totally Redid ports for D4.  They no longer use a 
				hash table.  They are slower.  InputOutputPorts
				have been turned off.
	13	12/17/93	hernan	Removed some stuff like port filter that really slowed things down...
	14	1/11/94	till	Bring back the hash table!
	15	1/11/94	till	random stuff
	16	2/14/94	till	much improved feedback detection
	17	2/14/94	sidney	rename descendants to knowndescendants
	18	2/16/94	till	mapdescendants changed it's name to mapKnownDescendants
	19	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	20	2/28/94	hernan	Commenting out dispose.
	21	3/1/94	till	implement naming convention revisions, remove :before & :after keywords in the process.
	22	3/1/94	till	(should've taken care of broadcast? -> broadcasting while I was in the vicinity.)
	23	3/1/94	Brian	Fixed bug in install-outputty port.
				This code could not have been tested before
				being checked in.  This leaves me mad and
				tired at 10pm.  Don, please test your code. -Brian
	24	3/3/94	till	Make hash table weak.
	25	3/3/94	till	update *all-weak-hash-tables*
	26	3/3/94	rod	Taking out the (eval-when () (push porthash...
				because it stops the build.
	28	3/4/94	till	weak hash table should work better now
	29	3/4/94	Brian	make port-hash-table #'eq test, fix ioport check
	30	3/4/94	till	make ports handle virtual properties
	31	3/7/94	sidney	*all-weak*hash-tables* is now a population
				Temporarily force unnamed ports to be in parents' knownchildren, until we implement a better way
	32	3/8/94	till	InputPorts need caching, a bunch of cleanup, weak-hash-table list push.
	33	3/8/94	Don	fix to addinputport
	34	3/11/94	sidney	Remove Initialize handler for port that was a temporary workaround to the input port problem that Don fixed
	35	3/18/94	till	Bug 1151658, port feedback is a little too eager
				also sped up ports while I was in the area.
	36	5/4/94	till	FeedbackAction keywords be gone, bug 1160501.
	37	5/13/94	till	Improve Inputports (of object).
	38	6/9/94	chip	fixed obsoleted slot reference 'wired-to --> 'wiredTo in the wirePorts handlers
	39	6/9/94	chip	fixed init value for Port's feedbackAction slot to be stop (instead of 'stop)
	40	6/15/94	Hernan	Fixing removePorts to work when no type is
				specified. Problem was that the case clause 
				whose key is nil is never matched (nil is confused
				with a list of keys which is empty!!!). Oh well...
	41	6/16/94	till	I thought I nuked those portType keywords before.
	42	6/16/94	till	portType keywords begone (one last thing).
	43	6/17/94	till	Robustify uniwiring of InputOutputPorts, bug 1168868.
	44	6/22/94	till	Fix the case statement on removePorts.
	45	7/6/94	till	Hernan sez that ports should transmit the property's 
				new value (as upposed to the value we initially set 
				it to... the two could be different).  That makes sense.
	46	7/14/94	till	Oops, :broadcast is still a keyword.  Fix it.
	47	8/5/94	till	attachPort of inputPort is a disaster.  Fixitquick.
				(also attachPort of OutputPort).
	48 	 9/29/94	chip    	now maintains the "has-ports?" object-flags bit
	49 	10/ 7/94	chip    	added initialize method
	50 	10/10/94	chip    	added update-ports-from-parents
	51 	10/10/94	It      	update-ports-from-parents now deals with non-SK8 parent-classes
	52 	10/17/94	sidney  	helpfindmychild function for loading unnamed port objects
	53 	10/21/94	chip    	update-ports-from-parents now uses do-SK8-parents
	54 	10/24/94	chip    	install-outputty-port uses setf-function-name (instead of setter-for-getter)
	55 	10/26/94	sidney  	allow non-nil non-symbol class-names for sk8 classes
	56 	 2/27/95	sidney  	initialize wiring of ports when loading from saved project
	57 	 3/ 1/95	sidney  	force recompile of this file because a macro has been changed
	58 	 3/29/95	rod     	fixing bug in unregister-port-with-object.
	59 	 4/ 5/95	till    	Bug 1236200, port feedback detection is checking for feedback around
							'objects' instead of around 'ports'.
	60 	 4/24/95	rod     	Making functions to clean up the port tables on
							close project.
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 5/23/96	sidney  	define find-port and safe ports here
	4  	 8/ 8/96	Hernan  	Fixed pleaseFindMyChild to conform to the new SK8 class structure (symbols for classes).
	5  	 8/ 9/96	Hernan  	Making pleaseFindMyChild use findPort if possible.
	2  	11/15/96	Hernan  	Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
