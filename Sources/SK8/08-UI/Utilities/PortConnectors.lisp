;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :UIDev)

(defvar notoksocketcolor red)
(defvar oksocketcolor uiColorPlugTool)
(defvar notokplugcolor uicolorportplugged)
(defvar okplugcolor uicolorportunplugged)

(new uibiglabel :objectname "PortProxyProto" :project ui
     :properties '((Port :inherit :value nil)))

(setf (mouseSensitivity PortProxyProto) 'normal)
(setf (textlocation PortProxyProto) 'centerleft) ;; HEW
(setf (fillcolor PortProxyProto) White)
(setf (framesize PortProxyProto) '(1 1))
(setf (texthoffset PortProxyProto) 15)
(new rectangle :objectname "PortProxyProtoColorRect" :size '(20 20) :project ui)
(setf (container PortProxyProtoColorRect) PortProxyProto)
(setf (mousesensitivity PortProxyProtoColorRect) 'transparent) ;; HEW
(tagpart PortProxyProto PortProxyProtoColorRect 'DisplayRect)

(new PortProxyProto :objectname "InputPortProxy" :project ui)
(setf (fillcolor (DisplayRect InputPortProxy)) okplugcolor)
(define-handler resized (InputPortProxy)
  (sk8-multival-bind (hh vv) (size me)
                     (setboundsrect (displayrect me) 0 0 vv vv)))

(new PortProxyProto :objectname "OutputPortProxy" :project ui)
(setf (fillcolor (DisplayRect OutputPortProxy)) oksocketcolor)
(define-handler resized (OutputPortProxy)
  (sk8-multival-bind (hh vv) (size me)
                     (setboundsrect (displayrect me) 0 0 vv vv)))

(define-handler initialize (InputPortProxy original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (push me (knownchildren InputPortProxy))
  )
(define-handler initialize (OutputPortProxy original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (push me (knownchildren OutputPortProxy))
  )

(defun DrawWires (outs ins)
  (let (wires 
        pos in
        (inports (mapcar #'port ins))
        )
    (dolist (o outs)
      (setf wires (wiredto (port o)))
      (dolist (w wires)
        (setf pos (position w inports))
        (when (and pos (neq (sk8::window (portobject w)) (sk8::window (portobject (port o))))) 
          (setf in (nth pos ins))
          (DrawXORLine (right o :physical t) (v o :physical t) 
                       (left in :physical t) (v in :physical t)))))))

(define-handler mousedown (InputPortProxy)
  ; (sleep 0.1)
  (when (down mouse)
    (let* ((hh (left me :physical t))
           (vv (v me :physical t))
           (p (port me))
           (OutputPortProxies (remove-if #'(lambda (x) (eq (container x) nil))
                                         (knownchildren outputportproxy)))
           (InputPortProxies (remove-if #'(lambda (x) (eq (container x) nil))
                                        (knownchildren Inputportproxy)))
           (compatiblePortProxies (remove-if-not #'(lambda (x) (portscompatible (port x) p)) OutputPortProxies))
           (wires (wiredto p))
           (incompats (remove-if #'(lambda (x) (memq x compatibleportproxies)) OutputPortProxies))
           ConnectToGuy)
      ;;;(declare (ignore inputportproxies))
      (withactorlocked (me)
        (dolist (i incompats)
          (setf (fillcolor (DisplayRect i)) notoksocketcolor))
        (dolist (i compatiblePortProxies)
          (setf (fillcolor (DisplayRect i)) oksocketcolor)))
      (DrawWires OutputPortProxies InputPortProxies)
      (setf ConnectToGuy (PortRubberBand compatiblePortProxies :hanchor hh :vanchor vv))
      (withCursor watchcursor
        (DrawWires OutputPortProxies InputPortProxies)
        (when ConnectToGuy
          (if (memq (port connecttoguy) wires) 
            (progn
              (play zoomdownsound)
              (unwirePorts (port connecttoguy) (port me))
              (if (eq (container connecttoguy) (container me))
                (disconnect me connecttoguy)
                (messagetouser "The ports have been unwired."))
              )
            (progn
              (play zoomupsound)
              (wirePorts (port connecttoguy) (port me))
              (if (eq (container connecttoguy) (container me))
                (connect me connecttoguy :theconnector portproxyconnector)
                (messagetouser "The ports have been wired together, but as they are in different windows, no connector will be visible.  To unwire them, redrag a line between them."
                               ))
              )))
        (withactorlocked (me)
          (dolist (i (append incompats compatiblePortProxies))
            (setf (fillcolor (DisplayRect i)) oksocketcolor)  ; *** should be ui::uicolorplugtool
            (forceredraw i)))))))

(define-handler mousedown (OutputPortProxy)
  ;;(sleep 0.1)
  (when (down mouse)
    (let* ((hh (right me :physical t))
           (vv (v me :physical t))
           (p (port me))
           (OutputPortProxies (remove-if #'(lambda (x) (eq (container x) nil)) (knownchildren outputportproxy)))
           (InputPortProxies (remove-if #'(lambda (x) (eq (container x) nil))
                                        (knownchildren Inputportproxy)))
           (compatiblePortProxies (remove-if-not #'(lambda (x) (portscompatible p (port x) )) InputPortProxies))
           (wires (wiredto p))
           (incompats (remove-if #'(lambda (x) (memq x compatibleportproxies)) InputPortProxies))
           ConnectToGuy)
      (withActorLocked ((sk8::window me))
        (dolist (i incompats)
          (setf (fillcolor (DisplayRect i)) notokplugcolor))
        (dolist (i compatiblePortProxies)
          (setf (fillcolor (DisplayRect i)) okplugcolor))
        )
      (DrawWires OutputPortProxies InputPortProxies)
      (setf ConnectToGuy (PortRubberBand compatiblePortProxies :hanchor hh :vanchor vv))
      (withActorLocked ((sk8::window me))
        (withCursor watchcursor
          (DrawWires OutputPortProxies InputPortProxies)
          (when ConnectToGuy
            (if (memq (port connecttoguy) wires) 
              (progn
                (play zoomdownsound)
                (unwirePorts (port me) (port connecttoguy) )
                (if (eq (container connecttoguy) (container me))
                  (disconnect me connecttoguy)
                  (messagetouser "The ports have been unwired."))
                )
              (progn
                (play zoomupsound)
                (wirePorts (port me) (port connecttoguy)  )
                (if (eq (container connecttoguy) (container me))
                  (connect me connecttoguy :theconnector portproxyconnector)
                  (messagetouser "The ports have been wired together, but as they are in different windows, no connector will be visible.  To unwire them, redrag a line between them."
                                 ))
                )))
          (dolist (i (append compatiblePortProxies incompats))
            (setf (fillcolor (DisplayRect i)) okplugcolor) ; *** ui::uicolorportunplugged
            (forceredraw i)))
        ))))


(defun PortRubberBand (PotentialConnects &key (hanchor (gs:f.round (eventH)))
                                            (vAnchor (gs:f.round (eventV)))
                                            (width 3)
                                            (height 3))
  ;; error checking the pen size: positive integers only!
  (setf width (gs:f.round width)
        height (gs:f.round height))
  (unless (plusp width) (setf width 3))
  (unless (plusp height) (setf height 3))
  ;; Ready? Go!
  (gs:let+ ((endx hanchor) (endy vAnchor) oldx oldy thedude lastdude (theRect (:rect)))
    (rlet ((old-pen :penstate))
      (unwind-protect
        (progn
          (#_SetRect theRect -3000 -3000 3000 3000)
          (with-port (gs:get-wmgr-port)
            (#_ClipRect theRect)
            (#_getpenstate old-pen)
            (#_PenSize width height)
            (#_PenPat *black-pattern*)
            (#_PenMode #$patxor)
            (#_MoveTo hanchor vAnchor)
            (#_LineTo hanchor vAnchor))
          (loop
            (unless (mouse-down-p) (return))
            (setf oldx endx oldy endy)
            (SK8-multival-setf (endx endy) (mouseLoc stage))
            (unless (and (= oldx endx) (= oldy endy))
              (with-port (gs:get-wmgr-port)
                ;; Set graphics state. 
                (#_PenSize width height)
                (#_PenPat *black-pattern*)
                (#_PenMode #$patxor)
                ;; erase
                (#_MoveTo hanchor vAnchor)
                (#_LineTo oldx oldy)
                ;; draw
                (#_MoveTo hanchor vanchor)
                (#_LineTo endx endy))
              (when lastdude (forceredraw lastdude)))
            (setf thedude (finddropobject endx endy))
            (when (memq thedude potentialconnects)
              (setf (Highlight thedude) (not (Highlight thedude)))
              (setf (Highlight thedude) (not (Highlight thedude)))
              (setf lastdude thedude)
              )))
        ;; erase the line
        (with-port (gs:get-wmgr-port)
          (#_ClipRect theRect)
          (#_PenSize width height)
          (#_PenPat *black-pattern*)
          (#_PenMode #$patxor)   
          (#_MoveTo hanchor vAnchor)
          (#_LineTo endx endy)
          (#_SetPenState old-pen))))
    ;; return the end point.
    (if (memq thedude potentialconnects) thedude nil)))

(defun getEditLayer (me layertype)
  (let ((res nil))
    (dolist (i (contents me))
      (if (inheritsfrom i layertype)
        (setf res i)))
    res))

(new Rectangle :objectname "PortEditLayer" :project ui)
(new complexRGBColor :objectname "DarkerRed" :project ui)
(setf (forered darkerred) 0)
(setf (foreblue darkerred) 50000)
(setf (foregreen darkerred) 50000)
(SETF (PENMODE darkerred) 'SUBPIN)
(setf (fillcolor porteditlayer) darkerred)
(define-handler mousedown (porteditlayer) t)
(define-handler mouseup (porteditlayer) t)


(new connector :objectname "PortProxyConnector" :project ui)
(setf (fillcolor portproxyconnector) green)
(setf (framecolor portproxyconnector) green)
(setf (linesize portproxyconnector) 5)
(setf (framesize portproxyconnector) '(0 0))
(define-handler activateText (portProxyConnector)
  (setf (Fillcolor me) yellow)
  (setf (framecolor me) yellow)
  (call-next-method))

(define-handler deactivateText (portProxyConnector)
  (setf (Fillcolor me) green)
  (setf (framecolor me) green)
  (call-next-method))

(define-handler keydown (portProxyConnector theChar)
  (if (eq thecHar #\delete)
    (progn
      (unwireports (port (startactor me)) (port (endactor me)))
      ;; (dispose me)
      (setf (container me) nil))
    (call-next-method)))

(define-handler mousedown (portProxyConnector)
  (withActorLocked (me) 
    (setf (keytarget (sk8::window me)) me))
  )

(define-handler doubleclick (portProxyConnector)
  (messagetouser (concatenate 'string
                              "This wire connects " (objectstring (port (startactor me)))
                              " to " (objectstring (port (endactor me))))))

(defparameter PortProxyTable (make-hash-table :test #'eq))

(define-handler AddEditLayer (PortEditLayer TheActor)
  (let* ((edla (new me :project ui)) 
         curval bounds
         (topbounds (boundsrect theactor))
         (tt (second topbounds))
         (hh (height inputportproxy))
         ins outs
         dc
         allouts allins)
    (withActorlocked (TheActor)
      (RemoveEditLayer me TheActor)
      (setf dc (deepcontents TheActor))
      (setBoundsRect edla 0 0 (width TheActor) (height TheActor))
      (setf (container edla) TheActor)
      (dolist (acts (cons TheActor dc))
        (setf bounds (boundsrect acts :physical t))
        (setf curval (- (second bounds) tt ))
        (setf ins nil outs nil)
        (dolist (i (remove-if-not #'portobject (remove-if-not #'portproperty (inputports acts)))) ;;(remove-if #'(lambda (x) (or (eq (portproperty x) 'verticalscroll) (eq (portproperty x) 'currentvalue))) (inputports acts)))
          (setf (gethash i PortProxyTable) (new inputportproxy :port i :container edla 
                                                :text (simpleobjectstring (portproperty i)) 
                                                :project ui))
          (setf (top (gethash i PortProxyTable) :resizing nil) curval)
          (incf curval hh)
          (incf curval 5)
          (push (gethash i PortProxyTable) ins))
        (align ins :alignlocation 'left :relativeactor acts)
        (setf curval (- (fourth bounds) tt))
        (dolist (i (remove-if-not #'portobject (remove-if-not #'portproperty (outputports acts)))) ;;;(remove-if #'(lambda (x) (or (eq (portproperty x) 'verticalscroll) (eq (portproperty x) 'currentvalue))) (outputports acts)))
          (setf (gethash i PortProxyTable) (new outputportproxy :container edla :port i :text (simpleobjectstring (portproperty i)) :project ui))
          (setf (bottom (gethash i PortProxyTable) :resizing nil) curval)
          (decf curval hh)
          (decf curval 5)
          (push (gethash i PortProxyTable) outs)
          )
        (setf allins (append allins ins ))
        (setf allouts (append allouts outs ))
        (align outs :alignlocation 'right :relativeactor acts))
      (let (wires pos in
                  (inports (mapcar #'port allins)))
        (dolist (o allouts)
          (setf wires (wiredto (port o)))
          (dolist (w wires)
            (setf pos (position w inports))
            (when pos 
              (setf in (nth pos allins))
              (connect o in :theconnector portproxyconnector))))))
    edla))


(define-handler RemoveEditLayer (PortEditLayer TheActor)
  (let ((edla (geteditlayer TheActor me)))
    (when edla 
      (setf (container edla) nil)
      (dolist (i (deepcontents edla))
        (if (sk8::is-a i portproxyproto)
          (remhash (port i) PortProxyTable)
          ))
      )
    ))

(new rectangle :objectname "PortEditLayerButton" :project ui
     :properties `(editedProject
                   editlayer
                   OnColor
                   OffColor))
(setf (onColor PortEditLayerButton) Red)
(setf (OffColor PortEditLayerButton) lightRed)

(setframesize PortEditLayerButton 2 2)
(setf (oncolor PortEditLayerButton) Red)
(setf (offcolor PortEditLayerButton) oksocketcolor)
(setf (fillcolor PortEditLayerButton) (offcolor PortEditLayerButton))
(setf (framecolor PortEditLayerButton) uirectangleoutbevel)

(define-handler mousedown (PortEditLayerButton)
  (withactorlocked (me)
    (setf (knownchildren inputportproxy) nil)
    (setf (knownchildren outputportproxy) nil)
    (if (eq (fillcolor me) (offColor me))
      (withCursor watchcursor 
        (deselect selectionhalo)
        (let ((con (contents stage)))
          (if (editedProject me)
            (setf con (remove-if #'(lambda (x) (neq (project x) (editedProject me))) con)))
          (setf con (remove-if #'(lambda (x) (eq x (sk8::window me))) con))
          (setf (editlayer me)
                (mapcar #'(lambda (x) (AddEditLayer porteditlayer x)) con))
          (if (editlayer me)
            (progn
              (setf (framecolor me) uirectangleinbevel)
              (setf (fillcolor me) (oncolor me))
              (setf (invokedWith PortEditLayerMode) me)
              (entermode porteditlayermode))
            (progn
              (messagetouser "No Ports Found!" :beep t)
              (setf (invokedWith PortEditLayerMode) nil)
              ))))
      (withCursor watchcursor 
        ;; (mapcar #'dispose (editlayer me))
        (dolist (i (editLayer me))
          (RemoveEditLayer i (sk8::window i)))
        (setf (invokedWith PortEditLayerMode) nil)
        (setf (editlayer me) nil)
        (setf (framecolor me) uirectangleoutbevel)
        (setf (fillcolor me) (offColor me))
        (exitmode porteditlayermode)))))

(new eventmode :objectname "PortEditLayerMode" :project ui
     :properties '(invokedWith))

(define-handler handleKeydown (PortEditLayerMode)
  (if (eq (gs:getEventKey) #\escape)
    (progn
      (mousedown (invokedWith me))
      (exitmode me)
      t)
    (call-next-method)))

(define-handler handlekeyup (PortEditLayerMode)
  (declare (ignore me))
  t)

(define-handler handleidle (PortEditLayerMode)
  (setf (Cursor stage) cursorcrosshair)
  t)

(define-handler handleMousedown (PortEditLayerMode)
  (sk8-multival-bind (hh vv) (mouseloc stage)
    (let ((theactor (finddropobject hh vv)))
      (if (or (inheritsfrom theactor portProxyConnector)
              (inheritsfrom theactor PortProxyProto)
              (inheritsfrom theactor PortEditLayer)
              (inheritsfrom theactor PortEditLayerButton))
        (call-next-method)
        (progn
          (if (eq (project theactor) ui)
            (progn
              (mousedown (invokedWith me))
              (exitmode me))      
            (ed-beep))
          t)))))

(define-handler handleMouseup (PortEditLayerMode)
  (sk8-multival-bind (hh vv) (mouseloc stage)
    (let ((theactor (finddropobject hh vv)))
      (if (or (inheritsfrom theactor portProxyConnector)
              (inheritsfrom theactor PortProxyProto)
              (inheritsfrom theactor PortEditLayer)
              (inheritsfrom theactor PortEditLayerButton))
        (call-next-method)
        t))))

(define-handler enterMode (PortEditLayerMode)
  (deselect selectionhalo)
  (call-next-method))

(define-handler exitMode (PortEditLayerMode)
  (call-next-method)
  (setf (cursor stage) standardcursor)
)








#|
	Change History (most recent last):
	1	9/20/93	rod	
	2	9/20/93	rod	
	3	9/20/93	rod	
	4	9/23/93	kleiman	duplicate -> new & package qualification errors
	5	10/13/93	hernan	quickdrawcolor -> ComplexRGBColor.
	6	10/20/93	rod	
	7	10/29/93	rod	
	8	11/8/93	rod	
	9	11/10/93	rod	
	10	12/21/93	sidney	Changes so files can be compiled
	12	2/12/94	kleiman	name changes
	13	2/14/94	sidney	rename descendants to knowndescendants
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/21/94	hernan	window -> sk8::window.
	16	2/22/94	hernan	Declaring some symbols public. This is necessary
				because these objects are created by duplicating
				old objects and thus are not cought by the
				scanner.
	17	2/22/94	hernan	Wouldn't you like to know...
	18	2/25/94	hernan	Using symbols instead of keywords for options!!!
	19	2/26/94	rod	
	20	2/26/94	rod	Got rid of a backquote properties definition.
	21	2/28/94	hernan	Avoiding calling dispose directly.
	22	2/28/94	hernan	Avoiding calling dispose directly.
	22	2/28/94	hernan	Second try!
	23	3/3/94	rod	
	25	3/5/94	rod	addproperty avoided wherever possible
	26	3/5/94	rod	
	27	3/8/94	rod	
	28	3/16/94	rod	
	29	3/17/94	rod	removing print
	30	3/30/94	Hernan	In portRubberBand, avoiding doing stuff within
				the wmgr-port where possible.
	31	4/13/94	Hernan	Avoiding use of contents when possible.
	32	4/26/94	rod	
	33	6/3/94	rod	
	34	6/3/94	rod	
	35	6/3/94	rod	
	36	6/22/94	Hernan	1169994: Setting the mouseSensitivity of
				portProxyProto to 'normal. (The mouseSensitivity
				became transparent when uibiglabel became
				transparent, which broke the port connectors
				stuff).
	37	6/23/94	rod	Making sure halo is deselected on enterMode.
	38	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	39	7/21/94	rod	Fixing bug of not showing scroller ports.
	40 	 9/ 1/94	Hernan  	The penmode of ComplexRGBColor is now stored
							as a symbol.
	41 	 2/16/95	sidney  	readable argument names for initialize handler
	42 	 3/29/95	rod     	dealing with bogus ports.
	43 	 4/25/95	rod     	Fixing YesOrNoDialog and MessageToUser calls to 
							not set the heights so it can be computed 
							manually.
	2  	 2/14/96	Brian   	removing gs:docontents
	2  	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	3  	10/ 9/96	Hernan  	Need to reset the xor pattern every time we redraw.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
