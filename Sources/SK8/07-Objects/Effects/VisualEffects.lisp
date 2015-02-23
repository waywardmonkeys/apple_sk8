;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright 1991 by Hernan Epelman-Wang for Apple Computer, Inc.
;;; Advanced Technology Group
;;;

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "VISUALEFFECTS")

#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
01-22-93 ruben id -> objectName
11-11-92 hernan took the dashes out of the names of the effects.
20-09-92 hernan changing this to work with MF VI
08-25-92 hernan visual effects are objects now. (and macs are not Crays yet!)
05-12-92 ruben d29 conversion

|#

#|

This test function runs through the defined effects changing the color of actor.

(defun test-visual-effects (actor)
  (dolist (effect (children visualEffect))
    (gs:with-visual-effect (actor effect :fast)
      (setf (fillcolor actor) green))
    (gs:with-visual-effect (actor effect :fast)
      (setf (fillcolor actor) Red))))

|#

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                      MAIN FUNCTIONS
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new effect :objectName "VisualEffect" :undisposable t :project sk8)

;;; A speed keyword is added to the main function. The argument which is optional has a default
;;; value of 'normal. The other values are 'fast and 'slow. This keyword is passed to the effect functions
;;; which interpret speed in slightly different ways. Speed is implemented by changing the delays
;;; the effect functions use. Delays are empty dotimes loops.

;;; A when clause is added to catch a case in which NIL is passed to specify that no
;;; speed keyword was given. In such a case, the default is assigned.

(defun sk8::visual-effect-blit (visual-effect gworld clos-window dither blit-region
                                            &optional speed)
  (declare (ignore blit-region))
  (when (null speed) (setq speed 'normal))
  (if (inheritsFrom visual-effect visualEffect)
    (effect-gWorld-to-Window clos-window gWorld dither speed visual-effect)
    (if dither             
      (gs:gWorld-to-window gworld clos-window t)
      (gs:gWorld-to-window gworld clos-window))))

(defun effect-gWorld-to-Window (window gWorld dither speed visual-effect)
  (with-port (gs:get-wmgr-port)
    (gs:lock-gworld gWorld)
    (unwind-protect
      (transfer-bitmap visual-effect
                       (gs:gworld-pixmap gWorld)
                       (gs:window-pixmap window)
                       (gs:gworld-rectangle gworld)
                       (gs:window-rectangle window)
                       (if dither 64 8)
                       speed
                       (rref (wptr window) :cGrafPort.visRgn))
      (gs:unlock-gworld gWorld))))

(defmacro |WITH VISUALEFFECT| ((theEffect &key on speed) &body body)
  (unless on (error "Keyword \"on\" is reguired in With VisualEffect calls"))
  `(withActorLocked (,on :effect ,theEffect :speed ,speed)
     ,@body))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                     GENERAL HELPERS
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(defun hang-on (miliseconds)
  (let ((target-time (+ miliseconds (get-internal-real-time))))
    (do ()
        ((> (get-internal-real-time) target-time) t))))

(defun round-up (dividend divisor)
  (multiple-value-bind (quot rem) (round dividend divisor)
    (if (minusp rem)
      (1- quot)
      quot)))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                    INDIVIDUAL EFFECTS: which ones are available and what are they?
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

#|

[1] Wipes. 
    Instances: wipe-up wipe-down wipe-left wipe-right
    Speeds   : Three speeds are available. This is a very fast effect!
    Comments : A well known effect. If a window is unlocked with a wipe-<direction>
               effect, the next card gets progressively unveiled in the direction
               <direction>.

[2] random-rows.
    Instances: random-rows
    Speeds   : Three speeds are available. A fast effect.
    Comments : The next bitmap is divided into a number of horizontal regions which
               get copied onto the destination in random order.

[3] Venetian Blinds.
    Instances: open-venetians, close-venetians.
    Speeds   : Three speeds available. A fast effect.
    Comments : typical venetian blind effects.

[4] Tiles.
    Instances: serial-tile random-tile incentric-tile
    Speeds   : serial and random tile are very fast. Incentric tile is rather slow
               and discontinuous. Three speeds are available.
    Comments : The next bitmap is divided into small square regions called tiles. The
               effects differ in the order in which the tiles are copied to the 
               destination (randomly in random-tile, one by one by column starting from
               the bottom left in serial-tile, and in concentric frames of tiles from the
               outside in in incentric-tile). Incentric tile has been superseeded by
               close iris and thus it is not available any more.

[5] Pushes.
    Instances: push-left push-right push-up push-down
    Speeds   : Three speeds are available. These effects are not very fast. Running in
               "fast" is not as fast as you might expect.
    Comments : the current card is pushed out of the window by the next card. The direction
               specifies where the next card will be pushed to.

[6] Dissolve.
    Instances: dissolve
    Speeds   : It is rather slow. The normal speed is tolerable. Speed depends on how many
               "blends" are done. The fast mode does only two blends and thus the effect is
               not very impressive.
    Comments : Dissolve is done by "blending" the next pixmap onto the current one with
               increasing weight on the new pixmap. Copying in blend mode is extremely slow.
               (The copy-deep-mask method is just as slow.)

[7] Stretches.
    Instances: zoom-in flip-left flip-right flip-up flip-down
    Speeds   : These effects are quite slow. This is because resizing a bitmap is a
               very expensive opertation. Only one speed is available and speed args
               are ignored.
    Comments : All these effects involve resizing the new image in some way. Left, right,
               bottom and top only stretch from one side of the immage. The image is
               stretched to the specified direction. In flip-down, for example,
               the new card starts flat at the top of the window and it grows towards
               the bottom.

               zoom-in is cool. The image starts at the center of the window and it
               stretches in all directions towards the ends of the window. It seems to be
               comming at you.

[8] fan-house.
    Instances: fan-house-left fan-house-right fan-house-down fan-house-up
    Speed    : A reasonably speedy effect. Three speeds are provided.
    Comments : Simply put, in this effect, a few copies of the new card collapse into
               each other.

[9] Slides.
    Instances: slide-left slide-right slide-down slide-up
    Speeds   : A reasonably fast effect. Three speeds are provided.
    Comments : the next card slides itme on top of the old card. It slides in the
               direction of the effect (eg slide-up slides upwards).

[10] Iris.
     Instances: open-iris
     Speeds   : This is a fast effect. Three speeds are provided.
     Comments : The new image appears in the center of the window as the iris opens.
                It opens to cover the whole window.

|#

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                         Dissolve
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

;;; NOTE: These are two attempts at the elusive dissolve visual effect.
;;; The algorithms are the two algorithms that appear in the "A digital
;;; dissolve effect" chapter of GRAPHIC GEMS. The method of generating the
;;; pseudo random sequence of integers is extremely fast and quite nice.
;;; What seems to really slow down these algorithms is the great number of
;;; calls to copy-bits that we need to do.

#|

(defun dissolve-slow-copy-bits (src-bitmap dest-bitmap src-rect dest-rect mode speed
                                         &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed src-rect))
  (gs:let+ ((height 64)
         (mask 3232)
         (right (rref dest-rect :rect.right))
         (bottom (rref dest-rect :rect.bottom))
         (hint (round-up right 64))
         (vint (round-up bottom 64))
         (rr (:rect)))
    (do ((row (truncate 1 64) (truncate seq 64))
         (col (mod 1 64) (mod seq 64))
         (seq (logxor (ash 1 -1) mask)
              (if (zerop (logand seq 1))
                (ash seq -1)
                (logxor (ash seq -1) mask))))
        ((= 1 seq) (progn (rset rr :rect.left 0)
                          (rset rr :rect.right hint)
                          (rset rr :rect.top 0)
                          (rset rr :rect.bottom vint)
                          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)))
      (when (< row height)
        (let ((lft (* col hint))
              (top (* row vint)))
          (rset rr :rect.left lft)
          (rset rr :rect.right (min (+ lft hint) right))
          (rset rr :rect.top top)
          (rset rr :rect.bottom (min (+ top vint) bottom))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))))))

;;; Having run out of possible optimizations, I think I will abandon dissolve for now.
;;; The original running time was 60 secs. After all the optimizations it is at 40 secs.

(defun dissolve-copy-bits (src-bitmap dest-bitmap src-rect dest-rect mode speed
                                         &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed src-rect))
  (gs:let+ ((height 64)
         (width 64)
         (mask 3232)
         (colmask (1- (ash 1 6)))
         (right (rref dest-rect :rect.right))
         (bottom (rref dest-rect :rect.bottom))
         (hint (round-up right 64))
         (vint (round-up bottom 64))
         (the-mode (gs:mode-arg mode))
         (rr (:rect)))
    (with-pointers ((sb src-bitmap)
                    (db dest-bitmap))
      (do ((row (ash 1 -6) (ash seq -6))
           (col (logand 1 colmask) (logand seq colmask))
           (seq (logxor (ash 1 -1) mask)
                (if (zerop (logand seq 1))
                  (ash seq -1)
                  (logxor (ash seq -1) mask))))
          ((= 1 seq) (progn (rset rr :rect.left 0)
                            (rset rr :rect.right hint)
                            (rset rr :rect.top 0)
                            (rset rr :rect.bottom vint)
                            (#_copyBits sb db rr rr the-mode mask-rgn)))
        (when (and (< row height) (< col width))
          (let ((lft (* col hint))
                (top (* row vint)))
            (rset rr :rect.left lft)
            (rset rr :rect.right (min (+ lft hint) right))
            (rset rr :rect.top top)
            (rset rr :rect.bottom (min (+ top vint) bottom))
            (#_copyBits sb db rr rr the-mode mask-rgn)))))))

(defun gen-rect-and-copy (row col hint vint right bottom
                                 src-bitmap dest-bitmap mode mask-rgn)
  (let* ((lft (* col hint))
         (rght (min (+ lft hint) right))
         (top (* row vint))
         (bttm (min (+ top vint) bottom)))
    (rlet ((rr :rect :left lft :right rght :top top :bottom bttm))
      (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))))

|#

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                        Stretches
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "FlipDown" :project sk8)

(define-handler transfer-bitmap (FlipDown src-bitmap dest-bitmap 
                                                     src-rect dest-rect mode speed
                                                     &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let* ((left (rref dest-rect :rect.left))
         (right (rref dest-rect :rect.right))
         (top (rref dest-rect :rect.top))
         (bottom (rref dest-rect :rect.bottom))
         (int (max 1 (round-up bottom 25))))
    (rlet ((st-rect :rect))
      (do* ((stretch-bottom (+ top int) (+ stretch-bottom int)))
           ((>= stretch-bottom bottom)
            (progn (rset st-rect :rect.bottom bottom)
                   (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
        (rset st-rect :rect.left left)
        (rset st-rect :rect.top top)
        (rset st-rect :rect.right right)
        (rset st-rect :rect.bottom stretch-bottom)
        (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))))

(new visualEffect :objectName "FlipUp" :project sk8)

(define-handler transfer-bitmap (FlipUp src-bitmap dest-bitmap 
                                                   src-rect dest-rect mode speed
                                                   &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let* ((left (rref dest-rect :rect.left))
         (right (rref dest-rect :rect.right))
         (top (rref dest-rect :rect.top))
         (bottom (rref dest-rect :rect.bottom))
         (int (max 1 (round-up bottom 25))))
    (do* ((stretch-top (- bottom int) (- stretch-top int)))
         ((<= stretch-top top)
          (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
            (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
      (rlet ((st-rect :rect :left left :right right :top stretch-top :bottom bottom))
        (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))))

(new visualEffect :objectName "FlipLeft" :project sk8)

(define-handler transfer-bitmap (FlipLeft src-bitmap dest-bitmap 
                                                     src-rect dest-rect mode speed
                                                     &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let* ((left (rref dest-rect :rect.left))
         (right (rref dest-rect :rect.right))
         (top (rref dest-rect :rect.top))
         (bottom (rref dest-rect :rect.bottom))
         (int (max 1 (round-up right 25))))
    (do* ((stretch-left (- right int) (- stretch-left int)))
         ((<= stretch-left left)
          (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
            (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
      (rlet ((st-rect :rect :left stretch-left :right right :top top :bottom bottom))
        (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))))

(new visualEffect :objectName "FlipRight" :project sk8)

(define-handler transfer-bitmap (FlipRight src-bitmap dest-bitmap 
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let* ((left (rref dest-rect :rect.left))
         (right (rref dest-rect :rect.right))
         (top (rref dest-rect :rect.top))
         (bottom (rref dest-rect :rect.bottom))
         (int (max 1 (round-up right 25))))
    (do* ((stretch-right (+ left int) (+ stretch-right int)))
         ((>= stretch-right right)
          (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
            (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
      (rlet ((st-rect :rect :left left :right stretch-right :top top :bottom bottom))
        (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))))

(new visualEffect :objectName "ZoomIn" :project sk8)

(define-handler transfer-bitmap (ZoomIn src-bitmap dest-bitmap 
                                                     src-rect dest-rect mode speed
                                                     &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let* ((left (rref dest-rect :rect.left))
         (right (rref dest-rect :rect.right))
         (top (rref dest-rect :rect.top))
         (bottom (rref dest-rect :rect.bottom))
         (int (max 1 (round-up right 50)))
         (midpoint (round-up right 2))
         (midhigh (round-up bottom 2))
         (inthigh (max 1 (round-up bottom 50))))
    (do* ((stretch-left (- midpoint int) (- stretch-left int))
          (stretch-right (+ midpoint int) (+ stretch-right int))
          (stretch-top (- midhigh inthigh) (- stretch-top inthigh))
          (stretch-bottom (+ midhigh inthigh) (+ stretch-bottom inthigh)))
         ((or (>= stretch-right right) (<= stretch-left left))
          (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
            (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
      (rlet ((st-rect :rect :left stretch-left :right stretch-right 
                      :top stretch-top :bottom stretch-bottom))
        (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))))

(new visualEffect :objectName "OpenIris" :project sk8)

(define-handler transfer-bitmap (OpenIris src-bitmap dest-bitmap 
                                                       src-rect dest-rect mode speed
                                                       &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 400) (fast 1) (otherwise 50))))
    (let* ((left (rref dest-rect :rect.left))
           (right (rref dest-rect :rect.right))
           (top (rref dest-rect :rect.top))
           (bottom (rref dest-rect :rect.bottom))
           (int (max 1 (round-up right 50)))
           (midpoint (round-up right 2))
           (midhigh (round-up bottom 2))
           (inthigh (max 1 (round-up bottom 50))))
      (do* ((stretch-left (- midpoint int) (- stretch-left int))
            (stretch-right (+ midpoint int) (+ stretch-right int))
            (stretch-top (- midhigh inthigh) (- stretch-top inthigh))
            (stretch-bottom (+ midhigh inthigh) (+ stretch-bottom inthigh)))
           ((or (>= stretch-right right) (<= stretch-left left))
            (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
              (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
        (rlet ((st-rect :rect :left stretch-left :right stretch-right 
                        :top stretch-top :bottom stretch-bottom))
          (gs:copy-bits src-bitmap dest-bitmap st-rect st-rect mode mask-rgn))
        (hang-on delay)))))

(new visualEffect :objectName "CloseIris" :project sk8)

(define-handler transfer-bitmap (CloseIris src-bitmap dest-bitmap
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 400) (fast 1) (otherwise 30))))
    (gs:let+ ((left (rref dest-rect :rect.left))
           (right (rref dest-rect :rect.right))
           (top (rref dest-rect :rect.top))
           (bottom (rref dest-rect :rect.bottom))
           (int (max 1 (round-up right 50)))
           (inthigh (max 1 (round-up bottom 50)))
           (temprgn (:region))
           (cliprgn (:region)))
      (do* ((stretch-left left (+ stretch-left int))
            (stretch-right right (- stretch-right int))
            (stretch-top top (+ stretch-top inthigh))
            (stretch-bottom bottom (- stretch-bottom inthigh)))
           ((or (>= stretch-left stretch-right) (>= stretch-top stretch-bottom))
            (rlet ((st-rect :rect :left left :right right :top top :bottom bottom))
              (gs:copy-bits src-bitmap dest-bitmap src-rect st-rect mode mask-rgn)))
        (rlet ((st-rect :rect :left stretch-left :right stretch-right 
                        :top stretch-top :bottom stretch-bottom))
          (#_RectRgn temprgn dest-rect)
          (#_RectRgn cliprgn st-rect)
          (#_DiffRgn temprgn cliprgn cliprgn)
          (#_sectRgn cliprgn mask-rgn cliprgn)
          (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode cliprgn))
        (hang-on delay)))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                         Blend
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

;;;  generate a white pixmap
;;;  until the pixmap is black do
;;;        call copyDeepMask
;;;        paint the pixmap darker
;;;  repeat

#|

(defun deep-dissolve-copy-bits (gWorld src-bitmap dest-bitmap src-rect dest-rect mode speed
                                           &optional (mask-rgn (%null-ptr)))
  (let ((mask (#_NewPixMap))
        (int (round-up 65535 20)))
    (setf (rref mask :PixMap.bounds) src-rect)
    (do ((gray 65535 (- gray int)))
        ((<= gray 0) nil)
      (rlet ((oldFore :RGBColor)
             (oldBack :RGBColor)
             (color :RGBColor :red gray :blue gray :green gray))
        (with-port gWorld
          (setf temPixMap (#_GetGWorldPixMap gWorld))
          (setf (rref gWorld :cGrafPort.portPixMap) mask)
          (#_GetForeColor oldFore)
          (#_GetBackColor oldBack)
          (#_RGBForeColor color)
          (#_RGBBackColor color)
          (#_lockPixels mask)
          (#_PaintRect src-rect)
          (#_unlockPixels mask)
          (setf mask (#_GetGWorldPixMap gWorld))
          (setf (rref gWorld :cGrafPort.portPixMap) temPixMap)
          (#_RGBForeColor oldFore)
          (#_RGBBackColor oldBack)))
      (gs:copy-deep-mask src-bitmap mask dest-bitmap 
                               src-rect src-rect dest-rect mode mask-rgn))))

;;; Approximations to dissolve! How about copying the src to the destination in blend
;;; mode a number of times altering the weight of the blend? Let us see. SLOW AND USELESS!

(defun blend-dissolve-copy-bits (src-bitmap dest-bitmap src-rect dest-rect mode speed
                                                &optional (mask-rgn (%null-ptr)))
  (declare (ignore speed))
  (let ((int (round-up 65535 5)))
    (do ((gray 1 (+ gray int)))
        ((>= gray 65535) nil)
      (rlet ((color :RGBColor :red gray :blue gray :green gray))
        (#_opColor color))
      (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect 32 mask-rgn))))

|#

;;; The Winner because of speed. It is the :fastest approach and it looks just like
;;; the other ones.

(new visualEffect :objectName "Blend" :project sk8)

(define-handler transfer-bitmap (Blend src-bitmap dest-bitmap 
                                                   src-rect dest-rect mode speed
                                                   &optional (mask-rgn (%null-ptr)))
  (declare (ignore mode))
  (let* ((num (case speed (slow 20) (fast 5) (otherwise 10)))
         (inc (round 65535 num))
         (intensity 0))
    (rlet ((color :RGBColor))       
      (dotimes (i num)
        (incf intensity inc)
        (rset color :RGBColor.blue intensity)
        (rset color :RGBColor.green intensity)
        (rset color :RGBColor.red intensity)
        (#_opColor color)
        (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect 32 mask-rgn))
      (rset color :RGBColor.blue 65535)
      (rset color :RGBColor.green 65535)
      (rset color :RGBColor.red 65535)
      (#_opColor color)
      (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect 32 mask-rgn))))
  
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                        fan-house Effect
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "FanHouseLeft" :project sk8)

(define-handler transfer-bitmap (FanHouseLeft src-bitmap dest-bitmap
                                                            src-rect dest-rect mode speed
                                                            &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar left (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left scroll-bar :right right :top top :bottom bottom)
                    (dest-pos :rect :left left :right (- right scroll-bar) :top top :bottom bottom)
                    (src-rectangle :rect :left left :right scroll-bar :top top :bottom bottom)
                    (src-pos :rect :left (- right scroll-bar) :right right :top top :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
               (hang-on delay)))))

(new visualEffect :objectName "FanHouseRight" :project sk8)

(define-handler transfer-bitmap (FanHouseRight src-bitmap dest-bitmap 
                                                             src-rect dest-rect mode speed
                                                             &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar left (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left left :right (- right scroll-bar) :top top :bottom bottom)
                    (dest-pos :rect :left scroll-bar :right right :top top :bottom bottom)
                    (src-rectangle :rect :left (- right scroll-bar) :right right :top top :bottom bottom)
                    (src-pos :rect :left left :right scroll-bar :top top :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

(new visualEffect :objectName "FanHouseUp" :project sk8)

(define-handler transfer-bitmap (FanHouseUp src-bitmap dest-bitmap
                                                          src-rect dest-rect mode speed
                                                          &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar top (+ scroll-bar int)))
             ((>= scroll-bar bottom)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left left :right right :top scroll-bar :bottom bottom)
                    (dest-pos :rect :left left :right right :top top :bottom (- bottom scroll-bar))
                    (src-rectangle :rect :left left :right right :top top :bottom scroll-bar)
                    (src-pos :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

(new visualEffect :objectName "FanHouseDown" :project sk8)

(define-handler transfer-bitmap (FanHouseDown src-bitmap dest-bitmap
                                                            src-rect dest-rect mode speed
                                                            &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar top (+ scroll-bar int)))
             ((>= scroll-bar bottom)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left left :right right :top top :bottom (- bottom scroll-bar))
                    (dest-pos :rect :left left :right right :top scroll-bar :bottom bottom)
                    (src-rectangle :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom)
                    (src-pos :rect :left left :right right :top top :bottom scroll-bar))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                           Slides
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________


(new visualEffect :objectName "SlideLeft" :project sk8)

(define-handler transfer-bitmap (SlideLeft src-bitmap dest-bitmap
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 300) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar left (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((src-rectangle :rect :left left :right scroll-bar :top top :bottom bottom)
                    (src-pos :rect :left (- right scroll-bar) :right right :top top :bottom bottom))
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn)
               (hang-on delay))))))

(new visualEffect :objectName "SlideRight" :project sk8)

(define-handler transfer-bitmap (SlideRight src-bitmap dest-bitmap
                                                         src-rect dest-rect mode speed
                                                         &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 300) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar left (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((src-rectangle :rect :left (- right scroll-bar) :right right :top top :bottom bottom)
                    (src-pos :rect :left left :right scroll-bar :top top :bottom bottom))
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn)
               (hang-on delay))))))

(new visualEffect :objectName "SlideDown" :project sk8)

(define-handler transfer-bitmap (SlideDown src-bitmap dest-bitmap
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 300) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up bottom 100))))
        (do* ((scroll-bar top (+ scroll-bar int)))
             ((>= scroll-bar bottom)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((src-rectangle :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom)
                    (src-pos :rect :left left :right right :top top :bottom scroll-bar))
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn)
               (hang-on delay))))))

(new visualEffect :objectName "SlideUp" :project sk8)

(define-handler transfer-bitmap (SlideUp src-bitmap dest-bitmap
                                                      src-rect dest-rect mode speed
                                                      &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 300) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up bottom 100))))
        (do* ((scroll-bar top (+ scroll-bar int)))
             ((>= scroll-bar bottom)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((src-rectangle :rect :left left :right right :top top :bottom scroll-bar)
                    (src-pos :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom))
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn)
               (hang-on delay))))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                          Pushes
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "PushLeft" :project sk8)

(define-handler transfer-bitmap (PushLeft src-bitmap dest-bitmap
                                                       src-rect dest-rect mode speed
                                                       &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar int (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left int :right (+ int (- right scroll-bar)) 
                                    :top top :bottom bottom)
                    (dest-pos :rect :left left :right (- right scroll-bar) :top top :bottom bottom)
                    (src-rectangle :rect :left left :right scroll-bar :top top :bottom bottom)
                    (src-pos :rect :left (- right scroll-bar) :right right :top top :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

(new visualEffect :objectName "PushRight" :project sk8)

(define-handler transfer-bitmap (PushRight src-bitmap dest-bitmap
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up right 100))))
        (do* ((scroll-bar int (+ scroll-bar int)))
             ((>= scroll-bar right)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left (- scroll-bar int) :right (- right int)
                                    :top top :bottom bottom)
                    (dest-pos :rect :left scroll-bar :right right :top top :bottom bottom)
                    (src-rectangle :rect :left (- right scroll-bar) :right right :top top :bottom bottom)
                    (src-pos :rect :left left :right scroll-bar :top top :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

(new visualEffect :objectName "PushUp" :project sk8) 

(define-handler transfer-bitmap (PushUp src-bitmap dest-Bitmap
                                                     src-rect dest-rect mode speed
                                                     &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up bottom 100))))
        (do* ((scroll-bar int (+ scroll-bar int)))
             ((>= scroll-bar bottom)
              (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
             (rlet ((dest-rectangle :rect :left left :right right
                                    :top int :bottom (- bottom scroll-bar))
                    (dest-pos :rect :left left :right right :top top :bottom (- bottom scroll-bar int))
                    (src-rectangle :rect :left left :right right :top top :bottom scroll-bar)
                    (src-pos :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom))
               (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
               (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
             (hang-on delay)))))

(new visualEffect :objectName "PushDown" :project sk8)

(define-handler transfer-bitmap (PushDown src-bitmap dest-bitmap 
                                                    src-rect dest-rect mode speed
                                           &optional (mask-rgn (%null-ptr)))
  (let ((delay (case speed (slow 200) (fast 1) (otherwise 15))))
      (let* ((left (rref dest-rect :rect.left))
             (right (rref dest-rect :rect.right))
             (top (rref dest-rect :rect.top))
             (bottom (rref dest-rect :rect.bottom))
             (int (max 1 (round-up bottom 100))))
        (do ((scroll-bar int (+ scroll-bar int)))
            ((>= scroll-bar bottom) 
             (gs:copy-bits src-bitmap dest-bitmap src-rect dest-rect mode mask-rgn))
          (rlet ((dest-rectangle :rect :left left :right right
                                 :top (max 0 (- scroll-bar int)) :bottom (- bottom int))
                 (dest-pos :rect :left left :right right :top scroll-bar :bottom bottom)
                 (src-rectangle :rect :left left :right right :top (- bottom scroll-bar) :bottom bottom)
                 (src-pos :rect :left left :right right :top top :bottom scroll-bar))
            (gs:copy-bits dest-bitmap dest-bitmap dest-rectangle dest-pos mode mask-rgn)
            (gs:copy-bits src-bitmap dest-bitmap src-rectangle src-pos mode mask-rgn))
          (hang-on delay)))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                        Tiles
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "RandomTile" :project sk8)

(define-handler transfer-bitmap (RandomTile src-bitmap dest-bitmap
                                                         src-rect dest-rect mode speed
                                                         &optional (mask-rgn (%null-ptr)))
  (declare (ignore src-rect))
  (gs:let+ ((delay (case speed (slow 150) (fast 1) (otherwise 45)))
         (height 10)
         (themask 96)
         (right (rref dest-rect :rect.right))
         (bottom (rref dest-rect :rect.bottom))
         (hint (max 1 (round-up right 10)))
         ;; Adding one to the next one ensures full coverage...
         (vint (1+ (round-up bottom 10)))
         (rr (:rect)))
    (do* ((seq (logxor (ash 1 -1) themask)
               (if (zerop (logand seq 1))
                 (ash seq -1)
                 (logxor (ash seq -1) themask)))
          (row (truncate seq 10) (truncate seq 10))
          (col (mod seq 10) (mod seq 10)))
         ((= 1 seq) (progn (rset rr :rect.left 0)            ;; Doing Region #1
                           (rset rr :rect.right hint)
                           (rset rr :rect.top 0)
                           (rset rr :rect.bottom vint)
                           (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
                           (let ((lft (* col hint))
                                 (top (* row vint)))         ;; Doing Region #0 which the
                             (rset rr :rect.left lft)        ;; algorithm does not produce.
                             (rset rr :rect.right (min (+ lft hint) right))
                             (rset rr :rect.top top)
                             (rset rr :rect.bottom (min (+ top vint) bottom))
                             (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))))
      (when (< row height)
        (let ((lft (* col hint))
              (top (* row vint)))
          (rset rr :rect.left lft)
          (rset rr :rect.right (min (+ lft hint) right))
          (rset rr :rect.top top)
          (rset rr :rect.bottom (min (+ top vint) bottom))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
          (hang-on delay))))))


;;; New and improved. Does not use lists for anything and thus we minimize the chance
;;; of GC in the middle of the effect.

(new visualEffect :objectName "SerialTile" :project sk8)

(define-handler transfer-bitmap (SerialTile src-bitmap dest-bitmap
                                                         src-rect dest-rect mode speed
                                                         &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 100) (fast 20) (otherwise 50))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (hint (max 1 (round-up right 10)))
           (vint (max 1 (round-up bottom 10))))
      (do ((x 0 (+ x hint))
           (xx hint (min right (+ xx hint))))
          ((>= x right))
        (do ((y 0 (+ y vint))
             (yy vint (min bottom (+ yy vint))))
            ((>= y bottom))
          (rlet ((rr :rect :left x :right xx :top y :bottom yy))
            (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
            (hang-on delay)))))))


;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                 Curtains (Venetian Blinds)
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "CloseVenetians" :project sk8)

(define-handler transfer-bitmap (CloseVenetians src-bitmap dest-bitmap
                                                             src-rect dest-rect mode speed
                                                             &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 450) (fast 30) (otherwise 150))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (vint (max 1 (round-up bottom 50))))
      (rlet ((rr :rect :left 0 :right right))
        (dotimes (i 5)
          (hang-on delay)
          (dotimes (j 10)
            (let ((top (* vint (+ (* 5 j) i))))
              (rset rr :rect.top top)
              (rset rr :rect.bottom (min (+ top vint) bottom))
              (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))))
        (rset rr :rect.top (* vint 50))
        (rset rr :rect.bottom bottom)
        (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)))))

(new visualEffect :objectName "OpenVenetians" :project sk8)

(define-handler transfer-bitmap (OpenVenetians src-bitmap dest-bitmap
                                                            src-rect dest-rect mode speed
                                                            &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 450) (fast 30) (otherwise 150))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (vint (max 1 (round-up bottom 50))))
      (rlet ((rr :rect :left 0 :right right))
        (dotimes (i 5)
          (hang-on delay)
          (dotimes (j 10)
            (let ((top (* vint (+ (* 5 j) (- 4 i)))))
              (rset rr :rect.top top)
              (rset rr :rect.bottom (min (+ top vint) bottom))
              (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))))
        (rset rr :rect.top (* vint 50))
        (rset rr :rect.bottom bottom)
        (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)))))

(new visualEffect :objectName "RandomRows" :project sk8)

(define-handler transfer-bitmap (RandomRows src-bitmap dest-bitmap
                                                         src-rect dest-rect mode speed
                                                         &optional (mask-rgn (%null-ptr)))
  (declare (ignore src-rect))
  (let ((delay (case speed (slow 150) (fast 1) (otherwise 45))))
    (let* ((height 40)
           (maskNum 48)
           (right (rref dest-rect :rect.right))
           (bottom (rref dest-rect :rect.bottom))
           (vint (max 1 (round-up bottom height))))
      (rlet ((rr :rect :left 0 :right right))
        (do* ((seq (logxor (ash 1 -1) maskNum)
                   (if (zerop (logand seq 1))
                     (ash seq -1)
                     (logxor (ash seq -1) maskNum)))
              (row seq seq))
             ((= 1 seq) (progn (rset rr :rect.top 0)             ;; Doing region #1
                               (rset rr :rect.bottom vint)
                               (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
                               (let ((top (* row vint)))         ;; Doing Region #0 which the
                                                                 ;; algorithm does not produce.
                                 (rset rr :rect.top top)
                                 (rset rr :rect.bottom (min (+ top vint) bottom))
                                 (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn))
                               (rset rr :rect.top (* vint height))
                               (rset rr :rect.bottom bottom)
                               (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)))
          (when (<= row height)
            (let ((top (* row vint)))
              (rset rr :rect.top top)
              (rset rr :rect.bottom (min (+ top vint) bottom))
              (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
              (hang-on delay))))))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________
;;;                                           Wipes
;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

(new visualEffect :objectName "WipeLeft" :project sk8)

(define-handler transfer-bitmap (WipeLeft src-bitmap dest-bitmap
                                                       src-rect dest-rect mode speed
                                                       &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 230) (fast 8) (otherwise 75))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (hint (max 1 (round-up right 50))))
      (do ((edge right (- edge hint)))
          ((<= edge 0))
        (rlet ((rr :rect
                   :left (max 0 (- edge hint)) :right edge :top 0 :bottom bottom))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
          (hang-on delay))))))

(new visualEffect :objectName "WipeRight" :project sk8)

(define-handler transfer-bitmap (WipeRight src-bitmap dest-bitmap
                                                        src-rect dest-rect mode speed
                                                        &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 230) (fast 8) (otherwise 75))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (hint (max 1 (round-up right 50))))
      (do ((edge 0 (+ edge hint)))
          ((>= edge right))
        (rlet ((rr :rect
                   :left edge :right (min right (+ edge hint)) :top 0 :bottom bottom))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
          (hang-on delay))))))

(new visualEffect :objectName "WipeUp" :project sk8)

(define-handler transfer-bitmap (WipeUp src-bitmap dest-bitmap
                                                     src-rect dest-rect mode speed
                                                     &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 230) (fast 8) (otherwise 75))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (vint (max 1 (round-up bottom 50))))
      (do ((edge bottom (- edge vint)))
          ((<= edge 0))
        (rlet ((rr :rect
                   :left 0 :right right :top (max 0 (- edge vint)) :bottom edge))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
          (hang-on delay))))))

(new visualEffect :objectName "WipeDown" :project sk8)

(define-handler transfer-bitmap (WipeDown src-bitmap dest-bitmap
                                                       src-rect dest-rect mode speed
                                                       &optional (mask-rgn (%null-ptr)))
  (declare (ignore dest-rect))
  (let ((delay (case speed (slow 230) (fast 8) (otherwise 75))))
    (let* ((right (rref src-rect :rect.right))
           (bottom (rref src-rect :rect.bottom))
           (vint (max 1 (round-up bottom 50))))
      (do ((edge 0 (+ edge vint)))
          ((>= edge bottom))
        (rlet ((rr :rect
                   :left 0 :right right :top edge :bottom (min bottom (+ edge vint))))
          (gs:copy-bits src-bitmap dest-bitmap rr rr mode mask-rgn)
          (hang-on delay))))))

;;;_______________________________________________________________________________________
;;;_______________________________________________________________________________________

#|
	Change History (most recent last):
	2	5/28/93	Hernan	Net copy error corrected.
	8	1/11/94	hernan	self -> me
	9	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	10	2/28/94	hernan	Still getting rid of keywords...
	11	7/14/94	Hernan	1171670: need this file to recompile for next build.
	12	7/18/94	Hernan	1174059: randomTile now covers the whole region.
	13 	 3/28/95	Hernan  	1233341: fixed every call to round-up where
							appropriate to make sure that the increments
							are non zero. This is what caused the infinite loop
							when the window was smaller than some threshold.
	14 	 4/17/95	sidney  	check in file again to fix up sourceserver resource screwup
	15 	 4/26/95	Hernan  	1233330: fixed closeIris to clip to the right region.
	3  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	4  	 2/13/97	Brian Roddy	
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
