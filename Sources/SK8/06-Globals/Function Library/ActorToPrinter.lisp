;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :ccl)

(provide "ACTORTOPRINTER")

(require "IMAGERENDERER")

(eval-when (eval compile)
   (require 'backquote)
   (require 'sysequ)
   (require 'toolequ)
   (require 'defrecord)
   (require 'fredenv)

   (defconstant $boldStyle #x100)
   (defconstant $italicStyle #x200)
   (defconstant $ulineStyle #x400)
   (defconstant $outlineStyle #x800)
   (defconstant $shadowStyle #x1000)
   (defconstant $condenseStyle #x2000)
   (defconstant $extendStyle #x4000)
   (defconstant $PrintErr #x944)
   (defconstant $iPrintSize 120)
   (defconstant $prJob.bJDocLoop (%i+ 62 6))
   (defconstant $iPrStatSize 26)
   (defconstant $bSpoolLoop 1)

   (defmacro $pnLoc.h (port &optional (val () val-p))
     (if val-p `(%put-word ,port ,val 50) `(%get-signed-word ,port 50)))
   (defmacro $pnLoc.v (port &optional (val () val-p))
     (if val-p `(%put-word ,port ,val 48) `(%get-signed-word ,port 48)))

)

(in-package :sk8dev)

;;; Capturing a pict by recording operations.

(new pixelMap :objectname "PictRecorderHelper" :project sk8)

(defun actor-to-object-based-pict (theActor)
  (let ((helperPixelMap sk8::PictRecorderHelper)
        thePict theRect)
    (gs::recompute-bounds-region theActor (gs::node-flags theActor))
    (setf theRect (gs::hrref (gs::node-boundsRegion theActor) :region.rgnbbox :storage :pointer))
    (rlet ((picFrame :rect :topLeft 0 
                     :bottom (- (rref theRect :rect.bottom) (rref theRect :rect.top))
                     :right (- (rref theRect :rect.right) (rref theRect :rect.left))))
      (initialize-snapshot-pixmap helperpixelmap theActor)
      (let ((thePort (mediaData helperPixelmap)))
        (let ((thePixmap (#_GetGworldPixmap thePort)))
          (with-locked-pixels-force (thePixmap)
            (with-port thePort
              (setf thePict (#_openPicture picFrame))
              (let ((gs::*fast-draw* nil))
                (gs::with-clipped-region (gs::node-boundsRegion theActor)
                  (funcall (gs::node-drawFunction theActor) theActor thePort (gs::node-boundsRegion theActor)))
                (#_closePicture)))))
        ;; Report an error if we run out of memory during the definition
        ;; of the pict.
        (when (#_emptyRect (gs::hrref thePict :picture.picframe :storage :pointer))
          (sk8-error OSHeapFullError))
        thePict))))

;;; Printing a pict handle. 

(defun get-print-pict-print-record (&aux pRec)
  (let (hPrintRec)
    (if (null pRec)
      (unwind-protect
        (progn
          (setq pRec (#_NewHandle :errchk CCL::$iPrintSize))
          (#_PrintDefault pRec)
          (CCL::prchk CCL::$err-printer-params)
          (setq hPrintRec pRec 
                pRec nil))
        (when pRec
          (setq hPrintRec nil)
          (#_DisposHandle :errchk pRec)))
      (progn
        (#_PrValidate hPrintRec)
        (CCL::prchk CCL::$err-printer-params)))
    hPrintRec))

(defun print-pict-to-printer (aPicture &optional (show-dialog t))
  (when (handlep aPicture)
    (unwind-protect
      (progn
        (#_PrOpen)
        (CCL::prchk CCL::$err-printer-load)
        (let ((pRec (get-print-pict-print-record)))
          (#_SetCursor *ARROW-CURSOR*)
          (when (or (and (not show-dialog)
                         (progn (#_prjobmerge pRec pRec)
                                t))
                    (#_PrJobDialog pRec)
                    (throw :cancel :cancel))
            ;_PrOpenDoc puts up a dialog window which causes the event system
            ;to get confused.  So we do the whole thing without interrupts, and
            ;make sure to clean up before handling errors.
            (handler-case
              (let ((*break-on-errors* nil))
                (with-macptrs (saved-port saved-device)
                  (CCL::get-gworld saved-port saved-device)
                  (without-interrupts
                   (unwind-protect
                     (progn
                       (with-macptrs ((printing-port (#_PrOpenDoc pRec (%null-ptr) (%null-ptr))))
                         (CCL::set-gworld printing-port)
                         (unwind-protect
                           (ccl::with-dereferenced-handle (ppRec pRec)
                             ppRec
                             (CCL::prchk CCL::$err-printer-start)
                             (rlet ((r :Rect))
                               (with-dereferenced-handles ((pPicture aPicture))
                                 (copy-record (pref pPicture :Picture.picFrame) :Rect r))
                               (#_PrOpenPage printing-port (%null-ptr))
                               ; For those in the audience, printing occurs here
                               (#_ClipRect r)
                               (#_DrawPicture aPicture r)
                               (#_PrClosePage printing-port)
                               )))
                         (#_PrCloseDoc printing-port)))
                     (CCL::set-gworld saved-port saved-device)))
                  (when (eq (%hget-byte pRec CCL::$prJob.bJDocLoop) CCL::$bSpoolLoop)
                    (CCL::prchk)
                    (%stack-block ((StRec CCL::$iPrStatSize))
                      (#_PrPicFile pRec (%null-ptr) (%null-ptr) (%null-ptr) StRec)
                      (CCL::set-gworld saved-port saved-device))
                    (CCL::prchk))))
              (error (c) (error c)))
            t)))
      (#_PrClose))))

(define-sk8-function actorToPrinter nil (theActor)
  (let ((thePict (actor-to-object-based-pict theActor)))
    (if (handlep thePict)
      (progn
        (print-pict-to-printer thePict)
        (#_disposHandle thePict))
      (messageToUser "Could not print."))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
