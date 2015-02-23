;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "PIXELMAPTOPICTTRANSLATOR")

(require "QDPICTURETOPICTTRANSLATOR" "objects;Import/Export:QDPictureToPICTTranslator")

;;; PixelMapToPICTTranslator
;;; _____________________  

(new QDPictureToPICTTranslator :objectName "PixelMapToPICTTranslator" :project sk8
     :internalObject PixelMap)

(defun pixmap-to-pict (obj)
  (let ((gWorld (mediaData obj))
        pictH helperGWorld)
    ;; Only do this if the pixel map actually has a gWorld to save.
    (when (macptrp gWorld)
      ;; NOTE: when the origin of the source gWorld is not 0, copying the pixmap onto
      ;; itself does not work. Thus, we need to copy it to another gWorld, but with
      ;; its top left set to 0. The following piece of code does that.
      ;; [1] Initialize helper gWorld for this size.
      (sk8-multival-bind (width height) (size obj)
        (initialize-snapshot-pixmap swatchTempSnapShotMedia nil :width width :height height))
      (setf helperGWorld (mediaData swatchTempSnapShotMedia))
      ;; [2] Blit the image to a similar rect whose topleft is at 0.
      (gs:with-offscreen-gWorld  
        (helperGWorld)
        (gs:with-gWorld-rectangle 
          (gWorld-rect helperGWorld)
          (let ((sourcePixm (#_getGWorldPixmap gWorld))
                (destPixm (#_getGWorldPixmap helperGWorld)))
            (setf pictH (#_OpenPicture gWorld-rect))
            (gs:Copy-Bits sourcePixm destPixm 
                          (gs:gWorld-rectangle gWorld) gWorld-rect 8 (%null-ptr))
            (#_ClosePicture))))
      (new QDPicture :mediaData pictH  :project (project obj)))))

;;; creates the QDPicture and lets the Translator handler do the work.

(define-handler exportToResource (sk8::PixelMapToPictTranslator source destination)
  (let ((thePict (pixmap-to-pict source)))
    (call-next-method me thePict destination)))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
