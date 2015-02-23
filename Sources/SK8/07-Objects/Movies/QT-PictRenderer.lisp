(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; QuickTimePictRenderer child of ImageRenderer

;;; This renderer renders the movie's fillRegion using its QuickTimePictGCHandle.  We reuse the prototype
;;; QuickTimePictRenderer over and over, each time setting it's media's handle toa new PICT handle.

#|
(unless (boundp 'QuickTimePictRenderer)
  (new ImageRenderer
       :objectname "QuickTimePictRenderer"
       :undisposable t
       :project sk8))

(unless (boundp 'QuickTimePictRendererMedia)
  (new QdPicture :objectname "QuickTimePictRendererMedia"
       :undisposable t
       :project sk8))

(setf (media QuickTimePictRenderer) QuickTimePictRendererMedia)

(define-handler render (QuickTimePictRenderer theActor region thePaper)
  (declare (ignore thePaper))
  (let ((oldFillColor (gs:node-fillcolor theActor)))
      (setf (gs:node-fillcolor theActor) QuickTimePictRenderer)
      (setf (handle (media me)) (moviePictGCHandle (if (eq me (fillcolor theActor))
                                                   (fillcolor theActor)
                                                   (frameColor theActor))))
      (call-next-method)
      (setf (gs:node-fillcolor theActor) oldFillColor)))
|#

#|
	Change History (most recent last):
	1	7/19/94	dy	
	2	8/10/94	dy	symbols with preserved case
	3  	10/21/94	Hernan  	Adding the paper argument to render.
	4  	10/31/94	dy      	comment out all the code because pauseWithHighQuality of QuickTimeRenderer has been removed
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
