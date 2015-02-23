(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(queue-build-fixup
 (unless (and (boundp 'MovieRectangle) (mf::object-class-p MovieRectangle))

;;; The indentation is nonstandard from here down so the doc accessing keyboard macros will work.
(new Rectangle
     :objectName "MovieRectangle"
     :project SK8)
)

(define-handler initialize :private (MovieRectangle original isNew initArgs)
                (declare (ignore original isNew initArgs))
                (call-next-method)
                (setf (fillColor me) (new QuickTimeRenderer :project (project me))))

(setf (frameSize MovieRectangle) '(0 0))
(setf (fillColor MovieRectangle) Black)
(setf (prototype MovieRectangle) t)
)


#|
	Change History (most recent last):
	1	5/6/94	dy	
	2	5/6/94	dy	queue-build-fixup
	3	7/19/94	dy	QT Renderer changes
	4	7/19/94	dy	change movieRectangle to Black
	5  	 8/24/94	dy      	fix initial property values
	6  	 2/16/95	sidney  	readable argument names for initialize handler
	2  	 8/17/95	sidney  	boundp check is not enough anymore
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
