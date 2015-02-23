;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETNEWRGBCOLORFROMUSER")

(require "GETNEWOBJECTFROMUSER" "objects;Dialogs:GetNewObjectFromUser")
(require "COMPLEXRGBCOLOR" "objects;Effects:ComplexRGBColor")

;;; _____________________________________________________________________________
;;;                GetNewFromUser(RGBColor)
;;; _____________________________________________________________________________

(define-handler getNewFromUser (RGBcolor &key 
                                            (existingObject me)
                                            (objectName nil) ((:project InProject) nil))
  (if (inheritsfrom me complexrgbcolor)
    (call-next-method)
    (progn
      ;; make sure we don't redefine the class
      (if (eq existingObject RGBcolor)
        (setq existingObject nil))
      
      ;; determine the project
      (unless inProject
        (setq inProject
              (if existingObject
                (project existingObject)
                (sk8-error GeneralProgrammaticError
                           :strings '("Project argument must be specified.")
                           )
                )))
      (when (and (not existingobject) (not objectname))
        (setf existingObject (call-next-method me :project inproject :locked t)))
      ;; get the maybe new color
      (let ((newMCLcolor (user-pick-color :color (if existingObject
                                                   (sk8::mcl-color existingObject)
                                                   *white-color*)
                                          :prompt (if existingObject
                                                    (concatenate 'string "Specify a color for " 
                                                                 (objectString existingObject :project (project existingobject)))
                                                    "Specify new Color")
                                          :position 0)))
        ;; make sure we got a color
        (when newMCLcolor
          ;; make a return renderer, if needed
          (unless existingObject
            (setq existingObject (new RGBcolor :objectName objectname :project inProject)))
          ;; install the new color vars
          (setf (foreRed existingObject) (color-red newMCLColor)
                (foreGreen existingObject) (color-Green newMCLColor)
                (foreblue existingObject) (color-blue newMCLColor))
          existingObject)))))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
