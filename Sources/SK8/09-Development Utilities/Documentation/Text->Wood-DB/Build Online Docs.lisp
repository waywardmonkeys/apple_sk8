;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

(defvar *full-compile-required* t)
(defvar *update-documentation-project* nil)
(defvar *compile-type-wanted* :RTF) ;; can also be :WOOD

(defun compile-new-documentation ()
  ;; [1] Update doc project if required.
  (when *update-documentation-project*
    (update-all-projects))
  ;; [2] Load the documentation parser files.
  (sk8-build-files
   "ccl;SK8:Development Utilities:Documentation:Text->Wood-DB:Object Definitions.lisp"
   "ccl;SK8:Development Utilities:Documentation:Text->Wood-DB:Doc Macros.lisp"
   "ccl;SK8:Development Utilities:Documentation:Text->Wood-DB:Doc Parser.lisp"
   "ccl;SK8:Development Utilities:Documentation:Text->Wood-DB:Parsed Docs->RTF.lisp")
  ;; [3] Start the process.
   (sk8dev::parse-macros)
  (if (eq *compile-type-wanted* :RTF)
    (sk8dev::docs->QuickView "SK8 Reference" (directory "ccl;Documentation:Text:*"))
    (sk8dev::parse-folder)))

(defun build-online-docs-dialog ()
  (MAKE-INSTANCE 'COLOR-DIALOG
    :WINDOW-TYPE
    :TOOL
    :WINDOW-TITLE
    "Build Online Documentation DB"
    :VIEW-POSITION
    #@(40 60)
    :VIEW-SIZE
    #@(400 151)
    :VIEW-FONT
    '("Chicago" 12 :SRCOR :PLAIN)
    :VIEW-SUBVIEWS
    (LIST (MAKE-DIALOG-ITEM
           'RADIO-BUTTON-DIALOG-ITEM
           #@(30 20)
           #@(300 16)
           "Compile for QuickView title."
           #'(LAMBDA (item$) 
               (declare (ignore item$))
               (setf *compile-type-wanted* :RTF))
           :RADIO-BUTTON-PUSHED-P t)
          (MAKE-DIALOG-ITEM
           'RADIO-BUTTON-DIALOG-ITEM
           #@(30 45)
           #@(330 16)
           "Compile for FrameMaker Manual (requires WOOD)"
           #'(LAMBDA (item$) 
               (declare (ignore item$))
               (setf *compile-type-wanted* :WOOD))
           :radio-button-pushed-p nil)
          (MAKE-DIALOG-ITEM
           'CHECK-BOX-DIALOG-ITEM
           #@(30 80)
           #@(300 16)
           "Update Documentation Project"
           #'(LAMBDA (item$) 
               (setf *update-documentation-project* (check-box-checked-p item$)))
           :CHECK-BOX-CHECKED-P nil)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(291 121)
           #@(62 16)
           "OK"
           #'(LAMBDA (item$)
               (window-close (view-window item$))
               (compile-new-documentation))
           :DEFAULT-BUTTON T)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(210 122)
           #@(62 16)
           "Cancel"
           #'(LAMBDA (item$)
               (window-close (view-window item$))
               (ed-beep))
           :DEFAULT-BUTTON NIL)
          )))

;;; And do it!

(build-online-docs-dialog)


#|
	Change History (most recent last):
	1	8/2/94	Hernan	New file. Compiles the online documentation.
	2	8/2/94	Hernan	The update is not required.
	2  	 2/ 5/96	Hernan  	Added functionality to create QuickView version of the
						reference manual.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
