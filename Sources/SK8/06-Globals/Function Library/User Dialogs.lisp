;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)

(provide "USERDIALOGS")

;;;
;;;

;;MCL3.0 - test, macFileCreator, files and directories arguments were never used, and
;;  now have been removed from the choose-file-dialog function
(define-sk8-function MacChooseFileDialog nil (&key directory
                                     ((:project InProject) nil)
                                     macFileType
                                     macFileCreator
                                     test
                                     files
                                     directories
                                     (buttonString "Choose"))
  (declare (ignore test macFileCreator files directories))
  (unless inproject (error "MacChooseFileDialog: Project argument is required"))
  (let ((res (catch :cancel
               (let ((path (choose-file-dialog :directory directory
                                               :mac-file-type macfiletype
                                               :button-string buttonstring))
                     newbie)
                 (if path (setf newbie (new file :project inproject)))
                 (setf (ospathname newbie) path)
                 newbie))))
    (if (eq res :cancel)
      nil
      res)))

(define-sk8-function MacChooseNewFileDialog nil (&key directory 
                                                          (prompt "New file:")
                                                          (buttonString "New")
                                                          ((:project InProject) nil)
                                                          )
  (unless inproject (error "MacChooseFileDialog: Project argument is required"))
  (let ((res (catch :cancel
               (let ((path (choose-new-file-dialog :directory directory :prompt prompt :button-string buttonstring))
                     newbie)
                 (if path (setf newbie (new file :project inproject)))
                 (setf (ospathname newbie) path)
                 newbie)
               )))
    (if (eq res :cancel)
      nil
      res)))

(define-sk8-function MacChooseDirectoryDialog nil (&key directory
                                                            ((:project InProject) nil)
                                                            )
  (unless inproject (error "MacChooseFileDialog: Project argument is required"))
  (let ((res (catch :cancel
               (let ((path (CHOOSE-DIRECTORY-DIALOG :directory directory))
                     newbie)
                 (if path (setf newbie (new file :project inproject)))
                 (setf (ospathname newbie) path)
                 newbie))))
    (if (eq res :cancel)
      nil
      res)))

(define-sk8-function MacChooseFileDefaultDirectory nil (&key ((:project InProject) nil))
  (unless inproject (error "MacChooseFileDialog: Project argument is required"))
  (let ((path (choose-file-default-directory))
                     newbie)
                 (if path (setf newbie (new file :project inproject)))
                 (setf (ospathname newbie) path)
                 newbie))

(define-sk8-function MacGetAnswerFromUser nil (message &key
                                                           (defaultAnswer "")
                                                           (width 300)
                                                           (height 100)
                                                           (top 40)
                                                           (left 40)
                                                           (okText "OK")
                                                           (cancelText "Cancel")
                                                           (allowEmptyStrings t))
  (get-string-from-user message :modeless nil
                                     :initial-string defaultAnswer
                                     :size (make-point width height)
                                     :position (make-point left top)
                                     :ok-text oktext
                                     :cancel-text canceltext
                                     :allow-empty-strings allowemptystrings))



; (MacGetAnswerFromUser "Hello sir!")

(define-sk8-function MacMessageToUser nil (message &key (oktext "OK")
                                                      (width 300)
                                                      (height 100)
                                                      (top 40)
                                                      (left 40))
  (message-dialog message :ok-text oktext :size (make-point width height) :position (make-point left top)))

;(MacMessageToUser "Hello out there")

(define-sk8-function MacYesOrNoDialog nil (message &key
                                                      (width 300)
                                                      (height 100)
                                                      (top 40)
                                                      (left 40)
                                                      (yesText "Yes")
                                                      (noText "No")
                                                      (cancelText "Cancel"))
  (y-or-n-dialog message :size (make-point width height)
                 :position (make-point left top)
                 :yes-text yestext
                 :no-text notext
                 :cancel-text canceltext))

(define-sk8-function MacSelectFromCollectionDialog nil (candidateList &key
                                                                           (title "Select an item:")
                                                                           (multipleValues nil)
                                                                           (buttonString "Select"))
  (let ((result (select-item-from-list candidateList :default-button-text buttonstring
                                       :modeless nil
                                       :selection-type (if multiplevalues :disjoint :single)
                                       :window-title title)))
    (if multiplevalues
      result
      (car result)
      )))

#|
	Change History (most recent last):
	7	10/15/93	rod	
	8	10/15/93	rod	
	9	2/25/94	hernan	Using symbols instead of keywords for options!!!
	10	6/3/94	rod	
	11	7/27/94	rod	Removing need to specify project.
	12 	 9/ 2/94	rod     	
	13 	 9/22/94	rod     	
	14 	11/14/94	rod     	Fixing default project to be false so users MUST
							specify a project.
	15 	 2/17/95	rod     	Fixing select-from-collection
	2  	 6/12/95	sidney  	remove extra arguments no longer in choose-file-dialog in MCL3.0
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
