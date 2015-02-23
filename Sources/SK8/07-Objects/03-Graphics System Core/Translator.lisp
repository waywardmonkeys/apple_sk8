(in-package :SK8Development)
(provide "TRANSLATOR")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; ______________
;;; Low level helpers.
;;; ______________

(defun uniqueResID (type)
  (let ((theID 0))
    (loop
      (setq theID (T_Unique1ID type))
      (unless (> 128 theID)
        (return-from UniqueResID theID)))))

(defun copy-resource-to-a-file (aFileName theHandle resourceType &optional resId)
  (with-open-res-file (nil aFileName :if-does-not-exist :create)
    ;; get the handle and make the resource...
    (let ((newResourceID (or resId (uniqueResID resourceType))))
      ;; If a resourceId is supplied, remove an old resource with the same ID.
      (when resId
        (sk8-multival-bind (h theError)
                           (T_Get1Resource resourcetype resId :notFoundOK t)
          (declare (ignore theError))
          (when h 
            (T_RemoveResource h)
            (#_disposHandle h))))
      ;; Now add the resource.
      (when-unwind
        (T_AddResource theHandle resourceType newResourceID "")
        (sendToLog (format nil "Could not add a ~a to file ~a.~%" resourceType aFileName)))
      (when-unwind
        (progn
          (T_WriteResource theHandle)
          (T_DetachResource theHandle))
        (progn
          (sendToLog (format nil "Could not write a ~a to file ~a.~%" resourceType aFileName))
          (T_RemoveResource theHandle)))
      newResourceID)))

;;; This function takes a handle of a specified type and copies it to the project's resource file.

(define-sk8-function copyResourceToProject nil (theProj theHandle resourceType)
  (let ((swapfile (ps::create-swapfile-if-needed theProj)))
    (copy-resource-to-a-file (physicalname swapfile) theHandle resourceType)))

;;; Writes the handle provided to the project file specified and sets the properties
;;; of the resource object that refers to it.

(defun writeHandleToProjectFile (theProj theHandle theResource)
  (let ((theNum (copyResourceToProject theProj theHandle (restype theResource))))
    (if theNum
      (setf (resourceId theResource) theNum)
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not write handle to resource file")))))

(defun importTypes (type &key justTranslators)
  (let (result)
    (unless (stringp type) (setf type (string type)))
    (mapKnownDescendants 
     Translator 
     #'(lambda (obj)
         (let ((intObj (internalObject obj)))
           (when (string= type (externalType obj))
             (if justTranslators 
               (pushnew obj result)
               (pushnew intobj result))))))
    result))

;;; A utility function to copy selected resources from one file to another... An error if the source file does not
;;; exist. Looks for resources of the right type and id. If the destination file does not exist,
;;; it is created. Any resources with the same resourceId will be overwritten.

(defmacro write-handle-to-possibly-other-file (sourceFile destinationFile handle resType resName resId)
  (let* ((aFileName (gensym))
         (oldHandle (gensym)))
    `(if (or (null ,destinationFile) (eq ,sourceFile ,destinationFile))
       (progn
         (let ((,oldHandle (require-trap #_get1Resource ,resType ,resId)))
           (when (handlep ,oldHandle)
             (T_removeResource ,oldHandle)))
         (T_addResource ,handle ,resType ,resid (if (stringp ,resname) ,resname "")))
       (let ((,aFileName (physicalname ,destinationFile)))
         (with-open-res-file (nil ,aFileName :if-does-not-exist :create)
           (let ((,oldHandle (require-trap #_get1Resource ,restype ,resId)))
             (when (handlep ,oldHandle)
               (T_removeResource ,oldHandle)))
           (T_addResource ,handle ,restype ,resid (if (stringp ,resname) ,resname "")))))))

(define-sk8-function macCopyResources nil (sourceFile destinationFile resType idList)
  (unless (fileExists sourceFile)
    (sk8-error FileNotFoundError :file sourceFile))
  (let ((source (physicalName sourceFile))
        (theHandle nil)
        (destinationAlreadyExisted (fileExists destinationFile)))
    (with-open-res-file (nil source)
      (dolist (i idList)
        (setf theHandle (T_get1Resource resType i))
        (when (handlep theHandle)
          (T_detachResource theHandle)
          (write-handle-to-possibly-other-file sourceFile destinationFile theHandle resType nil i)
          )))
    ;; If a destination file did not exist and was created, make it look like
    ;; a resEdit file.
    (when (and (not destinationAlreadyExisted) (fileExists destinationFile))
      (setf (macFileCreator destinationFile) "RSED")
      (setf (macFileType destinationFile) "RSRC"))
    *undefined*))

;;; ____________________________
;;; 1. The translator and the frameWork.
;;; ____________________________

(new object :objectName "Translator" :project sk8
     :properties '((internalObject :value nil)
                   (finalObject :value nil)))

;;; Returns the OSType the translator knows how to deal with. Default handler
;;; gets the resource type of the translator's internal object.

(define-handler  externalType (translator)
  (let ((theObject (internalObject me)))
    (when (and (internalObject me) (inheritsFrom theObject Media))
      (restype theObject))))

;;; The distinction between the thing this translator ultimately produces and the thing
;;; that is the analog of the media to be imported. Example: in a translator that takes a PICT
;;; and makes an imageRenderer out of it, the finalObject is ImageRenderer, but the internal
;;; object (the analog) is QDPicture.

;;; If false, the internalObject is returned.

(define-handler finalObject (translator)
  (or (slot-value me 'finalObject)
      (internalObject me)))

;;; __________________
;;; Importing.
;;; __________________

;;; CanImport. Returns whether the translator selected can do the import operation required.
;;; Gets the same arguments that import gets.

;;; source: a file, a resource object (pointing to a resource in another file), or SK8Clipboard.
;;; destination: a sk8Object. If provided, it is filled in with the result of the import. 
;;;          otherwise an object of the type in internalObject is created.

;;; source and destination must be valid for this translator.

(define-handler canImport (translator source destination)
  (and 
   ;; Destination has to be of the right type or not provided at all.
   (if destination 
     (inheritsFrom destination (internalObject me))
     t)
   ;; Checking source.
   (cond ((inheritsFrom source file)
          ;; Check whether the file's macfiletype is our externalType.
          (and (fileExists source) (string= (macFileType source) (externalType me))))
         ((inheritsFrom source Media)
          ;; Check that source = my internalObject.
          (or (inheritsFrom (baseParent source) (internalObject me))
              ;; This strangeness is required to deal with the case when two media objects
              ;; have the same restype (eg. PixelMap and QDPicture)
              (string-equal (restype source) (restype (internalObject me)))
              ))
         ((inheritsFrom source clipboard)
          ;; Check that I am one of the importTranslators applicable. 
          (if (incomingData source)
            (when (member (externalType me) (macResourceTypesAvailable source) :test #'string=)
              t)
            (let ((myInternalObject (internalObject me)))
              (dolist (c (typesInClipboard source) nil)
                (when (inheritsfrom c myInternalObject)
                  (return-from canImport t)))))))))

(define-sk8-function importTranslatorsApplicable nil (source)
  (let (result)
    (mapknownDescendants
     translator
     #'(lambda (c)
         (when (canImport c source nil) (push c result))))
    result))

;;; Given the source and destination of the import operation, it returns a translator
;;; that can import the source returning an object of the type of the destination.

(define-sk8-function findImportTranslator nil (source destination &optional finalDestination)
  (mapknowndescendants 
   translator
   #'(lambda (c)
       (when (canImport c source destination)
         (if finalDestination
           (when (inheritsFrom finalDestination (finalObject c))
             (return-from findimporttranslator c))
           (return-from findimporttranslator c))))))

;;; Importing is a two step process: get the data and make it persistent.

(define-handler sk8::import (translator &key source destination)
  ;; Error checking: destination should be of the right type.
  (unless (inheritsFrom destination (internalObject me))
    (SK8-error ArgumentTypeMismatchError
               :handlerName 'sk8::import :argumentName 'destination
               :object destination :expectedType (internalObject me)))
  (cond ((inheritsFrom source file) 
         (cond ((not (fileExists source)) 
                (sk8-error FileNotFoundError :file source))
               ((not (string= (macFileType source) (externalType me)))
                (sk8-error GeneralProgrammaticError
                           :strings '("Macfiletype of file " " does not match result resource.")
                           :objects (list source)))
               ;; Everything ok. Get on with it.
               (t
                (importFromFile me source destination))))
        ((inheritsFrom source Media) (importFromResource me source destination))
        ((inheritsFrom source clipboard) (importFromClipboard me source destination))))

#|

CASES:

1. Normal: resource from file A to project file.
           source = resource, destination = resource in project.

2. resource from file A to be used as resource from file A.
           source = resource, destination = resource in project whose file is
           set to the file of source.

3. file to be used as resource in project
           source = file, destination = resource in project

4. just get the handle in for playing with it. Just like case 2.

|#  

;;; The following three functions are only called from IMPORT. Error checking is done by import and these
;;; can thus not do any of it.

;;; Source = the original resource.
;;; Destination = a resource If its file is set to something OTHER than the resource's file, 
;;;           it is assumed to be the file of the source and no resource is really copied.

(define-handler importFromResource (translator source destination)
  (if (eq (slot-value destination 'file) :project)
    ;; Case 1: copy the resource to project file.
    (let ((theHandle (macGetResourceHandleFromId (file source) 
                                                 (externalType me) 
                                                 (resourceId source))))
      (if (and theHandle (handlep theHandle))
        ;; Everything ok. Copy it.
        (progn (writeHandleToProjectFile (project destination) theHandle destination)
               destination)
        ;; No handle. Report error.
        (sk8-error GeneralProgrammaticError
                   :strings '"Could not obtain handle from resource ~a" source)))
    ;; Case 2: copy the file and resource id of the source to the destination.
    (progn
      (setf (file destination) (findFileInProject (file source) (project destination)))
      (setf (resourceId destination) (resourceId source))
      destination)))

;;; These three methods return errors to allow people to define the ones they will
;;; support only.

(define-handler importFromFile (translator source destination)
  (if (eq (internalObject me) (finalObject me))
    (sk8-error GeneralProgrammaticError
               :strings '("importFromFile not implemented for " ".")
               :objects (list me))
    ;; Find the translator that imports the media and dispatch on it!
    (let ((lowerTranslator (findImportTranslator source destination destination)))
      (if lowerTranslator
        (importFromFile lowerTranslator source destination)
        (sk8-error GeneralProgrammaticError
                   :strings '("importFromFile not implemented for " ".")
                   :objects (list lowerTranslator))))))

;;; This is only called when the clipboard's incomingData is true and there is something
;;; there that can be imported. IMPORTANT: you have to write the handle to the project file. 
;;; Problem is that the clipboard is not a persistent repository (in a way that a res file
;;; can be) since it is overwritten the next time something is put there.

;;; source = the clipboard object.
;;; destination = the thing to make.

(define-handler importFromClipboard (translator source destination)
  (let ((theHandle (getScrap (externalType me))))
    (if (and theHandle (handlep theHandle))
      (progn (writeHandleToProjectFile (project destination) theHandle destination)
             destination)
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not obtain handle of type " " from " ".")
                 :objects (list (externalType me) source)))))

;;; __________________
;;; Exporting.
;;; __________________

;;; Checks both the source and the destination. 

(define-handler canExport (translator source destination)
  (and 
   ;; Source must be provided and be of the right type.
   (and source (inheritsFrom source (finalObject me)))
   ;; Check the destination.
   (cond ((inheritsFrom destination file) t)
         ((inheritsFrom destination Media)
          ;; Must be of the right type.
          (inheritsFrom destination (internalObject me)))
         ((inheritsFrom destination clipboard) t))))
   
(define-sk8-function findExportTranslator nil (source destination)
  (mapknowndescendants 
   translator
   #'(lambda (c)
       (when (canExport c source destination)
         (return-from findExportTranslator c)))))

(define-handler exportToFile (translator source destination)
  (declare (ignore source destination))
  (sk8-error GeneralProgrammaticError :strings '("Not implemented yet.")))

;;; The source and destination media objects.

(define-handler exportToResource (Translator source destination)
  (let ((theMedia (loadMedia source)))
    (if (handlep theMedia)
      ;; Write it to the export file.
      (setf (resourceId destination)
            (copy-resource-to-a-file (physicalName (file destination)) theMedia (restype source)
                                     (resourceId destination)))
      ;; Report an error.
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not find media to export.")))))

(define-handler exportToClipboard (translator source destination)
  (declare (ignore source destination))
  (sk8-error GeneralProgrammaticError :strings '("Not implemented yet.")))

(define-handler sk8::export (translator &key source destination)
  ;; Error checking: source should be of the right type.
  (unless (inheritsFrom source (finalObject me))
    (SK8-error ArgumentTypeMismatchError
               :handlerName 'sk8::export :argumentName 'source
               :object source :expectedType (finalObject me)))
  (cond ((inheritsFrom destination file) (exportToFile me source destination))
        ((inheritsFrom destination Media) (exportToResource me source destination))
        ((inheritsFrom destination clipboard) (exportToClipboard me source destination))))

;;; TextToTextTranslator (the clipboard needs it!)
;;; _____________________

(new translator :objectname "StringToTEXTTranslator" :project sk8
     :internalObject String
     :finalObject String)

(define-handler externalType (StringToTEXTTranslator)
  "TEXT")

;;; Can only import from the clipboard.

(define-handler canImport (StringToTEXTTranslator source destination)
  (declare (ignore destination))
  (if (or (inheritsFrom source file) (inheritsFrom source media))
    nil
    (call-next-method)))

(define-handler importFromClipboard (StringToTEXTTranslator source destination)
  (declare (ignore source destination))
  ;; (get-scrap (ccl::make-keyword (externalType me)))
  (scrap-to-string)
  )

(define-handler exportToClipboard (StringToTEXTTranslator source destination)
  (declare (ignore destination))
  (when (and source) ;;  (not (string-equal source (get-scrap :text))))
    ;; (put-scrap :text source)
    (string-to-scrap source)
    ))

#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	5  	 5/ 2/96	Hernan  	ExportToClipboard of StringToTEXTTranslator puts out the
						string no matter what. So what if it is slow? Nothing more
						annoying than not being able to paste something from SK8
						into Eudora.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
