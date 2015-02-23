;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;; copy resources from the existing application file and open projects into the new
;; application file during a buildstandalone. Because the resources may come from different
;; files, there could be conflicts in resource ids. We will build up a database of resource types and the
;; ids that are used in the new file. Since all resources will be automatically copied from the existing
;; application when save-application is eventually called, we initialize the database with the resources
;; that are in the existing application file. Then we go through all of the resources to be copied and assign
;; resource ids sequentially, beginning with 129, skipping any that are already marked as to be used.
;; All of this goes into an alist that will be wrapped in a closure that will be called by save-application to
;; do the actual transfer of resources. The alist will contain lists of source files and the resource types and
;; ids to transfer.

;; we'll keep track of resources in use with a hash table keyed on the resource type string. The value will be
;; the set of resource ids assigned for that type in the current application file, and the value of the last one
;; assigned. Assigning a new resource id is done by incrementing the value of the last one assigned, checking if it
;; conflicts with a resource from the current application, and increment and try again if it does.

;; create and load up the hash table of resource types and ids in use by the current application file
(defun initialize-table-of-resources-that-are-in-application-file ()
  (let ((theTable (make-hash-table :test #'equal)))
    (dolist (restype (macResourceTypesAvailable SK8ApplicationFile))
      (setf (gethash (string restype) theTable)
            (cons 128 (macGetResourceHandleIds SK8ApplicationFile restype))))
    theTable))

;; use the hash table to find the next available resource id for a given resource type.
;; return the resource id and advance the counter for the next time this is called
;; if newresid is non-nil, don't allocate a new one, but mark that as being used
(defun next-available-resourceid (theTable restype newresid)
  (let ((ress (gethash restype theTable)))
    (if ress
      (let ((resid (1+ (car ress)))
            (oldresids (cdr ress)))
        (if newresid
          (progn
            (unless (or (member newresid oldresids)  ;; the two ways that newresid cannot be allocated automatically
                        (> resid newresid))
              (if (= resid newresid)
                (setf (car ress) newresid)
                (setf (gethash restype theTable) (cons (car ress) (cons newresid oldresids)))))
            newresid)
          (loop (if (member resid oldresids)
                  (incf resid)
                  (return (setf (car ress) resid))))))
      (progn
        (unless newresid (setf newresid 129))   ;; 129 is the smallest we will allocate automatically
        (setf (gethash restype theTable) (list newresid))
        newresid))))

;; given the source file name string, the resource type string, and resource id, add an entry to the alist that will be used
;; to specify the copying of resources during the save-application. The format of the alist is:
;; ((fromfilename1 (resourceType1 (fromResId1 . toResId1) (fromResId2 . toResId2) ... ) ...) ... )
;; The alist is passed in as the first argument and is destructively modified.
;; It can be if we initialize the alist as '(nil nil). This will work if we either remove the
;; nil elements before we use the alist, or else make sure that they are ignored.
;; The hash table being used to track available resource ids is the second argument
;; Return the new resource id that the resource will have in the new application file
;; The new resource id is either passed in as an argument or is assigned to be the next available

(defun add-resource-entry-to-copying-alist (theAlist theTable fromFileName resType resid newresid)
  (let ((fileElement (assoc fromFileName theAlist :test #'equal)))
    (if fileElement
      (let ((typeElement (assoc resType (cdr fileElement) :test #'equal)))
        (if typeElement
          (let ((idElement (assoc resid (cdr typeElement) :test #'equal)))
            (if idElement
              (cdr idElement)    ;; this has already been specified
              (push (cons resid (setf newresid (next-available-resourceid theTable resType newresid)))
                    (cdr typeElement))))
          (push (list resType
                      (cons resid
                            (setf newresid (next-available-resourceid theTable resType newresid))))
                (cdr fileElement))))
      (push (list fromFileName
                  (list restype
                        (cons resId
                              (setf newresid (next-available-resourceid theTable resType newresid)))))
            (cdr theAlist)))
    newresid))

;; set up for copying unchanged all resources from a project file
(defun copy-project-resources-with-unchanged-ids (proj theAlist theTable)
  (let ((projfile (sk8::file proj))
        projfilename)
    (when projfile
      (setf projfilename (physicalname projfile))
      (with-open-res-file (refvar projfilename)
        (#_updateresfile refvar))    ;; make sure file is updated before we search through it
      (dolist (restype (macResourceTypesAvailable projfile))
        (unless (string= "CCLe" restype)  ;; hack that can be removed when we force build of temp proj files
          (dolist (resid (macGetResourceHandleIds projfile restype))
            (add-resource-entry-to-copying-alist theAlist theTable projfileName resType resid resid)))))))

(defun create-the-alist-of-resources-to-copy ()
  (let ((theAlist (list nil nil))
        (theTable (initialize-table-of-resources-that-are-in-application-file)))
    (copy-project-resources-with-unchanged-ids sk8::sk8 theAlist theTable)
    (when (and (boundp 'sk8::ui) (sk8::is-a sk8::ui sk8::project))
      (copy-project-resources-with-unchanged-ids sk8::ui theAlist theTable))
    (labels ((process-one-media-object (obj)
               (let ((resid (sk8::resourceId obj))
                     (rtype (sk8::restype obj))
                     (myproj (sk8::project obj))
                     (myfile (sk8::getValue 'sk8::file obj)))
                 (when (and resid rtype (eq myfile :project)  )
                   (let ((newresid
                          (if (or (eq myproj sk8::sk8)
                                  (and (boundp 'sk8::ui) (eq myproj sk8::ui)))
                            resid  ;; if in sk8 or ui proj, just mark as :application, since resource is already copied with same id
                            (add-resource-entry-to-copying-alist  ;; else copy resource and generate new id
                             theAlist
                             theTable
                             (physicalname (sk8::file obj))
                             rtype
                             resid
                             nil))))
                     (when newresid
                       (sk8::setvalue 'sk8::resourceId obj newresid)
                       (sk8::setvalue 'sk8::file obj :application)
                       ))))))
      (mf::map-all-descendants
       sk8::media
       #'process-one-media-object))
    ;; since we are merging project files into the standalone app, delete reference to the files in the project objects
    (mf::map-all-descendants sk8::project
                             #'(lambda (proj)
                                 (setf (sk8::file proj) nil)))
    (delete nil theAlist)))

#|
	Change History (most recent last):
	1  	 2/10/95	sidney  	Initial checkin - Functions to support copying of resources from project files to standalone application
	2  	 2/16/95	sidney  	clear reference to file in project objects when building standalone is merging project files into the app
	3  	 3/ 9/95	sidney  	ignore media objects that don't have a restype - They can't be real
	4  	 3/27/95	sidney  	don't copy prjectbuilder ui resources when not saving ui in buildstandalone
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
