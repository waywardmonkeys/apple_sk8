;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "ICONRSRCTOCICNTRANSLATOR")

(require "TRANSLATOR")

;;; _______________________________ 
;;; icl8 -> cicn convertor... SOME DAY this should be folded into the translator...
;;; _______________________________ 

;;; This function gets called with the resource file focused on the
;;; file that contains the icon. It creates a cicn with the same resId and
;;; returns it. (Written by Chris Flick)

;;;
;;; Deals with case that there is no ICN# corresponding to the icl8 resource.
;;; In that case, we calculate the mask using CalcCMask.
;;;
;;; NOTES & GOTCHAS:
;;; - The color search proc must return the #x0100 otherwise CalcCMask goes into (what
;;;   appears to be) an infinite loop.
;;;
;;;
;;; - QuickDraw.h defines RGBColor's color components to be unsigned-integers. MatchRec's components are
;;; defined to be signed-integers. These will never compare as the same. Yikes!
;;;
;;;
;;; AUTHOR: Chris Flick
;;;
;;; (c) 1995, Apple Computer, All Rights Reserved
;;;


;;; 
;;; SearchProc passed to CalcCMask that calculate a mask like ResEdit does.
;;;

;;;
;;; **** WARNING!!!! ****
;;;
;;; QuickDraw.h defines RGBColor's color components to be unsigned-integers. MatchRec's components are
;;; defined to be signed-integers. These will never compare as the same. Yikes!
;;;

(defrecord MatchRec 
  (red :unsigned-integer)
  (green :unsigned-integer)
  (blue :unsigned-integer)
  (matchData :signed-long)
  )

(defpascal CalcCMaskCICNSearchProc (:ptr pRGBColor
                                             :ptr pPosition 
                                             :word )
  (without-interrupts
   (let* ((currDevice (#_GetGDevice))
          (pMatchRec (%int-to-ptr (rref currDevice :GDevice.gdRefCon :storage :handle))); NOTE: A handle rref
          )
     
     (if (and (= (rref pRGBColor :RGBColor.red :storage :pointer) (rref pMatchRec :MatchRec.red :storage :pointer))
              (= (rref pRGBColor :RGBColor.green :storage :pointer) (rref pMatchRec :MatchRec.green :storage :pointer))
              (= (rref pRGBColor :RGBColor.blue :storage :pointer) (rref pMatchRec :MatchRec.blue :storage :pointer)) )
       (%put-long pPosition 0)
       (%put-long pPosition 1))))
  #x0100 )


#|
(defvar theFile "ccl;eWorld Color Art")
(probe-file theFile)

(cl-user::with-open-res-file 
 (nil theFile)(icl8->cicn 199))


(defvar pRGBColor (make-record :RGBColor))
(defvar pMatchRec (make-record :MatchRec))
(setf pPosition (#_NewPtr 4))
(rset pRGBColor :RGBColor.red 0 :storage :pointer)
(rset pRGBColor :RGBColor.green 0 :storage :pointer)
(rset pRGBColor :RGBColor.blue 0 :storage :pointer)

(rset pMatchRec :MatchRec.red 0 :storage :pointer)
(rset pMatchRec :MatchRec.green 0 :storage :pointer)
(rset pMatchRec :MatchRec.blue 0 :storage :pointer)

(rref (#_GetGDevice) :GDevice.gdRefCon :storage :handle)

(%put-long pPosition 2)
(if (and (= (rref pRGBColor :RGBColor.red :storage :pointer) (rref pMatchRec :MatchRec.red :storage :pointer))
         (= (rref pRGBColor :RGBColor.green :storage :pointer) (rref pMatchRec :MatchRec.green :storage :pointer))
         (= (rref pRGBColor :RGBColor.blue :storage :pointer) (rref pMatchRec :MatchRec.blue :storage :pointer))
         )
  (%put-long pPosition 0)
  (%put-long pPosition 1))

(%get-long pPosition )

|#

(defun icl8->cicn (resId)
  (let ((depth 8)
        (bitmapSize (* 4 32))
        (wmgr (gs:get-wmgr-port))	; We assume that this is smart enough to return the color grafport
        bounds icn icl8 gcicn errorResult)
    (without-interrupts
     (setf bounds (make-record :rect))
     (#_setRect bounds 0 0 32 32)

     (sk8-multival-setf (icn errorResult) (T_getResource "ICN#" resId :notfoundok t))

     (when icn  ;; nil if not found
       (#_hlock icn)
       (t_hNoPurge icn))

     (when (setf icl8 (T_getResource "icl8" resId))
       (#_hlock icl8)
       (t_hNoPurge icl8)
       (t_DetachResource icl8))
     
     ;; Allocate memory for 'cicn'.
     (setf gCicn (#_newHandleClear 84))
     ;; Fill in the cicn's bitmap fields.
     (with-dereferenced-handles ((gCicnPtr gCicn))
       
       (rset gCicnPtr :cicon.iconBMap.baseAddr (%null-ptr) :storage :pointer)
       (rset gCicnPtr :cicon.iconBMap.rowBytes 4 :storage :pointer)
       (rset gCicnPtr :cicon.iconBMap.bounds bounds :storage :pointer )
       ;; Fill in the cicn's mask bitmap fields.
       (rset gCicnPtr :cicon.iconMask.baseAddr (%null-ptr) :storage :pointer)
       (rset gCicnPtr :cicon.iconMask.rowBytes 4 :storage :pointer)
       (rset gCicnPtr :cicon.iconMask.bounds bounds :storage :pointer)
       ;; Fill in the cicn's pixmap fields.
       (rset gCicnPtr :cicon.iconPMap.baseAddr (%null-ptr) :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.rowBytes (logior 32 32768) :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.bounds bounds :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.pmVersion 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.packType 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.packSize 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.hRes #x00480000 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.vRes #x00480000 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.pixelSize depth :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.planeBytes 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.pmReserved 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.pixelType 0 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.cmpCount 1 :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.cmpSize depth :storage :pointer)
       (rset gCicnPtr :cicon.iconPMap.pmTable (#_getCTable depth) :storage :pointer)
       ;; Set the icl8 pixel image to the iconData field.
       (rset gCicnPtr :cicon.IconData icl8 :storage :pointer))
     ;; Resize the cicn for the bitmap image and the mask.
     (#_SetHandleSize gCicn (+ 82 (* bitmapSize 2)))
     
     ;; Copy the "ICN#" data into the iconMaskData array.
     ;; Note 1: this is an array of shorts, so divide bitMapSize by 2.
     ;; Note 2: the mask comes from the image.
     
     (if icn
       ; Easy case, we already have the ICN#.
       (with-dereferenced-handles ((gCicnPtr gCicn)
                                   (icnPtr icn))
         (#_BlockMove 
          icnPtr 
          (%int-to-ptr (+ (%ptr-to-int gCicnPtr) (+ 82 (the fixnum bitmapsize))))
          bitMapSize)
         (#_BlockMove 
          (%int-to-ptr (+ (%ptr-to-int icnPtr) bitMapSize))
          (rref gCicnPtr :cicon.iconMaskData :storage :pointer)
          bitMapSize))
       
       ;
       ; Else, construct b&w data and mask
       ;
       (with-dereferenced-handles ((gCicnPtr gCicn)
                                   (pIcl8Data icl8))
         ;
         ; Calculate the bitmap data and mask
         ;
         (let ( pBWDataPtr pCICNMaskPtr )
           (setf pBWDataPtr (%int-to-ptr (+ (%ptr-to-int gCicnPtr) (+ 82 (the fixnum bitmapsize)))))
           (setf pCICNMaskPtr (rref gCicnPtr :cicon.iconMaskData :storage :pointer))
           
           (let
             ((srcPixMap))
             (rlet (
                    (dstBits :Bitmap)
                    (srcRect :Rect)
                    (dstRect :Rect)
                    (seedRGB :Rect))
               
               ; Calculate the mask first. This involves calling CalcCMask which fills in the mask
               ; data in the CIcon.
               
               ;
               ; Set up the source Bitmap, namely a PixMap
               (setf srcPixMap (#_NewPixMap))	; ERROR Handling?
               
               (if (%null-ptr-p srcPixMap)	
                 ; Error, couldn't get the pixmap, so let's just set the
                 ; data and mask to black blocks.
                 (progn
                   ;
                   ; The following code is quick and dirty and will fill in both the black&white data and the mask
                   ; with solid areas of black. This is here in case we can't calculate the mask has problems.
                   ;
                   (dotimes (offset 32) 
                     (%put-long pBWDataPtr #xffffffff (* offset 4) ))
                   (dotimes (offset 32) 
                     (%put-long pCICNMaskPtr #xffffffff (* offset 4) )))
                 (progn
                   
                   ; In the next line, we set the pixmap's baseAddress to point to the dereferenced, locked
                   ; icl8.
                   (rset srcPixMap :Pixmap.baseAddr pIcl8Data :storage :handle)
                   (rset srcPixMap :Pixmap.rowBytes (logior 32 32768) :storage :handle)
                   (rset srcPixMap :Pixmap.bounds bounds :storage :handle)
                   (rset srcPixMap :Pixmap.pmVersion 0 :storage :handle)
                   (rset srcPixMap :Pixmap.packType 0 :storage :handle)
                   (rset srcPixMap :Pixmap.packSize 0 :storage :handle)
                   (rset srcPixMap :Pixmap.hRes #x00480000 :storage :handle)
                   (rset srcPixMap :Pixmap.vRes #x00480000 :storage :handle)
                   (rset srcPixMap :Pixmap.pixelSize 8 :storage :handle)
                   (rset srcPixMap :Pixmap.planeBytes 0 :storage :handle)
                   (rset srcPixMap :Pixmap.pmReserved 0 :storage :handle)
                   (rset srcPixMap :Pixmap.pixelType 0 :storage :handle)
                   (rset srcPixMap :Pixmap.cmpCount 1 :storage :handle)
                   (rset srcPixMap :Pixmap.cmpSize 8 :storage :handle)
                   (rset srcPixMap :Pixmap.pmTable (#_getCTable depth) :storage :handle)
                   
                   ; Set up the destination Bitmap. It will point at the mask data in out ColorIcon.
                   (rset dstBits :Bitmap.baseAddr pCICNMaskPtr :storage :pointer)
                   (rset dstBits :Bitmap.rowBytes 4 :storage :pointer)
                   (#_SetRect  (rref dstBits :Bitmap.bounds :storage :pointer) 0 0 32 32)
                   
                   ; Set up the source and destination rectangles
                   (#_SetRect srcRect 0 0 32 32)
                   (#_SetRect dstRect 0 0 32 32)	; Redundant
                   
                   ; Set RGBColor for seed.
                   (rset seedRGB :RGBColor.red #xffff :storage :pointer)
                   (rset seedRGB :RGBColor.green #xffff :storage :pointer)
                   (rset seedRGB :RGBColor.blue #xffff :storage :pointer)
                   
                   ; Copy the mask to the data for now
                   (let (pColorPort) 
                     (with-dereferenced-handles ((pPixmap srcPixMap))
                       (let (oldPixMap)
                         
                         (setf pColorPort (make-record :CGrafPort)) ; (#_NewPtr 108))
                         (with-port wmgr
                           (#_OpenCPort pColorPort)	; Create a new port
                           (with-port pColorPort
                             (setf oldPixMap (rref pColorPort :CGrafPort.portPixMap :storage :pointer))
                             (#_SetPortPix srcPixMap)  ; Set port's pixmap to our temporary pixmap
                             (rset pColorPort :CGrafPort.portRect srcRect :storage :pointer)
                             (#_RectRgn (rref pColorPort :CGrafPort.visRgn :storage :pointer) srcRect)
                             (#_PenNormal)
                             (#_ForeColor #$blackColor)
                             (#_BackColor #$whiteColor)
                             
                             (#_ClipRect dstRect)
                             
                             ; Calculate the pixels that can be reached from the outside bounds that are
                             ; of the seed color (seedRGB).
                             ; Use custom search proc to calculate only pixels on exterior of icon. The proc is
                             ; "CalcCMaskCICNSearchProc"
                             (#_CalcCMask  (rref pColorPort :GrafPort.portBits :storage :pointer) dstBits 
                              srcRect dstRect seedRGB 
                              CalcCMaskCICNSearchProc 0 )
                             
                             ; New destination bits
                             (rset dstBits :Bitmap.baseAddr pBWDataPtr :storage :pointer)
                             (#_CopyBits (rref pColorPort :GrafPort.portBits :storage :pointer) dstBits srcRect dstRect #$srcCopy (%null-ptr))
                             
                             
                             (#_SetPortPix oldPixMap))) ; Restore the port's old pixmap
                         (#_CloseCPort pColorPort))))
                   
                   
                   ; The next line will copy the mask over the black&white data.
                   ;  (#_BlockMove pCICNMaskPtr pBWDataPtr 128 )
                   
                   ; Release temporary PixMap                   
                   (#_DisposePixmap srcPixMap))))))))
     (#_HUnlock icl8))
    gCicn))

#|
(defvar theFile "ccl;eWorld Color Art")
(probe-file theFile)

(cl-user::with-open-res-file 
 (nil theFile)(icl8->cicn 99))

(testicl8tocicn)

(defun makecicnResource (theCICN) 
  (let ((cicnResHandle nil)
        (cicnColorTable (rref theCICN :cicon.IconPMap.pmTable :storage :Handle) )
        (cicnIconData (rref theCICN :cicon.IconData :storage :Handle) )
        blockSize err)
    
    (setf blockSize (#_GetHandleSize theCICN))	 ;; Check failure
    (setf cicnResHandle (#_NewHandle blockSize))	 ;; Check failure
    (with-dereferenced-handles ((theCICNPtr theCICN)
                                (theCopyPtr cicnResHandle))
      (#_BlockMove theCICNPtr theCopyPtr blockSize)
      )
    
    (with-dereferenced-handles ((colorTablePtr cicnColorTable))
      (let (colorTableSize)
        (setf colorTableSize (+ 8 (* (+ (rref colorTablePtr :ColorTable.ctSize :storage :pointer) 1) 8)))
        (setf err (#_PtrAndHand colorTablePtr cicnResHandle colorTableSize))))
    
    ;    (setf err (#_HandAndHand cicnColorTable cicnResHandle)) ;; Check failure
    (setf err (#_HandAndHand cicnIconData cicnResHandle))   ;; Check failure
    
    (rset cicnResHandle :CIcon.IconPMap.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconPMap.pmTable (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconMask.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconBMap.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconData (%null-ptr) :storage :Handle)
    cicnResHandle
    )
  )



(defvar theFile "ccl;eWorld Color Art")
(probe-file theFile)

(cl-user::with-open-res-file 
 (nil theFile)(icl8->cicn 99))

(setf (mediadata TrashFullIcon) *)
(mediadata TrashFullIcon)

(new Rectangle :project SK8 :objectname "Pipo" :boundsrect '(20 20 300 300) :container Stage)
(setf (fillcolor SK8::Pipo) TrashFullRenderer)
(setf (size SK8::Pipo) '(34 34))


(defun testicl8ToCICN ()
  (dolist (i '(99 100 101 102 103 104 199 200 201 202 203 204 208 209 210 211 212 213 214 215 216)) 
    (cl-user::with-open-res-file 
     (nil theFile)
     (let (theCICNRAMHandle
           theCICNResHandle
           oldCICNInResFile)
       (setf theCICNRAMHandle (icl8->cicn i))
       (setf theCICNResHandle (makecicnResource theCICNRAMHandle))
       (setf oldCICNInResFile (#_Get1Resource "cicn" i))
       (if oldCICNInResFile
         (progn
           (#_RmveResource oldCICNInResFile)
           (setf oldCICNInResFile nil))
         )
       (#_AddResource theCICNResHandle "cicn" i (%null-ptr))
       nil))))


(testicl8ToCICN)

(inspect theFile)

|#

;;; Given a cicn in RAM form, this function gets it ready to be turned into
;;; a resource. (Written by Chris Flick)

;;; someone needs to check this over a bit.  err ain't used.  -- till

(defun makecicnResource (theCICN) 
  (let ((cicnResHandle nil)
        (cicnColorTable (rref theCICN :cicon.IconPMap.pmTable :storage :Handle) )
        (cicnIconData (rref theCICN :cicon.IconData :storage :Handle) )
        blockSize err)
    
    (setf blockSize (T_GetHandleSize theCICN))	 ;; Check failure
    (setf cicnResHandle (T_NewHandle blockSize))	 ;; Check failure
    (with-dereferenced-handles ((theCICNPtr theCICN)
                                (theCopyPtr cicnResHandle))
      (#_BlockMove theCICNPtr theCopyPtr blockSize)
      )
    
    (with-dereferenced-handles ((colorTablePtr cicnColorTable))
      (let (colorTableSize)
        (setf colorTableSize (+ 8 (* (+ (rref colorTablePtr :ColorTable.ctSize :storage :pointer) 1) 8)))
        (setf err (#_PtrAndHand colorTablePtr cicnResHandle colorTableSize))))
    
    (setf err (#_HandAndHand cicnIconData cicnResHandle))   ;; Check failure
    
    (rset cicnResHandle :CIcon.IconPMap.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconPMap.pmTable (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconMask.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconBMap.baseAddr (%null-ptr) :storage :Handle)
    (rset cicnResHandle :CIcon.IconData (%null-ptr) :storage :Handle)))

;;; Finally, the sk8 function that given a file, copies the icl8s as cicns. Options
;;; available to copy to the same file or to a new file. Also allow copy
;;; of selected resources (not all of them).
;;; Note that the names and the ids are preserved if possible.

(define-sk8-function icl8ToCicn nil (sourceFile &key destinationFile selectedIds)
  (unless (fileExists sourceFile)
    (sk8-error FileNotFoundError :file sourceFile))
  ;; [1] Figure out which resources to use.
  (let* ((availableResources (macGetResourceHandleInfo sourceFile "icl8"))
         (selectedIds (if selectedIds
                        (mapcar #'(lambda (x) (memq x (cadr availableResources))) selectedIds)
                        (cadr availableResources)))
         cicnHandle)
    (when availableResources
      (withResourceFile (sourceFile)
        (mapcar #'(lambda (resName resId)
                    (when (memq resId selectedIds)
                      (setf cicnHandle (icl8->cicn resId))
                      (setf cicnHandle (makeCicnResource cicnHandle))
                      ;; And add it as a resource to the destination file. 
                      (write-handle-to-possibly-other-file sourceFile destinationFile cicnhandle "cicn" resName resId)))
                (car availableResources) (cadr availableResources))
        (#_updateResFile (#_curResFile))
        ))
    (when destinationFile
      (setf (macFileCreator destinationFile) "RSED")
      (setf (macFileType destinationFile) "RSRC"))
    *undefined*
    ))


#|

(new file :objectname "icl8File" :logicalName "sk8;icl8.rsrc" :project sk8)
(fileExists icl8File)
(macGetResourceHandleInfo icl8File "ICN#")
(icl8ToCicn icl8file)

(new file :objectname "newFile" :logicalName "sk8;iclsNoMore!" :project sk8)
(icl8ToCicn icl8file :destinationFile sk8::newFile)
(setf (macFileCreator sk8::newFile) "RSED")
(setf (macfiletype sk8::newFile) "RSRC")

|#

;;; IconRSRSTocicnTranslator
;;; _____________________  

(new translator :objectName "IconRsrcTocicntranslator" :project sk8
     :internalObject IconRSRC)

;;; Cannot import from file yet. Remove when an importFromFile method is written.

(define-handler canImport (IconRsrcTocicntranslator source destination)
  (declare (ignore destination))
  (if (inheritsFrom source file)
    nil
    (call-next-method)))

(define-handler exportToClipboard (IconRsrcTocicntranslator source destination)
  (declare (ignore destination))
  (let ((theHandle (macGetResourceHandleFromId (file source) 
                                               (restype source) 
                                               (resourceId source))))
    (if (handlep theHandle)
      (putScrap theHandle (externalType me))
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not obtain a cicn handle from " ".")
                 :objects (list source)))))


#|
	Change History (most recent last):
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
