;;; ================================================================
;;;  TiltViewport.lsp   v2.0
;;;  Viewport / View assistant for AutoCAD and SouthMap
;;;  NOTE: all prompt messages are ASCII, no encoding / garbage text.
;;; ----------------------------------------------------------------
;;;  COMMANDS
;;;    TVR  - Straighten tilted graphics INSIDE an existing viewport (MAIN)
;;;    TVP  - Turn a closed polyline drawn in paper space into a viewport
;;;    TVQ  - One-step tilted viewport: 2 corners + rotation + scale
;;;    TVU  - Unlock all viewports
;;; ----------------------------------------------------------------
;;;  LOAD:  APPLOAD  ->  pick this file
;;; ================================================================

(vl-load-com)

;;; Angle unit helpers -- keep all conversions in ONE place.
(defun _tv:d2r (d) (* d (/ pi 180.0)))   ; degrees -> radians
(defun _tv:r2d (r) (* r (/ 180.0 pi)))   ; radians -> degrees

;;; Normalize an angle to [-pi/2, +pi/2]
(defun _tv:norm (a)
  (while (> a pi) (setq a (- a (* 2.0 pi))))
  (while (< a (- pi)) (setq a (+ a (* 2.0 pi))))
  (cond
    ((> a (/ pi 2.0)) (- a pi))
    ((< a (- (/ pi 2.0))) (+ a pi))
    (T a)
  )
)

;;; VLA Coordinates -> list of (x y) points
(defun _tv:coords (ent / obj c r i n)
  (setq obj (vlax-ename->vla-object ent)
        c   (vlax-get obj 'Coordinates)
        r   nil i 0 n (length c))
  (while (< i n)
    (setq r (cons (list (nth i c) (nth (1+ i) c)) r)
          i (+ i 2))
  )
  (reverse r)
)

;;; Angle of the longest edge of a polyline
(defun _tv:angle (ent / vs n i p1 p2 len best blen)
  (setq vs (_tv:coords ent) n (length vs) best nil blen -1 i 0)
  (while (< i n)
    (setq p1  (nth i vs)
          p2  (nth (rem (1+ i) n) vs)
          len (distance p1 p2))
    (if (> len blen) (setq blen len best (list p1 p2)))
    (setq i (1+ i))
  )
  (if best (_tv:norm (angle (car best) (cadr best))) 0.0)
)

;;; Rotate a 2D point around a base by angle a (radians)
(defun _tv:rotpt (p b a / x y ca sa)
  (setq x  (- (car p) (car b)) y (- (cadr p) (cadr b))
        ca (cos a) sa (sin a))
  (list (+ (car b) (- (* x ca) (* y sa)))
        (+ (cadr b) (+ (* x sa) (* y ca))))
)

;;; Find the VIEWPORT entity matching the current CVPORT
(defun _tv:curvp ( / ss i ed res)
  (setq ss (ssget "_X" '((0 . "VIEWPORT"))) i 0 res nil)
  (if ss
    (while (< i (sslength ss))
      (setq ed (entget (ssname ss i)))
      (if (= (cdr (assoc 69 ed)) (getvar "CVPORT"))
        (setq res (ssname ss i))
      )
      (setq i (1+ i))
    )
  )
  res
)

;;; Create a lightweight polyline in the CURRENT space (paper space when in layout)
(defun _tv:mkrect (pts / obj arr i doc)
  (setq arr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length pts)))))
        i 0)
  (foreach p pts
    (vlax-safearray-put-element arr i (car p))
    (vlax-safearray-put-element arr (1+ i) (cadr p))
    (setq i (+ i 2))
  )
  (setq doc (vlax-get-property (vlax-get-acad-object) 'ActiveDocument)
        obj (vlax-get-property doc
              (if (= (getvar "TILEMODE") 0) 'PaperSpace 'ModelSpace))
  )
  (setq obj (vlax-invoke obj 'AddLightweightPolyline arr))
  (vlax-put obj 'Closed :vlax-true)
  (vlax-vla-object->ename obj)
)

;;; Unlock / lock a viewport by ename
(defun _tv:lock (vent flag / o)
  (if vent
    (progn
      (setq o (vlax-ename->vla-object vent))
      (if (vlax-property-available-p o 'DisplayLocked)
        (vlax-put o 'DisplayLocked flag)
      )
    )
  )
)

;;; ---------------------------------------------------------------
;;;  TVR  --  straighten graphics inside an EXISTING viewport
;;;  Works in PAPER SPACE only. Sets the viewport's TwistAngle property.
;;;  Does NOT change CVPORT, does NOT activate the viewport.
;;; ---------------------------------------------------------------
(defun c:TVR (/ oldcmd vent vobj ed a ans refent rp1 rp2 wasLocked tv-result tv-read)
  (vl-load-com)
  (if (= (getvar "TILEMODE") 1)
    (princ "\n[ERROR] Switch to a LAYOUT tab first (paper space).")
    (progn
      (setq oldcmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)

      ;; Step 1 - select the viewport (click its border), in paper space
      (princ "\nSelect the VIEWPORT to straighten (click its border): ")
      (setq vent (car (entsel)))

      (if (null vent)
        (princ "\n[CANCEL] Nothing selected.")
        (if (/= (cdr (assoc 0 (entget vent))) "VIEWPORT")
          (princ "\n[ERROR] Please select a VIEWPORT border.")
          (progn
            (setq vobj (vlax-ename->vla-object vent))
            (setq wasLocked
              (and (vlax-property-available-p vobj 'DisplayLocked)
                   (= (vla-get-DisplayLocked vobj) :vlax-true)))

            ;; Step 2 - get the twist angle
            ;; Option A: pick a reference line IN THE VIEWPORT (requires active viewport;
            ;;           user double-clicks into it first, or uses VPMax)
            ;; Option B: type the angle directly (works always)
            (princ "\nPick a reference LINE to make horizontal, or press Enter to type angle: ")
            (setq refent (car (entsel)))
            (cond
              ((and refent (= (cdr (assoc 0 (entget refent))) "LINE"))
               (setq ed (entget refent)
                     rp1 (cdr (assoc 10 ed))
                     rp2 (cdr (assoc 11 ed)))
               (setq a (- (_tv:norm (angle rp1 rp2))))
               (princ (strcat "\n  Reference line angle = "
                              (rtos (_tv:r2d (_tv:norm (angle rp1 rp2))) 2 2)
                              " deg -> twist = "
                              (rtos (_tv:r2d a) 2 2) " deg"))
              )
              ((null refent)
               (setq ans (getstring T "\nTwist angle in deg (0=reset, +/- flips direction) <0>: "))
               (if (or (null ans) (= ans "")) (setq ans "0"))
               (setq a (_tv:d2r (atof ans)))
              )
              (T
               (setq a nil)
               (princ "\n[ERROR] Reference must be a LINE.")
              )
            )

            ;; Step 3 - apply via VLA TwistAngle (no CVPORT, no viewport activation)
            (if a
              (if (not (vlax-property-available-p vobj 'TwistAngle))
                (princ "\n[ERROR] This CAD does not expose TwistAngle on VIEWPORT.")
                (progn
                  (if wasLocked (vla-put-DisplayLocked vobj :vlax-false))
                  (setq tv-result
                    (vl-catch-all-apply '(lambda () (vla-put-TwistAngle vobj a))))
                  (if wasLocked (vla-put-DisplayLocked vobj :vlax-true))
                  (if (vl-catch-all-error-p tv-result)
                    (princ "\n[ERROR] Failed to set twist. Unlock viewport (TVU) and retry.")
                    (progn
                      (princ (strcat "\n  Applied twist = " (rtos (_tv:r2d a) 2 2) " deg"))
                      (setq tv-read
                        (vl-catch-all-apply '(lambda () (vla-get-TwistAngle vobj))))
                      (if (vl-catch-all-error-p tv-read)
                        (princ "\n  Verify: readback unavailable (check view visually)")
                        (princ (strcat "\n  Verified twist = "
                                       (rtos (_tv:r2d tv-read) 2 2) " deg"))
                      )
                      (princ "\n[DONE] View straightened. If direction is wrong, run TVR again with opposite sign.")
                    )
                  )
                )
              )
            )
          )
        )
      )
      (setvar "CMDECHO" oldcmd)
    )
  )
  (princ)
)

;;; ---------------------------------------------------------------
;;;  TVP  --  convert an existing closed polyline into a viewport
;;; ---------------------------------------------------------------
(defun c:TVP (/ ent tw sc ang)
  (vl-load-com)
  (if (= (getvar "TILEMODE") 1)
    (princ "\n[ERROR] Switch to a LAYOUT tab first (paper space).")
    (progn
      (command "_.PSPACE")
      (princ "\nSelect a closed polyline in PAPER SPACE (drawn with REC): ")
      (setq ent (car (entsel)))
      (cond
        ((null ent)
         (princ "\n[CANCEL] Nothing selected.")
        )
        ((/= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
         (princ "\n[ERROR] Select a closed LWPOLYLINE (use REC), not a LINE.")
        )
        (T
         (setq ang (_tv:r2d (_tv:angle ent)))
         (princ (strcat "\n  Polyline long-edge angle = " (rtos ang 2 1) " deg"))
         (setq tw (getreal
                    (strcat "\nView twist angle in deg (Enter = no twist, suggested: "
                            (rtos (- ang) 2 1) ") : ")))
         (if (null tw) (setq tw 0.0))
         (setq sc (getstring T "\nScale denominator <1000 means 1:1000> : "))
         (if (or (= sc "") (null sc)) (setq sc "1000"))
         (command "_.MVIEW" "_O" ent "")
         (if (and tw (not (zerop tw)))
           (progn
             (command "_.UCS" "_Z" (rtos tw 2 6))
             (command "_.PLAN" "_C")
             (command "_.UCS" "_W")
           )
         )
         (command "_.ZOOM" (strcat "1/" sc "xp"))
         (command "_.PSPACE")
         (_tv:lock (entlast) :vlax-true)
         (princ (strcat "\n[DONE] Viewport created, scale 1:" sc ", locked."))
        )
      )
    )
  )
  (princ)
)

;;; ---------------------------------------------------------------
;;;  TVQ  --  draw a tilted rectangle and turn it into a viewport
;;; ---------------------------------------------------------------
(defun c:TVQ (/ p1 p2 rot base pts ent sc)
  (vl-load-com)
  (if (= (getvar "TILEMODE") 1)
    (princ "\n[ERROR] Switch to a LAYOUT tab first (paper space).")
    (progn
      (command "_.PSPACE")
      (setq p1 (getpoint "\nFirst corner of rectangle in PAPER SPACE: "))
      (if p1
        (progn
          (setq p2 (getcorner p1 "\nOther corner: "))
          (setq rot (getreal "\nRotation angle in deg (25 or -25, Enter=0): "))
          (if (null rot) (setq rot 0.0))
          (setq base (list (/ (+ (car p1) (car p2)) 2.0)
                           (/ (+ (cadr p1) (cadr p2)) 2.0))
                pts  (list (list (car p1) (cadr p1))
                           (list (car p2) (cadr p1))
                           (list (car p2) (cadr p2))
                           (list (car p1) (cadr p2))))
          (setq pts (mapcar '(lambda (p) (_tv:rotpt p base (_tv:d2r rot))) pts))
          (setq ent (_tv:mkrect pts))
          (princ (strcat "\n  Rectangle rotated " (rtos rot 2 1) " deg, creating viewport..."))
          (setq sc (getstring T "\nScale denominator <1000 means 1:1000> : "))
          (if (or (= sc "") (null sc)) (setq sc "1000"))
          (command "_.MVIEW" "_O" ent "")
          (command "_.ZOOM" (strcat "1/" sc "xp"))
          (command "_.PSPACE")
          (_tv:lock (entlast) :vlax-true)
          (princ (strcat "\n[DONE] Tilted viewport created, scale 1:" sc ", locked."))
        )
      )
    )
  )
  (princ)
)

;;; ---------------------------------------------------------------
;;;  TVU  --  unlock all viewports
;;; ---------------------------------------------------------------
(defun c:TVU (/ ss i o)
  (vl-load-com)
  (setq ss (ssget "_X" '((0 . "VIEWPORT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq o (vlax-ename->vla-object (ssname ss i)))
        (if (vlax-property-available-p o 'DisplayLocked)
          (vlax-put o 'DisplayLocked :vlax-false)
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n[DONE] " (itoa (sslength ss)) " viewport(s) unlocked."))
    )
    (princ "\n[INFO] No viewport found.")
  )
  (princ)
)

(princ "\n===== TiltViewport v2.2 loaded (angle fix). Commands: TVR / TVP / TVQ / TVU =====")
(princ)
