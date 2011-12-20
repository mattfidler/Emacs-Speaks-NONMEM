;;; esn-secondary-parameters.el --- Add secondary parameter calculations
;; 
;; Filename: esn-secondary-parameters.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 14:08:08 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 13:12:58 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 7
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 27-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 27 14:08:29 2010 (-0500) #1 (Matthew L. Fidler)
;;    Added header
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(declare-function esn-narrow-rec "esn-narrow")

; Calculates secondary parameters based on ADVAN used.
(defun esn-get-vc (&optional retvp)
  "* Gets the Vc parameter if known."
  (let (
	(advan (esn-advan))
	(trans (esn-trans))
  	(vc nil)
	(vp nil)
	)
    (unless trans
      (setq trans 1)
      )
    (unless advan
      (setq advan -1)
      )
    (cond
     ( (or (= advan 1) (= advan 2))
       (save-excursion
       	 (when (not (= trans 2) )
	   ;; Find Vc
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V\\|V[0-9]+\\|VC\\)[ \t]*=" nil t)
	     (setq vc (match-string 1))
	     )
	   )
	 )
       (when (not (= trans 2))
	 (setq vc "V")
	 )
       )
     ( (= advan 3)
       (when (or (= 5 trans) (= 6 trans)
		 (not (or (= 3 trans) (= 4 trans)))
		 )
	 ;; Find Vc
	 (goto-char (point-min))
	 (when (re-search-forward "^[ \t]*\\(V\\|V1\\|VC\\)[ \t]*=" nil t)
	   (setq vc (match-string 1))
	   )
	 ;; Find Vp
	 (goto-char (point-min))
	 (when (re-search-forward "^[ \t]*\\(V2\\|VP\\)[ \t]*=" nil t)
	   (setq vp (match-string 2))
	   )
	 )
       (cond
	( (= 3 trans)
	  (setq vc "V")
	  (setq vp "V2")
	  )
	( (= 4 trans)
	  (setq vc "V1")
	  (setq vp "V2")
	  )
	( (or (= 5 trans) (= 6 trans)
		  (not (or (= 3 trans) (= 4 trans)))
		  )
	  (if vc
	      (setq vp "V2")
	    (setq vc "V1"))
	  )
	)
       )
     ( (= advan 4)
       (save-excursion
	 (when (or (= 5 trans) (= 6 trans)
		   (not (or (= 3 trans) (= 4 trans)))
		   )
	   ;; Find Vc
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V\\|V2\\|VC\\)[ \t]*=" nil t)
	     (setq vc (match-string 1))
	     )
	   ;; Find Vp
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V3\\|VP\\)[ \t]*=" nil t)
	     (setq vp (match-string 2))
	     )
	   )
	 )
       (cond
	( (= 3 trans )
	  ;; In terms of CL, V, Q, and Vss.
	  (setq vc "V")
	  (setq vp "V3")
	  )
	( (= 4 trans )
	  ;; In terms of CL, V2, Q, and V3.
	  (setq vc "V2")
	  (setq vp "V3")
	  )
	)
       (when (or (= 5 trans) (= 6 trans)
		 (not (or (= 3 trans) (= 4 trans)))
		 )
	 (if vc
	     (setq vp "V3")
	   (setq vc "V2")
	   )
	 )
       )
     ( (or (= advan 11) (= advan 12))
       ;; Three compartment.
       )
     )
    (if retvp
	(symbol-value 'vp)
      (symbol-value 'vc)
      )
    )
  )

(defun esn-secondary-parameters ()
  "* Calculates Secondary Parmaeters \& Adds to control stream"
  (interactive)
  (save-restriction
  (let (
	(advan (esn-advan))
	(trans (esn-trans))
	(inf (esn-infusion))
	(ret "")
	(dur (if (esn-infusion "\\<DUR\\>")
		 "TI"
	       "DUR"))
	(dose (if (esn-infusion "\\<\\(DOSE\\|DSE\\|CTDS\\)\\>")
		  '( "FAMT" )
		'( "DOSE")))
	;; Calculate TAD?
	(tad (if (esn-infusion "\\<\\(TAD\\|TSLD\\)\\>")
		 nil
	       "TAD"))
	(vc nil)
	(vp nil)
	(d1 nil)
	(r1 nil)
	(p1 nil)
	(p2 nil)
	(alag nil)
	)
    (unless trans
      (setq trans 1) ; Assume default TRANS.
      )
    (unless advan
      (setq advan -1)
      )
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*;| Begin Secondary Parameters[ \t]*$" nil t)
	(beginning-of-line)
	(setq p1 (point))
	(when (re-search-forward "^[ \t]*;| End Secondary Parameters[ \t]*$" nil t)
	  (end-of-line)
	  (setq p2 (point))
	  )
	)
      )
    (cond
     ( (or (= advan 1)
	   (= advan 2))
       ;; One compartment
       (setq ret
	     (concat
	      "  ;| Begin Secondary Parameters\n"
	      "  IF (AMT.GT.0) THEN\n"
	      "    ;; Fixed Amount Variable\n"
	      (mapconcat (lambda(x)
			   (concat "    " x " = AMT\n")
			   )
			 dose "")
	      (when tad
		(concat
		 "    ;; Time After Dose Calculation\n"
		 "    TDOS = TIME\n"
		 "    TAD  = 0.0\n"
		 "  ENDIF\n"
		 "  IF(AMT.EQ.0) THEN\n"
		 "    TAD  = TIME - TDOS\n"
		 )
		)
	      "  ENDIF\n"))
       (save-excursion
       	 (if (= trans 2)
	     (setq vc "V")
	   ;; Find Vc
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V\\|V[0-9]+\\|VC\\)[ \t]*=" nil t)
	     (setq vc (match-string 1))
	     )
	   )
	 ;; Find Absorption Lag Time.
	 (goto-char (point-min))
	 (when (re-search-forward "^[ \t]*\\(ALAG[0-9]+\\)[ \t]*=" nil t)
	   (setq alag (match-string 1))
	   )
	 )
       (cond
	( (= trans 2)
	  (setq ret (concat
		     ret
		     "\n  K   = CL/" vc "\n"
		     ))
	  )
	( 't
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "^ *V *=" nil t)
	      (setq vc "V")
	      (setq ret (concat
			 ret
			 "\n  CL  = K*V\n"))
	      )
	    )
	  )
	)
       (setq ret
	     (concat
	      ret
	      "  HLK  = -DLOG(0.5)/K\n"
	      (when (= advan 2)
		"  HLKA = -DLOG(0.5)/KA\n"
		)
	      ))
       (if inf
	   (progn
	     ;; IV Infusions
	     (setq ret
		   (concat
		    ret
		    "  IF (AMT.GT.0.AND.RATE.EQ.0) THEN\n"
		    "    ;; Bolus Dose\n"
		    "    " dur "  = 0\n"
		    "  ENDIF\n"
		    "  IF (AMT.GT.0.AND.RATE.GT.0) THEN\n"
		    "    ;; Infusion Dose\n"
		    "    " dur "  = AMT/RATE\n"
		    "  ENDIF\n"
		    (if (not r1) ""
		      (concat
		       "  IF (AMT.GT.0.AND.RATE.EQ.-1) THEN\n"
		       "    " dur "  = AMT/" r1 "\n"
		       "  ENDIF\n"
		       )
		      )
		    (if (not d1) ""
		      (concat
		       "  IF (AMT.GT.0.AND.RATE.EQ.-2) THEN\n"
		       "    " dur "  = " d1 "\n"
		       "  ENDIF\n"
		       ))
		    (if (not vc) ""
		       "  AUC   = " (nth 0 dose) "/CL\n"
		       "  IF (" dur ".GT.0) THEN\n"
		       "    COEF  = (" (nth 0 dose) "/" dur "/CL\n"
		       "    CMAX  = COEF*(1-DEXP(-K*" dur "))\n"
		       "    AUMB = " (nth 0 dose) "/(CL*KA)\n"
		       "    AUMC  = AUMB + (" dur "*AUC/2)\n"
		       "    MRT   = (AUMC/AUC)-" dur "/2\n"
		       "  ENDIF\n"
		       "  IF (" dur ".EQ.0) THEN\n"
		       "    ;; IV Bolus\n"
		       "    CMAX  = " (nth 0 dose) "/" vc "\n"
		       "    AUMB  = " (nth 0 dose) "/(CL*KA)\n"
		       "  ENDIF\n"
		       "  VSS   = CL*MRT\n"
		       )
		    )
		   )
	     )
	 (when vc
	   (setq ret (concat
		      ret
		      "  AUC   = " (nth 0 dose) "/CL\n"
		      (if (= advan 2)
			  (if alag
			      (concat ;; KA with lag time.
				"  TMAX = DLOG(KA/K)/(KA-K)+" alag "\n"
				"  CMAX = " (nth 0 dose) "/" vc "*DEXP(-K*(TMAX-" alag "))\n"
				)
			    ;; KA without lag time.
			    (concat
			     "  TMAX = DLOG(KA/K)/(KA-K)\n"
			     "  CMAX = " (nth 0 dose) "/" vc "*DEXP(-K*TMAX)\n"
			     )
			    )
			;; No KA
			(concat
			 "  CMAX  = " (nth 0 dose) "/" vc "\n"
			 "  AUMB  = " (nth 0 dose) "/(CL*KA)\n"
			 "  VSS   = CL*MRT\n"
			 )
			)
		       )))
	 )
       (setq ret (concat ret "  ;| End Secondary Parameters\n"))
       )
     ( (= advan 3)
       ;; Two compartment IV infusion or Bolus.
       (setq ret
	     (concat
	      "  ;| Begin Secondary Parameters\n"
	      "  IF (AMT.GT.0) THEN\n"
	      "    ;; Fixed Amount Variable\n"
	      (mapconcat (lambda(x)
			   (concat "    " x " = AMT\n")
			   )
			 dose "")
	      (when tad
		(concat
		 "    ;; Time After Dose Calculation\n"
		 "    TDOS = TIME\n"
		 "    TAD  = 0.0\n"
		 "  ENDIF\n"
		 "  IF(AMT.EQ.0) THEN\n"
		 "    TAD  = TIME - TDOS\n"
		 )
		)
	      "  ENDIF\n"))

       (save-excursion
	 ;;
	 (goto-char (point-min))
	 (when (re-search-forward "^[ \t]*\\(D1\\)[ \t]*=" nil t)
	   (setq d1 (match-string 1))
	   )
	 ;;
	 (goto-char (point-min))
	 (when (re-search-forward "^[ \t]*\\(R1\\)[ \t]*=" nil t)
	   (setq r1 (match-string 1))
	   )
	 (when (or (= 5 trans) (= 6 trans)
		   (not (or (= 3 trans) (= 4 trans)))
		   )
	   ;; Find Vc
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V\\|V1\\|VC\\)[ \t]*=" nil t)
	     (setq vc (match-string 1))
	     )
	   ;; Find Vp
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V2\\|VP\\)[ \t]*=" nil t)
	     (setq vp (match-string 2))
	     )
	   )
	 )
       ;; Make sure we have Cl, Q, Vc, Vp, Vss, K, K12, K21, ALPHA, BETA, AOB
       (cond
	( (= 3 trans )
	  ;; In terms of CL, V, Q, and Vss.
	  (setq vc "V")
	  (setq vp "V2")
	  (setq ret
		(concat
		 ret
		 "  K     = CL/V\n"
		 "  K12   = Q/V\n"
		 "  V2    = VSS-V\n"
		 "  K21   = Q/V2\n"
		 "  TMP   = DSQRT((K12+K21+K)**2 - (4*K21*K))\n"
		 "  ALPHA = ((K12+K21+K)+TMP)/2\n"
		 "  BETA  = ((K12+K21+K)-TMP)/2\n"
		 "  AOB   = (K21-ALPHA)/(BETA-K21)\n"
		 )
		)
	  )
	( (= 4 trans )
	  ;; In terms of CL, V1, Q, and V2.
	  (setq vc "V1")
	  (setq vp "V2")
	  (setq ret
		(concat
		 ret
		 "  K   = CL/V1\n"
		 "  K12 = Q/V1\n"
		 "  VSS = V1 + V2\n"
		 "  K21 = Q/V2\n"
		 "  TMP   = DSQRT((K12+K21+K)**2 - (4*K21*K))\n"
		 "  ALPHA = ((K12+K21+K)+TMP)/2\n"
		 "  BETA  = ((K12+K21+K)-TMP)/2\n"
		 "  AOB   = (K21-ALPHA)/(BETA-K21)\n"
		 )
		)
	  )
	( (= 5 trans )
	  ;; In terms of AOB, ALPHA, BETA (and V or V1).
	  (setq ret
		(concat
		 ret
		 "  K21 = (AOB*BETA+ALPHA)/(AOB+1)"
		 "  K   = (ALPHA*BETA)/K21\n"
		 "  K12 = ALPHA+BETA-K21-K\n"
		 )
		)
	  )
	( (= 6 trans )
	  ;; In terms of ALPHA, BETA, K21.
	  (setq ret
		(concat
		 ret
		 "  K   = (ALPHA*BETA)/K21\n"
		 "  K12 = ALPHA+BETA-K21-K\n"
		 "  AOB = (K21-ALPHA)/(BETA-K21)\n"
		 )
		)
	  )
	('t
	 ;; TRANS1 -- In terms of K K12 K21 (and V1 or V)
	 (setq ret
	     (concat
	      ret
	      "  TMP   = DSQRT((K12+K21+K)**2 - (4*K21*K))\n"
	      "  ALPHA = ((K12+K21+K)+TMP)/2\n"
	      "  BETA  = ((K12+K21+K)-TMP)/2\n"
	      "  AOB   = (K21-ALPHA)/(BETA-K21)\n"
	      ))
	 )
	)
       (when (or (= 5 trans) (= 6 trans)
		 (not (or (= 3 trans) (= 4 trans)))
		 )
	 (if vc
	     (progn
	       ;; Need CL, Q, and V2
	       (setq ret (concat
			  ret
			  "  CL  = K*" vc "\n"
			  "  Q   = K12*" vc "\n"
			  "  V2  = Q/K21\n"
			  "  VSS = " vc " + V2\n"
			  )
		     )
	       (setq vp "V2")
	       )
	   (when vp
	     ;; Need Cl, Q, and V1.
	     (setq ret
		   (concat
		    ret
		    "  Q   = K21*" vp "\n"
		    "  V1  = Q/K12\n"
		    "  CL  = K*V1\n"
		    "  VSS = " vp " + V1\n"
		    )
		   )
	     (setq vc "V1")
	     )
	   )
	 )
       (setq ret
	     (concat
	      ret
	      "  HLA = -DLOG(0.5)/ALPHA\n"
	      "  HLB = -DLOG(0.5)/BETA\n"
	      "  HLK = -DLOG(0.5)/K\n"
	      ))
       (if inf
	   (progn
	     ;; IV Infusions
	     (setq ret
		   (concat
		    ret
		    "  IF (AMT.GT.0.AND.RATE.EQ.0) THEN\n"
		    "    ;; Bolus Dose\n"
		    "    " dur "  = 0\n"
		    "  ENDIF\n"
		    "  IF (AMT.GT.0.AND.RATE.GT.0) THEN\n"
		    "    ;; Infusion Dose\n"
		    "    " dur "  = AMT/RATE\n"
		    "  ENDIF\n"
		    (when r1
		      (concat
		       "  IF (AMT.GT.0.AND.RATE.EQ.-1) THEN\n"
		       "    " dur "  = AMT/" r1 "\n"
		       "  ENDIF\n"
		       )
		      )
		    (when d1
		      (concat
		       "  IF (AMT.GT.0.AND.RATE.EQ.-2) THEN\n"
		       "    " dur "  = " d1 "\n"
		       "  ENDIF\n"
		       ))
		    (when (and vc vp)
		      (concat
		       "  IF (" dur ".GT.0) THEN\n"
		       "    AUC   = " (nth 0 dose) "/" vc "/K\n"
		       "    ;; AB & BB are the intercepts following IV injection\n"
		       "    AB    = (" (nth 0 dose) "/" dur "/" vc ")*(K21-ALPHA)/(ALPHA-BETA)/ALPHA\n"
		       "    BB    = -1*(" (nth 0 dose) "/" dur "/" vc ")*(K21-BETA)/(ALPHA-BETA)/BETA\n"
		       "    CMAX  = AB*(DEXP(-ALPHA*" dur ")-1.0)+BB*(DEXP(-BETA*" dur ")-1.0)\n"
		       "    ;; AI & BI are the interecepts for the infusion\n"
		       "    AI    = -1.0*AB*" dur "*ALPHA\n"
		       "    BI    = -1.0*BB*" dur "*BETA\n"
		       "    AUMB = (AI/(ALPHA*ALPHA)+BI/(BETA*BETA))\n"
		       "    AUMC  = AUMB+" dur "*AUC/2\n" ;/2.d0?
		       "    MRT   = (AUMC/AUC)-" dur "/2\n"
		       "  ENDIF\n"
		       "  IF (" dur ".EQ.0) THEN\n"
		       "    ;; IV Bolus\n"
		       "    AB    = (" (nth 0 dose) "/" vc ")*(ALPHA-K21)/(ALPHA-BETA)\n"
		       "    BB    = -1*(" (nth 0 dose) "/" vc ")*(BETA-K21)/(ALPHA-BETA)\n"
		       "    AUC   = (AB/ALPHA + BB/BETA)\n"
		       "    CMAX  = " (nth 0 dose) "/" vc "\n"
		       "    AUMC  = (AB/(ALPHA*ALPHA)) + (BB/(BETA*BETA))\n"
		       "    AUMB  = AUMC\n"
		       "    MRT   = AUMC/AUC\n"
		       "  ENDIF\n"
		       ))
		    )
		   )
	     )
	 ;; IV Bolus
	 (setq ret
	       (concat
		ret
		"  ;; IV Bolus\n"
		"  AB    = (" (nth 0 dose) "/" vc ")*(ALPHA-K21)/(ALPHA-BETA)\n"
		"  BB    = -1*(" (nth 0 dose) "/" vc ")*(BETA-K21)/(ALPHA-BETA)\n"
		"  AUC   = (AB/ALPHA + BB/BETA)\n"
		"  CMAX  = " (nth 0 dose) "/" vc "\n"
		"  AUMB  = (AB/(ALPHA*ALPHA)) + (BB/(BETA*BETA))\n"
		"  AUMC  = AUMB\n"
		"  MRT   = AUMC/AUC\n"
		))

	 )
       (setq ret (concat ret "  ;| End Secondary Parameters\n"))
       )
     ( (= advan 4)
       ;; Two Compartment
       (setq ret
	     (concat
	      "  ;| Begin Secondary Parameters\n"
	      "  IF (AMT.GT.0) THEN\n"
	      "    ;; Fixed Amount Variable\n"
	      (mapconcat (lambda(x)
			   (concat "    " x " = AMT\n")
			   )
			 dose "")
	      (when tad
		(concat
		 "    ;; Time After Dose Calculation\n"
		 "    TDOS = TIME\n"
		 "    TAD  = 0.0\n"
		 "  ENDIF\n"
		 "  IF(AMT.EQ.0) THEN\n"
		 "    TAD  = TIME - TDOS\n"
		 )
		)
	      "  ENDIF\n"))
       (save-excursion
	 (when (or (= 5 trans) (= 6 trans)
		   (not (or (= 3 trans) (= 4 trans)))
		   )
	   ;; Find Vc
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V\\|V2\\|VC\\)[ \t]*=" nil t)
	     (setq vc (match-string 1))
	     )
	   ;; Find Vp
	   (goto-char (point-min))
	   (when (re-search-forward "^[ \t]*\\(V3\\|VP\\)[ \t]*=" nil t)
	     (setq vp (match-string 2))
	     )
	   )
	 )
              (cond
	( (= 3 trans )
	  ;; In terms of CL, V, Q, and Vss.
	  (setq vc "V")
	  (setq vp "V3")
	  (setq ret
		(concat
		 ret
		 "  K     = CL/V\n"
		 "  K23   = Q/V\n"
		 "  V3    = VSS-V\n"
		 "  K32   = Q/V3\n"
		 "  TMP   = DSQRT((K23+K32+K)**2 - (4*K32*K))\n"
		 "  ALPHA = ((K23+K32+K)+TMP)/2\n"
		 "  BETA  = ((K23+K32+K)-TMP)/2\n"
		 "  AOB   = (K32-ALPHA)/(BETA-K32)\n"
		 )
		)
	  )
	( (= 4 trans )
	  ;; In terms of CL, V2, Q, and V3.
	  (setq vc "V2")
	  (setq vp "V3")
	  (setq ret
		(concat
		 ret
		 "  K     = CL/V2\n"
		 "  K23   = Q/V2\n"
		 "  VSS   = V2 + V3\n"
		 "  K32   = Q/V3\n"
		 "  TMP   = DSQRT((K23+K32+K)**2 - (4*K32*K))\n"
		 "  ALPHA = ((K23+K32+K)+TMP)/2\n"
		 "  BETA  = ((K23+K32+K)-TMP)/2\n"
		 "  AOB   = (K32-ALPHA)/(BETA-K32)\n"
		 )
		)
	  )
	( (= 5 trans )
	  ;; In terms of AOB, ALPHA, BETA (and V or V2).
	  (setq ret
		(concat
		 ret
		 "  K32 = (AOB*BETA+ALPHA)/(AOB+1)"
		 "  K   = (ALPHA*BETA)/K32\n"
		 "  K23 = ALPHA+BETA-K32-K\n"
		 )
		)
	  )
	( (= 6 trans )
	  ;; In terms of ALPHA, BETA, K32.
	  (setq ret
		(concat
		 ret
		 "  K   = (ALPHA*BETA)/K32\n"
		 "  K23 = ALPHA+BETA-K32-K\n"
		 "  AOB   = (K32-ALPHA)/(BETA-K32)\n"
		 )
		)
	  )
	('t
	 ;; TRANS1 -- In terms of K K23 K32 (and V2 or V)
	 (setq ret
	     (concat
	      ret
	      "  TMP   = DSQRT((K23+K32+K)**2 - (4*K32*K))\n"
	      "  ALPHA = ((K23+K32+K)+TMP)/2\n"
	      "  BETA  = ((K23+K32+K)-TMP)/2\n"
	      "  AOB   = (K32-ALPHA)/(BETA-K32)\n"
	      ))
	 )
	)
       (when (or (= 5 trans) (= 6 trans)
		 (not (or (= 3 trans) (= 4 trans)))
		 )
	 (if vc
	     (progn
	       ;; Need CL, Q, and V3
	       (setq ret (concat
			  ret
			  "  CL  = K*" vc "\n"
			  "  Q   = K23*" vc "\n"
			  "  V3  = Q/K32\n"
			  "  VSS = " vc " + V3\n"
			  )
		     )
	       (setq vp "V3")
	       )
	   (when vp
	     ;; Need Cl, Q, and V2.
	     (setq ret
		   (concat
		    ret
		    "  Q   = K32*" vp "\n"
		    "  V2  = Q/K23\n"
		    "  CL  = K*V2\n"
		    "  VSS = " vp " + V2\n"
		    )
		   )
	     (setq vc "V2")
	     )
	   )
	 )
       (setq ret
	     (concat
	      ret
	      "  HLA  = -DLOG(0.5)/ALPHA\n"
	      "  HLB  = -DLOG(0.5)/BETA\n"
	      "  HLK  = -DLOG(0.5)/K\n"
	      "  HLKA = -DLOG(0.5)/KA\n"
	      "  AUC  = " (nth 0 dose) "/" vc "/K\n"
	      "  CA   = (" (nth 0 dose) "/" vc   ")*KA*(K32-ALPHA)/(ALPHA-BETA)/(ALPHA-KA)\n"
	      "  CB   = -1*(" (nth 0 dose) "/" vc ")*KA*(K32-BETA)/(ALPHA-BETA)/(BETA-KA)\n"
	      "  ;| End Secondary Parameters\n"
	      ))

       )
     ( (or (= advan 11) (= advan 12))
       ;; Three Compartment

       ;;

       ;; IV Bolus
       ;; d=con(3)
       ;; cmax = d*(a1 + b1 + c1)
       ;; v1 = d / cmax
       ;; e1 = 1
       ;; e2 = gamma*b1 + gamma*a1 + beta*c1 + beta*a1 + alpha*c1 + alpha*b1
       ;; e2 = - d*e2 / cmax
       ;; e3 = d*( alpha*gamma*b1 + gamma*beta*a1 + alpha*beta*c1 ) / cmax
       ;; k21 = .5 * (-e2 + sqrt(e2*e2 - 4*e3))
       ;; k31 = .5 * (-e2 - sqrt(e2*e2 - 4*e3))
       ;; k10 = alpha * beta * gamma / (k21 * k31)
       ;; e1 = alpha*beta + alpha*gamma + beta*gamma
       ;; e2 = k21 * (alpha + beta + gamma)
       ;; e3 = e1 - e2 - k10*k31 + k21*k21
       ;; k12 = e3 / (k31 - k21)
       ;; k13 = (alpha + beta + gamma) - (k10 + k12 + k21 + k31)
       ;; k10_hl=-dlog(.5)/k10
       ;; alpha_hl=-dlog(.5)/alpha
       ;; beta_hl=-dlog(.5)/beta
       ;; gamma_hl=-dlog(.5)/gamma
       ;; auc = d*((a1/alpha) + (b1/beta) + (c1/gamma))
       ;; cl=d/auc
       ;; aumc=d*(a1/(alpha*alpha) + b1/(beta*beta) + c1/(gamma*gamma))
       ;; mrt=aumc/auc
       ;; vss=cl*mrt
       ;; cld2=k12*v1
       ;; v2=cld2/k21
       ;; cld3=k13*v1
       ;; v3=cld3/k31

       ;; IV infusion

       ;; d=con(2)
       ;; ti=con(4) - con(3)
       ;; k0 = d/ti
       ;; a1 = k0 * (k21 - alpha) * (k31 - alpha)
       ;; a1 = a1 / (v * alpha * (gamma - alpha) * (beta - alpha))
       ;; b1 = k0 * (k21 - beta) * (k31 - beta)
       ;; b1 = b1 / (v * beta * (gamma - beta) * (alpha - beta))
       ;; c1 = k0 * (k21 - gamma) * (k31 - gamma)
       ;; c1 = c1 / (v * gamma * (alpha - gamma) * (beta - gamma))
       ;; cmax = a1 * (1.d0 - exp(-alpha*ti)) +  &
       ;;        b1 * (1.d0 - exp(-beta*ti)) +   &
       ;;        c1 * (1.d0 - exp(-gamma*ti))
       ;; k10 = alpha * beta * gamma / (k21 * k31)
       ;; e1 = alpha*beta + alpha*gamma + beta*gamma
       ;; e2 = k21 * (alpha + beta + gamma)
       ;; e3 = e1 - e2 - k10*k31 + k21*k21
       ;; k12 = e3 / (k31 - k21)
       ;; k13 = (alpha + beta + gamma) - (k10 + k12 + k21 + k31)
       ;; a = d*(k21-alpha)*(alpha-k31)/(v*(gamma-alpha)*(alpha-beta))
       ;; b = d*(k21-beta)*(k31-beta)/(v*(gamma-beta)*(alpha-beta))
       ;; c = d*(k21-gamma)*(k31-gamma)/(v*(gamma-alpha)*(gamma-beta))
       ;; k10_hl=-dlog(.5)/k10
       ;; alpha_hl=-dlog(.5)/alpha
       ;; beta_hl=-dlog(.5)/beta
       ;; gamma_hl=-dlog(.5)/gamma
       ;; auc = (a/alpha) + (b/beta) + (c/gamma)
       ;; cl=d/auc
       ;; aumcb=(a/(alpha*alpha) + b/(beta*beta) + c/(gamma*gamma))
       ;; aumc=aumcb + ti*auc/2.d0
       ;; mrt=(aumc/auc) - ti/2
       ;; vss=d*((aumc/auc) - ti/2.d0)/auc
       ;; cld2=k12*v
       ;; v2=cld2/k21
       ;; cld3=k13*v
       ;; v3=cld3/k31
       )
     )
    (while (string-match "\\\\" ret)
      (setq ret (replace-match "/" nil nil ret)))
    (save-excursion
      (if (and p1 p2)
	  (progn
	    ;; Replace
	    (goto-char p1)
	    (delete-region p1 p2)
	    (insert (substring ret 0 -1))
	    )
	;; Insert
	(goto-char (point-min))
	(when (re-search-forward "\\<\\$PK" nil t)
	  (save-restriction
	    (esn-narrow-rec)
	    (goto-char (point-max))
	    (unless (string= "" ret)
	      (insert "\n\n")
	      (insert ret)
	      )
	    (widen)
	    )
	  )
	)
      )
    )
  ))


(provide 'esn-secondary-parameters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-secondary-parameters.el ends here
