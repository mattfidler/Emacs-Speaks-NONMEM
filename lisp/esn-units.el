;;; esn-units.el --- Units conversion
;; 
;; Filename: esn-units.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Apr 22 12:17:48 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 15:18:41 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 30
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
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:17:57 2010 (-0500) #1 (Matthew L. Fidler)
;;    Added Error Handling
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

(declare-function esn-xpose-get-input-line "esn-xpose")
(declare-function esn-error "esn-exec")

;;
;; This is to convert units and provide tab completion to units for S1=...
;;

;;;###autoload
(defun esn-get-time-units (&optional prompt)
  "Gets the time or prompts for a time variable."
  (let (
	(time nil)
	(cfs case-fold-search)
	)
    (setq case-fold-search nil)
    (save-excursion
      (goto-char (point-min))
      (if (or
	   (re-search-forward (concat "[/(]\\(" esn-time-regexp "\\)\\(?:[)/]\\|[ \t]*$\\)") nil 't)
	   (re-search-forward (concat "[/(]\\(" esn-prefix-regexp "?" esn-time-regexp "\\)\\(?:[)/]\\>\\|[ \t]*$\\)") nil 't)
	   )
	  (setq time (cadr (assoc (match-string 1) esn-time-units)))
	)
      )
    (setq case-fold-search cfs)
    (symbol-value 'time)
    )
  )

(defun esn-get-cmt ()
  "Gets the compartment that the scale factor is referring to."
  (interactive)
  (save-excursion
    (let (
	  (advan nil)
	  (sc nil)
	  (v nil)
	  (vn nil)
	  (lst nil)
	  (case-fold-search 't)
	  )
      (save-excursion
	(beginning-of-line)
	(when (looking-at " *S\\([0-9CO]\\)\\>")
	  (setq sc (match-string 1))
	  )
	)
      (when sc
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward "\\<\\$SUB" nil t)
	  (if (re-search-forward "\\<ADVAN\\([0-9]+\\)\\>" nil t)
	      (setq advan (match-string 1))
	    )
	  )
	(setq v (upcase sc))
	(mapc (lambda (x)
		(let (
		      (what (nth 0 x))
		      (with (nth 1 x))
		      )
		  (when (string-match what sc)
		    (setq sc (replace-match with nil nil sc)))
		  )
		)
	      (nth 1 (assoc advan esn-mode-scale-translations))
              )
	(if (string-match "^[0-9]+$" sc)
	    (setq sc (concat "Compartment #" sc))
	  (setq sc (concat sc " Compartment"))
	  )
	(when (string= v "C")
	  (mapc (lambda (x)
		  (let (
			(what (nth 0 x))
			(with (nth 1 x))
			)
		    (when (and (string= with "Central")
			       (string-match "^[1-9][0-9]*$" what))
		      (setq v what)
		      )
		    )
		  )
		(nth 1 (assoc advan esn-mode-scale-translations))
                )
	  )
	(when (or (string= v "O") (string= v "0"))
	  (mapc (lambda (x)
		  (let (
			(what (nth 0 x))
			(with (nth 1 x))
			)
		    (when (and (string= with "Output")
			       (string-match "^[1-9][0-9]*$" what)))
		    (setq v what)
		    )
		  )
                (nth 1 (assoc advan esn-mode-scale-translations))
                )
          )
	(setq vn v)
	(setq v (concat "V" v))
	(save-excursion
	  (goto-char (point-min))
	  (unless (re-search-forward (format "\\<%s\\> *=" v) nil t)
	    (when (re-search-forward "\\<V\\> *=" nil t)
	      (setq v "V")
	      )
	    )
	  (goto-char (point-min))
	  (unless (re-search-forward (format "\\<%s\\> *=" v) nil t)
	    (when (re-search-forward "\\<CL\\> *=" nil t)
	      (goto-char (point-min))
	      (if (re-search-forward "\\<K\\> *=" nil t)
		  (setq v "CL/K")
		(if (re-search-forward (format "\\<K%s0\\> *=" vn) nil t)
		    (setq v (format "CL/K%s0" vn))
		  (if (re-search-forward "\\<KE\\> *=" nil t)
		      (setq v "CL/KE")
		    )
		  )
		)
	      )
	    )
	  )
	(save-excursion
	  (unless (re-search-backward "= *\\=" nil t)
	    (if (re-search-backward (format "= *%s *\\=" (regexp-quote v)) nil t)
		(setq v "")
	      (if (re-search-backward "=.*?\\=" nil t)
		  (setq v "*")
		)
	      )
	    )
	  )
	(setq sc (list sc v))
	)
      (symbol-value 'sc)
      )
    )
  )
(defun esn-get-dose-units (&optional ignoreScale)
  (let (
	(dose "")
	(case-fold-search nil)
	)
    (unless ignoreScale
      (let (
	    (pk (esn-rec "PK" 't))
	    )
	(when (string-match "^[ \t]*[Ss][0-9]+[ \t]*=.*?;.*?[Dd][Oo][Ss][Ee][ \t]*:[ \t]*\\([^;]*\\).*" pk)
	  (setq dose (match-string 1 pk))
	  )
	)
      )
    (when (string= dose "")
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward (eval-when-compile
				   (concat
				    "^[ \t]*;|? *[Aa][Mm][Tt]: *.*?\\<\\("
				    esn-prefix-regexp
				    "?\\(?:mol\\|g\\)\\(?: *\\(?:\\\\\\||\\|/\\) *"
				    esn-prefix-regexp
				    "?\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)?\\)\\>.*$")
				   ) nil t)

	  (setq dose (match-string 1))
	  )
	)
      )
    (symbol-value 'dose)
    )
  )
(defun esn-get-volume-units (&optional ignoreScale)
  (let (
	(volume "")
	)
    (unless ignoreScale
      (let (
	    (pk (esn-rec "PK" 't))
	    )
	(when (string-match "^[ \t]*[Ss][0-9]+[ \t]*=.*?;.*?[Vv][Oo][Ll][A-Za-z]*[ \t]*:[ \t]*\\([^;]*\\).*" pk)
	  (setq volume (match-string 1 pk))
	  )
	)
      )
    (when (string= volume "")
      ;; Get Volume
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward (concat
				  "^[ \t]*\\<\\(?:[Tt][Vv]\\)?[Vv][0-9]+[ \t]*=.*?;.*?\\<\\("
				  esn-prefix-regexp
				  "?[lL]\\(?: *\\(?:\\\\\\||\\|/\\) *\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)?\\)\\>") nil t)
	  (setq volume (match-string 1))
	  )
	)
      (when (string= volume "")
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward (concat
				    "^[ \t]*\\<\\(?:[Tt][Vv]\\)?\\(?:[Cc][Ll]\\|[Qq][0-9]+\\)+[ \t]*=.*?;.*?\\<\\("
				    esn-prefix-regexp
				    "?[lL]\\) *\\(?:\\\\\\||\\|/\\) *"
				    esn-time-regexp
				    " *\\( *\\(?:\\\\\\||\\|/\\) *\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)\\>") nil t)
	    (setq volume (concat (match-string 1) (match-string 2)))
	    )
	  )
	)
      (when (string= volume "")
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward (concat
				    "^[ \t]*\\<\\(?:[Tt][Vv]\\)?\\(?:[Cc][Ll]\\|[Qq][0-9]+\\)+[ \t]*=.*?;.*?\\<\\("
				    esn-prefix-regexp
				    "?[lL]\\) *\\(?:\\\\\\||\\|/\\) *"
				    esn-time-regexp "\\>") nil t)
	    (setq volume (match-string 1))
	    )
	  )
	)
      (when (string= volume "")
	(let (
	      (theta (esn-rec "THE" 't))
	      )
	  (when (string-match
		 (concat
		  ";.*?\\<[Vv]\\(?:[CcpP]\\|[0-9]+\\|[Oo][Ll][A-Za-z.]*\\).*?\\<\\("
		  esn-prefix-regexp
		  "?[lL]\\(?: *\\(?:\\\\\\||\\|/\\) *\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)?\\)\\>") theta)
	    (setq volume (match-string 1 theta))
	    )
	  (when (string= volume "")
	    (when (string-match
		   (concat
		    ";.*?\\<\\(?:[Cc][Ll]\\|[Qq]\\).*?\\(\\<"
		    esn-prefix-regexp
		    "?[lL]\\)"
		    " *\\(?:\\\\\\||\\|/\\) *"
		    esn-time-regexp
		    " *\\(\\(?:\\\\\\||\\|/\\) *"
		    "\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)"
		    "\\)"
		    ) theta)
	      (setq volume (concat (match-string 1 theta) (match-string 2 theta)))
	      )
	    )
	  (when (string= volume "")
	    (when (string-match
		   (concat
		    ";.*?\\<\\(?:[Cc][Ll]\\|[Qq]\\).*?\\(\\<"
		    esn-prefix-regexp
		    "?[lL]\\)"
		    " *\\(?:\\\\\\||\\|/\\) *"
		    esn-time-regexp
		    ) theta)
	      (setq volume (match-string 1 theta))
	      )
	    )
	  )
	)
      )
    (symbol-value 'volume)
    )
  )
(defun esn-units-tmp-fn (x conc)
  "* Temporary units function"
  (when (and (not (string= "" x)) (string= conc ""))
    (let (
          (reg x)
          (i 0)
          )
      (while (< i (length reg))
        (setq reg (concat
                   (substring reg 0 i)
                   "[" (downcase (substring reg i (+ i 1)))
                   (upcase (substring reg i (+ i 1))) "]"
                   (substring reg (+ i 1))))
        (setq i (+ i 4))
        )
      (setq reg (concat "^;|? *" reg " *: *.*?\\<\\(\\("
                        esn-prefix-regexp
                        "?g\\) *\\(?:\\\\\\||\\|/\\) *\\("
                        esn-prefix-regexp "?[lL]\\)\\|"
                        esn-prefix-regexp "?M\\)\\>.*$"
                        ))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward reg nil t)
          (setq conc (match-string 1))
          )
        )
      )
    )
  )
(defun esn-get-concentration-units (&optional ignoreScale)
  (let (
	(tlst '())
	(conc "")
	)
    (unless ignoreScale
      (let (
	    (pk (esn-rec "PK" 't))
	    )
	(when (string-match "^[ \t]*[Ss][0-9]+[ \t]*=.*?;.*?[Cc]\\(?:[pP]\\|[Oo][Nn][Cc][A-Za-z]*\\)[ \t]*:[ \t]*\\([^;]*\\).*" pk)
	  (setq conc (match-string 1 pk))
	  )
	)
      )
    (when (string= conc "")
      ;; Get Concentration
      (let (
            (lg (esn-mode-is-log))
            )
        (if (and lg (equal (type-of lg) 'string))
            (setq tlst lg)
          (setq tlst nil)
          )
        )
      (if tlst
	  (setq tlst (append tlst esn-mode-current-dv))
	(setq tlst esn-mode-current-dv)
	)
      (if (equal 'string (type-of tlst))
	  (esn-units-tmp-fn tlst conc)
	(mapc (lambda(x) (esn-units-tmp-fn x conc)) tlst)
	)
      )
    (while (string-match "[ \t\n]+" conc)
      (setq conc (replace-match "" nil nil conc)))
    (symbol-value 'conc)
    )
  )
;;;###autoload
(defun esn-scale (&optional no-insert)
  "* Gets scale factor from control stream"
  (interactive)
  (let (
	(case-fold-search nil)
	(cmt (esn-get-cmt))
	(scale "")
	(tmp1 "")
	(tmp2 "")
	(has-eq nil)
	(inp-reg (esn-xpose-get-input-line 't))
        (ret "")
	)
    (if (string= "*" (nth 1 cmt))
	(progn
	  ;(esn-magic-tab 't) ;; Skip scale check.
	  )
    (when cmt
      (let (
	    (dose (esn-get-dose-units 't))
	    (conc (esn-get-concentration-units 't))
	    (volume (esn-get-volume-units 't))
            (volume-dose nil)
	    (mw nil)
	    (bw nil)
	    (vbw nil)
	    (wtv "")
	    (dn nil)
	    (vn nil)
	    (tlst '());
	    )
	;; Now prompt
	(setq dose (esn-prompt (format "%s Dose Units: " (nth 0 cmt))
			       (concat "^[ \t]*\\([%]\\|\\(" esn-prefix-regexp "?\\(?:mol\\|g\\)\\(?: *\\(?:\\\\\\||\\|/\\) *" esn-prefix-regexp "?\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)?\\)\\)[ \t]*$")
			       "Invalid Dose Units, please re-enter dose units (e.g. %, mg, mmol, mg/kg, or mg/m^2): "
			       nil
			       dose
			       ))

	(setq conc (esn-prompt (format "%s Concentration Units (e.g. g/L or nM): " (nth 0 cmt))
			       (concat
				"^[ \t]*\\(\\("
				esn-prefix-regexp
				"?g\\) *\\(?:\\\\\\||\\|/\\) *\\("
				esn-prefix-regexp "?[lL]\\)\\|"
				esn-prefix-regexp "?M\\)[ \t]*$"
				)
			       "Invalid Concentration Units, please re-enter concentration units: "
			       nil
			       conc
			       ))

	(if (not (string-match "\\(?:\\\\\\||\\|/\\)" dose))
	    (setq volume
		  (esn-prompt (format "Desired %s Volume Units: " (nth 0 cmt))
			      (concat "^[ \t]*\\(" esn-prefix-regexp
				      "?[lL]\\)[ \t]*$")
			      "Invalid Compartment Volume Units, please re-enter volume units: "
			      nil
			      volume)
		  )
	  (if (string-match "2 *$" dose)
	      (setq volume
		  (esn-prompt (format "Desired %s Volume Units: " (nth 0 cmt))
			      (concat "^[ \t]*\\(" esn-prefix-regexp
				      "?[lL]\\)\\(?: *\\(?:\\\\\\||\\|/\\) *" esn-prefix-regexp "?m\\(?:\\^\\|\\*\\*\\)?2\\)?[ \t]*$")
			      "Invalid Compartment Volume Units, please re-enter volume units (L or L/m^2): "
			      nil
			      volume
			      )
		  )
	    (setq volume
		  (esn-prompt (format "Desired %s Volume Units: " (nth 0 cmt))
			      (concat "^[ \t]*\\(" esn-prefix-regexp
				      "?[lL]\\)\\(?: *\\(?:\\\\\\||\\|/\\) *" esn-prefix-regexp "?g\\)?[ \t]*$")
			      "Invalid Compartment Volume Units, please re-enter volume units (L or L/kg): "
			      nil
			      volume
			      )
		  )
	    )
	  )
	(when (string-match (concat " *\\(?:\\\\\\||\\|/\\) *\\("
				    esn-prefix-regexp
				    "?\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)[ \t]*$") dose)
	  (setq bw (match-string 1 dose))
	  (setq dose (replace-match "" nil nil dose))
	  (mapc (lambda(x)
		  (let (
			(case-fold-search 't)
			)
		    (when (string-match (format "^%s$" inp-reg) x)
		      (setq wtv (concat "/" (upcase x)))
		      )
		    )
		  )
		(if (string-match "2$" bw)
		    esn-xpose-bsa-variables
		  esn-xpose-weight-variables
		 )
		)
	  (when (string-match (concat " *\\(?:\\\\\\||\\|/\\) *\\(" esn-prefix-regexp "?\\(?:g\\|m\\(?:\\^\\|\\*\\*\\)?2\\)\\)[ \t]*$") volume)
	    (setq  vbw (match-string 1 volume))
	    (setq volume (replace-match "" nil nil volume))
	    (setq wtv "")
	    (setq dn bw)
	    (setq vn vbw)
	    )
	  )
	(if (and (string-match "M$" conc) (string-match "mol$" dose))
	    (progn
	      ;; Same as if converting g, convert concentration and string to match
	      (when (string-match "M$" conc)
		(setq tmp1 (replace-match "g/L" nil nil conc)))
	      (when (string-match "mol$" dose)
		(setq tmp2 (replace-match "g" nil nil dose)))
	      (setq scale (esn-get-scale-factor dn vn tmp2 volume tmp1))
	      )
	  (if (or (string-match "M$" conc) (string-match "mol$" dose))
	      (progn
		;; One of the units is in terms of moles, but the other is not.
		;; We need the molecular weight to calculate.
		(setq mw (esn-prompt "Molecular Weight (in g/mol aka daltons): "
				     "^[0-9.]"))
		(when (string-match "M$" conc)
		  (setq tmp1 (replace-match "g/L" nil nil conc))
		  (setq scale (esn-get-scale-factor dn vn dose volume tmp1 nil mw))
		  (setq conc (concat conc "; MW: " mw))
		  )
		(when (string-match "mol$" dose)
		  (setq tmp1 (replace-match "g" nil nil dose))
		  (setq scale (esn-get-scale-factor dn vn tmp1 volume conc mw))
		  (setq dose (concat dose "; MW: " mw))
		  )
		)
	    (setq scale (esn-get-scale-factor dn vn dose volume conc))
	    )
	  )
	(save-excursion
	  (setq has-eq (re-search-backward "=.*?\\=" nil t))
	  )
        (with-temp-buffer
          (unless has-eq
            (insert "="))
          (insert (nth 1 cmt))
          (insert wtv)
          (insert scale)
          (insert (concat "; Dose: " dose))
          (when bw
            (insert (concat "/" bw))
            )
          (insert (concat "; Volume: " volume))
          (when vbw
            (insert (concat "/" vbw))
            )
          (insert (concat "; Conc: " conc))
          (setq ret (buffer-substring (point-min) (point-max)))
          )
        (unless no-insert
          (insert ret)
          )
        (symbol-value 'ret)
	)
      )
    ))
  )

(defun esn-get-scale-factor (dn vn dose volume conc &optional times div)
  "This function returns the scale factor when the dose is given in units of ``dose'', volume has units of ``volume'' and the concentration measurments are in ``conc'' units.
Dose should be
mg or dg or some other prefix defined by esn-prefix-units and g.
volume should be mL or ml or some other prefix, with l/L.
Concentration shhould be ng/mL or some other prefix.
The divide sign can be any of the following characters ``\|/''.
"
  (let (
	(case-fold-search 't)
	(dp "")
	(vp "")
	(cv "")
	(cu "")
	(x 0)
	(y 0)
	(u 0)
	(v 0)
	(z 0)
	(scale1 "")
	(scale2 "")
	(scale "")
	)
    (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)g[ \t]*$" dose))
	(esn-error "Dose is not in the proper units should be in some multiple of g like mg or ng.")
      (setq dp (match-string 1 dose))
      (if (not (string= dp ""))
	  (if (not (assoc dp esn-prefix-units))
	      (esn-error "Dose unit unrecognized (%sg)." dp)
	    (setq x (- (cadr (assoc dp esn-prefix-units))))
	    )
	)
      )
    (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)?[lL][ \t]*$" volume))
	(esn-error "Volume should be in terms of l like ml or nl.")
      (setq vp (match-string 1 volume))
      (if (not (string= vp ""))
	  (if (not (assoc vp esn-prefix-units))
	      (esn-error "Volume unit unrecognized (%sg)." vp)
	    (setq y (- (cadr (assoc vp esn-prefix-units)))))
	)
      )
    (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)g[ \t]*[\\|/][ \t]*\\([A-Za-z]\\{0,2\\}\\)?[lL][ \t]*$" conc))
	(esn-error "Concentration should be in terms of g/l like ng/L or  mg/mL.")
      (setq cv (match-string 1 conc))
      (setq cu (match-string 2 conc))
      (if (not (string= cv ""))
	  (if (not (assoc cv esn-prefix-units))
	      (esn-error "Concentration unit not recognized (%sg/%sl)" cv cu)
	    (setq v (- (cadr (assoc cv esn-prefix-units))))
	    )
	)
      (if (not (string= cu ""))
	  (if (not (assoc cu esn-prefix-units))
	      (esn-error "Concentration unit not recognized (%sg/%sl)" cv cu)
	    (setq u (- (cadr (assoc cu esn-prefix-units))))
	    )
	)
      )
    ;;
    ;; Ok. Now figure out the result.
    ;;
    (setq z (- (- x y) (- v u)))
    (when (and dn vn)
      (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)g[ \t]*$" dn))
	  (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)m\\(?:\\^\\|\\*\\*\\)2[ \t]*$" dn))
	      (esn-error "Dose is not in the proper units should be in some multiple of g like mg or ng, OR some multiple of m^2 like mm^2.")
	    (setq dn (match-string 1 dn))
	    (if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)m\\(?:\\^\\|\\*\\*\\)2[ \t]*$" vn))
		(esn-error "Volume normization is not in the proper units should be in some multiple of m^2 like mm^2.")
	      (setq vn (match-string 1 vn))
	      )
	    (if (not (assoc vn esn-prefix-units))
		(esn-error "Volume normalization unit not recognized %sm^2" vn)
	      (setq v (cadr (assoc vn esn-prefix-units))))
	    (if (not (assoc dn esn-prefix-units))
		(esn-error "Dose normalization unit not recognized %sm^2" dn)
	      (setq u (cadr (assoc dn esn-prefix-units)))
	      )
	    ; Now correct for squared units.
	    (setq u (* u 2))
	    (setq v (* v 2))
	    )
	(setq dn (match-string 1 dn))
	(if (not (string-match "^[ \t]*\\([A-Za-z]\\{0,2\\}\\)g[ \t]*$" vn))
	  (esn-error "Volume normization is not in the proper units should be in some multiple of g like mg or ng.")
	  (setq vn (match-string 1 vn))
	  (if (not (assoc vn esn-prefix-units))
	      (esn-error "Volume normalization unit not recognized %sg" vn)
	    (setq v (cadr (assoc vn esn-prefix-units))))
	  (if (not (assoc dn esn-prefix-units))
	      (esn-error "Dose normalization unit not recognized %sg" dn)
	    (setq u (cadr (assoc dn esn-prefix-units)))
	    )
	  )
	)
      (setq z (+ z (- v u)))
      )
    (if (>= z 0)
	(setq scale "*")
      (setq scale "/")
      (setq z (- z))
      )
    (when (and times (not div))
      (if (string= "*" scale)
	  (progn
	    (setq scale1 (concat scale times (make-string z ?0)))
	    (setq scale2 (concat scale "(" times "*10**" (number-to-string z) ")"))
	    )
	(setq scale1 (concat "*" times scale "1" (make-string z ?0)))
	(setq scale2 (concat "*" times scale "(10**" (number-to-string z) ")"))
	)
      )
    (when (and div (not times))
      (if (string= "*" scale)
	  (progn
	    (setq scale1 (concat scale "1" (make-string z ?0) "/" div))
	    (setq scale2 (concat scale "10**" (number-to-string z) ")/" div))
	    )
	(setq scale1 (concat scale "(" div (make-string z ?0) ")"))
	(setq scale2 (concat scale "(" div "*10**" (number-to-string z ) ")"))
	)
      )
    (unless (or times div)
      (setq scale1 (concat scale "1" (make-string z ?0)))
      (setq scale2 (concat scale "(10**" (number-to-string z) ")"))
      )
    (if (>= 3 z)
	(setq scale scale1)
      (setq scale scale2)
      )
    (symbol-value 'scale)
    )
  )


(provide 'esn-units)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-units.el ends here
