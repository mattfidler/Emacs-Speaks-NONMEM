;;; esn-bov.el --- Between Subject Exploration Automation.
;;
;; Filename: esn-bov.el
;; Description: Between Subject Exploration Automation.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Apr 20 12:14:48 2010 (-0500)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 305
;; URL: http://esnm.sourceforge.net
;; Keywords:  EsN, BOV, IIV
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Create BOV control streams from current control stream using form.
;;  Currently doesn't support Mu-referencing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 30-Aug-2010    Matthew L. Fidler  
;;    Removed Variables 
;; 27-Aug-2010    Matthew L. Fidler  
;;    Fixed warnings.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize record regular expressions.
;; 22-Apr-2010    Matthew L. Fidler
;;    Addded Xmind Support.
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

(declare-function esn-update-rep-bef-after "esn-update")
(declare-function esn-update-get-purpose "esn-update")
(declare-function esn-mode "esn-mode")
(declare-function esn-error "esn-exec")
(declare-function esn-xmind-get-extended-parent-title "esn-xmind")
(declare-function esn-xmind-start-map "esn-xmind")
(declare-function esn-xmind-get-content "esn-xmind")
(declare-function esn-xmind-get-manifest "esn-xmind")
(declare-function esn-xmind-current-topic "esn-xmind")
(declare-function esn-xmind-default-prefix "esn-xmind")
(declare-function esn-xmind-update-add-child "esn-xmind")
(declare-function esn-xmind-update-xmind-part "esn-xmind")
(declare-function esn-xmind-del-tmp-dir "esn-xmind")
(declare-function esn-xmind-update-xmind-file "esn-xmind")
(declare-function esn-message "esn-exec")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-fix-numbering "esn-extended")
(declare-function esn-max-what "esn-properties")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOV
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esn-start)
(defvar esn-mode-bov-do-add 't)
(defvar esn-mode-bov-do-exp 't)
(defvar esn-mode-bov-do-ccv 't)
(defvar esn-mode-bov-add-ini "0.2")
(defvar esn-mode-bov-exp-ini "0.2")
(defvar esn-mode-bov-ccv-ini "0.2")
(defvar esn-mode-bov-cb nil)
(defvar esn-mode-bov-fn nil)
(defvar esn-mode-bov-tvs nil)
(defvar esn-mode-bov-mu-x nil)
(defvar esn-mode-bov-cross nil)
(defvar esn-mode-bov-xmind-current nil)
(defvar esn-mode-bov-xmind-txt nil)
(defvar esn-mode-bov-current-buffer-name nil)



(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar esn-bov-xmind "<topic branch=\"folded\" id=\"IDoev\"><title>Exploring Occasion Variability</title><children><topics type=\"attached\"><topic id=\"IDoevs\"><title>Single</title><children><topics type=\"attached\"><topic id=\"IDoevsa\"><title>Additive</title></topic><topic id=\"IDoevsc\"><title>CCV</title></topic><topic id=\"IDoevse\"><title>Exponential</title></topic></topics></children></topic><topic id=\"IDoevm\"><title>Multiple</title><children><topics type=\"attached\"><topic id=\"IDoevmac\"><title>Additive + CCV</title></topic><topic id=\"IDoevmae\"><title>Additive + Exponential</title></topic><topic id=\"IDoevmce\"><title>CCV + Exponential</title></topic><topic id=\"IDoevmace\"><title>Additive + CCV + Exponential</title></topic></topics></children></topic></topics></children></topic>"
  "* Xmind variability exploration"
  )


(defun esn-bov-add-xmind-get-xmind (current &optional add ccv exp)
  "* Gets the Xmind string for the current file"
  (let (
        tit
        id
        ret
        )
    (cond
     ( add
       (cond
        ( ccv
          (if exp
              (progn
                (setq tit "Additive + CCV + Exponential")
                (setq id "oevmace")
                )
            (setq tit "Additive + CCV")
            (setq id "oevmac")
            )
          )
        ( exp
          (setq tit  "Additive + Exponential")
          (setq id "oevmae")
          )
        ('t
         (setq tit "Additive")
         (setq id "oevsa")
         )
        )
       )
     ( ccv
       (cond
        ( exp
          (setq tit "CCV + Exponential")
          (setq id "oevmce")
          )
        ( 't
          (setq tit "CCV")
          (setq id "oevsc")
          )
        )
       )
     ( exp
       (setq tit "Exponential")
       (setq id "oevse")
       )
     ( 't
       (esn-error "Need at least ONE type of error")
       )
     )
    ;; Now get Text:
    (save-window-excursion
      (set-buffer esn-mode-bov-current-buffer-name)
      (setq ret (concat "; Xmind: " (esn-xmind-get-extended-parent-title (concat current id)) " > " tit))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-bov-add-xmind-explore (&optional current xmind-file use-two prefix)
  "* Updates the mind map to include BOV exploration on the current file (if not already present)."
  (interactive)
  (unless xmind-file
    (esn-xmind-start-map)
    )
  (let (
        (case-fold-search 't)
        (content (esn-xmind-get-content xmind-file))
        (manifest (esn-xmind-get-manifest xmind-file))
        (current (or current (esn-xmind-current-topic xmind-file use-two)))
        (pre (or prefix (esn-xmind-default-prefix)))
        (val esn-bov-xmind)
        )
    (if (string-match (format "\\<id=\"%soevmace\"" current) content)
        (message "Already has variability exploration in the mind-map.")
      (if (not current)
          (esn-error "Cannot proceed; Current control stream not in project.")
        (while (string-match "id=\"ID" val)
          (setq val (replace-match (format "id=\"%s" current) nil nil val)))
        (setq content (esn-xmind-update-add-child current val content))
        (esn-xmind-update-xmind-part content "content.xml" xmind-file)
        ;; Delete Thumbnail (since we can't update it).
        (while (string-match "<file-entry[^\n>]*?Thumbnail[^\n>]*?>"
                             manifest)
          (setq manifest (replace-match "" nil nil manifest))
          )
        (while (string-match "\n" manifest)
          (setq manifest (replace-match " " nil nil manifest))
          )
        (while (string-match "  +" manifest)
          (setq manifest (replace-match " " nil nil manifest)))
        (when (file-exists-p (format "%s/Thumbnails" esn-xmind-checkout-dir))
          (esn-xmind-del-tmp-dir (format "%s/Thumbnails" esn-xmind-checkout-dir))
          )
        (esn-xmind-update-xmind-part manifest "META-INF/manifest.xml" xmind-file)
        (esn-xmind-update-xmind-file)
        )
      )
    )
  )
(defun esn-bov-form-submit (&rest ignore)
  "* Starts creating control streams."
  (when (yes-or-no-p "Are you sure you want to create these control streams? ")
    (esn-message "Starting Automatic generation of Between Occasion Variability control streams")
    (when (esn-use-xmind-p)
      (condition-case nil
          (save-window-excursion
            (set-buffer esn-mode-bov-current-buffer-name)
            (esn-bov-add-xmind-explore esn-mode-bov-xmind-current)
            )
        (error (esn-error "Problem Adding Xmind variability exploration tree in mind map")
               )
        )
      )
    (condition-case nil
        (esn-mode-bov-file esn-mode-bov-cb esn-mode-bov-tvs esn-mode-bov-fn)
      (error
       (esn-error "Could not complete automatic generation of Between Occasion Variability control streams")
       ))
    (kill-buffer "*EsN: Exploring Between Occasion Variability*")
    (esn-message "Completed Automatic generation of Between Occasion Variability control streams")
    )
  )
(defun esn-bov-form-toggle (widget &rest ignore)
  "* Toggles for BOV form."
  (let (
        (type (widget-get widget ':type))
        (label (widget-get widget ':label))
        (value (widget-value widget))
        (tlst '())
        )
    (cond
     ( (string= type "var")
       (mapc (lambda(x)
               (unless (string= label x)
                 (add-to-list 'tlst x)))
             esn-mode-bov-tvs
             )
       (when value
         (add-to-list 'tlst label)
         )
       (setq esn-mode-bov-tvs tlst)
       (message "%s" esn-mode-bov-tvs)
       )
     ( (string= type "bov")
       (cond
        ( (string= label "add")
          (setq esn-mode-bov-do-add value)
          )
        ( (string= label "exp")
          (setq esn-mode-bov-do-exp value)
          )
        ( (string= label "ccv")
          (setq esn-mode-bov-do-ccv value)
          )
        )
       )
     ( (string= type "ini")
       (cond
        ( (string= label "add")
          (setq esn-mode-bov-add-ini value)
          )
        ( (string= label "exp")
          (setq esn-mode-bov-exp-ini value)
          )
        ( (string= label "ccv")
          (setq esn-mode-bov-ccv-ini value)
          )
        )
       )
     ( (string= type "opt")
       (cond
        ( (string= label "add")
          )
        )
       )
     )
    )
  )
(defun esn-bov-form (vars)
  "Create the widgets from the Widget manual."
  (switch-to-buffer "*EsN: Exploring Between Subject Variability*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))
  (widget-insert
   "This is an automation tool to create control streams exploring between subject variability.

It requires the parameters that you are exploring to be in the form:

TVCL = THETA(x)
CL = TVCL

OR

CL = THETA(x)

Variables:")
  (mapc (lambda(x)
          (widget-insert "\n  ")
          (widget-create 'checkbox
                         :label x
                         :type "var"
                         :notify 'esn-bov-form-toggle
                         t
                         )
          (widget-insert " ")
          (widget-insert x)
          )
        vars
        )
  (widget-insert "

Between Subject Variabilities:\n  ")
  (widget-create 'checkbox
                 :label "exp"
                 :type "bov"
                 :notify 'esn-bov-form-toggle
                 esn-mode-bov-do-exp
                 )
  (widget-insert " Exponential/Log-normal, i.e. CL=TVCL*DEXP(ETA(1))\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :label "exp"
                 :type "ini"
                 :size 13
                 :notify 'esn-bov-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bov-exp-ini
                 )
  (widget-insert "\n  ")
  
  (widget-create 'checkbox
                 :label "add"
                 :type "bov"
                 :notify 'esn-bov-form-toggle
                 esn-mode-bov-do-add
                 )
  (widget-insert " Additive, i.e. CL=TVCL+ETA(1)\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :size 13
                 :label "add"
                 :type "ini"
                 :notify 'esn-bov-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bov-add-ini
                 )
  (widget-insert "\n  ")
  (widget-create 'checkbox
                 :label "ccv"
                 :type "bov"
                 :notify 'esn-bov-form-toggle
                 esn-mode-bov-do-ccv
                 t
                 )
  (widget-insert " CCV/Proprotional, i.e. CL=TVCL*(1+ETA(1))\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :size 13
                 :label "ccv"
                 :type "ini"
                 :notify 'esn-bov-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bov-ccv-ini
                 )
  (if 't
      (widget-insert "\n\n ")
    (widget-insert "\n\nOptions:\n  ")
    (widget-create 'checkbox
                   :label "mu_x"
                   :type "opt"
                   :notify 'esn-bov-form-toggle
                   t
                   )
    (widget-insert " Use Mu-references, ")
    (widget-create 'checkbox
                   :label "cross"
                   :type "opt"
                   :notify 'esn-bov-form-toggle
                   t
                   )
    
    (widget-insert " Allow different types of variability (i.e., EXP & CCV) \n\n ")
    )
  (widget-create 'push-button
                 :notify 'esn-bov-form-submit
                 "Create Models")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )

(defun esn-mode-gen-bov-files ()
  "Form to generate all combinations of Between subject variability
based on the control stream opened."
  (interactive)
  (setq esn-mode-bov-current-buffer-name (current-buffer))
  (let (
        (cb (buffer-substring (point-min) (point-max)))
        (tvs '())
        (first 't)
        (case-fold-search 't)
        (fn (buffer-file-name))
        (max-lisp-eval-depth 10000)
        tmp
        no-tv
        )
    (when (esn-use-xmind-p)
      (setq esn-mode-bov-xmind-current (esn-xmind-current-topic))
      )
    (with-temp-buffer
      (insert cb)
      (goto-char (point-min))
      (while (re-search-forward "^\\([  \t]*\\)\\([A-Z][A-Z0-9]*\\)\\([  \t]*=[  \t]*\\)\\<TV\\2\\>.*$" nil t)
        (add-to-list 'tvs (match-string 2))
        (replace-match "\\1\\2\\3TV\\2")
        )
      (goto-char (point-min))
      (while (re-search-forward "^\\([  \t]*\\)\\([A-Z][A-Z0-9]*\\)\\([  \t]*=[  \t]*\\)\\<THETA(\\([0-9]+\\))[ \t]*$" nil t)
        (let (
              (cr (save-match-data (esn-get-current-record)))
              )
          (when (not (string= "ERR" cr)
                     )
            (setq tmp (match-string 2))
            (save-match-data
              (when (string-match "^TV" tmp)
                (setq tmp (replace-match "" nil nil tmp))
                (setq no-tv 't)
                )
              )
            (add-to-list 'tvs tmp)
            (if no-tv
                (replace-match "\\1\\2\\3THETA(\\4)")
              (replace-match "\\1TV\\2\\3THETA(\\4)\n\\1\\2\\3TV\\2")
              )
            (setq no-tv nil)
            )
          )
        )
      (goto-char (point-min))
      (while (re-search-forward 
              (eval-when-compile
                (esn-reg-record-exp "OME")
                ) 
              nil t)
        (end-of-line)
        (esn-narrow-rec)
        (delete-region (point-min) (point-max))
        (when first
          (insert "$OMEGA")
          (setq first nil)
          )
        (widen)
        )
      (esn-fix-numbering)
      (goto-char (point-min))
      (setq cb (buffer-substring (point-min) (point-max)))
      )
    (setq esn-mode-bov-cb cb)
    (setq esn-mode-bov-fn fn)
    (setq esn-mode-bov-tvs tvs)
    (esn-bov-form tvs)
    
    ;;
    )
  )

(defun esn-mode-bov-file (abuf avars fn)
  "Recursive function to generate all possible combinations of BOV variables."
  (let (
        (vars (cdr avars))
        (var (nth 0 avars))
        cbuf
        )
    (when esn-mode-bov-do-add
      (setq cbuf (esn-mode-bov-mod var abuf "add" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bov-file cbuf vars fn))
      )
    (when esn-mode-bov-do-ccv
      (setq cbuf (esn-mode-bov-mod var abuf "ccv" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bov-file cbuf vars fn))
      )
    (when esn-mode-bov-do-exp
      (setq cbuf (esn-mode-bov-mod var abuf "exp" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bov-file cbuf vars fn))
      )
    (unless (= 0 (length vars))
      (esn-mode-bov-file abuf vars fn))
    )
  )
(defun esn-mode-bov-file-internal (var abuf avars fn)
  "Internal Function to generate all variables"
  )

(defun esn-mode-bov-mod (avar abuf aerr fn)
  (let (
        (var avar)
        (err-fmt (cond
                  ((string= "add" aerr)
                   "+%s")
                  ((string= "ccv" aerr)
                   "*(1+%s)")
                  ((string= "exp" aerr)
                   "*DEXP(%s)")))
        (bov-lab (concat avar "_" aerr))
        (ini (cond
              ((string= "add" aerr)
               esn-mode-bov-add-ini)
              ((string= "ccv" aerr)
               esn-mode-bov-ccv-ini)
              ((string= "exp" aerr)
               esn-mode-bov-exp-ini)))
        (txt (cond
              ((string= "add" aerr)
               (format "Additive Between Subject Variability for %s" (upcase avar)))
              ((string= "ccv" aerr)
               (format "Proprotional Between Subject Variability for %s" (upcase avar)))
              ((string= "exp" aerr)
               (format "Lognormal Between Subject Variability for %s" (upcase avar)))))
        (ret nil)
        (nfn nil)
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (esn-skip-purpose 't)
        )
    (with-temp-buffer
      (insert abuf)
      (goto-char (point-min))
      (when (re-search-forward (format "^[  \t]*%s[  \t]*=[  \t]*TV%s" var var) nil t)
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (insert (format err-fmt (format "ETA(%s)" (+ 1 (esn-max-what "ETA")))))
        (goto-char (point-min))
        (when (re-search-forward "$OME" nil t)
          (end-of-line)
          (esn-narrow-rec)
          (goto-char (point-max))
          (insert (format "\n       %s ; %s" ini bov-lab))
          (widen)
          (esn-update-rep-bef-after
           (esn-update-purpose-before)
           (esn-update-purpose-after)
           (concat
            (esn-mode-bov-fn fn 't)
            (format (esn-update-input-files-line) (esn-update-get-purpose 't))
            (format (esn-update-input-files-line) txt))
           )
          (setq nfn (esn-mode-bov-fn fn))
          (when (esn-use-xmind-p)
            (goto-char (point-min))
            (when (re-search-forward ";[ \t]*Xmind:.*" nil t)
              (replace-match esn-mode-bov-xmind-txt))
            )
          (setq ret (buffer-substring (point-min) (point-max)))
          )
        )
      )
    (when (and ret nfn)
      (unless (and nfn (file-exists-p nfn))
        (with-temp-buffer
          (insert ret)
          (esn-mode)
          (write-file nfn)
          (esn-message "Wrote File %s" nfn)
          )
        )
      )
    (symbol-value 'ret)
    )
  )
(defun esn-mode-bov-fn (fn &optional real-text)
  "Gets the file name for the current buffer based on the previous file fn."
  (let (
        (bov '())
        (case-fold-search 't)
        (ret nil)
        add ccv exp
        )
    (esn-fix-numbering)
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\+ETA([0-9]+)" nil t)
      (if real-text
          (add-to-list 'bov (concat "Add "(upcase (match-string 1))))
        (add-to-list 'bov (concat (upcase (match-string 1)) "a"))
        )
      (setq add 't)
      )
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\*DEXP(ETA([0-9]+))" nil t)
      (if real-text
          (add-to-list 'bov (concat "Exp "(upcase (match-string 1))))
        (add-to-list 'bov (concat (upcase (match-string 1)) "e"))
        )
      (setq exp 't)
      )
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\*(1\\+ETA([0-9]+))" nil t)
      (if real-text
          (add-to-list 'bov (concat "CCV " (upcase (match-string 1))))
        (add-to-list 'bov (concat (upcase (match-string 1)) "c"))
        )
      (setq ccv 't)
      )
    (unless (or (not bov) (= 0 (length bov)))
      (if real-text
          (setq ret (mapconcat (lambda(x) x) (sort bov 'string-lessp) ", "))
        (setq bov (concat "bov_" (mapconcat (lambda(x) x) (sort bov 'string-lessp) "_")))
        (if (string-match "bov[A-Za-z0-9_]*" fn)
            (setq ret (replace-match bov 't 't fn))
          (when (string-match (format "\\([0-9]*%s\\)$" (regexp-opt esn-default-extension)) fn)
            (setq ret (replace-match (format "-%s-\\1" bov) 't nil fn))
            (while (string-match "--+" ret)
              (setq ret (replace-match "-" 't nil ret)))
            (while (string-match "-\\." ret)
              (setq ret (replace-match "." 't nil ret)))
            )
          )
        )
      )
    (unless real-text
      (when esn-mode-bov-xmind-current
        (setq esn-mode-bov-xmind-txt (esn-bov-add-xmind-get-xmind esn-mode-bov-xmind-current add ccv exp))
        )
      )
    (symbol-value 'ret)
    )
  )
(provide 'esn-bov)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-bov.el ends here
