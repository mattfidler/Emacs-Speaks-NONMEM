;;; esn-bsv.el --- Between Subject Exploration Automation.
;;
;; Filename: esn-bsv.el
;; Description: Between Subject Exploration Automation.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Apr 20 12:14:48 2010 (-0500)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 296
;; URL: http://esnm.sourceforge.net
;; Keywords:  EsN, BSV, IIV
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Create BSV control streams from current control stream using form.
;;  Currently doesn't support Mu-referencing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 30-Aug-2010    Matthew L. Fidler  
;;    Remove Variables
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
(require 'esn-start)

(declare-function esn-update-rep-bef-after "esn-update")
(declare-function esn-update-get-purpose "esn-update")
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
;; BSV
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar esn-mode-bsv-do-add 't)
(defvar esn-mode-bsv-do-exp 't)
(defvar esn-mode-bsv-do-ccv 't)
(defvar esn-mode-bsv-add-ini "0.2")
(defvar esn-mode-bsv-exp-ini "0.2")
(defvar esn-mode-bsv-ccv-ini "0.2")
(defvar esn-mode-bsv-cb nil)
(defvar esn-mode-bsv-fn nil)
(defvar esn-mode-bsv-tvs nil)
(defvar esn-mode-bsv-mu-x nil)
(defvar esn-mode-bsv-cross nil)
(defvar esn-mode-bsv-xmind-current nil)
(defvar esn-mode-bsv-xmind-txt nil)
(defvar esn-mode-bsv-current-buffer-name nil)

(require 'widget)

(eval-when-compile

  (require 'widget)
  (require 'wid-edit))

(defvar esn-bsv-xmind "<topic branch=\"folded\" id=\"IDev\"><title>Exploring Variability</title><children><topics type=\"attached\"><topic id=\"IDevs\"><title>Single</title><children><topics type=\"attached\"><topic id=\"IDevsa\"><title>Additive</title></topic><topic id=\"IDevsc\"><title>CCV</title></topic><topic id=\"IDevse\"><title>Exponential</title></topic></topics></children></topic><topic id=\"IDevm\"><title>Multiple</title><children><topics type=\"attached\"><topic id=\"IDevmac\"><title>Additive + CCV</title></topic><topic id=\"IDevmae\"><title>Additive + Exponential</title></topic><topic id=\"IDevmce\"><title>CCV + Exponential</title></topic><topic id=\"IDevmace\"><title>Additive + CCV + Exponential</title></topic></topics></children></topic></topics></children></topic>"
  "* Xmind variability exploration"
  )


(defun esn-bsv-add-xmind-get-xmind (current &optional add ccv exp)
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
                (setq id "evmace")
                )
            (setq tit "Additive + CCV")
            (setq id "evmac")
            )
          )
        ( exp
          (setq tit  "Additive + Exponential")
          (setq id "evmae")
          )
        ('t
         (setq tit "Additive")
         (setq id "evsa")
         )
        )
       )
     ( ccv
       (cond
        ( exp
          (setq tit "CCV + Exponential")
          (setq id "evmce")
          )
        ( 't
          (setq tit "CCV")
          (setq id "evsc")
          )
        )
       )
     ( exp
       (setq tit "Exponential")
       (setq id "evse")
       )
     ( 't
       (esn-error "Need at least ONE type of error")
       )
     )
    ;; Now get Text:
    (save-window-excursion
      (set-buffer esn-mode-bsv-current-buffer-name)
      (setq ret (concat "; Xmind: " (esn-xmind-get-extended-parent-title (concat current id)) " > " tit))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-bsv-add-xmind-explore (&optional current xmind-file use-two prefix)
  "* Updates the mind map to include BSV exploration on the current file (if not already present)."
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
        (val esn-bsv-xmind)
        )
    (if (string-match (format "\\<id=\"%sevmace\"" current) content)
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
(defun esn-bsv-form-submit (&rest ignore)
  "* Starts creating control streams."
  (when (yes-or-no-p "Are you sure you want to create these control streams? ")
    (esn-message "Starting Automatic generation of Between Subject Variability control streams")
    (when (esn-use-xmind-p)
      (condition-case nil
          (save-window-excursion
            (set-buffer esn-mode-bsv-current-buffer-name)
            (esn-bsv-add-xmind-explore esn-mode-bsv-xmind-current)
            )
        (error (esn-error "Problem Adding Xmind variability exploration tree in mind map")
               )
        )
      )
    (condition-case nil
        (esn-mode-bsv-file esn-mode-bsv-cb esn-mode-bsv-tvs esn-mode-bsv-fn)
      (error
       (esn-error "Could not complete automatic generation of Between Subject Variability control streams")
       ))
    (kill-buffer "*EsN: Exploring Between Subject Variability*")
    (esn-message "Completed Automatic generation of Between Subject Variability control streams")
    )
  )
(defun esn-bsv-form-toggle (widget &rest ignore)
  "* Toggles for BSV form."
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
             esn-mode-bsv-tvs
             )
       (when value
         (add-to-list 'tlst label)
         )
       (setq esn-mode-bsv-tvs tlst)
       (message "%s" esn-mode-bsv-tvs)
       )
     ( (string= type "bsv")
       (cond
        ( (string= label "add")
          (setq esn-mode-bsv-do-add value)
          )
        ( (string= label "exp")
          (setq esn-mode-bsv-do-exp value)
          )
        ( (string= label "ccv")
          (setq esn-mode-bsv-do-ccv value)
          )
        )
       )
     ( (string= type "ini")
       (cond
        ( (string= label "add")
          (setq esn-mode-bsv-add-ini value)
          )
        ( (string= label "exp")
          (setq esn-mode-bsv-exp-ini value)
          )
        ( (string= label "ccv")
          (setq esn-mode-bsv-ccv-ini value)
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
(defun esn-bsv-form (vars)
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
                         :notify 'esn-bsv-form-toggle
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
                 :type "bsv"
                 :notify 'esn-bsv-form-toggle
                 esn-mode-bsv-do-exp
                 )
  (widget-insert " Exponential/Log-normal, i.e. CL=TVCL*DEXP(ETA(1))\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :label "exp"
                 :type "ini"
                 :size 13
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bsv-exp-ini
                 )
  (widget-insert "\n  ")
  
  (widget-create 'checkbox
                 :label "add"
                 :type "bsv"
                 :notify 'esn-bsv-form-toggle
                 esn-mode-bsv-do-add
                 )
  (widget-insert " Additive, i.e. CL=TVCL+ETA(1)\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :size 13
                 :label "add"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bsv-add-ini
                 )
  (widget-insert "\n  ")
  (widget-create 'checkbox
                 :label "ccv"
                 :type "bsv"
                 :notify 'esn-bsv-form-toggle
                 esn-mode-bsv-do-ccv
                 t
                 )
  (widget-insert " CCV/Proprotional, i.e. CL=TVCL*(1+ETA(1))\n\t Initial estimate: ")
  (widget-create 'editable-field
                 :size 13
                 :label "ccv"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-bsv-ccv-ini
                 )
  (if 't
      (widget-insert "\n\n ")
    (widget-insert "\n\nOptions:\n  ")
    (widget-create 'checkbox
                   :label "mu_x"
                   :type "opt"
                   :notify 'esn-bsv-form-toggle
                   t
                   )
    (widget-insert " Use Mu-references, ")
    (widget-create 'checkbox
                   :label "cross"
                   :type "opt"
                   :notify 'esn-bsv-form-toggle
                   t
                   )
    
    (widget-insert " Allow different types of variability (i.e., EXP & CCV) \n\n ")
    )
  (widget-create 'push-button
                 :notify 'esn-bsv-form-submit
                 "Create Models")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )

(defun esn-mode-gen-bsv-files ()
  "Form to generate all combinations of Between subject variability
based on the control stream opened."
  (interactive)
  (setq esn-mode-bsv-current-buffer-name (current-buffer))
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
      (setq esn-mode-bsv-xmind-current (esn-xmind-current-topic))
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
                (esn-reg-record-exp "OME")) nil t)
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
    (setq esn-mode-bsv-cb cb)
    (setq esn-mode-bsv-fn fn)
    (setq esn-mode-bsv-tvs tvs)
    (esn-bsv-form tvs)
    
    ;;
    )
  )

(defun esn-mode-bsv-file (abuf avars fn)
  "Recursive function to generate all possible combinations of BSV variables."
  (let (
        (vars (cdr avars))
        (var (nth 0 avars))
        cbuf
        )
    (when esn-mode-bsv-do-add
      (setq cbuf (esn-mode-bsv-mod var abuf "add" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bsv-file cbuf vars fn))
      )
    (when esn-mode-bsv-do-ccv
      (setq cbuf (esn-mode-bsv-mod var abuf "ccv" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bsv-file cbuf vars fn))
      )
    (when esn-mode-bsv-do-exp
      (setq cbuf (esn-mode-bsv-mod var abuf "exp" fn))
      (unless (or (not cbuf) (= 0 (length vars)))
        (esn-mode-bsv-file cbuf vars fn))
      )
    (unless (= 0 (length vars))
      (esn-mode-bsv-file abuf vars fn))
    )
  )
(defun esn-mode-bsv-file-internal (var abuf avars fn)
  "Internal Function to generate all variables"
  )

(defun esn-mode-bsv-mod (avar abuf aerr fn)
  (let (
        (var avar)
        (err-fmt (cond
                  ((string= "add" aerr)
                   "+%s")
                  ((string= "ccv" aerr)
                   "*(1+%s)")
                  ((string= "exp" aerr)
                   "*DEXP(%s)")))
        (bsv-lab (concat avar "_" aerr))
        (ini (cond
              ((string= "add" aerr)
               esn-mode-bsv-add-ini)
              ((string= "ccv" aerr)
               esn-mode-bsv-ccv-ini)
              ((string= "exp" aerr)
               esn-mode-bsv-exp-ini)))
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
          (insert (format "\n       %s ; %s" ini bsv-lab))
          (widen)
          (esn-update-rep-bef-after
           (esn-update-purpose-before)
           (esn-update-purpose-after)
           (concat
            (esn-mode-bsv-fn fn 't)
            (format (esn-update-input-files-line) (esn-update-get-purpose 't))
            (format (esn-update-input-files-line) txt))
           )
          (setq nfn (esn-mode-bsv-fn fn))
          (when (esn-use-xmind-p)
            (goto-char (point-min))
            (when (re-search-forward ";[ \t]*Xmind:.*" nil t)
              (replace-match esn-mode-bsv-xmind-txt))
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
(defun esn-mode-bsv-fn (fn &optional real-text)
  "Gets the file name for the current buffer based on the previous file fn."
  (let (
        (bsv '())
        (case-fold-search 't)
        (ret nil)
        add ccv exp
        )
    (esn-fix-numbering)
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\+ETA([0-9]+)" nil t)
      (if real-text
          (add-to-list 'bsv (concat "Add "(upcase (match-string 1))))
        (add-to-list 'bsv (concat (upcase (match-string 1)) "a"))
        )
      (setq add 't)
      )
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\*DEXP(ETA([0-9]+))" nil t)
      (if real-text
          (add-to-list 'bsv (concat "Exp "(upcase (match-string 1))))
        (add-to-list 'bsv (concat (upcase (match-string 1)) "e"))
        )
      (setq exp 't)
      )
    (goto-char (point-min))
    (while (re-search-forward "^[  \t]*\\([A-Z][A-Z0-9]*\\)[  \t]*=[  \t]*TV\\1\\*(1\\+ETA([0-9]+))" nil t)
      (if real-text
          (add-to-list 'bsv (concat "CCV " (upcase (match-string 1))))
        (add-to-list 'bsv (concat (upcase (match-string 1)) "c"))
        )
      (setq ccv 't)
      )
    (unless (or (not bsv) (= 0 (length bsv)))
      (if real-text
          (setq ret (mapconcat (lambda(x) x) (sort bsv 'string-lessp) ", "))
        (setq bsv (concat "bsv_" (mapconcat (lambda(x) x) (sort bsv 'string-lessp) "_")))
        (if (string-match "bsv[A-Za-z0-9_]*" fn)
            (setq ret (replace-match bsv 't 't fn))
          (when (string-match (format "\\([0-9]*%s\\)$" (regexp-opt esn-default-extension)) fn)
            (setq ret (replace-match (format "-%s-\\1" bsv) 't nil fn))
            (while (string-match "--+" ret)
              (setq ret (replace-match "-" 't nil ret)))
            (while (string-match "-\\." ret)
              (setq ret (replace-match "." 't nil ret)))
            )
          )
        )
      )
    (unless real-text
      (when esn-mode-bsv-xmind-current
        (setq esn-mode-bsv-xmind-txt (esn-bsv-add-xmind-get-xmind esn-mode-bsv-xmind-current add ccv exp))
        )
      )
    (symbol-value 'ret)
    )
  )
(provide 'esn-bsv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-bsv.el ends here
