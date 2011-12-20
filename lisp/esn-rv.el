;;; esn-rv.el --- Random Error
;;
;; Filename: esn-rv.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Apr 22 12:16:59 2010 (-0500)
;; Version:
;; Last-Updated: Mon May  2 13:12:25 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 228
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Random error exploration functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 30-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 30 10:54:06 2010 (-0500) #221 (Matthew L. Fidler)
;;    Added other options from other files.
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


(declare-function esn-message "esn-exec")
(declare-function esn-update-rep-bef-after "esn-update")
(declare-function esn-update-get-purpose "esn-update")
(declare-function esn-narrow-rec "esn-narrow")
(require 'widget)
(defun esn-rv-form-submit (&rest ignore)
  "* Starts creating RV control streams"
  (when (yes-or-no-p "Are you sure you want to create these control streams? ")
    (esn-message "Starting Automatic generation of Random Variability control streams")
    (when esn-mode-rv-do-addprop
      (esn-rv-generate
       esn-mode-rv-add-prop
       esn-mode-rv-add-prop-tini
       (list esn-mode-rv-addprop-add-ini esn-mode-rv-addprop-prop-ini)
       "Additive + Proportional" "add_prop"
       )
      )
    (when esn-mode-rv-do-add
      (esn-rv-generate
       esn-mode-rv-add
       esn-mode-rv-add-tini
       (list esn-mode-rv-add-ini )
       "Additive" "add"
       )
      )
    (when esn-mode-rv-do-prop
      (esn-rv-generate
       esn-mode-rv-prop
       esn-mode-rv-prop-tini
       (list esn-mode-rv-prop-ini )
       "Proportional" "prop"
       )
      )
    (when esn-mode-rv-do-logn
      (esn-rv-generate
       esn-mode-rv-logn
       esn-mode-rv-logn-tini
       (list esn-mode-rv-logn-ini )
       "Log-normal" "logn"
       't
       )
      )
    (kill-buffer "*EsN: Exploring Residual Variability*")
    (esn-message "Completed Random Variability Exploration")
    )
  )
(defun esn-rv-get-file-name (file)
  "* Gets new file name"
  (let (
        (ret esn-mode-rv-fn)
        )
    (if (string-match (eval-when-compile (regexp-opt '(
                                                       "-add+prop-"
                                                       "-add-"
                                                       "-prop-"
                                                       "-log-"
                                                       "-logn-"
                                                       ) 't ) ) ret)
        (setq ret (replace-match (concat "-" file "-") 't 't ret))
      (when (string-match (format "\\([0-9]*%s\\)$" (regexp-opt esn-default-extension)) ret)
        (setq ret (replace-match (concat "-" file "-\\1") 't nil ret))
        )
      )
    (symbol-value 'ret)
    )
  )
(defun esn-rv-generate (w-format t-format inis text file &optional log)
  "* Generates a control stream based on format, ini and text and file-addition"
  (let (
        (case-fold-search 't)
        max-theta
        thetas
        i
        have-ini
        pt1
        pt2
        (fn (esn-rv-get-file-name file))
        (fn2 esn-mode-rv-fn)
        ndv
        )
    (when (string-match "[/\\\\]\\([^/\\\\]*\\)$" fn2)
      (setq fn2 (match-string 1 fn2))
      )
    (if (file-exists-p fn)
        (esn-message "Skipping %s, file already exists." fn)
    (save-window-excursion
      (set-buffer (find-file-noselect fn))
      (delete-region (point-min) (point-max))
      (insert esn-mode-rv-cb)
        (esn-update-rep-bef-after
         (esn-update-purpose-before)
         (esn-update-purpose-after)
         (concat
          text " (from " fn2 ")"
          (format (esn-update-input-files-line) (concat "Exploring Residual Error: " text))
          (format (esn-update-input-files-line) (esn-update-get-purpose 't))
          )
         )
      ;; Change DV to Log DV or vice versa as required.
      (when (or 
             (and esn-mode-rv-is-log (not log))
             (and esn-mode-rv-is-norm log)
             )
        (when (and esn-mode-rv-is-log (not log))
          (setq ndv (nth 0 esn-mode-rv-is-log))
          )
        (when (and esn-mode-rv-is-norm log)
          (setq ndv (nth 0 esn-mode-rv-is-norm))
          )
        (goto-char (point-min))
        (when (re-search-forward "\\<[$]INP[A-Z]*[ \t\n]*" nil t)
          (save-restriction
            (esn-narrow-rec)
            (goto-char (point-min))
            (when (re-search-forward "\\<DV\\>" nil t)
              (replace-match "XXXX")
              )
            (goto-char (point-min))
            (when (re-search-forward (format "\\<%s\\>" (regexp-quote ndv)) nil t)
              (replace-match "DV"))
            )
          )
        )
      (setq max-theta (esn-max-theta))
      (setq i max-theta)
      (setq thetas (mapcar (lambda(x)
                             (setq i (+ i 1))
                             (symbol-value 'i)
                             )
                           inis))
      (goto-char (point-min))
      (when (re-search-forward "\\<[$]ERR[A-Z]*[ \t\n]*" nil t)
        (save-restriction
          (esn-narrow-rec)
          (goto-char (point-min))
          (when (re-search-forward "^\\([ \t]*W[ \t]*=[ \t]*\\).*$" nil t)
            (replace-match (concat "\\1" (apply 'format w-format thetas)))
            )
          )
        )
      (goto-char (point-min))
      (while (re-search-forward "\\<[$]THE[A-Z]*[ \t\n]*" nil t)
        (setq have-ini 't)
        )
      (when have-ini
        (skip-chars-forward "\t \n")
        (save-restriction
          (esn-narrow-rec)
          (goto-char (point-max))
          (insert "\n")
          (setq pt1 (point))
          (insert (apply 'format t-format inis))
          (setq pt2 (point))
          (insert "\n")
          (goto-char pt1)
          (while (search-forward ";" pt2 t)
            (replace-match "")
            (esn-magic-semi)
            )
          )
        )
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (point))
      (save-buffer)
      (kill-buffer (current-buffer))
      (when (string-match "[/\\\\]\\([^/\\\\]*\\)$" fn2)
        (setq fn2 (match-string 1 fn2))
        )
      (esn-message "Created %s from %s" fn fn2)
      )
    )
    )
  )
(defun esn-rv-form-toggle (widget &rest ignore)
  "* Toggles for BSV form."
  (let (
        (type (widget-get widget ':type))
        (label (widget-get widget ':label))
        (value (widget-value widget))
        (tlst '())
        )
    (cond
     ( (string= type "rv")
       (cond
        ( (string= label "add")
          (setq esn-mode-rv-do-add value)
          )
        ( (string= label "prop")
          (setq esn-mode-rv-do-prop value)
          )
        ( (string= label "addprop")
          (setq esn-mode-rv-do-addprop value)
          )
        ( (string= label "logn")
          (setq esn-mode-rv-do-logn value)
          )
        )
       )
     ( (string= type "ini")
       (cond
        ( (string= label "add")
          (setq esn-mode-rv-add-ini value)
          )
        ( (string= label "prop")
          (setq esn-mode-rv-prop-ini value)
          )
        ( (string= label "addprop-add")
          (setq esn-mode-rv-addprop-add-ini value)
          )
        ( (string= label "addprop-prop")
          (setq esn-mode-rv-addprop-prop-ini value)
          )
        ( (string= label "logn")
          (setq esn-mode-rv-logn-ini value)
          )
       )
     )
    )
  )
  )
(defun esn-rv-get-cb ()
  "* Reformats buffer for use in RV exploration"
  (let (
        (cb (buffer-substring-no-properties (point-min) (point-max)))
        (case-fold-search 't)
        (thetas '())
        (est  "\\(?:([^)]*)\\|[0-9.]+\\)\\([ \t]?FIX\\(?:ED?\\)?\\)?")
        (com "\\= *;[;Cc]* *.*")
        tmp
        (ntheta 0)

        )
    (setq esn-mode-rv-fn (buffer-file-name))
    (setq esn-mode-rv-is-log (esn-mode-is-log 't))
    (setq esn-mode-rv-is-norm (esn-mode-is-log 't 't))
    (with-temp-buffer
      (insert cb)
      (goto-char (point-min))
      (when (re-search-forward "\\<[$]ERR[A-Z]*[ \t\n]+" nil t)
        (save-restriction
          (esn-narrow-rec)
          (goto-char (point-min))
          (when (re-search-forward "^\\([ \t]*\\<W[ \t]*=[ \t]*\\)\\(.*\\)" nil t)
            (setq tmp (match-string 2))
            (replace-match "\\1")
            (while (string-match "\\<THETA(\\([0-9]+\\))" tmp)
              (add-to-list 'thetas (string-to-number (match-string 1 tmp)))
              (setq tmp (replace-match "" nil nil tmp)))
            )
          )
        )
      (setq thetas  (sort thetas '<))
      ;; Now Remove the initial estimates of THETA(x)s.
      (goto-char (point-min))
      (while (re-search-forward "\\<[$]THE[A-Z]*[ \t\n]" nil t)
        (save-restriction
          (esn-narrow-rec)
          (goto-char (point-min))
          (while (re-search-forward est nil t)
            (if (esn-in-comment-p)
                (end-of-line)
              (setq ntheta (+ ntheta 1))
              (when (memq ntheta thetas)
                (replace-match "")
                (when (looking-at com)
                  (replace-match "")
                  )
                (skip-chars-backward " \t")
                (when (looking-at "[ \t]*")
                  (replace-match "")
                  )
                (skip-chars-backward "\n")
                (when (looking-at "\n+\\([ \t]*\\)")
                  (replace-match "\n\\1"))
                )
              )
            )
          )
        )
      (esn-renumber-theta)
      (setq esn-mode-rv-cb (buffer-substring (point-min) (point-max)))
      )
    )
  )
(defun esn-rv-form ()
  "Create the widgets from the Widget manual."
  (interactive)
  (esn-rv-get-cb)
  (switch-to-buffer "*EsN: Exploring Residual Variability*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))
  (widget-insert
"This is an automation tool to create control streams exploring residual variability.

It requires the changes to errors to be in the form of:

 $ERROR
  IPRED = F
  W     = .............
  DEL   = 0
  IF (W.EQ.0) THEN
    DEL = 1
  ENDIF
  Y     = F+EPS(1)*W

$SIGMA 1 FIXED

Where

  W = THETA(x) for Additive and Lognormal errors. THETA(x)
      represents SD of variability

  W = THETA(x)*IPRED for Proportional errors.  THETA(x)
      represents the SD of residual error

  W = SQRT(THETA(X)**2+THETA(Y)**2*IPRED**2).  THETA(x) Additive
      SD; THETA(y) Proportional SD

It also requires the DV of the dataset to have a corresponding
variable like LDV that log(DV)=LDV for Lognormal exploration.

Residual variability explored:\n\n " )
  (widget-create 'checkbox
                 :label "add"
                 :type "rv"
                 :notify 'esn-rv-form-toggle
                 esn-mode-rv-do-add
                 )
  (widget-insert " Additive Residual Error\n\t Additive SD Estimate:")
  (widget-create 'editable-field
		 :size 13
                 :label "add"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-rv-add-ini
		 )
  (widget-insert "\n ")
  (widget-create 'checkbox
                 :label "prop"
                 :type "rv"
                 :notify 'esn-rv-form-toggle
                 esn-mode-rv-do-prop
                 )
  (widget-insert " Proportional/Constant Coefficient of Variation Residual Error\n\t Proportional SD estimate:")
  (widget-create 'editable-field
		 :size 13
                 :label "prop"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-rv-prop-ini
		 )
  (widget-insert "\n ")
  (widget-create 'checkbox
                 :label "addprop"
                 :type "rv"
                 :notify 'esn-rv-form-toggle
                 esn-mode-rv-do-addprop
                 )
  (widget-insert " Additive+Proportional Residual Error\n\t Additive SD Estimate: ")
  (widget-create 'editable-field
		 :size 13
                 :label "addprop-add"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-rv-addprop-add-ini
		 )
  (widget-insert "\n\t Proportional SD Estimate: ")
  (widget-create 'editable-field
		 :size 13
                 :label "addprop-prop"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-rv-addprop-prop-ini
		 )
  (widget-insert "\n ")
  (widget-create 'checkbox
                 :label "logn"
                 :type "rv"
                 :notify 'esn-rv-form-toggle
                 esn-mode-rv-do-logn
                 )
  (widget-insert " Lognormal Residual Error\n\t Lognormal SD: ")
  (widget-create 'editable-field
		 :size 13
                 :label "logn"
                 :type "ini"
                 :notify 'esn-bsv-form-toggle
                 :valid-regexp "^[0-9]*\\([.][0-9]*\\)$"
                 esn-mode-rv-logn-ini
		 )
  (widget-insert "\n\n")
  (widget-create 'push-button
		 :notify 'esn-rv-form-submit
		 "Create Models")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
)

(provide 'esn-rv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-rv.el ends here
