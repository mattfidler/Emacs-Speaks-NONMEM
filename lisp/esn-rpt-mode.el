;;; esn-rpt-mode.el --- EsN's output display mode.
;;
;; Filename: esn-rpt-mode.el
;; Description: Displays NONMEM output.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Feb  1 16:07:31 2010 (-0600)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 53
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: 23.x
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
;; 16-Dec-2010    Matthew L. Fidler  
;;    Added autoload cookies and broke apart esn-get-rpt and esn-switch-rpt
;; 15-Jun-2010    Matthew L. Fidler  
;;    Added Switching between control stream and output in archived PLT tools runs.
;; 22-Apr-2010    Matthew L. Fidler  
;;    Added EsN error reporting.
;; 01-Feb-2010    Matthew L. Fidler
;;    Added Report Mode Switch again.
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


(declare-function esn-error "esn-exec")
(declare-function esn-plt-find-timestamps "esn-plt")

(defvar esn-rpt-mode-hook nil)

;;;###autoload
(defun esn-get-rpt (&optional file-name)
  "Function that gets the name of the NONMEM output based on the current file.

FILE-NAME is an optional argument that changes the file name from
the current buffer file name to whatever is supplied."
  (let (rpt-name timestamps)
    (if (esn-use-plt-archive-p file-name)
        (progn
          (setq rpt-name (or file-name (buffer-file-name)))
          ;; Swap Control. for Output.
          (when (string-match "Control[.]" rpt-name)
            (setq rpt-name (replace-match "Output." 't 't rpt-name)))
          ;; Swap CONTROL for RAWOUTPUT
          (when (string-match "CONTROL" rpt-name)
            (setq rpt-name (replace-match "RAWOUTPUT" 't 't rpt-name)))
          (if (file-exists-p rpt-name)
              (set-buffer  (find-file-noselect (concat rpt-name)))
            (esn-error (format "Could not find run output. Need to submit (%s)" rpt-name))))
      (if (not (esn-use-plt-p))
          (mapc (lambda(x)
                  (when (file-exists-p x)
                    (setq rpt-name x)))
                esn-nonmem-output-extension)
        ;; Now use grep to try to find report name.
        (setq timestamps (esn-plt-find-timestamps))
        (if (= (length timestamps) 0)
            (esn-error "Could not find run output.  Need to submit, or make the grep command accessible to emacs")
          (setq rpt-name (concat "../TEXTFILES/RAWOUTPUT/Output." (nth 0 timestamps) ".txt")))))
    (symbol-value 'rpt-name)))

(defun esn-get-mod (&optional file-name)
  "Function that gets the model name of the NONMEM output based on the current file.

FILE-NAME is an optional argument that changes the file name from
the current buffer file name to whatever is supplied."
  (let (mod-name timestamps)
    (if (esn-use-plt-archive-p file-name)
        (progn
          (setq mod-name (or file-name (buffer-file-name)))
          ;; Swap Output. for Control.
          (when (string-match "Output[.]" mod-name)
            (setq mod-name (replace-match "Control." 't 't mod-name)))
          ;; Swap RAWOUTPUT for CONTROL 
          (when (string-match "RAWOUTPUT" mod-name)
            (setq mod-name (replace-match "CONTROL" 't 't mod-name)))
          (if (file-exists-p mod-name)
              (set-buffer  (find-file-noselect (concat mod-name)))
            (esn-error (format "Could not find corresponding control stream (%s)." mod-name))))
      (if (not (esn-use-plt-p))
          (mapc (lambda(x)
                  (when (file-exists-p x)
                    (setq mod-name x)))
                (append '(".txt") esn-default-extension))
        ;; Now use grep to try to find report name.
        ;; I'm not sure this is required for Models.
        (setq timestamps (esn-plt-find-timestamps))
        (if (= (length timestamps) 0)
            (esn-error "Could not find run output.  Need to submit, or make the grep command accessible to emacs")
          (setq mod-name (concat "../TEXTFILES/CONTROL/Control." (nth 0 timestamps) ".txt")))))
    (symbol-value 'mod-name)))
  
;;;###autoload
(defun esn-switch-rpt ()
  "* Function to switch to output listing"
  (interactive)
  (let ((rpt-name (esn-get-rpt)))
    (if (not rpt-name)
        (esn-error "Could not find run output. Need to submit")
      (find-file rpt-name))))

(defvar esn-rpt-mode-map
  (let (
	(esn-rpt-mode-map (make-keymap))
	)
    (define-key esn-rpt-mode-map "\C-c\C-c" 'esn-switch-mod)
    esn-rpt-mode-map
    )
  )

(defun esn-switch-mod ()
  (interactive)
  (let
      (
       (fn (buffer-file-name))
       (i 0)
       )
    (if (string-match (regexp-opt esn-nonmem-output-extension 't) fn)
	(setq fn (replace-match "" nil nil fn))
      )
    (while (< i (length esn-default-extension))
      (if (not (file-exists-p (concat fn (nth i esn-default-extension)))) nil
	(setq i (length esn-default-extension))
	(set-buffer (find-file-noselect (concat fn (nth i esn-default-extension))))
	)
      (setq i (+ i 1))
      )
    )
  )

(defvar esn-rpt-font-lock-keywords nil
  "Font lock keywords for Report Mode."
  )

(defun esn-rpt-mode ()
  (interactive)
  (kill-all-local-variables)
  (auto-revertro-mode)
  (toggle-read-only 1) ;; to protect the buffer.
  (use-local-map esn-rpt-mode-map)
  ;;
  (setq esn-rpt-font-lock-keywords
	(list
	 '("^\\(0MINIMIZATION TERMINATED\\)$"
	   (1 font-lock-warning-face))
	 '("\\(\\*+\\)"
	   (1 font-lock-builtin-face))
	 '("^\\(0R MATRIX ALGORITHMICALLY SINGULAR\\)$"
	   (1 font-lock-warning-face))
	 '("^\\(0COVARIANCE STEP ABORTED\\)$"
	   (1 font-lock-warning-face))
	 '("^\\(0\\)\\([^:\n]*:\\)\\(.*\\)$"
		(1 font-lock-warning-face)
		(2 font-lock-type-face)
		(3 font-lock-comment-face))
	 '("^\\(0\\)\\(.*\\)$"
		(1 font-lock-warning-face)
		(2 font-lock-type-face))
	 '("^\\([^:\n]*:\\)\\(.*\\)$"
	   (1 font-lock-builtin-face)
	   (2 font-lock-comment-face))
	 )
	)
  (set (make-local-variable 'font-lock-defaults) '(esn-rpt-font-lock-keywords))
  (font-lock-mode)
  (run-hooks 'esn-rpt-mode-hook)
  (setq mode-name "NONMEM rpt")
  (setq major-mode 'esn-rpt-mode)
  )

(add-to-list 'auto-mode-alist '("\\.rpt\\'" . esn-rpt-mode))
(provide 'esn-rpt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-rpt-mode.el ends here
