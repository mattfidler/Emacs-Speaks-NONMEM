;;; esn-completion-sources.el --- Defines Completion sources for Auto-complete mode.
;; 
;; Filename: esn-completion-sources.el
;; Description: Defines Completion sources for Auto-complete mode
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Dec  8 15:23:00 2010 (-0600)
;; Version: 
;; Last-Updated: Thu Apr 28 00:11:35 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 47
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This defines Emacs Speaks NONMEM sources for Auto-complete-mode.  Used to generate all completion routines
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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
(declare-function esn-after-completion "esn-completion")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-complete-is-known-option "esn-completion")
(declare-function esn-rec3 "esn-fun")
(declare-function esn-complete-is-known-option-value "esn-completion")
(declare-function esn-complete-defined-variables "esn-completion")
(eval-when-compile
  (require 'esn-start)
  (require 'esn-vars)
  )

(defvar ac-source-esn-records nil
  "* Esn ac source records.")

(defvar ac-source-esn-options-cached nil
  "* Are the options cached?")

(defvar ac-source-esn-options-list '()
  "* Esn after options cache")

(defvar ac-source-esn-options nil
  "* Esn ac source options.")

(defvar ac-source-esn-option-value-cached nil
  "* Are the options cached?")

(defvar ac-source-esn-option-value-list '()
  "* Esn after options cache")

(defvar ac-source-esn-option-value nil
  "* Esn ac source options.")

(defvar ac-source-esn-xmind nil
  "* Esn ac source Xmind completions.")

(setq ac-source-esn-records
      '(
        (candidates . (lambda() (symbol-value 'esn-current-records-complete)))
        (prefix . "\\<[$][A-Za-z0-9_]*")
        (requires . 1)
        (action . esn-ac-after-record)
        (symbol . "R")
        (summary . esn-complete-meta)
        (document . esn-complete-help)))

(defun esn-ac-after-record ()
  "* Defines completion function after record completion"
  (condition-case error
      (progn
        (setq ac-source-esn-options-cached nil)
        (when (looking-back (eval-when-compile (esn-reg-records)))
          (esn-after-completion (match-string 0))))
    (error
     (message "Esn After record error: %s" (error-message-string error)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options completion
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-source-esn-options
      '(
        (init . esn-ac-options)
        (prefix . "\\(?:[^= \t\n]\\|^\\)[ \t]*\\<\\([A-Za-z0-9_]*\\)")
        (available . (lambda () (esn-current-rec)))
        (requires 1)
        (candidates . ac-source-esn-options-list)
        (action . esn-ac-after-option)
        (symbol . "O")
        (summary . esn-complete-meta)
        (document . esn-complete-help)
        (candidate-face . esn-ac-option-candidate-face)
        (selection-face . esn-ac-option-selection-face)))

(defface esn-ac-option-candidate-face
  '((t (:background "violet" :foreground "black")))
  "Auto-completion Face for esn options candidate."
  :group 'auto-complete
  :group 'esn-display
  :group 'esn-completion)

(defface esn-ac-option-selection-face
  '((t (:background "DarkViolet" :foreground "white" :weight bold)))
  "Auto-completion Face for the esn-options selected candidate."
  :group 'auto-complete
  :group 'esn-display
  :group 'esn-completion)

(defun esn-ac-options ()
  "* Initialization function to determine the options passed."
  (condition-case error
      (progn
        (let (
              (rec (esn-get-current-rec))
              lst
              )
          (unless (and ac-source-esn-options-cached 
                       (string= ac-source-esn-options-cached rec))
            (if (and rec 
                     (progn
                       (setq lst (esn-complete-is-known-option rec "" 't))
                       (symbol-value 'lst)))
                (progn
                  (setq ac-source-esn-options-cached rec)
                  (setq ac-source-esn-options-list lst))
              (setq ac-source-esn-options-cached rec)
              (setq ac-source-esn-options-list lst)))))
    (error
     (message "ESN Auto-complete options initialization error: %s" (error-message-string error)))))

(defun esn-ac-after-option ()
  "* Defines completion function after option completion"
  (condition-case error
      (progn
        (when (looking-back "\<[A-Za-z0-9_]*")
          (esn-after-completion (match-string 0))))
    (error
     (message "After Option completion Error: %s" (error-message-string error)))))

;; See esn-advices to see how EsN implements the automatic code finishing.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option value completion routines.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ac-source-esn-option-value
      '(
        (init . esn-ac-option-values)
        (prefix . "\\<[A-Z_0-9]\\{3,\\}[ \t]*[= \t][ \t]*\\([A-Z0-9_]+\\)")
        (available . (lambda ()
                       (let (
                             (ret (esn-current-rec))
                             opt
                             ret)
                         (when (ret)
                           (when (looking-back "\\<\\([A-Z_0-9]\\{3,\\}\\)[ \t]*[= \t][ \t]*[A-Z0-9_]+")
                             (setq opt (concat (esn-rec3 (match-string 1)) "="))
                             (setq lst (esn-complete-is-known-option-value rec opt ""))
                             (if lst
                                 (setq ac-source-esn-option-value-list lst)
                               (setq ac-source-esn-option-value-list '()))
                             (setq ac-source-esn-option-value-cached (concat rec ":" opt))
                             (setq ret lst)))
                         (symbol-value 'ret))))
        (requires . 1)
        (candidates . ac-source-esn-option-value-list)
        (action . esn-ac-after-option-value)
        (symbol . "V")
        (summary . esn-complete-meta)
        (document . esn-complete-help)))

(defun esn-ac-after-option-value ()
  "* Defines completion function after option=value completion"
  nil)

(defun esn-ac-option-values ()
  "* Initialization function to determine the known values to the specified option. "
  (condition-case error
      (progn
        (let (
              (debug-on-error 't)
              (rec (esn-get-current-rec))
              opt
              lst
              )
          (if rec
              (when (looking-back "\\<\\([A-Z_0-9]\\{3,\\}\\)[ \t]*[= \t][ \t]*[A-Z0-9_]+")
                (setq opt (concat (esn-rec3 (match-string 1)) "="))
                (unless (and ac-source-esn-option-value-cached
                             (string= ac-source-esn-option-value-cached (concat rec ":" opt)))
                  (setq lst (esn-complete-is-known-option-value rec opt ""))
                  (if lst
                      (setq ac-source-esn-option-value-list lst)
                    (setq ac-source-esn-option-value-list '()))
                  (setq ac-source-esn-option-value-cached (concat rec ":" opt)))))))
    (error
     (message "Esn AC option value error: %s" (error-message-string error)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xmind completion routines
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq ac-source-esn-xmind
      '(
        (prefix . "[;C|]*[ \t]*\\(?:Free\\|X\\)?\\(?:mind\\(?:map\\)?\\|map\\)[ \t]*[:=][ \t]*\\(.*\\)")
        (requires . 1)
        (candidates . esn-xmind-list-of-completions)
                                        ;        (action . esn-ac-after-option-value)
        (symbol . "X")
                                        ;        (summary . esn-complete-meta)
        (document . esn-complete-xmind-echo)))

(defun esn-complete-xmind-echo (arg)
  "* Returns argument for ac-echoing"
  arg)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbreviated LHS completion routines.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-ac-esn-lhs ()
  "Generates esn LHS completion list."
  (condition-case error
      (progn
        (let (ret)
          (setq ret (append 
                     
                     (eval-when-compile (mapc (lambda(x) (concat x "(")) esn-complete-operators))
                     (esn-complete-defined-variables)))
          (symbol-value 'ret)))
    (error
     (message "Esn LHS auto-completion error: %s" (error-message-string error)))))

(defvar ac-source-esn-lhs nil)

(setq ac-source-esn-lhs
      '(
        (candidates . esn-ac-esn-lhs)
        (symbol . "L")
        (requires . 1)
        (available . esn-is-abbrev-p)
        (prefix . "^[ \t]*[A-Za-z0-9_]+[ \t]*=[^\n=]*\\<\\(.*\\)")))


(defvar esn-ac-sources nil
  "Variable defining the Auto-completion sources for EsN.")

(setq esn-ac-sources '(ac-source-esn-lhs ac-source-esn-options ac-source-esn-option-value ac-source-esn-records))

(setq esn-ac-sources '(ac-source-esn-records))
(provide 'esn-ac-sources)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-ac-sources.el ends here
