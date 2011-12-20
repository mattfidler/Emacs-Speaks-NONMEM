;;; esn-company.el --- Provide an interface for Company Mode
;; 
;; Filename: esn-company.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue Aug 17 12:25:56 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 06:53:29 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 513
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
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 09:44:51 2010 (-0500) #322 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
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

(declare-function esn-complete-meta "esn-help")
(declare-function esn-complete-help "esn-help")
(eval-when-compile
  (require 'esn-start)
  (require 'cl))
(require 'esn-ac-sources)
(require 'esn-completion)
(require 'company nil t)

(defun esn-company-remove-keys (&rest ignored)
  "* Removes Esn keys to company keymap when esn-mode is enabled"
  (when (eq major-mode 'esn-mode)
    ;; Add pre and post command hooks again.
    (mapc 
     (lambda(x)
       (define-key company-active-map x 'self-insert-command))
     (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
           "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))
;;;###autoload
(defun esn-company-start (&rest ignored)
  "* Start Company completion within EsN."
  (interactive)
  (when (and (= esn-completion-type 1) (fboundp 'company-mode))
    (esn-completion-off)
    ;; Change keys to allow Upcase insertion while completing.
    (add-hook 'company-completion-started-hook 'esn-company-add-keys)
    (add-hook 'company-completion-cancelled-hook 'esn-company-remove-keys)
    (add-hook 'company-completion-finished-hook 'esn-company-remove-keys)
    (add-hook 'company-completion-finished-hook 'esn-after-completion)
    (company-mode 1)
    (set (make-local-variable 'company-backends) '(company-esn))))

(defun esn-company-add-keys (&rest ignored)
  "* Adds Esn keys to Company Keymap when esn-mode is enabled"
  (when (eq major-mode 'esn-mode)
    ;; remove pre and post command hooks again.
    (mapc 
     (lambda(x)
       (define-key company-active-map x 'esn-upcase-char-self-insert))
     (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
           "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))

(defun esn-complete-candidates (prefix)
  "* Return a list of completion candidates that start with the prefix"
  (esn-complete-prefix nil prefix))
(defun company-esn (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (let (ret (debug-on-error 't))
    (case command
      ('interactive (company-begin-backend 'company-esn))
      ('prefix 
       ;; The back-end should return the text to be completed.  It must be text
       ;; immediately before `point'.  Returning nil passes control to the next
       ;; back-end.  The function should return 'stop if it should complete but
       ;; cannot \(e.g. if it is in the middle of a string\).  If the returned
       ;; value is only part of the prefix (e.g. the part after \"->\" in C), the
       ;; back-end may return a cons of prefix and prefix length, which is then
       ;; used in the `company-minimum-prefix-length' test.
       (setq ret (esn-complete-prefix))
       )
      ('candidates
       ;; The second argument is the prefix to be completed.  The return value
       ;; should be a list of candidates that start with the prefix.
       (setq ret esn-current-completions))
      ('meta
       ;; The second argument is a completion candidate.  The back-end should
       ;; return a (short) documentation string for it.
       (setq ret (esn-complete-meta (printc arg))))
      ('doc-buffer
       ;; The second argument is a completion candidate.  The back-end should
       ;; create a buffer (preferably with `company-doc-buffer'), fill it with
       ;; documentation and return it.
       (setq ret (esn-complete-help (printc arg)))))
    ;;The back-end should return nil for all commands it does not support or does
    ;;not know about.  It should also be callable interactively and use
    ;;`company-begin-backend' to start itself in that case.
    (symbol-value 'ret)))

(provide 'esn-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-company.el ends here
