;;; esn-start.el --- Starting Emacs Speaks NONMEM
;;
;; Filename: esn-start.el
;; Description: Dynamically load Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew Fidler
;; Created: Thu Apr 2212:13:43 2010 (-0500)
;; Version:
;; Last-Updated: Thu Feb 16 10:24:56 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 36
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
;; 16-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Feb 16 10:24:36 2012 (-0600) #35 (Matthew L. Fidler)
;;    Added esn-autoloads 
;; 21-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Dec 21 09:13:04 2011 (-0600) #31 (Matthew L. Fidler)
;;    Took out the `make-local-hook' function
;; 07-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Dec  7 11:10:58 2010 (-0600) #21 (Matthew L. Fidler)
;;    Bugfix to have esn-path added to load path without trailing slash.
;; 12-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Nov 12 19:24:22 2010 (-0600) #17 (Matthew L. Fidler)
;;    Took out esn-autoloads.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 14:16:27 2010 (-0500) #13 (Matthew L. Fidler)
;;    Removed requirement of esn-yas.
;; 08-Sep-2010    Matthew L. Fidler
;;    Last-Updated: Wed Sep  8 10:28:09 2010 (-0500) #7 (Matthew L. Fidler)
;;    Added autoloads variables and options to esn-start.el
;; 03-May-2010    Matthew L. Fidler
;;    Last-Updated: Mon May  3 15:32:30 2010 (-0500) #2 (Matthew L. Fidler)
;;    Added .con to the list of known control streams.
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


(defgroup esn-mode nil
  "Emacs Speaks NONMEM.  Allows color-coding, code-compeltion, wrapping, displaying lines over 80 characters, correct captilization without caps-lock."
  :group 'emacs)

(defvar esn-w32
  (memq system-type '(emx win32 w32 mswindows ms-dos windows-nt))
  "*Non-nil represents emacs running on windows.")

(setq esn-path (file-name-directory (or
                                     load-file-name
                                     buffer-file-name)))

(defun esn-turn-on-when-find-problem ()
  "Turns on ESN mode when $PROBLEM statement is found in a file"
  (interactive)
  (let ((ret nil))
    (save-excursion
      (setq ret (re-search-forward "^[ \t]*\\$[Pp][Rr][Oo][A-Za-z]" nil 't))
      (when ret
        (beginning-of-line)
        (while (and ret (not (bobp)))
          (if (not (looking-at "^\\([ \t]*\\|[ \t]*;.*\\|^[ \t]*\\$[Pp][Rr][Oo][A-Za-z].*\\)$"))
              (setq ret nil)
            (forward-line -1)
            (beginning-of-line)))))
    (symbol-value 'ret)))

(add-to-list 'magic-mode-alist
             '( esn-turn-on-when-find-problem . esn-mode))

(defcustom esn-default-extension
  '(".mod"
    ".ctl"
    ".pltc"
    ".con"
    ".nmctl")
  "* List of extensions for NONMEM models."
  :type '(repeat
          (string :tag "Extension for NONMEM model"))
  :group 'esn-mode)

(defcustom esn-nonmem-output-extension '(
                                         ".lst"
                                         ".rpt"
                                         ".out"
                                         ".res"
                                         )
  "* Default extension for NONMEM reports."
  :type '(repeat
          (string :tag "Extension for NONMEM report"))
  :group 'esn-mode)

(defcustom esn-nonmem-default-output-extension ".lst"
  "* Default extension for NONMEM reports.  Used for execution of NONMEM."
  :type 'string
  :group 'esn-mode)

(with-temp-buffer
  (let (
        (case-fold-search 't)
        (i 0))
    (while (< i (length esn-default-extension))
      (insert (format "(add-to-list 'auto-mode-alist '(\"\\\\%s\\\\'\" . esn-mode))"  (nth i esn-default-extension)))
      (setq i (+ i 1))))
  (eval-buffer))

(add-to-list 'load-path esn-path)
(add-to-list 'load-path (concat esn-path "lisp"))
(add-to-list 'load-path (concat esn-path "etc/cookies"))
(add-to-list 'image-load-path (concat esn-path "etc/icons"))

(add-hook 'esn-mode-hook (lambda () (interactive (flyspell-prog-mode))))
(autoload 'esn-mode "esn-mode" "Emacs Speaks NONMEM mode" t)
;; Now add autolook functions for text-mode and fundamental-mode
(defun esn-recheck-mode-hook ()
  "* This Hook checks on the fly what mode should be run.  This way when $PROBLEM is entered in a text-mode, emacs speaks NONMEM is invoked."
  (when (and
         (not (minibufferp))
         (not (string-match "[*]" (buffer-name)))
         (> magic-mode-regexp-match-limit (point))
         (or
          (string= major-mode "text-mode")
          (string= major-mode "fundamental-mode")))
    ;; Only when its going to make a difference
    (set-auto-mode 't)))

(defun esn-turn-on-check ()
  "Turns on a check for a recognized Control stream in .txt files and the like."
  (interactive)
  (add-hook 'post-command-hook 'esn-recheck-mode-hook nil t))
(add-hook 'text-mode-hook 'esn-turn-on-check)
(add-hook 'fundamental-mode-hook 'esn-turn-on-check)
(require 'esn-autoloads)
(autoload 'esn-mode "esn-mode" "EsN mode" t)
(provide 'esn-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-start.el ends here
