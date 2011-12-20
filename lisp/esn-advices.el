;;; esn-advices.el --- Advices for keys.
;; 
;; Filename: esn-advices.el
;; Description: Advices
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Aug 30 12:29:20 2010 (-0500)
;; Version: 0.13
;; Last-Updated: Thu Apr 28 00:24:15 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 75
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs NONMEM 
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
;; 30-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 30 09:39:49 2010 (-0600) #31 (Matthew L. Fidler)
;;
;;    I have found these advices to be unreliable in Emacs 23.2.
;;    (They affect other packages).  I'm working on a way to remove
;;    them.
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

(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-magic-bs "esn-magic-keys")


;;backward-delete-char-untabify -- Backspace
(defun esn-delete-backward-char (&optional pre forward)
  "Esn delete-backward-char"
  (interactive "p")
  (let ((n (or pre 1))
        (i 0))
    (while (< i n)
      (esn-magic-bs forward)
      (setq i (+ i 1)))))

(defun esn-backward-delete-char-untabify (&optional pre)
  "Esn delete-backward-char-untabify"
  (interactive "p")
  (esn-delete-backward-char pre))

(defun esn-delete-char (&optional pre)
  "Esn delete-char"
  (interactive "p")
  (esn-delete-backward-char pre t))


(defun esn-theta-del-fixed ()
  "* Function that deletes the word FIXED in the $THETA block"
  (interactive)
  (message "Delete FIXED")
  (let (
        (case-fold-search 't)
        (tmp "")
        )
    (save-match-data
      (save-excursion
        (if (not (re-search-backward "(\\([0-9.Ee+-]+\\)[ \t,]*FIX\\(?:ED?\\)?[ \t]*\\=" nil t))
            (when (re-search-backward "FIX\\(?:ED?\\)?[ \t]*\\=" nil t)
              (replace-match ""))
          (setq tmp (match-string 1))
          (if (< 0 (string-to-number tmp))
              (replace-match "(0, \\1")
            (replace-match "(\\1")))))))

(defun esn-backward-kill-word (&optional pre)
  "Esn backward-kill-word"
  (interactive "p")
  (let ((n (or pre 1))
        (i 0))
    (if (and (string= "THE" (esn-get-current-record))
             (save-match-data
               (string-match "\\<[Ff][Ii][Xx]\\(?:[Ee][Dd]?\\)\\>" (buffer-substring (save-excursion
                                                                                       (forward-word -1)
                                                                                       (point))
                                                                                     (point)))))
        (progn
          (esn-theta-del-fixed)
          (setq n (- n 1))
          (unless (> n 0)
            (esn-backward-kill-word n)))
      (backward-kill-word n))))


(when nil
  (defadvice ac-expand (around
                        esn-ac-expand
                        ;; preactivate
                        activate
                        )
    "Changes tab completion to insert extra completion items when applicable."
    ad-do-it
    (when (eq major-mode 'esn-mode)
      (message "Expanded.. After")
      (when (looking-back "\\<[A-Za-z0-9_]*[ \t=]")
        (esn-after-completion (match-string 0))))))

(defun esn-dlgopen-browse-file (&optional types)
  "* Provides get file from dialog box"
  (interactive)
  (let (
        (type (or types "Data Files|*.csv;*.tbl;*.tab;*.txt|All Files|*.*"))
        (tmp-file (make-temp-file "vbs"))
        (ret nil)
        )
    (with-temp-file tmp-file
      (insert "
Option Explicit

Dim objDialog, boolResult

Set objDialog = CreateObject(\"UserAccounts.CommonDialog\")

objDialog.InitialDir = \"")
                                        ; Insert current dirrectory
      (insert "\"
objDialog.Filter = \"")
                                        ; Insert Filter
      (insert type)
      (insert "\"
objDialog.FilterIndex = 1

boolResult = objDialog.ShowOpen
Wscript.Echo(\"(setq ret\")
If boolResult = 0 Then
        Wscript.Echo(\"nil\")
Else
        Wscript.Echo(\"\"\"\" &Replace(objDialog.FileName,\"\\\",\"\/\") & \"\"\"\")
End If
Wscript.Echo(\")\")
")
      )
    (rename-file tmp-file (concat tmp-file ".vbs"))
    (setq tmp-file (concat tmp-file ".vbs"))
    (with-temp-buffer
      (insert
       (shell-command-to-string
        (format "cscript %s"
                tmp-file
                )
        )
       )
      (goto-char (point-min))
      (forward-line 2)
      (end-of-line)
      (delete-region (point-min) (point))
      (eval-buffer)
      )
    (delete-file tmp-file)
    (symbol-value 'ret)
    )
  )
(when nil
  (defadvice write-file  (around
                          esn-delete-char
                          ;; preactivate
                          activate
                          )
    "* When in EsN mode doing a SAVE-AS, set some variables for
proper file updating."
    
    (when (eq major-mode 'esn-mode)
      ;; Last Full Name.
      (setq esn-last-full-name (buffer-file-name))
      (setq esn-save-last-full-name esn-last-full-name)
      ;; Last Purpose, from last saved file.
      (with-temp-buffer
        (insert-file-contents esn-save-last-full-name)
        (setq esn-last-purpose (esn-update-get-purpose))
        )
      )
    ad-do-it
    )
  )
(provide 'esn-advices)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-advices.el ends here
