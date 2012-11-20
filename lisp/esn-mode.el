;;; esn-mode.el --- A Mode for Editing NONMEM files
;;
;; Filename: esn-mode.el
;; Description: A mode for editing NONMEM files
;; Author: Matthew L. Fidler
;; Maintainer: Matthew Fidler
;; Created: Tue Jan 26 15:04:17 2010 (-0600)
;; Version: 0.13
;; Last-Updated: Thu Dec 22 17:16:32 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 515
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs NONMEM
;; Compatibility: 23.x.  Let me know if any other versions work.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; See http://esnm.sourceforge.net
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 22-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Dec 22 17:06:05 2011 (-0600) #513 (Matthew L. Fidler)
;;    Attempted to support `org-table-comment'
;; 07-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  7 13:06:00 2011 (-0600) #505 (Matthew L. Fidler)
;;    Make local hook removed.
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 10:39:23 2010 (-0600) #369 (Matthew L. Fidler)
;;    Added Imenu support.
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 10:38:33 2010 (-0600) #367 (Matthew L. Fidler)
;;    Added Imenu support.
;;
;; 13-Sep-2010    Matthew L. Fidler
;;    Last-Updated: Mon Sep 13 11:42:20 2010 (-0500) #291 (Matthew L. Fidler)
;;    Removed esn-symbols initialization (now in font-lock)
;; 10-Sep-2010    Matthew L. Fidler
;;    Last-Updated: Fri Sep10 16:43:40 2010 (-0500) #289 (Matthew L. Fidler)
;;    Added Code folding by default when selected.
;; 24-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Tue Aug 24 11:57:14 2010 (-0500) #235 (Matthew L. Fidler)
;;
;;    Had Esn determine which version is being used when a file is open, and
;;    assign appropriate regular expressions.
;;
;; 23-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Mon Aug 23 14:45:37 2010 (-0500) #233 (Matthew L. Fidler)
;;    Took out explicit reference to magic return.
;; 17-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Tue Aug 17 15:13:46 2010 (-0500) #207 (Matthew L. Fidler)
;;    Added company-mode support
;; 13-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Fri Aug 13 08:41:32 2010 (-0500) #192 (Matthew L. Fidler)
;;    Removed all font-lock references and put in esn-fontlock
;; 11-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Wed Aug 11 14:27:45 2010 (-0500) #184 (Matthew L. Fidler)
;;    Made esn-get-current-record-stop local
;; 27-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jul 27 15:12:20 2010 (-0500) #148 (Matthew L. Fidler)
;;    Added Esn-help
;; 13-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jul 13 11:06:52 2010 (-0500) #144 (Matthew L. Fidler)
;;    Added local variable esn-xpose-get-input-line-save
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 17:01:26 2010 (-0500) #142 (Matthew L. Fidler)
;;    Added other local variables for speed.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 16:39:27 2010 (-0500) #135 (Matthew L. Fidler)
;;    Added absolute directory hash as a local variable.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 15:44:24 2010 (-0500) #133 (Matthew L. Fidler)
;;    Added local variables for individual control stream's xmind map update optimizations.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 13:22:43 2010 (-0500) #114 (Matthew L. Fidler)
;;    Changed local variable declarations.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 12:49:43 2010 (-0500) #109 (Matthew L. Fidler)
;;    Added local variable esn-xmind-last-topic-title
;; 15-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jun 15 16:38:54 2010 (-0500) #104 (Matthew L. Fidler)
;;    Added PLT tools archive checking for files in the correct locations...
;; 05-May-2010    Matthew L. Fidler
;;    Last-Updated: Wed May  5 15:01:23 2010 (-0500) #80 (Matthew L. Fidler)
;;    Added warning font to $DATA file, and other input files.
;; 18-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Thu Feb 18 08:32:32 2010 (-0600) #67 (Matthew L. Fidler)
;;    Changed default size of max-lisp-eval-depth
;; 15-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb 15 12:34:26 2010 (-0600) #60 (Matthew L. Fidler)
;;    Add bugfix for killing emacs
;; 11-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Thu Feb 11 11:23:00 2010 (-0600) #58 (Matthew L. Fidler)
;;    Added Save completion when Emacs is exited.
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 20:57:47 2010 (-0600) #36 (Matthew L. Fidler)
;;    Make Timer to update Xmind map to have faster saves.
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 10:07:15 2010 (-0600) #30 (Matthew L. Fidler)
;;
;;    Removing adaptive font lock in comments removes adaptive font lock.
;;    Changing back.
;;
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 09:34:22 2010 (-0600) #25 (Matthew L. Fidler)
;;    Adaptive font lock only occurs outside of comments now.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 16:49:57 2010 (-0600) #22 (Matthew L. Fidler)
;;
;;    Added Xmind's Last Parent Topic as a local variable to detect when someone
;;    changes the Xmind parent, and the whole tree should be moved.
;;
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:05:12 2010 (-0600) #2 (Matthew L. Fidler)
;;    Added Xmind support
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

(declare-function esn-mode-add-header-to-file "esn-update")
(declare-function esn-add-update-topics-now "esn-xmind")
(declare-function esn-plt-auto-now "esn-plt")
(declare-function esn-after-save-function "esn-update")
(declare-function esn-should-be-new-version "esn-vc")
(declare-function esn-mode--keymap "esn-keys-menu")
(declare-function esn-mode--fontlock "esn-fontlock")
(declare-function esn-update-save-variables "esn-update")
(declare-function esn-start-folding "esn-hs")
(declare-function esn-message "esn-exec")
(declare-function esn-build-yas "esn-yas")
(declare-function esn-company-start "esn-company")
(declare-function esn-cui-start "esn-cui")
(declare-function esn-ac-start "esn-ac")
(require 'cl)

(defvar esn-mode-ver 0.13)
(setq esn-last-time (float-time))
(setq esn-load-time-start (float-time))

(defun esn-r (txt &optional filename noerror)
  "*Emacs Portable Time"
  (require txt filename noerror)
  (message "[EsN] %s loaded in %1f seconds, %1f seconds elapsed since initial load"
           (symbol-name txt)
           (- (float-time) esn-last-time)
           (- (float-time) esn-load-time-start))
  (setq esn-last-time (float-time)))

;;(when (< max-lisp-eval-depth 10000)
;;  (setq max-lisp-eval-depth 10000))
;; Load w32-browser if present.
(esn-r 'w32-browser nil t)
(esn-r 'esn-rec-hooks)
(esn-r 'esn-magic-keys)
(esn-r 'esn-templates)
(esn-r 'esn-tables)
(esn-r 'esn-vars)
(esn-r 'esn-options)
(esn-r 'esn-fun)
(esn-r 'esn-properties)
(esn-r 'esn-which)
(esn-r 'esn-keys-menu)
(esn-r 'esn-autoloads)
(esn-r 'esn-advices)
(esn-r 'esn-imenu)
(esn-r 'esn-update)
(esn-r 'esn-which)
(esn-r 'esn-version-switch)
(esn-r 'esn-dnd)
(esn-r 'esn-indent)
(esn-r 'esn-narrow)
(esn-r 'esn-fontlock)
(esn-r 'esn-align)
(esn-r 'esn-hide)
(esn-r 'esn-input)
(esn-r 'esn-toolbar)
(esn-r 'esn-exec)
(esn-r 'esn-yas)
(esn-r 'esn-help)
(esn-r 'esn-tab-pred)
(esn-r 'esn-completion)

(require 'org-table-comment nil t)


(defun esn-insert-new-buffer-strings()
  "Insert new buffer template."
  ;;  (esn-mode-buffer-template-get)
  (when esn-update-add-header-on-create
    (esn-mode-add-header-to-file 't))
  (goto-char (point-max))
  ;;  (esn-update-header)
  )


;; syntax - leave it in, we might fool with it later.

(defvar esn-mode-syntax-table nil
  "Syntax table for esn-mode." )

(if esn-mode-syntax-table
    ()
  (setq esn-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" "." esn-mode-syntax-table)
  ;;  (modify-syntax-entry ?\\ "\\" esn-mode-syntax-table)
  (modify-syntax-entry ?\n ">" esn-mode-syntax-table)
  (modify-syntax-entry ?\f ">" esn-mode-syntax-table)
  (modify-syntax-entry ?\; "<" esn-mode-syntax-table)
  (modify-syntax-entry ?# "." esn-mode-syntax-table)
  (modify-syntax-entry ?/ "." esn-mode-syntax-table)
  (modify-syntax-entry ?* "." esn-mode-syntax-table)
  (modify-syntax-entry ?+ "." esn-mode-syntax-table)
  (modify-syntax-entry ?- "." esn-mode-syntax-table)
  (modify-syntax-entry ?@ "_" esn-mode-syntax-table)
  (modify-syntax-entry ?= "." esn-mode-syntax-table)
  (modify-syntax-entry ?% "." esn-mode-syntax-table)
  (modify-syntax-entry ?< "." esn-mode-syntax-table)
  (modify-syntax-entry ?> "." esn-mode-syntax-table)
  (modify-syntax-entry ?& "." esn-mode-syntax-table)
  (modify-syntax-entry ?| "." esn-mode-syntax-table)
  (modify-syntax-entry ?_ "w" esn-mode-syntax-table)
  (modify-syntax-entry ?1 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?2 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?3 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?4 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?5 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?6 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?7 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?8 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?9 "w" esn-mode-syntax-table)
  (modify-syntax-entry ?. "w" esn-mode-syntax-table)
                                        ;  (modify-syntax-entry ?\' "\"" esn-mode-syntax-table)
  )


(defun esn-mode--ini ()
  "Mode Step 1."
  (kill-all-local-variables)
  (setq major-mode 'esn-mode)
  (set-syntax-table esn-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'esn-indent-line)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start-skip) ";\\W*")
  ;; Call the after-save-function.
  (esn-after-save-function)
  ;; Turn on completion modes.
  (cond
   ((= 2 esn-completion-type)
    (esn-r 'esn-ac)
    (esn-ac-start))
   ((= 1 esn-completion-type)
    (esn-r 'esn-company)
    (esn-company-start))
   (t
    (esn-r 'esn-comint)
    (esn-comint-complete)))
  ;;
  ;; Column Wrapping length
  ;;
  (set (make-local-variable 'fill-column) esn-character-limit)
  ;;  (setq-default fill-column esn-character-limit)
  )



(defun esn-mode--local-vars ()
  (setq ess-nuke-trailing-whitespace-p nil)
  (setq esn-buffer-should-be-read-only nil)
  (setq esn-commit-last-version nil)
  (setq esn-var-names '())
  (setq esn-wfn-dups '())
  (set (make-local-variable 'max-lisp-eval-depth) 10000000)
  (setq esn-mode 't)
  )
(defun esn-mode--build-new ()
  (when (and esn-build-new-buffer (zerop (buffer-size)))
    (esn-insert-new-buffer-strings)
    (setq esn-from-new-file 't)))

(defun esn-mode--hooks ()
  (interactive)
  (add-hook 'write-contents-hooks 'esn-update-header nil 't)
  (add-hook 'after-save-hook 'esn-after-save-function nil 't)
  (add-hook 'post-command-hook 'esn-post-command-hook nil 't)
  (add-hook 'pre-command-hook 'esn-pre-command-hook nil t)
  (add-hook 'window-scroll-functions 'esn-window-scroll-functions nil t)
  (add-hook 'kill-buffer-hook 'esn-kill-buffer-hook nil t)
  (add-hook 'kill-emacs-hook 'esn-kill-emacs-hook nil t)
  (add-hook 'before-change-functions 'esn-before-change-functions-hook nil t)
  ;; In case some other post-command hook ruins it for everyone else.
  ;;    (run-with-timer 1 3 'esn-mode--hooks)
  ;;  (add-hook 'window-size-change-functions 'esn-window-size-change-functions nil t)
  )
(defvar esn-buffers-list '()
  "* List of Buffers that Emacs Speaks NONMEM is currently editing")

(defun esn-kill-buffer-hook ()
  "* Defines functions run on Buffer Kill"
  (condition-case nil
      (when (esn-use-xmind-p)
        (esn-add-update-topics-now))
    (error nil))
  ;; (condition-case nil
  ;;     (when (esn-use-plt-p)
  ;;       (esn-plt-auto-now)
  ;;       (when (and esn-plt-gen-xpose esn-plt-gen-xpose-r-script)
  ;;         (esn-mode-xpose-gen-summary-now)))
  ;;   (error nil))
  ;; (condition-case nil
  
  ;;     (when (and (esn-use-xpose-p)
  ;;                esn-mode-xpose-summary-document)
  ;;       (esn-mode-xpose-gen-summary-now))
  ;;   (error nil))
  (condition-case nil
      (when esn-buffers-list
        (setq esn-buffers-list (remove (current-buffer) esn-buffers-list)))
    (error nil)))

(defun esn-kill-emacs-hook ()
  "* Hook run when Emacs closes (finishes save of xmind topics, etc.)"
  (save-excursion
    (mapc (lambda(buf)
            (set-buffer buf)
            (esn-kill-buffer-hook))
          esn-buffers-list)))

(defun esn-mode--ini-functions ()
  (esn-after-save-function) ;; Do the after save stuff.
  (set-buffer-modified-p nil)
  (esn-should-be-new-version))

(defvar esn-mode-skip-ini nil)
(defvar esn-mode-tried-ini nil)
(defun esn-set-mode-name ()
  "Sets the Mode name based on what assumptions are used"
  (if (esn-use-plt-archive-p)
      (progn
        (setq mode-name "EsN(PLT-Archive)")
        (toggle-read-only 1))
    (if (esn-use-plt-p)
        (if (esn-use-xpose-p)
            (setq mode-name "EsN(Xpose/PLT)")
          (setq mode-name "EsN(PLT)"))
      (if (esn-use-pdx-p)
          (if (esn-use-xpose-p)
              (setq mode-name "EsN(Xpose/PDx)")
            (setq mode-name "EsN(PDx)"))
        (if (esn-use-xpose-p)
            (if (esn-use-plt-p)
                (setq mode-name "EsN(Xpose/Pirana)")
              (if (esn-use-census-p)
                  (setq mode-name "EsN(Xpose/Census)")
                (setq mode-name "EsN(Xpose)")))
          (if (esn-use-plt-p)
              (setq mode-name "EsN(Pirana)")
            (if (esn-use-census-p)
                (setq mode-name "EsN(Census)")
              (setq mode-name "EsN"))))))))

(defun esn-mode ()
  "Major mode for editing NonMem code."
  (interactive)
  (let ((debug-on-error t))
    (kill-all-local-variables)
    (setq major-mode 'esn-mode)
    (unless esn-mode-skip-ini
      (setq esn-mode-tried-ini 't)
      (message "Starting ESN mode")
      (let (
            (help-file (and (buffer-file-name) (string-match "nm\\(7\\|vi\\|v\\)[/\\]help[/\\]" (buffer-file-name)))))
        (unless help-file
          (esn-mode--ini)
          (use-local-map esn-mode-map)
          (esn-mode--keymap)
          (esn-mode--fontlock)
          (setq esn-completing-upcase 't)
          (esn-update-save-variables)
          (esn-mode--build-new)
          (esn-mode--hooks)
          (esn-mode--ini-functions))
        ;; Give auto-complete a chance to work.
        
        (when (and (featurep 'auto-complete)
                   (not (featurep 'esn-ac)))
          (require 'esn-ac))



        ;; Switch regular expressions to the current selected version
        (esn-switch-variables)
        ;;
        ;; Update Header information.
        ;;
        (if help-file
            (progn
              (setq mode-name "EsN(Help)")
              (toggle-read-only 1)
              )
          (add-to-list 'esn-buffers-list (current-buffer)))))    
    (when esn-start-hideshow-code-folding
      (esn-start-folding))
    (when (not esn-mode-echo-initially)
      (setq esn-mode-echo-initially 't)
      (esn-message "ESN-mode (%s) successfully loaded" esn-mode-ver))
    (when (featurep 'yasnippet)
      (esn-build-yas))
    (esn-company-start)
    (esn-cui-start)
    (esn-ac-start)
    (unless (boundp 'org-table-comment-before-edit-hook)
      (set (make-local-variable 'org-table-comment-before-edit-hook) nil))
    (unless (boundp 'org-table-comment-after-edit-hook)
      (set (make-local-variable 'org-table-comment-after-edit-hook) nil))
    (add-hook 'org-table-comment-before-edit-hook 'esn-orgtbl-comment-mode-start-hook)
    (add-hook 'org-table-comment-after-edit-hook 'esn-orgtbl-comment-mode-stop-hook)
    (run-mode-hooks 'esn-mode-hook)

    (setq esn-use-pirana-saved nil)
    (esn-set-mode-name)

    (message "Opened in ESN-mode (%s)" esn-mode-ver)
    ))

(with-temp-buffer
  (let (
        (case-fold-search 't)
        (i 0)
        )
    (while (< i (length esn-default-extension))
      (insert (format "(add-to-list 'auto-mode-alist '(\"\\\\%s\\\\'\" . esn-mode))"  (nth i esn-default-extension)))
      (setq i (+ i 1))))
  (eval-buffer))


(setq magic-mode-alist (cons '("[ \t\n]*\\$[Pp][Rr][Oo][A-Za-z]*[ \t]*" . esn-mode) magic-mode-alist))
(provide 'esn-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-mode.el ends here
