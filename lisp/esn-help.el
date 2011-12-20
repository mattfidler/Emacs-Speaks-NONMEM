;;; esn-help.el --- Esn-Help
;; 
;; Filename: esn-help.el
;; Description: Interfaces with NONMEM's html help
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Jul 27 14:16:15 2010 (-0500)
;; Version: 0.1
;; Last-Updated: Mon May  2 12:24:50 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 356
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
;; 04-Nov-2010      
;;    Last-Updated: Thu Nov  4 16:31:53 2010 (-0500) #251 (us041375)
;;    Made ? work the same as F1 (when enabled)
;; 04-Nov-2010      
;;    Last-Updated: Thu Nov  4 16:22:47 2010 (-0500) #248 (us041375)
;;    Wrapped tip.
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
(require 'layout-restore nil 't)
(require 'esn-vars)
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-get-current-rec "esn-narrow")


(defvar esn-help-link-keymap nil
  "The special keymap for Esn mode help-links")

(if esn-help-link-keymap
    ()
  (setq esn-help-link-keymap (make-sparse-keymap))
  (define-key esn-help-link-keymap [(mouse-1)] 'esn-follow-help)
  (define-key esn-help-link-keymap [(mouse-3)] 'esn-follow-help)
  (define-key esn-help-link-keymap (kbd "C-RET") 'esn-follow-help)
  (define-key esn-help-link-keymap (kbd "RET") 'esn-follow-help)
  ;;  (define-key esn-help-link-keymap [(mouse-3)] 'esn-pop-help-link-history)
  )

(defvar esn-overlay-category-help-record nil)
(defvar esn-help-font-lock-keywords nil)

(defvar esn-overlay-category-help-crossreference nil)

(unless esn-overlay-category-help-crossreference
  (put 'esn-overlay-category-help-crossreference 'local-map esn-help-link-keymap)
  (put 'esn-overlay-category-help-crossreference 'mouse-face 'highlight)
  (put 'esn-overlay-category-file 'face 'underline))

(unless esn-overlay-category-help-record
  (put 'esn-overlay-category-help-record 'local-map esn-help-link-keymap)
  (put 'esn-overlay-category-help-record 'mouse-face 'highlight)
  )



(defun esn-follow-help ()
  "Follows help link"
  (interactive)
  (block find-help-action 
    (mapc (lambda(over)
            (when (overlay-get over 'esn-help-record)
              (let ((lst (file-expand-wildcards "*.ctl"))
                    (cur-rec (esn-rec3 (buffer-substring-no-properties (overlay-start over) (overlay-end over)))))
                (mapc (lambda(ctl-name)
                        (when (string= cur-rec (esn-rec3 ctl-name))
                          (find-file ctl-name)
                          (return-from find-help-action)))
                      lst))))
          (overlays-at (point)))))

(defun esn-help-records-link (limit)
  "Links help records (and highlights them) with fontlock."
  (let ((ret (re-search-forward esn-exact-current-records-reg limit t))
        over)
    (when ret
      (setq over (make-overlay (match-beginning 0) (match-end 0)))
      (overlay-put over 'category 'esn-overlay-category-help-record)
      (overlay-put over 'esn-help-record t))
    (symbol-value 'ret)))

(defun esn-help-bold-sections-1 (limit)
  "Make Section labels bold."
  (let ((ret (re-search-forward "^[ \t]*\\([-A-Za-z][-A-Za-z ]*\\):[ \t]*$" limit t)))
    (add-text-properties (match-beginning 1) (match-end 1) '(face bold))
    (symbol-value 'ret)))

(defun esn-help-bold-sections-2 (limit)
  "Make Section labels bold."
  ;; Completely upper case.  Can a section or variable labels.
  (let ((ret (re-search-forward "^[ \t]*\\([-A-Z][-A-Z ]*\\)[ \t]*$" limit t)))
    (let ((curi (current-indentation))
          (set-bold t))
      (save-excursion
        (while (and (not (bobp))
                  (= curi (current-indentation)))
        (forward-line -1)
        (beginning-of-line))
        (when (looking-at ".*:[ \t]*$")
          (setq set-bold nil)))
      (when set-bold
        (add-text-properties (match-beginning 1) (match-end 1) '(face bold))))
    (symbol-value 'ret)))

(defun esn-help-bold-sections-3 (limit)
  "Make sections bold -- Assumes indent of one space is a bold section in NONMEM help."
  (let ((ret (re-search-forward "^ \\([-A-Za-z][-A-Za-z ]*\\)[ \t]*$" limit t)))
    (add-text-properties (match-beginning 1) (match-end 1) '(face bold))))

(defun esn-help-link-references (limit)
  "Links references")

(defalias 'e 'esn-nm-help-fontlock)
(defun esn-nm-help-fontlock ()
  "Builds fontlock and linking for Emacs Speaks NONMEM help"
  (interactive)
  (when (eq major-mode 'esn-mode)
    (font-lock-mode -1)
    (set (make-local-variable 'esn-assumed-version) "1000000")
    (esn-switch-variables)
    (set (make-local-variable 'esn-help-font-lock-keywords)
         `(
           (esn-help-bold-sections-1)
           (esn-help-bold-sections-2)
           (esn-help-bold-sections-3)
           (esn-help-link-references
            (0 font-lock-function-name-face))
            (esn-help-records-link
             (0 font-lock-function-name-face))))
    (set (make-local-variable 'font-lock-defaults)
         '(esn-help-font-lock-keywords t))
    (when (and (boundp 'font-lock-mode) font-lock-mode)
      (font-lock-mode -1))
    ;; Turn of spell-check fontifying.
    (when (and (boundp 'speck-mode) speck-mode)
      (speck-mode -1))
    (font-lock-mode 1)))


(defun esn-nm-help ()
  "* NONMEM help button."
  (interactive)
  (let (
        help
        (rec (esn-get-current-record))
        opt
        eq
        (lst3 '())
        (lst2 '())
        (lst '())
        tip
        (pt (point))
        )
    (save-excursion
      (re-search-forward "\\=[A-Z0-9_]*" nil t)
      (skip-chars-forward " \t=")
      (skip-chars-backward " \t")
      (when (looking-back "\\<[$A-Z0-9_]*[ \t]*=?")
        (setq help (match-string 0))
        (while (string-match "[ \t]+" help)
          (setq help (replace-match "" nil nil help)))
        (cond
         ( (progn ;; Records help.
             (if (not (string-match "^[$]" help))
                 nil
               (setq lst (all-completions (concat "$" (esn-rec3 help)) esn-current-records-complete))
               (mapc (lambda(x) (add-to-list 'lst2 (esn-rec3 x))) lst)
               (= 1 (length lst2))
               )
             )
           (setq tip (esn-complete-help (nth 0 lst)))
           )
         ( (progn ;; Option values
             (setq lst (assoc rec esn-current-record-complete-option-values))
             (if (not lst)
                 nil
               (setq lst (nth 1 lst))
               (if (not (looking-back "\\<\\([A-Z0-9_a-z]*\\)[ \t]*[ \t=][ \t]*[A-Za-z0-9_._]+"))
                   nil
                 (setq opt (concat (esn-rec3 (match-string 1) 't) "="))
                 (setq lst (assoc opt lst))
                 (if (not lst)
                     nil
                   ;; Found an option
                   (setq lst (nth 1 lst))
                   (setq lst (all-completions (esn-rec3 help) lst))
                   (= 1 (length lst))
                   )
                 )
               )
             )
           (setq tip (esn-complete-help (nth 0 lst)))
           )
         ( (progn ;; Options help
             (setq lst (assoc rec esn-current-record-complete-options))
             (if (not lst)
                 nil
               (setq lst (nth 1 lst))
               (setq eq (string-match "=" help))
               (setq opt (esn-rec3 help 't))
               (setq lst (all-completions opt lst))
               (setq lst2 '())
               (setq lst3 '())
               (mapc (lambda(x) (when (or (not eq) (string-match "=" x)) 
                                  (add-to-list 'lst2 (esn-rec3 x 't))
                                  (if eq
                                      (add-to-list 'lst3 x)
                                    (unless (string-match "=" x)
                                      (add-to-list 'lst3 x)
                                      )
                                    )
                                  )
                       ) lst)
               (= 1 (length lst2))
               )
             )
           (setq tip (esn-complete-help (nth 0 lst3)))
           )
         )
        )
      )
    (when tip
      (with-temp-buffer
        (insert tip)
        (goto-char (point-min))
        (goto-char (point-at-eol))
        (insert "\n")
        (forward-line 1)
        (let ((fill-column esn-help-wrap-to))
          (fill-paragraph )
          )
        (setq tip (buffer-substring-no-properties (point-min) (point-max)))
        )
      (cond
       ( (and (interactive-p) (memq last-command '(esn-nm-help esn-magic-?)))
         (if (get-buffer " *EsN Help*")
             (progn
               (kill-buffer " *EsN Help*")
               (if (featurep 'layout-restore)
                   (layout-restore)
                 (delete-other-windows)
                 )
               (message "Open HTML help about this item.")
               )
           ;; Create Buffer.
           (when (featurep 'layout-restore)
             (layout-save-current))
           (with-current-buffer (get-buffer-create " *EsN Help*")
             (erase-buffer)
             (insert tip)
             (goto-char (point-min))
             (display-buffer (current-buffer)))
           )
         )
       ( (featurep 'pos-tip)
         (goto-char pt)
         (pos-tip-show tip)
         )
       ( (featurep 'popup)
         (goto-char pt)
         (popup-tip tip)
         )
       )
      )
    )
  )
(defun esn-complete-meta (complete)
  "* Defines a short documentation string for the option"
  (let (
        (rec (esn-get-current-record))
        (opt (esn-rec3 complete))
        tmp
        (ret "")
        )
    (cond
     ( (string-match (eval-when-compile (esn-reg-records)) complete)
       (setq tmp (assoc complete esn-current-nm-help-records))
       (when tmp
         (setq ret (nth 1 tmp))
         )
       )
     ( 't
       
       )
     )
    (symbol-value 'ret)
    )
  )

(defun esn-complete-help (complete)
  "* Defines a help completion"
  (let (
        (rec (esn-get-current-rec))
        tmp
        (ret "")
        (lst (esn-current-record-options-help))
        )
    (cond
     ((progn
        (setq tmp (assoc complete esn-current-nm-help-records-discussion))
        (symbol-value 'tmp)
        )
      (setq ret (nth 1 tmp))
      )
     (
      (progn
        (setq tmp (assoc complete lst))
        (symbol-value 'tmp)
        )
      (if (typep (nth 1 tmp) 'string)
          (setq ret (nth 1 tmp))
        (setq ret (concat complete " option, multiple possible values"))
        )
      )
     ( (and 
        (looking-back "\\<\\([A-Z0-9_a-z]*\\)[ \t]*[ \t=][ \t]*[A-Z0-9_.]+")
        (progn
          (setq tmp (all-completions (esn-rec3 (match-string 1)) lst))
          (symbol-value 'tmp)
          )
        )
       (cond
        ( (= 1 (length tmp))       
          (setq ret (format "%s" (nth 1 (assoc complete (nth 1 (assoc (nth 0 tmp) lst))))))
          )
        ( 't
          (setq tmp nil)
          )
        )
       )
     ('t 
      )
     )
    (symbol-value 'ret)
    )
  )
(provide 'esn-help)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-help.el ends here
