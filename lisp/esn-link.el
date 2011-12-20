;;;; esn-link.el --- Hyperlink Functions for Emacs Speaks NONMEM
;;
;; Filename: esn-link.el
;; Description: Hyperlink functions for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Feb  1 10:47:52 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Mon May  2 12:34:03 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 246
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Hyperlink functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 21-Dec-2010      
;;    Last-Updated: Tue Dec 21 08:51:08 2010 (-0600) #230 (US041375)
;;    First attempt at linking PLT tools MSFO files.
;; 11-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 11 07:56:33 2010 (-0600) #174 (Matthew L. Fidler)
;;    Changed linking mechanism to use font-lock (should be faster?)
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 16:07:43 2010 (-0600) #129 (Matthew L. Fidler)
;;    Changed keybinding to follow links to Control-return AND right clicking.
;; 09-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  9 13:27:23 2010 (-0500) #121 (Matthew L. Fidler)
;;    Bugfix linking of $DATA files.  Also included linking of $INCLUDE files.
;; 23-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 23 14:52:01 2010 (-0500) #109 (Matthew L. Fidler)
;;    Bugfix for esn-link map
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 11:13:30 2010 (-0500) #104 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 15-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Jun 15 16:57:10 2010 (-0500) #100 (Matthew L. Fidler)
;;    Linked Data of PLT archive to RAW Data from PLT archives.
;; 15-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Jun 15 16:31:41 2010 (-0500) #85 (Matthew L. Fidler)
;;    Added correct linking to AllRecords and FirstRecords etc for PLT tools archive. 
;; 05-May-2010    Matthew L. Fidler  
;;    Last-Updated: Wed May  5 14:50:20 2010 (-0500) #71 (Matthew L. Fidler)
;;    Only links when exists OR a special PLT tools file like AllRecords.txt
;; 22-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Thu Apr 22 12:05:32 2010 (-0500) #24 (Matthew L. Fidler)
;;    Added EsN Error handling
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 22:36:41 2010 (-0600) #14 (Matthew L. Fidler)
;;    Added Link overlays for WINDOW ONLY (should save some time...)
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 10:51:19 2010 (-0600) #3 (Matthew L. Fidler)
;;    Removed MSFO=msfo.outputfile links for now.  Possibly make a version
;;    based on the last run-id...?
;; 01-Feb-2010    Matthew L. Fidler
;;    Added Header.
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

(declare-function esn-plt-find-timestamps "esn-plt")
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-error "esn-exec")

(require 'esn-start)
(defvar esn-link-keymap nil
  "The special keymap for Esn mode links")
(defvar esn-overlay-category-file nil)  
(defvar esn-make-link-overlays-timer nil
  "* Timer for Link overlays")

(defvar esn-link-keymap nil
  "The special keymap for Esn mode links")
(defvar esn-overlay-category-file nil)  
(defvar esn-make-link-overlays-timer nil
  "* Timer for Link overlays")

(if esn-link-keymap
    ()
  (setq esn-link-keymap (make-sparse-keymap))
  (define-key esn-link-keymap [(mouse-1)] 'esn-follow-link)
  ;(define-key esn-link-keymap [(mouse-3)] 'esn-follow-link)
  ;(define-key esn-link-keymap (kbd "C-RET") 'esn-follow-link)
                                        ;;  (define-key esn-link-keymap [(mouse-3)] 'esn-pop-link-history)
  ) 

(unless esn-overlay-category-file
  (put 'esn-overlay-category-file 'esn-link t)
  (put 'esn-overlay-category-file 'local-map esn-link-keymap)
  (put 'esn-overlay-category-file 'mouse-face 'highlight)
  (put 'esn-overlay-category-file 'face 'underline))

(defun esn-rm-link-overlays()
  "* Removes the link overlays"
  (interactive)
  (let (
        (olist
         (overlays-in (point-min) (point-max))))
    (mapc (lambda(x)
            (when (overlay-get x 'esn-link)
              (delete-overlay x)))
          olist)))

;;(defalias 'e 'esn-make-link-msfo)
(defalias 'e 'esn-make-link-msfo-2)
;;(defalias 'e 'esn-make-link-include)
;;(defalias 'e 'esn-make-link-include-2)
;;(defalias 'e 'esn-make-link-ref)
;;(defalias 'e 'esn-make-link-ref-2)
;;(defalias 'e 'esn-make-link-outfiles)
;;(defalias 'e 'esn-make-link-outfiles-2)
;;(defalias 'e 'esn-make-link-data-include)

(defun esn-make-link-msfo (limit)
  "Links MFSO files using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-msfo) nil
          (let ((ret (re-search-forward (eval-when-compile (format "\\<MSFO?[ \t=]?%s" esn-reg-filename-2)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (if (not (and esn-plt-use-msfo-outputfile (esn-use-plt-p)))
                  ;; Get the last MSFO output
                  (progn
                    (setq over (make-overlay (match-beginning 2) (match-end 2)))
                    (overlay-put over 'category 'esn-overlay-category-file)
                    (overlay-put over 'esn-msfo (match-string 2))
                    (overlay-put over 'esn-link "")
                    (overlay-put over 'esn-ref "")
                    )
                (when (string= "msfo.outputfile" (match-string 2))
                  (let ((timestamps (save-match-data (esn-plt-find-timestamps))))
                    (when (and (> (length timestamps) 0)
                               (file-exists-p (concat "../MISCELLANEOUS/MSFO/msfo." (nth 0 timestamps))))
                      (setq over (make-overlay (match-beginning 2) (match-end 2)))
                      (overlay-put over 'category 'esn-overlay-category-file)
                      (overlay-put over 'esn-msfo (concat "../MISCELLANEOUS/MSFO/msfo." (nth 0 timestamps)))
                      (overlay-put over 'esn-link "")
                      (overlay-put over 'esn-ref ""))))))
            (symbol-value 'ret))))
        (error
         (message "Error when linking MSFO files (MSFO='' form): %s" (error-message-string error))
         nil)))

(defun esn-make-link-msfo-2 (limit)
  "Links MFSO files using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-msfo) nil
          (let ((ret (re-search-forward (eval-when-compile (format "\\<MSFO?[ \t=]?%s" esn-reg-filename-3)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo (match-string 2))
              (overlay-put over 'esn-link "")
              (overlay-put over 'esn-ref ""))
            (when (and ret (match-string 2) (string= "msfo.outputfile" (match-string 2)))
              (let ((timestamps (save-match-data (esn-plt-find-timestamps))))
                (when (and (> (length timestamps) 0)
                           (file-exists-p (concat "../MISCELLANEOUS/MSFO/msfo." (nth 0 timestamps))))
                  (setq over (make-overlay (match-beginning 2) (match-end 2)))
                  (overlay-put over 'category 'esn-overlay-category-file)
                  (overlay-put over 'esn-msfo (concat "../MISCELLANEOUS/MSFO/msfo." (nth 0 timestamps)))
                  (overlay-put over 'esn-link "")
                  (overlay-put over 'esn-ref ""))))
            (symbol-value 'ret))))
    (error
     (message "Error when linking MSFO files (MSFO=file form): %s" (error-message-string error))
     nil)))

(defun esn-make-link-include (limit)
  "Links INCLUDE files using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-include-files) nil
          (let ((ret (re-search-forward (eval-when-compile (format ";+C?[ \t]*include[ \t]*[:=][ \t]*%s" esn-reg-filename-2)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link (match-string 2))
              (overlay-put over 'esn-ref ""))
            (symbol-value 'ret))))
        (error
         (message "Error when linking include statements (quote): %s" (error-message-string error))
         nil)))

(defun esn-make-link-include-2 (limit)
  "Links INCLUDE files using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-include-files) nil
          (let ((ret (re-search-forward (eval-when-compile (format ";+C?[ \t]*include[ \t]*[:=][ \t]*%s" esn-reg-filename-3)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link (match-string 2))
              (overlay-put over 'esn-ref ""))
            (symbol-value 'ret))))
    (error
     (message "Error when linking include statements: %s" (error-message-string error))
     nil)))

(defun esn-make-link-ref (limit)
  "Link reference models using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-ref-models) nil
          (let ((ret (re-search-forward (eval-when-compile (format ";+C?[ \t]*\(?:[Rr]ef\|Par\)[^=:]*?[:=][ \t]*%s" esn-reg-filename-2)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link "")
              (overlay-put over 'esn-ref (match-string 2)))
            (symbol-value 'ret))))
    (error
     (message "Error when linking Reference model and parent model statements (quote): %s" (error-message-string error))
     nil)))

(defun esn-make-link-ref-2 (limit)
  "Link reference models using font-lock"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-ref-models) nil
          (let ((ret (re-search-forward (eval-when-compile (format ";+C?[ \t]*\(?:[Rr]ef\|Par\)[^=:]*?[:=][ \t]*%s" esn-reg-filename-3)) limit t))
                over)
            (when (and ret (esn-file-exists-p (match-string 2)))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link "")
              (overlay-put over 'esn-ref (match-string 2)))
            (symbol-value 'ret))))
    (error
     (message "Error when linking Reference model and parent model statements: %s" (error-message-string error))
     nil)))

(defun esn-make-link-outfiles (limit)
  "Link Output files through font-lock."
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-outfiles) nil
          (let ((ret (re-search-forward (eval-when-compile (format
                                                            "\\<\\(?:FILE?[= \t]+\\|OPEN *( *[0-9]+ *, *FILE *= *\\)%s" esn-reg-filename-2)) limit t))
                over)
            (when (and ret (or
                            (esn-file-exists-p (match-string 2))
                            (save-match-data
                              (string-match (eval-when-compile
                                              (regexp-opt
                                               '(
                                                 "AllRecords"
                                                 "FirstRecords"
                                                 "ExtraRecords"
                                                 "cwtab.est"
                                                 "cwtab.deriv"
                                                 ) 't)) (match-string 2)))))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link (match-string 2))
              (overlay-put over 'esn-ref ""))
            (symbol-value 'ret))))
  (error
   (message "Error when linking FILE statements: %s" (error-message-string error))
   nil)))

(defun esn-make-link-outfiles-2 (limit)
  "Link Output files through font-lock."
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (if (not esn-link-outfiles) nil
          (let ((ret (re-search-forward (eval-when-compile (format
                                                            "\\<\\(?:FILE?[= \t]+\\|OPEN *( *[0-9]+ *, *FILE *= *\\)%s" esn-reg-filename-3)) limit t))
                over)
            (when (and ret (or
                            (esn-file-exists-p (match-string 2))
                            (save-match-data
                              (string-match (eval-when-compile
                                              (regexp-opt
                                               '(
                                                 "AllRecords"
                                                 "FirstRecords"
                                                 "ExtraRecords"
                                                 "cwtab.est"
                                                 "cwtab.deriv"
                                                 ) 't)) (match-string 2)))))
              (setq over (make-overlay (match-beginning 2) (match-end 2)))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link (match-string 2))
              (overlay-put over 'esn-ref ""))
            (symbol-value 'ret))))
    (error
     (message "Error when linking FILE statments: %s" (error-message-string error))
     nil)))

(defun esn-make-link-data-include (limit)
  "Link Data and include files"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (let ((ret (re-search-forward (eval-when-compile (esn-reg-data-rec '("DAT"
                                                                             "INC"
                                                                             ))) limit t))
              over)
          (when (and ret (esn-file-exists-p
                          (if (esn-use-plt-archive-p)
                              (save-match-data
                                (let ((data (buffer-file-name)))
                                  (when (string-match "Control[.]" data)
                                    (setq data (replace-match "RawData." 't 't data)))
                                  (when (string-match "CONTROL" data)
                                    (setq data (replace-match "RAWDATA" 't 't data)))
                                  (when (string-match "TEXTFILES" data)
                                    (setq data (replace-match "TABLES" 't 't data)))
                                  data))
                            (match-string 2))))
            (setq over (make-overlay (match-beginning 2) (match-end 2)))
            (overlay-put over 'category 'esn-overlay-category-file)
            (overlay-put over 'esn-msfo "")
            (overlay-put over 'esn-link (if (esn-use-plt-archive-p)
                                            (save-match-data
                                              (let (
                                                    (data (buffer-file-name)))
                                                (when (string-match "Control[.]" data)
                                                  (setq data (replace-match "RawData." 't 't data)))
                                                (when (string-match "CONTROL" data)
                                                  (setq data (replace-match "RAWDATA" 't 't data)))
                                                (when (string-match "TEXTFILES" data)
                                                  (setq data (replace-match "TABLES" 't 't data)))
                                                (message "%s" data)
                                                (symbol-value 'data)))
                                          (match-string 2)))
            (overlay-put over 'esn-ref ""))
          ret))
  (error
   (message "Error when linking $DATA and $INCLUDE records: %s" (error-message-string error))
   nil)))

;;;###autoload
(defun esn-make-link-overlays ()
  "* Creates an idle timer to create link overlays"
  (when esn-make-link-overlays-timer
    (cancel-timer esn-make-link-overlays-timer))
  (setq esn-make-link-overlays-timer
        (run-with-timer 0.25 nil 'esn-make-link-overlays-actual)))

(defun esn-make-link-overlays-actual ()
  "* Creates the link overlays"
  (interactive)
  (save-restriction
    (save-excursion
      (let (
            (case-fold-search 't)
            (window-stop (save-excursion
                           (goto-char (window-start))
                           (forward-line (window-height))
                           (end-of-line)
                           (point)))
            over
            beg end
            )
        (esn-rm-link-overlays)
        (goto-char (window-start))
        ;; Link to files
        (when (and esn-link-msfo (not (and esn-plt-use-msfo-outputfile (esn-use-plt-p))))
          (while (re-search-forward "[ \n]MSFO? *= *\\([^ \n\t]*\\)" window-stop t)
            (unless (esn-in-comment-p)
              (setq end (point))
              (setq beg (save-excursion
                          (backward-char (length (match-string 1)))
                          (point)))
              (when (esn-file-exists-p (match-string 1))
                (setq over (make-overlay beg end))
                (overlay-put over 'category 'esn-overlay-category-file)
                (overlay-put over 'esn-msfo (match-string 1))
                (overlay-put over 'esn-link "")
                (overlay-put over 'esn-ref "")))))
        (when esn-link-include-files
          (goto-char (window-start))
          (when (re-search-forward ";[ \t]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][\t]*[:=]" window-stop t)
            (save-restriction
              (narrow-to-region (point) (save-excursion (end-of-line) (point)))
              (while (re-search-forward "[^ \t,:=;]+" window-stop t)
                (setq end (point))
                (setq beg (save-excursion
                            (backward-char (length (match-string 0)))
                            (point)))
                (when (esn-file-exists-p (match-string 0))
                  (setq over (make-overlay beg end))
                  (overlay-put over 'category 'esn-overlay-category-file)
                  (overlay-put over 'esn-link (match-string 0))
                  (overlay-put over 'esn-msfo "")
                  (overlay-put over 'esn-ref ""))))))
        (when esn-link-ref-models
          (goto-char (window-start))
          (when (re-search-forward ";+C?[ \t]*\\(?:[Rr]ef\\|Par\\)[^=:]*?[=:][ \t]*\\([^ \t\n]*\\)" window-stop t)
            (setq end (point))
            (setq beg (save-excursion
                        (backward-char (length (match-string 1)))
                        (point)))
            (when (esn-file-exists-p (match-string 1))
              (setq over (make-overlay beg end))
              (overlay-put over 'category 'esn-overlay-category-file)
              (overlay-put over 'esn-msfo "")
              (overlay-put over 'esn-link "")
              (overlay-put over 'esn-ref (match-string 1)))))
        (when esn-link-outfiles
          (goto-char (window-start))
          (unless (esn-in-comment-p)
            (while (re-search-forward "[ \n]\\(?:FILE? *[= \t] *\\|OPEN *( *[0-9]+ *, *FILE *= *\\)\\([^ \n\t]*\\)" window-stop t)
              (setq end (point))
              (setq beg (save-excursion
                          (backward-char (length (match-string 1)))
                          (point)))
              (when (or 
                     (esn-file-exists-p (match-string 1))
                     (save-match-data
                       (string-match (eval-when-compile
                                       (regexp-opt
                                        '(
                                          "AllRecords"
                                          "FirstRecords"
                                          "ExtraRecords"
                                          "cwtab.est"
                                          "cwtab.deriv"
                                          ) 't)) (match-string 1))))
                (setq over (make-overlay beg end))
                (overlay-put over 'category 'esn-overlay-category-file)
                (overlay-put over 'esn-link (match-string 1))
                (overlay-put over 'esn-msfo "")
                (overlay-put over 'esn-ref "")))))
        (goto-char (window-start))
        (while (re-search-forward (eval-when-compile (esn-reg-data-rec '("DAT"
                                                                         "INC"
                                                                         )))
                                  window-stop t)
          (unless (esn-in-comment-p)
            (let* (
                   (end (point))
                   beg
                   over
                   )
              (setq beg (match-beginning 2))
              (setq end (match-end 2))
              (when (esn-file-exists-p (if (esn-use-plt-archive-p)
                                           (save-match-data
                                             (let (
                                                   (data (buffer-file-name)))
                                               (when (string-match "Control[.]" data)
                                                 (setq data (replace-match "RawData." 't 't data)))
                                               (when (string-match "CONTROL" data)
                                                 (setq data (replace-match "RAWDATA" 't 't data)))
                                               (when (string-match "TEXTFILES" data)
                                                 (setq data (replace-match "TABLES" 't 't data)))
                                               data
                                               
                                               ))
                                         (match-string 2)))
                (setq over (make-overlay beg end))
                (overlay-put over 'esn-link (if (esn-use-plt-archive-p)
                                                (save-match-data
                                                  (let (
                                                        (data (buffer-file-name)))
                                                    (when (string-match "Control[.]" data)
                                                      (setq data (replace-match "RawData." 't 't data)))
                                                    (when (string-match "CONTROL" data)
                                                      (setq data (replace-match "RAWDATA" 't 't data)))
                                                    (when (string-match "TEXTFILES" data)
                                                      (setq data (replace-match "TABLES" 't 't data)))
                                                    (message "%s" data)
                                                    (symbol-value 'data)))
                                              (match-string 2)))
                (overlay-put over 'category 'esn-overlay-category-file)
                (overlay-put over 'esn-msfo "")
                (overlay-put over 'esn-ref "")))))))))

(defun esn-follow-link ()
  "* Function called on Links"
  (interactive)
  (let* (
         (event last-command-event)
         pos
         olist
         file
         )
    (cond ((eq (car event) 'mouse-1)
           (setq pos (nth 1 (cadr event)))))
    (when pos
      (setq olist (overlays-at pos))
      (unless (string= "nil" (format "%s" (car olist)))
        (setq file (overlay-get (car olist) 'esn-link))
        (if (not (string= file ""))
            (if (not (and (or 
                           (esn-use-plt-p)
                           (esn-use-plt-archive-p))
                          (string-match "\\(AllRecords\\|FirstRecords\\|ExtraRecords\\|cwtab.est\\|cwtab.deriv\\)" file)))
                (progn
                  (setq file (esn-get-abs-dir file))
                  (when file
                    (setq file (expand-file-name file))
                    (when esn-w32
                      (while (string-match "/" file)
                        (setq file (replace-match "\\" 't 't file)))))
                  (if (and esn-w32 (fboundp 'w32-browser))
                      (w32-browser (esn-get-abs-dir file))
                    (if (and esn-w32 (fboundp 'mswindows-shell-execute))
                        (mswindows-shell-execute (esn-get-abs-dir file))
                      (find-file (esn-get-abs-dir file)))))
              (let (
                    (timestamps (if (esn-use-plt-archive-p)
                                    (let (
                                          (buf (buffer-file-name)))
                                      (if (string-match "[0-9]\\{6\\}-[0-9]\\{6\\}" (buffer-file-name))
                                          (list
                                           (match-string 0 (buffer-file-name)))
                                        (list )))
                                  (esn-plt-find-timestamps))))
                (if (= (length timestamps) 0)
                    (message "Could not find run.  Need to submit, or make the grep command accessible to emacs")
                  (cond
                   ( (string-match "AllRecords" file)
                     (setq file (concat (if (esn-use-plt-archive-p) "../" "") 
                                        "../TABLES/ALLRECORDS-PROCESSED/AllRecords." (nth 0 timestamps) ".csv")))
                   ( (string-match "FirstRecords" file)
                     (setq file (concat (if (esn-use-plt-archive-p) "../" "") 
                                        "../TABLES/FIRSTRECORDS-PROCESSED/FirstRecords." (nth 0 timestamps) ".csv")))
                   ( (string-match "ExtraRecords" file)
                     (setq file (concat (if (esn-use-plt-archive-p) "../" "") 
                                        "../TABLES/EXTRARECORDS-PROCESSED/ExtraRecords." (nth 0 timestamps) ".csv")))
                   ( (string-match "cwtab.est" file)
                     (setq file (concat (if (esn-use-plt-archive-p) "../" "") 
                                        "../MISCELLANEOUS/CWRESIDUALS-ESTIMATES/CWRES.est." (nth 0 timestamps) ".txt")))
                   ( (string-match "cwtab.deriv" file)
                     (setq file (concat (if (esn-use-plt-archive-p) "../" "") 
                                        "../TABLES/CWDERIV-PROCESSED/CWRES.deriv." (nth 0 timestamps) ".csv")))
                   ( 't
                     (setq file nil)))
                  (when file
                    (setq file (expand-file-name file))
                    (when esn-w32
                      (while (string-match "/" file)
                        (setq file (replace-match "\\" 't 't file))))
                    (if (and esn-w32 (fboundp 'w32-browser))
                        (if (string-match "CWRES.*txt$" file)
                            (find-file file)
                          (w32-browser file))
                      (if (and esn-w32 (fboundp 'mswindows-shell-execute))
                          (if (string-match "CWRES.*txt$" file)
                              (find-file file)
                            (mswindows-shell-execute file)))
                      (find-file file)))))))
        ;; Now other types.
        (setq file (overlay-get (car olist) 'esn-msfo))
        (unless (string= "" file)
          (let (
                (buf (buffer-file-name))
                (case-fold-search 't)
                (inhibit-read-only 't)
                (inhibit-point-motion-hooks 't)
                (ret (buffer-substring (point-min) (point-max))))
            (get-buffer-create (format "*MSFI from %s *" buf))
            (switch-to-buffer-other-window (format "*MSFI from %s *" buf))
            (delete-region (point-min) (point-max))
            (insert ret)
            ;; Remove Records
            (goto-char (point-min))
            (save-excursion
              (save-restriction
                ;; Remove $COVARIANCE $TABLE, $THETA, $SIGMA, and $OMEGA records.
                (while (re-search-forward (eval-when-compile
                                            (esn-reg-record-exp '( "TAB"
                                                                   "THE"
                                                                   "SIG"
                                                                   "OME"
                                                                   "COV"
                                                                   "EST"
                                                                   ) nil 't)) nil t)
                  (when (esn-narrow-rec)
                    (delete-region (point-min) (point-max))
                    (widen)))
                (goto-char (point-min))
                (while (re-search-forward ";| Pred Variables.*?;" nil t)
                  (replace-match ""))
                (goto-char (point-min))
                (while (re-search-forward "\n\n\n+" nil t)
                  (replace-match "\n\n"))
                (goto-char (point-max))
                (insert "$MSFI ")
                (insert file)
                (insert "\n")))))
        (setq file (overlay-get (car olist) 'esn-ref))
        (unless (or (not file) (string= "" file))
          (if (and file (file-exists-p file))
              (find-file-other-window file)
            (let (
                  (found nil))
              (mapc (lambda(x)
                      (when (and (not found)
                                 (file-exists-p (concat file x)))
                        (find-file-other-window (concat file x))
                        (setq found 't)))
                    (append
                     esn-default-extension
                     (list
                      ".txt")))
              (when (not found)
                (esn-error "Could not find the reference model %s" file)))))))))
(provide 'esn-link)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-link.el ends here
