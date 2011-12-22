;;; esn-input.el --- Esn Input Completion Options and routines.
;;
;; Filename: esn-input.el
;; Description: Esn Input completion options and routines.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Jan 27 14:25:25 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Thu Dec 22 17:11:39 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 346
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Input routines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 22-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Dec 22 16:46:52 2011 (-0600) #341 (Matthew L. Fidler)
;;    Changed Input description to an org-style comment table.  Use
;;    orgtbl mode for better editing.
;; 21-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Dec 21 09:32:06 2011 (-0600) #324 (Matthew L. Fidler)
;;    Bug fix for input when using a cygwin head command.
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 14:34:37 2010 (-0600) #307 (Matthew L. Fidler)
;;    Cygwin head.exe support bug fix.
;; 01-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Wed Dec  1 11:04:34 2010 (-0600) #300 (Matthew L. Fidler)
;;    Bug fixes for header.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 12:17:14 2010 (-0500) #256 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 11:10:11 2010 (-0500) #254 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 11:29:55 2010 (-0500) #36 (Matthew L. Fidler)
;;    Added support for CSV files with headers on the second line (to allow for comments)
;; 22-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Thu Apr 22 12:05:26 2010 (-0500) #21 (Matthew L. Fidler)
;;    Added Esn error handling
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan27 14:26:10 2010 (-0600) #1 (Matthew L. Fidler)
;;
;;    Changed errors in $INPUT auto-completion to messages.  Sometimes this is
;;    called outside of an interactive function, and these errors should be
;;    ignored.
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

(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(declare-function esn-header-regexp-quote "esn-update")
(declare-function esn-wrap "esn-magic-keys")
(declare-function esn-message "esn-exec")
(declare-function esn-rec "esn-properties")
(declare-function esn-error "esn-exec")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-magic-wrap "esn-magic-keys")

(require 'esn-start)
;;;###autoload

(defvar esn-org-table-comment-swap nil)


(defun esn-orgtbl-comment-mode-start-hook ()
  "A hook to run before a commented orgtbl is edited"
  (when (eq major-mode 'esn-mode)
    (esn-cancel-all-timers)
    (setq esn-org-table-comment-swap esn-wrapping-of-records)
    (setq esn-wrapping-of-records)
    (setq esn-run-save-fn 't)))

;;;###autoload
(defun esn-orgtbl-comment-mode-stop-hook ()
  "A hook to run after a comment orgtbl is edited"
  (when (eq major-mode 'esn-mode)
    (setq esn-org-table-comment-swap esn-wrapping-of-records)
    (setq esn-run-save-fn nil)
    (setq esn-get-current-record-start nil)
    (setq esn-yas-last-theta nil)))


(defun esn-add-drop-if-appropriate (header)
  "* Define =DROP,DV=,TIME= rules"
  (let (
        (comp (split-string header))
        (known-vars (format "^%s$" (regexp-opt (append
                                                esn-xpose-cotab
                                                esn-xpose-catab
                                                esn-xpose-mutab
                                                esn-xpose-extra
                                                esn-xpose-sdtab
                                                esn-xpose-mytab
                                                esn-xpose-xptab
                                                esn-xpose-sim-cotab
                                                esn-xpose-sim-catab
                                                esn-xpose-sim-mutab
                                                esn-xpose-sim-extra
                                                esn-xpose-sim-sdtab
                                                esn-xpose-sim-mytab
                                                esn-xpose-sim-xptab
                                                esn-mode-dv-vars
                                                esn-mode-time-vars
                                                esn-mode-reserved-vars
                                                ) 't)))
        (dv-vars (format "^%s$" (regexp-opt esn-mode-dv-vars 't)))
        (time-vars (format "^%s$" (regexp-opt esn-mode-time-vars 't)))
        (date-vars (format "^%s$" (regexp-opt esn-mode-date-vars 't)))
        (dropped 0)
        (lp 0)
        (dv nil)
        (time nil)
        (max-num (if (>= (string-to-number (esn-update-get-version)) 7)
                     50
                   20))
        ret)
    (setq lp (length comp))
    ;; This is where the =DROP rules are instituted.
    (setq ret (mapconcat 
               (lambda(x)
                 (let ((ret x))
                   (unless (string-match known-vars x)
                     (when (< max-num lp)
                       (setq ret (concat x "=DROP"))
                       (setq dropped (+ dropped 1))))
                   (when (string-match dv-vars x)
                     (if dv
                         (progn
                           (setq ret (concat x "=DROP"))
                           (setq dropped (+ dropped 1)))
                       (setq ret (concat "DV=" x))
                       (setq dv 't)))
                   (when (string-match date-vars x)
                     (setq ret (concat x "=DROP")))
                   (when (string-match time-vars x)
                     (if time
                         (progn
                           (setq ret (concat x "=DROP"))
                           (setq dropped (+ dropped 1)))
                       (setq ret (concat "TIME=" x))
                       (setq time 't)))
                   (symbol-value 'ret)))
               comp " "))
    (while (string-match ";=DROP ->=DROP \\([^=]*\\)=DROP" ret)
      (setq ret (replace-match ";->\\1\n" nil nil ret)))
    (setq esn-input-number-of-vars (- lp dropped))
    (symbol-value 'ret)))

(defun esn-input-before-data ()
  "* Puts the data statment right after the input statement."
  (save-restriction
    (let (
          (esn-dat "")
          (dmn nil)
          (dmx nil)
          (case-fold-search 't)
          (inhibit-read-only 't)
          (inhibit-point-motion-hooks 't))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile (esn-reg-record-exp "INP")) nil t)
          (end-of-line)
          (esn-narrow-rec)
          (setq dmn (point-min))
          (setq dmx (point-max))
          (widen)
          (goto-char dmn)
          (while (looking-at (eval-when-compile (format "^\\(%s\\| *;[|C]\\)" (esn-reg-record-exp "INP"))))
            (forward-line -1)
            (beginning-of-line))
          (end-of-line)
          (setq dmn (point))
          (goto-char dmx)
          (skip-chars-backward " \t\n")
          (setq dmx (point))
          (esn-narrow-to-current-problem)
          (save-excursion
            (when (re-search-backward (eval-when-compile (esn-reg-record-exp "DAT")) nil t)
              (while (looking-at (eval-when-compile (format "^\\(%s\\| *;[|C]\\)" (esn-reg-record-exp "DAT"))))
                (forward-line -1)
                (beginning-of-line))
              (beginning-of-line)
              (re-search-forward (esn-header-regexp-quote) nil t)
              (end-of-line)
              (setq esn-dat (buffer-substring dmn dmx))
              (delete-region dmn dmx)
              (insert esn-dat)
              (esn-wrap)))
          (widen)
          (goto-char dmx))))))

(defun esn-add-ignore-data (header)
  "* Function that adds IGNORE= to the data file when not present."
  (save-restriction
    (let (
          (ignore header)
          (case-fold-search 't))
      (when (string-match "^ *\"?\\([A-Za-z]\\)" ignore)
        (setq ignore (concat " IGNORE=" (match-string 1 header)))
        (save-excursion
          (esn-narrow-to-current-problem)
          (goto-char (point-min))
          (when (re-search-forward (eval-when-compile (esn-reg-record-exp "DAT")) nil t)
            (esn-narrow-rec)
            (goto-char (point-min))
            (if (re-search-forward "[ \n]*IGNO\\(?:RE?\\)? *= *\\(['\"]?\\)[A-Za-z]\\1" nil t)
                (replace-match ignore)
              (goto-char (point-max))
              (insert ignore))
            (widen))
          (widen))))))

(defun esn-input-get-header (data &optional no-message)
  "This gets the header of a CSV file.  First try to use head since it takes up less memory then loading the whole file."
  (if (and data (file-exists-p data))
      (let ((case-fold-search 't)
            (errs '("command not found"
                    "is not recognized"
                    "'head'"
                    "^[ \t\n]*$"
                    ))
            (header (if esn-w32
                        ;; Use Cygwin paths when needed...
                        (let ((head (executable-find "head")) (f data))
                            (when (string-match "cygwin" head)
                              (when (string-match "^\\([A-Z]\\):/" f)
                                (setq f (replace-match (concat "/cygdrive/" (downcase (match-string 1 data)) "/") t t f))))
                            (concat head " -n1 " f))
                      (concat "head -n1 " data)))
            (p1 (point))
            (p2 (point)))
        (setq header (esn-command-to-string header))
        (when (not (string-match "^#?\\([A-Za-z][^,]*\"?,\\)+$" (concat header ",")))
          (setq header (esn-command-to-string (concat "head -n2 " data)))
          (with-temp-buffer
            (insert header)
            (goto-char (point-min))
            (forward-line 1)
            (beginning-of-line)
            (setq header (buffer-substring (point) (point-max)))))
        (when (string-match (regexp-opt errs) header)
          (if (> esn-mode-tab-complete-input-size (nth 7 (file-attributes header)))
              (unless no-message
                (esn-message "Input file too large to open with emacs  (causes a long wait).  Consider finding header information with GNU 'head' command.  Use Cygwin or equivalent.")
                (setq header "")
                ""
                )
            (unless no-message
              (esn-message "Find Header information faster with GNU 'head' command.  Use Cygwin or equivalent for Windows."))
            (find-file data)
            (save-excursion
              (goto-char (point-min))
              (setq p1 (point))
              (end-of-line)
              (setq p2 (point))
              (setq header (buffer-substring p1 p2)))
            (kill-buffer (current-buffer))))
        (symbol-value 'header))
    (unless no-message
      (esn-message "Data file ``%s'' does not exist.  Can't auto-complete $INPUT record." data))
    ""))
;;;###autoload
(defun esn-mode-add-input-variables ()
  "Defines input variable aliases automatically if not present."
  (when esn-update-input-add-comment
    (let (
          (add-inp nil)
          (comment "")
          (pt nil))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (eval-when-compile (esn-reg-record-exp "INP")) nil t)
          (unless (re-search-forward ";[|C] *INPUT Variables" nil t)
            (when (re-search-forward (eval-when-compile (esn-reg-records)) nil t)
              (beginning-of-line)
              (setq add-inp (point))))))
      (when add-inp
        (save-excursion
          (goto-char add-inp)
          (insert (esn-mode-get-input-comment)))))))

(defun esn-mode-get-input-comment ()
  "* Gets the input items for an input string."
  (let (
        (ret "")
        (inp (esn-rec "INP" 't)))
    (with-temp-buffer
      (insert inp)
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" nil 't)) nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\<[A-Z][A-Z0-9]* *= *\\(DROP\\|SKIP\\)" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\(DROP\\|SKIP\\) *= *[A-Z][A-Z0-9]*\\>" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "=" nil t)
        (replace-match " "))
      (setq inp (split-string (upcase (buffer-substring (point-min) (point-max))))))
    (mapc (lambda(x)
            (if (assoc (upcase x) esn-variable-labels)
                (setq ret (concat ret "\n;| " (upcase x) (make-string (- 6 (min 4 (length x))) ? )
                                  " | "
                                  (cadr (assoc (upcase x) esn-variable-labels))
                                  (make-string (- 67 (length (cadr (assoc (upcase x) esn-variable-labels)))) ? )
                                  "|"))
              (setq ret (concat ret "\n;| " (upcase x) (make-string (- 6 (min 4 (length x))) ? )
                                " | " x
                                (make-string (- 67 (length x)) ? ) "|"))))
          inp)
    (setq ret (concat ";|-----------------------------------------------------------------------------|\n"
                      ";|                                INPUT Variables                              |\n"
                      ";|--------+--------------------------------------------------------------------|"
                      ret
                      "\n;|--------+--------------------------------------------------------------------|\n"))
    (symbol-value 'ret)))

(defun esn-format-input-line (inp)
  "Format the input line so the columns are upper case and no more than 4 characters."
  ;; TODO:  Add=DROP to unrecognized $INPUT lines.
  (setq esn-mode-input-comments "")
  (let (
        (case-fold-search 't)
        (word "")
        (header inp)
        (header2 "")
        (oword "")
        (lst '())
        (lst2 (split-string inp "[^A-Za-z0-9 _.]" 't)))
    (setq header (upcase header))
    (while (string-match "[^A-Z0-9,]" header)
      (setq header (replace-match "" nil nil header)))
    (while (string-match "," header)
      (setq header (replace-match " " nil nil header)))
    (setq lst (split-string header "[^A-Za-z0-9]" 't))
    (setq header2 header)
    (when esn-update-input-add-comment
      (let (
            (what (nth 0 lst)))
        (while (> (length lst) 0)
          (when what
            (if (assoc (upcase what) esn-variable-labels)
                (setq esn-mode-input-comments (concat esn-mode-input-comments "\n;C " (upcase what) ":" (make-string (- 6 (min 4 (length what))) ? )
                                                      (cadr (assoc (upcase what) esn-variable-labels))))
              (setq esn-mode-input-comments (concat esn-mode-input-comments "\n;C " (upcase what) ":" (make-string (- 6 (min 4 (length what))) ? ) (nth 0 lst2)))))
          (pop lst)
          (pop lst2)
          (setq what (nth 0 lst))))
      (setq esn-mode-input-comments (concat
                                     ";C ----------------------------------------------------------------------------\n"
                                     ";C                                INPUT Variables\n"
                                     ";C ----------------------------------------------------------------------------"
                                     esn-mode-input-comments
                                     "\n;C ----------------------------------------------------------------------------"
                                     )))
    
    (while (string-match "\\<[A-Z]\\{5,\\}\\>" header)
      (setq word (match-string 0 header))
      (setq oword word)
      (setq header (replace-match "abcde" nil nil header))
      (when (string-match (format "\\<%s\\>" (regexp-quote word)) header2)
        (setq header2 (replace-match "abcde" nil nil header2)))
      (while (and word (< 4 (length word))
                  (string-match "[AEIOU]" word))
        (setq word (replace-match "" nil nil word))
                                        ;       (message "Chopped Word %s" word))
        (if (and word (< 4 (length word)))
            (setq word (substring word 0 4)))
        (if (string-match "abcde" header)
            (if esn-update-input-add-comment
                (progn
                  (setq header (replace-match word nil nil header))
                  (when (string-match "abcde" header2)
                    (setq header2 (replace-match (format "%s" word) nil nil header2))
                    (if (assoc (upcase word) esn-variable-labels)
                        (when (string-match (format ";C %s\\:\\( +\\).*" (regexp-quote oword)) esn-mode-input-comments)
                          (setq esn-mode-input-comments (replace-match (format ";C %s:\\1%s" (upcase word)
                                                                               (cadr (assoc (upcase word) esn-variable-labels)))
                                                                       't nil esn-mode-input-comments)))
                      (when (string-match (format ";C %s\\:" (regexp-quote oword)) esn-mode-input-comments)
                        (setq esn-mode-input-comments (replace-match (format ";C %s:" word) 't nil esn-mode-input-comments))))))
              (setq header (replace-match word nil nil header))
              (setq header2 header))
          (esn-error "Somethings wrong"))))
    (symbol-value 'header2)))

(defun esn-add-input-line ()
  "This adds an input line if it can."
  (interactive)
  (save-restriction
    (save-excursion
      (if esn-mode-tab-complete-input
          (let (
                (case-fold-search 't)
                (data "")
                (rcdir esn-completing-current-directory)
                (inform "INPUT line can be automatically generated if $DATA is in control stream, and (2) the data is a CSV with header names included.")
                (header "")
                (inhibit-read-only 't)
                (esw esn-skip-wrap))
            (save-excursion
              (esn-narrow-to-current-problem)
              (goto-char (point-min))
              (if (not (re-search-forward (eval-when-compile (esn-reg-data-rec "DAT")) nil t))
                  (message inform)
                (setq data (match-string 2))
                (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) data)
                    (setq data (replace-match "" nil nil data)))
                (if (not (string-match "\\.\\(CSV\\|csv\\)" data))
                    (message inform)
                  (goto-char (point-min))
                  (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "INP")) nil t))
                      (message "Can't figure out how this function was called.  There is no $INPUT record located.")
                    (save-excursion
                      (esn-narrow-rec)
                      (goto-char (point-min))
                      (re-search-forward (eval-when-compile (esn-reg-record-exp "INP")) nil t)
                      (skip-chars-forward " \t\n")
                      (if (not (eobp))
                          (progn
                            (message "The $INPUT record must be empty to automatically fill it in.")
                            (widen)
                            (esn-narrow-to-current-problem))
                        (widen)
                        (esn-narrow-to-current-problem)
                        (goto-char (point-min))
                        (re-search-forward (eval-when-compile (esn-reg-record-exp "INP")) nil t)
                        (setq header (esn-input-get-header data 't))
                        
                        (if (not (string-match "^#?\\([A-Za-z][^,]*\"?,\\)+$" (concat header ",")))
                            (esn-error inform)
                          (esn-add-ignore-data header)
                          (setq header (esn-format-input-line header))))))))
              (widen))
            (insert (esn-add-drop-if-appropriate header))
            (when (and (not (string= header "")) esn-mode-tab-complete-input-after-data)
              (esn-input-before-data))
            (when esn-update-input-add-comment
              (save-excursion
                (if (not (string= "DAT" (esn-get-current-record)))
                    (insert (concat "\n" esn-mode-input-comments))
                  (when (re-search-backward (eval-when-compile (esn-reg-record-exp "DAT"))  nil t)
                    (insert (concat esn-mode-input-comments "\n"))))))
            (save-excursion
              (when (re-search-backward (eval-when-compile (esn-reg-record-exp "INP")) nil t)
                (end-of-line)
                (esn-magic-wrap nil nil 't))))))))

(defun esn-font-lock-get-data-cols ()
  "* This gets and assigns the number of data columns if inputting a CSV file."
  (save-restriction
    (save-excursion
      (let (
            (case-fold-search 't)
            (data "")
            (rcdir esn-completing-current-directory)
            (header "")
            (inhibit-read-only 't)
            (esw esn-skip-wrap)
            ret)
        (save-excursion
          (esn-narrow-to-current-problem)
          (goto-char (point-min))
          (if (not (re-search-forward (eval-when-compile 
                                        (esn-reg-data-rec "DAT")) nil t)) nil
            (setq data (match-string 2))
            (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) data)
                (setq data (replace-match "" nil nil data)))
            (unless (string= esn-font-lock-input-data data)
              (if (not (file-exists-p data))
                  (setq esn-font-lock-input-number 0)
                (setq header (split-string (esn-input-get-header data 't) "[,; \t]" 't))
                (setq esn-font-lock-input-data data)
                (setq esn-font-lock-input-number (length header))))))
        (symbol-value 'esn-font-lock-input-number)))))

(defun esn-space-input (&optional rec)
  "* Adds the variable and Data input comments"
  (setq esn-xpose-get-input-line-save nil)
  (when (string= "INP" (save-excursion (skip-chars-backward " \t\n") (esn-get-current-record)))
    (save-excursion
      (save-restriction
        (save-match-data
          (esn-narrow-to-current-problem)
          (goto-char (point-min))
          (if (and (= esn-number-input-begin (point-min))
                   (= esn-number-input-end (point-max)))
              't
            (setq esn-number-input-begin (point-min))
            (setq esn-number-input-end (point-max))
            (let (
                  (case-fold-search 't)
                  (inp (esn-rec "INP" 't))
                  (debug-on-error 't)
                  lst
                  present
                  dups
                  over-cols
                  (nkeep 0)
                  (ndrop 0)
                  (ncols (esn-font-lock-get-data-cols))
                  (n 0)
                  (max-num (if (>= (string-to-number (esn-update-get-version)) 7)
                               50
                             20))
                  tmp
                  begin
                  )
              (with-temp-buffer
                (insert inp)
                (goto-char (point-min))
                (while (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" nil 't)) nil t)
                  (replace-match ""))
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match ""))
                (goto-char (point-min))
                (while (re-search-forward "[ \t]*=[ \t]*" nil t)
                  (replace-match "="))
                (goto-char (point-min))
                (while (re-search-forward "[ \t][ \t]+" nil t)
                  (replace-match " "))
                (goto-char (point-min))
                (while (re-search-forward "\\(DROP\\|SKIP\\)=\\([A-Z].*?\\>\\)" nil t)
                  (replace-match "\\2=\\1"))
                (setq lst 
                      (mapcar 
                       (lambda(x) 
                         (let (
                               (ret (split-string x "=" 't))
                               d1
                               d0
                               prs
                               )
                           (setq n (+ n 1))
                           (when (and (nth 1 ret)
                                      (string-match "^\\(DROP\\|SKIP\\)$" (nth 1 ret)))
                             (setq ndrop (+ ndrop 1))
                             (setq d1 't))
                           ;; Without a label, SKIP and DROP are still dropping and
                           ;; skipping a parameter
                           (when (string-match "^\\(DROP\\|SKIP\\)$" (nth 0 ret))
                             (setq ndrop (+ ndrop 1))
                             (setq d0 't))
                           ;; Figure out which labels are duplicated.
                           (when present
                             (unless d0 
                               (setq prs (format "^%s$" (regexp-opt present 'words)))
                               (when (string-match prs (nth 0 ret))
                                 (add-to-list 'dups (nth 0 ret))))
                             (when (nth 1 ret)
                               (unless d1
                                 (unless prs
                                   (setq prs (format "^%s$" (regexp-opt present 'words))))
                                 (when (string-match prs (nth 1 ret))
                                   (add-to-list 'dups (nth 1 ret))))))
                           (add-to-list 'present (nth 0 ret))
                           (when (nth 1 ret)
                             (add-to-list 'present (nth 1 ret)))
                           ;; Is this over the number of columns requested?
                           (when (and (not (= 0 ncols)) (< ncols n))
                             (unless d0
                               (add-to-list 'over-cols (nth 0 ret)))
                             (when (nth 1 ret)
                               (unless d1
                                 (add-to-list 'over-cols (nth 1 ret)))))
                           (symbol-value 'ret))) (split-string (buffer-substring (point-min) (point-max)) "[ \t]" 't))))
              (setq nkeep (- n ndrop))
              (goto-char (point-min))
              (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" nil 't)) nil t)
              (setq begin (point))
              (if (re-search-backward (concat "^ *;" (regexp-quote esn-sub-begin) " Variables [0-9]+/[0-9]+ " (regexp-quote esn-sub-end) ";") nil 't)
                  (replace-match (format ";%s Variables %s/%s %s;" esn-sub-begin nkeep max-num esn-sub-end))
                (goto-char begin)
                (beginning-of-line)
                (insert (format ";%s Variables %s/%s %s;\n" esn-sub-begin nkeep max-num esn-sub-end)))
              (unless (= 0 ncols)
                (goto-char begin)
                (if (re-search-backward (concat "^ *;" (regexp-quote esn-sub-begin) " Data [0-9]+[ \t]*[(][=][0-9]+[)] " (regexp-quote esn-sub-end) ";") nil 't)
                    (replace-match (format ";%s Data %s (=%s) %s;" esn-sub-begin n ncols esn-sub-end))
                  (goto-char begin)
                  (beginning-of-line)
                  (insert (format ";%s Data %s (=%s) %s;\n" esn-sub-begin n ncols esn-sub-end))))
              (cond
               ( (and over-cols dups)
                 (setq tmp (regexp-opt (append over-cols dups (list "z")) 'words))
                 (when (string-match "z" tmp)
                   (setq tmp (replace-match (eval-when-compile (esn-reg-records)) 't 't tmp)))
                 (setq tmp (esn-gen-cookies-quote tmp))
                 (with-temp-buffer
                   (insert "(setq tmp \"" tmp "\")")
                   (eval-buffer))
                 (setq esn-number-input-warning-reg tmp))
               ( (and over-cols (not dups))
                 (setq tmp (regexp-opt (append over-cols (list "z")) 'words))
                 (when (string-match "z" tmp)
                   (setq tmp (replace-match (eval-when-compile (esn-reg-records)) 't 't tmp)))
                 (setq tmp (esn-gen-cookies-quote tmp))
                 (with-temp-buffer
                   (insert "(setq tmp \"" tmp "\")")
                   (eval-buffer))
                 (setq esn-number-input-warning-reg tmp))
               ( (and (not over-cols) dups)
                 (setq tmp (regexp-opt (append dups (list "z")) 'words))
                 (when (string-match "z" tmp)
                   (setq tmp (replace-match (eval-when-compile (esn-reg-records)) 't 't tmp)))
                 (setq tmp (esn-gen-cookies-quote tmp))
                 (with-temp-buffer
                   (insert "(setq tmp \"" tmp "\")")
                   (eval-buffer))
                 (setq esn-number-input-warning-reg tmp))
               ( (and (not over-cols) (not dups))
                 (setq esn-number-input-warning-reg nil)))
              't )))))))

(esn-rec-post-hook "input" 'esn-space-input)

(provide 'esn-input)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-input.el ends her
