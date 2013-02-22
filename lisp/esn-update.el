;;; esn-update.el --- Updates upon save for Emacs Speaks NONMEM
;;
;; Filename: esn-update.el
;; Description: Describes the updates on save for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Jan 26 14:50:03 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Wed Dec 21 09:52:04 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 426
;; URL: http://esnm.sourceforge.net
;; Keywords:  header, esn, update
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is the update functions for Emacs Speaks NONMEM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 14:19:38 2010 (-0500) #336 (Matthew L. Fidler)
;;    Cached version information per buffer.  Reset on save.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 12:49:02 2010 (-0500) #332 (Matthew L. Fidler)
;;    Created `esn-msfo-name' for use in Yasnippet
;; 17-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 17 11:27:10 2010 (-0500) #308 (Matthew L. Fidler)
;;    Added bug fix for add-header-to-file when buffer is not associated with file.
;; 24-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Tue Aug 24 11:57:22 2010 (-0500) #258 (Matthew L. Fidler)
;;    Made a prompt for a version update the regular expressions.
;; 13-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jul 13 10:54:09 2010 (-0500) #241 (Matthew L. Fidler)
;;    Added bug-fix where this file had spaces and semi-colons.  Probably by opening in esn-mode....
;; 13-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jul 13 09:48:48 2010 (-0500) #186 (Matthew L. Fidler)
;;    Added code to take out white-spaces and returns before $PROBLEM statement
;; 13-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jul 13 09:10:47 2010 (-0500) #169 (Matthew L. Fidler)
;;    Fixed bug on update-copyright.
;; 12-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jul 12 17:02:44 2010 (-0500) #166 (Matthew L. Fidler)
;;    Added bug-fix for updating relative paths.
;; 12-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jul 12 16:17:38 2010 (-0500) #163 (Matthew L. Fidler)
;;    Bug-fix for II=DROP saving as II=D
;; 01-Jul-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jul  1 13:31:57 2010 (-0500) #156 (Matthew L. Fidler)
;;    Added fix to make sure that EsN doesn't try to wrap records in $PK type blocks
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 13:46:43 2010 (-0500) #147 (Matthew L. Fidler)
;;    Added a saving of the xmind title so that there is not a save of the xmind
;;    topic unless it changes.
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 16:38:01 2010 (-0500) #145 (Matthew L. Fidler)
;;    Made CMT=DROP not change to a blank item.
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 13:34:38 2010 (-0500) #141 (Matthew L. Fidler)
;;    Added putting WIDE on $DATA record on save under PLT tools.
;; 10-May-2010    Matthew L. Fidler
;;    Last-Updated: Mon May 10 11:22:03 2010 (-0500) #105 (Matthew L. Fidler)
;;    Only exclude the $PROBLEM statement when the $PRO is not included in
;;    the purpose update.
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 09:17:36 2010 (-0500) #100 (Matthew L. Fidler)
;;    Added a save-restriction when narrowing to the header.
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 08:34:41 2010 (-0500) #84 (Matthew L. Fidler)
;;    Added bugfix to copyright update.  Sometimes copyright would have trailing
;;    coma without need.
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 08:17:07 2010 (-0500) #82 (Matthew L. Fidler)
;;    Added bugfix for is-cp function.
;; 03-May-2010    Matthew L. Fidler
;;    Last-Updated: Mon May  3 16:37:15 2010 (-0500) #77 (Matthew L. Fidler)
;;    Add Header information
;; 22-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Fri Apr  2 09:07:10 2010 (-0500) #72 (Matthew L. Fidler)
;;    Added EsN error handling
;; 02-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Fri Apr  2 09:06:54 2010 (-0500) #71 (Matthew L. Fidler)
;;    Fixed alias errors
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 22:58:34 2010 (-0600) #67 (Matthew L. Fidler)
;;    Changed to use magic wrap timer
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 18:31:11 2010 (-0600) #62 (Matthew L. Fidler)
;;    No longer requires default xmind map to be created
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 11:41:18 2010 (-0600) #50 (Matthew L. Fidler)
;;
;;    Fix DATE= DROP translating to DATE for Emacs Speaks NONMEM automatic fixing
;;    of aliases for tables.
;;
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 10:42:40 2010 (-0600) #42 (Matthew L. Fidler)
;;    Added PLT tools MSFO= msfo.outputfile update.
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 14:46:56 2010 (-0600) #35 (Matthew L. Fidler)
;;
;;    Added Bug Fix for opening files when Xmind is enabled, but the control
;;    stream is not associated with the appropriate Xmind file.
;;
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 08:37:29 2010 (-0600) #27 (Matthew L. Fidler)
;;    Bug Fix of Auto-update of parent model.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:20:02 2010 (-0600) #25 (Matthew L. Fidler)
;;    Added updating of Xmind map associated with current file.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 16:43:34 2010 (-0600) #17 (Matthew L. Fidler)
;;    Added Wrote message to update-header to display in message area.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 16:42:33 2010 (-0600) #16 (Matthew L. Fidler)
;;    Added Xmind auto-update of Xmind's parent comment.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 14:36:11 2010 (-0600) #12 (Matthew L. Fidler)
;;    Change tool-bar after save (if needed).
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 14:50:31 2010 (-0600) #1 (Matthew L. Fidler)
;;    Added Xmind update upon save.
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
(declare-function esn-xpose-run-number "esn-xpose")
(declare-function esn-get-extension "esn-xpose")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-fix-numbering "esn-extended")
(declare-function esn-plt-add-wide "esn-plt")
(declare-function esn-secondary-parameters "esn-secondary-parameters")
(declare-function esn-add-update-topics "esn-xmind")
(declare-function esn-80-clear "esn-80")
(declare-function esn-xpose-save "esn-xpose")
(declare-function esn-file-coding-system "esn-coding")
(declare-function esn-cwres "esn-cwres")
(declare-function esn-pdx-set-run-number "esn-pdx")
(declare-function esn-xmind-update-current-file "esn-xmind")
(declare-function esn-should-be-new-version "esn-vc")
(declare-function esn-plt-auto "esn-plt")
(declare-function esn-on-save-use-vc "esn-vc")
(declare-function esn-table-split "esn-tab-pred")
(declare-function esn-xmind-default-map-name "esn-xmind")
(declare-function esn-edit "esn-vc")
(declare-function esn-table-pred-undo-split "esn-tab-pred")
(declare-function esn-undo-numbering "esn-extended")
(declare-function esn-hide-header "esn-hide")
(declare-function esn-set-mode-name "esn-mode")
(require 'esn-magic-keys)
(require 'esn-options)
;;
;; this provides any NONMEM updating functions
;;
(defvar esn-updated-modification-line nil
  "* Variable that tells if there was a modification line added.")

(defvar esn-vc-first-prompt 't
  "* Variable that tells if the VC has already been prompted for...")

(defun esn-fix-input-aliases ()
  "* Fixes the input aliases of TIME= TSFD to TIME to allow proper
output with headers.  It does this by checking the variables
outputted within the tables to make sure that they match what is
inputted AND makes sure that the variables outputted do not
interfere with reserved NONMEM variables."
  (interactive)
  (let ((np (esn-num-problems))
        (tab "")
        (i 0)
        (tab-reg "")
        (tab-reg2 "")
        (oneheader nil)
        (inp-reg 
         (eval-when-compile 
           (format "\\(?:%s\\)" 
                   (regexp-opt 
                    '(
                      "ADDL"
                      "AMT"
                      "CALL"
                      "CMT"
                      "CONT"
                      "DATE"
                      "DV"
                      "EVID"
                      "ID"
                      "II"
                      "MDV"
                      "PCMT"
                      "RATE"
                      "SS"
                      "TIME") nil))))
        (al '()))
    (save-restriction
      (while (< i np)
        (setq i (+ i 1))
        (esn-narrow-to-problem i)
        ;; First Set aliases to NONMEM reserved variables to be kept throughout
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" 't)) nil t)
            (save-restriction
              (end-of-line)
              (esn-narrow-rec)
              (goto-char (point-min))
              (while (re-search-forward (format "\\<\\(%s\\)\\( *= *\\)\\([A-Za-z][A-Za-z0-9_]*\\)\\>" inp-reg) nil t)
                (add-to-list 'al (list (match-string 3) (match-string 1)))
                (unless (or
                         (string= (upcase (match-string 3)) "DROP")
                         (string= (upcase (match-string 3)) "SKIP"))
                  (when (not (or
                              (string= (upcase (match-string 1)) "CMT")
                              (string= (upcase (match-string 1)) "DATE")
                              (string= (upcase (match-string 1)) "DAT1")
                              (string= (upcase (match-string 1)) "DAT2")
                              (string= (upcase (match-string 1)) "DAT3")))
                    (backward-char (+ (length (match-string 3)) (length (match-string 2))))
                    (delete-char (+ (length (match-string 3)) (length (match-string 2)))))))
              (goto-char (point-min))
              (while (re-search-forward
                      (format "\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\( *= *\\)\\(%s\\)\\>" inp-reg)
                      nil t)
                (add-to-list 'al (list (match-string 1) (match-string 3)))
                (unless (or
                         (string= (upcase (match-string 1)) "DROP")
                         (string= (upcase (match-string 1)) "SKIP"))
                  (when (not (or
                              (string= (upcase (match-string 1)) "CMT")
                              (string= (upcase (match-string 2)) "DATE")
                              (string= (upcase (match-string 2)) "DAT1")
                              (string= (upcase (match-string 2)) "DAT2")
                              (string= (upcase (match-string 2)) "DAT3")))
                    (backward-char (length (match-string 0)))
                    (delete-char (+ (length (match-string 1)) (length (match-string 2))))))))
            (goto-char (point-min))
            (while (re-search-forward (eval-when-compile (esn-reg-record-exp "TAB" 't)) nil t)
              (save-restriction
                (esn-narrow-rec)
                (mapc (lambda (x)
                        (goto-char (point-min))
                        (while (re-search-forward (format "\\<%s\\>" (regexp-quote (car x))) nil t)
                          (replace-match (format "%s" (nth 1 x)) nil t)))
                      al)
                (goto-char (point-max)))))
          (save-excursion
            (goto-char (point-min))
            (setq oneheader (re-search-forward "\\<oneheader\\>" nil t))
            (when oneheader
              ;; Look at current table to see what the variables requested are...
              (setq tab (esn-rec "TAB" 't 't))
              (while (string-match (format "%s[A-Z]*"
                                           (regexp-opt '("APPE"
                                                         "COND"
                                                         "FORW"
                                                         "NOAP"
                                                         "NOFO"
                                                         "NOPR"
                                                         "OMIT"
                                                         "ONEH"
                                                         "PRIN"
                                                         "UNCO"
                                                         "FIRS"
                                                         "$TAB") 't)) tab)
                (setq tab (replace-match "" nil nil tab)))
              (while (string-match "\\<FILE[ \t]*[= \t][ \t]*[^ \t]*" tab)
                (setq tab (replace-match "" nil nil tab))))
            (unless (string= tab "")
              (setq tab-reg (format "\\(%s\\) *= *\\([A-Z0-9]+\\)"(regexp-opt (split-string tab))))
              (setq tab-reg2 (format "\\([A-Z0-9]+\\) *= *\\(%s\\)"(regexp-opt (split-string tab))))
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" 't 't)) nil t)
                  (save-restriction
                    (esn-narrow-rec)
                    (goto-char (point-min))
                    (while (re-search-forward tab-reg nil t)
                      (unless (or
                               (string= (upcase (match-string 2)) "DROP")
                               (string= (upcase (match-string 2)) "SKIP")
                               (string= (upcase (match-string 1)) "DROP")
                               (string= (upcase (match-string 1)) "SKIP"))
                        (replace-match "\\1")))
                    (goto-char (point-min))
                    (while (re-search-forward tab-reg2 nil t)
                      (unless (or
                               (string= (upcase (match-string 1)) "DROP")
                               (string= (upcase (match-string 1)) "SKIP")
                               (string= (upcase (match-string 2)) "DROP")
                               (string= (upcase (match-string 2)) "SKIP"))
			
                        (replace-match "\\2")))))))))))))

(defun esn-mirror-purpose-problem ()
  "* This function mirrors the purpose on the $PROBLEM statement. (First line Only)"
  (interactive)
  (let ((p (esn-update-get-purpose 't))
        (case-fold-search 't))
    (when (string-match "^\\(.*\\)$" p)
      (setq p (match-string 1 p)))
    (when (> (length p) 62)
      (setq p (substring p 0 62)))
    (unless (string-match "^[ \t]*$" p)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\($PRO[A-Z]+\\) *.*$" nil t)
          (replace-match (concat "\\1" p) 't))))))

(defun esn-update-modification ()
  "* Update modification information"
  (interactive)
  (let* (line
         (case-fold-search 't)
         (inhibit-read-only 't)
         (inhibit-point-motion-hooks 't)
         (mod-txt (esn-update-rep-bef-after (esn-update-modification-begin)
                                            (esn-update-modification-end)))
         (tmp-mod-sep (esn-update-modification-line))
         (mod-sep (if (string-match "%s.*" tmp-mod-sep)
                      (replace-match "" nil nil tmp-mod-sep)
                    tmp-mod-sep))
         (mod-lst
          (mapcar (lambda(x)
                    (let (
                          (ret x))
                      ;; Take out wrapping.
                      (if (string-match " *\n;[;C]* *" ret)
                          (setq ret (replace-match " " nil nil ret)))
                      (symbol-value 'ret)))
                  (if (and mod-txt (string-match (regexp-quote mod-sep) mod-txt))
                      (split-string mod-txt (regexp-quote mod-sep))
                    '())))
         (tmp-lst '())
         (tmp "")
         (uname (esn-update-authorship-name-and-user))
         (date-time (esn-update-modification-log-date-time))
         (update-reason (esn-update-modification-add-reason))
         (update-users-only (= (esn-update-modification-type) 1))
         (update-luser (=      (esn-update-modification-type) 3))
         (update-luser-date (= (esn-update-modification-type) 4))
         (update-user-date (=  (esn-update-modification-type) 2))
         (add-user 't)
         (first 't)
         (user-name "")
         (dt "")
         (day "")
         (year "")
         (reg-user "")
         (update-line (esn-update-modification-dt-every-time)))
    (setq esn-updated-modification-line nil)
    ;; user style?
    (if uname
        (setq user-name (format "%s (%s)" (esn-user-full-name) (user-login-name)))
      (setq user-name (format "%s" (esn-user-full-name))))
    (setq reg-user (regexp-quote user-name))
    (if date-time
        (setq dt (format-time-string "%a %b %d %T %Z %Y"))
      (setq dt (format-time-string "%a %b %d %Y")))
    (setq day (format "\\<%s\\>" (regexp-quote (format-time-string "%a %b %d"))))
    (setq year (format "\\<%s\\>" (regexp-quote (format-time-string "%Y"))))
    ;; Update distinct users
    ;; Update if last user is not current user.
    (unless update-reason
      (setq line (format "%s, %s" dt user-name)))
    (mapc
     (lambda(x)
       (let (
             (ret x))
         (when update-users-only
           (when (string-match reg-user ret)
             (when update-reason
               ;; Keep reason since already present.
               ;; Update User name and date/time.
               (setq line ret)
               (when update-line
                 ;; only update line when asked to.
                 (when (string-match "^[^,]*," line)
                   (setq line (replace-match (concat dt ",") nil nil line)))
                 (when (string-match ",[^,]*$" line)
                   (setq line (replace-match (concat ", " user-name) nil nil line)))))
             (setq ret line); Replace with most recent update.
             (setq add-user nil);Don't add twice.
             ))
         (when update-user-date
           (when (and (string-match reg-user ret)
                      (string-match day ret)
                      (string-match year ret))
             (when update-reason
               ;; Keep reason since already present.
               ;; Update User name and date/time.
               (setq line ret)
               (when update-line
                 (when (string-match "^[^,]*," line)
                   (setq line (replace-match (concat dt ",") nil nil line)))
                 (when (string-match ",[^,]*$" line)
                   (setq line (replace-match (concat ", " user-name) nil nil line)))))
	     ;; Day, Year and user matches, replace.
             (setq ret line) ; Replace with most recent update.
             (setq add-user nil) ;Don't add twice.
             ))
         (when update-luser
           (when (and first
                      (string-match reg-user ret))
                                        ; Last user= current user.
             (when update-reason
               ;; Keep reason since already present.
               ;; Update User name and date/time.
               (setq line ret)
               (when update-line
                 (when (string-match "^[^,]*," line)
                   (setq line (replace-match (concat dt ",") nil nil line)))
                 (when (string-match ",[^,]*$" line)
                   (setq line (replace-match (concat ", " user-name) nil nil line)))))
             (setq ret line)
             (setq add-user nil)))
         (when update-luser-date
           (when (and first
                      (string-match reg-user ret)
                      (string-match day ret)
                      (string-match year ret))
             (when update-reason
               ;; Keep reason since already present.
               ;; Update User name and date/time.
               (setq line ret)
               (when update-line
                 (when (string-match "^[^,]*," line)
                   (setq line (replace-match (concat dt ",") nil nil line)))
                 (when (string-match ",[^,]*$" line)
                   (setq line (replace-match (concat ", " user-name) nil nil line)))))
             (setq ret line)
             (setq add-user nil)))
         (when first
           (unless (string-match ret  "^[ \t\n;]$")
             (setq first nil)))
         (unless (string-match ret "^[ \t\n;]$")
           (setq ret (format (esn-update-modification-line) ret))
           (add-to-list 'tmp-lst ret))))
     mod-lst)
    (setq mod-txt (apply 'concat (reverse tmp-lst)))
    (when add-user
      ;; Before save use RCS to add the previous version to the RCS log (via
      ;; command line)
      (when 't
        (setq esn-commit-last-version 't))
      (when nil
        (let (
              (bs (buffer-substring (point-min) (point-max)))
              (pt (point)))
          (setq esn-mode-skip-ini 't)
          (revert-buffer 't 't 't)
          (setq esn-mode-skip-ini nil)
          (setq esn-commit-last-version 't)
          (delete-region (point-min) (point-max))
          (insert bs)
          (goto-char pt)))
      (setq esn-updated-modification-line 't)
      (when update-reason
        (if (> (length mod-txt)  0)
            ;; Only prompt for reason on a new modification line.
            (setq line (format "%s, %s, %s" dt
                               (esn-prompt "Reason for modification? "
                                           "[^ \t\n;]"
                                           "Reason for modification (there must be a reason...)? "
                                           )
                               user-name
                               ))
          (setq line (format "%s, %s, %s" dt "Initial Author" user-name))))
      (setq mod-txt (concat (format (esn-update-modification-line) line)
                            mod-txt)))
    ;; Now wrap
    (when (string-match "^ *\\(;+C*\\)" tmp-mod-sep)
      (setq tmp (match-string 1 tmp-mod-sep)))
    (setq tmp (concat "\n" tmp
                      (make-string (- (length (esn-update-modification-line))
                                      (+ 3 (length tmp))) ? )))
    (with-temp-buffer
      (insert mod-txt)
      (goto-char (point-min))
      (while (re-search-forward (format "^.\\{%s\\}" (1+ esn-character-limit)) nil t)
        (backward-word)
        (skip-chars-backward "(")
        (insert tmp))
      (setq mod-txt (buffer-substring (point-min) (point-max))))
    (esn-update-rep-bef-after (esn-update-modification-begin)
                              (esn-update-modification-end)
                              mod-txt)))
;;
;; Narrow to header.
;;
(defun esn-narrow-to-header ()
  "Narrows the buffer to the header."
  (interactive)
  (let (
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't)
        (h-reg (esn-header-regexp-quote))
        (narrowed nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward h-reg nil t)
        (narrow-to-region (- (point) (length (match-string 0)))
                          (point))
        (setq narrowed 't))
      (symbol-value 'narrowed))))
(defun esn-string-lessp (s1 s2)
  "* Case insesntivie string-lessp"
  (string-lessp (downcase s1) (downcase s2)))
(defun esn-regexp-time-string (format-string)
  "Changes a format-time-string's format string to a regular expression."
  (let (
        (ret format-string)
        (case-fold-search nil)
        (fmts (list
               (list "%D" "%m/%d/%y")
               (list "%R" "%H:%M")
               (list "%T" "%H:%M:%S")
               (list "%r" "%I:%M%S %p")
               (list "%Y" "[0-9]\\\\\\\\{4\\\\\\\\}")
               (list "%y" "[0-9]\\\\\\\\{2\\\\\\\\}")
               (list "%C" "[0-9]\\\\\\\\{2\\\\\\\\}")
               (list "%G" "[0-9]\\\\\\\\{4\\\\\\\\}")
               (list "%g" "[0-9]\\\\\\\\{2\\\\\\\\}")
               (list "%m" "[0-9]\\\\\\\\{2\\\\\\\\}")
               (list "%b" "[^ \n].*?")
               (list "%h" "[^ \n].*?")
               (list "%B" "[^ \n].*?")
               (list "%d" "[0-3][0-9]")
               (list "%e" "[ 1-3]?[0-9]")
               (list "%u" "[1-7]")
               (list "%w" "[0-6]")
               (list "%a" "[^ \n].*?")
               (list "%A" "[^ \n].*?")
               (list "%U" "[ 0-5]?[0-9]")
               (list "%W" "[ 0-5]?[0-9]")
               (list "%V" "[ 0-5]?[0-9]")
               (list "%j" "[ 0-3]?[ 0-9]?[0-9]")
               (list "%H" "[0-2][0-9]")
               (list "%I" "[0-1][0-9]")
               (list "%k" "[ 1-2][0-9]")
               (list "%l" "[ 1][0-9]")
               (list "%p" "[^ \n].*?")
               (list "%M" "[0-6][0-9]")
               (list "%S" "[0-6][0-9]")
               (list "%Z" "[^ \n].*?")
               (list "%z" "[0-9]+")
               (list "%s" "[0-9]+")
               (list "%c" "[^ \n].*?")
               (list "%x" "[^ \n].*?")
               (list "%X" "[^ \n].*?"))))
    (mapc (lambda(x)
            (let (
                  (var (regexp-quote (nth 0 x)))
                  (var-to (nth 1 x)))
              (while (string-match var ret)
                (setq ret (replace-match var-to nil nil ret)))))
          fmts
          )
    (symbol-value 'ret)))
(defun esn-update-rep-bef-after (bef aft &optional rep nospace)
  "Replaces or gets contents of portion with before and after. Only replaces when rep is non-nil.  When nospace is non-nil don't add space."
  (let (
        (beg nil)
        (end nil)
        (ret nil)
        (before bef)
        (after aft)
        (case-fold-search 't)
        (time (if (esn-update-creation-date-time)
                  (esn-regexp-time-string (esn-date-time-format))
                (esn-regexp-time-string (esn-date-format))))
        (author (if (esn-update-authorship-name-and-user)
                    "[^ \n].*? ([^ \n].*?)"
                  "[^ \n].*?")))
    (when before
      (while (string-match "[ \t\n]" (substring before 0 1))
        (setq before (substring before 1)))
      (while (string-match "[ \t\n]" (substring before -1))
        (setq before (substring before 0 -1 )))
      (while (string-match "[ \t\n]" (substring after 0 1))
        (setq after (substring after 1)))
      (while (string-match "[ \t\n]" (substring after -1))
        (setq after (substring after 0 -1 )))
      (save-excursion
        (goto-char (point-min))
        (when (if (string= before "$PRO")
                  (re-search-forward (eval-when-compile (format "%s[ \t]+" (esn-reg-record-exp "PRO" 't))) nil t)
                (search-forward before nil 't))
          (setq beg (point))
          (setq after (regexp-quote after))
          (when (string-match "\\\\\\$creation-date\\\\\\$" after)
            (setq after (replace-match time nil nil after)))
          (when (string-match "\\\\\\$initial-author\\\\\\$" after)
            (setq after (replace-match author nil nil after)))
	  
          (if (re-search-forward after nil 't)
              (progn
                (beginning-of-line)
                (skip-chars-backward "\n")
                (setq end (point)))
            (end-of-line)
            (setq end (point)))
          (if (not rep)
              (progn
                (setq ret (buffer-substring beg end)))
            (goto-char beg)
            (delete-region beg end)
            (if (or nospace (string= before "$PRO"))
                (insert rep)
              (insert (concat " " rep)))
            (setq ret 't)))))
    (when (not ret)
      (setq ret ""))
    (symbol-value 'ret)))
;; Prompting functions
(defvar esn-skip-purpose nil)
(defun esn-update-prompt-purpose (&optional cp)
  "Updates the purpose."
  (interactive)
  (if esn-skip-purpose
      esn-skip-purpose
    (let* (
           (inhibit-read-only 't)
           (inhibit-point-motion-hooks 't)
           (case-fold-search 't)
           (prp (esn-update-get-purpose 't))
           (prp-is-pro (string= "$PRO" (esn-update-purpose-before))))
      (when (string-match "^ +" prp)
        (setq prp (replace-match "" nil nil prp)))
      (when (string-match " +$" prp)
        (setq prp (replace-match "" nil nil prp)))
      (save-excursion
        (goto-char (point-min))
        (when (if prp-is-pro
                  (re-search-forward 
                   (eval-when-compile 
                     (format "%s[ \t]+" 
                             (esn-reg-record-exp "PRO" 't))) 
                   nil t)
                (search-forward (esn-update-purpose-before) nil t))
          (setq prp (esn-prompt
                     (format "Purpose/Description%s "
                             (if (and cp (esn-update-purpose-cp)
                                      esn-last-purpose)
                                 " (must be new and not empty): "
                               " (must not be empty): "))
                     "^.*?[^ \t\n                               ;].*?$"
                     (format "Purpose/Description%s "
                             (if (and cp (esn-update-purpose-cp)
                                      esn-last-purpose)
                                 " (must be new and not empty): "
                               " (must not be empty): "))
                     nil
                     prp))
          (when (string-match "^ +" prp)
            (setq prp (replace-match "" nil nil prp)))
          (when (string-match " +$" prp)
            (setq prp (replace-match "" nil nil prp)))
          (esn-update-rep-bef-after
           (esn-update-purpose-before)
           (esn-update-purpose-after)
           prp
           )
          (message "New purpose:\n%s\n\nOld Purpose:\n%s" prp 
                   esn-last-purpose)
          (when (and cp (esn-update-purpose-cp)
                     esn-last-purpose)
            (while (string= esn-last-purpose
                            (esn-update-get-purpose))
              (message "Last:\n%s\n\nCurrent:\n%s" esn-last-purpose
                       (esn-update-get-purpose))
              (ding)
              (setq prp (esn-prompt
                         "Purpose/Description (must be different \& not empty): "
                         "^.*?[^ \t\n;].*?$"
                         "Purpose/Description (must be different \& not empty): "
                         nil
                         prp
                         ))
              (when (string-match "^ +" prp)
                (setq prp (replace-match "" nil nil prp)))
              (when (string-match " +$" prp)
                (setq prp (replace-match "" nil nil prp)))
	      
              (esn-update-rep-bef-after
               (esn-update-purpose-before)
               (esn-update-purpose-after)
               prp))))))))
(defvar esn-update-get-version-cache nil
  )
(make-variable-buffer-local 'esn-update-get-version-cache)

;;;###autoload
(defun esn-update-get-version ()
  (if esn-update-get-version-cache
      esn-update-get-version-cache
    (let (
          (inhibit-read-only 't)
          (inhibit-point-motion-hooks 't)
          (case-fold-search 't)
          (ret nil)
          (tmp (esn-update-rep-bef-after (esn-update-version-begin)
                                         (esn-update-version-after))))
      (when (string= ""  tmp)
        (setq ret "-1"))
      (when tmp
        (mapc (lambda(x)
                (let (
                      (ver (nth 0 x))
                      (reg (regexp-quote (nth 1 x))))
                  (when (string-match reg tmp)
                    (setq ret ver))))
              esn-mode-nm-versions
              ))
      (when (string= ret "-1")
        (setq ret esn-assumed-version))
      (symbol-value 'ret))))

(defun esn-update-get-purpose (&optional no-fix)
  "Gets the purpose for the file.
If no-fix is nil, then remove irrelevant characters and downcase buffer.
"
  (let (
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't)
        (ret "")
        (tmp (esn-update-rep-bef-after (esn-update-purpose-before)
                                       (esn-update-purpose-after))))
    (unless no-fix
      (if (not tmp)
          (setq tmp "")
        (while (string-match "\\<$[Pp][Rr][Oo][A-Z]*[ \t]+" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (while (string-match "^ *;[;Cc|]* *" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (while (string-match "\n *;[;Cc|]* *" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (while (string-match "\n" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (while (string-match " +" tmp)
          (setq tmp (replace-match "" nil nil tmp)))
        (while (string-match "^ +" tmp)
          (setq tmp (replace-match "" nil nil tmp)))
        (while (string-match " +$" tmp)
          (setq tmp (replace-match "" nil nil tmp)))
        (setq tmp (downcase tmp))))
    (when (not tmp)
      (setq tmp ""))
    (symbol-value 'tmp)))

(defun esn-update-version ()
  "Updates the NONMEM version."
  (interactive)
  (let* (
         (inhibit-read-only 't)
         (inhibit-point-motion-hooks 't)
         (case-fold-search 't)
         (ver esn-nm-version)
         (ver-there nil)
         (versions (mapcar (lambda(x)
                             (if (string= ver (nth 0 x))
                                 (setq ver-there 't))
                             (nth 0 x))
                           esn-mode-nm-versions
                           ))
         (prompt-ver (mapconcat
                      (lambda(x) x)
                      versions ","))
         (ver-reg (format "^ *\\(%s\\) *$"
                          (regexp-opt versions 't))))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (esn-update-version-begin) nil t)
        (unless ver-there
          (setq ver (esn-prompt (format "NONMEM version (%s): " prompt-ver)
                                ver-reg
                                (format "Unknown NONMEM version, choose again (%s): " prompt-ver))))
        (esn-update-rep-bef-after (esn-update-version-begin)
                                  (esn-update-version-after)
                                  (nth 1 (assoc ver esn-mode-nm-versions)))
        ;; Switch regular expressions to the currently selected version
        (esn-switch-variables)))))

;; Copy functions
(defun esn-remove-header ()
  ;; Remove Default Header Information
  (interactive)
  (let (
        (esn-header-small nil))
    (esn-remove-header-0)
    (setq esn-header-small 't)
    (esn-remove-header-0)
    (save-excursion
      (delete-region (point-min) (save-excursion
                                   (goto-char (point-min))
                                   (skip-chars-forward " \t\n")
                                   (point))))))
(defun esn-remove-header-0 ()
  ;; Remove Default Header Information
  (let (
        (esn-force-census nil)
        (esn-force-no-census 't)
	
        (esn-force-pdx nil)
        (esn-force-no-pdx 't)
	
        (esn-force-pirana nil)
        (esn-force-no-pirana 't)
	
        (esn-force-plt nil)
        (esn-force-no-plt 't))
    (esn-remove-header-1))
  ;; Remove Census Header Information
  (let (
        (esn-force-census 't)
        (esn-force-no-census nil)
	
        (esn-force-pdx nil)
        (esn-force-no-pdx 't)
	
        (esn-force-pirana nil)
        (esn-force-no-pirana 't)
	
        (esn-force-plt nil)
        (esn-force-no-plt 't))
    (esn-remove-header-1))
  ;; Remove PDx Header Information
  (let (
        (esn-force-census nil)
        (esn-force-no-census 't)
	
        (esn-force-pdx 't)
        (esn-force-no-pdx nil)
	
        (esn-force-pirana nil)
        (esn-force-no-pirana 't)
	
        (esn-force-plt nil)
        (esn-force-no-plt 't))
    (esn-remove-header-1))
  ;; Remove Pirana Header Information
  (let (
        (esn-force-census nil)
        (esn-force-no-census 't)
	
        (esn-force-pdx nil)
        (esn-force-no-pdx 't)
	
        (esn-force-pirana 't)
        (esn-force-no-pirana nil)
	
        (esn-force-plt nil)
        (esn-force-no-plt 't))
    (esn-remove-header-1))
  ;; Remove PLT Header Information
  (let (
        (esn-force-census nil)
        (esn-force-no-census 't)
	
        (esn-force-pdx nil)
        (esn-force-no-pdx 't)
	
        (esn-force-pirana nil)
        (esn-force-no-pirana 't)
	
        (esn-force-plt 't)
        (esn-force-no-plt nil))
    (esn-remove-header-1)))
(defun esn-remove-header-1 ()
  "Removes header from file."
  (interactive)
  (save-excursion
    (let (
          (header-regexp (esn-header-regexp-quote))
          (case-fold-search 't)
          prp
          )
      (setq prp (esn-update-get-purpose 't))
      (with-temp-buffer
        (insert prp)
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (delete-region (point-min) (point))
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (delete-region (point) (point-max))
        (setq prp (buffer-substring-no-properties (point-min) (point-max))))
      (unless (string= "" prp)
        (setq esn-update-cp-purpose-save prp))
      (goto-char (point-min))
      (when (re-search-forward header-regexp nil t)
        (setq esn-update-cp-save-vals
              (mapcar (lambda (x)
                        (let (
                              (before (nth 0 x))
                              (after (nth 1 x))
                              (ret (list nil nil nil)))
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward before nil 't)
                              (setq ret (list before after
                                              (esn-update-rep-bef-after before after)))))
                          (symbol-value 'ret)))
                      (esn-update-cp-save-terms)))
        (goto-char (point-min))
        (when (re-search-forward header-regexp nil t)
          (replace-match ""))))))
(defun esn-get-header-old-name (header-p old-file-name ret)
  "* This is a subroutine to get the old header name."
  (let (
        (esn-header-small nil))
    (esn-get-header-old-name-0 header-p old-file-name ret)
    (setq esn-header-small 't)
    (esn-get-header-old-name-0 header-p old-file-name ret)))
(defun esn-get-header-old-name-0 (header-p old-file-name ret)
  "* This is a subroutine to get the header.  Its called multiple
times, changing which header is used so that Emacs Speaks NONMEM
can check ALL headers used."
  ;; Default
  (unless (symbol-value header-p) (let (
                                        (esn-force-census nil)
                                        (esn-force-no-census 't)
					
                                        (esn-force-pdx nil)
                                        (esn-force-no-pdx 't)
					
                                        (esn-force-pirana nil)
                                        (esn-force-no-pirana 't)
					
                                        (esn-force-plt nil)
                                        (esn-force-no-plt 't))
                                    (esn-get-header-old-name-1 header-p old-file-name ret))
          ;; Census
          )(unless (symbol-value header-p) (let (
                                                 (esn-force-census 't)
                                                 (esn-force-no-census nil)
						 
                                                 (esn-force-pdx nil)
                                                 (esn-force-no-pdx 't)
						 
                                                 (esn-force-pirana nil)
                                                 (esn-force-no-pirana 't)
						 
                                                 (esn-force-plt nil)
                                                 (esn-force-no-plt 't))
                                             (esn-get-header-old-name-1 header-p old-file-name ret))
                   ;; PDx POP
                   )(unless (symbol-value header-p) (let (
                                                          (esn-force-census nil)
                                                          (esn-force-no-census 't)
							  
                                                          (esn-force-pdx 't)
                                                          (esn-force-no-pdx nil)
							  
                                                          (esn-force-pirana nil)
                                                          (esn-force-no-pirana 't)
							  
                                                          (esn-force-plt nil)
                                                          (esn-force-no-plt 't))
                                                      (esn-get-header-old-name-1 header-p old-file-name ret))
                            ;; Pirana
                            )(unless (symbol-value header-p) (let (
                                                                   (esn-force-census nil)
                                                                   (esn-force-no-census 't)
								   
                                                                   (esn-force-pdx nil)
                                                                   (esn-force-no-pdx 't)
								   
                                                                   (esn-force-pirana 't)
                                                                   (esn-force-no-pirana nil)
								   
                                                                   (esn-force-plt nil)
                                                                   (esn-force-no-plt 't))
                                                               (esn-get-header-old-name-1 header-p old-file-name ret))
                                     ;; PLT
                                     )(unless (symbol-value header-p) (let (
                                                                            (esn-force-census nil)
                                                                            (esn-force-no-census 't)
									    
                                                                            (esn-force-pdx nil)
                                                                            (esn-force-no-pdx 't)
									    
                                                                            (esn-force-pirana 't)
                                                                            (esn-force-no-pirana nil)
									    
                                                                            (esn-force-plt 't)
                                                                            (esn-force-no-plt nil))
                                                                        (esn-get-header-old-name-1 header-p old-file-name ret))))
(defun esn-get-header-old-name-1 (header-p old-file-name ret)
  "* This is a subroutine to get the header.  Its called multiple
times, changing which header is used so that Emacs Speaks NONMEM
can check ALL headers used."
  (save-restriction
    (if (and (esn-update-file-name-long)
             (esn-narrow-to-header))
        (save-restriction
          (let (
                (blank nil))
            (save-excursion
              (goto-char (point-min))
              (skip-chars-forward " \t\n")
              (setq blank (= (point) (point-max))))
            (unless blank
              ;; Header present, get from header.
              (set header-p 't)
              (set old-file-name
                   (esn-update-rep-bef-after (esn-update-name-begin)
                                             (esn-update-name-end)))
              (while (string-match " *\n *;+C* *" (symbol-value old-file-name))
                (set old-file-name (replace-match "" nil nil (symbol-value old-file-name))))
              (while (string-match "\\\\" (symbol-value old-file-name))
                (set old-file-name (replace-match "/" nil nil (symbol-value old-file-name))))
              (while (string-match "^ +" (symbol-value old-file-name))
                (set old-file-name (replace-match ""  nil nil (symbol-value old-file-name))))
              (while (string-match " +$" (symbol-value old-file-name))
                (set old-file-name (replace-match "" nil nil (symbol-value old-file-name))))
              (unless (string= old-file-name (buffer-file-name))
                (set ret 't))
              ;; Set esn-last-small-name.
              (when (string-match "/\\([^/]*\\)$" (symbol-value old-file-name))
                (set old-file-name (match-string 1 (symbol-value old-file-name))))
              (when (string-match ".[^.]*$" (symbol-value old-file-name))
                (set old-file-name (match-string 1 (symbol-value old-file-name))))
              (setq esn-last-small-name (symbol-value old-file-name))
              (widen)))))))
(defun esn-is-cp ()
  "Returns if the file is a copy that needs to update the header.

When in PLT tools mode, and `esn-update-file-name-long' is
disabled, and `esn-plt-gen-graphics' is enabled, we assume the file
is a copy when the graphics script has not been generated yet.

When the `esn-last-full-name' variable is different from the
current buffer name defined by the function `buffer-file-name',
emacs is performing a save-as operation, and the old-file name is
pulled from `esn-last-full-name' instead of getting it from the
buffer.  In this case, no guessing is required for the last file
name.

Gets the old file name from the
 (1) Header, from file name field.

If there is no header present, then look for old file name in:
 (1) Tables and FORTRAN FILE= statements.
 (2) $EST and $NONP MSFO= statements.

If any of the files are Xpose-type tables assume that this was an
Xpose run, and construct old file name from the run-number.

If the files have a conserved alphanumeric sequence of
`esn-update-conserved-sequence-length' characters or greater,
assume that sequence was the old file name.

If none of the conditions hold, assume that this file was NOT a copy.

If the file was copied, then update `esn-last-small-name'.

If the file was copied *AND* it had a header, then return true.
Otherwise return false.

To speed up computation, *AND* to keep from mis-updating the
`esn-last-small-name' after the update-function changes output
variables, when computed `esn-save-copy-stat' is set to true and
`esn-save-copy-var' is set to the header copy status.

The `esn-last-small-name' `esn-save-copy-stat' and
`esn-save-copy-var' are updated with `esn-update-header'.  The
`esn-save-copy-stat' should be set to `nil' so that a Save-As,
will register as a copy.

The variable `esn-last-full-name' is updated on open and on running
`esn-update-header'.

Only looks at first $PROBLEM.
"
  (save-restriction
    (if (not (buffer-file-name))
        't
      (if (not (file-exists-p (buffer-file-name)))
          't
        (if (and (esn-use-plt-p)
                 esn-plt-gen-graphics
                 (not (esn-update-file-name-long)))
            (let (
                  (pltg "")
                  (tmp "")
                  (file (buffer-file-name))
                  (script ""))
              (when (string-match "\\([/\\\\]\\)\\([^/\\\\]+\\)\\([/\\\\]\\)\\([^/\\\\]+\\)$" file)
                (setq tmp (match-string 2 file))
                (setq script (concat (replace-match (match-string 1 file) 't 't file) "SCRIPTS-GRAPHICS" (match-string 1 file)))
                (with-temp-buffer
                  (insert tmp)
                  (goto-char (point-min))
                  (while (re-search-forward "\\W+" nil t)
                    (replace-match "-"))
                  (setq tmp (buffer-substring (point-min) (point-max))))
                (when (string-match "^-" tmp)
                  (setq tmp (replace-match "" nil nil tmp)))
                (when (string-match "-$" tmp)
                  (setq tmp (replace-match "" nil nil tmp)))
                (setq tmp (concat "-" tmp)))
              ;; PLT Graphics
              (setq pltg file)
              (when (string-match ".*[/\\\\]" pltg)
                (setq pltg (replace-match "" nil nil pltg)))
              (when (string-match "[.][^.]*$" pltg)
                (setq pltg (replace-match (concat tmp ".pltg") nil nil pltg)))
              (message "Checking for %s" (expand-file-name (concat"../SCRIPTS-GRAPHICS/" pltg)))
              (file-exists-p (expand-file-name (concat"../SCRIPTS-GRAPHICS/" pltg))))
          (if (and esn-last-full-name
                   (not (string= esn-last-full-name ""))       ; Can't tell
                   (string= esn-last-full-name (buffer-file-name)))
              (if esn-save-copy-stat
                  (progn
                    esn-save-copy-var
                    )
                (esn-narrow-to-problem 1)
                (let (
                      (inhibit-read-only 't)
                      (inhibit-point-motion-hooks 't)
                      (case-fold-search 't)
                      (old-file-name "")
                      (header-p nil)
                      (case-fold-search 't)
                      (tmp "")
                      (xpose-tabs
                       (format  "%s\\([0-9]+\\)\\([^ \t\n]*?\\)\\(\\(?:%s\\)\\(?:\\.deriv\\)?\\)\\(?:$\\|[ \t\n]\\)"
                                (eval-when-compile
                                  (regexp-opt
                                   '(
                                     "cotab"
                                     "catab"
                                     "mutab"
                                     "extra"
                                     "sdtab"
                                     "patab"
                                     "cwtab"
                                     "mytab"
                                     "xptab"
                                     "co"
                                     "ca"
                                     "mu"
                                     "xt"
                                     "sd"
                                     "pa"
                                     "cw"
                                     "my"
                                     "xp"
                                     ) 't))
                                (regexp-opt
                                 (list
                                  esn-xpose-data-sim-extension
                                  esn-xpose-data-extension))))
                      (file-length esn-update-conserved-sequence-length)
                      (start-conserved nil)
                      (conserved "")
                      (tmp-conserve "")
                      (tmp-lst "")
                      (i 0)
                      (j 0)
                      (large "")
                      (small "")
                      (ret nil)
                      (xp-ext "")
                      (xp-run "")
                      (tmp "")
                      tmp-conserved
                      )
                  (save-excursion
                    (goto-char (point-min))
                    ;; Get Default Header Information
                    (esn-get-header-old-name 'header-p 'old-file-name 'ret)
                    (if header-p
                        (progn
                          ;;                  (message "Found a header.")
                          )
                      (when (not header-p)
                        ;; Header not present
                        (goto-char (point-min))
                        (while (and (string= old-file-name "")
                                    (re-search-forward "\\(?:MSFO?\\|FILE?\\)[ \t]*[= \t][ \t]*\\(.*\\)" nil t))
                          (setq tmp (match-string 1))
                          (when (string-match "[/\\\\]\\([^/\\\\]*\\)$" tmp)
                            (setq tmp (match-string 1 tmp)))
                          (when (string-match "\\.[^.]*$" tmp)
                            (setq tmp (replace-match "" nil nil tmp)))
                          (when (string-match "['\"]$" tmp)
                            (setq tmp (replace-match "" nil nil tmp)))
                          (when (string-match "^['\"]" tmp)
                            (setq tmp (replace-match "" nil nil tmp)))
                          (if (string-match xpose-tabs tmp)
                              (progn
                                ;; Its an Xpose Table.
                                (setq xp-ext (match-string 3 tmp))
                                (setq xp-run (match-string 2 tmp))
                                (unless (and xp-ext (string-match"\\.deriv$" xp-ext))
                                  ;; If this file name ende in .deriv, skip.
                                  (if (or (not xp-ext)
                                          (= 0 (length xp-ext)))
                                      (progn
                                        ;; Since there is no extension, the best guess is
                                        ;; that the file is a standard Xpose control
                                        ;; stream named runX.mod
                                        (setq old-file-name (concat "run" xp-run)))
                                    ;; There is an extension.  Assume copied from current
                                    ;; directory to try to figure out where the file is
                                    ;; located.
				    
                                    ;; Check for files in the same directory having the form:
                                    ;; ext[-_~]?0*run.ext
                                    (let (
                                          (fls '()))
                                      (mapc (lambda(y)
                                              (mapc (lambda(x)
                                                      (when (string-match
                                                             (format
                                                              "%s[-_~]?0*%s%s"
                                                              (regexp-quote xp-ext)
                                                              (if (string= "0" xp-run)
                                                                  ""
                                                                (regexp-quote xp-run))
                                                              (regexp-quote y)) x)
                                                        (add-to-list 'fls x)))
                                                    (file-expand-wildcards (concat xp-ext "*" xp-run
                                                                                   y))))
                                            esn-default-extension
                                            )
                                      ;; If that file is present and unique,
                                      ;; assume that it is the old file.
                                      (if (= 1 (length fls))
                                          (setq old-file-name (nth 0 fls))
                                        (if (= 0 (length fls))
                                            ;; File not Found.  Assume file was:
                                            ;; extensionRun.xpose-xtension
                                            (progn
                                              (message "Last File not found in current directory")
                                              (setq old-file-name
                                                    (concat xp-ext xp-run
                                                            esn-xpose-default-extension))
                                              (when (string= (buffer-file-name) old-file-name)
                                                (message "Since copied setting to different file name.")
                                                (setq old-file-name (concat xp-ext "-" xp-run
                                                                            esn-xpose-default-extension))))
                                          ;; Multiple matches.
                                          (message "Multiple possibilies for the last file. Use the first")
                                          (setq old-file-name (nth 0 fls))))))))
                            (when start-conserved
                              (unless (string= conserved "")
                                (if (>= (length tmp) (length conserved))
                                    (progn
                                      (setq small conserved)
                                      (setq large tmp))
                                  (setq small tmp)
                                  (setq large conserved))
                                (if (string-match (regexp-quote small) large)
                                    (setq conserved small)
                                  (setq tmp-conserved "")
                                  (setq i 0)
                                  (while (< i (length small))
                                    (setq j (+ i file-length))
                                    (while (<= j (length small))
                                      (when (and
                                             (string-match (regexp-quote
                                                            (substring small i j)) large)
                                             (< (length tmp-conserved)  (- j i)))
                                        (setq tmp-conserved (substring small i j)))
                                      (setq j (+ j 1)))
                                    (setq i (+ i 1)))
                                  (if (string= tmp-conserved "")
                                      (setq conserved "")
                                    (setq conserved tmp-conserved)))))
                            (unless start-conserved
                              (setq start-conserved 't)
                              (setq conserved tmp)))))
                      (widen)
                      (unless (string-match "run[0-9]+" old-file-name)
                        (if (>= (length conserved) 4)
                            (setq old-file-name conserved)
                          (setq old-file-name "")))
                      ;; Setting esn-last-small-name.
                      (if (string= "" old-file-name)
                          (setq esn-last-small-name nil)
                        (setq esn-last-small-name conserved))))
                  (setq esn-save-copy-stat 't)
                  (setq esn-save-copy-var ret)
                  (symbol-value 'ret)))
            ;; Save as
            (let (
                  (inhibit-read-only 't)
                  (inhibit-point-motion-hooks 't)
                  (case-fold-search 't)
                  (old-file esn-last-full-name)
                  (reg (esn-header-regexp-quote))
                  (ret nil))
              (when (and old-file (string-match "/\\([^/]*\\)$" old-file))
                (setq old-file (match-string 1 old-file)))
              (when (and old-file (string-match "\\.[^.]*$" old-file))
                (setq old-file (replace-match "" nil nil old-file)))
              (message "This is a save-as, last file name was %s (small)" old-file)
              (setq esn-last-small-name old-file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward reg nil 't)
                  (setq ret 't)))
              (symbol-value 'ret))))))))
;; Header update functions.
(defun esn-update-header-copyright (&optional time author)
  "Update the header copyright, with optional time for updating.
time is an additional time (usually from file) that should be added.
author is additional author string (usualy from file) that should be added.
"
  (interactive)
  (let (
        (case-fold-search 't)
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (beg nil)
        (begA nil)
        (endA nil)
        (years-lst '())
        (txt "")
        (line ""))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (esn-update-copyright-begin) nil t)
        (setq beg (point))
	
        (if (not (re-search-forward "\\= \\(\\(?: *[0-9]+ *,?\\)+ *\\)" nil t))
            (progn
              (setq begA (point)))
          (setq begA (point))
          (mapc (lambda(x)
                  (let (
                        (ret x))
                    (while (string-match "  +" ret)
                      (setq ret (replace-match " " nil nil ret)))
                    (when (string-match "^ *" ret)
                      (setq ret (replace-match "" nil nil ret)))
                    (when (string-match " *$" ret)
                      (setq ret (replace-match "" nil nil ret)))
                    (unless (string-match "^ *$" ret)
                      (add-to-list 'years-lst ret)))) (split-string (match-string 1) "[^0-9]+")))
        (add-to-list 'years-lst
                     (format-time-string "%Y"))
        (when time
          (add-to-list 'years-lst
                       (format-time-string "%Y")))
        (setq years-lst (sort years-lst 'string-lessp))
        (if (search-forward (esn-update-copyright-end) nil t)
            (progn
              (beginning-of-line)
              (skip-chars-backward "\n")
              (setq endA (point)))
          (end-of-line)
          (setq endA (point)))
        (goto-char beg)
        (beginning-of-line)
        (setq txt (buffer-substring (point) beg))
        ;; Take out years that we known about.
        (while (string-match (format "\\<%s\\>,?" (regexp-opt years-lst 't)) txt)
          (setq txt (replace-match "" nil nil txt)))
        ;; Take out the company.
        (while (string-match (format "\\<%s\\>" (regexp-quote (esn-update-copyright-company))) txt)
          (setq txt (replace-match "" nil nil txt)))
        ;; Take out carriage returns
        (while (string-match "\n *;? *" txt)
          (setq txt (replace-match " " nil nil txt)))
        ;; Take out double spaces.
        (while (string-match "  +" txt)
          (setq txt (replace-match " " nil nil txt)))
        (setq beg (point))
        (setq txt (concat txt " " (mapconcat (lambda(x) x)
                                             years-lst
                                             ", ")))
        (if (not (esn-update-copyright-with-authorship))
            (setq txt (concat txt " " (esn-update-copyright-company)))
          (if (string-match "\\(.*?;+C*\\)" txt)
              (setq line (match-string 1 txt))
            (setq line ";"))
          (setq line (concat "\n" line
                             (make-string (+ 1 (- (length txt) (length line))) ? )))
          (setq txt
                (esn-fix-header-list-colon-name-wrap
                 (concat txt " "
                         (esn-update-header-author
                          author
                          begA endA
                          (esn-update-copyright-company)))
                 line)))
        (goto-char beg)
        (while (string-match ", +," txt)
          (setq txt (replace-match "," 't 't txt)))
        (when (string-match ",[ \t]*+$" txt)
          (setq txt (replace-match "" 't 't txt)))
        (delete-region  beg endA)
        (insert txt)))))
(defun esn-update-header-author (&optional author
                                           beg end
                                           co
                                           )
  "Updates the authors in the header.
author is additional author string (usualy from file) that should be added.
beg beginning of author field (used to add authors to copyright information)
end end of author field (used to add authors to copyright information)
co Company name to add (used to add authors to copyrright information)
"
  (interactive)
  (let* (
         (inhibit-read-only 't)
         (inhibit-point-motion-hooks 't)
         (case-fold-search 't)
         (authors-lst '())
         (beg (or beg nil))
         (end (or end nil))
         (txt "")
         (tmp "")
         (case-fold-search 't)
         (f-attr (file-attributes (buffer-file-name)))
         (uid (nth 2 f-attr))
         (uname (user-login-name))
         (fname (esn-user-full-name))
         (always-add (esn-update-authorship-always-add))
         (justGet beg))
    (when co
      (add-to-list 'always-add co))
    (when author
      (add-to-list 'authors-lst author))
    (save-excursion
      (goto-char (point-min))
      (when (or beg (re-search-forward (regexp-quote (esn-update-author-begin)) nil t))
        (unless beg
          (setq beg (point))
          (beginning-of-line)
          (setq txt (buffer-substring (point) beg))
          (if (re-search-forward (regexp-quote (esn-update-author-end)) nil t)
              (progn
                (beginning-of-line)
                (skip-chars-backward "\n")
                (setq end (point)))
            (end-of-line)
            (setq end (point))))
        (setq tmp (buffer-substring beg end))
        (while (string-match ";" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (while (string-match "\n" tmp)
          (setq tmp (replace-match " " nil nil tmp)))
        (when (and tmp (not (string= tmp "")))
          (mapc (lambda(x)
                  (let (
                        (ret x))
                    (while (string-match "  +" ret)
                      (setq ret (replace-match " " nil nil ret)))
                    (when (string-match "^ *" ret)
                      (setq ret (replace-match "" nil nil ret)))
                    (when (string-match " *$" ret)
                      (setq ret (replace-match "" nil nil ret)))
                    (unless (string-match
                             (format "^ *%s *$" (regexp-opt always-add 't))
                             ret)
                      (unless (string-match "^ *$" ret)
                        (add-to-list 'authors-lst ret)))))
                (split-string tmp ",")))
        (unless (esn-update-authorship-always-add-at-end)
          (mapc (lambda (x)
                  (add-to-list 'authors-lst x))
                always-add
                ))
        (if (esn-update-authorship-name-and-user)
            (add-to-list 'authors-lst (format "%s (%s)" fname
                                              uname))
          (add-to-list 'authors-lst (format "%s" fname)))
        (if esn-update-authorship-sort-function
            (setq authors-lst (sort authors-lst
                                    esn-update-authorship-sort-function)))
        (when (esn-update-authorship-always-add-at-end)
          (setq authors-lst (append (reverse authors-lst)
                                    always-add)))
        (unless justGet
          (setq txt (esn-fix-header-list-colon-name-wrap
                     (concat txt " "
                             (mapconcat (lambda(x) x)
                                        authors-lst ", "))))
          (goto-char beg)
          (beginning-of-line)
          (delete-region (point) end)
          (insert txt))))
    (when justGet
      (setq txt (mapconcat (lambda(x) x)
                           authors-lst ", "))
      (symbol-value 'txt))))
(defun esn-update-header-file-name ()
  "Updates the file name in the header."
  (interactive)
  (let (
        (fn (buffer-file-name))
        (txt "")
        (beg nil)
        (end nil)
        (ilen))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (esn-update-name-begin) nil t)
        (setq beg (point))
        (beginning-of-line)
        (setq txt (buffer-substring (point) beg))
        (setq txt (concat txt " " fn))
        (setq txt (esn-fix-header-file-name-wrap txt))
        (when (string-match (format " *;+C* *%s *" (regexp-quote (esn-update-name-begin))) txt)
          (setq txt (replace-match "" nil nil txt)))
        (esn-update-rep-bef-after (esn-update-name-begin)
                                  (esn-update-name-end)
                                  txt)))))
(defun esn-update-prob-name ()
  "Update the problem name in the Control stream."
  (save-excursion
    (let* (
           (run (esn-runname-noext ))
           (p1 nil)
           (inhibit-read-only 't)
           (inhibit-point-motion-hooks 't)
           (case-fold-search 't)
           (lsn (or esn-last-small-name
                    run
                    )))
      (save-excursion
        (while (re-search-forward (eval-when-compile (esn-reg-record-exp "PRO")) nil t)
          (beginning-of-line)
          (setq p1 (point))
          (end-of-line)
          (narrow-to-region p1 (point))
          (goto-char (point-min))
          (while (re-search-forward (regexp-quote lsn) nil t)
            (replace-match run))
          (goto-char (point-max))
          (widen))))))

(defun esn-update-io-files (&optional output)
  "Update the input/output files in the header."
  (interactive)
  (save-excursion
    (let (
          (itxt "")
          (inhibit-read-only 't)
          (inhibit-point-motion-hooks 't)
          (case-fold-search 't))
      (mapc (lambda(tmp)
              (setq itxt
                    (concat itxt
                            (format
                             (if output
                                 (esn-update-output-files-line)
                               (esn-update-input-files-line))
                             tmp))))
            (if output
                (esn-get-output-files)
              (esn-get-input-files)))
      (setq itxt (esn-fix-header-file-name-wrap itxt))
                                        ;    (while (string-match "\\([^\\]\\)\\\\\\([^\\]\\)" itxt)
                                        ;      (setq itxt (replace-match "\\1\\\\\\\\\\2" nil nil itxt))
                                        ;      )
      (esn-update-rep-bef-after (if output
                                    (esn-update-output-files-begin)
                                  (esn-update-input-files-begin))
                                (if output
                                    (esn-update-output-files-end)
                                  (esn-update-input-files-end))
                                itxt))))

(defun esn-update-save-date ()
  "Update the save date in header."
  (interactive)
  (let (
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't))
    (esn-update-rep-bef-after (esn-update-save-date-before)
                              (esn-update-save-date-after)
                              (if (esn-update-complete-date-time)
                                  (format-time-string (esn-date-time-format))
                                (format-time-string (esn-date-format))))))

(defun esn-header-regexp-quote (&optional header)
  "Quotes the header for searching with regular expressions."
  (let (
        (case-fold-search 't)
        (header-regexp (regexp-quote (or header (esn-header))))
        (lst (list
              (list (esn-update-input-files-begin)
                    (esn-update-input-files-end))
              (list (esn-update-author-begin)
                    (esn-update-author-end))
              (list (esn-update-copyright-begin)
                    (esn-update-copyright-end))
              (list (esn-update-output-files-begin)
                    (esn-update-output-files-end))
              (list (esn-update-modification-begin)
                    (esn-update-modification-end))
              (list (esn-update-version-begin)
                    (esn-update-version-after))
              (list (esn-update-name-begin)
                    (esn-update-name-end))
              (list (esn-update-save-date-before)
                    (esn-update-save-date-after))
              (list (esn-update-purpose-before)
                    (esn-update-purpose-after)))))
    (while (string-match "\\\\\\$\\(small-name\\|initial-author\\|creation-date\\)\\\\\\$" header-regexp)
      (setq header-regexp (replace-match ".*?" nil nil header-regexp)))
    (while (string-match "\n*\\\\\\$\\(after-header\\)\\\\\\$\n*" header-regexp)
      (setq header-regexp (replace-match "" nil nil header-regexp)))
    (unless (string= "$PRO" (esn-update-purpose-before))
      (if (string-match "\\\\\\$PRO.*?\n" header-regexp)
          (setq header-regexp (replace-match "" nil nil header-regexp))))
    (while (string-match "[ \t\n]" (substring header-regexp 0 1))
      (setq header-regexp (substring header-regexp 1)))
    (while (string-match "[ \t\n]" (substring header-regexp -1))
      (setq header-regexp (substring header-regexp 0 -1)))
    (while (string-match "---+" header-regexp)
      (setq header-regexp (replace-match "--+" nil nil header-regexp)))
    (while (string-match "= = = +" header-regexp)
      (setq header-regexp (replace-match "= = +" nil nil header-regexp)))
    (while (string-match "\\*\\*\\*+" header-regexp)
      (setq header-regexp (replace-match "\\\\*\\\\*+" nil nil header-regexp)))
    (while (string-match "\n\n+$" header-regexp)
      (setq header-regexp (replace-match "" nil nil header-regexp)))
    (while (string-match "^\n\n+" header-regexp)
      (setq header-regexp (replace-match "" nil nil header-regexp)))
    (while (string-match ";+\\([^+]\\)" header-regexp)
      (setq header-regexp (replace-match ";+\\1" nil nil header-regexp)))
    (while (string-match "\\(?:;\\+[\n ]\\)\\{2,\\}" header-regexp)
      (setq header-regexp (replace-match ";+ " nil nil header-regexp)))
    (while (string-match "\\(?:;\\+[\n ]\\)\\{2,\\}" header-regexp)
      (setq header-regexp (replace-match ";+ " nil nil header-regexp)))
    (while (string-match ";\\+ \\([^*]\\)" header-regexp)
      (setq header-regexp (replace-match ";+ *\\1" nil nil header-regexp)))
    (mapc (lambda(x)
            (add-to-list 'lst x))
          (esn-update-editable-regions))
    (mapc (lambda(x)
            (with-temp-buffer
              (insert header-regexp)
              (esn-update-rep-bef-after (nth 0 x) (nth 1 x) "\\(?:.\\|\n\\)*?" 't)
              (setq header-regexp (buffer-substring (point-min)
                                                    (point-max)))))
          lst )
    (while (string-match ": +" header-regexp)
      (setq header-regexp (replace-match ":" nil nil header-regexp)))
    (while (string-match (regexp-quote ":\\(?:.") header-regexp)
      (setq header-regexp (replace-match "\\\\(?:." nil nil header-regexp)))
    (symbol-value 'header-regexp)))

(defun esn-mode-add-header-to-file (&optional template)
  (interactive)
  (when (buffer-file-name)
    (let* (
           (case-fold-search 't)
           (inhibit-read-only 't)
           (inhibit-point-motion-hooks 't)
           (header (esn-header))
           (tmp nil)
           (header-regexp (esn-header-regexp-quote))
           (f-attr (file-attributes (buffer-file-name)))
           (uid (nth 2 f-attr))
           (uname (user-login-name uid))
           (fname (esn-user-full-name uid))
           (tm (nth 6 f-attr))
           (prp nil)
           (ret nil)
           (run (esn-runname-noext)))
      (with-temp-buffer
        (insert header)
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (delete-region (point-min) (point))
        (setq header (buffer-substring-no-properties (point-min) (point-max))))
      (save-excursion
        ;; File updating functions
        (goto-char (point-min))
        (unless (re-search-forward header-regexp nil t)
          (setq ret f-attr)
	  
          (unless (or template (not (esn-after-problem))
                      (string= "$PRO" (esn-update-purpose-before)))
            (when (string-match (eval-when-compile (format "%s.*?\n" (esn-reg-record-exp "PRO" 't))) header)
              (setq header (replace-match "" nil nil header))))
          (unless template
            (when (string-match "\\$after-header\\$" header)
              (setq header (replace-match "" nil nil header))))
          (when (not (esn-after-problem))
            (when (string-match (eval-when-compile (format "%s.*?\n" (esn-reg-record-exp "PRO" 't))) header)
              (setq header (concat (replace-match "" nil nil header) "\n" (match-string 0 header)))))
          (when template
            (when (string-match "\\$after-header\\$" header)
              (setq header (replace-match esn-insert-after-header nil nil header))))
          (while (string-match "\\$small-name\\$" header)
            (setq header (replace-match run nil nil header)))
	  
          (while (string-match "\\$initial-author\\$" header)
            (if (esn-update-authorship-name-and-user)
                (setq header (replace-match (format "%s (%s)" fname uname)
                                            nil nil header))
              (setq header (replace-match fname nil nil header))))
          (while (string-match "\\$creation-date\\$" header)
            (if (esn-update-creation-date-time)
                (setq header (replace-match (format-time-string (esn-date-time-format) tm)
                                            nil nil header))
              (setq header (replace-match (format-time-string (esn-date-format) tm) nil nil header))))
          (unless (string= header "")
            (while (string-match "[ \t\n]" (substring header 0 1))
              (setq header (substring header 1)))
            (while (string-match "[ \t\n]" (substring header -1))
              (setq header (substring header 0 -1))))
          (setq header (concat (if template  "" "\n") header "\n"))
          (if template
              (insert header)
            (save-excursion
              (goto-char (point-min))
              (if (esn-after-problem)
                  (if (not (re-search-forward "\\<\\$PRO[A-Z]*" nil t))
                      (progn
                        (goto-char (point-min))
                        (esn-remove-header)
                        (insert header))
                    (end-of-line)
                    (save-excursion
                      (esn-remove-header))
                    (insert header))
                (goto-char (point-min))
                (when (save-excursion
                        (re-search-forward "\\<\\$PRO[A-Z]*" nil t))
                  (when (string-match (eval-when-compile (format "%s.*?\n" (esn-reg-record-exp "PRO" 't))) header)
                    (setq header (concat (replace-match "" nil nil header)))))
                (save-excursion
                  (esn-remove-header))
                (insert header)
                (goto-char (point-min))
                (skip-chars-forward " \t\n")
                (delete-region (point-min) (point)))))
          (when (and (not template) (esn-is-cp))
            ;; First fill in the saves.
            (mapc (lambda(x)
                    (let (
                          (before (nth 0 x))
                          (after (nth 1 x))
                          (rep (nth 2 x)))
                      (when before
                        (save-excursion
                          (goto-char (point-min))
                          (when (search-forward before nil 't)
                            (goto-char (point-min))
                            (esn-update-rep-bef-after before after rep 't))))))
                  esn-update-cp-save-vals)
            (unless (string= esn-update-cp-purpose-save "")
              (esn-update-rep-bef-after
               (esn-update-purpose-before)
               (esn-update-purpose-after)
               esn-update-cp-purpose-save)
              (setq esn-update-cp-purpose-save "")))))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward header-regexp nil t)
          (let (
                (eob nil))
            (save-excursion
              (skip-chars-forward " \n")
              (setq eob (eobp)))
            (when (and (not eob) (looking-at "\n+"))
              (replace-match "\n")))))
      (symbol-value 'ret))))

(defun esn-update-open ()
  "Update OPEN(50,\"FILE\") statments."
  (save-restriction
    (let (
          (inhibit-read-only 't)
          (inhibit-point-motion-hooks 't)
          (fn (esn-runname-noext 't))
          (case-fold-search 't)
          (lsn (or esn-last-small-name
                   (esn-runname-noext 't))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward esn-fortran-start nil t)
          (save-excursion
            (let ((ts-start (+ (point)))
                  (ts-end (if (search-forward esn-fortran-stop nil t)
                              (- (point) (length esn-fortran-stop))
                            nil)))
              (when ts-end
                (narrow-to-region ts-start ts-end)
                (goto-char ts-start)
                (when (re-search-forward (regexp-quote lsn) nil t)
                  (replace-match fn))
                (goto-char ts-end)
                (goto-char (point-max))
                (widen)))))))))

(defun esn-msfo-name ( &optional nonparametric  problem-number)
  "Gets MSFO name for the current file"
  (let (
        (run (esn-runname-noext 't))
        (i (or problem-number (esn-problem-number))))
    (if (and esn-plt-use-msfo-outputfile (esn-use-plt-p))
        "msfo.outputfile"
      (concat esn-completing-current-directory
              (esn-mangle-problem-file-name
               (concat run
                       (if (not nonparametric)
                           esn-msfo-est
                         esn-msfo-non))
               i)))))

(defun esn-update-msfo ()
  "Update taking into consideration subproblems, and $NONPARAMETRIC"
  (interactive)
  (save-restriction
    (save-excursion
      (let (
            (np (esn-num-problems))
            (i 0)
	    
            (est nil)
            (tmp "")
            (case-fold-search 't))
        (while (< i np)
          (setq i (+ 1 i))
          (esn-narrow-to-problem i)
          (goto-char (point-min))
          (setq est nil)
          (while (re-search-forward (eval-when-compile (esn-reg-record-exp '("EST" "NON") nil 't)) nil t)
            (setq est (string-match "^EST$" (match-string 1)))
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq tmp (esn-msfo-name (not est) i))
            (when (re-search-forward esn-msfo-start nil t)
              (when (looking-at "[ \t]*[^ \t\n]*")
                (replace-match tmp)))
            (goto-char (point-max))
            (widen)
            (esn-narrow-to-problem i))
          (goto-char (point-max))
          (widen))))))


(defun esn-update-tables ()
  "A Function that updates tables both names and contents.
Updates table names.

 * If there is only one table name per problem blow it away and
   name it after the file.  This occurs only when Xpose spoofing
   is NOT used.

 * If there are multiple table names per problem, just replace
   the old name with the new name.

 * If Xpose tables are invoked, don't play with table names at
   all, let Xpose extensions handle it.

 * Update table variables based on Xpose, and if Xpose is 
   disabled, put them all into one table based on user options.
"
  (interactive)
  (save-restriction
    (save-excursion
      (let* (
             (np (esn-num-problems))
             (i 0)
             (run (esn-runname-noext 't))
             (inhibit-read-only 't)
             (inhibit-point-motion-hooks 't)
             (case-fold-search 't)
             (ntab 0)
             (tab-re (eval-when-compile (esn-reg-record-exp "TAB" 't 't)))
             (file-re "FILE= [ \t]*")
             (tmp "")
             (lsn (or esn-last-small-name run))
             (rn (esn-xpose-run-number))
             (xpose-tabs
              (format  "%s\\([0-9]+\\)\\([^ \t\n]*?\\)\\(\\(?:%s\\)\\(?:\\.deriv\\)?\\)\\(?:$\\|[ \t\n]\\)"
                       (eval-when-compile
                         (regexp-opt
                          '(
                            "cotab"
                            "catab"
                            "mutab"
                            "extra"
                            "sdtab"
                            "patab"
                            "cwtab"
                            "mytab"
                            "xptab"
                            "co"
                            "ca"
                            "mu"
                            "xt"
                            "sd"
                            "pa"
                            "cw"
                            "my"
                            "xp"
                            ) 't))
                       (regexp-opt
                        (list
                         esn-xpose-data-sim-extension
                         esn-xpose-data-extension)))))
        (while (< i np)
          (setq i (+ 1 i))
          (esn-narrow-to-problem i)
          (goto-char (point-min))
          (setq ntab 0)
          (while (and (<= ntab  1)
                      (re-search-forward tab-re nil t))
            (setq ntab (+ 1 ntab)))
          (goto-char (point-min))
          (while (re-search-forward tab-re nil t)
            (esn-narrow-rec)
            (goto-char (point-min))
            (when (re-search-forward file-re nil t)
              (if (and rn (looking-at xpose-tabs))
                  (progn
                    (replace-match (format "\\1%s%s" rn (esn-get-extension (or (match-string 4) "")))))
                (when (looking-at "\\([^ \t]*\\)")
                  (if (and esn-update-table-force-update-if-only-one-output
                           (= ntab 1)
                           (not esn-xpose-tables-for-non-xpose-ctl-streams))
                      (replace-match
                       (esn-mangle-problem-file-name
                        (concat run esn-table-extension)
                        i ))
                    (setq tmp (match-string 1))
                    (when (string-match (regexp-quote lsn) tmp)
                      (setq tmp (replace-match run nil nil tmp)))
                    (when (looking-at "[^ \t]*")
                      (replace-match tmp))))))
            (goto-char (point-max))
            (widen)
            (esn-narrow-to-problem i)))
        (goto-char (point-max))
        (widen)))))

(defun esn-update-relative-paths ()
  "Function that updates the paths on a save.
If a relative path is smaller, use it.
If an absolute path is smaller, use it."
  (let* ((case-fold-search 't)
         (cdir (if esn-default-directory
                   (expand-file-name esn-default-directory)
                 esn-default-directory))
         (rcdir esn-completing-current-directory)
         (ncdir nil)
         (nrcdir nil)
         (win esn-w32))
    (if (not esn-compress-paths)
        nil
      (while (not (string-match "^\\(?:[A-Za-z]:\\)?/$" cdir))
        (if (string-match "[^/]*/$" cdir)
            (progn
              (setq ncdir (replace-match "" nil nil cdir))
              (setq nrcdir (concat rcdir "../"))
              (if (< (length rcdir) (length cdir))
                  (save-excursion
                    ;;(message "Replacing `\\([\"']?\\)%s\\([A-Za-z]\\)' with `\\1%s\\2'" (regexp-quote cdir) rcdir)
                    (goto-char (point-min))
                    (while (re-search-forward (format "\\([\"']?\\)%s\\([A-Za-z]\\)" (regexp-quote cdir)) nil t)
                      (replace-match (concat "\\1" rcdir "\\2"))))
                ;; (message "Replacing `\\([\"']?\\)%s\\([A-Za-z]\\)' with `%s'" (regexp-quote rcdir) cdir)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward  (format "\\([\"']?\\)%s\\([A-Za-z]\\)" (regexp-quote rcdir)) nil t)
                    (replace-match (concat "\\1" cdir "\\2")))))
              (setq cdir ncdir)
              (setq rcdir nrcdir)))))
    (if (not (and win (and (not (= 4 esn-save-coding-system)) esn-fix-windows-slashes))) nil
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\(?:[a-zA-Z]:\\|\\.\\.?[/\\\\]\\).*?/" nil t)
          (save-excursion
            (if (re-search-backward "/\\= " nil t)
                (unless (esn-in-comment-p)
                  (replace-match "\\\\")))))))))

(defun esn-wrap-80 ()
  "Wraps items over `esn-character-limit'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "^.\\{%s,\\}" (+ 0 esn-character-limit)) nil t)
      (let ((rec (esn-get-current-record)))
        (save-excursion
          (if (save-match-data (string-match "^\\(TAB\\|INP\\|BIN\\)" rec))
              (progn
                (esn-align-tab-hook))
            (unless (save-match-data
                      (string-match
                       (format "^%s$" (regexp-opt esn-records-not-wrapped 't)) rec))
              (unless (esn-in-comment-p)
                (esn-magic-wrap nil nil 't)))))))))

(defun esn-update-essentials ()
  "Update header essentials."
  (interactive)
  (when esn-wfn-extended
    (esn-fix-numbering))
  (when esn-wfn-caps
    (esn-upcase-buffer))
  ;; We need to make sure there are NO TABS in the file (nonmem will choke!)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max))))
  ;; (message "Have a trailing return on the file")
  (save-excursion
    (goto-char (point-max))
    (backward-char 1)
    (if (not (looking-at "\n"))
        (progn
          (forward-char 1)
          (insert "\n")))))

;;  default-tab-width
(defun esn-update-header ()
  "Function that is run on save."
  (interactive)
  (setq esn-vc-committing-buffer (buffer-file-name))
  (when esn-plt-add-wide-to-data
    (esn-plt-add-wide))
  (save-excursion
    (when esn-mode-generate-secondary-parameters
      (esn-secondary-parameters))
    (when (and esn-xmind-update-xmind-comment
               (esn-use-xmind-p))
      (esn-add-update-topics))
    (setq esn-table-split-pt (point))
    (esn-80-clear)
    (setq esn-run-save-fn 't))
  (save-excursion
    (save-excursion
      (if (esn-xpose-save) nil
        (let ((inhibit-read-only 't)
              (inhibit-point-motion-hooks 't)
              (case-fold-search 't)
              (bs (buffer-substring (point-min) (point-max)))
              (lstlen 0)
              (i 0)
              (hutmp "")
              (inhibit-read-only 't)
              (fattr nil)
              (uid nil)
              (uname nil)
              (fname nil)
              (tm nil)
              (author nil)
              (tmp "")
              (cp (esn-is-cp))
              (max-lisp-eval-depth 1024))
          (esn-file-coding-system)
          ;; (message "Add Header.")
          (esn-update-relative-paths)                      ;; Update relative Paths.
          ;; CWRES first in case tables are needed.
          (when esn-automatically-generate-cwres
            (esn-cwres))
          ;; Next tables
          (esn-update-name)
          (when esn-update-add-header-if-not-present
            (when (and
                   (esn-update-redo-header-if-cp)
                   cp)
              (esn-remove-header))
            (setq fattr (esn-mode-add-header-to-file))
            (when fattr
              (setq uid (nth 2 fattr))
              (setq uname (user-login-name uid))
              (setq fname (esn-user-full-name uid))
              (setq tm (nth 6 fattr))
              (if (esn-update-authorship-name-and-user)
                  (setq author (format "%s (%s)" fname
                                       uname))
                (setq author (format "%s" fname))))
            ;; Header portions that are updated EVERY TIME.
            (when esn-update-problem-file-name
              (esn-update-prob-name))
            (when (esn-update-complete-date)
              (esn-update-save-date))
            (when (esn-update-input-files)
              (esn-update-io-files nil))
            (when (esn-update-output-files)
              (esn-update-io-files 't))
            (when (esn-update-file-name-long)
              (esn-update-header-file-name ))
            (when (esn-update-modified-by)
              (esn-update-modification))
            (when (esn-update-copyright)
              (esn-update-header-copyright tm author))
            (when (esn-update-authorship)
              (esn-update-header-author author))
	    
            (when (esn-update-purpose)
              (setq tmp (esn-update-get-purpose))
              (when (not (and cp (esn-update-purpose-cp)))
                (when (or (not tmp) (string-match "^[ \t\n ;]*$" tmp))
                  (esn-update-prompt-purpose)))
              (when (and cp (esn-update-purpose-cp))
                (when esn-last-purpose
                  (message "%s, Current Purpose: %s" esn-last-purpose (esn-update-get-purpose))
                  (when (or (string-match "^[ \t\n;]*$" (esn-update-get-purpose))
                            (string= esn-last-purpose (esn-update-get-purpose)))
                    (esn-update-prompt-purpose 't))))
              (when (string= tmp "")
                ;; Update purpose when empty.
                (esn-update-prompt-purpose)))
            (when (esn-mirror-problem-purpose)
              (esn-mirror-purpose-problem))
            (when (esn-use-pdx-p)
              (esn-pdx-set-run-number))
            (when esn-drop-aliases
              (esn-fix-input-aliases))
            (unless cp
              (when (esn-update-version)
                (setq tmp (esn-update-get-version))
                (unless tmp
                  (esn-update-version))))
            (when cp
              ;; Copy functions.
              (when (esn-update-cp-version)
                (esn-update-version))))
	  
          ;;
          ;; List of records and other items where one should do (esn-indent-line) before saving.
          ;;nput
	  
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (format "^\\(%s[A-Z]*\\)[ \t]+" (regexp-opt esn-update-list)) nil 't)
                                        ;         (message "Formatting %s record" (match-string 1))
              (end-of-line)
              (esn-indent-line)
              (end-of-line))
            (setq i (+ i 1)))
          (esn-update-essentials)
          (message "Finished automatically updating portions of the file (headers etc...)"))))
    ;; Update Xmind object (if supported)
    (when (esn-use-xmind-p)
      (esn-xmind-update-current-file))
    ;; Reupdate relevant information.
    (esn-update-save-variables)
    ;; Add to version control.
    (if (and esn-vc-upon-new-modification-log-line
             esn-updated-modification-line)
        (setq esn-commit-last-version 't)
      (if (esn-is-cp)
          (setq esn-commit-last-version 't)))
    (esn-should-be-new-version nil nil 't)
    (when (esn-use-plt-p)
      ;; Never commit with PLT tools.
      (setq esn-commit-last-version nil)
      (esn-plt-auto)
      ;; Create Xpose script when using PLT tools.
      ;; (when (and esn-plt-gen-xpose esn-plt-gen-xpose-r-script)
      ;;   (esn-mode-xpose-gen-summary))
      )
    (when esn-vc-first-prompt
      (setq esn-vc-first-prompt nil)
      (esn-on-save-use-vc))
    
    (setq esn-commit-last-version nil)
    ;; (when (and (esn-use-xpose-p) esn-mode-xpose-summary-document)
    ;;   (esn-mode-xpose-gen-summary))
    (esn-wrap-80)
    (if esn-table-split-pred
        (progn
          (esn-table-split)))
    (delete-region (point-min) (save-excursion (goto-char (point-min)) (skip-chars-forward " \t\n") (point)))
                                        ;  (message "Wrote %s" (buffer-file-name))
    nil))

;;;###autoload
(defun esn-update-save-variables ()
  "* This updates the save variables upon  opening a file."
  (setq esn-save-copy-stat nil)
  (setq esn-last-full-name (buffer-file-name))
  (setq esn-last-purpose (esn-update-get-purpose))
  (esn-is-cp)
  (setq esn-default-directory (buffer-file-name))
  (when esn-default-directory
    (if (string-match "/\\([^/]*\\)$" esn-default-directory)
        (setq esn-default-directory (replace-match "/" nil nil esn-default-directory)))))

(defun esn-after-save-function ()
  "Function called after the file has been saved AND on opening a file."
  (interactive)
  (setq esn-update-get-version-cache nil)
  (setq esn-var-names '())
  (setq esn-wfn-dups '())
  (setq esn-vc-first-prompt 't)
  (save-excursion
    (when (and esn-xmind-update-xmind-comment
               (esn-use-xmind-p)
               (and (esn-xmind-default-map-name) (file-exists-p (esn-xmind-default-map-name))))
      (esn-add-update-topics)                          ; Shouldn't set buffer modification.
      )
    ;; We need to make sure there are NO TABS in the file (some regular
    ;; expressions don't work correctly)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
    (esn-edit)
    (if esn-table-split-pred
        (esn-table-pred-undo-split))
    (if esn-wfn-extended
        (progn
          (esn-r 'esn-extended)
          (if esn-wfn-prefer-lc
              (esn-downcase-buffer))
          (esn-undo-numbering)
          (set (make-local-variable 'esn-saved-var-names) esn-var-names)))
    (esn-hide-header)
    (when esn-use-hyperlinks
      (esn-make-link-overlays))
    (esn-set-mode-name)
                                        ;    (message "Wrote/Opened %s" (buffer-file-name))
    (setq esn-run-save-fn nil)
    (set-buffer-modified-p nil)))


(defun esn-fix-header-list-colon-name-wrap (txt &optional ln)
  "Fixes header wrapping issues for colon lists.  If ln is a line, then skip looking for the colon and use ln as the line break."
  (let ((ret txt)
        (tmp "")
        (findColon ln)
        (line (or ln "\n;")))
    (when (string-match " *\\(;+C*\\)" txt)
      (setq line (concat "\n" (match-string 1 txt)))
      (setq findColon line))
    (while (string-match (format "^\\(.\\{%s,\\}\\)$" esn-character-limit) ret)
      (setq tmp (concat (match-string 1 ret)))
      (when (and findColon (string-match "^\\(.*?;+C*\\)\\(.*?: *\\)" tmp))
        (setq line (concat "\n" (match-string 1 tmp)
                           (make-string (length (match-string 2 tmp)) ? ))))
      (with-temp-buffer
        (insert tmp)
        (goto-char (point-min))
        (forward-char esn-character-limit)
        (if (re-search-backward "," nil t)
            (progn
              (skip-chars-forward ", ")
              (insert line))
          (if (re-search-backward " " nil t)
              (progn
                (skip-chars-forward ", ")
                (insert line))
            (skip-chars-forward ", ")
            (insert line)))
        (beginning-of-line)
        (while (looking-at (format "^.\\{%s,\\}" esn-character-limit))
          (forward-char esn-character-limit)
          (if (re-search-backward "," nil t)
              (progn
                (skip-chars-forward ", ")
                (insert line))
            (if (re-search-backward " " nil t)
                (progn
                  (skip-chars-forward ", ")
                  (insert line))
              (skip-chars-forward ", ")
              (insert line)))
          (beginning-of-line))
        (goto-char (point-min))
        (while (re-search-forward " +$" nil t)
          (replace-match ""))
        (setq tmp (buffer-substring (point-min) (point-max))))
      (when (string-match (format "^\\(.\\{%s,\\}\\)$" esn-character-limit) ret)
        (setq ret (replace-match tmp nil nil ret))))
    (symbol-value 'ret)))

(defun esn-fix-header-file-name-wrap (txt)
  "Fixes header wrapping issues for file names."
  (let ((tmp "")
        (tmp2 "")
        (itxt txt)
        (win esn-w32))
    ;; Fix the \ problem in windows.
    (while (string-match "\\\\" itxt)
      (setq itxt (replace-match "/" nil nil itxt)))
    ;; Fix Wrapping problems
    (setq itxt (concat itxt "\n"))
    (while (string-match (format "^\\(.\\{%s,\\}\\)$" esn-character-limit) itxt)
      (setq tmp (concat (match-string 1 itxt) "\n"))
      (setq tmp2 "")
      (while (string-match (format "^\\(.\\{%s,\\}\\)$" esn-character-limit) tmp)
        (if (not (string-match "\\(/[^/]*\\)$" tmp))
            (progn
                                        ;             (message "%s not a recognized file, and cannot be wrapped." itxt)
              (setq tmp ""))
          (setq tmp2 (concat (match-string 1 tmp) tmp2))
          (setq tmp (replace-match "" nil nil tmp))))
      (if (not (string-match "^\\(.*?;+C*\\)\\(.*?\\)\\(/\\|\\<[a-zA-Z]:\\)" tmp))
                                        ;         (message "%s cannot be wrapped for some reason." itxt)
          nil
        (setq tmp2 (concat (match-string 1 tmp)  (make-string (length (match-string 2 tmp)) ? ) "  " tmp2))
        (setq tmp (concat tmp "\n" tmp2)))
      (if (not (string-match (format "^\\(.\\{%s,\\}\\)$" esn-character-limit) itxt))
          (error "Something misbehaving.")
        (setq itxt (replace-match tmp nil nil itxt))))
    
    (while (string-match "\n\\{2,\\}" itxt)
      (setq itxt (replace-match "\n" nil nil itxt)))
    (if (string= (substring itxt -1) "\n")
        (setq itxt (substring itxt 0 -1)))
    (while (and
            (and win (and (not (= 4 esn-save-coding-system)) esn-fix-windows-slashes))
            (string-match "/" itxt))
      (setq itxt (replace-match "\\\\" nil nil itxt)))
    (symbol-value 'itxt)))

(defun esn-update-name ()
  "Updating file names"
  (interactive)
  (run-hooks 'esn-before-update-hook)
  (when esn-update-table
    (esn-update-tables))
  ;; Update tables
  (run-hooks 'esn-table-update-hook)
  (when (or esn-update-table
            esn-pdx-generate-tables
            esn-plt-generate-tables
            esn-generate-one-table)
    (esn-table-split 't))
  (when esn-update-msfo
    (esn-update-msfo))
  (if esn-update-fortran
      (esn-update-open)))

(provide 'esn-update)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-update.el ends here
