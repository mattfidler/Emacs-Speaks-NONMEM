;;; esn-get-cookies.el --- This generates cookies based on NONMEM help
;;
;; Filename: esn-get-cookies.el
;; Description: Get code highlighting and cookies for NONMEM based on NONMEM help.
;; Author: Matthew L. Fidler
;; Maintainer:  Matthew L. Fidler
;; Created: Thu Jul 29 12:28:00 2010 (-0500)
;; Version:
;; Last-Updated: Thu Feb 16 10:37:41 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 1856
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
;; 10-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Dec 10 14:56:16 2010 (-0600) #1833 (Matthew L. Fidler)
;;    Generate hooks for pre and post-commands in each record
;; 09-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Dec  9 17:07:41 2010 (-0600) #1822 (Matthew L. Fidler)
;;    Added help index cookies (for linking help data)
;; 09-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Dec  9 12:37:42 2010 (-0600) #1738 (Matthew L. Fidler)
;;    Changed path locations.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 10:49:07 2010 (-0500) #1706 (Matthew L. Fidler)
;;    Added variables.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 10:40:07 2010 (-0500) #1463 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 13-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 13 08:47:31 2010 (-0500) #1241 (Matthew L. Fidler)
;;
;;    Changed from looking for an Option/Value and next KNOWN record
;;    to looking for Option/Value and next Known or Unknown record.
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
(require 'esn-autoloads)
;;(require 'esn-pk)
(defvar esn-nm-cookie-dir nil)
(defvar esn-nm-cookie-ver nil)
(setq debug-on-quit 't)
;;;###autoload
(defun esn-gen-cookies ()
  "* Generate Cookies"
  (interactive)
  (let ((all-recs '())
        (all-abbrev '()))
    (setq esn-nm-cookie-dir "~usb/Documents/NONMEM/nm7/help")
    (setq esn-nm-cookie-ver "7")
    (esn-gen-cookies-1)
    (esn-gen-help)
    (setq esn-nm-cookie-dir "~usb/Documents/NONMEM/nmvi/help")
    (setq esn-nm-cookie-ver "6")
    (esn-gen-cookies-1)
    (esn-gen-help)
    (setq esn-nm-cookie-dir "~usb/Documents/NONMEM/nmv/help")
    (setq esn-nm-cookie-ver "5")
    (esn-gen-cookies-1)
    (esn-gen-help)
    (with-temp-file (concat esn-path "lisp/esn-rec-hooks.el")
      (insert ";;; Pre and post-command record hooks for all known records in NONMEM (auto-generated)\n")
      (mapc (lambda(rec)
              (insert "(defvar esn-" rec "-post-command-hook nil)\n"
                      "(defvar esn-" rec "-pre-command-hook nil)\n"
                      "(defvar esn-" rec "-modification-hook nil)\n"))
            all-recs)
      (insert "(defvar esn-all-abbrev-recs '(")
      (mapc (lambda(rec)
              (insert "\"" (downcase rec) "\" "))
            all-abbrev)
      (insert "))\n(defvar esn-not-abbrev-rec '(")
      (mapc (lambda(rec)
              (unless (member (upcase rec) all-abbrev )
                (insert "\"" rec "\" ")))
            all-recs)
      (insert "))\n")
      (insert "(provide 'esn-rec-hooks)\n")
      )
    (esn-gen-cookies-vars))
  )
(defun esn-gen-cookies-vars ()
  "* Generate variable cookies."
  ;; Currently get from variables.for.  The files for 6 and 7 are quite different.
  (interactive)
  (with-temp-buffer
    (insert-file-contents "f:/Documents/NONMEM/nm7/help/variable.for")
    (goto-char (point-min))
    (let (
          (division nil)
          (first nil)
          (next nil)
          (first-list '())
          (second-list '())
          (ret '())
          )
      (when (re-search-forward "\\<MODULES:" nil t)
        (setq division (match-beginning 0))
        )
      (goto-char (point-min))
      (while (re-search-forward "\\<[$][A-Z]+" division 't)
        (setq first (esn-rec3 (match-string 0)))
        (save-excursion
          (if (re-search-forward "\\<[$][A-Z]+" division 't)
              (setq next (match-beginning 0))
            (setq next division)
            )
          )
        (setq first-list '())
        (while (re-search-forward ":[ \t]\\(.*\\)$" next 't)
          (setq first-list (append first-list (split-string (match-string 1) "[ ,\t\n]+" 't)))
          )
        (add-to-list 'second-list (list first first-list))
        )
      (setq ret second-list)
      (goto-char division)
      (when (re-search-forward "REFERENCES:" nil t)
        (setq next (match-beginning 0)))
      (goto-char division)
      (while (re-search-forward ":[ \t]\\(.*\\)$" next 't)
        (setq first-list (split-string (match-string 1) "[ ,\t\n]+" 't))
        (forward-line 1)
        (mapc
         (lambda(x)
           (let (
                 (rec (esn-rec3 x))
                 )
             (setq ret
                   (mapcar
                    (lambda (y)
                      (if (string= rec (nth 0 y))
                          (list rec (append (nth 1 y) first-list))
                        y))
                    ret))))
         (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) nil 't)))
      (with-temp-file (concat "./esn-nm-vars-7.el")
        (insert ";; -*-no-byte-compile: t; -*-\n")
        (insert "(defvar esn-nm-vars-7 '(\n")
        (mapc (lambda(x)
                (let (
                      (tmp (concat "\\<" (regexp-opt (append (mapcar (lambda(x) (concat x "!")) (nth 1 x)) (list "z")
                                                             (if (string= "INF" (nth 0 x))
                                                                 (list "OMEGA" "SIGMA")
                                                               '())
                                                             ))))
                      )
                  (while (string-match "~" tmp)
                    (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
                  (while (string-match "(#)" tmp)
			 (setq tmp (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp)))
                  (while (string-match "#" tmp)
                    (setq tmp (replace-match "[0-9]+" nil nil tmp)))
                  (while (string-match "!" tmp)
                    (setq tmp (replace-match "\\\\>" nil nil tmp)))
                  (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
                    (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                    )
                  (when (string-match "z" tmp)
                    (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                    )
                  (setq tmp (esn-gen-cookies-quote tmp))
                  (insert "(\"")
                  (insert (nth 0 x))
                  (insert "\" \"")
                  (insert tmp)
                  (insert "\")\n")
                  )
                )
              ret
              )
        (insert ") \"* NONMEM 7 built-in variables (from variables.for);  Added OMEGA and SIGMA to $INFN.  Its not defined in variables.for, but is used in CWRES code.\")\n(provide 'esn-nm-vars-7)\n")
        )
      )
    )
  (with-temp-buffer
    (insert-file-contents "f:/Documents/NONMEM/nmvi/help/variable.for")
    (goto-char (point-min))
    (when (re-search-forward "AES" nil t)
      (let (
            (start (point-at-bol))
            division
            first
            next
            first-list
            second-list
            ret
            recs
            vars
            )
        (setq first-list '())
        (setq second-list '())
        (setq ret '())
        (when (re-search-forward "^[ \t]+COMMON" nil t)
          (setq division (point-at-bol)))
        (goto-char start)
        (while (re-search-forward "\\<[$][A-Z]+" division 't)
          (setq first (esn-rec3 (match-string 0)))
          (save-excursion
            (if (re-search-forward "\\<[$][A-Z]+" division 't)
                (setq next (match-beginning 0))
              (setq next division)
              )
            )
          
          (setq first-list '())
          (while (re-search-forward "^[ \t]+[^ \t]+[ \t]+\\(.*\\)$" next 't)
            
            (setq first-list (append first-list (split-string (match-string 1) "[ ,\t\n]+" 't)))
            )
          (add-to-list 'second-list (list first first-list))
          )
        (goto-char division)
        (goto-char (point-at-eol))
        (setq ret second-list)
        (while (re-search-forward "^ [A-Z0-9]+  +\\(\\(?:[A-Z0-9_]+ \\)+\\)  +\\(.*\\)" nil t)
          (setq recs (match-string 2))
          (setq vars (match-string 1))
          (forward-line 1)
          (goto-char (point-at-bol))
          (while (looking-at "^   +\\(.*\\)")
            (setq vars (concat vars " " (match-string 1)))
            (forward-line 1)
            (goto-char (point-at-bol))
            )
          (setq first-list (split-string vars nil 't))
          (mapc
           (lambda(x)
             (let (
                   (rec (esn-rec3 x))
                   )
               (setq ret
                     (mapcar
                      (lambda (y)
                        (if (string= rec (nth 0 y))
                            (list rec (append (nth 1 y) first-list))
                          y)
                        )
                      ret
                      )
                     )
               )
             )
           (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) nil 't)
           )
          )
        (with-temp-file (concat esn-path "etc/cookies/esn-nm-vars-6.el")
          (insert ";; -*-no-byte-compile: t; -*-\n")
          (insert "(defvar esn-nm-vars-6 '(\n")
          (mapc (lambda(x)
                  (let (
                        (tmp (concat "\\<" (regexp-opt (append (mapcar (lambda(x) (concat x "!")) (nth 1 x)) (list "z")
                                                               (if (string= "INF" (nth 0 x))
                                                                   (list "OMEGA" "SIGMA")
                                                                 '()) 
                                                               ))))
                        )
                    (while (string-match "~" tmp)
                      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
                    (while (string-match "(#)" tmp)
                      (setq tmp (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp)))
                    (while (string-match "#" tmp)
                      (setq tmp (replace-match "[0-9]+" nil nil tmp)))
                    (while (string-match "!" tmp)
                      (setq tmp (replace-match "\\\\>" nil nil tmp)))
                    (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
                      (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                      )
                    (when (string-match "z" tmp)
                      (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                      )
                    (setq tmp (esn-gen-cookies-quote tmp))
                    (insert "(\"")
                    (insert (nth 0 x))
                    (insert "\" \"")
                    (insert tmp)
                    (insert "\")\n")
                    )
                  )
                ret
                )
          (insert ") \"* NONMEM 6 built-in variables (from variables.for); Added OMEGA and SIGMA to $INFN.  Its not defined in variables.for, but is used in CWRES code.\")\n(provide 'esn-nm-vars-6)\n"))))))

(defun esn-gen-help ()
  "* Generate Help Index"
  (interactive)
  (let (
        file
        title
        keywords
        index-lst
        ret
        )
    (with-temp-buffer
      (insert-file-contents (concat esn-nm-cookie-dir "/index"))
      (goto-char (point-min))
      (while (re-search-forward "~~" nil t)
        (replace-match "~"))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*~[ \t]*" nil t)
        (replace-match "~"))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*?\\)~\\(.*?\\)~\\(.*\\)$" nil t)
        (setq file (match-string 1))
        (setq title (match-string 2))
        (add-to-list 'index-lst (list title file))
        (save-match-data
          (setq keywords (split-string (match-string 3) nil 't)))
        (replace-match "")
        (mapc
         (lambda(key)
           (add-to-list 'index-lst (list key file)))
         keywords)))
    (setq ret (concat "'("
                      (mapconcat
                       (lambda(elt)
                         (concat "(\"" (nth 0 elt) "\" \"" (nth 1 elt) "\")"))
                       index-lst " ") ")"))
    (with-temp-file (concat esn-path "etc/cookies/esn-help-index-" esn-nm-cookie-ver ".el")
      (insert ";; -*-no-byte-compile: t; -*-\n")
      (insert (format "(defvar esn-nm-help-index-%s %s\n\"Index of help cross-references\")\n(defvar esn-nm-help-index-reg-%s \"%s\")\n(provide 'esn-help-index-%s)\n"
                      esn-nm-cookie-ver ret
                      esn-nm-cookie-ver
                      (esn-gen-cookies-quote (regexp-opt (mapcar (lambda(x) (nth 0 x)) index-lst) 'words))
                      esn-nm-cookie-ver)))))


(defun esn-add-to-opt-list (list-var option txt &optional value paren)
  "Adds to an option list"
  (let (
        (val value)
        (opt option)
        text
                                        ;        (debug-on-error 't)
        )
    (setq text (concat option (if value
                                  (concat (if paren "[" "") "=" value)
                                "") "\n" txt))
    (if (string= opt "-NEG")
        (setq opt "-")
      )
    (if (not val)
        (add-to-list list-var (list opt text))
      (when (and
             (string= val "c")
             (string= opt "MATRIX")
             )
        (setq val "R,S") ; R and S matrices.
        )
      (when (and
             (string= val "s1")
             (string= opt "MUM")
             )
        (setq val "(mum)")
        )
      (when (and
             (string= val "s2")
             (string= opt "GRD")
             )
        (setq val "(grd)")
        )
      (when (and
             (string= val "s3")
             (string= opt "FORMAT")
             )
        (setq val "(s3format)")
        )
      (when (and
             (string= val "list")
             (string= opt "ZERO")
             )
        (setq val "zerolist")
        )
      (when (string= val "(list)")
        (cond
         ( (string= opt "TRANSLATE")
           (setq val "translist")
           )
         ( (or (string= opt "ACCEPT")
               (string= opt "IGNORE")
               )
           (setq val "skip") ; Don't add these options.
           )
         )
        )
      (while (string-match "\\]\\[" val)
        (setq val (replace-match "," nil nil val)))
      (while (string-match "|" val)
        (setq val (replace-match "," nil nil val))
        )
      (while (string-match "\\(\\[\\|\\]\\)" val)
        (setq val (replace-match "" nil nil val))
        )
      (when (string-match "\\([^ ]\\) +([^)]+)$" val)
        (setq val (replace-match "\\1" nil nil val)))
      (while (string-match "[ \t]+" val)
        (setq val (replace-match "" nil nil val))
        )
      (while (string-match "\\(\\[\\|\\]\\)" val)
        (setq val (replace-match "" nil nil val))
        )
      (cond
       ( (string-match "^n[0-9]*$" val)
         (setq val (list "#"))
         )
       ( (string-match "(" val)
         (setq val (list val))
         )
       ( (string-match "[,]" val)
         (setq val (split-string val "[ ,]" 't))
         )
       ( 't
         (setq val (list val))
         )
       )
      (mapc (lambda(x)
              (if (not (assoc (concat opt "=") (symbol-value list-var)))
                  (progn
                    (add-to-list list-var (list (concat opt "=") 
                                                (list (list x text)))))
                (set list-var
                     (mapcar
                      (lambda (elt)
                        (let (
                              (ret elt)
                              tlst
                              )
                          (when (string= (concat opt "=") (nth 0 elt))
                            (setq ret (list (nth 0 elt) 
                                            (append (nth 1 elt)
                                                    
                                                    (list (list x text)))))
                            )
                          (symbol-value 'ret)
                          )
                        )
                      (symbol-value list-var)
                      )
                     )
                                        ;(when (string-match "DISPLAY" opt)
                                        ;      (esn-message "%s: %s\n\n%s" opt x (symbol-value list-var)))
                )
                                        ;           (when paren
                                        ;             (if (not (assoc opt (symbol-value list-var)))
                                        ;                 (add-to-list list-var (list opt text))
                                        ;               )
              )
            val
            )
      )
    )
                                        ;    )
  )
(defun esn-get-options ()
  "* Get options from a help.ctl file."
  (let (
                                        ;        (debug-on-error 't)
        (case-fold-search nil)          ;
        op1
        val
        txt
        tmp
        i
        (opt-lst '())
        has-paren
        p1
        var
        )
    (save-excursion
      ;;
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*=[ \t]*" nil t)
        (replace-match "=")
        )
      (goto-char (point-min))
      (when (re-search-forward "^ LINK\\>" nil t)
        (replace-match "LINK, TO, AND, BY\n      Usage: LINK")
        )
      (goto-char (point-min))
      (while (re-search-forward "\\<\\(FROM\\|TO\\)\\>[ \t]+n" nil t)
        (replace-match "\\1=n")
        )
      ;; Take out change marks and hyphenation.
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "-[ \t]*\n[ \t]*" nil t)
        (replace-match ""))
      ;; Make each option stand on a separate line.
      (goto-char (point-min))
      (while (re-search-forward "^ \\(-?[A-Z]\\{3,\\}\\|FO\\|(format)\\)\\( .*\\)$" nil t)
        (replace-match (format " \\1\n %s\\2" (make-string (length (match-string 1)) ? )))
        )
      ;; Fix $PRIOR specific problems
      (goto-char (point-min))
      (when (re-search-forward "^   ESTIMATION" nil t)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]+\\(ESTIMATION\\|PROBLEM=\\|ITYP\\|NTHETA\\|IFND\\)" nil t)
          (replace-match "\n \\1"))
        (goto-char (point-min))
        (while (re-search-forward "\\<\\([A-Z0-9]*\\) or \\([A-Z0-9]*\\)\\>" nil t)
          (replace-match "\\1,\\2")
          )
        (goto-char (point-min))
        (when (re-search-forward "^ subroutine[ \t]*\n" nil t)
          (replace-match " TNPRI,NWPRI"))
        (goto-char (point-min))
        (when (re-search-forward "^ conditional" nil t)
          (beginning-of-line)
          (delete-region (point) (progn (re-search-forward "ESTIMATION" nil t) (beginning-of-line) (point)))
          )
        (goto-char (point-min))
        (while (re-search-forward "^ \\([A-Z].*?\\),[ \t]+" nil t)
          (replace-match " \\1,")
          (beginning-of-line))
                                        ;        (message "%s" (buffer-substring (point-min) (point-max)))
        )
      ;; Fix $DAT specific problesm
      (goto-char (point-min))
      ;; Fix $SCA specific problems
      (goto-char (point-min))
      (when (re-search-forward "^ UNIT" nil t)
        (message "Found UNIT!")
        (beginning-of-line)
        (delete-region (point-min) (point))
        (when (re-search-forward " When DV" nil t)
          (beginning-of-line)
          (delete-region (point) (point-max))
          )
        (goto-char (point-min))
        (while (re-search-forward  "\\<\\(ORD0\\|ABS0\\)\\>" nil t)
          (replace-match "\\1\n      "))
        (when (search-forward "A maximum of 900 data records" nil t)
          (replace-match "")
          (setq p1 (point))
          (delete-region p1 (point-max))
          )
        (message "%s" (buffer-substring (point-min) (point-max)))
        )
      ;; Fix $SUB specific problem
      (goto-char (point-min))
      (when (re-search-forward "SUBROUTINES=kind" nil t)
        (replace-match "SUBROUTINES=[DP,LIBRARY]")
        )
      ;; Fix $INCLUDE specific problem
      (goto-char (point-min))
      (while (re-search-forward "\\<NM-?TRAN\\>" nil t)
        (replace-match ""))
      ;; Fix $TAB specific problem
      (goto-char (point-min))
      (when (search-forward "The NONMEM output file will contain information" nil t)
        (beginning-of-line)
        (delete-region (point)
                       (progn
                         (when (search-forward "(See Displayed PRED-Defined Items)." nil t)
                           (end-of-line)
                           )
                         (point)
                         ))
        )
      (goto-char (point-min))
      (when (search-forward "They must be specified for each table to which they apply" nil t)
        (end-of-line)
        (delete-region (point-min) (point))
        )
      ;; Fix $EST specific problems
      (goto-char (point-min))
      (when (re-search-forward "^ METHOD=kind.*$" nil t)
        (replace-match "")
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "If a MSFO is output, then the" nil t)
            (replace-match "")
            (setq p1 (point))
            (when (search-forward "(See model_specification_file)." nil t)
              (replace-match "")
              (delete-region p1 (point))
              )
            )
          )
        (setq p1 (point))
        (when (re-search-forward "^ [A-Z]" nil t)
          (save-restriction
            (narrow-to-region p1 (point))
            (goto-char (point-min))
            (while (re-search-forward "\\<\\([A-Z0-9]*\\) or \\([A-Z0-9]*\\)\\>" nil t)
              (replace-match "[\\1,\\2]")
              )
            (goto-char (point-min))
            (while (re-search-forward "^\\(      \\)\\([A-Z]\\{3,\\}\\)\\( .*\\)" nil t)
              (replace-match (format "\\1\\2\n\\1%s\\3" (make-string (length (match-string 2)) ? )))
              (skip-chars-backward " \t|")
              (if (re-search-backward "\n\\=" nil t)
                  (when (looking-at "\n[ \t|]*")
                    (replace-match "")))
              )
            (goto-char (point-min))
            (while (re-search-forward "^      \\(\\[?[A-Z,0-9]+\\]?\\)[ \t]*$" nil t)
              (replace-match " METHOD=\\1"))
            )
          ;; Drop Descriptions of Estimation types FOCE, FOCEI, etc.
          (goto-char (point-min))
          (while (re-search-forward "^ METH=COND [A-Z]+.*" nil t)
            (replace-match ""))
          ;; Add NOOBT and such to test
          (goto-char (point-min))
          (while (re-search-forward "\\<NO\\(THETA\\|OMEGA\\|SIGMA\\)BOUNDTEST\\>" nil t)
            (setq tmp (substring (match-string 1) 0 1))
            (insert (format ",NO%sBT,NO%sBOUNDTEST" tmp tmp ))
            )
          )
        )
      )
    ;; Now get Options!
    (goto-char (point-min))
    (while (re-search-forward "^ \\(-?[A-Z0-9][A-Z][A-Z0-9]*.*\\|(format)\\|filename\\|list[0-9]+\\|format[0-9]+\\)" nil t)
      (setq op1 (match-string 1))
      (replace-match "")
      (forward-line 1)
      (beginning-of-line)
      ;; Now get text associated with option.
      (when (looking-at "^[ \t]*")
        (setq tmp (match-string 0))
        (setq txt "")
        (while (cond
                ( (= (point) (point-max))
                  nil
                  )
                ( (looking-at (format "%s\\(.*\\)" tmp))
                  (setq txt (concat txt " " (match-string 1)))
                  (replace-match "")
                  't
                  )
                ( (looking-at "^[ \t]$")
                  (setq txt (concat txt "\n"))
                  (replace-match "")
                  't
                  )
                ( 't
                  nil
                  ))
          (beginning-of-line)
          (when (looking-at ".*\n")
            (replace-match ""))
          (skip-chars-forward "\n"))
        (with-temp-buffer
          (insert txt)
          ;; Look for "May also be coded" sections of text and add them to the possibilities.
          (goto-char (point-min))
          (while (re-search-forward "\\(?:[Ii]t[ \t]+\\)?\\(?:[Mm]ay\\|[Cc]an\\)[ \t\n]+\\(?:also[ \t\n]\\)?+be[ \t\n]+coded\\([^.;]*?\\)[;.]" nil t)
            (unless (save-match-data
                      (save-excursion 
                        (backward-char (length (match-string 0)))
                        (re-search-backward "[*][ \t]*\\=" nil t)
                        )
                      )
              (setq tmp (match-string 1))
              (replace-match "")
              (when (looking-at "g[.]\\([^.;]*?\\)[;.]")
                (setq tmp (concat tmp (match-string 1)))
                (replace-match "")
                (while (string-match (regexp-opt '(
                                                   "and"
                                                   "shorter"
                                                   "substrings"
                                                   "strings"
                                                   "e"
                                                   ) 'words) tmp)
                  (setq tmp (replace-match "" nil nil tmp))
                  )
                (when (string-match "e[ \t]*$" tmp)
                  (setq tmp (replace-match "" nil nil tmp)))
                )
              (goto-char (point-min))
              (while (string-match " or " tmp)
                (setq tmp (replace-match "," 't 't tmp)))
              (unless (string-match "treated" tmp)
                (setq op1 (concat (mapconcat (lambda(x) x) (split-string tmp "[ ,]" 't) ",") "," op1))
                                        ;(message "%s" tmp)
                )
              (skip-chars-backward " ;.\t\n")
              )
            )
          (goto-char (point-min))
          (while (re-search-forward ";" nil t)
            (replace-match "`"))
          (goto-char (point-max))
          (while (re-search-forward "`" nil t)
            (replace-match ";"))
          (setq txt (buffer-substring (point-min) (point-max)))
          )
        (cond
         ( (string-match "[=]" op1)
           (setq tmp (split-string op1 "[=]" 't))
           (when (= 2 (length tmp) )
             (setq op1 (nth 0 tmp))
             (when (string-match "\\[" op1)
               (setq op1 (replace-match "" nil nil op1))
               (setq has-paren 't)
               )
             (setq var (nth 1 tmp))
             (cond
              ( (string-match "[,|]" op1)
                ;; Multiple Options with =
                (mapc (lambda(x)
                        (esn-add-to-opt-list 'opt-lst x txt var has-paren)
                        )
                      (split-string op1 "[, |]" 't)
                      )
                )
              ( 't
                (esn-add-to-opt-list 'opt-lst op1 txt var has-paren)
                )
              )
             )
           )
         ( (string-match "[,|]" op1)
           ;; Multiple options.
           (mapc (lambda(x)
                   (esn-add-to-opt-list 'opt-lst x txt)
                   )
                 (split-string op1 "[, |]" 't)
                 )
           )
         ( 't
           ;; Single Option
           (esn-add-to-opt-list 'opt-lst op1 txt)
           )
         )
        )
      )
    (symbol-value 'opt-lst)
    )
  )
(defun esn-get-option ()
  "* Get a single option from a help.ctl file."
  (let (
        opt
        val
        cnt
        p1
        ret
        (case-fold-search nil)
        )

    ;; Options are specified as follows:
    ;; COMRES=n1  ('common reserve')
    ;; DERIV2=NO
    ;; DERIV2=NOCOMMON
    ;;  filename
    ;;  (format)
    ;;  OPTIONS:
    ;; IGNORE=(list)
    ;; NOREWIND|REWIND
    ;;

    ;; METHOD=kind
    ;;      Values for kind are:

    ;;      0 or ZERO
    ;;           Always set etas to 0 during the computation of the objective
    ;;           function.   Also called the "first order (FO) method."  This
    ;;           is the default.
    
    ;;      1 or CONDITIONAL
    ;;           Use conditional estimates for the etas during  the  computa-
    ;;           tion  of  the  objective  function.   METHOD=1  (without the
    ;;           LAPLACIAN option) is also called  the  "first  order  condi-
    ;;           tional estimation (FOCE) method."  The conditional estimates
    ;;           of the etas are referred to as Conditional  Parametric  Etas
    ;;           (CPE).

    ;;  THETABOUNDTEST, OMEGABOUNDTEST, SIGMABOUNDTEST
    ;;  PRINT=[E][R][S]
    ;;  CTYPE=[0|1|2|3]
    ;;  NOTITLE=[0,1]
    ;;  NPOPETAS[=n]
    ;;  DISPLAY[=ALL|CNT]
    ;;  FROM n1
    ;;  TO n2
    ;;  init [FIXED]
    ;;  ([low,] init [,up] [FIXED])
    ;;  ([low,] init [,up]) [FIXED]
    ;;  (low,,up)

    ;; WARNINGMAXIMUM
    ;;      Refers to general (non-data) warning messages.  May also be coded
    ;;      WARNMAXIMUM and  shorter  substrings,  e.g.,  WMAX,   WARN.   May
    ;;      appear more than once, with cumulative effect.

    ;;      WARNINGMAXIMUM=NONE


    ;; The  positions of the values correspond to the positions of data items
    ;; in the $INPUT record in a 1-to-1 manner.  Each value is one of:
    ;;   DOSE (Use the value from the dose record.)
    ;; Initial "FIXES"
    (when nil
      (when (re-search-forward "^ \\([A-Za-z][A-Z0-9|_]+\\)\\(\\[?[= ][^ \t\n]+\\)?" nil t)
        (goto-char (point-min))
        (setq opt (match-string 1))
        (setq val (match-string 2))
        (replace-match "")
        (setq p1 (point))
        (setq cnt (buffer-substring
                   (point)
                   (if (not (re-search-forward "^ \\([A-Z][A-Z0-9|_]+\\)\\(\\[[ =][^ \t\n]+\\)?" nil t))
                       (point-max)
                     (beginning-of-line)
                     (skip-chars-backward "\t\n \f")
                     (point))
                   )
              )
        (delete-region p1 (point))
        (setq ret (list opt val cnt))
        )

      )
    )
  )

(defun esn-gen-data-vars ()
  "* Get reserved DATA variables used in $INPUT record"
  (interactive)
  (let (
        (dat-files (file-expand-wildcards (concat esn-nm-cookie-dir "/*.dat")))
        (dat-lst '())
        abbrev
        )
    (mapc
     (lambda(x)
       (with-temp-buffer
         (setq abbrev nil)
         (message "Looking at %s" x)
         (insert-file-contents x)
         (goto-char (point-min))
         (when (re-search-forward "USAGE:" nil t)
           (save-restriction
             (narrow-to-region (point)
                               (progn
                                 (if (re-search-forward "DISCUSSION:" nil t)
                                     (beginning-of-line)
                                   (message "Not figured out %s" x)
                                   (point-max))
                                 (point)
                                 )
                               )
             (goto-char (point-min))
             (when (re-search-forward "[$]INPUT" nil 't)
               (replace-match "")
               (while (re-search-forward "[.]\\{3\\}" nil t)
                 (replace-match ""))
               (goto-char (point-min))
               (while (re-search-forward "\\[.*?\\]" nil t)
                 (replace-match " DAT1 DAT2 DAT3 L1"))
               (goto-char (point-min))
               (while (re-search-forward "\\([A-Z0-9_]+\\)" nil 't)
                 (add-to-list 'dat-lst (match-string 1))
                 (replace-match ""))
               )
             )
           )
         )
       )
     dat-files
     )
    ;; Missing    RAW_ MRG_ RPT_ and L2
                                        ;    (message "%s" dat-lst)
    (symbol-value 'dat-lst)
    )
  )
(defun esn-gen-cookies-1 ()
  "* Generates Cookies"
  ;; Need to figure out what to do about $MODEL compartment options
  ;; AND $PRIOR Subroutine arguments
  (let (
        (case-fold-search nil)
        (ctl (file-expand-wildcards (concat esn-nm-cookie-dir "/*.ctl")))
        (records '())
        (records-help '())
        (options '())
        tmp
        tmp2
        ret
        ret2
        p1
        p2
        added
        (tmp-lst '())
        (tmp-lst2 '())
        (tmp-lst3 '())
        (tmp-lst4 '())
        (records-lst '())
        (dat-lst (esn-gen-data-vars))
        (lhs-lst '())
        (rhs-lst '())
        (no-lst '())
        (records-meaning '())
        (records-discussion '())
        abbrev
        (records-abbrev '())
        (records-aliases '())
        complete
        meaning
        discussion
        i
        )
    (mapc
     (lambda(x)
       (with-temp-buffer
         (setq abbrev nil)
         (message "Looking at %s" x)
         (insert-file-contents x)
         (goto-char (point-min))
         (setq meaning nil)
         (setq discussion nil)
         (when (re-search-forward "MEANING:[ \t]*\\(\\(?:\n\\|.\\)*?\\)\n[ \t]+[A-Z]+:" nil t)
           (setq meaning (match-string 1))
           (while (string-match "-?[ \t]*\n[ \t]*" meaning)
             (setq meaning (replace-match "" nil nil meaning)))
           (while (string-match "  +" meaning)
             (setq meaning (replace-match " " nil nil meaning)))
           )
         (when (re-search-forward "DISCUSSION:[ \t]*\\(\\(?:\n\\|.\\)*?\\)\n[ \t]+[A-Za-z -]+:" nil t)
           (setq discussion (match-string 1))
           (with-temp-buffer
             (insert discussion)
             (goto-char (point-min))
             (skip-chars-forward " \t\n")
             (delete-region (point-min) (point))
             (goto-char (point-max))
             (skip-chars-backward " \t\n")
             (delete-region (point) (point-max))
             (goto-char (point-min))
             (while (re-search-forward "^[ \t]+" nil t)
               (replace-match ""))
             (setq discussion (buffer-substring (point-min) (point-max)))
             )
           )
         (goto-char (point-min))
         (when (re-search-forward "USAGE:" nil t)
           (when (re-search-forward (eval-when-compile (esn-reg-records nil 't)) nil t)
             (setq abbrev (looking-at "[Aa]bbreviated code"))
             )
           )
         (goto-char (point-min))
         (if (not (re-search-forward "^[ \t]*OPTIONS:[ \t]*$" nil t))
             (progn
               (setq options nil)
               (setq p1 (point-max))
               )
           (save-restriction
             (save-excursion
               (setq tmp nil)
               (narrow-to-region (progn
                                   (re-search-forward "^[ \t]*OPTIONS:[ \t]*$" nil t)
                                   (when (re-search-forward "^   DOSE (" nil 't)
                                     (beginning-of-line)
                                     (setq tmp 't)
                                     )
                                   (point))
                                 (progn
                                   (goto-char (point-max))
                                   (while (re-search-backward "^[ \t]\\(REFERENCES?\\|EXAMPLES?\\|RECORD ORDER\\):" nil t))
                                   (when tmp
                                     (when (re-search-backward "^\\(   -\\)" nil 't)
                                       (replace-match "\\1NEG"))
                                     (end-of-line)
                                     )
                                   (point)
                                   ))
               (when tmp
                 (goto-char (point-min))
                 (while (re-search-forward "^[ \t]*" nil t)
                   (replace-match " ")
                   )
                 (goto-char (point-min))
                 (while (re-search-forward "[()]" nil t)
                   (replace-match ""))
                 )
               (goto-char (point-min))
               (setq options (esn-get-options))
               )
             (goto-char (point-min))
             (when (re-search-forward "^[ \t]*OPTIONS:[ \t]*" nil t)
               (replace-match "")
               (delete-region (point) (progn (re-search-forward "^ \\(REFERENCE\\|RECORD ORDER\\)" nil t) (beginning-of-line) (point))))
             )
           (setq p1 (point))
           )
         (goto-char (point-min))
         (when (re-search-forward "USAGE:" nil t)
           (when (re-search-forward "\\<[$]\\([A-Z][A-Za-z0-9_]*\\)\\>" nil t)
             (setq tmp (concat "$" (match-string 1)))
             (when (string-match "\\(THETA\\|OMEGA\\|SIGMA\\)" tmp)
               (esn-add-to-opt-list 'options "FIXED" "Fix an estimate value")
               (if (not (string-match "\\(OMEGA\\|SIGMA\\)" tmp))
                   (progn
                     (esn-add-to-opt-list 'options "INF" "+Infinity")
                     (esn-add-to-opt-list 'options "-INF" "-Infinity")
                     )
                 (esn-add-to-opt-list 'options "BLOCK" "Block estimates")
                 (esn-add-to-opt-list 'options "SAME" "Same as previous Block")
                 (esn-add-to-opt-list 'options "DIAGONAL" "Diagonal")
                 )
               )
             (when (string-match "INP" tmp)
               ;; Add Input options not initially recognized.
               ;; =DROP and =SKIP
               (esn-add-to-opt-list 'options "DROP" "Drop from dataset")
               (esn-add-to-opt-list 'options "SKIP" "Drop from dataset")
               )
             (when (string-match "SUB" tmp)
               ;; Add subroutine options that were not initial picked up.
               ;; Add ADVAN & SS options
               (setq i 1)
               (while (<= i (if (string= esn-nm-cookie-ver "7") 13 12))
                 (setq tmp2 (esn-mode-short-advan i))
                 (esn-add-to-opt-list 'options (format "ADVAN%s" i) tmp2)
                 (esn-add-to-opt-list 'options "ADVAN" tmp2 (format "ADVAN%s" i))
                 (setq tmp2 (format "SS%s" i))
                 (esn-add-to-opt-list 'options "SS" tmp2 tmp2)
                 (esn-add-to-opt-list 'options tmp2 tmp2)
                 (setq i (+ 1 i))
                 )
               ;; Add Trans
               (setq i 1)
               (while (<= i 6)
                 (setq tmp2 (format "TRANS%s" i))
                 (esn-add-to-opt-list 'options tmp2 tmp2)
                 (esn-add-to-opt-list 'options "TRANS" tmp2 tmp2)
                 (setq i (+ 1 i))
                 )
               ;; Add User Supplied Routines
               (mapc (lambda(x)
                       (esn-add-to-opt-list 'options x (format "%s user supplied routine" x) "(usersub)")
                       )
                     '("CRIT" "MIX" "PRED" "CRIT" "MIX" "PRIOR" "CONTR" "CCONTR"
                       "USMETA" "SPTWO" "INLETA" "PRED" "PK" "ERROR" "MODEL" "DES"
                       "AES" "TOL" "INFN" "OTHER")
                     )
               )
             (when abbrev
               (add-to-list 'records-abbrev tmp)
               ;; For Abbreviated code:
               ;; Get Left Handed Quantities
               (setq tmp-lst '())
               (cond
                ( (string= tmp "$AES")
                  (setq tmp-lst (list "E" "E(#)"))
                  )
                ( 't
                  ;; Rest
                  (save-excursion
                    (save-restriction
                      (goto-char (point-min))
                      (when (re-search-forward "^[ \t]*Left-hand.*?:[ \t]*$" nil t)
                        (setq p1 (point))
                        (when (re-search-forward "^[ \t]*\\(Right-hand.*?:\\|PSEUDO ASSIGNMENT STATEMENTS\\)[ \t]*$" nil t)
                          (beginning-of-line)
                          (setq p2 (point))
                          (let (
                                (case-fold-search nil)
                                )
                            (setq tmp2 (buffer-substring p1 p2))
                            (with-temp-buffer
                              (insert tmp2)
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z0-9_]+-defined\\|PK\\|FORTRAN\\|AES\\|ADVAN[0-9]+\\|[$][A-Z]*\\)\\>" nil 't )
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<THETA " nil t)
                                (replace-match ""))
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z_0-9]+\\)(\\(?:[0-9]+\\|[ni]\\))" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 1) "(#)"))
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z_]+\\)[0-9ni]\\>" nil 't)
                                (setq tmp2 (match-string 1))
                                (unless (string-match "^[VQK]" tmp2)
                                  (add-to-list 'tmp-lst (concat tmp2 "#!"))
                                  )
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<[A-Z_]+\\>" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 0) "!"))
                                (replace-match ""))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
               ;; Now put in LHS list.
               (add-to-list 'lhs-lst (list tmp tmp-lst))
               (setq tmp-lst '())
               (cond 
                ( 't
                  ;; Rest
                  (save-excursion
                    (save-restriction
                      (goto-char (point-min))
                      (when (re-search-forward "^[ \t]*Right-hand.*?:[ \t]*$" nil t)
                        (setq p1 (point))
                        (when (re-search-forward "^[ \t]*\\(Forbidden.*?:\\|PSEUDO ASSIGNMENT STATEMENTS\\)[ \t]*$" nil t)
                          (let (
                                (case-fold-search nil)
                                )
                            (beginning-of-line)
                            (setq p2 (point))
                            (setq tmp2 (buffer-substring p1 p2))
                            (with-temp-buffer
                              (insert tmp2)
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z0-9_]+-defined\\|PK\\|FORTRAN\\|AES\\|ADVAN[0-9]+\\|[$][A-Z]*\\)\\>" nil 't )
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<THETA " nil t)
                                (replace-match ""))
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z_0-9]+\\)(\\(?:[0-9]+\\|[ni]\\))" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 1) "(#)"))
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z]\\)[0-9ni]\\>" nil 't)
                                (add-to-list 'tmp-lst (concat (match-string 1) "#!"))
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<[A-Z_]+\\>" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 0) "!"))
                                (replace-match ""))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
               (add-to-list 'rhs-lst (list tmp tmp-lst))
               (setq tmp-lst '())
               ;; Now forbidden list
               (cond
                ( 't
                  ;; Rest
                  (save-excursion
                    (save-restriction
                      (let (
                            (case-fold-search nil)
                            )
                        (goto-char (point-min))
                        (when (re-search-forward "^[ \t]*Forbidden.*:[ \t]*$" nil t)
                          (setq p1 (point))
                          (when (re-search-forward "^[ \t]*\\(.*?:\\|PSEUDO ASSIGNMENT STATEMENTS\\|RECORD ORDER:\\|REFERENCES:\\)[ \t]*$" nil t)
                            (beginning-of-line)
                            (setq p2 (point))
                            (setq tmp2 (buffer-substring p1 p2))
                            (with-temp-buffer
                              (insert tmp2)
                              (goto-char (point-min))
                              (while (re-search-forward "\\<\\([A-Z_]+\\)(\\(?:[0-9]+\\|n\\))" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 1) "(#)"))
                                (replace-match "")
                                )
                              (goto-char (point-min))
                              (while (re-search-forward "\\<[A-Z_]+\\>" nil t)
                                (add-to-list 'tmp-lst (concat (match-string 0) "!"))
                                (replace-match ""))
                              )
                            )
                          )

                        )
                      )
                    )
                  )
                )
               (add-to-list 'no-lst (list tmp tmp-lst))
               ;; Get Right Handed Quantities (x)
               ;; Get Forbidden Variable Names (x)
               ;; Pseudo-Assignment variables
               ;; May not include
               ;; Get Record Order:
               ;;
               )
             (add-to-list 'records (list tmp options))
             (when meaning
               (add-to-list 'records-meaning (list tmp meaning))
               )
             (when discussion
               (add-to-list 'records-discussion (list tmp discussion))
               )
             (add-to-list 'records-help (list tmp (list x (buffer-substring (point-min) (point-max)))))
             (when (string= "$INCLUDE" tmp)
               (add-to-list 'records (list "INCLUDE" options))
               (add-to-list 'records-help (list "INCLUDE" (list x (buffer-substring (point-min) (point-max)))))
               )
             (setq tmp2 tmp)
             (setq tmp-lst '())
             (when (re-search-forward "DISCUSSION:" nil t)
               (setq p2 (point))
               (while (re-search-forward "[Mm]ay[ \t\n]+also[ \t\n]+be[ \t\n]+coded\\(\\(?:.\\|n\\)*?\\)[;.]" p1 t)
                 (setq tmp (match-string 1))
                 (replace-match "")
                 (goto-char p2)
                 (while (string-match " or " tmp)
                   (setq tmp (replace-match "," 't 't tmp)))
                 (mapc (lambda(y)
                         (unless (string= y "$INFILE")
                           (add-to-list 'records (list y options))
                           )
                         (add-to-list 'records-help (list y (list x (buffer-substring (point-min) (point-max)))))
                         (when meaning
                           (add-to-list 'records-meaning (list y meaning))
                           )
                         (when discussion
                           (add-to-list 'records-discussion (list y discussion))
                           )
                         (add-to-list 'tmp-lst y)
                         (when abbrev
                           (add-to-list 'records-abbrev y)
                           )
                         )
                       (split-string tmp "[ ,]" 't)
                       )
                 )
               )
             (add-to-list 'records-aliases (list tmp2 tmp-lst))
             )
           )
         )
       )
     ctl
     )
    (with-temp-file (concat esn-path "etc/cookies/esn-nm-cookies-help-records-" esn-nm-cookie-ver ".el")
      (insert ";; -*-no-byte-compile: t; -*-\n")
      (insert (format "(defvar esn-nm-help-records-discussion-%s '(%s)\n\"* Records discussion\")\n"
                      esn-nm-cookie-ver
                      (mapconcat 
                       (lambda(x)
                         (concat "(\"" (nth 0 x) "\" \"" (esn-gen-cookies-quote (nth 1 x)) "\")")
                         )
                       records-discussion
                       " "
                       )
                      ))
      (insert (format "(defvar esn-nm-help-records-%s '(%s)\n\"* Records help\")\n(provide 'esn-nm-cookies-help-records-%s)"
                      esn-nm-cookie-ver
                      (mapconcat 
                       (lambda(x)
                         (concat "(\"" (nth 0 x) "\" \"" (nth 1 x) "\")")
                         )
                       records-meaning
                       " "
                       )
                      esn-nm-cookie-ver
                      ))

      )
    (setq complete (format "(defvar esn-records-complete-%s '%s\n\"* Completion records\")" esn-nm-cookie-ver (concat "(" (mapconcat (lambda(x) (concat "\"" (nth 0 x) "\"")) records " ") ")")))
    (mapc (lambda(x)
            (add-to-list 'all-recs (downcase (esn-rec3 (nth 0 x)))))
          records)
    (setq tmp-lst '())
    (setq complete (format "%s\n(defvar esn-record-complete-options-%s '(%s)\n\"* Option completions\")\n"
                           complete
                           esn-nm-cookie-ver
                           (mapconcat 
                            (lambda(x)
                              (let (
                                    (frec (esn-rec3 (nth 0 x)))
                                    (fret "")
                                    added-info
                                    )
                                (unless (and tmp-lst (string-match (regexp-opt tmp-lst 't) frec))
                                  (when (nth 1 x)
                                    (setq fret 
                                          (format 
                                           "(\"%s\" (%s))"
                                           frec
                                           (mapconcat
                                            (lambda(y)
                                              (let ((case-fold-search nil))
                                                (if (string-match "[a-z]" (nth 0 y))
                                                    ""
                                                  (setq added-info 't)
                                                  (concat "\"" (nth 0 y) "\"")
                                                  )
                                                )
                                              )
                                            (nth 1 x)
                                            " "
                                            )
                                           ))
                                    (add-to-list 'tmp-lst frec)
                                    (unless added-info
                                      (setq fret "")
                                      )
                                    )
                                  )
                                (symbol-value 'fret)
                                )
                              )
                            records
                            " ")
                           ))
    (setq tmp-lst '())
    (mapc (lambda(x)
            (add-to-list 'tmp-lst (esn-rec3 x)))
          records-abbrev)
    (mapc (lambda(x)
            (add-to-list 'all-abbrev (esn-rec3 x)))
          records-abbrev)
    (setq complete 
          (format "%s\n(defvar esn-abbrev-records-%s '(%s)\n\"* Abbreviated records 3-character abbreviation\")"
                  complete
                  esn-nm-cookie-ver
                  (mapconcat (lambda(x) (concat "\"" x "\"")) tmp-lst " ")
                  )
          )
    (setq tmp-lst '())
    (setq tmp-lst2 '())
    (setq complete
          (format "%s\n(defvar esn-record-complete-option-values-%s '(%s)\n\"*Values completions\")"
                  complete
                  esn-nm-cookie-ver
                  (mapconcat 
                   (lambda(x)
                     (let (
                           (frec (esn-rec3 (nth 0 x)))
                           (fret "")
                           added-info
                           )
                       (unless (and tmp-lst (string-match (regexp-opt tmp-lst 't) frec))
                         (when (nth 1 x)
                           (setq fret 
                                 (format 
                                  "(\"%s\" (%s))"
                                  frec
                                  (mapconcat
                                   (lambda(y)
                                     (let ((case-fold-search nil) (fret2 "") (added-info-2 nil))
                                       (if (string-match "[a-z]" (nth 0 y))
                                           (setq fret2 "")
                                         (when (nth 1 y)
                                           (unless (typep (nth 1 y) 'string)
                                             (setq fret2 (format "(\"%s\" (%s))" 
                                                                 (esn-rec3 (nth 0 y))
                                                                 (mapconcat
                                                                  (lambda(z)
                                                                    (if  (string-match "[a-z#]" (nth 0 z))
                                                                        ""
                                                                      (setq added-info-2 't)
                                                                      (setq added-info 't)
                                                                      (format "\"%s\"" (nth 0 z))))
                                                                  (nth 1 y)
                                                                  " ")))
                                             (unless added-info-2
                                               (setq fret2 "")))))
                                       (symbol-value 'fret2)))
                                   (nth 1 x)
                                   " ")))
                           (add-to-list 'tmp-lst frec)
                           (unless added-info
                             (setq fret ""))))
                       (symbol-value 'fret)))
                   records
                   " ")))
    (with-temp-buffer
      (insert complete)
      (goto-char (point-min))
      (while (re-search-forward "\\([()]\\) +\\([()]\\)" nil t)
        (if (and (string= (match-string 1) ")") (string= (match-string 2) "("))
            (replace-match "\\1 \\2")
          (replace-match "\\1\\2")))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t)
        (replace-match " "))
      (setq complete (buffer-substring (point-min) (point-max)))
      )
    (with-temp-file (concat esn-path "etc/cookies/esn-nm-cookies-complete-" esn-nm-cookie-ver ".el")
      (insert ";; -*-no-byte-compile: t; -*-\n")
      (insert complete)
      (insert (concat "\n(provide 'esn-nm-cookies-complete-" esn-nm-cookie-ver ")"))
      )
    (mapc
     (lambda(x)
       (setq ret (concat ret "(defvar esn-records-help-" 
                         (esn-rec3 (nth 0 x)) "-"
                         esn-nm-cookie-ver " '(\n"))
       (setq added nil)
       (when (nth 1 x)
         (mapc (lambda(y)
                 (unless (string= "filename" (nth 0 y))
                   (setq added 't)
                   (setq ret (concat ret "\t\t(\"" (nth 0 y) "\""))
                   (if (typep (nth 1 y) 'string)
                       (setq ret (concat ret " \"" (esn-gen-cookies-quote (nth 1 y)) "\""))
                     (let (
                           (tmp " (\n")
                           (last-value nil)
                           (all-equal 't)
                           )
                       (setq last-value nil)
                       (setq all-equal 't)
                       (mapc (lambda(z)
                               (when all-equal
                                 (if (not last-value)
                                     (setq last-value (esn-gen-cookies-quote (nth 1 z)))
                                   (unless (string=  (esn-gen-cookies-quote (nth 1 z)) last-value)
                                     (setq all-equal nil)
                                     )
                                   )
                                 )
                               (setq tmp (concat tmp "\t\t\t(\"" (nth 0 z) "\" \"" (esn-gen-cookies-quote (nth 1 z)) "\")\n"))
                               )
                             (nth 1 y)
                             )
                       (if all-equal
                           (setq ret (concat ret " \"" last-value "\""))
                         (setq ret (concat ret tmp ")")))))
                   (setq ret (concat ret ")\n"))))
               (nth 1 x))
                                        ;         (setq ret (concat ret "\n\t)\n"))
         )
       (when added
         (setq ret (concat ret ")\n"))
         (setq ret (concat ret "\n\"* " (nth 0 x) " help for NONMEM " esn-nm-cookie-ver "\")\n"))
         (setq ret (concat ret "\n(provide 'esn-records-help-" 
                           (esn-rec3 (nth 0 x)) "-"
                           esn-nm-cookie-ver ")\n"))
         (setq ret (concat ret "\n(provide 'esn-nm-cookies-help-" 
                           (esn-rec3 (nth 0 x)) "-"
                           esn-nm-cookie-ver ")\n"))
         (with-temp-file (concat esn-path "etc/cookies/esn-nm-cookies-help-" (esn-rec3 (nth 0 x)) "-" esn-nm-cookie-ver ".el")
           (insert ";; -*-no-byte-compile: t; -*-\n")
           (insert ret)
           )
         )
       (setq ret "")
       )
     records
     )
    (setq ret (concat ret ")\n \"* NONMEM " esn-nm-cookie-ver " records help variable\"\n)"))
                                        ;    (setq ret (concat ret "(provide 'esn-nm-cookies-help-" esn-nm-cookie-ver ")\n"))
    (setq ret "")
    ;; Get Records Reg.
    (setq ret (concat ret "\n(defvar esn-exact-records-reg-" esn-nm-cookie-ver
                      " \""
                      (esn-gen-cookies-quote (regexp-opt (append '("$include")
                                                                 (mapcar (lambda(x) (nth 0 x)) records)) 'words))
                      "\"\n\"Exact regular expression of ALL known records for NONMEM " esn-nm-cookie-ver "\")\n"))
    (setq tmp-lst '())
    (mapc (lambda(x)
            (when (and (>= (length (nth 0 x)) 4)
                       
                       (not (assoc (concat (substring (nth 0 x) 0 4) "~") records)))
              (add-to-list 'tmp-lst (concat (substring (nth 0 x) 0 4) "~"))))
          records)
    (mapc (lambda(x)
            (add-to-list 'tmp-lst (nth 0 x)))
          records)
    (setq tmp (regexp-opt tmp-lst 'words))
    (setq records-lst tmp-lst)
    (while (string-match "~" tmp)
      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
    (setq tmp (esn-gen-cookies-quote tmp))
    (setq ret (concat ret "\n(defvar esn-records-reg-" esn-nm-cookie-ver " \""
                      tmp
                      "\"\n\"* Regular expression of ALL known records for NONMEM " esn-nm-cookie-ver "\")\n"))
    ;; Create = Expresssion
    (setq tmp (regexp-opt tmp-lst nil))
    (setq records-lst tmp-lst)
    (while (string-match "~" tmp)
      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
    (setq tmp (concat "\\(\\<" tmp "\\>\\|\\<[A-Z][A-Z0-9_]*\\(?:([ \t]*[0-9]+[ \t]*)\\)?\\>\\)"))
    (setq ret (concat ret "\n(defvar esn-records-word-reg-" esn-nm-cookie-ver 
                      " \"" 
                      (esn-gen-cookies-quote tmp) 
                      "\"\n\"* Regular expression of all known records or Variables\")"))

    ;; Regular expression for all known options
    (setq ret (concat ret "\n(defvar esn-record-options-reg-" esn-nm-cookie-ver " '(\n"))
    (setq tmp-lst2 '())
    (setq tmp-lst3 '())
    (mapc (lambda(y)
            (when (nth 1 y)
              (let (
                    (rec (nth 0 y))
                    )
                (when (string-match "^[$]" rec)
                  (setq rec (replace-match "" nil nil rec))
                  (when (> (length rec) 3)
                    (setq rec (substring rec 0 3))
                    )
                  (when (or (not tmp-lst2) (not (string-match (regexp-opt  tmp-lst2 't) rec)))
                    (add-to-list 'tmp-lst2 rec)
                    (setq ret (concat ret  "\n\t(\"" rec "\" \""))
                    (message "Record Options for Record %s" rec)
                    (setq tmp-lst '())
                    (mapc (lambda(x)
                            (let (
                                  (op (nth 0 x))
                                  (case-fold-search nil)
                                  op3
                                  )
                              (when (string-match "=" op)
                                (setq op (replace-match "" nil nil op)))
                              (when (and (>= (length op) 3)
                                         (not (assoc (substring op 0 3) (nth 1 y))))
                                (unless (string-match "[a-z]" op)
                                  (if (not (string= (substring op 0 1) "-"))
                                      (progn
                                        (add-to-list 'tmp-lst (concat (substring op 0 3) "~"))
                                        (add-to-list 'tmp-lst3 (concat (substring op 0 3) "~"))
                                        )
                                    (when (and (>= (length op) 4)
                                               (not (assoc (substring op 0 4) (nth 1 y)))
                                               )
                                      (add-to-list 'tmp-lst (concat (substring op 0 4) "~"))
                                      (add-to-list 'tmp-lst3 (concat (substring op 0 4) "~"))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          (nth 1 y)
                          )
                    (mapc (lambda(x)
                            (let (
                                  (op (nth 0 x))
                                  (case-fold-search nil)
                                  )
                              (when (string-match "=" op)
                                (setq op (replace-match "" nil nil op)))
                              (unless (string-match "[a-z]" op)
                                (add-to-list 'tmp-lst op)
                                (add-to-list 'tmp-lst3 op)
                                )
                              )
                            )
                          (nth 1 y)
                          )
                    (setq tmp (regexp-opt (append tmp-lst (list "z")) 'words))
                    (while (string-match "~" tmp)
                      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp))
                      )
                    (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
                      (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                      )
                    (when (string-match "z" tmp)
                      (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
                      )
                    (setq tmp (esn-gen-cookies-quote tmp))
                    (setq ret (concat ret tmp))
                    (setq ret (concat ret "\")"))
                    )
                  )
                )
              )
            )
          records
          )
    (setq ret (concat ret ")\n\"* Regular expressions for Options by Record OR next record for NONMEM " esn-nm-cookie-ver "\")\n"))
    (setq tmp-lst '())
    (mapc (lambda(x)
            (when (string= x "INC")
              (add-to-list 'tmp-lst (concat x))
              )
            (add-to-list 'tmp-lst (concat "$" x))
            )
          tmp-lst2
          )
    (setq ret (concat ret "\n(defvar esn-records-with-options-reg-" esn-nm-cookie-ver " \"\\\\<" (esn-gen-cookies-quote (regexp-opt tmp-lst 't)) "[A-Z0-9_]*\\\\>\"\n\"* Regular expression of Records with Options\")\n"))
    (setq tmp2 (regexp-opt tmp-lst3 'words))
    (while (string-match "~" tmp2)
      (setq tmp2 (replace-match "[A-Z0-9_]*" 't 't tmp2)))

    (setq ret (concat ret "\n(defvar esn-records-all-options-" esn-nm-cookie-ver " \""
                      (esn-gen-cookies-quote tmp2) "\"\n\"* Regular expression of ALL options\")\n"))
    (setq ret (concat ret "\n(defvar esn-records-options-val-reg-" esn-nm-cookie-ver " '(\n"))
    (setq tmp-lst2 '())
    (setq tmp2 (concat "\n(defvar esn-records-which-options-have-val-reg-" esn-nm-cookie-ver " '(\n"))
    (let ( (first 't))
      (mapc
       (lambda(y)
         (when (nth 1 y)
           (let (
                 (rec (nth 0 y))
                 print-rec
                 )
             (when (string-match "^[$]" rec)
               (setq rec (replace-match "" nil nil rec))
               (when (> (length rec) 3)
                 (setq rec (substring rec 0 3))
                 )
               (when (or (not tmp-lst2)
                         (not (string-match (regexp-opt tmp-lst2 't) rec)))
                 (setq tmp-lst3 '())
                 (setq print-rec nil)
                 (mapc
                  (lambda(x)
                    (let (
                          (op (nth 0 x))
                          (case-fold-search nil)
                          )
                      (when (string-match "=" op)
                        (add-to-list 'tmp-lst2 rec)
                        (setq op (replace-match "" nil nil op))
                        (when (> (length op) 3)
                          (setq op (substring op 0 3))
                          )
                        (when (or (not tmp-lst3)
                                  (not (string-match (regexp-opt tmp-lst3 't) op))
                                  )
                          (add-to-list 'tmp-lst3 op)
                          (unless print-rec
                            (setq ret (concat ret (if first "" ")\n") "(\"" rec "\"\n"))
                            (setq first nil)
                            (setq print-rec 't)
                            )
                          (setq tmp-lst4 '())
                          (mapc (lambda(z)
                                  (add-to-list 'tmp-lst4 (nth 0 z))
                                  )
                                (nth 1 x))
                          (unless (and
                                   (string= rec "SUB")
                                   (not (string= op "SUB"))
                                   )
                            (mapc (lambda(z)
                                    (when (and (>= (length (nth 0 z)) 3)
                                               (not (assoc (substring (nth 0 z) 0 3) records)))
                                      (unless (string-match "[a-z]" (nth 0 z))
                                        (add-to-list 'tmp-lst4 (concat (substring (nth 0 z) 0 3) "~"))
                                        )
                                      )
                                    )
                                  (nth 1 x)
                                  )
                            )
                          (setq tmp (regexp-opt tmp-lst4 'words))
                          (while (string-match "~" tmp)
                            (setq tmp (replace-match "[A-Z0-9_]*" 't 't tmp)))
                          (when (string-match "c[0-9]+" tmp)
                            (setq tmp (replace-match "[\"']?.+?[\"']?" 't 't tmp))
                            )
                          (when (string-match "#" tmp)
                            (setq tmp (replace-match "[+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)" 't 't tmp))
                            )
                          (when (string-match "(grd)" tmp)
                            (setq tmp (replace-match "[GNDS]+" 't 't tmp))
                            )
                          (when (string-match "(nr,ns,r0,r1)" tmp)
                            (setq tmp (replace-match "([ \t]*[0-9.]+[ \t,]+[0-9.]+[ \t,]+[0-9.]+[ \t,]+[0-9.]+[ \t]*)" nil nil tmp)))
                          (when (string-match "(mum)" tmp)
                            (setq tmp (replace-match "[MNDX]+" 't 't tmp))
                            )
                          (when (string-match "(s3format)" tmp)
                            (setq tmp (replace-match "[,st][0-9.A-Z]+" 't 't tmp))
                            )
                          (when (string-match "zerolist" tmp)
                            (setq tmp (replace-match "\\(?:[0-9]+\\|([ \t]*\\(?:[0-9]+[ \t,]+\\)?[ \t]*)\\)" 't 't tmp))
                            )
                          (when (string-match "translist" tmp)
                            (setq tmp (replace-match "([ \t]*\\(?:II\\|TIME\\)/24\\(?:\\|[.]00\\|[.]000\\)\\(?:[ \t,]+\\(?:II\\|TIME\\)/24\\(?:\\|[.]00\\|[.]000\\)\\)?[ \t]*)" 't 't tmp))
                            )
                          (when (string-match (regexp-quote "\\|skip") tmp)
                            (setq tmp (replace-match "" nil nil tmp)))
                          (when (string-match "skip" tmp)
                            (setq tmp "skip"))
                          ;; (usersub): Name may contain as many
                          ;; characters as fit a single line.  It must
                          ;; not start with a digit.  If name contains
                          ;; commas, semi- colons, equal signs, or
                          ;; parentheses, it must be surrounded by
                          ;; single quotes ' or double quotes ".
                          (when (string-match "(usersub)" tmp)
                            (setq tmp (replace-match esn-reg-filename 't 't tmp))
                            )
                          (when (string-match "filename" tmp)
                            (setq tmp (replace-match esn-reg-filename 't 't tmp))
                            )
                          (when nil
                            (when (string-match "list" tmp)
                              (setq tmp (replace-match "([^)]*)" 't 't tmp))
                              )
                            )
                          ;; Fortran Format specification can be:
                          ;; [r] fs

                          ;; r is a nonzero, unsigned, integer constant
                          ;; called a repeat specification

                          (unless (string= tmp "skip")
                            (setq ret (concat ret "\t(\"" op "\" "))
                            (setq tmp (esn-gen-cookies-quote tmp))
                            (setq ret (concat ret "\"" tmp "\""))
                            (setq ret (concat ret ")\n"))
                            )
                          )
                        )
                      )
                    )
                  (nth 1 y)
                  )
                 ;; Now put tmp-lst3 into regular expression
                 (when tmp-lst3
                   (setq tmp (concat "\\<\\(" (regexp-opt tmp-lst3) "[A-Z0-9_]*\\|" (esn-reg-records 't) "\\)\\>"))
                   (while (string-match "~" tmp)
                     (setq tmp (replace-match "[A-Z0-9_]*" 't 't tmp)))
                   (setq tmp2 (concat tmp2 "\t(\"" rec "\" \"" (esn-gen-cookies-quote tmp) "\")\n"))
                   )
                 )
               )
             )
           )
         )
       records
       ))
    (setq tmp2 (concat tmp2 ")\n\"* Regular expressions for which Options have Values in NONMEM " esn-nm-cookie-ver "\")\n"))
    (setq ret (concat ret ")\n)\n\"* Regular expressions for Option Values by Record and Option for NONMEM " esn-nm-cookie-ver "\")\n" tmp2))
    (setq tmp-lst '())
    (mapc (lambda(x)
            (when (string= "INC" x)
              (add-to-list 'tmp-lst "INC"))
            (add-to-list 'tmp-lst (concat "$" x)))
          tmp-lst2)
    (setq ret (concat ret "\n(defvar esn-records-which-have-val-reg-" esn-nm-cookie-ver " \"\\\\<" (esn-gen-cookies-quote (regexp-opt tmp-lst 't)) "[A-Z0-9_]*\\\\>\"\n\"* Regular expression for Records with OPTION=VALUE for NONMEM " esn-nm-cookie-ver "\")\n"))
    ;; Abbreviated records
    (setq ret (concat ret "(defvar esn-abbrev-records-regexp-" esn-nm-cookie-ver " \""))
    (setq tmp-lst '())
    (setq tmp (regexp-opt records-abbrev 'words))
    ;;Initial substrings of record or option names, and of length 3 or more, are recognized as abbreviations.
    (mapc (lambda(x)
            (when (and (>= (length x) 4)
                       (not (string-match tmp (substring x 0 4) ))
                       )
              (add-to-list 'tmp-lst (concat (substring x 0 4) "~"))
              )
            )
          records-abbrev
          )
    (mapc (lambda(x)
            (add-to-list 'tmp-lst x)
            )
          records-abbrev
          )
    (setq tmp (regexp-opt tmp-lst 'words))
    (while (string-match "~" tmp)
      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
    (setq tmp (esn-gen-cookies-quote tmp))

    (setq ret (concat ret tmp "\"\n\"* Regular expression for Abbreviated Records in NONMEM " esn-nm-cookie-ver "\")\n"))

    (setq tmp (regexp-opt (append dat-lst (list "z")) 'words))
    (while (string-match "~" tmp)
      (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
    (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
      (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
      )
    (when (string-match "z" tmp)
      (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
      )
    (setq tmp (esn-gen-cookies-quote tmp))


    (setq ret (concat ret "(defvar esn-input-data-items-" esn-nm-cookie-ver " \""
                      tmp
                      "\"\n\"* Regular expression for reserved NONMEM " esn-nm-cookie-ver " input keywords.\")\n"))

    (setq ret (concat ret "(defvar esn-abbrev-lhs-" esn-nm-cookie-ver " '(\n"))
    (mapc
     (lambda(x)
       (message "%s" x)
       (let (
             (var (nth 0 x))
             (lst (nth 1 x))
             (debug-on-error 't)
             )
         (if (string= var "$AESINITIAL")
             (setq var "AES0")
           (when (string-match "[$]" var)
             (setq var (replace-match "" nil nil var))
             )
           (when  (>= (length var) 3)
             (setq var (substring var 0 3) )
             )
           )
         (setq tmp (concat "\\<" (regexp-opt (append lst (mapcar (lambda(x) (concat x "!"))  (list "z"))) 't)))
         (while (string-match "~" tmp)
           (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
         (while (string-match "(#)" tmp)
           (setq tmp (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp)))
         (while (string-match "#" tmp)
           (setq tmp (replace-match "[0-9]+" nil nil tmp)))
         (while (string-match "!" tmp)
           (setq tmp (replace-match "\\\\>" nil nil tmp)))
         (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
           (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
           )
         (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
           (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
           )
         (when (string-match "z" tmp)
           (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
           )
         (setq ret (concat ret "\t(\"" var "\" \"" (esn-gen-cookies-quote tmp) "\")\n"))
         )
       )
     lhs-lst
     )
    (setq ret (concat ret "\n)\n\"* Left Handed Regular Expressions\")\n"))


    (setq ret (concat ret "(defvar esn-abbrev-rhs-" esn-nm-cookie-ver " '(\n"))
    (setq ret2 (concat "(defvar esn-abbrev-rhs-norec-" esn-nm-cookie-ver " '(\n"))
    (mapc
     (lambda(x)
       (message "%s" x)
       (let (
             (var (nth 0 x))
             (lst (nth 1 x))
             (debug-on-error 't)
             )
         (if (string= var "$AESINITIAL")
             (setq var "AES0")
           (when (string-match "[$]" var)
             (setq var (replace-match "" nil nil var))
             )
           (when  (>= (length var) 3)
             (setq var (substring var 0 3) )
             )
           )
         (setq tmp2 (concat "\\<" (regexp-opt lst 't)))
         (setq tmp (concat "\\<" (regexp-opt (append lst (mapcar (lambda(x) (concat x "!")) (list "z"))) 't)))
         (while (string-match "~" tmp)
           (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
         (while (string-match "(#)" tmp)
           (setq tmp (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp)))
         (while (string-match "#" tmp)
           (setq tmp (replace-match "[0-9]+" nil nil tmp)))
         (while (string-match "!" tmp)
           (setq tmp (replace-match "\\\\>" nil nil tmp)))
         (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
           (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
           )
         (when (string-match "z" tmp)
           (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp))
           )
         (setq tmp (esn-gen-cookies-quote tmp))
         (setq ret (concat ret "\t(\"" var "\" \"" tmp "\")\n"))
	 
         (setq tmp2 (concat "\\<" (regexp-opt lst 't)))
         (unless (string= tmp2 "\\<")
           (while (string-match "~" tmp2)
             (setq tmp2 (replace-match "[A-Z0-9_]*" nil nil tmp2)))
           (while (string-match "(#)" tmp2)
             (setq tmp2 (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp2)))
           (while (string-match "#" tmp2)
             (setq tmp2 (replace-match "[0-9]+" nil nil tmp2)))
           (while (string-match "!" tmp2)
             (setq tmp2 (replace-match "\\\\>" nil nil tmp2)))
           (setq tmp2 (esn-gen-cookies-quote tmp2))
           (setq ret2 (concat ret2 "\t(\"" var "\" \"" tmp2 "\")\n"))
           )
	 
         )
       )
     rhs-lst
     )
    (setq ret (concat ret "\n)\n\"* Right Handed Regular Expressions\")\n"
                      ret2 "\n)\n\"* Right Handed Regular Expressions without records\")\n"))
    
    (setq ret (concat ret "(defvar esn-abbrev-no-" esn-nm-cookie-ver " '(\n"))
    (mapc
     (lambda(x)
       (message "%s" x)
       (let (
             (var (nth 0 x))
             (lst (nth 1 x))
             (debug-on-error 't)
             )
         (if (string= var "$AESINITIAL")
             (setq var "AES0")
           (when (string-match "[$]" var)
             (setq var (replace-match "" nil nil var)))
           (when  (>= (length var) 3)
             (setq var (substring var 0 3))))
         (setq tmp (concat "\\<" (regexp-opt (append lst (mapcar (lambda(x) (concat x "!")) (list "z"))) 't)))
         (while (string-match "~" tmp)
           (setq tmp (replace-match "[A-Z0-9_]*" nil nil tmp)))
         (while (string-match "(#)" tmp)
           (setq tmp (replace-match "([ \t]*[0-9]+[ \t]*)" nil nil tmp)))
         (while (string-match "#" tmp)
           (setq tmp (replace-match "[0-9]+" nil nil tmp)))
         (while (string-match "!" tmp)
           (setq tmp (replace-match "\\\\>" nil nil tmp)))
         (when (string-match "\\[\\([A-Z0-9_]*?\\)z\\([A-Z0-9_]*?\\)\\]" tmp)
           (setq tmp (replace-match "\\\\(?:[\\1\\2]\\\\|INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp)))
         (when (string-match "z" tmp)
           (setq tmp (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\)" 't nil tmp)))
         (setq tmp (esn-gen-cookies-quote tmp))
         (setq ret (concat ret "\t(\"" var "\" \"" tmp "\")\n"))))
     no-lst)
    (setq ret (concat ret "\n)\n\"* Forbidden Regular Expressions\")\n"))
    
    (setq ret (concat ret"\n(provide 'esn-nm-cookies-" esn-nm-cookie-ver ")\n"))
    (with-temp-file (concat esn-path "/etc/cookies/esn-nm-cookies-" esn-nm-cookie-ver ".el")
      (insert ";; -*-no-byte-compile: t; -*-\n")
      (insert ret))))

(provide 'esn-get-cookies)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-get-cookies.el ends here
