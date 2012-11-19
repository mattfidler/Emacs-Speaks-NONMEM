;;; esn-align.el --- Alignment functions for Emacs Speaks NONMEM
;; 
;; Filename: esn-align.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Feb 10 08:32:43 2010 (-0600)
;; Version: 0.11
;; Last-Updated: Mon May  2 11:24:25 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 920
;; URL: esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility:23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This provides the alignment routines for Emacs Speaks NONMEM
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 23-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Dec 23 15:49:24 2010 (-0600) #875 (Matthew L. Fidler)
;;    Added new alignment for $TABLE $INPUT and $BIND
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 20:47:53 2010 (-0500) #727 (Matthew L. Fidler)
;;    Bug fix for aligning abbreviated code at equals sign.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 16:02:48 2010 (-0500) #724 (Matthew L. Fidler)
;;    Bug fix.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 13:57:32 2010 (-0500) #722 (Matthew L. Fidler)
;;    Made align function also use the exit hook.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 13:51:48 2010 (-0500) #716 (Matthew L. Fidler)
;;
;;    Now using esn-exit-record hook when available coupled with a timer for
;;    aligning and numbering matrices.
;;
;; 13-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 13 08:56:04 2010 (-0500) #140 (Matthew L. Fidler)
;;    Change esn-align-matrix to only align the current matrix...
;; 13-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 13 08:09:30 2010 (-0500) #136 (Matthew L. Fidler)
;;    Bug fix for align at equals.
;; 30-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 30 10:52:35 2010 (-0500) #132 (Matthew L. Fidler)
;;    Changed equals align to abbreviated records regular expression.
;; 30-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 30 10:19:51 2010 (-0500) #129 (Matthew L. Fidler)
;;    Removed variables
;; 27-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 27 09:48:25 2010 (-0500) #127 (Matthew L. Fidler)

;;    Changed esn-tmp-rep internal subroutine to external
;;    esn-est-tmp-rep to let compiler have no warnings.

;; 19-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Thu Aug 19 09:40:46 2010 (-0500) #113 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 12-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 12 13:21:00 2010 (-0500) #107 (Matthew L. Fidler)
;;    Removed $INPUT and $BIND alignment.
;; 11-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 11 08:42:12 2010 (-0500) #105 (Matthew L. Fidler)
;;    Bug fix for new NONMEM operators.
;; 03-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Aug  3 12:56:48 2010 (-0500) #66 (Matthew L. Fidler)
;;    Fixed timers to speed up movement and deletion in $THETA/$OMEGA block.
;; 25-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 25 08:51:22 2010 (-0500) #39 (Matthew L. Fidler)
;;    Changed alignment commands so they don't cause the buffer modified flag to be tripped.  This allows movement with alignment that doesn't require a save.
;; 17-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Jun 17 15:57:56 2010 (-0500) #28 (Matthew L. Fidler)
;;    Increased timer amounts
;; 11-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Feb 11 11:02:58 2010 (-0600) #23 (Matthew L. Fidler)
;;    Removed align-matrix timer.  Broke matrix alignment.
;; 11-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Feb 11 10:35:55 2010 (-0600) #14 (Matthew L. Fidler)
;;    Changed esn-align-matrix to be an idle-timer function.
;; 10-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 10 23:30:43 2010 (-0600) #9 (Matthew L. Fidler)
;;    Added pre-calculated regexp-opt for records that are aligned.
;; 10-Feb-2010    Matthew L. Fidler  
;;
;;    Added Timer for align-at-equals, canceled if called outside of equals
;;    alignment records.
;;
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
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(declare-function esn-fill-record "esn-fill")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-use-pdx-p "esn-which")
(declare-function esn-eta-error "esn-properties")
(declare-function esn-rec "esn-properties")


(eval-when-compile
  (require 'esn-start)
  (require 'esn-macros)
  (require 'esn-fun))
(require 'esn-rec-hooks)
(defvar esn-align-equals-fun-timer nil
  "* Alignment timer")
(defvar esn-align-matrix-timer nil
  "* Defines a matrix alignment timer"
  )
(defvar esn-number-theta-timer nil
  "* Alignment timer")
(defvar esn-number-eta-timer nil
  "* Alignment timer")
(defvar esn-number-eps-timer nil
  "* Alignment timer")


(defgroup esn-spacing-and-alignment nil
  "* EsN's spacing and alignment options"
  :group 'esn-changes-while-editing
  )

(defcustom esn-update-theta nil
  "* If not nil, then esn-mode will align and space the theta record."
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$theta-options
  )

(defcustom esn-update-omega nil
  "* If not nil, then esn-mode will align and space the omega record."
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$omega-options
  )

(defcustom esn-update-sigma nil
  "* If not nil, then esn-mode will align and space the sigma record"
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$sigma-options
  )


;;;

(defcustom esn-add-theta-number-to-labels nil 
  "* Place Theta numbers in labels"
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$theta-options
  )

(defcustom esn-take-out-tv-theta nil
  "* Take out the TV in TVCL type parameters for variable labels"
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$theta-options)

(defcustom esn-display-variable-name-in-labels nil
  "* Display variable name in THETA/OMEGA/SIGMA labels"
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$theta-options
  :group 'esn-$omega-options
  :group 'esn-$sigma-options)


(defcustom esn-add-eta-number-to-labels nil
  "* Determines if ETA numbers are added to labels"
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$omega-options)

(defcustom esn-align-matrices 't
  "* Defines if alignment for matrices is implemented."
  :type 'boolean
  :group 'esn-spacing-and-alignment
  :group 'esn-$sigma-options
  :group 'esn-$omega-options)

(defcustom esn-align-at-equals 't
  "* Defines if align at Equals is implemented."
  :type 'boolean
  :group 'esn-mode
  :group 'esn-spacing-and-alignment)

(defcustom esn-align-add-comma 't
  "* Add a `,' to separate THETA estimates.  Fixes a PsN 2.2.5 bug."
  :type 'boolen
  :group 'esn-spacing-and-alignment
  :group 'esn-$theta-options)


(defun esn-align-input-bind-get-bind (&optional destructive)
  "* Gets BIND from the narrowed region and returns a list"
  (let ((bin (esn-rec "BIN" 't nil destructive))
        start
        (ret '()))
    (with-temp-buffer
      (insert bin)
      (while (string-match (eval-when-compile (esn-reg-record-exp "BIN" 't 't)) bin start)
        (setq start (match-beginning 0))
        (setq bin (replace-match "" nil nil bin)))
      (setq ret (split-string bin nil 't)))))

(defun esn-align-input-bind-get-input (&optional destructive)
  "* Gets input from the narrowed region and returns a list"
  (let ((inp (esn-rec "INP" 't nil destructive))
        (tmp "")
        (tmp2 "")
        (ret '()))
    (with-temp-buffer
      (insert inp)
      (goto-char (point-min))
      (while (re-search-forward "\\([^ \t\n]+\\)" nil 't)
        (unless (save-match-data (string-match (eval-when-compile (esn-reg-record-exp "INP" 't)) (match-string 0)))
          (setq tmp (match-string 0))
          (when (looking-at "[ \t=]\\(DROP\\|SKIP\\)\\>")
            (setq tmp2 (match-string 0))
            (when (save-excursion (goto-char (match-end 0)) (not (save-match-data (looking-at "[ \t]*="))))
              (setq tmp (concat tmp tmp2))
              (goto-char (match-end 0))))
          (add-to-list 'ret tmp))))
    (setq ret (reverse ret))
    (symbol-value 'ret)))

(defun esn-align-input-bind-space (inp-q bind-q inp-b &optional bind-current)
  "* Make the spacing the same between $INPUT and $BIND items"
  (let ((len1 (length (symbol-value inp-q)))
        (len2 (length (symbol-value bind-q)))
        (i 0)
        (lst1 '())
        (lst2 '())
        (overs '())
        pt)
    (save-restriction
      (when bind-current
        (esn-narrow-rec)
        (setq pt (point-min))
        (save-excursion
          (untabify (point-min) (point-max))))
      (set inp-b (symbol-value (> len1 len2)))
      (setq lst1 
            (mapcar
             (lambda(x)
               (let ((y (nth i (if (symbol-value inp-b) 
                                   (symbol-value inp-q) 
                                 (symbol-value bind-q))))
                     yl
                     (z x)
                     (zl (length x))
                     bin
                     inp
                     spaces)
                 (setq yl (length y))
                 (when bind-current
                   (if inp-b
                       (progn
                         (setq bin z)
                         ;; Bin is larger than INP then one space in-betwen, otherwise 1+difference in length
                         (setq spaces (if (> zl yl) 1 (+ 1 (- yl zl)))))
                     (setq bin y)
                     (setq spaces (if (> yl zl) 1 (+ 1 (- zl yl)))))
                   ;; Insert spaces
                   (save-excursion
                     (goto-char pt)
                     (search-forward bin nil 't)
                     (cond
                      ( (looking-at (format "\\( \\{%s\\}\\)\\( +\\)\\(?:\\<\\|-\\)" spaces))
                        ;; Too big, remove spaces at end.
                                        ;                        (message "%s, %s large" bin spaces)
                        (goto-char (match-end 1))
                        (delete-char (length (match-string 2)))) 
                      ( (= spaces 1) ;; Skip looking at one space
                                        ;                        (message "%s, %s one space" bin spaces)
                        )
                      ( (looking-at (format "\\( \\{1,%s\\}\\)\\(?:\\<\\|-\\)" (- spaces 1)))
                        ;; Too small, add spaces at end
                                        ;                        (message "%s, %s small" bin spaces)
                        (goto-char (match-end 1))
                        (insert (make-string  (- spaces (length (match-string 1))) ? )))
                      ( 't
                                        ;                        (message "%s, %s large" bin space)
                        ))
                     (setq pt (point))))
                 (if (> zl yl)
                     (setq y (concat y (make-string (- zl yl) ? )))
                   (setq z (concat z (make-string (- yl zl) ? ))))
                 (push y lst2)
                 (setq i (+ i 1))
                 
                 (symbol-value 'z)))
             (if (symbol-value inp-b)
                 (symbol-value bind-q)
               (symbol-value inp-q))))
      (setq lst2 (reverse lst2))
      (while (< i (if (symbol-value inp-b) len1 len2))
        (push (nth i (if (symbol-value inp-b) (symbol-value inp-q) (symbol-value bind-q))) overs)
        (setq i (+ i 1)))
      (if (symbol-value inp-b)
          (progn
            (set bind-q (symbol-value 'lst1))
            (set inp-q (symbol-value 'lst2)))
        (set inp-q (symbol-value 'lst1))
        (set bind-q (symbol-value 'lst2)))
      (setq overs (reverse overs))
      (symbol-value 'overs))))

(defun esn-align-input-bind-output-input-bind (input &optional second-space is-bind prior-lst before-txt after-txt)
  "* Returns an output list of $INPUT records to interleave with bind.  
If second-space is true, later records are just indented with spaces.
If is-bind is true, produces $BIND records"
  (let ((rec (if is-bind "$BIND " "$INPUT"))
        (ret-lst '())
        (cur-line "")
        (lst prior-lst)
        (first (if before-txt before-txt ""))
        (bin-replace (eval-when-compile (concat "$D " (make-string 1 595)))) ; beta with hook
        (inp-replace (eval-when-compile (concat "$D " (make-string 1 616)))) ; i with stroke
        p0
        pm
        ps)
    (save-restriction
      (save-excursion
        (widen)
        (when lst
          (esn-narrow-rec)
          (setq ps (point-min))
          (widen)
          (if (re-search-backward (eval-when-compile (esn-reg-record-exp "PRO" nil )) nil t)
              (setq p0 (point))
            (setq p0 (point-min)))
          (goto-char ps))
        (setq cur-line rec)
        (when second-space
          (setq rec (make-string 6 ? )))
        (mapc (lambda(x)
                (let ( 
                      tmp
                      )
                  (setq tmp (concat cur-line  " " x))
                  (cond
                   ( (> (length tmp) fill-column)
                     (when lst
                       (setq pm (point))
                       (save-excursion
                         (goto-char p0)
                         (if (search-forward (if is-bind  inp-replace bin-replace) pm 't)
                             (replace-match (concat first (pop lst)))
                           (goto-char pm)
                           (re-search-backward (if is-bind (eval-when-compile
                                                             (esn-reg-record-exp "BIN" 't))
                                                 (eval-when-compile
                                                   (esn-reg-record-exp "INP" 't))) nil t)                             
                           (insert (concat first (if is-bind "\n" "") (pop lst) "\n"))
                           (setq first "")))
                       (skip-chars-forward " \t")
                       (insert (concat "\n" (if is-bind "$BIND  " "$INPUT "))))
                     (push cur-line ret-lst)
                     (setq cur-line (concat rec " " x)))
                   ( 't
                     (setq cur-line tmp)))
                  (when lst
                    (search-forward x nil 't))))
              input)
        (push cur-line ret-lst)
        (setq ret-lst (reverse ret-lst))
        (when lst
          (save-restriction
            (save-excursion
              (widen)
              (esn-narrow-rec)
              (when after-txt
                (goto-char (point-max))
                (insert (concat "\n" after-txt)))
              (goto-char (point-min))
              (widen)
              (delete-region (point) (progn (skip-chars-backward " \t\n") (point)))
              (insert (concat "\n" (if is-bind "\n" "") (pop lst) "\n")))))
        (symbol-value 'ret-lst)))))
(defun esn-align-input-bind-interleave (input bind &optional input-bigger)
  "* Puts input and bind into one interleaved list"
  (let ((ret '())
        (i 0)
        (space (make-string 6 ? ))
        pt)
    (mapc (lambda(x)
            (let (
                  (inp (if input-bigger (nth i input) x))
                  (bin (if input-bigger x (nth i bind))))
              (setq i (+ i 1))
              (setq ret (push inp ret))
              (setq ret (push bin ret))))
          (if input-bigger 
              bind
            input))
    (while (< i (if input-bigger (length input) (length bind)))
      ;; Remove $BIND or $INPUT record information...
      (setq ret (push (concat space (substring (nth i (if input-bigger input bind)) 6)) ret))
      (setq i (+ i 1)))
    (setq ret (reverse ret))
    (symbol-value 'ret)))

(defun esn-align-input-bind-finish-wrap ()
  "* Finishes wrapping of the record"
  (interactive)
  ;; Placeholder for now.
  )
(defvar esn-started-align-input-bind nil
  "* To stop recursive calls to esn-align-input-bind"
  )
(defun esn-align-input-bind ()
  "* Aligns and interleaves BIND and INPUT records"
  (interactive)
  (setq esn-started-align-input-bind nil)
  (unless esn-started-align-input-bind
    (setq esn-started-align-input-bind 't)
                                        ;    (condition-case nil
    (let (p1 p2 p3 p4 inp-pre bin-pre inp-post bin-post current input-bigger
             over-lst pre-txt (i 0) (rec (esn-get-current-rec))
             (bin-replace (eval-when-compile (concat "$D "(make-string 1 595)))) ; beta with hook
             (inp-replace (eval-when-compile (concat "$D " (make-string 1 616)))) ; i with stroke
             only-one)
      (save-restriction
        (save-excursion
          (esn-narrow-to-current-problem)
          (setq p1 (point-min))
          (setq p4 (point-max))
          (cond 
           ( (string= "INP" rec)
             (setq only-one (not (esn-rec "BIN"))))
           ( (string= "BIN" rec)
             (setq only-one (not (esn-rec "INP")))))))
      (if only-one
          (progn
            ;; If it is only one record (either $INPUT or $BIND , just fill
            ;; it in regularly. 
            (esn-fill-record))
        (setq p2 esn-get-current-record-start)
        (setq p3 esn-get-current-record-stop)
        (save-restriction
          (save-excursion 
            (narrow-to-region p2 p3)
            (setq current (if (string= "BIN" rec)
                              (esn-align-input-bind-get-bind)
                            (esn-align-input-bind-get-input)))))
        (when current
          ;; Only destroy when there are ITEMS in $INPUT or $BIND.
          (save-excursion
            (save-restriction
              (narrow-to-region p3 p4)
              (setq bin-post (esn-align-input-bind-get-bind bin-replace))
              (setq inp-post (esn-align-input-bind-get-input inp-replace))))
          (save-excursion
            (save-restriction
              (narrow-to-region p1 p2)
              (setq bin-pre (esn-align-input-bind-get-bind bin-replace))
              (setq inp-pre (esn-align-input-bind-get-input inp-replace))))
          (esn-get-current-rec 't)
          (setq p2 esn-get-current-record-start)
          (setq p3 esn-get-current-record-stop)
          (save-excursion
            (save-restriction
              (esn-narrow-to-current-problem)
              (setq p1 (point-min))
              (setq p4 (point-max))))
          (cond
           ( (string= "BIN" rec)
             (cond
              ( (and inp-pre current (not (or bin-pre inp-post bin-post)))
                ;; There is at least one $INPUT record prior, with no prior $BIND, no post $INPUT and no post $BIND.
                (save-excursion
                  (save-restriction
                    (setq over-lst (esn-align-input-bind-space 'inp-pre 'current 'input-bigger 't))))
                (cond
                 ( input-bigger
                   (save-excursion
                     ;; Now insert current record information.
                     (esn-align-input-bind-output-input-bind current nil 't
                                                             (esn-align-input-bind-output-input-bind inp-pre)
                                                             nil
                                                             (concat "\n" (mapconcat (lambda(x) x) 
                                                                                     (esn-align-input-bind-output-input-bind over-lst 't)
                                                                                     "\n"
                                                                                     )))))
                 ( 't
                   ;; Bind is bigger
                   (esn-align-input-bind-output-input-bind current nil 't
                                                           (esn-align-input-bind-output-input-bind inp-pre)
                                                           nil
                                                           nil)
                   (esn-align-input-bind-finish-wrap))))
              ( (and inp-pre bin-pre current (not (or inp-post bin-post)))
                ;; There is at least one $INPUT record prior, with at least
                ;; one prior $BIND, no post $INPUT and no post $BIND.
                (save-excursion
                  (save-restriction
                    (setq over-lst (esn-align-input-bind-space 'inp-pre 'bin-pre 'input-bigger 't))))
                (cond
                 ( input-bigger
                   (message "%s" 
                            (mapconcat (lambda(x) x)
                                       (esn-align-input-bind-output-input-bind inp-pre) "\n"))
                   (message "%s"
                            (mapconcat (lambda(x) x)
                                       (esn-align-input-bind-output-input-bind bin-pre nil 't) "\n"))
                   ;; Working here.
                   )
                 ( 't
                   )))
              ( 't
                (message "Default"))))
           ( (string= "INP" rec)
             (message "Input record")))))
                                        ;          (error nil))
      (setq esn-started-align-input-bind nil))))
(defun esn-align-equals-fun-hook ()
  "Hook for PK alignment function."
  (when esn-align-equals-fun-timer 
    (cancel-timer esn-align-equals-fun-timer))
  (when (eq major-mode 'esn-mode)
    (esn-align-equals-fun-actual)))
;;;###autoload

(defun esn-align-equals-fun ()
  "Defines alignment function for PK -- Creates an idle timer."
  (when esn-align-equals-fun-timer 
    (cancel-timer esn-align-equals-fun-timer))
  ;; Only set timer if at appropriate records
  (when (and esn-align-at-equals
             (string-match esn-current-abbrev-records-regexp (concat "$" (esn-get-current-record))))
    (setq esn-align-equals-fun-timer (run-with-idle-timer 1 nil 'esn-align-equals-fun-hook))
    (add-hook 'esn-exit-record-hook 'esn-align-equals-fun-hook 't)))

(esn-abbrev-post-hook 'esn-align-equals-fun)


(defun esn-align-equals-fun-actual ()
  "Defines alignment function for PK.  Should not cause buffer modification."
  (interactive)
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (save-restriction
      (if (and esn-align-at-equals 
               (string-match esn-current-abbrev-records-regexp
                             (concat "$" (esn-get-current-record))))
          (save-excursion
            (let
                ((pm (point-min)) 
                 (buf-mod (buffer-modified-p)))
              (esn-narrow-rec)
              (unless (= pm (point-min))
                (goto-char (point-min))
                (let (
                      (align-to-tab-stop nil)
                      (space-num '()))
                  (goto-char (point-min))
                  (while (search-forward ";" nil t)
                    (insert " "))
                  ;; Switch out NONMEM 7 operators as follows:
                  ;; ==  -> ^^
                  ;; /=  -> /^
                  ;; >=  -> >^
                  ;; <=  -> <^
                  (goto-char (point-min))              
                  (while (re-search-forward "\\([=/><]\\)=" nil t)
                    (cond
                     ( (string= "==" (match-string 0))
                       (replace-match "^^"))
                     ( 't
                       (replace-match "\\1^"))))
                  (goto-char (point-min))
                  (while (re-search-forward "^\\( +\\)" nil t)
                    (add-to-list 'space-num (length (match-string 1))))
                  ;; Align different alignments to different spaces.
                  (mapc (lambda(space)
                          (let (
                                (sp (make-string space ? )))
                            (align-regexp (point-min) (point-max)
                                          (format "^%s[^=\n \t()]*\\(?:([0-9.]+\\(?: *, *[0-9.]+\\)?)\\)?\\([ \t]*\\)[=]" sp)
                                          1 1 t)
                            (align-regexp (point-min) (point-max)
                                          (format "^%sIF *([^)]*) *[^=\n \t]*\\([ \t]*\\)[=]" sp)
                                          1 1 t)))
                        space-num
                        )
                  (esn-align-matrix-comment))
                (untabify (point-min) (point-max))
                (goto-char (point-min))
                (while (search-forward ";" nil t)
                  (delete-char 1))
                (goto-char (point-max))
                (while (re-search-backward "=\\([^ ]\\)" nil t)
                  (forward-char 1)
                  (insert " "))
                (when (and nil (string= "DES" (esn-get-current-record)))
                  (let (
                        (mxa (esn-max-a))
                        (i 1)
                        (align-to-tab-stop nil))
                    (goto-char (point-min))
                    (while (re-search-forward "=" nil t)
                      (skip-chars-forward " \t")
                      (unless (looking-at "[-+]")
                        (insert "+")))
                    (goto-char (point-min))
                    (while (re-search-forward "=" nil t)
                      (setq i 1)
                      (while (<= i mxa)
                        (unless (re-search-forward
                                 (format "\\=[ \t]*[+-].*?[Aa](%s)[^ \t\n]*" i)
                                 nil t)
                          (and (looking-at "[ \t]*[+-]\\([^ \t\n]*\\)")
                               (not (string-match "A([0-9.]+)" (match-string 1)))
                               (re-search-forward "\\=[ \t]*[+-]\\([^ \t\n]*\\)" nil t))
                          (insert (format "+~A(%s)" i)))
                        (setq i (+ i 1))))
                    (align-regexp (point-min) (point-max)
                                  "\\([ \t]*\\)[+-].*?A([0-9.]+)[^ \t\n]*"
                                  1 1 't)
                    (untabify (point-min) (point-max))
                    (goto-char (point-min))
                    (while (re-search-forward "\\+~A([0-9.]+)" nil t)
                      (unless (esn-in-comment-p (length (match-string 0)))
                        (replace-match (make-string (length (match-string 0)) ? ))))
                    (goto-char (point-min))
                    (while (re-search-forward "  +\\(;.*$\\|$\\)" nil t)
                      (backward-char (length (match-string 1)))
                      (just-one-space))))
                ;; Switch back NONMEM 7 operators as follows:
                ;; ==  <- ^^
                ;; /=  <- /^
                ;; >=  <- >^
                ;; <=  <- <^
                (goto-char (point-min))              
                (while (re-search-forward "\\([/><]\\|\\^\\)\\^" nil t)
                  (cond
                   ( (string= "^^" (match-string 0))
                     (replace-match "=="))
                   ( 't
                     (replace-match "\\1="))))
                (goto-char (point-max)))
              (widen)
              (unless buf-mod
                (set-buffer-modified-p nil))))))))
(defun esn-align-matrix-comment ()
  "Aligns the matrix comment for point-min and point-max"
  (goto-char (point-min))
  (while (re-search-forward ";" nil t)
    (insert "~")
    (end-of-line))
  (goto-char (point-min))
  (let ((del nil) (nchar 0))
    (while (re-search-forward "^ *;~" nil t)
      (save-excursion
        (beginning-of-line)
        (when (looking-at "\\( *\\);~")
          (setq nchar (length (match-string 1)))
          (forward-line -1)
          (beginning-of-line)
          (if (not (looking-at "\\(.*?\\)[ \t]*;~"))
              (setq del 't)
            (if (< nchar
                   (- (length (match-string 1)) (floor (* 0.1
                                                          (length (match-string 1))))))
                (setq del 't)
              (setq del nil)))))
      (when del
        (backward-char 1)
        (delete-char 1)
        (end-of-line))))
  ;; Don't align comments if they are on a blank line.
  (align-regexp (point-min) (point-max)
                "\\([ \t]*\\);~" 1 1 nil)
  (if (esn-number-estimates)
      (progn
        (align-regexp (point-min) (point-max)
                      ";~[Cc]?\\(?:\\[[AFPafp]\\]\\)?\\(?:[ \t]*\\)\\(?:\\(?:THETA\\|EPS\\|ETA\\)(\\)?[0-9.]+\\(?:)\\)?\\([ \t]*\\)." 1 1 nil)
        (align-regexp (point-min) (point-max)
                      ";~[Cc]?\\(?:\\[[AFPafp]\\]\\)?\\(?:[ \t]*\\(?:\\(?:THETA\\|EPS\\|ETA\\)(\\)?[0-9.]+\\(?:)\\)?[ \t]*[-=]\\).*?\\([ \t]*\\)[-=]" 1 1 nil))
    (align-regexp (point-min) (point-max)
                  ";~.*?\\([ \t]*\\)[-=]" 1 1 nil))
  (goto-char (point-min))
  (while (re-search-forward ";~" nil t)
    (backward-char 1)
    (delete-char 1)
    (end-of-line))
  (untabify (point-min) (point-max)))

(defun esn-theta-add-comma ()
  "Adds commas to the thetas."
  (goto-char (point-min))
  (let ((pt nil))
    (while (re-search-forward "(" nil t)
      (while (re-search-forward "\\=[ \t]*\\(?:-?INF\\|FIX\\(?:ED?\\)?\\|-?[0-9.]+\\)[ \t]*" nil t)
        (unless (re-search-forward "\\=\\([,)]\\|[ \t]*\\(?:$\\|;\\)\\)" nil t)
          (setq pt (point))
          (if (re-search-backward ")[^(]*\\=" nil t)
              (goto-char pt)
            (unless (esn-in-comment-p)
              (just-one-space)
              (backward-char 1)
              (insert ","))))))
    (align-regexp  (point-min) (point-max) ",\\( *\\)" 1 1 t)
    (untabify (point-min) (point-max))))

(defun esn-align-matrix-hook ()
  "Hook to align matrix."
  (when esn-align-matrix-timer
    (cancel-timer esn-align-matrix-timer))
  (when (eq major-mode 'esn-mode)
    (let (
          (rec (esn-get-current-rec)))
      (when (or (string= rec "THE") (string= rec "OME")
                (string= rec "THT") (string= rec "SIG"))
        (esn-align-matrix-actual-1))))
  ;; Remove hook.
  (remove-hook 'esn-exit-record-hook 'esn-align-matrix-hook 't))


(defun esn-align-matrix ()
  (interactive)
  ;; Add timer and run if the record has been exited.
  (when esn-align-matrix-timer
    (cancel-timer esn-align-matrix-timer))
  (setq esn-align-matrix-timer (run-with-timer 1 nil 'esn-align-matrix-hook))
  (add-hook 'esn-exit-record-hook 'esn-align-matrix-hook 't 't))

;; Add alignment to THETA OMEGA and SIGMA blocks
(esn-tos-post-hook 'esn-align-matrix)

(defun esn-align-matrix-actual-1 ()
  "Align matricies and comments.  Should not cause buffer to be modified"
  (interactive)
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (save-restriction
      (let ((rec (esn-get-current-record)))
        (when (and esn-align-matrices
                   (string-match "\\(THE\\|THT\\|OME\\|SIG\\)"
                                 rec))
          (save-excursion
            (save-excursion
              (re-search-backward "\\<\\$" nil t)
              (end-of-line)
              (esn-narrow-rec))
            (let ((align-to-tab-stop nil)
                  (case-fold-search 't)
                  (add-it nil)
                  (pmx (point-max))
                  (blk nil)
                  (len 0)
                  (per "?")
                  (in-com nil)
                  (pt (point))
                  (esn-tmp-goto-pt nil)
                  (same nil)
                  (buf-mod (buffer-modified-p)))
              ;; Update point.
              (goto-char (point-min))
              (when (re-search-forward "\\<BLOCK *\\(( *[0-9]+ *)\\)?" nil t)
                (widen)
                (setq blk 't)
                (narrow-to-region (point) pmx))
              (when (and blk (re-search-forward "\\<SAME?\\>" nil t))
                (setq blk nil)
                (setq same nil))
              (unless blk
                (unless same
                  ;; Standard align.
                  (when (and esn-align-add-comma
                             (or (string= rec "THE") (string= rec "THT")))
                    ;;
                    (esn-theta-add-comma))
                  (esn-align-matrix-comment)))
              (when blk
                (goto-char pt)
                (save-excursion
                  ;; Step one, put periods in where they are needed.
                  (goto-char (point-min))
                  (while (re-search-forward "-?[0-9]+\\>" nil t)
                    (setq add-it 't)
                    (save-excursion
                      (when (re-search-backward "\\.[0-9]+\\=" nil t)
                        (setq add-it nil))
                      (when (re-search-backward ";.*\\=" nil t)
                        (setq add-it nil)))
                    (if add-it
                        (if (looking-at " ")
                            (replace-match "!")
                          (insert "!"))))
                  ;; Step two, take out periods and ! in comments.
                  (goto-char (point-min))
                  (while (re-search-forward "[.!]" nil t)
                    (save-match-data
                      (setq in-com (re-search-backward ";.*\\=" nil t)))
                    (when in-com
                      (if (string= (match-string 0)  ".")
                          (replace-match "~~,~~")
                        (replace-match "~,~"))))
                  ;; Step three, take out double spaces unless currently in the double
                  ;; spaced area.
                  )
                (skip-chars-backward " \t")
                (setq pt (point))
                (goto-char (point-max))
                (while (re-search-backward "[^ ]  +" nil t)
                  (unless (= pt (point))
                    (forward-char 2)
                    (looking-at " *")
                    (replace-match "")))
                ;; Step four, put in ~ spacers for numbers and periods like " ."
                (goto-char (point-min))
                (while (re-search-forward " \\." nil t)
                  (unless (esn-in-comment-p)
                    (backward-char 2)
                    (insert "~")
                    (delete-char 1)
                    (forward-char 1)))
                (align-regexp (point-min) (point-max)
                              "\\([ \t]*\\)[.!]" 1 0 t)
                (goto-char (point-min))
                (while (re-search-forward "~~,~~" nil t)
                  (replace-match "."))
                (goto-char (point-min))
                (while (re-search-forward "~,~" nil t)
                  (replace-match "!"))
                (untabify (point-min) (point-max))
                (goto-char (point-min))
                (while (re-search-forward "\\(-?[0-9]+\\)\\( *\\)\\([.!]\\)" nil t)
                  (setq per (match-string 3))
                  (backward-char (length (match-string 0)))
                  (insert (make-string (length (match-string 2)) ? ))
                  (forward-char (length (match-string 1)))
                  (delete-region (point) (+ (point) (length (match-string 2))))
                  (when (looking-at "!")
                    (replace-match " ")))
                ;; Now take out ~.
                (goto-char (point-min))
                (while (re-search-forward "~" nil t)
                  (unless (esn-in-comment-p)
                    (replace-match " ")))
                (esn-align-matrix-comment))
              (widen)
              (unless buf-mod
                (set-buffer-modified-p nil)))))))))


(defun esn-number-theta-hook ()
  "Hook to number THETAs."
  (when esn-number-theta-timer
    (cancel-timer esn-number-theta-timer))
  (when (eq 'esn-mode major-mode)
    (esn-number-est))
  (remove-hook 'esn-exit-record-hook 'esn-number-theta-hook 't))

(defun esn-number-theta ()
  (interactive)
  (if (esn-number-estimates)
      (when esn-number-theta-timer
        (cancel-timer esn-number-theta-timer))
    (add-hook 'esn-exit-record-hook 'esn-number-theta-hook 't 't)
    (setq esn-number-theta-timer (run-with-idle-timer 1 nil 'esn-number-theta-hook))))

(esn-rec-post-hook "theta" 'esn-number-theta)

(defun esn-number-eta-hook ()
  "Hook to number ETAs."
  (when esn-number-eta-timer
    (cancel-timer esn-number-eta-timer))
  (when (eq 'esn-mode major-mode)
    (esn-number-est "OME"))
  (remove-hook 'esn-exit-record-hook 'esn-number-eta-hook 't))

(defun esn-number-eta ()
  (interactive)
  (when (or (esn-number-estimates) (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels))
    (when esn-number-eta-timer
      (cancel-timer esn-number-eta-timer))
    (add-hook 'esn-exit-record-hook 'esn-number-eta-hook 't 't)
    (setq esn-number-eta-timer (run-with-idle-timer 1 nil 'esn-number-eta-hook))))

(esn-rec-post-hook "omega" 'esn-number-eta)

(defun esn-number-eps-hook ()
  "Hook to renumber EPS."
  (when esn-number-eps-timer
    (cancel-timer esn-number-eps-timer))
  (when (eq major-mode 'esn-mode)
    (esn-number-est "SIG"))
  (remove-hook 'esn-exit-record-hook 'esn-number-eps-hook 't))

(defun esn-number-eps ()
  (interactive)
  (when (or (esn-number-estimates) (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels))
    (when esn-number-eps-timer
      (cancel-timer esn-number-eps-timer))
    (add-hook 'esn-exit-record-hook 'esn-number-eps-hook 't 't)
    (setq esn-number-eps-timer (run-with-idle-timer 1 nil 'esn-number-eps-hook))))

(esn-rec-post-hook "sigma" 'esn-number-eps)


(defun esn-number-est-change-it (val)
  (let (
        (num "\\<[+-]?[0-9.]+\\(?:[Ee][+-][0-9]+\\)?\\>"))
    (cond
     ((looking-at (format "\\(([ \t]*\\)%s\\([ \t,]*\\(?:FIX[A-Z]+\\>\\)?)\\)" num))
      (replace-match (format "\\1%s\\2" val)))
     ((looking-at (format "\\(([ \t]*%s\\(?:[ \t]+\\|[ \t]*,[ \t]*\\)\\)\\(%s\\|\\)" num num))
      (replace-match (format "\\1%s" val))
      (re-search-forward "[^)\n]*)" nil t))
     ((looking-at (format "%s" num))
      (replace-match (format "%s" val))))))
(defun esn-est-tmp-rep (n what var-types var fpe same)
  "* Subroutine that changes numbering"
  (let (tmp)
    (when (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels
               (or (string= what "OME")
                   (string= what "SIG")))
      (setq tmp "")
      (when (<= n (length var-types))
        (setq tmp (nth (- n 1) var-types))
        (if (string-match "?" tmp)
            (setq tmp "")
          (if (string-match "ADD" tmp)
              (setq tmp "A")
            (setq tmp "P"))))
      (if (string= tmp "")
          (if (looking-at "[ \t]*\\[[Ff]\\]")
              (replace-match ""))
        (if (not (looking-at "[ \t]*\\[\\([APFapf]\\)\\]"))
            (unless (looking-at "[ \t]*\\[")
              (insert (format "[%s]" tmp)))
          (unless (string= (upcase (match-string 1)) tmp)
            (replace-match (format "[%s]" tmp))))))
    (re-search-forward "\\=[ \t]*\\[[AFPafp]\\]" nil t)
    (skip-chars-forward " \t")
    (unless (and var fpe)
      (when (esn-number-estimates)
        (if (looking-at "\\(\\(?:\\(?:THETA\\|EPS\\|ETA\\)(\\)?\\)\\([0-9.]+\\)\\(\\(?:)\\)?\\)")
            (progn
              (unless (string= (match-string 2) (number-to-string n))
                (replace-match (concat "\\1" (number-to-string n) "\\3") 't nil))
              (save-excursion
                (forward-char (length (match-string 0)))
                (when (and (esn-mode-use-cov-type-labels) (looking-at "\\([ \t]*\\)COV\\>"))
                  (forward-char (length (match-string 1)))
                  (delete-char 3))))
          (skip-chars-backward " \t")
          (when (looking-at "[ \t]*")
            (replace-match ""))
          (if (and (esn-tos-comments-use-c) (not (esn-use-pdx-p)))
              (insert "C"))
          (if (esn-full-numbering)
              (progn 
                (cond
                 ( (or (not what) (string= what "THE"))
                   (insert (format " THETA(%s) - " n))
                   (if (and (esn-use-pdx-p)
                            esn-mode-auto-generate-brackets-for-theta
                            )
                       (insert "[]")))
                 ( (string= what "OME")
                   (insert (format " ETA(%s) - " n)))
                 ( (string= what "SIG")
                   (insert (format " EPS(%s) - " n)))
                 ( 't
                   (insert (format " %s - " n))))
                (when (and (esn-mode-use-cov-type-labels) (looking-at "[ \t]*COV\\>"))
                  (replace-match "")))
            (insert (format " %s - " n))
            (if (and (or (not what) (string= what "THE"))
                     (esn-use-pdx-p)
                     esn-mode-auto-generate-brackets-for-theta
                     )
                (insert "[]"))
            (when (and (esn-mode-use-cov-type-labels) (looking-at "[ \t]*COV\\>"))
              (replace-match ""))))))
    (end-of-line)
    (unless same
      (while (re-search-forward "\\=\n[ \t]*;.*" nil t)))))
(defun esn-number-est (&optional what var fpe)
  "Numbers commented estimates; Also replaces if var and fpe are non-nil"
  (interactive)
  (save-restriction (save-excursion
                      (let* (
                             (the-search (esn-reg-record-exp (or what "THE")))
                             (ret '())
                             (vars nil)
                             (lblk nil)
                             (blk nil)
                             (same nil)
                             (est (if (or (not what) (string= (upcase what) "THE"))
                                      "\\(?:([^)]*)\\|[0-9.]+\\)\\([ \t]+FIX\\(?:ED?\\)?\\)?"
                                    "\\(?:[0-9.]+\\([ \t]+FIX\\(?:ED?\\)?\\)?\\|BLOCK *\\(?:( *[0-9]+\\([ \t]+FIX\\(?:ED?\\)?\\)? *)\\)? +\\(SAME\\)?\\)"))
                             (com (if (or (not what) (string= (upcase what) "THE"))
                                      "\\= *;[;Cc]* *"
                                    "\\= *)? *;[;Cc]* *"))
                             (comsame ";[;Cc]* *")
                             (is-block-def nil)
                             (i 0)
                             (j 0)
                             (lj 0)
                             (lblkn 0)
                             (nrow 0)
                             (ncol 0)
                             (case-fold-search 't)
                             (tmp "")
                             (mtch "")
                             (do-blk (save-excursion
                                       (not (re-search-backward "\\<BLOCK *([^)]*\\=" nil t))))
                             (var-types '()))
                        (when (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels
                                   (string= what "OME"))
                          (setq var-types (esn-eta-error)))
                        (save-excursion
                          (goto-char (point-min))
                          (while (re-search-forward the-search nil t)
                            (end-of-line)
                            (esn-narrow-rec)
                            (goto-char (point-min))
                            (setq blk (re-search-forward "\\<BLOCK\\>" nil t))
                            (goto-char (point-min))
                            (setq same (re-search-forward "\\<SAME\\>" nil t))
                            (goto-char (point-min))
                            (if blk
                                (if same
                                    (progn
                                      (setq i 0)
                                      (goto-char (point-min))
                                      (while (and (< i lblk) (re-search-forward comsame nil t))
                                        (setq j (+ j 1))
                                        (esn-est-tmp-rep j what var-types var fpe same)
                                        (setq i (+ i 1)))
                                      (while (< i lblk)
                                        (setq j (+ j 1))
                                        (setq i (+ i 1))))
                                  (setq lj j)
                                  (setq i 0)
                                  (setq lblk 0)
                                  (while (re-search-forward est nil t)
                                    (if (esn-in-comment-p)
                                        (end-of-line)
                                      (setq mtch (match-string 0))
                                      (save-excursion
                                        (setq is-block-def (re-search-backward "\\<BLOCK *( *[0-9]+ *)?\\=" nil t)))
                                      (while (and is-block-def (re-search-forward est nil t))
                                        (if (esn-in-comment-p)
                                            (end-of-line)
                                          (setq mtch (match-string 0))
                                          (save-excursion
                                            (setq is-block-def (re-search-backward "\\<BLOCK *( *[0-9]+ *)?\\=" nil t)))))

                                        ;               (replace-match "")
                                      (setq i (+ 1 i))
                                      (setq nrow (/ (+ -1 (sqrt (+ 1 (* 8 i)))) 2))
                                      (if (not (= (floor nrow) (ceiling nrow)))
                                          (progn
                                            (if (and (or (and
                                                          (esn-use-pdx-p)
                                                          esn-mode-use-omega-sigma-var-type-labels
                                                          )
                                                         (esn-mode-use-cov-type-labels))
                                                     (or (string= what "OME")
                                                         (string= what "SIG")))
                                                (progn
                                                  (when (re-search-forward com nil t)
                                                    (if (and (esn-use-pdx-p)
                                                             esn-mode-use-omega-sigma-var-type-labels
                                                             )
                                                        (if (looking-at "[ \t]*\\[\\([FAPfap]\\)\\]")
                                                            (unless (string= "F" (upcase (match-string 1)))
                                                              (replace-match "[F]")
                                                              (when (looking-at "[ \t]*\\(\\(?:ETA\\|EPS\\)(\\)?[0-9.]\\(?:)\\)?[ \t]*[=-]")
                                                                (replace-match "")))
                                                          (unless (looking-at "[ \t]*\\[")
                                                            (insert "[F]"))))
                                                    (when (looking-at "[ \t]*\\(?:\\(?:THETA\\|EPS\\|ETA\\)(?\\)?[0-9.]+\\(?:)\\)?\\([ \t]*\\)[=-]?")
                                                      (replace-match ""))
                                                    (when (esn-mode-use-cov-type-labels)
                                                      (unless (looking-at "[ \t]*COV\\>")
                                                        (insert "COV "))))
                                                  (setq nrow (ceiling nrow))
                                                  (setq ncol (- nrow (- (/ (* nrow (+ nrow 1)) 2) i)))
                                                  (setq nrow (+ lj nrow)))
                                              (when (and var fpe)
                                                (setq nrow (ceiling nrow))
                                                (setq ncol (- nrow (- (/ (* nrow (+ nrow 1)) 2) i)))
                                                (setq nrow (+ lj nrow))
                                                (setq ncol (+ lj ncol))
                                                (setq tmp (assoc (format "%s%s,%s%s" var ncol var nrow) fpe))
                                                (when tmp
                                                  (setq tmp (cadr tmp))
                                                  (save-match-data
                                                    (with-temp-buffer
                                                      (insert mtch)
                                                      (goto-char (point-min))
                                                      (esn-number-est-change-it tmp)
                                                      (setq tmp (buffer-substring (point-min) (point-max)))))
                                                  (replace-match (format "%s" tmp))))
                                              (when (re-search-forward com nil t)
                                                (skip-chars-backward " \t")
                                                (when (looking-at "\\([ \t]*\\[\\([FAPfap]\\)\\]\\)?[ \t]*\\(?:\\(?:THETA\\|EPS\\|ETA\\)(?\\)?[0-9.]+\\(?:)\\)?\\([ \t]*\\)[=-]?")
                                                  (replace-match ""))))
                                            (re-search-forward com nil t))
                                        (setq nrow (+ 1 j))
                                        (when (and var fpe)
                                          (setq tmp (assoc (format "%s%s,%s%s" var nrow var nrow) fpe))
                                          (unless tmp
                                            (setq tmp (assoc (format "%s%s" var nrow) fpe)))
                                          (when tmp
                                            (setq tmp (cadr tmp))
                                            (save-match-data
                                              (with-temp-buffer
                                                (insert mtch)
                                                (goto-char (point-min))
                                                (esn-number-est-change-it tmp)
                                                (setq tmp (buffer-substring (point-min) (point-max)))))
                                            (replace-match (format "%s" tmp))))
                                        (setq lblk (+ 1 lblk))
                                        (if (re-search-forward com nil t)
                                            (progn
                                              (setq j (+ j 1))
                                              (esn-est-tmp-rep j what var-types var fpe same))
                                          (setq j (+ j 1)))))))
                              (setq lblk 0)
                              (while (re-search-forward est nil t)
                                (if (esn-in-comment-p)
                                    (end-of-line)
                                  (setq mtch (match-string 0))
                                  (setq lblk (+ 1 lblk))
                                  (setq nrow (+ j 1))
                                  (when (and var fpe)
                                    (setq tmp (assoc (format "%s%s,%s%s" var nrow var nrow) fpe))
                                    (unless tmp
                                      (setq tmp (assoc (format "%s%s" var nrow) fpe)))
                                    (when tmp
                                      (setq tmp (cadr tmp))
                                      (save-match-data
                                        (with-temp-buffer
                                          (insert mtch)
                                          (goto-char (point-min))
                                          (esn-number-est-change-it tmp)
                                          (setq tmp (buffer-substring (point-min) (point-max)))))
                                      (replace-match (format "%s" tmp))))
                                  (if (re-search-forward com nil t)
                                      (progn
                                        (progn
                                          (setq j (+ j 1))
                                          (esn-est-tmp-rep j what var-types var fpe same)))
                                    (setq j (+ j 1))))))
                            (goto-char (point-max))
                            (widen)
                            (when (and blk do-blk)
                              (esn-narrow-rec)
                              (goto-char (point-min))
                              (while (re-search-forward "BLOCK *(\\( *[0-9]+ *\\))" nil t)
                                (backward-char (+ 1 (length (match-string 1))))
                                (delete-char (length (match-string 1)))
                                (unless same
                                  (setq lblkn (- j lj)))
                                (insert (number-to-string lblkn)))
                              (goto-char (point-max))
                              (widen))))))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alignment for $TABLE, $BIND and $INPUT.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-align-tab-calc-space (lst num)
  "Calculate the spacing internal function.  LST contains the word lengths. NUM is how many words should be on a line."
  (let ((i 0)
        test
        (hash (make-hash-table :test 'equal))
        (ret 0) )
    (while (< i (length lst))
      (setq test (nth i lst))
      (setq i (+ i 1))
      (cond
       ((= 0 (mod i num)) ;; Restart
        (puthash 0 (max (+ 1 test) (gethash 0 hash 0)) hash))
       (t ;; Add
        (puthash (mod i num) (max (+ 2 test) (gethash (mod i num) hash 0)) hash))))
    (setq ret 0)
    (maphash (lambda(key val)
               (setq ret (+ ret val)))
             hash)
    (symbol-value 'ret)))

(defun esn-align-tab-internal ()
  "Alignment & Wrap function for $TABLE, $BIND and $INPUT"
  (save-excursion
    (save-restriction
      (re-search-backward (eval-when-compile (esn-reg-record-exp '("BIN" "INP" "TAB"))) nil t)
      (goto-char (match-end 0))
      (when (esn-narrow-rec)
        (let (lst rec num fill last-point i
                  (align-to-tab-stop nil))
          (goto-char (point-min))
          ;; Figure out the lengths of all the words in the record.
          (setq last-point (point))
          (esn-forward-w)
          (re-search-forward "\\=[ \t]*;.*" nil t)
          (while (not (= last-point (point)))
            (push (- (point) last-point) lst)
            (skip-chars-forward " \t\n")
            (setq last-point (point))
            (esn-forward-w)
            (re-search-forward "\\=[ \t]*;.*" nil t))
          (setq lst (reverse lst))
          ;; Calculate optimal spacing
          (setq rec (pop lst))
          (setq num 0)
          (setq fill 0)
          (while (and (<= num (length lst))
                      (>= fill-column (+ rec 1 fill)))
            (setq num (+ num 1))
            (setq fill (esn-align-tab-calc-space lst num)))
          (setq num (- num 1))
          ;; Remove all returns
          (goto-char (point-min))
          (while (re-search-forward "\n" nil t)
            (unless (save-match-data (looking-back ";.*\n"))
              (replace-match " ")))
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+" nil t)
            (replace-match " "))
          (goto-char (point-min))
          ;; Figure out the lengths of all the words in the record.
          (setq last-point (point))
          (esn-forward-w)
          (re-search-forward "\\=[ \t]*;.*" nil t)
          (setq i -1)
          (while (not (= last-point (point)))
            (setq i (+ i 1))
            (when (and (not (= i 0)) (= 0 (mod i num)))
              (unless (esn-in-comment-p)
                (insert "\n  ")))
            (skip-chars-forward " \t\n")
            (setq last-point (point))
            (esn-forward-w)
            (re-search-forward "\\=[ \t]*;.*" nil t))
          (align-regexp (point-min) (point-max) "\\(\\s-*\\) " 1 1 t)
          (goto-char (point-min))
          (esn-forward-w)
          (when (looking-at "  ")
            (replace-match " "))
          (goto-char (point-min))
          (while (re-search-forward "^ " nil t)
            (replace-match "")
            (end-of-line))
          ;; Take out trailing returns.
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (delete-region (point) (point-max)))))))

(defvar esn-align-tab-timer nil)

(defun esn-align-tab-hook ()
  "Hook for Table alignment function."
  (remove-hook 'esn-exit-record-hook 'esn-align-tab-hook 't)
  (when esn-align-tab-timer
    (cancel-timer esn-align-tab-timer))
  (when (eq major-mode 'esn-mode)
    (esn-align-tab-internal)))

(esn-rec-modification-hook "theta" 'esn-number-theta)
;;;###autoload
(defun esn-align-tab ()
  "Defines alignment function for PK -- Creates an idle timer."
  (interactive)
  (when esn-align-tab-timer 
    (cancel-timer esn-align-tab-timer))
  ;; Only set timer if at appropriate records
  (setq esn-align-tab-timer (run-with-idle-timer 1 nil 'esn-align-tab-hook))
  (add-hook 'esn-exit-record-hook 'esn-align-tab-hook nil 't))

(esn-rec-post-hook "input" 'esn-align-tab)
(esn-rec-post-hook "table" 'esn-align-tab)
(esn-rec-post-hook "bind" 'esn-align-tab)

(provide 'esn-align)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-align.el ends here
