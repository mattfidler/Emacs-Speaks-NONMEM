;;; esn-extended.el --- Extended Control Stream Support
;;
;; Filename: esn-extended.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Nov 13 16:03:16 2010 (-0600)
;; Version: 0.13
;; Last-Updated: Mon May  2 10:32:29 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 21
;; URL:
;; Keywords: Emacs NONMEM Extended Control Stream
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `esn-start'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 18-Mar-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Dec 13 12:31:58 2010 (-0600) #9 (Matthew L. Fidler)
;;
;;    Bug fix for esn-wfn-tmp-rep.  I'm not sure why, but it doesn't
;;    work in Emacs 23.3
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
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-rec "esn-properties")
(declare-function esn-xpose-get-input-line "esn-xpose")
(declare-function esn-get-current-record "esn-narrow")

(eval-when-compile
  (require 'esn-macros))

(require 'esn-start)
;;CaseFoldSearch
(defvar esn-wfn-dups nil
  "Variable for storing last duplicates.")


(defun esn-undo-numbering (&optional the ome sig var)
  "Changes THETA, ETA, and EPS to extended variable names."
  (interactive)
  (let ((tvar (or the (esn-get-variable-names "THE")))
        (avar (or ome (esn-get-variable-names "OME")))
        (svar (or sig (esn-get-variable-names "SIG")))
        (dups '())
        (there "")
        (case-fold-search 't))
    (mapc (lambda(x)
            (if (string-match (format "\\<%s\\>" x) there)
                (add-to-list 'dups (downcase x))
              (setq there (concat there " " x))))
          (append tvar avar svar))
    (esn-undo-numbering-tos tvar avar svar dups var)))

(defun esn-undo-numbering-tos (tvar avar svar dups var)
  "This is a function that changes all known THETA, ETA, etc to their extended control stream name. ERR -> SIGMA"
  (esn-undo-region-tos tvar "THETA" dups var)
  (esn-undo-region-tos avar "ETA" dups var)
  (esn-undo-region-tos svar "EPS" dups var)
  (esn-undo-region-tos svar "ERR" dups var)
  (when esn-wfn-extended-zero
    (esn-undo-region-zero avar dups var)))

(defun esn-undo-region-zero (lst dups v)
  "This undoes the numbering for the ZERO= in the estimation
step (used for hybrid)."
  (save-restriction
    (save-excursion
      (let (
            (case-fold-search 't) ;
            (reg (eval-when-compile (esn-reg-record-exp "\\(EST\\)" 't 't)))
            (zero "ZERO? *= *\\(([^)]*)\\|[0-9]+\\)")
            (rep "")
            (i 0)
            (var (or v ""))
            (is-dup nil)
            tmp
            )
        (goto-char (point-min))
        (while (re-search-forward reg nil t)
          (esn-narrow-rec)
          (goto-char (point-min))
          (when (re-search-forward zero nil t)
            (narrow-to-region (- (point) (length (match-string 1))) (point))
            (while (< i (length lst))
              (setq is-dup nil)
              (mapc (lambda(x)
                      (setq is-dup (or is-dup (string= (downcase (nth i lst)) x)))
                      ) dups)
              (unless is-dup
                (goto-char (point-min))
                (setq tmp (nth i lst))
                (unless (string-match "^[ \t\n]*$" tmp)
                  (while (re-search-forward
                          (format "\\<%s\\>" (+ i 1))
                          nil t)
                    (unless (string= (downcase tmp) (downcase var))
                      (replace-match tmp 't)
                      (setq tmp "")
                      )
                    )
                  )
                )
              (setq i (+ i 1))
              )
            (goto-char (point-max))
            (widen)
            )
          (goto-char (point-max))
          (widen)
          )
        )
      )
    ))
(defun esn-undo-mu-x (reg var num)
  ;; Make sure that the replacements are only in appropriate records AND not in
  ;; a comment.
  (when esn-wfn-mu-var
    (let (
          (nm-ver (esn-update-get-version))
          )
      (when (string= "-1" nm-ver)
        (setq nm-ver esn-assumed-version))
      (unless (>= (string-to-number nm-ver) 7)
        (save-restriction
          (widen)
          (save-match-data
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward reg nil t)
                (esn-narrow-rec)
                (goto-char (point-min))
                (while (re-search-forward (format "\\<MU_\\(%s\\)" num) nil t)
                  (unless (esn-in-comment-p)
                    (backward-char (length (match-string 1)))
                    (delete-char (length (match-string 1)))
                    (if (looking-at (format "[ \t]*\\+[ \t]*\\(%s\\|ETA(%s)\\)" var num))
                        (progn
                          (backward-char 1)
                          (delete-char 1)
                          )
                      (if (not
                           (re-search-backward
                            (format "\\(%s\\|ETA(%s)\\)[ \t]*\\+[ \t]*MU_\\=" var num)
                            nil
                            t))
                          (insert var)
                        (forward-char (- (length (match-string 0)) 1))
                        (delete-char 1)
                        )
                      )
                    )
                  )
                )
              (goto-char (point-max))
              (widen)
              )
            )
          )
        )
      )
    )
  )
(defun esn-undo-region-tos (lst what dups v)
  (save-restriction
    (save-excursion
      (let (
            (case-fold-search 't)
            (reg esn-current-abbrev-records-regexp)
            (used '())
            (name "")
            (is-dup nil)
            (var (or v ""))
            (num nil)
            tmp
            )
        (goto-char (point-min))
                                        ;    (when (string= what "ETA")
                                        ;      (esn-undo-mu-x reg tmp num)
                                        ;      )
        (while (re-search-forward reg nil t)
          (esn-narrow-rec)
          (goto-char (point-min))
          (while (re-search-forward (format "\\<%s(\\([0-9]+\\))" what)
                                    nil t)
            (setq num (string-to-number (match-string 1)))
            (setq name (- num 1))
            (when (and lst (< name (length lst)))
              (setq tmp (nth name lst))
              (unless (string= "" tmp)
                (unless (string= (downcase tmp) (downcase what))
                  (setq is-dup nil)
                  (mapc (lambda(x)
                          (setq is-dup (or is-dup (string= (downcase tmp) x)))
                          ) dups)
                  (unless is-dup
                    (unless (or (esn-in-comment-p)
                                (save-excursion
                                  (backward-char (length (match-string 0)))
                                  (esn-in-comment-p)
                                  )
                                )
                      (unless (string= (downcase tmp) (downcase var))
                        (progn
                          (replace-match tmp 't)
                          )
                                        ;                   (hlt-highlight-region (- (point) (length tmp)) (point)
                                        ;                                         'font-lock-keyword-face)
                        )
                      )
                    )
                  )
                )
              )
            )
          (goto-char (point-max))
          (widen)
          )
        )
      )
    ))

(defun esn-fix-numbering (&optional the ome sig hi var)
  "Change symbols to appropriate values.

Only change var if var is non-nil
"
  (interactive)
  (let (
        (tvar (or the (esn-get-variable-names "THE")))
        (avar (or ome (esn-get-variable-names "OME")))
        (svar (or sig (esn-get-variable-names "SIG")))
        (dups '())
        (there "")
        (ob (current-buffer))
        (case-fold-search 't)
        (ndup "")
        )
    (mapc (lambda(x)
            (if (string-match (format "\\<%s\\>" x) there)
                (add-to-list 'dups (downcase x))
              (setq there (concat there " " x))
              )
            )
          (append tvar avar svar))
    ;; Get Ndups
    (mapc (lambda(x)
            (let (
                  (is-new 't)
                  )
              (mapc (lambda(y)
                      (when (string= (downcase y) (downcase x))
                        (setq is-new nil)
                        )
                      )
                    esn-wfn-dups)
              (when is-new
                (setq ndup x)))
            )
          dups)
    (setq esn-wfn-dups dups)
    (esn-fix-numbering-tos tvar avar svar dups hi var ndup)
    )
  )
(defun esn-fix-numbering-tos (tvar avar svar dups &optional hi var ndup)
  "This is a function that changes known symbols to their appropriate values."
  (esn-fix-numbering-region-tos tvar "THETA" dups hi var ndup)
  (esn-fix-numbering-region-tos avar "ETA" dups hi var ndup)
  (esn-fix-numbering-region-tos svar "EPS" dups hi var ndup)
  (when esn-wfn-extended-zero
    (esn-fix-numbering-zero avar dups hi var ndup)
    )
  )
(defun esn-fix-numbering-mu-x (reg var num)
  (interactive)
  (when esn-wfn-mu-var
    (let (
          (nm-ver (esn-update-get-version))
          )
      (when (string= "-1" nm-ver)
        (setq nm-ver esn-assumed-version))
      (unless (>= (string-to-number nm-ver) 7)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward reg nil t)
            (esn-narrow-rec)
            (goto-char (point-min))
            (while (re-search-forward
                    (format "\\<mu_\\(%s\\)\\>" var)
                    nil 't)
              (unless (esn-in-comment-p)
                (backward-char (length (match-string 1)))
                (delete-char (length (match-string 1)))
                (insert (number-to-string num))
                )
              )
            (goto-char (point-min))
            (while (re-search-forward
                    (format "\\<mu\\([ \t]*\\+[ \t]*\\(?:%s\\|ETA(%s)\\)\\)" var num)
                    nil t)
              (unless (esn-in-comment-p)
                (backward-char (length (match-string 1)))
                (insert (format "_%s" num))
                (forward-char (length (match-string 1)))
                )
              )
            (goto-char (point-min))
            (while (re-search-forward
                    (format "\\(?:%s\\|ETA(%s)\\)[ \t]*\\+[ \t]*MU\\>" var num)
                    nil
                    t)
              (unless (esn-in-comment-p)
                (insert (format "_%s" num))
                )
              )
            (widen)
            )
          )
        )
      )
    ))
(defun esn-fix-numbering-zero (lst dups &optional hi var ndup)
  "This fixes the numbering for the ZERO= in the estimatino step (used for hybrid)."
  (save-excursion
    (let (
          (case-fold-search 't)         ;
          (reg "\n\\$\\(EST\\)[A-Z]*[ \n\t]")
          (zero "ZERO? *= *\\(([^)]*)\\|[0-9]+\\)")
          (rep "")
          (i  0)
          (is-dup nil)
          tmp
          )
      (goto-char (point-min))
      (while (re-search-forward reg nil t)
        (setq i 0)
        (esn-narrow-rec)
        (goto-char (point-min))
        (when (re-search-forward zero nil t)
          (narrow-to-region (- (point) (length (match-string 1))) (point))
          (while (< i (length lst))
            (setq is-dup nil)
            (mapc (lambda(x)
                    (setq is-dup (or is-dup (string= (downcase (nth i lst)) x)))
                    ) dups)
            (unless is-dup
              (goto-char (point-min))
              (setq tmp (number-to-string (+ i 1)))
              (unless (string-match "^[ \t\n]$" tmp)
                (while (re-search-forward
                        (format "\\<%s\\>" (regexp-quote (nth i lst)))
                        nil t)
                  (when (not hi)
                    ;; Replace when not highlighting.
                    (if (not var)
                        (replace-match tmp)
                      ;; Var is defined only replace that variable.
                      ;; Ignore all others.
                      (when (string= (downcase (match-string 0)) (downcase var))
                        (replace-match tmp)
                        )
                      )
                    )
                                        ;                 (hlt-highlight-region (- (point) (length (match-string 0))) (point)  'font-lock-keyword-face)
                  (setq tmp "")
                  )
                )
              )
            (setq i (+ i 1))
            )
          (goto-char (point-max))
          (widen)
          )
        (goto-char (point-max))
        (widen)
        )
      )
    )
  )
(defun esn-wfn-tmp-rep (
                        ;; unquoted
                        lst
                        hi
                        var
                        reg
                        ;; quoted
                        pmin-q
                        pmax-q
                        prev-q
                        md-q
                        used-q
                        )
  "Temporary function to replace appropriate items."
  (goto-char (point-min))
  (while (re-search-forward (format "\\<%s\\>" (regexp-quote (nth i lst))) nil t)
    (save-excursion
      (re-search-backward (format "\\<%s\\="
                                  (regexp-quote (nth i lst))) nil t)
      (if (re-search-forward "\\=.*?=" nil t)
          (progn
            (if (and (not (assoc i (symbol-value used-q)) )
                     (re-search-forward (format "\\=.*?\\<%s\\>" (regexp-quote (nth i lst))) nil t))
                (progn
                  (save-excursion
                    (set pmin-q (point-min))
                    (set pmax-q (point-max))
                    (widen)
                    (beginning-of-line)
                    (set prev-q (re-search-backward (format "^\\(?:[ \t]*\\<\\$[A-Za-z][A-Za-z0-9]*\\)?[ \t]*%s[ \t]*="
                                                            (regexp-quote (nth i lst)))
                                                    nil t)))
                  (narrow-to-region (symbol-value pmin-q) (symbol-value pmax-q))
                  (unless (symbol-value prev-q)
                    (unless (string= (downcase (nth i lst)) (downcase what))
                      (when (not hi)
                        ;; Not highlighting, replace
                        (if (not var)
                            (progn
			      (re-search-backward (format "\\<%s\\=" (regexp-quote (nth i lst))) nil t)
                              (message "spot 1")
                              (replace-match (format "%s(%s)" what (+ i 1)))
                              (when (string= (downcase what) "eta")
                                (esn-fix-numbering-mu-x reg
                                                        (nth i lst)
                                                        (+ i 1))
                                )
                              )
                          ;; Var defined, only replace it.
                          (when (string= (downcase (match-string 0)) (downcase var))
                            (message "spot 2")
                            (re-search-backward (format "\\<%s\\=" (regexp-quote (nth i lst))) nil t)
                            (replace-match (format "%s(%s)" what (+ i 1)))
                            (when (string= (downcase what) "eta")
                              (esn-fix-numbering-mu-x reg
                                                      (nth i lst)
                                                      (+ i 1))))))))))
            (esn-add-to-alist used-q (cons i 't)))
        (save-excursion
          (set pmin-q (point-min))
          (set pmax-q (point-max))
          (widen)
          (beginning-of-line)
          (set prev-q (re-search-backward (format "^\\(?:[ \t]*\\<\\$[A-Za-z][A-Za-z0-9]*\\)?[ \t]*%s[ \t]*="
                                                  (regexp-quote (nth i lst))
                                                  )
                                          nil 't
                                          ))
          )
        (narrow-to-region (symbol-value pmin-q) (symbol-value pmax-q))
        (unless (symbol-value prev-q)
          (unless (string= (downcase (nth i lst)) (downcase what))
            (if (re-search-forward (format "\\=\\<%s\\>" (regexp-quote (nth i lst))) nil t)
                (progn
                  
                  (unless (esn-in-comment-p (length (match-string 0)))
                    (looking-at ".")
                    (when (looking-at "\\([ \t\n.+-\\*)\\/]\\|$\\)")
                      (if (not var)
                          (progn
			    (delete-char (- (length (nth i lst))))
			    (insert (format "%s(%s)" what (+ i 1)))
                            (when (string= (downcase what) "eta")
                              (esn-fix-numbering-mu-x reg
                                                      (nth i lst)
                                                      (+ i 1))))
                        ;; Var is defined
                        (when (string= (downcase (match-string 0)) (downcase var))
                          (message "spot 4")
                          (replace-match (format "%s(%s)" what (+ i 1)))
                          (when (string= (downcase what) "eta")
                            (esn-fix-numbering-mu-x reg
                                                    (nth i lst)
                                                    (+ i 1)))))))))))))))
(defun esn-fix-numbering-region-tos (lst what dups &optional hi var ndup)
  "This fixes the numbering for extended control streams for a specific variable."
  (save-restriction
    (save-excursion
      (let (
            (reg esn-current-abbrev-records-regexp)
            (i 0)
            (used '())
            (case-fold-search 't)
            (is-dup nil)
            (pmin nil)
            (pmax nil)
            (prev nil)
            (esn-run-save-fn 't)
            (md nil)
            (p1 nil)
            (p2 nil)
            (esn-rep nil)
            (ndupn -1)
            (found-decomp nil)
            (all-dups '())
            (found-dups '())
            )
	
        (goto-char (point-min))
        (while (re-search-forward reg nil t)
          (esn-narrow-rec)
          (setq i 0)
          (while (< i (length lst))
            (setq is-dup nil)
            (mapc (lambda(x)
                    (setq is-dup (or is-dup (string= (downcase (nth i lst)) x)))
                    ) dups)
            (unless is-dup
              (esn-wfn-tmp-rep lst hi var reg 'pmin 'pmax 'prev 'md 'used)
              )
            (when (and (not (string= (or ndup "") ""))
                       (string= (downcase (or ndup "")) (downcase (nth i lst)))
                       )
              ;; Now look to see if the THETA/ETA/EPS is present in the file.
              ;; If the variable is NOT present, then assign it to be the
              ;; duplicate that needs to be replaced.
              (save-excursion
                (goto-char (point-min))
                (setq found-decomp (re-search-forward
                                    (format "\\<%s(%s)"
                                            what (+ i 1)
                                            )
                                    nil t))
                )
              (add-to-list 'all-dups i)
              (when found-decomp
                (add-to-list 'found-dups i))
              )
            (setq i (+ i 1))
            )
          (goto-char (point-max))
          (widen)
          (mapc (lambda(x)
                  (let (
                        (is-new 't)
                        )
                    (mapc (lambda(y)
                            (when (= y x)
                              (setq is-new nil)
                              )
                            )
                          found-dups)
                    (when is-new
                      (setq ndupn x))
                    ))
                all-dups))
        (when (and (not (string= (or ndup "") ""))
                   (not (= ndupn -1))
                   )
          (setq var nil)
          (setq hi nil)
          (setq i ndupn)
          (goto-char (point-min))
          (while (re-search-forward reg nil t)
            (esn-narrow-rec)
            (goto-char (point-min))
            (esn-wfn-tmp-rep lst hi var reg 'pmin 'pmax 'prev 'md 'used)
            (goto-char (point-max))
            (widen)))))))


(defvar esn-var-names '()
  "Defines a list of saved variable names.  Used to speed up before and after commands.")

;;;###autoload
(defun esn-get-variable-names (&optional what cm cmsame)
  "Gets the theta names from the appropriate block, and returns
them as a list.  Only get variable names from the first $PROBLEM
statement"
  (save-restriction
    (if (assoc what esn-var-names)
        (progn
          (nth 1 (assoc what esn-var-names)))
      (let* ((theta (esn-rec (or what "THE") 't 't))
             (ret '())
             (vars nil)
             (lblk 0)
             (blk nil)
             (same nil)
             (est "\\(?:([^();\n]*?\\(?:[();]\\|$\\)\\|[0-9.]+\\)")
             (com (or cm "\\= *;.*?\\([A-Z][A-Za-z0-9_.]*\\).*" ))
             (comsame (or cmsame (if esn-wfn-var-label-skip-tos ";.*?\\([A-Z][A-Za-z0-9_.]*\\).*")))
             (i 0)
             (nrow 0)
             (ncol 0)
             (case-fold-search 't)
             (cur "")
             (inp-reg-lst (esn-xpose-get-input-line) )
             (esn-inp "")
             (semi nil))
        (setq esn-inp (upcase (concat "\\(" (mapconcat (lambda (x) x) inp-reg-lst "\\|") "\\)")))
        (with-temp-buffer
          (insert theta)
          (when esn-wfn-var-label-skip-tos
            (goto-char (point-min))
            (while (re-search-forward ";[C;]*" nil t)
              (replace-match ";"))
            (goto-char (point-min))
            (while (re-search-forward
                    (eval-when-compile
                      (format ".*\\<%s"
                              (regexp-opt '("THETA" "ETA" "OMEGA" "EPS" "ERR" "SIGMA") 't)))
                    nil t)
              (replace-match "")))
          (when esn-wfn-var-label-skip-afp
            (goto-char (point-min))
            (while (re-search-forward
                    (eval-when-compile
                      (format "%s"
                              (regexp-opt '("[A]" "[a]" "[F]" "[f]" "[P]" "[p]") 't))) nil t)
              (replace-match "")))
          (goto-char (point-min))
          (while (re-search-forward "\\<FIX\\(?:ED?\\)?\\>" nil t)
            (replace-match ""))
          (goto-char (point-min))
          (while (not (eobp))
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq blk (re-search-forward "\\<BLOCK\\>" nil t))
            (goto-char (point-min))
            (setq same (re-search-forward "\\<SAME\\>" nil t))
            (goto-char (point-min))
            (while (re-search-forward "\\<\\(BLOCK *\\(?:( *[0-9]+ *)\\)\\|SAME\\)" nil t)
              (replace-match ""))
            (goto-char (point-min))
            (if blk
                (if same
                    (progn
                      (setq i 0)
                      (goto-char (point-min))
                      (while (and (< i lblk) (re-search-forward comsame nil t))
                        (setq cur (match-string 1))
                        (when (string-match (format "^\\(%s\\|%s\\)$"
                                                    esn-wfn-excluded-variables
                                                    esn-inp)
                                            (upcase cur))
                          (setq cur ""))
                        (setq ret (push cur ret))
                        (setq i (+ i 1)))
                      (while (< i lblk)
                        (setq ret (push "" ret))
                        (setq i (+ i 1))))
                  (setq i 0)
                  (setq lblk 0)
                  (while (re-search-forward est nil t)
                    (if (not
                         (and
                          (string= "(" (substring (match-string 0) 0 1))
                          (let* (
                                 (ch (substring (match-string 0) -1))
                                 (ret (or
                                       (string= ch ";")
                                       (string= ch "(")
                                       (string= ch ")"))))
                            (symbol-value 'ret))))
                        (replace-match "")
                      (replace-match (substring (match-string 0) -1))
                      (re-search-backward ";\\=" nil t))
                    (if (esn-in-comment-p)
                        (end-of-line)
                      (replace-match "")
                      (setq i (+ 1 i))
                      (setq nrow (/ (+ -1 (sqrt (+ 1 (* 8 i)))) 2))
                      (if (not (= (floor nrow) (ceiling nrow)))
                          (re-search-forward com nil t)
                        (setq lblk (+ 1 lblk))
                        (if (re-search-forward com nil t)
                            (progn
                              (setq cur (match-string 1))
                              (when (string-match  (format "^\\(%s\\|%s\\)$"
                                                           esn-wfn-excluded-variables
                                                           esn-inp
                                                           )
                                                   (upcase cur))
                                (setq cur ""))
                              (setq ret (push cur ret)))
                          (setq ret (push "" ret)))))))
              (setq lblk 0)
              (while (re-search-forward est nil t)
                (if (not
                     (and
                      (string= "(" (substring (match-string 0) 0 1))
                      (let* ((ch (substring (match-string 0) -1))
                             (ret (or
                                   (string= ch ";")
                                   (string= ch "(")
                                   (string= ch ")"))))
                        (symbol-value 'ret))))
                    (replace-match "")
                  (replace-match (substring (match-string 0) -1))
                  (re-search-backward ";\\=" nil t))
                (if (esn-in-comment-p)
                    (end-of-line)
                  (setq lblk (+ 1 lblk))
                  (if (re-search-forward com nil t)
                      (progn
                        (progn
                          (setq cur (match-string 1))
                          (when (string-match (format "^\\(%s\\|%s\\)$"
                                                      esn-wfn-excluded-variables
                                                      esn-inp)
                                              (upcase cur))
                            (setq cur ""))
                          (setq ret (push cur ret))))
                    (setq ret (push "" ret))))))
            (goto-char (point-max))
            (widen)
            (if (re-search-forward "\\$[A-Z]*[ \n\t]" nil t) nil
              (goto-char (point-max)))))
        (setq ret (reverse ret))
        (add-to-list `esn-var-names (list what ret))
        (symbol-value 'ret)))))

(defun esn-fix-variable-label (arg &optional killp undo pt)
  "Before inserting deleting, etc. checks to see if the labels
  are affected."
  (interactive "*p\nP")
  (when (and esn-update-wfn-labels-when-editing
             esn-wfn-extended)
    (let (
          (deactivate-mark nil)
          (case-fold-search 't)
          pt1 pt2
          )
      (save-excursion
        (when pt
          (goto-char pt)
          )
                                        ;    (message "%s,%s" this-command undo)
        (let (
              (inserting (memq this-command '(self-insert-command
                                              esn-upcase-char-self-insert
                                              esn-magic-quote
                                              esn-magic-$
                                              esn-magic-semi
                                              esn-magic-space
                                              )
                               ))
              (deleting (memq this-command '(esn-magic-bs-del
                                             delete-char
                                             backward-delete-char-untabify
                                             delete-backward-char
                                             )))
              (in-comment nil)
              (rec nil)
              (in-var nil)
              (p1 nil)
              (p2 nil)
              (this-command this-command)
              )
          (if (not (or inserting deleting))
              (if undo
                  (esn-undo-numbering)
                ;; Now check for kill-word and the like.
                (cond
                 ;; Viper modes
                 ((and (boundp 'viper-mode) viper-mode)
                  (message "Viper")
                  ;; For viper mode, just undo the numbering whenever we
                  ;; are in a comment.
                  (setq in-comment (save-excursion
                                     (re-search-backward "\\;.*\\=" nil t)
                                     ))
                  (when in-comment
                    (setq rec (esn-get-current-record))
                    (when (or (string= rec "THE")
                              (string= rec "OME")
                              (string= rec "SIG")
                              )
                      (save-excursion
                        (setq pt1 (point))
                        (when (re-search-backward ";")
                          (re-search-forward "\\=;[;C]*" nil t)
                          (re-search-forward "\\=[ \t]*\\[[PFApfa]\\]" nil t)
                          (re-search-forward (eval-when-compile
                                               (format "\\=[ \t]*%s"
                                                       (regexp-opt '(
                                                                     "THETA"
                                                                     "ETA"
                                                                     "OMEGA"
                                                                     "EPS"
                                                                     "ERR"
                                                                     "SIGMA"
                                                                     )
                                                                   't)))
                                             nil t)
                          (if (not (and
                                    (re-search-forward
                                     "\\=.*?\\([A-Z][A-Za-z0-9_.]*\\)" nil t)))
                              (progn
                                (setq in-var nil)
                                (esn-undo-numbering))
                            ;; Update point.
                            (setq this-command 'esn-upcase-char-self-insert)
                            (setq p1 (save-excursion
                                       (goto-char (point))
                                       (backward-char (length (match-string 1)))
                                       (point)))
                            (esn-undo-numbering))))))))
                ;; Yank
                ((memq this-command '(yank-pop
                                      yank
                                      cua-paste
                                      transpose-chars
                                      transpose-words
                                      ))
                 (setq this-command 'esn-upcase-char-self-insert)
                 (setq p1 (point)))
                ;; Kill
                ((memq this-command '(kill-word))
                 (setq this-command 'esn-magic-bs-del)
                 (setq p1 (point))
                 (setq p2 (progn (forward-word arg) (point))))
                ((memq this-command '(backward-kill-word))
                 (setq this-command 'esn-magic-bs-del)
                 (setq p1 (point))
                 (setq p2 (progn (forward-word (- arg)) (point))))
                ;; Delete whole line doesn't change variable labels, just (possibly) rearranges them.
                ((memq this-command '(kill-line
                                      backward-delete-char-untabify
                                      ))
                 (setq this-command 'esn-magic-bs-del)
                 (setq p1 (point))
                 (setq pt (save-excursion (end-of-line)
                                          (re-search-backward ";.*\\=" nil t)
                                          (re-search-forward "\\=;[;C]*" nil t)
                                          (re-search-forward "\\=[ \t]*\\[[PFApfa]\\]" nil t)
                                          (re-search-forward (eval-when-compile
                                                               (format "\\=[ \t]*%s"
                                                                       (regexp-opt '(
                                                                                     "THETA"
                                                                                     "ETA"
                                                                                     "OMEGA"
                                                                                     "EPS"
                                                                                     "ERR"
                                                                                     "SIGMA"
                                                                                     )
                                                                                   't))) nil t)
                                          (re-search-forward "\\=.*?[A-Z]" nil t)
                                          (point)))
                 
                 ((memq this-command '(kill-region
                                       cua-cut-region
                                       ))
                  (setq this-command 'esn-magic-bs-del)
                  (setq p1 (point))
                  (setq p2 (mark 't))
                  (unless p2
                    (setq p1 nil)))
                 )
                (when (or p1 p2)
                  ;; (message "%s,%s" p1 p2)
                  )
                (when p1
                  (esn-fix-variable-label nil nil nil p1))
                (when p2
                  (esn-fix-variable-label nil nil nil p2))
                (unless (or p1 p2)
                  (esn-undo-numbering)))
            ;; Inserting...
            (setq in-comment (save-excursion
                               (re-search-backward "\\;.*\\=" nil t)))
            (when in-comment
              (setq rec (esn-get-current-record))
              (when (or (string= rec "THE")
                        (string= rec "OME")
                        (string= rec "SIG"))
                (save-excursion
                  (setq pt1 (point))
                  (when (re-search-backward ";")
                    (re-search-forward "\\=;[;C]*" nil t)
                    (re-search-forward "\\=[ \t]*\\[[PFApfa]\\]" nil t)
                    (re-search-forward
                     (eval-when-compile
                       (format "\\=[ \t]*%s"
                               (regexp-opt '("THETA" "ETA" "OMEGA" "EPS" "ERR" "SIGMA")
                                           't)))
                     nil t)
                    (if (not (re-search-forward
                              "\\=.*?\\([A-Z][A-Za-z0-9_.]*\\)" nil t))
                        (setq in-var nil)
                      (when (and (<= pt1 (point))
                                        ;                            (>= pt1 (- (point) (length (match-string 1)))
                                 )
                        (setq in-var (match-string 1))))))))
            (if  in-var
                (progn
                  (unless killp
                    (message "Changing Label, move away from label to reapply (%s)"
                             in-var))
                  (esn-fix-numbering nil nil nil nil in-var))
              (esn-undo-numbering))))))))

(esn-tos-post-hook (lambda()
                     (condition-case error
                         (progn
                           (esn-fix-variable-label nil 't))
                       (error
                        (message "Fix variable label Post-command error: %s" (error-message-string error))))))

(provide 'esn-extended)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-extended.el ends here
