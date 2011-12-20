;;; esn-yas.el --- YaSnippet support for Emacs Speaks NONMEM
;;
;; Filename: esn-yas.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Fri Sep 17 10:59:18 2010 (-0500)
;; Version:
;; Last-Updated: Mon May  2 15:04:52 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1068
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Builds Yasnippets based on EsN options.  Also provides functions
;;  for yasnippet expansion.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 15:27:09 2010 (-0500) #1055 (Matthew L. Fidler)
;;    There are some strange bugs on the pending snippets but I can't reproduce them.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 14:37:27 2010 (-0500) #1045 (Matthew L. Fidler)
;;    adding back `esn-tv-par'
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 13:57:26 2010 (-0500) #1040 (Matthew L. Fidler)
;;    Finished functions for tv expansion.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 12:56:57 2010 (-0500) #1009 (Matthew L. Fidler)
;;    Added new snippets
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 14:18:40 2010 (-0500) #957 (Matthew L. Fidler)
;;
;;    Bug fix for speed when using yasnippets.  For some reason the SVN trunk
;;    calls the exit hook when it shouldn't
;;
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 10:45:26 2010 (-0500) #952 (Matthew L. Fidler)
;;    Added post-command-hook wrapping.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 10:43:16 2010 (-0500) #948 (Matthew L. Fidler)
;;    Esn Yas start problems causing problems in post-command-hook
;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 14:40:18 2010 (-0500) #936 (Matthew L. Fidler)
;;    Yasnippet bug fix
;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 14:37:59 2010 (-0500) #932 (Matthew L. Fidler)
;;    Yas bug-fix.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 14:15:03 2010 (-0500) #867 (Matthew L. Fidler)
;;    Changed code to work with SVN trunk of Yasnippet.  Works better...
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 11:39:23 2010 (-0500) #45 (Matthew L. Fidler)
;;    When starting Yas, remove all alignment timers.
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
(declare-function esn-get-time-units "esn-units")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-alert "esn-exec")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-align-equals-fun-hook "esn-align")
(declare-function esn-number-theta-hook "esn-align")
(declare-function esn-align-matrix-hook "esn-align")
(declare-function esn-number-eta-hook "esn-align")

(require 'esn-options)

(add-hook 'yas/after-exit-snippet-hook 'esn-yas-after)
(add-hook 'yas/before-expand-snippet-hook 'esn-yas-before)

;; Take off pre and post-command hooks while Yasnippet completion is in effect

(defun esn-yas-theta (&optional theta theta-label unit lower upper)
  "* Function that determines the current THETA number and returns THETA(x)"
  (add-to-list 'esn-yas-pending-thetas (list (esn-yas-current-theta theta) "THETA()"
                                             (or unit "")
                                             lower
                                             upper
                                             (or theta-label "")))
  (format "THETA(%s)" (esn-yas-current-theta theta))
  )
(defun esn-yas-current-theta (&optional theta)
  "* Function that determines the current theta number based on esn-yas-last-theta-value"
  (let (
        (ret (or esn-yas-last-theta 1))
        (val (or theta esn-yas-last-theta-value))
        (s 0)
        (i 1)
        )
    (while (< i val)
      (if (member i esn-yas-added-2-thetas)
          (setq s (+ 2 s))
        (setq s (+ 1 s))
        )
      (setq i (+ i 1))
      )
    (setq ret (+ (or esn-yas-last-theta 1) s))
    (symbol-value 'ret)
    )
  )

(defun esn-yas-format (text &rest args)
  "* Yas based format.  Takes the following extra arguments:
 %NEXT-THETA% = Next theta #
 %CURRENT-THETA% = Current Theta #
 %TVCURRENT% = Current Tv

When %NEXT-THETA% is found do the following, set esn-yas-added-2-thetas to true

"
  (if (not text)
      ""
    (let (
          (ret text)
          (theta (esn-yas-current-theta))
          )
      (while (string-match "%TVCURRENT%" ret)
        (setq ret (replace-match esn-yas-last-tv 't 't ret))
        )
      (while (string-match "%CURRENT-THETA%" ret)
        (setq ret (replace-match (number-to-string theta) 't 't ret))
        )
      (while (string-match "%NEXT-THETA%" ret)
        (setq ret (replace-match (number-to-string (+ 1 theta)) 't 't ret))
        (add-to-list 'esn-yas-added-2-thetas esn-yas-last-theta-value)
        )
      (setq ret (apply 'esn-format ret args))
      (symbol-value 'ret))))
;;;###autoload
(defun esn-yas-before ()
  "* Hook for Before YAS completion"
  (condition-case error
      (progn
        (when (eq major-mode 'esn-mode)
          (setq esn-yas-start (point-at-bol))
          (when esn-add-update-topics-timer
            (cancel-timer esn-add-update-topics-timer)
            )
                                        ;    (esn-kill-buffer-hook); Kill all timers NOW.
          (setq esn-run-save-fn 't)
          (when esn-align-equals-fun-timer 
            (cancel-timer esn-align-equals-fun-timer))
          (when esn-align-matrix-timer
            (cancel-timer esn-align-matrix-timer))
          (when esn-number-theta-timer
            (cancel-timer esn-number-theta-timer))
          (when esn-number-eta-timer
            (cancel-timer esn-number-eta-timer))
          (when esn-number-eps-timer
            (cancel-timer esn-number-eps-timer))
          (setq esn-yas-last-theta (esn-yas-next-theta))
          (setq esn-yas-pending-thetas nil)
          (setq esn-yas-pending-omegas nil)
          (setq esn-yas-added-2-thetas nil)
          (setq esn-yas-no-mu nil)
          (when (esn-in-comment-p)
            (goto-char (point-at-eol))
            (insert "\n"))))
    (error
     (message "esn before YAS hook error: %s" (error-message-string error)))))

(defun esn-yas-no-snippets-expanding ()
  (if (fboundp 'yas/snippets-at-point)
      (= 0 (length (yas/snippets-at-point 'all-snippets)))
    't))
;;;###autoload
(defun esn-yas-after ()
  "* Hook for After YAS completion"
  (condition-case error
      (progn
        (setq esn-run-save-fn nil)
        (setq esn-get-current-record-start nil)
        (setq esn-yas-last-theta nil))
    (error
     (message "Esn Yas after-command-hook error: %s" (error-message-string error)))))

(defmacro esn-yas-mu (theta-number)
  "* Macro for initial MU_# for theta number"
  (list 'esn-yas-mu-trf 'text '1 theta-number)
  )
(defvar esn-yas-no-mu nil
  "* Defines if there is no MU-referencing mirrors in this template...."
  )
(make-variable-buffer-local 'esn-yas-no-mu)
(defun  esn-yas-par-tv (theta-number tv-number)
  "* Esn Yas Par tv expansion."
  (prog1 (esn-yas-par theta-number (concat "TV" (yas/field-value tv-number)) "")
    (setq esn-yas-no-mu (esn-yas-mu tv-number))
    )
  )
(defun esn-yas-par (theta-number tv units &rest rest)
  "* Parameter value abbreviation"
  (let (
        (ret (apply 'esn-yas-eta-types tv nil units theta-number rest))
        )
    (setq ret (esn-yas-choose ret 'esn-yas-eta-types-transform 'esn-add-omega-theta))
    (setq ret (esn-yas-mu-trf ret nil tv))
    (when esn-yas-no-mu
      (setq esn-yas-no-mu (concat esn-yas-no-mu (esn-yas-mu-trf ret 1 theta-number)))
      )
    (symbol-value 'ret)
    )
  )
(defmacro esn-yas-scale ()
  "* Macro for scale"
  (list 'unless 'yas/modified-p (list 'esn-yas-fix-pending-units (list 'esn-scale ''t)))
  )
(defun esn-yas-choose (values &optional transform-function function &rest ignore)
  "* EsN Yas chooser; Optionally apply function "
  (let (
        (ret (yas/choose-value (reverse values)))
        )
    (when transform-function
      (save-excursion
        (save-match-data
          (save-restriction
            (when ret
              (setq ret (funcall transform-function ret))
              )
            )
          )
        )
      )
    (when function
      (save-excursion
        (save-match-data
          (save-restriction
            (when ret
              (funcall function ret)
              )
            )
          )
        )
      )
    (unless ret
      (setq ret yas/text)
      (unless ret
        (setq ret "")
        )
      )
    (symbol-value 'ret)
    )
  )

(defun esn-yas-units (scale-text unit-type)
  "* Changes unit-type to the correct unit based on the scale-factor text."
  (let (
        (ret unit-type)
        (volume "")
        (time "hr")
        )
    (when (string-match "Volume:[ \t]*\\([A-Z]*[Ll]\\)[ \t]*\\(;\\|$\\)" scale-text)
      (setq volume (match-string 1 scale-text))
      (when (string-match "\\<Volume\\>" ret)
        (setq ret (replace-match volume 't 't ret))
        )
      (setq time (esn-get-time-units 't))
      (unless time
        (setq time "hr"))
      (when (string-match "\\<Time\\>" ret)
        (setq ret (replace-match time 't 't ret)))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-yas-number-pre-snippet (text)
  "* Changes all $ to numbered $"
  (let (
        (i 0)
        (ret "")
        )
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward "$" nil 't)
        (setq i (+ i 1))
        (insert (number-to-string i))
        )
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-yas-fix-pending-units (text)
  "* Fixes units on pending variables text=scale factor text"
  (setq esn-yas-pending-thetas (reverse
                                (mapcar (lambda(x)
                                          (append (list (car x) (cadr x) (esn-yas-units text (caddr x)))
                                                  (cdddr x))
                                          )
                                        esn-yas-pending-thetas
                                        )))
  (setq esn-yas-pending-omegas
        (reverse
         (mapcar (lambda(x)
                   (append (list (car x) (cadr x) (esn-yas-units text (caddr x)))
                           (cdddr x))
                   )
                 esn-yas-pending-omegas
                 ))
        )
  (let (
        (theta-snippet "")
        (omega-snippet "")
        )
    (setq theta-snippet
          (esn-yas-number-pre-snippet
           (mapconcat
            (lambda(x)
              (let (
                    (ret "      (")
                    (tv "")
                    (no-comment nil)
                    (fmt (nth 5 x))
                    tmp
                    v
                    )
                (cond
                 ( (nth 3 x)
                   (setq ret (concat ret (nth 3 x) ",$")))
                 ( (nth 4 x)
                   (setq ret (concat ret ",$"))
                   )
                 ( 't
                   (setq ret (concat ret "$"))
                   )
                 )
                (cond
                 ( (nth 4 x)
                   (setq ret (concat ret "," (nth 4 x) ") ; "))
                   )
                 ( 't
                   (setq ret (concat ret ") ; "))
                   )
                 )
                (when (esn-use-pdx-p)
                  (setq ret (concat ret "["))
                  )
                (when (string-match "\\<TV\\([A-Z][A-Z0-9_]*\\)\\>" (nth 1 x))
                  (setq tv (match-string 1 (nth 1 x)))
                  )
                (setq ret (concat ret (esn-yas-format fmt tv)))
                (if (string= "" (nth 2 x))
                    (when (esn-use-pdx-p)
                      (setq ret (concat ret "]"))
                      )
                  (if (esn-use-pdx-p)
                      (setq ret (concat ret ","))
                    (setq ret (concat ret " (")))
                  (setq ret (concat ret (nth 2 x)))
                  (if (esn-use-pdx-p)
                      (setq ret (concat ret "]"))
                    (setq ret (concat ret ")"))
                    )
                  )
                (symbol-value 'ret)
                )
              )
            esn-yas-pending-thetas
            "\n"
            )
           ))
    (setq theta-snippet (list theta-snippet 'esn-yas-theta-snippet-start))
    (setq esn-yas-pending-thetas nil)
    ;; Now get omega snippet.
    (setq omega-snippet
          (esn-yas-number-pre-snippet
           (mapconcat
            (lambda(x)
              (let (
                    (eta (nth 0 x))
                    (text (nth 1 x))
                    (unit (nth 2 x))
                    v no-comment
                    pdx-unit
                    par
                    (comment " ; ")
                    (ret "       ")
                    )
                (when (string-match "\\<TV\\([A-Z][A-Z0-9_]*\\)\\>" text)
                  (setq par (match-string 1 text))
                  )
                (setq v (esn-yas-get-eta-type-ref text 'no-comment))
                (when (esn-use-pdx-p)
                  (setq comment (concat " ;" (nth 10 (nth v esn-eta-types)) " "))
                  )
                (setq ret (concat
                           ret "$ " comment
                           (format esn-yas-omega-formatting par)
                           " ("  (nth 0 (nth v esn-eta-types)) ")"))
                )
              )
            esn-yas-pending-omegas
            "\n"
            )
           )
          )
    (setq esn-yas-pending-omegas nil)
    (setq omega-snippet (list omega-snippet 'esn-yas-omega-snippet-start))
    (add-to-list 'esn-yas-pending-snippets omega-snippet)
    (add-to-list 'esn-yas-pending-snippets theta-snippet)
    )
  (symbol-value 'text)
  )
(defun esn-yas-omega-snippet-start ()
  "* Condition statement for pending $OMEGA snippets."
  (let ((ret nil))
    (cond
     ( (esn-rec "OME")
       (save-restriction
         ;; If present add to record if it doesn't contain BLOCK or SAME.
         (esn-narrow-to-current-problem)
         (goto-char (point-min))
         (while (re-search-forward (eval-when-compile (esn-reg-record-exp "OME" 't 't)) nil t)
           )
         (esn-narrow-rec)
         (goto-char (point-min))
         (if (re-search-forward "\\<\\(BLOCK[ \t]*(\\|SAME\\)" nil t)
             (progn
               ;; Add
               (goto-char (point-max))
               (widen)
               (skip-chars-forward " \t\n")
               (insert "\n$OMEGA\n")
               )
           (goto-char (point-max))
           (insert "\n")
           (widen)
           )
         )
       (setq ret 't)
       )
     ( 't
       (let (
             (expr (esn-reg-record-exp esn-yas-insert-omega-after))
             )
         (save-restriction
           (esn-narrow-to-current-problem)
           (goto-char (point-min))
           (while (re-search-forward expr nil t)
             )
           (esn-narrow-rec)
           (goto-char (point-max))
           (insert "\n$OMEGA\n")
           (widen)
           't
           )
         )
       (setq ret 't)
       )
     )
    (symbol-value 'ret)
    )
  )
(defun esn-yas-set-pending-buffer (&optional skip-back)
  "* Sets information about pending snippet in current buffer"
  (let ((max-val 0))
    (goto-char (point-min))
    (while (re-search-forward "[$][{]?\\([0-9]+\\)" nil t)
      (setq max-val (max (string-to-number (match-string 1)) max-val)))
    (goto-char (point-max))
    (if skip-back
        (insert (format "\n${%s:;Jump back to code block...$(esn-yas-jump)}" (+ 1 max-val)))
      (insert (format "\n${%s:;Pending blocks below...$(esn-yas-pending)}" (+ 1 max-val)))
      )
    
    )
  )
(defun esn-yas-set-pending (snip &optional skip-back)
  "* Sets information about a pending snippet"
  (let ((ret snip))
    (with-temp-buffer
      (insert snip)
      (esn-yas-set-pending-buffer skip-back)
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(defvar esn-yas-current-snip "")
(defvar esn-yas-current-snip-fn nil)
(defvar esn-yas-jump-pt nil)
(make-variable-buffer-local 'esn-yas-current-snip)
(make-variable-buffer-local 'esn-yas-current-snip-fn)
(make-variable-buffer-local 'esn-yas-jump-pt)
(defun esn-yas/abort-all-snippets ()
  "Abort all snippets."
  (interactive)
  (mapc #'(lambda (snippet)
            (yas/abort-snippet snippet)
            )
        (yas/snippets-at-point)))
(defun esn-yas-jump ()
  "* Jumps back to point in block"
  (unless (or yas/moving-away-p yas/modified-p)
    (setq esn-yas-pending-snippet-timer
          (run-with-timer 1 nil
                          (lambda()
                            (when esn-yas-jump-pt
                              (goto-char esn-yas-jump-pt)
                              (setq esn-yas-jump-pt nil)
                              (yas/exit-all-snippets)
                              (recenter)
                              )
                            )
                          )
          )
    ""
    )
  )
(defun esn-yas-pending ()
  "* Expands pending snippets"
  (unless (or yas/moving-away-p yas/modified-p)
    (when (and (not esn-yas-pending-snippets) esn-yas-pending-thetas)
      (esn-yas-fix-pending-units (buffer-substring (point-min) (point-max))))
    (when esn-yas-no-mu
      (unless (string= "" esn-yas-no-mu)
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (if (not (re-search-forward "\\<[$]\\(PRED?\\|PK\\)\\>" nil t))
                (esn-alert "Could not find $PRED/$PK.  Inserting MU references at the beginning of the file.")
              )
            (insert (concat esn-yas-no-mu "\n"))
            )
          )
        )
      )
    (unless esn-yas-jump-pt
      (setq esn-yas-jump-pt (point)))
    (let (
          (snip (pop esn-yas-pending-snippets))
          )
      (setq esn-yas-current-snip (nth 0 snip))
      (if esn-yas-pending-snippets
          (setq esn-yas-current-snip (esn-yas-set-pending esn-yas-current-snip))
        (setq esn-yas-current-snip (esn-yas-set-pending esn-yas-current-snip 't))
        )
      (setq esn-yas-current-snip-fn (nth 1 snip))
      (let (
            (rec (esn-get-current-rec ))
            (esn-run-save-fn nil)
            )
        (save-excursion
          (goto-char esn-get-current-record-stop)
          (backward-char 1)
          (cond
           ( (string= "PK" rec)
             (esn-align-equals-fun-hook)
             )
           ( (string= "THE" rec)
             (esn-number-theta-hook)
             (esn-align-matrix-hook)
             )
           ( (string= "OME" rec)
             (esn-number-eta-hook)
             (esn-align-matrix-hook)
             )
           )
          )
        )
      (setq esn-yas-pending-snippet-timer
            (run-with-timer 0.25 nil
                            (lambda()
                              (if (and esn-yas-current-snip-fn (funcall esn-yas-current-snip-fn))
                                  (let ((yas/indent-line nil))
                                    (yas/exit-all-snippets)
                                    (yas/expand-snippet esn-yas-current-snip)))))
            )
      )
    ""
    )
  )
(defun esn-yas-theta-snippet-start ()
  "* Condition statement for pending $THETA snippets"
  (cond
   ( (esn-rec "THE")
     (save-restriction
       ;; If present, add to record
       (esn-narrow-to-current-problem)
       (goto-char (point-min))
       (while (re-search-forward (eval-when-compile (esn-reg-record-exp "THE" 't 't)) nil t)
         )
       (esn-narrow-rec)
       (goto-char (point-max))
       (insert "\n")
       (widen)
       't
       )
     )
   ( 't
     (let (
           (expr (esn-reg-record-exp esn-yas-insert-theta-after))
           )
       (save-restriction
         (esn-narrow-to-current-problem)
         (goto-char (point-min))
         (while (re-search-forward expr nil t)
           )
         (esn-get-current-rec)
         (goto-char esn-get-current-record-stop2)

         (insert "\n$THETA\n")
         't
         )
       )
     't
     )
   )
  )
(defun esn-add-omega-theta (text)
  "* Adds Omega and Theta estimates to list based on selected eta types"
  (let (
        (theta (esn-yas-current-theta))
        (unit esn-yas-last-unit)
        (eta nil)
        v no-comment
        first
        )
    (save-match-data
      (when (string-match "\\<ETA(\\([0-9]+\\))" text)
        (setq eta (match-string 1 text)))
      (when (string= eta "")
        (when (string-match "\\<MU_\\([0-9]+\\)\\>" text)
          (setq eta (match-string 1 text)))
        )
      )
    (when eta
      (add-to-list 'esn-yas-pending-omegas (list eta text unit))
      )
    (setq v (esn-yas-get-eta-type-ref text 'no-comment))
    (mapc (lambda(th)
            (let (
                  theta-label
                  lower
                  upper
                  (theta-label (if (or first
                                       (= 3 esn-yas-mu-referencing-type))
                                   (nth 0 th) "%s") )
                  unt
                  )
              (setq first 't)
              (cond
               ( (= 3 esn-yas-mu-referencing-type)
                 (unless (string= (nth 3 th) "")
                   (setq lower (nth 3 th))
                   )
                 (unless (string= (nth 4 th) "")
                   (setq upper (nth 4 th)))
                 (setq unt "")
                 )
               ( 't
                 (unless (string= (nth 1 th) "")
                   (setq lower (nth 1 th))
                   )
                 (unless (string= (nth 2 th) "")
                   (setq upper (nth 2 th)))
                 (if first
                     (setq unt unit)
                   (setq unt ""))
                 )
               )
              (add-to-list 'esn-yas-pending-thetas (list theta text unt lower upper theta-label))
              )
            )
          (nth 9 (nth v esn-eta-types))
          )
    )
  )
(defun esn-yas-eta-types-transform (text &optional eta)
  "* Transforms the ETA types returned from the list of eta types and transforms them to code"
  (let ( v no-comment ret par tv (maxeta (or eta (+ 1 (esn-max-eta)))))
    (setq tv esn-yas-last-tv)
    (setq par tv)
    (when (string-match "^TV\\([A-Z][A-Z0-9]*\\)$" par)
      (setq par (match-string 1 par)))
    (setq v (esn-yas-get-eta-type-ref text 'no-comment))
    (setq ret (concat (esn-yas-format (nth 1 (nth v esn-eta-types)) par tv maxeta) " ; " (nth 0 (nth v esn-eta-types))))
    (symbol-value 'ret)
    )
  )
(defun esn-yas-eta-types (tv &optional eta units theta-value &rest args)
  "* Returns a list of eta types for YAS completion"
  (let (
        (maxeta (or eta (+ 1 (esn-max-eta))))
        (par tv)
        )

    (when (string-match "^TV" par)
      (setq par (replace-match "" nil nil par)))
    (setq esn-yas-last-tv tv)
    (when units
      (setq esn-yas-last-unit units)
      )
    (when theta-value
      (setq esn-yas-last-theta-value theta-value)
      )
    (mapcar (lambda(x)
              (concat par " ~ " (nth 0 x))
              )
            esn-eta-types)
    )
  )
(defun esn-yas-next-theta ()
  "* Returns next theta"
  (+ 1 (esn-max-theta 't))
  )
(defun esn-yas-next-eta ()
  "* Returns next eta"
  (+ 1 (esn-max-eta 't))
  )
(defun esn-yas-get-eta-type-ref (text no-comment-q)
  "*Gets the index number of the esn-eta-types based on text.  If no comment in text but found match anyway, set no-comment-q to true."
  (let (
        (i 0)
        (v nil)
        (len 0)
        tmp
        )
    ;; Find item with largest match.
    (mapc (lambda(x)
            (when (string-match (nth 0 x) text)
              (if (not v)
                  (progn
                    (setq v i)
                    (setq len (length (nth 0 x)))
                    )
                (setq tmp (length (nth 0 x)))
                (when (> tmp len)
                  (setq v i)
                  (setq len tmp)
                  )
                )
              )
            (setq i (+ i 1))
            )
          esn-eta-types
          )
    (unless v
      (setq tmp text)
      (when (string-match ";.*" tmp)
        (setq tmp (replace-match "" 't 't tmp)))
      (while (string-match " +" tmp)
        (setq tmp (replace-match "" 't 't tmp)))
      (while (string-match "MU_[0-9]+" tmp)
        (setq tmp (replace-match "MU_%s" 't 't tmp)))
      (while (string-match "ETA([0-9]+)" tmp) ;; Should take care of THETA(%s) too.
        (setq tmp (replace-match "ETA(%s)" 't 't tmp)))
      (while (string-match "\\<TV[A-Z][A-Z0-9_]*\\>" tmp)
        (setq tmp (replace-match "%s" 't 't tmp)))
      (setq i 0)
      (when (string-match "%s" tmp)
        (mapc (lambda(x)
                (unless v
                  (when (string-match (regexp-quote tmp) (nth 1 x))
                    (setq v i)
                    )
                  (setq i (+ i 1))
                  )
                )
              esn-eta-types
              )
        )
      (when v
        (set no-comment-q 't)
        )
      )
    (symbol-value 'v)
    )
  )

(defun esn-yas-mu-trf (text &optional type theta-number ref)
  " Transforms MU and THETAs appropriately when MU-referencing is requested
nil:  Parameter = ...
Type 1: Initial MU_x reference
Type 2: TVx = ...
"
  (let (
        (theta (if theta-number
                   (if (eq (type-of theta-number) 'string)
                       theta-number
                     (esn-yas-current-theta theta-number)
                     )))
        (ret "")
        (eta "")
        (v nil)
        (tv "")
        (par "")
        (tmp "")
        no-comment
        )
    (if (string= text "")
        (if (not type)
            (setq ret theta)
          (case type
            ( '1
              (setq ret "")
              )
            ( '2
              (setq ret (format "THETA(%s)" theta))
              )
            )
          )
      (setq v (esn-yas-get-eta-type-ref text 'no-comment))
      (if (not v)
          (if (not type)
              (setq ret theta)
            (case type
              ( '1
                (setq ret "")
                )
              ( '2
                (setq ret (format "THETA(%s)" theta))
                )
              )
            )
        (when no-comment
          (setq text (concat text " ; " (nth 0 (nth v esn-eta-types))))
          )
        (case type
          ( 'nil
            (if (not esn-yas-use-mu-referencing)
                (setq ret text) ; No Mu referencing
              
              (when (string-match "\\<ETA(\\([0-9]+\\))" text)
                (setq eta (match-string 1 text)))
              (when (string= eta "")
                (when (string-match "\\<MU_\\([0-9]+\\)\\>" text)
                  (setq eta (match-string 1 text)))
                )
              (if (= esn-yas-mu-referencing-type 1)
                  (progn
                    ;; MU reference after
                    (setq tmp (nth 2 (nth v esn-eta-types)))
                    (if  (string= tmp "")
                        (setq ret text) ; No mu referencing
                      ;; Mu referencing after
                      (when (string-match "\\<TV[A-Za-z][A-Za-z0-9_]*\\>" text)
                        (setq tv (match-string 0 text))
                        )
                      (setq ret (esn-yas-format (concat text "\n  " tmp) eta tv))
                      )
                    )
                ;; Switch to VAR = FUN(MU_x+ETA(x))
                (setq tmp (nth (case esn-yas-mu-referencing-type
                                 (2 5)
                                 (3 8)
                                 )
                               (nth v esn-eta-types)))
                (if (string= "" tmp)
                    (setq ret text) ; No Mu referencing.
                  (if (string= "" eta)
                      (setq ret text) ; No Bsv
                    (when (string-match "\\<TV\\([A-Za-z][A-Za-z0-9_]*\\)\\>" text)
                      (setq par (match-string 1 text))
                      )
                    (setq ret (esn-yas-format
                               (concat tmp
                                       (if (= esn-yas-mu-referencing-type 1)
                                           ""
                                         (concat
                                          " ; "
                                          (nth 0 (nth v esn-eta-types)))
                                         )
                                       ) par eta eta))
                    )
                  )
                )
              )
            )
          ( '1
            ;; Type 1 are the initial Mus
            (if (or (not esn-yas-use-mu-referencing) (= esn-yas-mu-referencing-type 1))
                (setq ret "")
              ;; Put in Mu-referecing code.
              (when (string-match "\\<ETA(\\([0-9]+\\))" text)
                (setq eta (match-string 1 text)))
              (when (string= eta "")
                (when (string-match "\\<MU_\\([0-9]+\\)\\>" text)
                  (setq eta (match-string 1 text)))
                )
              (setq tmp (nth (case esn-yas-mu-referencing-type
                               ('2 3)
                               ('3 6)
                               )
                             (nth v esn-eta-types)))
              (unless (string= tmp "")
                (if (string= eta "")
                    (setq ret "")
                  (setq ret (esn-yas-format (concat "\n  " tmp) eta theta))
                  )
                )
              )
            )
          ( '2 ; TVx = THETA(x) type
            (if (or (not esn-yas-use-mu-referencing) (= esn-yas-mu-referencing-type 1))
                (setq ret (esn-yas-format "THETA(%s)" theta))
              ;; Put in mu-referencing code
              (setq tmp (nth (case esn-yas-mu-referencing-type
                               ('2 4)
                               ('3 7)
                               )
                             (nth v esn-eta-types)))
              (if (string= "" tmp)
                  (setq ret (format "THETA(%s)" theta)) ;; No mu-referencing
                ;; Mu referencing
                (when (string-match "\\<ETA(\\([0-9]+\\))" text)
                  (setq eta (match-string 1 text)))
                (when (string= eta "")
                  (when (string-match "\\<MU_\\([0-9]+\\)\\>" text)
                    (setq eta (match-string 1 text)))
                  )
                (if (string= eta "")
                    (setq ret (format "THETA(%s)" theta)) ;; No BSV
                  (setq ret (esn-yas-format tmp eta theta))
                  )
                )
              )
            )
          )
        )
      )
    (symbol-value 'ret)
    )
  )
(defmacro esn-yas-tv (theta-number &optional no-mu)
  "* Macro for Typical parameter value"
  (if no-mu
      (list 'progn
            (list 'setq 'esn-yas-no-mu '"")
            (list 'esn-yas-mu-trf 'text '2 theta-number)
            )
    (list 'esn-yas-mu-trf 'text '2 theta-number)
    )
  )

(defun esn-yas-tv-mu (text &optional paren-or-action)
  "* Defines the MU=TVx pattern for TV expansion"
  (let (
        (p (or paren-or-action 1))
        (ret "")
        )
    (cond
     ( (string-match "DEXP" text)
       (cond
        ( (= p 1)
          (setq ret "DLOG(")
          )
        ( 't
          (setq ret ")")
          )
        )
       )
     )
    (symbol-value 'ret)
    )
  )
(defun esn-yas-esn-snippet (snip)
  "* Changes esn snippets to Yas snippets
Supported code:
  TVPar = THETA(Units) where Units is in Volume and Time.
  This causes the parameter to be selected based on a prompt of between subject error structure

  SCALE -- Prompts for the scale factor

  $0 starting point"
  (let (
        (case-fold-search 't)
        (i 1)
        (mexp "")
        (mu-format "%s${%s:$(esn-yas-mu %s)}")
        (par-format "%s = ${%s:$(esn-yas-tv %s)}%s\n  ${%s:$$(esn-yas-par %s \"%s\" \"%s\")}")
        (scale-format "${%s:$$(esn-yas-scale)}")
        ret
        )
    (with-temp-buffer
      (insert snip)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(TV[A-Z][A-Z0-9]*\\)[ \t]*=[ \t]*THETA(\\([^)]*\\))\\(.*\\)" nil t)
        (replace-match
         (format par-format (match-string 1) i i (match-string 3) i i (match-string 1) (match-string 2)) 't 't)
        (setq mexp (format mu-format mexp i i))
        (setq i (+ i 1))
        )
      (when (re-search-forward "\\<SCALE\\>" nil t)
        (replace-match (format scale-format i) 't 't)
        (setq i (+ i 1))
        )
      (goto-char (point-min))
      (when (re-search-forward "[$]\\(PRED\\|PK\\)" nil 't)
        (insert mexp)
        )
      ;; Fix pending snippets.
      (esn-yas-set-pending-buffer)
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-expand-snippet (snippet)
  "* Expands snippet and then finishes snippet expansion"
  (yas/expand-snippet snippet)
  )
;; Build Compartment Yasnippets
;;;###autoload
(defun esn-build-yas ()
  "* Build Yasnippets for Esn-mode."
  (interactive)
  (when (boundp 'yas/root-directory)
    (let (
          (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                       yas/root-directory
                     (nth 0 yas/root-directory)
                     ))
          added-snippets
          file
          atfile
          snippet-list
          )
      (setq new-dir (concat new-dir "esn-mode/"))
      (unless (file-exists-p new-dir)
        (make-directory new-dir 't)
        )
      (when (or (interactive-p) (not (file-exists-p (concat new-dir ".yas-parents"))))
        (mapc
         (lambda(x)
           (if (memq (nth 0 x) '(5 6 7 8 9 13))
               nil
             (mapc
              (lambda(y)
                (setq file (let (
                                 (str (format "%s %s" (nth 1 x) (nth 1 y)))
                                 (ret "")
                                 )
                             (cond
                              ( (string-match "One" str)
                                (setq ret "1C")
                                )
                              ( (string-match "Two" str)
                                (setq ret "2C")
                                )
                              ( (string-match "Three" str)
                                (setq ret "3C")
                                )
                              )
                             (if (not (string-match "First" str))
                                 (setq ret (concat ret ".IV")))
                             (cond
                              ( (string-match "AOB" str)
                                (setq ret (concat ret ".AOB"))
                                )
                              ( (string-match "ALPHA" str)
                                (setq ret (concat ret ".ALPHA"))
                                )
                              ( (string-match "VSS" str)
                                (setq ret (concat ret ".VSS")))
                              ( (not (string-match "CL" str))
                                (setq ret (concat ret ".K"))
                                )
                              )
                             (symbol-value 'ret)
                             ))
                (setq atfile file)
                (while (string-match "[.]" atfile)
                  (setq atfile (replace-match "_" nil nil atfile)))
                (setq atfile (downcase (concat atfile ".yasnippet")))
                (if (not (file-exists-p (concat new-dir atfile)))
                    (with-temp-file (concat new-dir atfile)
                      (message "Adding Snippet %s" file)
                      (setq added-snippets 't)
                      (insert "# -*- mode: snippet -*-\n")
                      (insert "# name: ")
                      (insert (concat (nth 1 x) " " (nth 1 y)))
                      (insert "\n# key: ")
                      (insert file)
                      (insert "\n# group: ")
                      (insert (cond
                               ( (string-match "One Compartment" (nth 1 x))
                                 "One Compartment"
                                 )
                               ( (string-match "Two Compartment" (nth 1 x))
                                 "Two Compartment"
                                 )
                               ( (string-match "Three Compartment" (nth 1 x))
                                 "Three Compartment"
                                 )
                               ))
                      (insert "\n# contributor: Matthew L. Fidler")
                      (insert "\n# --\n")
                      (insert (esn-yas-esn-snippet
                               (format
                                "$SUBROUTINES ADVAN%s TRANS%s\n$PK\n%s%s"
                                (nth 0 x) (nth 0 y)
                                (mapconcat
                                 (lambda(z)
                                   (if (not (string-match "^TV" (nth 0 z)))
                                       ""
                                     (format "\n  %s = THETA(%s) ; %s\n"
                                             (nth 0 z)
                                             (cond
                                              ( (string-match "clearance" (nth 1 z))
                                                "Volume/Time")
                                              ( (string-match "volume" (nth 1 z))
                                                "Volume")
                                              ( (string-match "\\(rate\\|alpha\\|beta\\|gamma\\)" (nth 1 z))
                                                "1/Time")
                                              ( 't
                                                "")
                                              )
                                             (with-temp-buffer
                                               (insert (nth 1 z))
                                               (goto-char (point-min))
                                               (when (re-search-forward "\\<Typical\\> *" nil 't)
                                                 (replace-match ""))
                                               (buffer-substring (point-min) (point-max))
                                               )
                                             )
                                     )
                                   )
                                 (nth 3 y)
                                 ""
                                 )
                                (mapconcat
                                 (lambda(z)
                                   (if (string-match "^S[0-9]+" (nth 0 z))
                                       (if (string-match "central" (nth 1 z))
                                           (concat "\n  " (nth 0 z) " = SCALE \n")
                                         ""
                                         )
                                     "")
                                   )
                                 (nth 3 x)
                                 ""
                                 )
                                ))

                              )
                      )
                  )
                )
              (nth 4 x)
              )
             )
           )
         esn-advan-trans-vars
         )
        (with-temp-file (concat new-dir "tv.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: TV
# key: TV
# contributor: Matthew L. Fidler
# --\n

  TV$1 = ${2:$(esn-yas-tv 1)}
  ${2:$$(esn-yas-par-tv 1 1)}
  ${3:;Pending blocks below...$(esn-yas-pending)}$0
  ")
          )
        (with-temp-file (concat new-dir "ccv.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: CCV/Proportional
# key: PROP
# group: Residual Errors (THETA)
# contributor: Matthew L. Fidler
# --\n
$ERROR
  IPRED = F
  IRES  = DV-IPRED
  W     = `(esn-yas-theta 1 \"Proportional/CCV Error (SD)\" nil \"0\")`*IPRED
  W     = 0
  IF (W.EQ.0) THEN
    DEL = 1
  ENDIF
  IWRES = IRES/(W+DEL)
  Y     = F+EPS*W
  ${1:;Pending blocks below...$(esn-yas-pending)}$0
  ")
          )
        (with-temp-file (concat new-dir "addprop.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: Additive+CCV/Proportional 
# key: ADDPROP
# group: Residual Errors (THETA)
# contributor: Matthew L. Fidler
# --\n
$ERROR
  IPRED = F
  IRES  = DV-IPRED
  W     = SQRT(`(esn-yas-theta 1 \"Additive Error (SD)\" nil \"0\")`**2+`(esn-yas-theta 2 \"Proportional Error (SD)\" nil \"0\")`**2*IPRED**2)
  W     = 0
  IF (W.EQ.0) THEN
    DEL = 1
  ENDIF
  IWRES = IRES/(W+DEL)
  Y     = F+EPS*W
  ${1:;Pending blocks below...$(esn-yas-pending)}$0
  ")
          )
        (with-temp-file (concat new-dir "add.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: Additive
# key: ADD
# group: Residual Errors (THETA)
# contributor: Matthew L. Fidler
# --\n
$ERROR
  IPRED = F
  IRES  = DV-IPRED
  W     = `(esn-yas-theta 1 \"Additive Error (SD)\" nil \"0\")`
  W     = 0
  IF (W.EQ.0) THEN
    DEL = 1
  ENDIF
  IWRES = IRES/(W+DEL)
  Y     = F+EPS*W
  ${1:;Pending blocks below...$(esn-yas-pending)}$0
  ")
          )

        (with-temp-file (concat new-dir "foce.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: FOCE
# key: FOCE
# group: Estimation Methods
# contributor: Matthew L. Fidler
# --\n
$ESTIMATION METHOD=CONDITIONAL NOLAPLACIAN MAXEVALS=${1:9999} SIGDIGITS=${2:3} ${3:NOABORT}
            PRINT=${4:1} MSFO=`(esn-msfo-name)`

$0
  ")
          )
        (with-temp-file (concat new-dir "focec.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: FOCE with Covariance
# key: FOCEC
# group: Estimation Methods
# contributor: Matthew L. Fidler
# --\n
$ESTIMATION METHOD=CONDITIONAL NOLAPLACIAN MAXEVALS=${1:9999} SIGDIGITS=${2:3} ${3:NOABORT}
            PRINT=${4:1} MSFO=`(esn-msfo-name)`

$COVARIANCE PRINT=E

$0
  ")
          )
        (with-temp-file (concat new-dir "focei.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: FOCE with Interaction
# key: FOCEI
# group: Estimation Methods
# contributor: Matthew L. Fidler
# --\n

$ESTIMATION METHOD=CONDITIONAL INTERACTION NOLAPLACIAN MAXEVALS=${1:9999}
            SIGDIGITS=${2:3} ${3:NOABORT} PRINT=${4:1} MSFO=`(esn-msfo-name)`

$0
  ")
          )
        (with-temp-file (concat new-dir "foceic.yasnippet")
          (insert "# -*- mode: snippet -*-
# name: FOCE with Interaction and Covariance
# key: FOCEIC
# group: Estimation Methods
# contributor: Matthew L. Fidler
# --\n

$ESTIMATION METHOD=CONDITIONAL INTERACTION NOLAPLACIAN MAXEVALS=${1:9999}
            SIGDIGITS=${2:3} ${3:NOABORT} PRINT=${4:1} MSFO=`(esn-msfo-name)`

$COVARIANCE PRINT=E

$0
  ")
          )
        
        (with-temp-file (concat new-dir ".yas-parents")
          (insert "text-mode")
          )
        )
      (yas/load-directory new-dir)
      )
    )
  )
(when nil
  (yas/define-snippets 'esn-mode 
                       (append 
                        '(
                          ("TV"
                           
                           "TV" ; Name
                           nil ; Condition
                           nil ; Group
                           nil ; Let
                           (or load-file-name buffer-file-name) ; File
                           nil ;Keybinding
                           )
                          )
                        yas-snippets
                        )
                       )
  )                   
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Snippets
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; (KEY TEMPLATE NAME CONDITION GROUP EXPAND-ENV FILE KEYBINDING)

(provide 'esn-yas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-yas.el ends here
