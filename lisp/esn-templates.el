;;; esn-templates.el --- EsN templates
;; 
;; Filename: esn-templates.el
;; Description: Templates for files for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Jan 12 09:06:53 2011 (-0600)
;; Version: 
;; Last-Updated: Wed Apr 27 18:58:38 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 104
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `help-fns'.
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
(defmacro esn-seq (n)
  "Creates a sequence of integers"
  (let ((ar (make-symbol "ar"))
        (i (make-symbol "i")))
    `(let (,ar '())
       (dotimes (,i ,n)
          (setq ,ar (append ,ar (list ,i))))
       ,ar)))

(defun esn-seq-concat (n &optional format sep function)
  "Creates a concatenated sequence of integers
N specifies the number to stop with 1 .. N inclusive
FORMAT specifies the format of the variable
FUNCTION is the function to call on the integer of interest
SEP specifies the separator"
  (mapconcat (lambda(x) (format (or format "%s") (if function
                                                     (funcall function (+ x 1))
                                                   (+ x 1))))
             (esn-seq n)
             (or sep ",")))
(defun esn-parse-template (template)
  "Fills in a EsN
TEMPLATE is the template to be parsed.

Within the template the following may be used:

Constants used with `format-time-string' such as %Y may be used.

$FN defines a function or variable to put in content
% % represents a space
$(EXPR) defines an expression to evaluate
"
  (let ((ret template)
        var
        p1 p2 expr (cur-buf (current-buffer))
        (start 0))
    ;; First replace % % types
    (while (string-match "[%]\\( +\\)[%]" ret start)
      (setq ret (replace-match "\\1" t nil ret))
      (setq start (+ (match-beginning 0) (length (match-string 1 ret)))))
    ;; Then replace %FN%
    (setq start 0)
    (while (string-match "[$]\\([^(][^ \t\n]*\\)" ret start)
      (setq var (intern-soft (match-string 1 ret)))
      (save-match-data
        (cond
         ((not var) ;; Not defined, leave alone.
          
          (setq var ""))
         ((fboundp var) ;; Function
          (condition-case error
              (setq var (format "%s" (funcall var)))
            (error
             (message "Error while running `%s': %s"
                      (symbol-name var)
                      (error-message-string error))
             (setq var ""))))
         ((boundp var) ;; Variable
          (setq var (format "%s" (symbol-value var))))))
      (setq ret (replace-match var t t ret))
      (setq start (+ (match-beginning 0) (length var))))
    (setq start 0)
    ;; Now replace $() constructs
    (with-temp-buffer
      (insert ret)
      (with-syntax-table text-mode-syntax-table
        (goto-char (point-min))
        (while (re-search-forward "[$][(]" nil t)
          (backward-char 1)
          (setq p1 (point))
          (forward-sexp 1)
          (setq p2 (point))
          (setq expr (buffer-substring-no-properties p1 p2))
          (delete-region (- p1 1) p2)
          (condition-case error
              (save-excursion
                (set-buffer cur-buf)
                (setq var (eval (read expr)))
                )
            (error
             (message "[EsN] Error evaluating expression in template %s: %s" expr (error-message-string error))
             (setq var (concat "$" expr))))
          (insert var))
        (setq ret (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Put in format-time-string constructs
    (setq ret (format-time-string ret))
    (symbol-value 'ret)))
(defmacro* def-esn-template (template-name description &key content group)
  "Define EsN Template"
  (declare (indent 1))
  `(progn
     (defcustom ,template-name ,content
       ,(format  "EsN template for %s (See `esn-parse-template' for a description)" description)
       :type 'string
       :group ,group)
     (defun ,template-name ()
       ,(format "%s (uses `%s')" description (symbol-name template-name))
       (interactive)
       (esn-parse-template ,template-name))))


(provide 'esn-templates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-templates.el ends here
