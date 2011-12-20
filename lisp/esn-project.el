;;; esn-project.el --- Project functions
;; 
;; Filename: esn-project.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue Jan 11 13:23:53 2011 (-0600)
;; Version: 
;; Last-Updated: Wed Apr 27 18:59:03 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 32
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `cl', `help-fns'.
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
(eval-when-compile
  (require 'cl))

(defvar esn-project-reg "\\(;[;C]*[ \t]*\\(?:Free\\|X\\)?\\(?:mind\\(?:map\\)?\\|map\\|org\\)[ \t]*[:=][ \t]*\\)\\([^ \t].*?\\)\\([ \t]*\\)$"
  "Regular expression for project orginzations.  Separated by >  For example:

; Xmind: Project > One compartment model or
; Org: Project > One compartment model

Will create/place the control stream information in the appropriate place.
"
  )

;;;###autoload
(defmacro* def-esn-project-update (function-name &key apply-function message)
  "Defines a project update to do on control streams
FUNCTION-NAME is the resulting function.
APPLY-FUNCTION is the function that should be applied to the control stream."
  (declare (indent 1))
  `(defun ,function-name ()
     ,(concat "Interactive function calling function `" (symbol-name apply-function) "' on every control stream in the project directory")
     (interactive)
     (let (
           (files (esn-project-get-control-streams))
           (i 0))
       (mapc (lambda(file)
               (save-window-excursion
                 (let (
                       (opened (get-file-buffer file))
                       (esn-skip-read-only-check 't)
                       (case-fold-search 't))
                   (if opened
                       (set-buffer opened)
                     (set-buffer (find-file-noselect file 't)))
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward "\\<[$]PRO" nil t)
                       (setq i (+ i 1))
                       (esn-message
                        ,(format "%s #%%s, based on file: %%s" (if message message
                                                                 (format "`%s' update" (symbol-name apply-function)))) i file)
                       (condition-case err
                           (progn
                             (,apply-function))
                         (error (esn-alert "Error updating %s: %s" file
                                           (error-message-string err)))))
                     (unless opened
                       (kill-buffer (current-buffer)))))))
             files))))


;;;###autoload
(defun esn-project-get-control-streams ()
  "* Gets the control stream possibilities based on the current directory."
  (interactive)
  (let* (
         (dirs (if (esn-use-plt-p)
                   (remove-if-not (lambda(x)
                                    (and
                                     (file-directory-p x)
                                     (string-match "work" (downcase x))
                                     (not (string-match "\\(plta\\|xmind\\)" (downcase x)))
                                     (not (string-match "[/\\\\][.]plt" (downcase x)))))
                                  (file-expand-wildcards "../*"))
                 '("./")))
         (ret
          (apply 'append
                 (mapcar
                  (lambda(ext)
                    (apply 'append
                           (mapcar
                            (lambda(dir)
                              (file-expand-wildcards (concat dir "/*" ext)))
                            dirs)))
                  (append esn-default-extension (list ".txt"))))))
    (symbol-value 'ret)))


(provide 'esn-project)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-project.el ends here
