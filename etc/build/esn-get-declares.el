;;; esn-get-declares.el --- Get the declare-functions
;; 
;; Filename: esn-get-declares.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Apr 27 22:53:39 2011 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 11:38:34 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 74
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Required feature `esn-get-declares' was not provided.
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

(defvar esn-declares '())
(setq esn-declares nil)
;;;###autoload
(defun esn-gen-declares ()
  "Generate Emacs Speaks NONEM Autoloads using `update-file-autoloads' and put in `esn-autoloads.el'"
  (interactive)
  (let (
        (all-files (file-expand-wildcards (concat esn-path "lisp/*.el")))
	tmp
	(defs esn-declares)
	defs-try
	p1 p2
	declares
	)
    (save-excursion
      (setq p1 (point-at-bol))
      (beginning-of-line)
      (when (looking-at "\\([ \t]*;.*\n\\)+")
        (setq defs-try (split-string (match-string 0) "[ ,;\t\n]+" t))))
    (unless esn-declares 
      (mapc
       (lambda(el-file)
	 (with-temp-buffer
	   (insert-file-contents el-file nil nil nil t)
	   (goto-char (point-min))
	   (setq tmp el-file)
	   (when (string-match (regexp-quote (concat esn-path "lisp/")) el-file)
	     (setq tmp (replace-match "" nil nil el-file)))
	   (when (string-match ".el$" tmp)
	     (setq tmp (replace-match "" nil nil tmp)))
	   
           (while (re-search-forward "[(]def\\(?:un\\*?\\|alias\\)[ \t\n]*'?\\([-A-Z0-9a-z/<>=]+\\)" nil t)
	     (add-to-list 'defs (list (match-string 1) tmp)))))
       all-files)
      (setq esn-declares defs))
    (setq declares
	  (mapconcat
	   (lambda(x)
             (let ((tmp (assoc x defs)))
	       (if (not tmp)
		   ""
		 (format "(declare-function %s \"%s\")\n" (nth 0 tmp) (nth 1 tmp)))))
	   defs-try ""))
    (save-excursion
      (setq p1 (point-at-bol))
      (beginning-of-line)
      (when (looking-at "\\([ \t]*;.*\n\\)+")
        (replace-match declares)
	(indent-region (match-beginning 0) (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-get-declares.el ends here
