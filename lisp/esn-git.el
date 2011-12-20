;;; esn-git.el --- Esn GIT routines
;; 
;; Filename: esn-git.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:19:24 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 18:59:38 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 2
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


;; Add GIT support to EsN

(defun esn-git-installed()
  "* Assess if GIT is installed.  Returns the version."
  (interactive)
  (let (
	(git-version (esn-command-to-string "git version"))
	(ret nil)
	)
    (when (string-match "git version \\(.*\\)$" git-version)
      (setq ret (match-string 1 git-version))
      )
    (message "%s" ret)
    (symbol-value 'ret)
    )
  )

(let (
      (tmp (esn-git-installed))
      (ver 0)
      )
  (when tmp
    (when (string-match "[0-9]+\\.[0-9]+" tmp)
      (setq ver (string-to-number (match-string 0 tmp))) 
      (unless (< ver 1.6)
	;; Make changes to git-emacs to support 1.6
	(defun git--exec (cmd outbuf inbuf &rest args) 
	  "Execute 'git' clumsily for 1.6"
	  (apply #'call-process 
		 "git"				; cmd
		 inbuf				; in buffer
		 outbuf				; out buffer
		 nil				; display
		 (append (list cmd) args)))	; args
	)
      )
    )
  )


(provide 'esn-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-git.el ends here
