;;; esn-vc.el --- EsN's version control
;; 
;; Filename: esn-vc.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Apr 22 12:19:16 2010 (-0500)
;; Version: 
;; Last-Updated: Fri May 13 16:27:01 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 32
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
;; 27-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 27 10:51:20 2010 (-0500) #17 (Matthew L. Fidler)
;;    Bug fixes... Some parentheses were in the wrong place.
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:19:20 2010 (-0500) #1 (Matthew L. Fidler)
;;    Added EsN's error procedure
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

(declare-function esn-error "esn-exec")

;; Mtn --  Monotone -- not suppported.
;; C:\Python25\Scripts\bzr.bat

(defun esn-hookless-save-buffer ()
  "Hookless save buffer."
  (let (write-contents-functions
        write-file-functions
        before-save-hook
        after-save-hook))
  (save-buffer))

(defun esn-vc-diff (historic &optional not-urgent)
  "Differences from last save."
  (interactive (list current-prefix-arg t))
  (let (
	(bm (buffer-modified-p))
	(file (buffer-file-name))
	(ob (current-buffer))
	(cb nil)
	(write-contents-funtions nil))
    (if bm
	(if (y-or-n-p (format "Buffer %s modified; save it? " (buffer-name)))
	    (progn
	      (esn-update-header)
	      (esn-hookless-save-buffer)
	      (set-buffer-modified-p nil))
	  (esn-error "Buffer must be saved to view differences."))
      (esn-update-header)
      (set-buffer-modified-p nil))
    (if historic
	(call-interactively 'vc-version-diff)
      (call-interactively 'vc-diff))
    (setq cb (current-buffer))
    (set-buffer ob)
    (esn-after-save-function)
    (set-buffer cb)))

(defun esn-vc-commit ()
  "Commits a version to the repository."
  (interactive)
  (let (
	(files (append (list (buffer-file-name))
		       	 (esn-get-input-files)
			 (esn-get-output-files 't)))
	(bm (buffer-modified-p)))
    (esn-update-header)
    (set-buffer-modified-p nil)
    (esn-on-save-use-vc files)
    (esn-after-save-function)
    (set-buffer-modified-p bm)))

(defconst esn-commit-message-start
  "--- Enter your commit message.  Type `C-c C-c' to commit. ---\n")

(defconst esn-commit-message-end
  "--- Files in bold will be committed.  Selection can be toggled. ---\n")

;; Git support.

(defun esn-git-installed ()
  "* Determines if Git is installed.  Return command name."
  (interactive)
  (let (
	(exe
	 (or (executable-find "git")
	     (dolist (path '("~/bin/git" "/usr/bin/git" "/usr/local/bin/git"))
	       (when (file-executable-p path)
		 (return path))))))
    (symbol-value 'exe)))
(defvar esn-git-bin ""
  "Defines the executable location for Git."
  )
(defvar esn-git-ver -1
  "Defines the version for GIT."
  )
(defun esn-git-version ()
  " Determines the Git Version and/or if installed.  If not
installed, return nil, otherwise return version number, and set
the esn-git-bin variable.
"
  (when (= esn-git-ver -1)
    (setq esn-git-bin (esn-git-installed))
    (if (not esn-git-bin)
	(setq esn-git-ver nil)
      (setq esn-git-ver (cdr (esn-run-command esn-git-bin "version")))
      (if (not (string-match "\\<[0-9]+\\.[0-9]+" esn-git-ver))
	  (setq esn-git-ver 0)
	(setq esn-git-ver (string-to-number (match-string 0 esn-git-ver))))))
  (symbol-value 'esn-git-ver))
;; Bazaar support.
(defun esn-bzr-installed ()
  "Determines if Bazar is installed.  Return command name."
    (let (
	(exe
	 (or (executable-find "bzr")
	     (dolist (path '("~/bin/bzr" "/usr/bin/bzr" "/usr/local/bin/bzr"
			     "C:/Python25/Scripts/bzr.bat"
			     "C:/Python24/Scripts/bzr.bat"
			     ))
	       (when (file-executable-p path)
		 (return path))))))
      (symbol-value 'exe)))
;; Darcs support.

(defun esn-darcs-installed ()
  "Determines if Darcs is installed.  Return command name."
;  (error "%s" (concat esn-path "bin/darcs"))
    (let (
	(exe
	 (or (executable-find "darcs")
	     (dolist (path (append '("~/bin/darcs"
			     "/usr/bin/darcs"
			     "/usr/local/bin/darcs"
			     )
				   (list
				    (if esn-w32
					(concat esn-path "bin/darcs.exe")
				      "/usr/local/bin/darcs"
				      ))))
	       (when (file-executable-p path)
		 (return path))))))
      (symbol-value 'exe)))

;; RCS installed
(defun esn-rcs-installed ()
  "Determines if RCS is installed.  Return command name."
;  (error "%s" (concat esn-path "bin/rcs"))
    (let (
	(exe
	 (or (executable-find "rcs")
	     (dolist (path (append '("~/bin/ci"
			     "/usr/bin/ci"
			     "/usr/local/bin/ci"
			     )
				   (list
				    (if esn-w32
					(concat esn-path "bin/ci.exe")
				      "/usr/local/bin/ci"
				      ))))
	       (when (file-executable-p path)
		 (return path))))))
      (if (getenv "TZ")
	  (symbol-value 'exe)
	(message "To use RCS, please specify the time zone in the environment variable \"TZ.\"")
	nil
	)))


;; Monotone support
;; Need signing, not supported, only rudimentary installation check.

(defun esn-mtn-installed ()
  "Determines if Mtn is installed.  Return command name."
  (let (
	(exe
	 (or (executable-find "mtn")
	     (dolist (path '("~/bin/mtn" "/usr/bin/mtn" "/usr/local/bin/mtn"))
	       (when (file-executable-p path)
		 (return path))))))
    (symbol-value 'exe)))


;; Hg support

(defun esn-hg-installed()
  (interactive)
  "Determines if Hg is installed.  Return command name."
  (let (
	(exe
	 (or (executable-find "hg")
	     (dolist (path '("~/bin/hg" "/usr/bin/hg" "/usr/local/bin/hg"))
	       (when (file-executable-p path)
		 (return path))))))
    (when (and esn-w32 exe)
      (setq exe (concat esn-path "hgb.bat")))
    (symbol-value 'exe)))

(defun esn-run-command-add-files (command add-files &rest args)
  (let* (exit-code
	 (output
	  (with-output-to-string
	    (with-current-buffer
		standard-output
	      (let ((add-file (split-string add-files " +")))
	      (setq exit-code
		    (apply 'call-process command nil t nil (append args add-file))))))))
    (cons exit-code output)))


(defun esn-run-command (command &rest args)
  "Run the shell command COMMAND, returning (EXIT-CODE . COMMAND-OUTPUT).
The list ARGS contains a list of arguments to pass to the command."
  (let* (exit-code
	 (output
	  (with-output-to-string
	    (with-current-buffer
		standard-output
	      (setq exit-code
		    (apply 'call-process command nil t nil args))))))
    (cons exit-code output)))

(defun esn-add-to-vc (roots files comment &optional search)
  "* Actual commit function."
  (cond
   ( (or (and (not search) (string= esn-vc-backend "git") (esn-git-version))
	 (and search (esn-git-version)))
     ;; Git is the backend.
     (let (
	   (git-bin esn-git-bin)
	   (git-version esn-git-ver)
	   (file (make-temp-file "esn-git"))
	   (buf nil))
       (setq buf (find-file file))
       (insert comment)
       (save-buffer buf)
       (kill-buffer buf)
       (mapc (lambda(root)
	       (unless (file-exists-p (concat root ".git"))
		 ;; Initialize Git repository.
		 (if (>= esn-git-ver 1.6)
		     (esn-run-command git-bin "init" root)
		   (esn-run-command (concat git-bin "-init") root)))
	       ;; Now  Add files for this root directory.
	       (let (
		     (add-files
		      (mapconcat
		       (lambda (file)
			 (when file
			   (let ((ret ""))
			     (when (string-match (regexp-quote root) file)
			       (setq ret (replace-match "" nil nil file)))
			     (symbol-value 'ret))))
		       files
		       " "
		       )))
		 (save-window-excursion
		   (setq buf (find-file root))
		   (if (>= esn-git-ver 1.6)
		       (progn
			 (message "%s" (nth 1 
                                   (esn-run-command-add-files git-bin add-files "add")))
			 (message "%s" (nth 1
                                   (esn-run-command-add-files git-bin add-files "commit" "-F" file)))
			 (kill-buffer buf))
		     (message "%s" (nth 1 (esn-run-command-add-files (concat git-bin "-add") add-files)))
		     (message "%s" (nth 1 (esn-run-command-add-files (concat git-bin "-commit") add-files "-F" file)))
		     (kill-buffer buf)))))
	     roots
	     )
       (delete-file file)))
   ( (or (and (not search) (string= esn-vc-backend "hg") (esn-hg-installed))
	 (and search (esn-hg-installed)))
     ;; Hg is the backend.
     (let (
	   (hg-bin (esn-hg-installed))
	   (file (make-temp-file "esn-hg"))
	   (buf nil))
       (setq buf (find-file file))
       (insert comment)
       (save-buffer buf)
       (kill-buffer buf)
       (mapc (lambda(root)
	       (unless (file-exists-p (concat root ".hg"))
		 ;; Initialize Hg repository.
		 (esn-run-command hg-bin "init" root))
	       ;; Now  Add files for this root directory.
	       (let (
		     (add-files
		      (mapconcat
		       (lambda (file)
			 (when file
			   (let ((ret ""))
			     (when (string-match (regexp-quote root) file)
			       (setq ret (replace-match "" nil nil file)))
			     (symbol-value 'ret))))
		       files
		       " "
		       )))
		 (save-window-excursion
		   (setq buf (find-file root))
		   (mapc (lambda(x)
			   (when (and x
				      (not (string= x "\"\""))
				      (not (string= x "")))
			     (message "%s" (nth 1 (esn-run-command-add-files hg-bin x "add")))
			     (message "%s" (nth 1 (esn-run-command-add-files hg-bin x "commit" "-l" file)))))
			 (split-string add-files " +"))
		   (kill-buffer buf))))
	     roots
	     )
       (delete-file file)))
   ( (or (and (not search) (string= esn-vc-backend "bzr") (esn-bzr-installed))
	 (and search (esn-bzr-installed)))
     ;; Bazaar is the backend.
     (let (
	   (bzr-bin (esn-bzr-installed))
	   (file (make-temp-file "esn-bzr"))
	   (buf nil))
       (message "%s" file)
       (setq buf (find-file file))
       (insert comment)
       (save-buffer buf)
       (kill-buffer buf)
       (mapc (lambda(root)
	       (unless (file-exists-p (concat root ".bzr"))
		 ;; Initialize Bzr repository.
		 (esn-run-command bzr-bin "init" root))
	       ;; Now  Add files for this root directory.
	       (let (
		     (add-files
		      (mapconcat
		       (lambda (file)
			 (when file
			   (let ((ret ""))
			     (when (string-match (regexp-quote root) file)
			       (setq ret (replace-match "" nil nil file)))
			     (symbol-value 'ret))))
		       files
		       " "
		       )))
		 (save-window-excursion
		   (setq buf (find-file root))
		   (message "%s" (nth 1 (esn-run-command-add-files bzr-bin add-files "--no-recurse" "add")))
		   (message "%s" (nth 1 (esn-run-command-add-files bzr-bin add-files "commit" "-F" file)))
		   (kill-buffer buf))))
	     roots
	     )
       (delete-file file)))

   ( (or (and (not search) (string= esn-vc-backend "darcs") (esn-darcs-installed))
	 (and search (esn-darcs-installed)))
     ;; Darcs is the backend.
     (let (
	   (darcs-bin (esn-darcs-installed))
	   (file (make-temp-file "esn-darcs"))
	   (buf nil))
       (setq buf (find-file file))
       (insert comment)
       (save-buffer buf)
       (kill-buffer buf)
       (mapc (lambda(root)
	       (esn-run-command darcs-bin "initialize" (concat "--repodir=" root))
	       ;; Now  Add files for this root directory.
	       (let (
		     (add-files
		      (mapconcat
		       (lambda (file)
			 (when file
			   (let ((ret ""))
			     (when (string-match (regexp-quote root) file)
			       (setq ret (replace-match "" nil nil file)))
			     (symbol-value 'ret))))
		       files
		       " "
		       )))
		 (save-window-excursion
		   (setq buf (find-file root))
		   (message "%s" (nth 1 
                                      (esn-run-command-add-files 
                                       darcs-bin 
                                       add-files 
                                       "--not-recursive" 
                                       (concat "--repodir=" root)
                                       "add")))
		   (message "%s" (nth 1 
                                      (esn-run-command-add-files 
                                       darcs-bin 
                                       add-files 
                                       (concat "--repodir=" root) 
                                       (concat "--logfile=" file) 
                                       "record" ))))
		 (kill-buffer buf)))
             roots
             )
       (delete-file file)))
   ( (or (and (not search) (string= esn-vc-backend "rcs") (esn-rcs-installed))
	 (and search (esn-rcs-installed)))
     ;; Rcs is the backend.
     (let (
	   (rcs-bin (esn-rcs-installed))
	   (file (make-temp-file "esn-rcs"))
	   (buf nil))
       (setq buf (find-file file))
       (insert comment)
       (save-buffer buf)
       (kill-buffer buf)
       (mapc (lambda(root)
	       (unless (and root (file-exists-p (concat root "RCS")))
	       ;; Initialize Rcs repository.
		 (make-directory (concat root "RCS")))
	       ;; Now  Add files for this root directory.
	       (let (
		     (add-files
		      (mapconcat
		       (lambda (file)
			 (when file
			   (let ((ret ""))
			     (when (string-match (regexp-quote root) file)
			       (setq ret (replace-match "" nil nil file)))
			     (symbol-value 'ret))))
		       files
		       " "
		       )))
		 (save-window-excursion
		   (setq buf (find-file root))
		   (message (cdr (esn-run-command-add-files rcs-bin add-files "-l" (concat "-t" file))))
		   (kill-buffer buf))))
             roots
             )
       (delete-file file)))))
(defun esn-comment-commit (&optional btn)
  "* Commit function on comment buffer."
  (interactive)
  ;; First get the files that are committed.
  (let (
	(files '())
	(roots '())
	(comment "")
	(pt nil)
        p1
	)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward esn-commit-message-end nil t)
	(while (re-search-forward "^\\* \\(.*\\)$" nil t)
	  (when (eq (get-text-property (- (point) 1) 'face) 'bold)
	    (add-to-list 'files (match-string 1))))))
    (mapc (lambda(x)
	    (let (
		  (fn x))
	      (when (string-match "^\\(.*[\////]\\)[^\////]*$" fn)
		(setq fn (match-string 1 fn))
		(add-to-list 'roots fn))))
	  files
	  )
    (save-excursion
      (goto-char (point-min))
      (when (search-forward esn-commit-message-start nil t)
	(skip-chars-forward "\t\n\f ")
	(setq p1 (point))
	(goto-char (point-max))
	(when (search-backward esn-commit-message-end nil t)
	  (skip-chars-backward "\t\n\f ")
	  (setq comment (buffer-substring p1 (point))))))
    (esn-add-to-vc roots files comment)
    (switch-to-buffer "*EsN: Commit*")
    (kill-buffer (current-buffer))
    (setq esn-commit-last-version nil)
    (delete-other-windows)
    (switch-to-buffer (get-file-buffer esn-vc-committing-buffer))
    (save-buffer)))
(defun esn-comment-cancel (&optional btn)
  "* Cancel function on comment buffer."
  (interactive))
(defun esn-comment-toggle (&optional btn)
  "* Toggles file in comment buffer."
  (esn-commit-toggle-file (point)))
(defun esn-commit-space (&optional pre)
  "* Magic space function for Commits."
  (interactive "p")
  (let (
	(do-it 't))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "\\* ")
	(setq do-it nil)))
    (unless do-it
      (save-excursion
	(unless (search-backward esn-commit-message-end nil t)
	  (setq do-it 't))))
    (if do-it
	(self-insert-command pre)
      (esn-commit-toggle-file (point)))))
(defun esn-commit-toggle-file (pos)
  "Toggle whether or not the file at POS will be committed."
  ;; Lifted from mercurial.
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((face (get-text-property pos 'face))
	  (inhibit-read-only t)
	  bol)
      (beginning-of-line)
      (setq bol (+ (point) 2))
      (end-of-line)
      (if (eq face 'bold)
	  (progn
	    (remove-text-properties bol (point) '(face nil))
	    (message "%s will not be committed"
		     (buffer-substring bol (point))))
	(add-text-properties bol (point) '(face bold))
	(message "%s will be committed"
		 (buffer-substring bol (point)))))))

(defvar esn-button-mode-map
  (let (
	(case-fold-search 't)
	(esn-button-mode-map (make-keymap)))
    ;;
    ;; Keys
    ;;
    (define-key esn-button-mode-map "\r" 'push-button)
    (define-key esn-button-mode-map [mouse-1] 'push-button)
    (define-key esn-button-mode-map [mouse-2] 'push-button)
    (define-key esn-button-mode-map [mouse-3] 'push-button)
    esn-button-mode-map)
  "Keymap for buttons in commit log")
(defvar esn-commit-mode-map
    (let (
	(case-fold-search 't)
	(esn-commit-mode-map (make-keymap)))
    ;;
    ;; Keys
    ;;
    (define-key esn-commit-mode-map " " 'esn-commit-space)
    (define-key esn-commit-mode-map "\C-c\C-c" 'esn-comment-commit)
    (define-key esn-commit-mode-map "\C-c\C-c" 'esn-comment-commit)
    (define-key esn-commit-mode-map "\C-ck" 'esn-comment-cancel)
    (define-key esn-commit-mode-map "\C-x\C-s" 'esn-comment-commit)
    esn-commit-mode-map)
  "Commit log key map.")



(defun esn-vc-mode ()
  "EsN Major mode for Commit Logs."
  (interactive)
  (use-local-map esn-commit-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table
	major-mode 'esn-vc-mode
	mode-name "EsN-Version")
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (run-hooks 'text-mode-hook 'esn-vc-mode-hook)
  (when esn-vc-auto-commit
    (esn-comment-commit)))

(defun esn-get-comment-commit (files)
  "Possible comment prompt"
  (let (
	(buf-name (format "*EsN: Commit*"))
	(pt nil)
	(txt nil)
	(comment "")
	(maxmod nil)
	(fn (buffer-file-name)))
    (mapc (lambda(x)
	    (when x
	      (let (
		    (mod (nth 5 (file-attributes x))))
		(when mod
		  (when (and maxmod (time-less-p maxmod mod))
		    (setq mod maxmod))
		  (unless maxmod
		    (setq maxmod mod))))))
	  files
	  )
    (when (string-match "[/\\\\]\\([^/\\\\]*$\\)" fn)
      (setq fn (match-string 1 fn)))
    (setq comment (format "Edited %s on %s.\nSaving previous results (%s)" fn
			   (format-time-string "%a %b %d %T %Z %Y")
			   (format-time-string "%a %b %d %T %Z %Y" maxmod)))
    (pop-to-buffer (get-buffer-create buf-name))
    (let ((inhibit-read-only 't))
    (when (= (point-min) (point-max))
      (setq pt (point))
      (insert "Commit")
      (make-button pt (point)
		   'action 'esn-comment-commit
		   'keymap esn-button-mode-map
		   )
      (insert " ")
      (setq pt (point))
      (insert "Cancel" )
      (make-button pt (point)
		   'action 'esn-comment-cancel
		   'keymap esn-button-mode-map
		   )
      (insert "\n\n")
      (setq pt (point))
      (insert esn-commit-message-start)
      (skip-chars-backward "\n")
      (add-text-properties pt (point) '(face bold-italic))
      (skip-chars-forward "\n")
      (insert "\n\n")
      (insert "\n")
      (setq pt (point))
      (insert esn-commit-message-end)
      (add-text-properties pt (point) '(face bold-italic))
      (setq pt (point))
      (setq txt (concat "* " (mapconcat (lambda(x) x) files "\n* ")))
      (while (string-match "* \n" txt)
	(setq txt (replace-match "" nil nil txt)))
      (insert txt)
      (add-text-properties pt (point) '(face bold))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\* \\(.*\\)$" nil t)
	  (make-button (- (point) (length (match-string 1))) (point)
			    'action 'esn-comment-toggle
			    'keymap esn-button-mode-map)))
      (insert "\n\n")
      (setq pt (point))
      (insert "Commit")
      (make-button pt (point)
		   'action 'esn-comment-commit
		   'keymap esn-button-mode-map
		   )
      (insert " ")
      (setq pt (point))
      (insert "Cancel" )
      (make-button pt (point)
		   'action 'esn-comment-cancel
		   'keymap esn-button-mode-map
		   )
      (insert "\n")
      (goto-char (point-min))
      (when (search-forward esn-commit-message-start)
	(add-text-properties (point-min) (point)
			   '(read-only t))
      (goto-char (point-max))
      (search-backward esn-commit-message-end)
      (add-text-properties (point) (point-max)
			   '(read-only t)))
      (backward-char 2)
      (insert comment))
    (esn-vc-mode))))
(defun esn-on-save-use-vc (&optional files)
  "Function that calls EsN's vc."
  (interactive)
  (when (and esn-commit-last-version  esn-use-version-control)
    (setq esn-vc-committing-buffer (buffer-file-name))
    ;; Don't do anything unless the file exists.
    (if (not (and (buffer-file-name) (file-exists-p (buffer-file-name))))
	(setq esn-commit-last-version nil)
      (when (not files)
	(let (
	      (files (append (append
			      (esn-get-input-files)
			      (esn-get-output-files 't))))
	      (files2 '()))
	  (mapc (lambda(x)
		  (when x
		    (when (and x (file-exists-p x))
		      (add-to-list 'files2 x))))
		files
		)
	  (add-to-list 'files2 (buffer-file-name))
	  (esn-on-save-use-vc files2)))
      (when files
	(esn-get-comment-commit files)
	(esn-error "Need to commit last version (%s;%s)" esn-vc-committing-buffer files)))))

(defvar esn-skip-read-only-check nil
  "* Defines a variable to skip the read-only and version-control check"
  )

(defun esn-should-be-new-version (&optional inputs outputs no-prompt)
  "Figures out if there should be a new version or not (based on timestamps.)"
  (interactive)
  (unless esn-skip-read-only-check
    (let* (
           (case-fold-search 't)
           (fn (buffer-file-name))
           (inp (or inputs (esn-get-input-files)))
           (out (or outputs (esn-get-output-files 't)))
           (fn-exist (if fn
                         (file-exists-p fn)
                       fn)) ;; Control stream exists.
           (inp-new nil)
           (out-new nil)
           (files (append inp out))
           (nv nil)
           (prompt (not no-prompt))
           (edit nil))
      (when (buffer-file-name)
        (mapc (lambda (file)
                (setq inp-new (or inp-new (if (and file (file-exists-p file))
                                              (file-newer-than-file-p file fn)
                                            nil
                                            ))))
              inp
              ) ;; Control stream newer than inputs.
        (mapc (lambda (file)
                (when file
                  (setq out-new (or out-new (if (and file (file-exists-p file))
                                                (file-newer-than-file-p file fn)
                                              nil)))))
              (esn-get-output-files))
        (if (not fn-exist)
            (message "New file %s" fn)
          (if inp-new
              (if (not prompt)
                  (progn
                    (setq esn-commit-last-version 't)
                    (message "Inputs changed"))
                (setq nv (yes-or-no-p "Inputs have been updated since this file was edited.  Edit anyway? "))
                (setq edit 't))
					;(ding)
                                        ;	  (message "Inputs are in the correct date/time sequence.")
            (if out-new
                (if (not prompt)
                    (progn
                      (setq esn-commit-last-version 't)
                      (message "Outputs updated"))
                  (if esn-prompt-to-edit-file-with-newer-outputs
                      (setq nv (yes-or-no-p "Output(s) have been created since this file was edited.  Edit anyway? "))
                    (setq nv 't))
                  (setq edit 't))
                                        ;	    (message "Outputs older than control stream.")
              )))
        (if nv
            (progn
              )
          (when edit
            (setq esn-buffer-should-be-read-only 't)
            (esn-edit)))))))

;;;###autoload
(defun esn-edit ()
  "* Defines the function that is used to start an editing of a repository."
  (interactive)
  (when (buffer-file-name)
    (when (file-exists-p (buffer-file-name))
      (when (and buffer-read-only (not esn-buffer-should-be-read-only))
	(toggle-read-only))
      (when (and (not buffer-read-only) esn-buffer-should-be-read-only)
	(toggle-read-only)))))

(provide 'esn-vc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-vc.el ends here
