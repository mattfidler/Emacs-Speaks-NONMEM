;;; esn-header-setup.el --- Header Setup
;; 
;; Filename: esn-header-setup.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Apr 22 12:18:27 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 12:24:14 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 8
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
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:18:31 2010 (-0500) #1 (Matthew L. Fidler)
;;    Added Error Handling
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
;; 
(declare-function esn-header-mode "esn-header-setup")
(declare-function esn-header-regexp-quote "esn-update")
(declare-function esn-error "esn-exec")

(defgroup esn-mode-header-setup nil
  "Options about header setup"
  :group 'esn-mode )

; Main Function is esn-header-setup ()
(defcustom esn-hs-cp-region-text
  "
Select Regions of header to be retained upon copy.

   Move to the line where you wish to have the region between
   retained upon copy.  When you have arrived at the location,
   press CTL-c CTL-c to continue.
"
  "Text to prompt for saved regions of the header."
  :group 'esn-mode-header
  :type 'string
  )
  
(defcustom esn-hs-editable-text
  "
Select Editable Regions of the header.

   Move to the line where you wish to have a user editable region.
   When you have arrived at the location,  press CTL-c CTL-c to continue.

"
  "Text to prompt for editable regions of the header."
  :group 'esn-mode-header
  :type 'string
  )

(defcustom esn-hs-version-information
  "

NONMEM versions

   This lists the versions that ESN recognizes.  The format is
   small version=Version Text Displayed.  Therefore,

          5=NONMEM VI Level 1.0

   implies that when prompted and the user enters 5, the version
   put into the header is NONMEM VI Level 1.0

   After done press CONTROL-c CONTROL-c, or use the menu to
   select:

   Setting-Up ESN Header ->  Save Version List


"
  "Text displayed for NONMEM version information prompt"
  :group 'esn-mode-header
  :type 'string
  )
(defcustom esn-hs-modification-types
  "
Types of Modification logs:

  (1) Add modification lines for every user who modified the file.

  (2) Add a modification line for each user and for the same user
      if the last modification was not on the same date.

  (3) Add a modficaiton line if the last user wasn't the current
      user.

  (4) Add a modification line if the last user wasn't the current
      user, or the last modification was not on the same day.

"
  "Modification Options text displayed when prompting"
  :type 'string
  :group 'esn-mode-header-setup
  )
(defcustom esn-hs-always-authors
  "
List of Authors that are always added:

   In the above list the authors that are always added.  Each
   author should be on a new line.  If there are no authors, the
   above buffer snould be blank.

   After done press CONTROL-c CONTROL-c, or use the menu to
   select:

   Setting-Up ESN Header ->  Save Authorship List
"
   "Authorship text"
   :type 'string
   :group 'esn-mode-heaer-setup
   )

(defcustom esn-hs-upd-var
  "
Location of %s Update:

  In the above buffer, move to the point where you wish %s to be
  update on file save.

  After done press CONTROL-c CONTROL-c, or use the menu to
  select:

  Setting-Up ESN Header ->  Set Location for %s
"
  "Initial Variables text"
  :type 'string
  :group 'esn-mode-header-setup
  )
(defcustom esn-hs-ini-var
  "
Location of %s Variable:

  In the above buffer, move to the point where you wish %s to be
  inserted on file creation, copy, or save as.

  After done press CONTROL-c CONTROL-c, or use the menu to
  select:

  Setting-Up ESN Header ->  Set Location for %s
"
    "Initial Variables text"
  :type 'string
  :group 'esn-mode-header-setup
  )
(defcustom esn-hs-step1
  "
Step 1:

  In the above buffer, insert your header.  Make sure that this
  header is NOT FILLED IN, otherwise this procedure may KEEP THE
  VALUES you filled in every time.

  After done press CONTROL-c CONTROL-c, or use the menu to
  select:

  Setting-Up ESN Header -> Finish Inserting Header.
"
  "Step 1 text."
  :type 'string
  :group 'esn-mode-header-setup
  )

(defvar esn-header-mode-hook nil)
(defun esn-header-mode--keymap ()
  (use-local-map esn-header-mode-map)
  (setq esn-header-viper-mode-map (make-sparse-keymap))
  (if (not (and (boundp 'viper-mode) viper-mode))
      (esn-define-viper-keys nil "esn-header")
    (esn-define-viper-keys "-viper" "esn-header")
    (message "Viper")
    (add-hook 'esn-header-mode-hook (lambda()
			       (viper-modify-major-mode 'esn-header-mode 'emacs-state esn-viper-mode-map)
			       (viper-modify-major-mode 'esn-header-mode 'insert-state esn-viper-mode-map)
			       (viper-change-state-to-vi)
			       ))
    )
  (make-variable-buffer-local 'esn-set-read-only)
  (setq esn-set-read-only nil)
  (make-variable-buffer-local 'esn-hide-uneditable-components)
  (setq esn-hide-uneditable-components nil)
  (make-variable-buffer-local 'esn-hide-the-header)
  (setq esn-hide-the-header nil)
  (make-variable-buffer-local 'esn-caps-tool)
  (setq esn-caps-tool nil)
  (make-variable-buffer-local 'esn-wrapping-of-records)
  (setq esn-wrapping-of-records nil)
  (make-variable-buffer-local 'esn-mode-auto-indent)
  (setq esn-mode-auto-indent nil)
  (make-variable-buffer-local 'esn-wfn-prefer-lc)
  (setq esn-wfn-prefer-lc nil)
  )

(defun esn-setup-header-mode (fn xfn name &optional initxt o)
  "* Setup Header mode
fn is the function to be called for next step (with saving progress).
name is the funciton name
xfn is the function to be called for the next step without saving.
o is other information in the menu (possible another function)
"
  (let (
	(tmp-key "")
	(tmp-menu "")
	)
  (with-temp-buffer
    (when xfn
      (setq tmp-key
	    (format
	     "(define-key esn-header-mode-map \"\\C-x\\C-k\" %s)
 (define-key esn-header-mode-map \"\\C-w\" %s)
"
	     xfn xfn))
      (setq tmp-menu
	    (format "\n[\"Exit without saving\" %s]" xfn))
      
      )
    (insert (format "
 (setq esn-header-mode-map
      (let (
	    (esn-header-mode-map (make-keymap))
	    )
	(define-key esn-header-mode-map \"\\C-c\\C-c\" %s)
	(define-key esn-header-mode-map \"\\C-x\\C-s\" %s)
	(define-key esn-header-mode-map \"\\C-s\" %s)
%s
	esn-header-mode-map)
      )
 (defun esn-header-menu-setup ()
  (setq esn-header-menu
	'(\"Setting Up ESN header\"
	  [\"%s\" %s]%s%s
	  ))
  (easy-menu-define esn-header-menu esn-header-mode-map \"ESN header setup menu\" esn-header-menu)
  (easy-menu-add esn-header-menu esn-header-mode-map)
  )
 (defun esn-header-mode () \"Major mode for setting up NONMEM header.\"
 (esn-mode--ini)
 (esn-mode--fontlock)
 (esn-header-mode--keymap)
 (esn-header-menu-setup)
  
 (message \"%s\")
 (setq major-mode 'esn-header-mode)
 (setq mode-name \"ESN-Header-Setup\")
 (run-mode-hooks 'esn-header-mode-hook)
) 
" fn fn fn
tmp-key
 name fn (if o
	     (concat "\n" o)
	   "") tmp-menu (or initxt "")))
  (eval-buffer)
))
)
(defun esn-edit-header-setup (txt fn xfn name &optional header ro initxt other)
  "TXT= Help text.
fn= save function run on exit.
xfn= just exit function.
name = Menu name
header = initial header contents
ro = read-only buffer.
other = Other menu items to add.
"
  (esn-setup-header-mode fn xfn name initxt other)
  (when (get-buffer "*esn-header-setup*")
    (kill-buffer (current-buffer)))
  (get-buffer-create "*esn-header-setup*")
  (when (get-buffer "*esn-header-setup-instructions*")
    (kill-buffer (current-buffer)))
  (get-buffer-create "*esn-header-setup-instructions*")
  (switch-to-buffer "*esn-header-setup*")
  (when header
    (insert header))
  (when (and ro (not buffer-read-only))
    (setq buffer-read-only 't)
    )
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (delete-other-windows)
  (switch-to-buffer-other-window "*esn-header-setup-instructions*")
  (insert txt)
  (when (not buffer-read-only)
    (set-buffer-modified-p nil)
    (setq buffer-read-only 't))
  (other-window 1)
  (esn-header-mode)
  )
(defun esn-stop-edit-header-setup (&optional bef justClose)
  "Stops editing the header.
When bef=\"txt\" insert txt in the header and return.
When bef=\"\", get list of before and after, respectively.
When bef=nil, return header contents.
When justClose=t, just close it.
"
  (when justClose
    (switch-to-buffer "*esn-header-setup*")
    (kill-buffer (current-buffer))
    (switch-to-buffer "*esn-header-setup-instructions*")
    (kill-buffer (current-buffer))
    )
  (unless justClose
  (let (
	(tmp nil)
	)
    (switch-to-buffer "*esn-header-setup*")
    (when buffer-read-only
      (setq buffer-read-only nil)
      )
    (when (and bef (not (string= bef "")))
      (if (re-search-backward " \\=" nil t)
	  (forward-char 1)
	(insert " ")
	)
      (insert bef)
      (when (looking-at "[^ ]")
	(insert " "))
      )
    (if (or (not bef) (and bef (not (string= bef ""))))
	(progn
	  (setq tmp (buffer-substring (point-min) (point-max)))
	  (while (string-match " +$" tmp)
	    (setq tmp (replace-match "" nil nil tmp)))
	  (while (string-match ":$" tmp)
	    (setq tmp (replace-match ": " nil nil tmp)))
	  )
      ;; Now get before and after stuff.
      (let (
	    (pt (point))
	    (bef "")
	    (aft "")
	    (bol nil)
	    )
	(save-excursion
	  (beginning-of-line)
	  (when (looking-at ".*")
	    (setq bef (match-string 0))
	    (when (string-match "^ *;+ *" bef)
	      (setq bef (replace-match "" nil nil bef)))
	    (when (string-match " *;" bef)
	      (setq bef (replace-match "" nil nil bef)))
	    (when (string-match "^ *" bef)
	      (setq bef (replace-match "" nil nil bef)))
	    (when (string-match " *$" bef)
	      (setq aft (replace-match "" nil nil bef)))
	    )
	  )
	(save-excursion
	  (end-of-line)
	  (if (eobp)
	      (error "There must be at least something on the line after.")
	    (forward-line 1)
	    (beginning-of-line)
	    (when (looking-at ".*")
	      (setq aft (match-string 0))
	      (when (string-match "^ *;+ *" aft)
		(setq aft (replace-match "" nil nil aft)))
	      (when (string-match " *;" aft)
		(setq aft (replace-match "" nil nil aft)))
	      (when (string-match " *$" aft)
		(setq aft (replace-match "" nil nil aft)))
	      (when (string-match "^ *" aft)
		(setq aft (replace-match "" nil nil aft)))
	      (when (string-match "^\\(-\\|[=_|;]\\)\\{4,\\}$" aft)
		(setq aft (substring (match-string 0 aft) 0 4))
		)
	      )
	    )
	  )
	(setq tmp (list bef aft))
	)
      )
    (kill-buffer (current-buffer))
    (switch-to-buffer "*esn-header-setup-instructions*")
    (kill-buffer (current-buffer))
    (symbol-value 'tmp)
    ))
  )
(defun esn-options-setup ()
  (interactive)
  (esn-header-setup 't)
  )
(defun esn-header-setup (&optional extraSetup)
  "* This function sets up a user header and template for ESN."
  (interactive)
  (let (
	(txt esn-header)
	(case-fold-search 't)
	)
    (when extraSetup
      (setq esn-completing-current-directory (esn-prompt "Way to specify current directory in control stream (if unsure leave blank): " ))
      (setq esn-use-version-control
	    (yes-or-no-p "Automatically place models in version control? "))
      (when esn-use-version-control
	(setq esn-vc-auto-commit
	      (yes-or-no-p "Use default descriptions for versions (instead of letting the user input a description)? ")))
      (setq esn-update-add-header-on-create
	    (yes-or-no-p "Add header for new file?" ))
      (if esn-update-add-header-on-create
	  (setq esn-update-add-header-if-not-present
	      (yes-or-no-p "Add header if not present in control stream? "))
	(setq esn-update-add-header-if-not-present nil)
	)

      )
    (when (or (not extraSetup) (and extraSetup esn-update-add-header-on-create))
      (when (not extraSetup)
	(setq esn-update-add-header-on-create 't)
	(setq esn-update-add-header-if-not-present 't)
	)
      (while (string-match "\\$\\(after-header\\|creation-date\\|initial-author\\|small-name\\)\\$" txt)
	(setq txt (replace-match "" nil nil txt))
	)
      (while (string-match " +$" txt)
	(setq txt (replace-match "" nil nil txt)))
      (while (string-match ":$" txt)
	(setq txt (replace-match ": " nil nil txt)))
      
      (while (string-match "\\$PRO[A-Z]+ *\n?" txt)
	(setq txt (replace-match "" nil nil txt)))
      ;; Set all status to not prompted
      (mapc (lambda(x)
	      (with-temp-buffer
		(insert (format "(setq %s nil)" x ))
		(eval-buffer)
		)
	      )
	    (list "esn-hs-authorship-already-prompted"
		  "esn-hs-date-prompted"
		  "esn-hs-date-time-prompted"
		  "esn-hs-authorship-already-prompted-2"
		  )
	    )
      (esn-edit-header-setup
       esn-hs-step1
       "(lambda() (interactive) (esn-header-setup2))"
       nil
       "Finish inserting Header"
       txt
       nil
       "Modify/Add Header Text"
       )
      )
    )
  )
(defvar esn-hs-authorship-already-prompted nil
  "* Defines if authorship was already defined."
  )
(defun esn-hs-authorship2 (&optional resume)
  "Save authorship."
  (let (
	(txt (esn-stop-edit-header-setup))
	(lst '())
	)
    (mapc (lambda(x)
	    (let (
		  (ret x)
		  )
	      (when (string-match "^ +" ret)
		(setq ret (replace-match "" nil nil ret))
		)
	      (when (string-match " +$" ret)
		(setq ret (replace-match "" nil nil ret))
		)
	      (unless (string= ret "")
		(add-to-list 'lst ret))
	      )
	    )
	  (split-string txt "\n")
	  )
    (setq esn-update-authorship-always-add lst)
    )
  ; Now resume editing process.
  (esn-header-setup-options resume)
  )
(defvar esn-hs-authorship-already-prompted-2 nil
  "* Defines if authorship was already defined."
  )
(defun esn-hs-authorship (&optional resume)
  "Update authorhsip "
  (unless esn-hs-authorship-already-prompted-2
    (esn-hs-authorship-p)
    (let (
	  (txt
	   (mapconcat  (lambda(x) x) esn-update-authorship-always-add "\n"))
	  (case-fold-search 't)
	  )
      (setq esn-hs-fn-wait 't)
      (esn-edit-header-setup
       esn-hs-always-authors
       (format "(lambda() (interactive) (esn-hs-authorship2 \"%s\"))" resume)
       nil
       "Save Always Added Authorship List"
       txt
       nil
       "Modify Always Added Authorship List"
       )
      )
    (setq esn-hs-authorship-already-prompted-2 't)
    )
  )
(defun esn-hs-authorship-p (&optional resume)
  "Update authorhsip "
  (unless esn-hs-authorship-already-prompted
    (setq esn-update-authorship-name-and-user
	  (yes-or-no-p "Author should be Full Name (user) instead of just Full Name? "))
    (setq esn-hs-authorship-already-prompted 't)
    )
  )
(defun esn-hs-creation-dt (&optional resume)
  "Function run on Creation Date/Time."
  (setq esn-update-creation-date-time (yes-or-no-p
				       "Would you like the initial creation string to be Date and Time instead of just the date?"))
  (if esn-update-creation-date-time
      (esn-hs-date-time)
    (esn-hs-date)
    )
  )
(defun esn-hs-last-save-dt (&optional resume)
  (setq esn-update-compelte-date-time
	(yes-or-no-p
	 "Would you like the last save to be Date and Time instead of just the date? "))
  )
(defvar esn-hs-date-prompted nil
  "Defines if the Date format has already been prompted for."
  )
(defun esn-hs-date (&optional resume)
  (unless esn-hs-date-prompted
    (setq esn-hs-date-prompted 't)
    )
  )
(defvar esn-hs-date-time-prompted nil
  "Defines if the Date-Time Format has already been prompted for."
  )
(defun esn-hs-date-time (&optional resume)
  (unless esn-hs-date-time-prompted
    (setq esn-hs-date-time-prompted 't)
    )
  )
(defun esn-hs-purpose (&optional resume)
  "Defines the purpose options."
  (setq esn-update-purpose-cp
	(yes-or-no-p "Update purpose to be a different if this file comes from a copy? "))
  )
(defun esn-hs-input-files (&optional resume)
  )
(defun esn-hs-output-files (&optional resume)
  )
(defun esn-hs-mod-log (&optional resume)
  "Defines the type of modification log."
  (when (get-buffer "*esn-header-setup*")
    (kill-buffer (current-buffer)))
  (get-buffer-create "*esn-header-setup*")
  (switch-to-buffer "*esn-header-setup*")
  (insert esn-hs-modification-types)
  (delete-other-windows)
  (let (
	(tmp (esn-prompt "Modification Type (1-4)? "
				      "^ *[1-4] *$"
				      "Modification Type (must be 1-4)? "
				      ))
	)
    (when (string-match "^ *" tmp)
      (setq tmp (replace-match "" nil nil tmp)))
    (when (string-match " *$" tmp)
      (setq tmp (replace-match "" nil nil tmp)))
    (setq esn-update-modification-type (string-to-number tmp))
    )
  (when (get-buffer "*esn-header-setup*")
    (kill-buffer (current-buffer)))
  (esn-hs-authorship-p)
  (setq esn-update-modification-log-date-time
	(yes-or-no-p
	"Use Date and Time for modification log? "))
  (setq esn-update-modification-add-reason
	(yes-or-no-p
	 "Whenever adding a modification log line, add a reason? "))
  (setq esn-update-modification-dt-every-time
	(yes-or-no-p
	 "On save, update the modfication date instead of updating the modification date when adding a new modification line? "))
  (setq esn-vc-upon-new-modificaiton-log-line
	(yes-or-no-p
	 "With a new modification line, create a new version of the file (using RCS)? "))
  )
(defun esn-hs-nmver2 (&optional resume)
  "Finishes saving the options for the version information."
  (let (
	(txt (esn-stop-edit-header-setup))
	)
    (setq esn-mode-nm-versions
	  (mapcar (lambda(x)
		    (mapcar (lambda(y)
			      (let ((ret y))
				(when (string-match "^ +" ret)
				  (setq ret (replace-match "" nil nil ret)))
				(when (string-match " +$" ret)
				  (setq ret (replace-match "" nil nil ret)))
				(symbol-value 'ret)
				)
			      )
			    (split-string x " *= *")
			    )
		    )
		  (split-string txt " *\n *")))
    )
    (setq esn-nm-version (esn-prompt "Default NONMEM version (if it doesn't match a short NONMEM version, ESN will prompt): " "[^ \t\n]"))
    (setq esn-update-cp-version (yes-or-no-p "Update version on copy? "))
    (esn-header-setup-options resume)
  )
(defun esn-hs-nmver (&optional resume)
  "This is the function that allows users to enter the version information."
  (let (
	(txt (mapconcat (lambda (x)
			  (if x
			    (mapconcat (lambda(y) y)
				       x
				       " = "
				       )
			    ""
			    )
			  
			  )
			esn-mode-nm-versions
			"\n"
			))
	)
    (setq esn-hs-fn-wait 't)
    (esn-edit-header-setup
     esn-hs-version-information
     (format "(lambda() (interactive) (esn-hs-nmver2 \"%s\"))" resume)
     nil
     "Save Version List"
     txt
     nil
     "Modify Version List"
     )
    )
  )

(defun esn-cp-save (&optional saveVar)
  "Set Editable regions of the header."
  (let (
	(tmp nil)
	(tmp-lst '())
	(continue 't)
	)
    (if (not saveVar)
	(progn
	  (setq continue (yes-or-no-p "Select a region to be retained upon file copy? "))
	  (setq esn-update-cp-save-terms nil)
	)
      (setq tmp-lst (esn-stop-edit-header-setup ""))
      (add-to-list 'esn-update-cp-save-terms tmp-lst)
      (message "%s" esn-update-editable-regions)
      (setq continue (yes-or-no-p "Select another region to be retained upon file copy? "))
      )
    (when continue
      (esn-edit-header-setup
       esn-hs-cp-region-text
       "(lambda() (interactive) (esn-cp-save 't))"
       "(lambda() (interactive) (esn-final-save))"
       "Set location for an editable region."
       esn-header
       't
       "Use above buffer to set the loaction for a saved region."
       )
      )
    (unless continue
      (when esn-update-purpose
	(add-to-list 'esn-update-cp-save-terms
		     (list esn-update-purpose-before
			   esn-update-purpose-after
			   )
		     )
	)
      (esn-final-save)
      )
    )
  )

(defun esn-hs-editable (&optional saveVar)
  "Set Editable regions of the header."
  (let (
	(tmp nil)
	(tmp-lst '())
	(continue 't)
	)
    (if (not saveVar)
	(progn
	  (setq continue (yes-or-no-p "Select an editable region of the header? "))
	  (setq esn-update-editable-regions nil)
	)
      (setq tmp-lst (esn-stop-edit-header-setup ""))
      (add-to-list 'esn-update-editable-regions tmp-lst)
      (message "%s" esn-update-editable-regions)
      (setq continue (yes-or-no-p "Add another editable region to the header? "))
      )
    (when continue
      (esn-edit-header-setup
       esn-hs-editable-text
       "(lambda() (interactive) (esn-hs-editable 't))"
       "(lambda() (interactive) (esn-cp-save))"
       "Set location for an editable region."
       esn-header
       't
       "Use above buffer to set the loaction for an editable region."
       )
      )
    (unless continue
      (when esn-update-purpose
	(add-to-list 'esn-update-editable-regions
		     (list esn-update-purpose-before
			   esn-update-purpose-after
			   )
		     )
	)
      (esn-cp-save)
      )
    )
  )
(defun esn-final-save ()
  "Defines the final function that is run before the final options are saved."
  (let (
	(tmp esn-header)
	)
    (while (string-match "[ \t\n]" (substring tmp 0 1))
      (setq tmp (substring tmp 1)))
    (while (string-match "[ \t\n]" (substring tmp -1))
      (setq tmp (substring tmp 0 -1)))
    (setq esn-header (concat "$PROBLEM $small-name$\n"
			     tmp "\n$after-header$"))
    )
  (esn-header-setup-save-values)
  )

(defun esn-hs-copyright (&optional resume)
  (setq esn-update-copyright-company
	(esn-prompt "Company Name/Institution for Copyrights: "))
  (setq esn-update-copyright-with-authorship
	(yes-or-no-p
	 "Add Authorship information to copyright (example (c) 1800 John Doe, EsN)? "	 ))
  (when esn-update-copyright-with-authorship
    (esn-hs-authorship resume)
    )
  )
(defcustom esn-hs-settable-options-list
   '(
     (1 "Initial Author" "esn-hs-authorship-p"
      "initial-author" "" "" "")
     (1 "Creation Date" "esn-hs-creation-dt"
	"creation-date" "" "" "")
     (2 "Purpose" "esn-hs-purpose"
       "esn-update-purpose-before" "esn-update-purpose-after"
       "esn-update-purpose")
     (2 "Model File Name" ""
      "esn-update-name-begin" "esn-update-name-end"
      "esn-update-file-name-long"
      )
     (2 "Input Files" "esn-hs-input-files"
      "esn-update-input-files-begin" "esn-update-input-files-end"
      "esn-update-input-files"
      )
     (2 "Output Files" "esn-hs-output-files"
      "esn-update-output-files-begin" "esn-update-output-files-end"
      "esn-update-output-files"
      )
     (2 "Modification Log" "esn-hs-mod-log"
       "esn-update-modification-begin" "esn-update-modification-end"
       "esn-update-modified-by"
       )
     (2 "Last Date Saved" "esn-hs-last-save-dt"
       "esn-update-save-date-before" "esn-update-save-date-after"
       "esn-update-complete-date"
       )
     (2 "Copyright Information" "esn-hs-copyright"
       "esn-update-copyright-begin" "esn-update-copyright-end"
       "esn-update-copyright"
       )
     (2 "Authorship Information" "esn-hs-authorship"
       "esn-update-author-begin" "esn-update-author-end"
       "esn-update-authorship"
       )
     (2 "NONMEM Version Information" "esn-hs-nmver"
	"esn-update-version-begin" "esn-update-version-after"
	"esn-update-version"
	)
     )
   "List and order of Header settable options."
   :type '(repeat
	   '(choice
	     (const :tag "Added on creation" 1)
	     (const :tag "Added on save" 2)
	     )
	   (string :tag "Prompt Name ")
	   (string :tag "Function of extra options, or empty string")
	   (string :tag "Before Variable OR location variable")
	   (string :tag "After Variable")
	   (string :tag "Variable to determine if this region is updated ")
	   )
   :group 'esn-mode-header-setup
   )
(defun esn-header-setup2 ()
  "Run after ESN has set up header."
  (setq esn-header (esn-stop-edit-header-setup))
  (esn-header-setup-options)
  )
(defvar esn-hs-fn-wait nil
  "*Defines if the function wishes to resume the esn-header-setup-options."
  )

(defun esn-header-setup-options (&optional resume no-update)
  "Setup options."
  (let (
	(cur 0)
	(i 0)
	(tmp-lst '())
	)
    (when resume
      (while (< i (length esn-hs-settable-options-list))
	(when (string= (nth 1 (nth i esn-hs-settable-options-list)) resume)
	  (setq cur i)
	  (setq i (length esn-hs-settable-options-list))
	  )
	(setq i (+ i 1))
	)
      ;; Don't update for esn-hs-fn-wait.
      (when (and no-update (not esn-hs-fn-wait))
	(esn-stop-edit-header-setup nil 't)
	;; Set option update to nil in case started, but stopeed it later.
	(when (= 2 (nth 0 (nth cur esn-hs-settable-options-list)))
	  (with-temp-buffer
	    (insert (format "(setq %s nil)"
			    (nth 5 (nth cur esn-hs-settable-options-list))))
	    (eval-buffer)
	    )
	  )
	)
      ;; Don't update for esn-hs-fn-wait.
      (unless (or no-update esn-hs-fn-wait)
	;; Save options on resume.
	(when (= (nth 0 (nth cur esn-hs-settable-options-list))
		 1)
	  ;; This is an after save, insert and exit.
	  (setq esn-header (esn-stop-edit-header-setup
			    (concat "$"
				    (nth 3 (nth cur esn-hs-settable-options-list)) "$")
			    ))
	  )
	(when (= (nth 0 (nth cur esn-hs-settable-options-list))
		 2)
	  ;; This is an insertion point save.
	  (setq tmp-lst (esn-stop-edit-header-setup ""))
	  (with-temp-buffer
	    (insert "(setq ")
	    (insert (nth 3 (nth cur esn-hs-settable-options-list)))
	    (insert "\"")
	    (insert (nth 0 tmp-lst))
	    (insert "\")\n(setq ")	  
	    (insert (nth 4 (nth cur esn-hs-settable-options-list)))
	    (insert "\"")
	    (insert (nth 1 tmp-lst))
	    (insert "\")\n")
	    (eval-buffer)
	    )
	  )
	)
      ;; Resume to next unless resuming from function, then resume current.
      (unless esn-hs-fn-wait
	(setq cur (+ cur 1))
	)
      )
    (if (< cur (length esn-hs-settable-options-list))
	(let (
	      (type (nth 0 (nth cur esn-hs-settable-options-list)))
	      (pn   (nth 1 (nth cur esn-hs-settable-options-list)))
	      (func (nth 2 (nth cur esn-hs-settable-options-list)))
	      (beg  (nth 3 (nth cur esn-hs-settable-options-list)))
	      (end  (nth 4 (nth cur esn-hs-settable-options-list)))
	      (var  (nth 5 (nth cur esn-hs-settable-options-list)))
	      (tmp-b nil)
	      )
 	  (when (= type 1)
	    (if (not (or esn-hs-fn-wait
		      (yes-or-no-p
		       (format
			"On creation/copy/save add %s to this header? "
			pn))))
		(esn-header-setup-options pn 't)
	      (if (not esn-hs-fn-wait)
		  (unless (string= func "")
		    (with-temp-buffer
		      (insert (format "(%s \"%s\")" func pn))
		      (eval-buffer)
		      )
		    )
		;; We have skipped the function since we are resuming.
		(setq esn-hs-fn-wait nil)
		)
	      (unless esn-hs-fn-wait
		;; Now edit header.
		(esn-edit-header-setup
		 (format esn-hs-ini-var pn pn pn)
		 (format
		  "(lambda() (interactive) (esn-header-setup-options \"%s\"))"
		  pn
		  )
		 (format
		  "(lambda() (interactive) (esn-header-setup-options \"%s\" 't))"
		  pn)
		 (format "Set Location for %s" pn)
		 esn-header
		 't
		 (format "Use above buffer to set location for %s" pn)
		 )
		)
	      )
	    )
	  (when (= type 2)
	    (if esn-hs-fn-wait
		(setq tmp-b 't) ;; Function called, therefore on-save is true.
	      (setq tmp-b (yes-or-no-p
			   (format "On save, update %s? " pn)
			   ))
	      (with-temp-buffer
		(insert "(setq ")
		(insert var)
		(insert " ")
		(if tmp-b
		    (insert "'t)")
		  (insert "nil)"))
		(eval-buffer)
		)
	      )
	    (if (not tmp-b)
		(esn-header-setup-options pn 't)
	      ;; Evaulate Function (as long as not already evaluating and we are
	      ;; resuming)
	      (if (not esn-hs-fn-wait)
		  (unless (string= func "")
		    (with-temp-buffer
		      (insert (format "(%s \"%s\")" func pn))
		      (eval-buffer)
		      )
		    )
		(setq esn-hs-fn-wait nil)
		)
	      ;; If the function set esn-hs-fn-wait, skip this, let the function
	      ;; resume.
	      (unless esn-hs-fn-wait
		;; Now Edit Header
		(esn-edit-header-setup
		 (format esn-hs-upd-var pn pn pn)
		 (format
		  "(lambda() (interactive) (esn-header-setup-options \"%s\"))"
		  pn
		  )
		 (format
		  "(lambda() (interactive) (esn-header-setup-options \"%s\" 't))"
		  pn
		  )
		 (format "Set Location for %s update upon save" pn)
		 esn-header
		 't
		 (format "Use above buffer to set location for %s update upon save" pn)
		 )
		)

	      )
	    )
	  )
      (esn-hs-editable)
      )
    )
  )
(defun esn-header-str-quote (txt)
  "String Quote."
  (if (not txt)
      ""
  (let (
	(var txt)
	)
    (while (string-match "^\\\\\\([^\\\\]\\)" var)
      (setq var (replace-match "\\\\\\\\\\1" nil nil var))
      )
    (while (string-match "\\([^\\\\]\\)\\\\$" var)
      (setq var (replace-match "\\1\\\\\\\\" nil nil var)))
    (while (string-match "\\([^\\\\]\\)\\\\\\([^\\]\\)" var)
      (setq var (replace-match "\\1\\\\\\\\\\2" nil nil var)))
    (while (string-match "\\([^\\\\]\\)\"" var)
      (setq var (replace-match "\\1\\\\\"" nil nil var)))
    (while (string-match "^\"" var)
      (setq var (replace-match "\\\\\"" nil nil var)))
    (symbol-value 'var)
    ))
  )
(defun esn-header-setup-save-values ()
  "* This function Save Values. 
Save the values to a variable file (doesn't allow customize)
Save the values to a customized file (allows customize)
Save the values to your personal customizations.
"
  (interactive)
  (save-restriction
  (let (
	(esn-h-options (concat esn-path "lisp/esn-options-header.el"))
	(options-buffer "")
	(var-bufer "")
	(tmp "")
	(h-reg (esn-header-regexp-quote))
	)
    (condition-case nil
        (save-excursion
          (goto-char (point-min))
          (re-search-forward h-reg nil 't)
          )
      (error
       (message "%s" h-reg)
       (esn-error "Header is setup inccorectly.  Will not save.")
       )
      )
    (setq esn-update-add-header-if-not-present 't)
    (setq esn-hide-the-header 't)
    (setq esn-set-read-only 't)
    (setq esn-update-header-init 't)
    (setq esn-update-redo-header-if-cp 't)
    (when (string-match "^ *\\(;+\\)" esn-header)
      (setq tmp (match-string 1 esn-header))
      (when (string-match ";+" esn-update-modification-line)
	(setq esn-update-modification-line
	      (replace-match tmp nil nil esn-update-modification-line))
	)
      (when (string-match ";+" esn-update-input-files-line)
	(setq esn-update-input-files-line
	      (replace-match tmp nil nil esn-update-input-files-line)))
      (when (string-match ";+" esn-update-output-files-line)
	(setq esn-update-output-files-line
	      (replace-match tmp nil nil esn-update-output-files-line)))
      )
    (save-window-excursion
      (find-file esn-h-options)
      (setq options-buffer (buffer-substring (point-min)
					     (point-max)))
      (kill-buffer (current-buffer))
      ;; Change all values to nil
      (with-temp-buffer
	(let (
	      (beg nil)
	      )
	  (insert options-buffer)
	  (goto-char (point-min))
	  (while (re-search-forward "(defcustom *[A-Z0-9-a-z_]*[ \t\n]*" nil t)
	    (if (not (looking-at "'?(" ))
		(if (looking-at "\"")
		    (progn
		      (setq beg (point))
		      (when (re-search-forward "[^\\\\]\"" nil t)
			(delete-region beg (point))
			(insert "nil"))
		      )
		  (when (looking-at "[^ \t\n]*")
		    (replace-match "nil")
		    )
		  )
	      (if (looking-at "'")
		  (delete-char 1))
	      (setq beg (point))
	      (forward-list 1)
	      (delete-region beg (point))
	      (insert "nil")
	      )
	    )
	  ;; Now add set options.
	  (mapc (lambda(x)
		  (let (
			(type (nth 0 x))
			(bol (nth 5 x))
			(befaft (list (nth 3 x)
				      (nth 4 x)
				      )
				)
			(bol-v nil)
			)
		    (when (= type 2)
		    (mapc (lambda(y)
			    (goto-char (point-min))
			    (let (
				  (var nil)
				  )
			      (with-temp-buffer
				(insert (format "(setq var %s)" y))
				(insert (format "(customize-save-variable '%s %s)" y y))
				(eval-buffer)
				)
			      (goto-char (point-min))
			      (if (re-search-forward
				   (format "\\<\\(%s\\)\\>[ \t\n]*nil" y)
						     nil 't)
				  (replace-match
				   (format "%s \"%s\""
					   (match-string 1)
					   (esn-header-str-quote var)
					   )))
			      )
			    )
			  befaft)
		    (with-temp-buffer
		      (insert (format "(setq bol-v %s)" bol))
		      (insert (format "(customize-save-variable '%s %s)" bol bol))
		      (eval-buffer)
		      )
		    (when bol-v
		      (goto-char (point-min))
		      (if (re-search-forward
			   (format "\\<\\(%s\\)\\>[ \t\n]+nil" bol)
			   nil 't)
			  (replace-match (format "%s 't" bol))
			)
		      )
		    ))
		  )
		esn-hs-settable-options-list
		)
	  ;; Now set all booleans not defined above.
	  (mapc (lambda(x)
		  (let (
			(tmp nil)
			)
		    (with-temp-buffer
		      (insert (format "(setq tmp %s)" x))
		      (insert (format "(customize-save-variable '%s %s)" x x))
		      (eval-buffer)
		      )
		    (when tmp
		      (goto-char (point-min))
		      (if (re-search-forward
			   (format "\\<\\(%s\\)\\>[ \t\n]+nil"
				   x)
			   nil 't)
			  (replace-match (format "%s 't" x))
			)
		      )
		    )
		  )
		(list
		 "esn-update-authorship-name-and-user"
		 "esn-update-creation-date-time"
		 "esn-update-purpose-cp"
		 "esn-update-modification-log-date-time"
		 "esn-update-modification-add-reason"
		 "esn-update-complete-date-time"
		 "esn-update-cp-version"
		 "esn-update-add-header-if-not-present"		 
		 "esn-update-copyright-with-authorship"
		 "esn-update-authorship-always-add-at-end"
		 "esn-hide-the-header"
		 "esn-update-redo-header-if-cp"
		 "esn-set-read-only"
		 "esn-update-header-init"
		 "esn-update-modification-dt-every-time"
		 "esn-update-add-header-on-create"
		 "esn-update-add-header-if-not-present"
		 "esn-use-version-control"
		 "esn-show-options"
		 "esn-vc-upon-new-modificaiton-log-line"
		 "esn-vc-auto-commit"
		 )
		)
	  ;; Update Strings not defined above
	  (mapc (lambda (x)
		  (let (
			(tmp nil)
			)
		    (with-temp-buffer
		      (insert (format "(setq tmp %s)" x))
		      (insert "(setq tmp (esn-header-str-quote tmp))")
		      (insert (format "(customize-save-variable '%s %s)" x x))
		      (eval-buffer)
		      )
		    (message "Looking for %s `%s'" x tmp)
		    (goto-char (point-min))
		    (if (re-search-forward
			 (format "\\<\\(%s\\)\\>[ \t\n]+nil"
				 (regexp-quote x)) nil 't)
			(replace-match
			 (format "%s \"%s\"" x tmp)
			 )
		      )
		    )
		  )
		(list
		 "esn-header"
		 "esn-update-copyright-company"
		 "esn-update-input-files-line"
		 "esn-update-output-files-line"
		 "esn-update-modification-line"
		 "esn-date-format"
		 "esn-date-time-format"
		 "esn-nm-version"
		 "esn-completing-current-directory"
		 )
		)
	  )
	;; Update Lists
	(mapc (lambda (x)
		(let (
		      (tmp nil)
		      (var "'(")
		      )
		  (with-temp-buffer
		    (insert
		     (format "(setq tmp %s)" x)
		     )
		    (insert (format "(customize-save-variable '%s %s)" x x))
		    (eval-buffer)
		    )
		  (mapc (lambda(y)
			  (setq var (concat
				     var
				     (format "\"%s\" "
					     (esn-header-str-quote y)
					     )
				     )
				)
			  )
			tmp
			)
		  (setq var (concat var ")"))
		  (goto-char (point-min))
		  (while (re-search-forward
			  (format "\\<\\(%s\\)\\>[ \t\n]+nil"
				  x)
			  nil t)
		    (replace-match (format "%s %s" x var))
		    )
		  )
		)
	      (list
	       "esn-update-authorship-always-add"
	       )
	      )
	;; Update Composite Lists
	(mapc (lambda(x)
		(let (
		      (tmp nil)
		      (var "")
		      )
		  (with-temp-buffer
		    (insert (format "(setq tmp %s)" x))
		    (insert (format "(customize-save-variable '%s %s)" x x))
		    (eval-buffer)
		    )
		  (setq var "'(")
		  (mapc (lambda(y)
			  (setq var (concat var "\n("))
			  (mapc (lambda(z)
				  (setq var (concat
					     var
					     (format "\"%s\" "
						     (esn-header-str-quote z)
						     )
					     )
					)

				  )
				y
				)
			  (setq var (concat var ")"))
			  )
			tmp
			)
		  (setq var (concat var "\n)"))
		  (goto-char (point-min))
		  (while (re-search-forward
			  (format "\\<\\(%s\\)\\>[ \t\n]+nil"
				  x)
			  nil t)
		    (replace-match (format "%s %s" x var))
		    )
		  )
		)
	      (list
	       "esn-update-cp-save-terms"
	       "esn-update-editable-regions"
	       "esn-mode-nm-versions"
	       )
	      )
	;; Last update numbered variables
	(mapc (lambda(x)
		(let ( (tmp nil))
		(with-temp-buffer
		  (insert (format "(setq tmp %s)" x))
		  (insert (format "(customize-save-variable '%s %s)" x x))
		  (eval-buffer)
		  )
		(goto-char (point-min))
		(while (re-search-forward
			(format "\\<\\(%s\\)\\>[ \t\n]+nil" x)
			nil t)
		  (replace-match (format "%s %s" x tmp))))
		)
	      (list
	       "esn-update-modification-type"
	       "esn-update-conserved-sequence-length"
	       )
	      )
	(setq options-buffer (buffer-substring (point-min) (point-max)))

	)
	
      ;; Change all defcustoms to defvar.  Remove defgroup.
      (let (
	    (esn-show-options nil)
	    )
      (with-temp-buffer
	(insert options-buffer)
	(goto-char (point-min))
	(let (
	      (beg nil)
	      (end nil)
	      )
	  (while (search-forward "(defcustom" nil 't)
	    (when (search-backward "(" nil 't)
	      (setq beg (point))
	      (forward-list 1)
	      (setq end (point))
	      (narrow-to-region beg end)
	      (goto-char (point-min))
	      (while (re-search-forward ":[A-za-z]\\(.*\\|\n\\)*)" nil t)
		(replace-match "")
		)
	      (goto-char (point-max))
	      (insert ")")
	      (goto-char (point-min))
	      (when (search-forward "defcustom" nil 't)
		(replace-match "defvar"))
	      (goto-char (point-max))
	      (widen)
	      )
	    )
	  (goto-char (point-min))
	  (while (search-forward "(defgroup" nil 't)
	    (when (search-backward "(" nil 't)
	      (setq beg (point))
	      (forward-list 1)
	      (delete-region beg (point))
	      )
	    )
	  )
	(goto-char (point-min))
	(when (re-search-forward "\\((defvar[ \t\n]+esn-show-options[ \t\n]+\\)[^ \t\n]*" nil t)
	  (replace-match "\\1nil")
	  )
	(setq var-bufer (buffer-substring (point-min) (point-max)))
	)
      ;; Now save values.
      (find-file "~/esn-options-header.el")
      (delete-region (point-min) (point-max))
      (insert var-bufer)
      (save-buffer)
      (find-file "~/esn-options-header-cust.el")
      (delete-region (point-min) (point-max))
      (insert options-buffer)
      (save-buffer)
      (kill-buffer (current-buffer))
      ;; Save customizations to ~/.emacs
      (customize-save-customized)
      )
    )
  )))
(provide 'esn-header-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-header-setup.el ends here
