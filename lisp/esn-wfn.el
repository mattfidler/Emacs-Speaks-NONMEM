;;; esn-wfn.el --- Wings for NONMEM support
;; 
;; Filename: esn-wfn.el
;; Description: Wings for NONMEM support
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Feb  1 09:16:37 2010 (-0600)
;; Version:  0.1
;; Last-Updated: Mon May  2 14:59:30 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 115
;; URL: http://esnm.sourceforge.net
;; Keywords: Wings for NONMEM, Emacs Speaks NONMEM
;; Compatibility: 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Wings for nonmem and extended control stream support
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 16:08:44 2010 (-0600) #111 (Matthew L. Fidler)
;;    Moved `esn-wfn-runname' to esn-properties. 
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 16:04:58 2010 (-0600) #108 (Matthew L. Fidler)
;;    Moved extended control stream support to `esn-extended.el'
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 13:08:51 2010 (-0600)x #106 (Matthew L. Fidler)
;;    Moved `esn-mode-wfn-installed' to `esn-which.el'
;; 09-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  9 13:58:57 2010 (-0500) #101 (Matthew L. Fidler)
;;    Bug-fix for abbreviated records.
;; 09-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  9 12:21:56 2010 (-0500) #98 (Matthew L. Fidler)
;;    Changed method to check if Wings for NONMEM is installed.
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 09:42:59 2010 (-0500) #96 (Matthew L. Fidler)
;;    Took out dependence on esn-wfn-model-records and used abbreviated records instead
;; 02-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  2 13:05:13 2010 (-0500) #71 (Matthew L. Fidler)
;;    Bug fix for get variable names.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 11:50:59 2010 (-0500) #23 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 09-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug  9 10:39:53 2010 (-0500) #20 (Matthew L. Fidler)
;;    Removed font-lock information from file and moved to esn-fontlock.el
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:10:26 2010 (-0500) #16 (Matthew L. Fidler)
;;    Added EsN Error handling
;; 01-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  1 10:08:19 2010 (-0600) #11 (Matthew L. Fidler)
;;    Now highlights when in a comment...
;; 01-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  1 09:28:36 2010 (-0600) #7 (Matthew L. Fidler)
;;    Extended Highlighting only when THETA, OMEGA and SIGMA are in the control
;;    stream.
;; 01-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  1 09:16:54 2010 (-0600) #1 (Matthew L. Fidler)
;;    Make Extended highlighting only occur when you are not in a comment.
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
(declare-function esn-xpose-get-input-line "esn-xpose")

(defun esn-wfn-cmd (cmd &rest ARGS)
  "Runs a Wings for NONMEM command."
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
        (save-buffer)
      (esn-error "Buffer must be saved to run Wings For NONMEM.")))
  (unless (and esn-mode-wfn-bat (file-exists-p esn-mode-wfn-bat))
    (esn-error "Wings for NONMEM batch file does not exist, or ESN does not know where it is located."))
  ;; Save Version before submitting.
  ;;  (esn-add-new-version)
  ;; Need to get the batch file and edit it.
  (let ((fn "")
        (current-directory (buffer-file-name))
        (run (esn-runname-noext))
        (batch ""))
    (when (string-match "\\([\\\\/]\\)\\([^\\\\/]*\\)" current-directory)
      (setq current-directory (replace-match "\\1" nil nil current-directory)))
    (find-file esn-mode-wfn-bat)
    (setq batch (buffer-substring (point-min) (point-max)))
    (kill-buffer (current-buffer))
    (find-file (format "%s-%s.bat" run cmd))
    (delete-region (point-min) (point-max))
    (insert batch)
    (insert "\n")
    (insert cmd)
    (insert " ")
    (insert (mapconcat (lambda(x)
                         x
                         ) ARGS " "))
    (insert "\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (get-buffer-create "*esn-wfn*")
    (switch-to-buffer-other-window "*esn-wfn*")
    (insert "================================================================================\n")
    (insert cmd)
    (insert " ")
    (insert (mapconcat (lambda(x)
                         x
                         ) ARGS " "))
    (insert "\n")
    (insert "BATCH FILE at ")
    (insert (format "%s-%s.bat" run cmd))
    (insert "\n================================================================================\n")
    (start-process-shell-command "*esn-wfn*" "*esn-wfn*"
                                 (format "%s-%s.bat" run cmd))))

(defun esn-wfn-submit ()
  "Submits run to WFN to run."
  (interactive)
  (let ((run (esn-runname-noext)))
    (esn-wfn-cmd esn-mode-wfn-nmgo run)))

(defun esn-wfn-bs ()
  "Bootstrapps run with Wings for NONMEM"
  (interactive)
  (let ((run (esn-runname-noext))
        (bs 0))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
          (save-buffer)
        (esn-error "Buffer must be saved to run Wings For NONMEM.")))
    (setq bs (esn-prompt "Number of Bootstrap Replications: "
                         "^[0-9]+$"
                         "Number of Bootstrap Repliactions (Must be an integer): "
                         ))
    (esn-wfn-cmd esn-mode-wfn-nmbs run bs)))

(defun esn-wfn-rt ()
  "Submits control stream for randomization test."
  (interactive)
  ;; Need COVARIATE in dataset (need input line)
  ;; Need Number of runs.
  (let ((run (esn-runname-noext))
        (var "")
        (bs 0)
        (inp (esn-xpose-get-input-line)))
    (if (not (and (esn-rec "INP") (esn-rec "DAT")))
        (esn-error "Randomization test requires an $INPUT and $DATA record.")
      (when (buffer-modified-p)
        (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
            (save-buffer)
          (esn-error "Buffer must be saved to run Wings For NONMEM.")))
      (setq var (downcase (esn-prompt "Covariate to randomize: "
                                      inp
                                      "Covariate to randomize (Variable must be in $INPUT): ")))
      (setq bs (esn-prompt "Number of Bootstrap Replications: "
                           "^[0-9]+$"
                           "Number of Bootstrap Repliactions (Must be an integer): "))
      (esn-wfn-cmd esn-mode-wfn-nmrt var run bs))))

(provide 'esn-wfn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-wfn.el ends here
