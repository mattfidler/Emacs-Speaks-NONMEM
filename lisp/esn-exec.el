;;; esn-exec.el --- Execution Routines for Emacs Speaks NONMEM
;;
;; Filename: esn-exec.el
;; Description: Execution Routines for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Jan 26 15:15:11 2010 (-0600)
;; Version:
;; Last-Updated: Mon May  2 10:15:04 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 184
;; URL: http://esnm.sourceforge.net/
;; Keywords: Emacs Speaks NONMEM, Execution
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library includes the subroutines for executing external programs from
;; within emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 02-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May  2 10:07:45 2011 (-0500) #183 (Matthew L. Fidler)
;;    Remvoed `esn-mode-xpose-gen-summary-now'  Current doesn't do anything.
;; 02-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May  2 10:04:01 2011 (-0500) #181 (Matthew L. Fidler)
;;    Took out w32-short-file-name requirement.  
;; 10-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Dec  7 17:09:36 2010 (-0600) #170 (Matthew L. Fidler)
;;    Added autoload-cookies.c
;; 01-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Dec  1 13:35:41 2010 (-0600) #161 (Matthew L. Fidler)
;;    Changed Emacs Speaks NONMEM to EsN-mode on esn-messaging to make it work with growl for windows.
;; 29-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep 29 09:09:53 2010 (-0500) #137 (Matthew L. Fidler)
;;    Slight revision to esn-launch-xmind.
;; 28-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 28 16:06:51 2010 (-0500) #130 (Matthew L. Fidler)
;;    Made esn-alert, esn-message, and esn-error less prone to errors...
;; 28-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 28 15:42:09 2010 (-0500) #125 (Matthew L. Fidler)
;;    Added esn-alert
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 11:27:19 2010 (-0500) #120 (Matthew L. Fidler)
;;    Changed requirements for executing nmqual
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:20:41 2010 (-0500) #105 (Matthew L. Fidler)
;;    Added esn-message
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:03:18 2010 (-0500) #94 (Matthew L. Fidler)
;;    Added esn-error function to (optionally) interact with todochiku
;; 11-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Thu Feb 11 21:26:43 2010 (-0600) #86 (Matthew L. Fidler)
;;    Added kill buffer hook.
;; 11-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Thu Feb 11 11:30:59 2010 (-0600) #57 (Matthew L. Fidler)
;;    Require PLT generation before launch with w32
;; 09-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  9 21:13:26 2010 (-0600) #53 (Matthew L. Fidler)
;;    Changed PLT tools to support automation timer.
;; 08-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  8 17:11:46 2010 (-0600) #51 (Matthew L. Fidler)
;;    Added a "before start" function that should be called to update things like Xmind maps.
;; 02-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Tue Feb  2 14:50:55 2010 (-0600) #46 (Matthew L. Fidler)
;;
;;    Changed the way PLT tools submits jobs to make sure they use a backslash
;;    in windows.
;;
;; 29-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Fri Jan 29 16:50:03 2010 (-0600) #28 (Matthew L. Fidler)
;;    Try to look for Xmind executable if not specified correctly.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:45:06 2010 (-0600) #19 (Matthew L. Fidler)
;;    Added Launch of Xmind individual map.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:16:21 2010 (-0600) #5 (Matthew L. Fidler)
;;    Add Xmind support.
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

(declare-function esn-xmind-start-ctl-map "esn-xmind")
(declare-function esn-xmind-start-map "esn-xmind")
(declare-function esn-kill-buffer-hook "esn-mode")
(declare-function esn-xmind-default-map-name "esn-xmind")
(declare-function esn-dlgopen-browse-file "esn-advices")
(declare-function esn-psn-runname "esn-psn")
(declare-function esn-runname-noext "esn-properties")
(declare-function esn-plt-auto-now "esn-plt")
(declare-function esn-use-xmind-p "esn-which")
(declare-function esn-use-plt-p "esn-which")
(declare-function esn-use-xpose-p "esn-which")
(declare-function esn-command-to-string "esn-fun")



(require 'esn-start)
(require 'esn-options)
(require 'esn-vars)
(defun esn-before-exec ()
  "* Function to be run before executing..."
  (when (esn-use-xmind-p)
    (esn-xmind-start-ctl-map)
    (esn-xmind-start-map))
  (esn-kill-buffer-hook))

(defun esn-error (&rest args)
  "* Applies message, and optionally sends to todochiku"
  (condition-case err
      (if (featurep 'todochiku)
          (todochiku-message "EsN-mode"
                             (apply 'format args)
                             (todochiku-icon 'alert)))
    (error nil))
  (apply 'error args))

;;;###autoload
(defun esn-alert (&rest args)
  "* Esn Alert message"
  (condition-case err
      (if (featurep 'todochiku)
          (todochiku-message "EsN-mode"
                             (apply 'format args)
                             (todochiku-icon 'alert)))
    (error nil))
  (apply 'message args))

;;;###autoload
(defun esn-message (&rest args)
  "* Applies message, and possibly sends to todochiku"
  (condition-case error
      (progn
        (when (featurep 'todochiku)
          (todochiku-message "EsN-mode"
                             (apply 'format args)
                             (todochiku-icon 'emacs))))
    (error
     (message "`esn-message' error: %s" (error-message-string error))))
  (message (apply 'format args)))
(defun esn-xmind-cmd (xmind &optional individual)
  "Runs xmind command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t)
                     )
                   (buffer-substring (point-min) (point-max))
                   ))
  (get-buffer-create "*esn-xmind*")
  (save-window-excursion
    (set-buffer "*esn-xmind*")
    (insert "================================================================================\n")
    (insert xmind)
    (insert " ")
    (insert (esn-xmind-default-map-name))
    (insert "\n")
    (insert "\n================================================================================\n"))
  (start-process-shell-command "*esn-xmind*"
                               "*esn-xmind*"
                               (concat 
                                xmind
                                " "
                                (if individual
                                    (let ((f (buffer-file-name)))
                                      (when  (string-match "[.][^.]*$" f)
                                        (setq f (replace-match "" 't 't f)))
                                      (setq f (concat f ".xmind"))
                                      (symbol-value 'f)
                                      )
                                  (esn-xmind-default-map-name))))
  )

;;;###autoload
(defun esn-xmind-ctl ()
  "Starts Xmind control file summary."
  (interactive)
  (esn-xmind-start-ctl-map)
  (let ((xmind esn-xmind-exe))
    (if (not (and xmind (file-exists-p xmind)))
        (progn
          (if esn-w32
              (progn
                (message "Find Xmind executable")
                (setq esn-xmind-exe (esn-dlgopen-browse-file "Xmind Executable|xmind.exe"))
                (if (not (file-exists-p esn-xmind-exe))
                    (progn
                      (esn-error "Xmind is not specified, and cannot be loaded.")
                      )
                  (customize-save-variable 'esn-xmind-exe esn-xmind-exe)
                  (customize-save-customized)
                  (esn-xmind-cmd xmind 't)
                  )
                )
            (progn
              (esn-error "Xmind is not specified and cannot be loaded.")
              )
            )
          )
      (esn-xmind-cmd xmind 't)
      )
    )
  )
;;;###autoload
(defun esn-xmind-project ()
  "Starts Xmind project."
  (interactive)
  (esn-xmind-start-map)
  (let ((xmind esn-xmind-exe))
    (if (not (and (not (string= "" xmind)) (file-exists-p xmind)))
        (if esn-w32
            (progn
              (message "Find Xmind executable")
              (setq esn-xmind-exe (esn-dlgopen-browse-file "Xmind Executable|xmind.exe"))
              (if (not (file-exists-p esn-xmind-exe))
                  (esn-error "Xmind is not specified, and cannot be loaded.")
                (customize-save-variable 'esn-xmind-exe esn-xmind-exe)
                (customize-save-customized)
                (esn-xmind-cmd esn-xmind-exe)))
          (esn-error "Xmind is not specified and cannot be loaded."))
      (esn-xmind-cmd xmind))))

;; Setup g77 environmental variables on windows, if not already setup.
(when esn-w32
  (when esn-exec-g77-base
    (unless (string-match "\\([/\\\\][gG]77[/\\\\]bin\\|[Gg]77[Pp]]ortable[/\\\\][Aa]pp[/\\\\]bin\\)" (getenv "path"))
      (setenv "PATH" (concat esn-exec-g77-base "\\bin;" (getenv "PATH")))
      (setenv "LIBRARY_PATH" (concat esn-exec-g77-base "\\lib;" (getenv "LIBRARY_PATH"))))))

;;;###autoload
(defun esn-nmfe-cmd (nmfe &rest ARGS)
  "Runs nmfe6 command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t))
                   (buffer-substring (point-min) (point-max))))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
        (save-buffer)
      (esn-error "Buffer must be saved to run NONMEM.")))
  (get-buffer-create "*esn-nm*")
  (switch-to-buffer-other-window "*esn-nm*")
  (insert "================================================================================\n")
  (insert nmfe)
  (insert " ")
  (insert (mapconcat (lambda(x)
                       x
                       ) ARGS " "))
  (insert "\n")
  (insert "\n================================================================================\n")
  (apply 'start-process-shell-command "*esn-nm*"
         "*esn-nm*"
         nmfe
         ARGS)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'esn-finish-with-R))
(defun esn-nmqual-cmd (nmqual &rest ARGS)
  "Runs NMQual command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t)
                     )
                   (buffer-substring (point-min) (point-max))
                   ))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
        (save-buffer)
      (esn-error "Buffer must be saved to run NmQual.")))
  (get-buffer-create "*esn-nmqual*")
  (switch-to-buffer-other-window "*esn-nmqual*")
  (insert "================================================================================\n")
  (insert "perl ")
  (insert nmqual)
  (insert " ")
  (insert (mapconcat (lambda(x)
                       x
                       ) ARGS " "))
  (insert "\n")
  (insert "\n================================================================================\n")
  (apply 'start-process-shell-command "*esn-nmqual*"
         "*esn-nmqual*"
         "perl"
         (append (list nmqual) ARGS))
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'esn-finish-with-R))
;;;###autoload
(defun esn-nmqual-submit ()
  "Submits run to WFN to run."
  (interactive)
  (esn-before-exec)
  (let ((run (esn-psn-runname))
        (runlst (concat (esn-runname-noext) esn-nonmem-default-output-extension))
        (nmqual esn-exec-nmqual))
    (esn-nmqual-cmd nmqual run runlst)))

;;;###autoload
(defun esn-plt-submit ()
  "Submits run to PLT Tools to run."
  (interactive)
  (esn-before-exec)
  (if (not (esn-use-plt-p))
      (esn-error "PLT tools is not supported for this control stream")
    (if (and esn-w32 esn-plt-win-open (fboundp 'w32-browser))
        (progn
          (w32-browser (if (fboundp 'w32-short-file-name)
			   (w32-short-file-name (esn-plt-auto-now 't))
			 (esn-plt-auto-now 't))))
      (if (not (esn-use-plt-p))
          (esn-error "Cannot run PLT tools because:\n\t(1) PLT is not installed\n\t(2) PLT tools is not enabled, or \n\t(3) Control stream is not in a directory containing WORK\n")
        (let ((plta (esn-plt-auto-now 't))
              (dir esn-exec-plt)
              (opath (getenv "PATH"))
              (cmd (if (and esn-exec-plt esn-w32)
                       (if (fboundp 'w32-short-file-name)
			   (w32-short-file-name esn-exec-plt)
			 esn-exec-plt)
                     esn-exec-plt)))
          (setq plta (file-truename plta))
          (when esn-w32
            (setq plta (if (fboundp 'w32-short-file-name)
			   (w32-short-file-name plta)
			 plta))
            (while (string-match "/" plta)
              (setq plta (replace-match "\\" nil 't plta)))
            (when cmd
              (while (string-match "/" cmd)
                (setq cmd (replace-match "\\" nil 't cmd))))
            )
          (progn
            (progn
              (when (and esn-exec-plt (file-exists-p esn-exec-plt))
                (setenv "PATH" (mapconcat (lambda(x)
                                            ;; Take out esn-exec directory.
                                            (let (
                                                  (ret "")
                                                  )
                                              (when esn-w32
                                                (setq ret (if (fboundp 'w32-short-file-name)
							      (w32-short-file-name x)
							    x))
                                                (while (string-match "/" x)
                                                  (setq x (replace-match "\\" nil 't))
                                                  )
                                                
                                                )
                                              )
                                            )
                                          (split-string (getenv "PATH") "[;:]+" 't)
                                          (if esn-w32 ";" ":")
                                          ))
                ;; Add PLT tools utility directory.
                (when (buffer-modified-p)
                  (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
                      (save-buffer)
                    (esn-error "Buffer must be saved to run NONMEM.")))
                (get-buffer-create "*esn-plt-tools*")
                (when (string-match "[/\\\\][^/\\\\]*$" dir)
                  (setq dir (replace-match "" 't 't dir)))
                (switch-to-buffer-other-window "*esn-plt-tools*")
                (setq default-directory dir)
                (insert "================================================================================\n")
                (insert cmd)
                (insert " ")
                (insert plta)
                (insert "\n")
                (insert "\n================================================================================\n")
                (start-process-shell-command "*esn-plt-tools*"
                                             "*esn-plt-tools*"
                                             (concat
                                              cmd
                                              " "
                                              plta)))))
          (setenv "PATH" opath))))))

;;;###autoload
(defun esn-nm-submit ()
  "Submits run to nmfe to run."
  (interactive)
  (esn-before-exec)
  (if esn-current-run
      (esn-error "Already running %s" esn-current-run)
    (setq esn-current-run (esn-runname-noext))
    (let ((run (esn-psn-runname))
          (runlst (concat (esn-runname-noext) esn-nonmem-default-output-extension))
          (nmfe esn-exec-nm))
      (esn-clean-nm-files (esn-runname-noext))
      ;; (when (and (not (file-exists-p (concat (esn-runname-noext) ".R"))) (esn-use-xpose-p) esn-mode-xpose-summary-document)
      ;;   (esn-mode-xpose-gen-summary-now))
      (esn-nmfe-cmd nmfe run runlst))))

;;;###autoload
(defun esn-finish-with-R (process event &optional buffer)
  "* Finishes the submission with R -- requires Ess."
  (interactive)
  ;;  (message "%s, %s" process event)
  (let ((ess-installed nil)
        (buf (or buffer (buffer-name (current-buffer)))))
    (esn-move-nm-files esn-current-run)
    (if (not  (boundp 'inferior-R-program-name))
        (progn
          (setq esn-current-run nil)
          (insert "================================================================================\n")
          (insert "Done\n")
          (insert "================================================================================\n"))
      (let ((R (executable-find inferior-R-program-name))
            (rscript (concat esn-current-run ".R"))
            (rout (concat esn-current-run ".Rout"))
            (tail (executable-find "tail")))
        (if (not (and rscript (file-exists-p rscript)))
            (progn
              (setq esn-current-run nil)
              (insert "================================================================================\n")
              (insert "Done\n")
              (insert "================================================================================\n"))
          (unless R
            (setq R (ess-find-newest-R)))
          (if (not R)
              (progn
                (message "Sorry, R could not be found on your system.")
                (setq esn-current-run nil))
            (when (string-match "Rterm" R)
              (setq R (replace-match "R" nil nil R)))
            ;; Now execute R.
            (insert "================================================================================\n")
            (insert (format "%s CMD BATCH %s\n" R rscript))
            (insert "================================================================================\n")
            (set-process-sentinel
             (apply 'start-process-shell-command buf
                    buf
                    R
                    (list "CMD" "BATCH" rscript))
             #'esn-R-finished)
            (when tail
              (apply 'start-process-shell-command buf buf
                     "tail"
                     (list "--follow=name"
                           "--retry"
                           rout)))))))))

;;;###autoload
(defun esn-rm-file (file)
  "Removes file if exists"
  (when (and file (file-exists-p file))
    (delete-file file)))

;;;###autoload
(defun esn-clean-nm-files (run)
  "Cleans NONMEM files before submission"
  (esn-rm-file (concat run ".FMSG"))
  (esn-rm-file (concat run ".FMSG.gz"))
  (esn-rm-file (concat run ".PRDERR"))
  (esn-rm-file (concat run ".PRDERR.gz"))
  (esn-rm-file (concat run ".Rout"))
  (esn-rm-file (concat run ".Rout.gz"))
  (esn-rm-file (concat run ".doc"))
  (esn-rm-file (concat run ".rtf"))
  (esn-rm-file (concat run ".rtf.gz"))
  (esn-rm-file (concat run ".tex"))
  (esn-rm-file (concat run ".ptex"))
  )
(defun esn-move-nm-files (run)
  "Clean Up NONMEM files after submission."
  (let ((gzip (executable-find "gzip")))
    (when esn-w32
      (setq gzip (if (fboundp 'w32-short-file-name)
		     (w32-short-file-name gzip)
		   gzip)))
    (mapc (lambda(x)
            (esn-rm-file x))
          (list
           "FCON"
           "FCON2"
           "FDATA"
           "FREPORT"
           "FSUBS"
           "fsubs.for"
           "FSTREAM"
           "nonmem.exe"
           "G77COMPILE.BAT"
           (concat run ".tex")
           (concat run ".ptex")))
    (mapc (lambda(x)
            (when (and x (file-exists-p x))
              (rename-file x (concat run "." x) t)
              (when gzip
                (insert (esn-command-to-string (concat gzip " -v -9 "  run "." x))))))
          (list
           "FMSG"
           "PRDERR"))))
;;;###autoload
(defun esn-R-finished (process event)
  "Let user know that R process is completed."
  (let ((proc (get-buffer-process (current-buffer)))
        (run esn-current-run)
        (gzip (executable-find "gzip")))
    (when esn-w32
      (setq gzip (if (fboundp 'w32-short-file-name)
		     (w32-short-file-name gzip)
		   gzip)))
    ;; Delete remaining processes.
    (sleep-for 2)
    (condition-case nil
        (while (not (string-match "no process" (format "%s" proc)))
          (delete-process proc)
          (setq proc (get-buffer-process (current-buffer))))
      (error nil))
    (when (and gzip (and run (file-exists-p (concat run ".Rout"))))
      (insert (esn-command-to-string (concat gzip " -v -9 "  run ".Rout"))))
    (esn-move-nm-files esn-current-run)
    (insert "================================================================================\n")
    (insert "Done\n")
    (insert "================================================================================\n")
    (setq esn-current-run nil)))

(provide 'esn-exec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-exec.el ends here
