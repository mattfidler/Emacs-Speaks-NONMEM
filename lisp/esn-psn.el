;;; esn-psn.el --- Perl Speaks NONMEM integration
;; 
;; Filename: esn-psn.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Apr 22 12:16:12 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 13:11:02 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 12
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
;; 26-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 26 14:43:31 2010 (-0500) #6 (Matthew L. Fidler)
;;    Bug fix for checking PsN 
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
(declare-function esn-xpose-get-all-variables "esn-xpose")
(declare-function esn-get-variable-names "esn-extended")
(require 'esn-remote)

;; Prompt for Stratify on and samples

(defun esn-psn-cmd (cmd &rest ARGS)
  "Runs a Wings for NONMEM command."
  (let ((buf (current-buffer)))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
          (save-buffer)
        (esn-error "Buffer must be saved to run Perl speaks NONMEM.")))
    ;; Add to RCS before command.
    ;;  (esn-add-new-version)
    (get-buffer-create "*esn-psn*")
    (switch-to-buffer-other-window "*esn-psn*")
    (insert "================================================================================\n")
    (insert cmd)
    (insert " ")
    (insert (mapconcat (lambda(x) x) ARGS " "))
    (insert "\n================================================================================\n")
    (if (save-excursion
          (set-buffer buf)
          (esn-is-remote-p))
        (progn
          (insert "(Remote)\n")
          (save-excursion
            (set-buffer buf)
            (esn-remote-command (concat cmd " "
                                        (mapconcat (lambda(x) x) ARGS " ")))))
      (apply 'start-process-shell-command "*esn-psn*"
             "*esn-psn*"
             cmd ARGS))))

(defun esn-psn-sumo ()
  "Summarizing of NONMEM Run"
  (interactive)
  (let ((nmo (esn-get-nonmem-output)))
    (if (not nmo)
        (esn-error "This utility requires a successful NONMEM run with an output file.")
      (when (buffer-modified-p)
        (esn-error "Buffer must be saved AND successfully submitted to NONMEM.")))
    (let ((run (esn-psn-runname)))
      (esn-psn-cmd esn-mode-psn-sumo-command run))))

(defun esn-psn-npc ()
  "Performs Numerical Predicitve Check (NPC)"
  (interactive)
  (esn-psn-vpc 't))

(defun esn-psn-vpc (&optional npc)
  "Performs Visual Predicitve Check (VPC)"
  (interactive)
  (let ((nmo (esn-get-nonmem-output))
        (msfo nil)
        (msfo-lst '())
        (msfo-reg nil)
        (tmp-lst '())
        (case-fold-search 't)
        (args '())
        (samples 0)
        (stratify "")
        (inp (esn-xpose-get-input-line))
        (var (esn-xpose-get-all-variables))
        (dv "")
        (idv "")
        (bin-type "")
        (ob (current-buffer))
        (run (esn-psn-runname))
        (tmp ""))
    (if (not nmo)
        (esn-error "This utility requires a successful NONMEM run with an output file.")
      (when (buffer-modified-p)
        (esn-error "Buffer must be saved AND successfully submitted to NONMEM.")))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\<\\$NON[A-Z]+[ \t\n]*" nil t)
        ;; MSFO required when there is NONPARAMETRIC estimation.
        (goto-char (point-min))
        (while (re-search-forward "\\<MSFO *[= \t] *\\([^ \t\n]\\)" nil t)
          (add-to-list 'msfo-lst (match-string 1)))
        ;; Make sure that the MSFO files exist.
        (mapc (lambda (x)
                (let ((ret nil))
                  (when (and x (file-exists-p (esn-get-abs-dir x)))
                    (when (string-match "[/\\\\]\\(^[/\\\\]*\\)$" x)
                      (setq ret (match-string 1 x))
                      (add-to-list 'tmp-lst ret)))))
              msfo-lst)
        (when (= 0 (length tmp-lst))
          (esn-error "$NONPARAMETRIC estimation, but there are no MSFO output files.  Therefore NPC or VPC cannot be run."))
        (if (> 1 (length tmp-lst))
            (setq msfo-reg (regexp-opt tmp-lst 't))
          ;; Only 1 MSFO present, use it.
          (setq msfo (nth 0 tmp-lst)))))
    (setq samples (esn-prompt "Number of  Replications: "
                              "^[0-9]+$"
                              "Number of Replications (Must be an integer >=20): "))
    (while (< 20 (string-to-number samples))
      (ding)
      (setq samples (esn-prompt "Number of  Replications (Must be an integer >= 20): "
                                "^[0-9]+$"
                                "Number of Replications (Must be an integer >=20): ")))
    (add-to-list 'args (format "--samples=%s" samples))
    (unless (or msfo msfo-reg)
      ;; Choose between initial and final estimates.
      (when (string-match "[/\\\\]\\([^/\\\\]*$\\)" nmo)
        (setq nmo (match-string 1 nmo)))
      (when (y-or-n-p "Use final parameter estimates? ")
        ;; Use the listing file to get final estimates.
        (add-to-list 'args (format "--lst=%s" nmo))))
    (when msfo-reg
      (setq msfo (esn-prompt "MSFO file used: "
                             msfo-reg
                             "MSFO file used (must be used)")))
    (when msfo
      (add-to-list 'args (format "--msfo=%s" msfo)))
    
    (setq stratify (esn-prompt "Statify on (STRT, PRED, variable on input line, or nothing; Default Nothing): "
                               (format "\\(?:%s\\|STRT\\|PRED\\)" inp)
                               "Invalid input, statify on (STRT, PRED, variable on input line, or nothing; Default Nothing): "
                               ""))
    (while (string= "dv" (downcase stratify))
      (ding)
      (setq stratify (esn-prompt "Can't be DV; Statify on (STRT, PRED, variable on input line, or nothing; Default Nothing): "
                                 (format "\\(?:%s\\|STRT\\|PRED\\)" inp)
                                 "Invalid input, statify on (STRT, PRED, variable on input line, or nothing; Default Nothing): "
                                 ""))
      )
    (unless (string= stratify "")
      (add-to-list 'args (format "--stratify_on=%s" (upcase stratify))))
    (setq dv (esn-prompt "Dependent Variable (DV, WRES, ..., or any other variable defined; default DV): "
                         var
                         "Invalid Input; Dependent Variable (DV, WRES, ..., or any other variable defined; default DV): "
                         "DV"))
    ;; Make sure that do_esimation=TRUE.
    (add-to-list 'args (format "--dv=%s" (upcase dv)))
    (unless npc
                                        ; Now VPC only options.
      (setq idv (esn-prompt "Independent variable to bin on (Default TIME): "
                            inp
                            "Invalid input, Independent variable to bin on (Default TIME): "
                            "TIME"
                            ))
      (unless (string= "time" (downcase idv))
        (add-to-list 'args (format "--idv=%s" (upcase idv))))
      (save-window-excursion
        (switch-to-buffer-other-window "*esn-psn-options*")
        (insert "1. Bin based on unique values of the independent variable.\n")
        (insert "2. Bin based on user-defined list of values of the independent variable.\n")
        (insert "3. Bin based on user-defined list of observation counts in each bin.\n")
        (insert "4. Bin into N bins, of equal width based on independent variable.\n")
        (insert "5. Bin into N bins, containing equal observation counts.\n")
        (insert "6. Bin width X, with overlay of Y% of consecutive bins.\n")
        (insert "7. Bin observations X, with overaly of Y% of consecutive bins.\n")
        (insert "8. Bin width X\n")
        (insert "9. Bin observations X\n")
        (setq bin-type (esn-prompt "Type of Binning (1-9; Default 1): "
                                   "^[1-9]$"
                                   "Invalid Input; Type of Binning (1-9; Default 1): "
                                   "1"))
        (kill-buffer (current-buffer))
        (switch-to-buffer ob))
      
      (when (string= "2" bin-type)
        (add-to-list 'args "--bin_by_count=0")
        (setq tmp (esn-prompt
                   "Boundaries between bins (list 1.2,1.3,...): "
                   "\\(\\(?:[0-9.]+ *,?\\)+\\)"
                   "Invalid list, Boundaries between bins (list 1.2,1.3,...): "))
        (setq tmp (mapconcat (lambda(x) x)
                             (split-string tmp "[ \f\t\n\r\v,]+")
                             ","))
        
        (add-to-list 'args (format "--bin_array=%s" tmp)))
      (when (string= "3" bin-type)
        (add-to-list 'args "--bin_by_count=1")
        (setq tmp (esn-prompt
                   "Observation counts of bins (list 100,200,...): "
                   "\\(\\(?:[0-9]+ *,?\\)+\\)"
                   "Invalid list, Observation counts of bins (list 100,200,...): "))
        (setq tmp (mapconcat (lambda(x) x)
                             (split-string tmp "[ \f\t\n\r\v,]+")
                             ","))
        (add-to-list 'args (format "--bin_array=%s" tmp)))
      (when (string= "4" bin-type)
        (add-to-list 'args "--bin_by_count=0")
        (add-to-list 'args
                     (format "--no_of_bins=%s"
                             (esn-prompt
                              "Number of bins: "
                              "^[0-9]+$"
                              "Invalid number of bins, please re-enter: "))))
      (when (string= "5" bin-type)
        (add-to-list 'args "--bin_by_count=1")
        (add-to-list 'args
                     (format "--no_of_bins=%s"
                             (esn-prompt
                              "Number of bins: "
                              "^[0-9]+$"
                              "Invalid number of bins, please re-enter: "))))
      
      (when (string= "6" bin-type)
        (add-to-list 'args "--bin_by_count=0")
        (add-to-list 'args
                     (format "--single_bin_size=%s"
                             (esn-prompt
                              "Bin Width: "
                              "^[0-9.]+$"
                              "Invalid Width, please re-enter: "
                              )
                             ))
        (add-to-list 'args
                     (format "--overlap=%s"
                             (esn-prompt
                              "Overlap %: "
                              "^[0-9.]+$"
                              "Invalid %, Please reenter Overlap %"))))
      (when (string= "7" bin-type)
        (add-to-list 'args "--bin_by_count=1")
        (add-to-list 'args
                     (format "--single_bin_size=%s"
                             (esn-prompt
                              "Number of Observations per bin: "
                              "^[0-9]+$"
                              "Number of Observations per bin: ")))
        (add-to-list 'args
                     (format "--overlap=%s"
                             (esn-prompt
                              "Overlap %: "
                              "^[0-9.]+$"
                              "Invalid %, Please reenter Overlap %"))))
      (when (string= "8" bin-type)
        (add-to-list 'args "--bin_by_count=0")
        (add-to-list 'args
                     (format "--single_bin_size=%s"
                             (esn-prompt
                              "Bin Width: "
                              "^[0-9.]+$"
                              "Invalid Width, please re-enter: "))))
      (when (string= "9" bin-type)
        (add-to-list 'args "--bin_by_count=1")
        (add-to-list 'args
                     (format "--single_bin_size=%s"
                             (esn-prompt
                              "Number of Observations per bin: "
                              "^[0-9]+$"
                              "Number of Observations per bin: ")))))
    (setq args (append args run))
    (if npc
        (apply 'esn-psn-cmd esn-mode-psn-npc-command args)
      (apply 'esn-psn-cmd esn-mode-psn-vpc-command args))))

(defun esn-psn-llp ()
  "Log Likelihood Profiling of Run"
  (interactive)
  (let ((nmo (esn-get-nonmem-output)))
    (if (not nmo)
        (esn-error "This utility requires a successful NONMEM run with an output file.")
      (when (buffer-modified-p)
        (esn-error "Buffer must be saved AND successfully submitted to NONMEM.")))
    (let ((run (esn-psn-runname))
          (sig-vars (esn-get-variable-names "SIG"))
          (sig-max (esn-max-eps))
          (sig-bare '())
          (ome-vars (esn-get-variable-names "OME"))
          (ome-max (esn-max-eta))
          (ome-bare '())
          (the-vars (esn-get-variable-names "THE"))
          (the-max (esn-max-theta))
          (the-bare '())
          (args-lst '())
          (args '())
          (reg "")
          (case-fold-search 't)
          (i 1)
          var)
      (when (string-match "[/\\\\]\\([^/\\\\]*$\\)" nmo)
        (setq nmo (match-string 1 nmo)))
      (while (<= i sig-max)
        (add-to-list 'sig-bare (format "EPS(%s)" i))
        (setq i (+ i 1)))
      (setq i 1)
      (while (<= i ome-max)
        (add-to-list 'sig-bare (format "ETA(%s)" i))
        (setq i (+ i 1)))
      (setq i 1)
      (while (<= i the-max)
        (add-to-list 'sig-bare (format "THETA(%s)" i))
        (setq i (+ i 1)))
      (setq reg (format "^\\(?:[ \t]*\\(?:%s\\)[ \t]*,?\\)+[ \t]*$" (regexp-opt (append sig-vars sig-bare ome-vars ome-bare the-vars the-bare) )))
      (setq var (downcase (esn-prompt "Variables to Profile (THETA(x),ETA(x),EPS(x), or variable labels): "
                                      reg
                                      "Not all variables found, variables to Profile (THETA(x),ETA(x),EPS(x), or variable labels): ")))
      (mapc (lambda(x)
              (let ((the-vars (nth 0 x))
                    (theta (nth 1 x))
                    (i 0))
                (while (< i (length the-vars))
                  (while (string-match (format "\\<%s\\>" (nth i the-vars)) var)
                    (setq var (replace-match (format "%s(%s)" theta (+ i 1)) nil nil var)))
                  (setq i (+ i 1)))))
            (list (list the-vars "THETA")
                  (list ome-vars "ETA")
                  (list sig-vars "EPS")))
      (setq args-lst (mapcar (lambda(x)
                               (let (
                                     (lst '())
                                     (ret "")
                                     )
                                 (while (string-match (format "\\<%s(\\([0-9]+\\))" x) var)
                                   (add-to-list 'lst (match-string 1 var))
                                   (setq var (replace-match "" nil nil var))
                                   )
                                 (setq ret (mapconcat (lambda(y) y) lst ","))
                                 (symbol-value 'ret)))
                             (list "THETA" "ETA" "EPS")))
      (unless (string= "" (nth 0 args-lst))
        (add-to-list 'args (format "--thetas='%s'" (nth 0 args-lst))))
      (unless (string= "" (nth 1 args-lst))
        (add-to-list 'args (format "--omegas='%s'" (nth 1 args-lst))))
      (unless (string= "" (nth 2 args-lst))
        (add-to-list 'args (format "--sigmas='%s'" (nth 2 args-lst))))
      (add-to-list 'args (format "--max_iterations=%s" esn-mode-psn-llp-max-iterations))
      (when esn-mode-psn-llp-mplots
        (add-to-list 'args "--mplots"))
      (add-to-list 'args (format "--normq=%s" esn-mode-psn-normq-number))
      (add-to-list 'args (format "--ofv_increase=%s" esn-mode-psn-objv-increase-number))
      (add-to-list 'args (format "--outputfile='%s'" nmo))
      (when esn-mode-psn-llp-rplots
        (add-to-list 'args "--rplots"))
      (setq args (append args (list (format "--model=%s" run))))
      (apply 'esn-psn-cmd esn-mode-psn-llp-command args))))

(defun esn-psn-cdd ()
  "Submits control stream for case deletion diagnostics."
  (interactive)
  ;; Need COVARIATE in dataset (need input line)
  ;; Need Number of runs.
  (let ((run (esn-psn-runname))
        (var "")
        (bs 0)
        (inp (esn-xpose-get-input-line))
        (args '()))
    (if (not (and (esn-rec "INP") (esn-rec "DAT")))
        (esn-error "Randomization test requires an $INPUT and $DATA record.")
      (when (buffer-modified-p)
        (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
            (save-buffer)
          (esn-error "Buffer must be saved to run Perl Speaks NONMEM.")))
      (setq var (downcase (esn-prompt "Case Column: "
                                      inp
                                      "Case Column: (Variable must be in $INPUT): "
                                      )))
      (add-to-list 'args (format "--case_column=%s" var))
      (when esn-mode-psn-cdd-rplots
        (add-to-list 'args "--rplots"))
      (if esn-mode-psn-cdd-use-random-instead-of-consecutive
          (add-to-list 'args "--selection_method='random'")
        (add-to-list 'args "--selection_method='consecutive'"))
      (if esn-mode-psn-cdd-xv
          (add-to-list 'args "--xv")
        (add-to-list 'args "--noxv"))
      (setq args (append args (list run)))
      (apply 'esn-psn-cmd esn-mode-psn-cdd-command args))))

(defun esn-psn-bs ()
  "Bootstrap run with PsN"
  (interactive)
  (let ((run (esn-psn-runname))
        (bs 0)
        (args '("--summary")))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
          (save-buffer)
        (esn-error "Buffer must be saved to run Wings For NONMEM.")))
    
    (setq bs (esn-prompt "Number of Bootstrap Replications: "
                         "^[0-9]+$"
                         "Number of Bootstrap Repliactions (Must be an integer): "))
    (when esn-mode-psn-bs-bca
      (add-to-list 'args "--bca"))
    (when esn-bs-mplots
      (add-to-list 'args "--mplots"))
    (when esn-bs-rplots
      (add-to-list 'args "--rplots"))
    (when esn-skip-covariance-step-terminated
      (add-to-list 'args "--skip_covariance_step_terminated"))
    (when esn-skip-estimate-near-boundary
      (add-to-list 'args "--skip_estimate_near_boundary"))
    (when esn-skip-minimization-terminated
      (add-to-list 'args "--skip_minimization_terminated"))
    (when esn-skip-with-covstep-warnings
      (add-to-list 'args "--skip_with_covstep_warnings"))
    (add-to-list 'args (format "--covariance_step_successful_limit='%s'"
                               esn-bs-covariance-step-successful-limit))
    (add-to-list 'args (format "--covariance_step_warnings_limit='%s'"
                               esn-bs-covariance-step-warnings-limit))
    (add-to-list 'args (format "--covariance_step_warnings_limit='%s'"
                               esn-bs-estimate-near-boundary-limit))
    (add-to-list 'args (format "--minimization_successful_limit='%s'"
                               esn-bs-estimate-successful-limit))
    (add-to-list 'args (format "--samples=%s" bs))
    (setq args (append args (list run)))
    (apply 'esn-psn-cmd esn-mode-psn-boostrap-command args)))

(defun esn-psn-execute ()
  "Submit job  with PsN"
  (interactive)
  (let ((run (esn-psn-runname))
        (bs 0)
        (args '()))
    
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Run %s modified; save it? " (buffer-name)))
          (save-buffer)
        (esn-error "Buffer must be saved to run Perl Speaks NONMEM.")))
    
    (if esn-mode-psn-clean
        (add-to-list 'args "--clean=1")
      (add-to-list 'args "--clean=0"))
    (when esn-mode-psn-compress
      (add-to-list 'args "--compress"))
    (when esn-mode-psn-drop-dropped
      (add-to-list 'args "--drop_dropped"))
    (when (> esn-mode-psn-maxevals 9999)
      (add-to-list 'args (format "--handle_maxevals='%s'" esn-mode-psn-maxevals)))
    (when (and esn-mode-psn-tweak-inits (> esn-mode-psn-retries 1))
      (add-to-list 'args "--tweak_inits")
      (add-to-list 'args (format "--retries='%s'"esn-mode-psn-retries))
      (when esn-mode-psn-picky
        (add-to-list 'args (format "--picky"))))
    (if esn-mode-psn-ex-summarize
        (add-to-list 'args "--summarize")
      (when esn-mode-psn-quick-summary
        (add-to-list 'args "--quick_summary")))
    (when esn-mode-psn-calculate-shrinkage
      (add-to-list 'args "--shrinkage"))
    (when esn-mode-psn-calculate-cwres
      (add-to-list 'args "--compute_cwres"))
    (when esn-mode-psn-wrap-data
      (add-to-list 'args "--wrap_data"))
    (when esn-mode-psn-verbose
      (add-to-list 'args "--verbose"))
    (when esn-mode-psn-sde
      (add-to-list 'args "--sde"))
    (setq args (append args (list run)))
    (apply 'esn-psn-cmd esn-mode-psn-execute-command args)))

(defun esn-psn-runname ()
  "Gets run-name, based on the current buffer."
  (let ((fn (buffer-file-name)))
    (if (string-match "[\\\\/]\\([^\\\\/]*\\)$" fn)
        (setq fn (match-string 1 fn)))
    (symbol-value 'fn)))


(provide 'esn-psn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-psn.el ends here
