;;; esn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (esn-80-show esn-80-clear) "esn-80" "lisp/esn-80.el"
;;;;;;  (19896 44034))
;;; Generated autoloads from lisp/esn-80.el

(autoload 'esn-80-clear "esn-80" "\
Clears the 80 line highlighting.

\(fn)" t nil)

(autoload 'esn-80-show "esn-80" "\
Show 80 character line (useful for NONMEM 5).

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-ac-start) "esn-ac" "lisp/esn-ac.el" (19896
;;;;;;  63572))
;;; Generated autoloads from lisp/esn-ac.el

(autoload 'esn-ac-start "esn-ac" "\
Starts auto-completion through auto-complete-mode

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-align-tab esn-align-equals-fun) "esn-align"
;;;;;;  "lisp/esn-align.el" (19902 55994))
;;; Generated autoloads from lisp/esn-align.el

(autoload 'esn-align-equals-fun "esn-align" "\
Defines alignment function for PK -- Creates an idle timer.

\(fn)" nil nil)

(autoload 'esn-align-tab "esn-align" "\
Defines alignment function for PK -- Creates an idle timer.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-gen-profile esn-gen-autoloads) "esn-autoloads-gen"
;;;;;;  "lisp/esn-autoloads-gen.el" (19896 45228))
;;; Generated autoloads from lisp/esn-autoloads-gen.el

(autoload 'esn-gen-autoloads "esn-autoloads-gen" "\
Generate Emacs Speaks NONEM Autoloads using `update-file-autoloads' and put in `esn-autoloads.el'

\(fn)" t nil)

(autoload 'esn-gen-profile "esn-autoloads-gen" "\
Generate Emacs Speaks NONMEM profile

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-file-coding-system) "esn-coding" "lisp/esn-coding.el"
;;;;;;  (19896 45226))
;;; Generated autoloads from lisp/esn-coding.el

(autoload 'esn-file-coding-system "esn-coding" "\
* Function that changes to the appropriate file system.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-company-start) "esn-company" "lisp/esn-company.el"
;;;;;;  (19897 21818))
;;; Generated autoloads from lisp/esn-company.el

(autoload 'esn-company-start "esn-company" "\
* Start Company completion within EsN.

\(fn &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (esn-cui-start) "esn-cui" "lisp/esn-cui.el" (19897
;;;;;;  22064))
;;; Generated autoloads from lisp/esn-cui.el

(autoload 'esn-cui-start "esn-cui" "\
* Start completion UI for EsN.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-quote-infn) "esn-cwres" "lisp/esn-cwres.el"
;;;;;;  (19897 22116))
;;; Generated autoloads from lisp/esn-cwres.el

(autoload 'esn-quote-infn "esn-cwres" "\
* Quote INFN template to a regular expression.

\(fn ARG)" nil nil)

;;;***

;;;### (autoloads (esn-R-input-data esn-R-list) "esn-ess" "lisp/esn-ess.el"
;;;;;;  (19902 56382))
;;; Generated autoloads from lisp/esn-ess.el

(autoload 'esn-R-list "esn-ess" "\
Gets R command and returns list of output.  Uses esn.lisp R function contained in esnR.

CMD R command to produce list.

FORCE-STRING boolean indicating that the output should be a list of strings even if it is numbers.

\(fn CMD &optional FORCE-STRING)" nil nil)

(autoload 'esn-R-input-data "esn-ess" "\
Loads data into R process.  Returns output on completion.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-R-finished esn-clean-nm-files esn-rm-file
;;;;;;  esn-finish-with-R esn-nm-submit esn-plt-submit esn-nmqual-submit
;;;;;;  esn-nmfe-cmd esn-xmind-project esn-xmind-ctl esn-message
;;;;;;  esn-alert) "esn-exec" "lisp/esn-exec.el" (19902 51834))
;;; Generated autoloads from lisp/esn-exec.el

(autoload 'esn-alert "esn-exec" "\
* Esn Alert message

\(fn &rest ARGS)" nil nil)

(autoload 'esn-message "esn-exec" "\
* Applies message, and possibly sends to todochiku

\(fn &rest ARGS)" nil nil)

(autoload 'esn-xmind-ctl "esn-exec" "\
Starts Xmind control file summary.

\(fn)" t nil)

(autoload 'esn-xmind-project "esn-exec" "\
Starts Xmind project.

\(fn)" t nil)

(autoload 'esn-nmfe-cmd "esn-exec" "\
Runs nmfe6 command

\(fn NMFE &rest ARGS)" nil nil)

(autoload 'esn-nmqual-submit "esn-exec" "\
Submits run to WFN to run.

\(fn)" t nil)

(autoload 'esn-plt-submit "esn-exec" "\
Submits run to PLT Tools to run.

\(fn)" t nil)

(autoload 'esn-nm-submit "esn-exec" "\
Submits run to nmfe to run.

\(fn)" t nil)

(autoload 'esn-finish-with-R "esn-exec" "\
* Finishes the submission with R -- requires Ess.

\(fn PROCESS EVENT &optional BUFFER)" t nil)

(autoload 'esn-rm-file "esn-exec" "\
Removes file if exists

\(fn FILE)" nil nil)

(autoload 'esn-clean-nm-files "esn-exec" "\
Cleans NONMEM files before submission

\(fn RUN)" nil nil)

(autoload 'esn-R-finished "esn-exec" "\
Let user know that R process is completed.

\(fn PROCESS EVENT)" nil nil)

;;;***

;;;### (autoloads (esn-get-variable-names) "esn-extended" "lisp/esn-extended.el"
;;;;;;  (19902 52878))
;;; Generated autoloads from lisp/esn-extended.el

(autoload 'esn-get-variable-names "esn-extended" "\
Gets the theta names from the appropriate block, and returns
them as a list.  Only get variable names from the first $PROBLEM
statement

\(fn &optional WHAT CM CMSAME)" nil nil)

;;;***

;;;### (autoloads (esn-fill-record) "esn-fill" "lisp/esn-fill.el"
;;;;;;  (19902 52934))
;;; Generated autoloads from lisp/esn-fill.el

(autoload 'esn-fill-record "esn-fill" "\
Not documented

\(fn &optional R)" t nil)

;;;***

;;;### (autoloads (esn-mode--fontlock) "esn-fontlock" "lisp/esn-fontlock.el"
;;;;;;  (19902 56860))
;;; Generated autoloads from lisp/esn-fontlock.el

(autoload 'esn-mode--fontlock "esn-fontlock" "\
Fontlock building

\(fn &optional LST)" t nil)

;;;***

;;;### (autoloads (esn-format esn-mangle-problem-file-name esn-prompt
;;;;;;  esn-command-to-string esn-get-input-files esn-get-output-files
;;;;;;  esn-get-nonmem-output esn-get-abs-dir esn-file-exists-p esn-renumber
;;;;;;  esn-renumber-omsimp esn-renumber-thsimp esn-renumber-a_0
;;;;;;  esn-renumber-cden_ esn-renumber-p esn-renumber-a esn-renumber-dadt
;;;;;;  esn-renumber-mnext esn-renumber-mpast esn-renumber-mtime
;;;;;;  esn-renumber-err esn-renumber-eps esn-renumber-eta esn-renumber-theta
;;;;;;  esn-theta-down esn-eta-up esn-renumber-com esn-insert-after
;;;;;;  esn-replace-in-record esn-search-in-record esn-backward-w
;;;;;;  esn-forward-w esn-in-rec-p esn-narrow-to-problem esn-user-full-name
;;;;;;  esn-advan esn-trans esn-infusion esn-rec3) "esn-fun" "lisp/esn-fun.el"
;;;;;;  (19917 40080))
;;; Generated autoloads from lisp/esn-fun.el

(autoload 'esn-rec3 "esn-fun" "\
Changes a string to a current record/option abbreviation

Current Record = Three letters of current record excluding $

For $AES0 and $AESINITIAL return AES0

When OPTION=, return OPT=

\(fn STRING &optional NO-EQUALS)" nil nil)

(autoload 'esn-infusion "esn-fun" "\
Determines if there is any infusion information in the model.

\(fn &optional REG)" nil nil)

(autoload 'esn-trans "esn-fun" "\
Gets the TRANS number of the run.

\(fn)" nil nil)

(autoload 'esn-advan "esn-fun" "\
Gets the ADVAN number of the run.

\(fn)" nil nil)

(autoload 'esn-user-full-name "esn-fun" "\
Gets the full user name or prompts for it and saves to .emacs if not present.

\(fn &optional UID)" t nil)

(autoload 'esn-narrow-to-problem "esn-fun" "\
Narrow to the problem number num.

\(fn &optional NM)" t nil)

(autoload 'esn-in-rec-p "esn-fun" "\
* Defines point is in record or not

\(fn)" nil nil)

(autoload 'esn-forward-w "esn-fun" "\
Defines the forward-word routine used for wrapping.  See `esn-backward-w'.

\(fn &optional CHARS)" t nil)

(autoload 'esn-backward-w "esn-fun" "\
Defines the backward-word routine used for wrapping.
Hence these are things that should not be separated on a line.
 (1) A = B is a word
 (2) (A B C) is a word
 (3) A = (B C) is a word.
 (4) 0.9 FIXED is a word
 (5) (0 1 2) FIXED is a word (NM6)

\(fn &optional CHARS)" t nil)

(autoload 'esn-search-in-record "esn-fun" "\
* Searches re within the record rec.

\(fn RE REC)" nil nil)

(autoload 'esn-replace-in-record "esn-fun" "\
* Replaces re with rep in the record rec.  If not found return nil, otherwise return true.
all = replace all matches
remEmpty = delete empty record.

\(fn RE REP REC &optional ALL REMEMPTY)" nil nil)

(autoload 'esn-insert-after "esn-fun" "\
Inserts a text what after records lst

\(fn WHAT LST &optional GOTOPOINT IS-YAS-SNIPPET)" nil nil)

(autoload 'esn-renumber-com "esn-fun" "\
Renumber COM.  Changes COMSAV if appropriate.

\(fn)" t nil)

(autoload 'esn-eta-up "esn-fun" "\
Defines alt-up swapping in $OMEGA block

\(fn)" t nil)

(autoload 'esn-theta-down "esn-fun" "\
Defines the alt-down swapping in $theta block

\(fn &optional WHAT)" t nil)

(autoload 'esn-renumber-theta "esn-fun" "\
Renumber the THETAs

\(fn)" t nil)

(autoload 'esn-renumber-eta "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-eps "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-err "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-mtime "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-mpast "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-mnext "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-dadt "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-a "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-p "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-cden_ "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-a_0 "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-thsimp "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber-omsimp "esn-fun" "\
Not documented

\(fn)" t nil)

(autoload 'esn-renumber "esn-fun" "\
Renumbers X and fills in the gaps, say X=com.  Returns the max com if below isn't specified.  Otherwise returns a list of  max com and number of missing items below the variable below.

\(fn WHAT &optional BELOW)" t nil)

(autoload 'esn-file-exists-p "esn-fun" "\
Determines if file exists using absolute directory for file based on the NONMEM run options.

\(fn FILE)" nil nil)

(autoload 'esn-get-abs-dir "esn-fun" "\
Gets the absolute directory for file based on the NONMEM run options.

\(fn FILE)" nil nil)

(autoload 'esn-get-nonmem-output "esn-fun" "\
Get the NONMEM run output if it exists.

\(fn &optional EXTS)" nil nil)

(autoload 'esn-get-output-files "esn-fun" "\
Gets the output files for the current NONMEM run.  If addOutput is true, include the NONMEM run output, R script, and R outout file if they exists

\(fn &optional ADDOUTPUT)" t nil)

(autoload 'esn-get-input-files "esn-fun" "\
Gets the input files for the current NONMEM run.

\(fn)" t nil)

(autoload 'esn-command-to-string "esn-fun" "\
Executes a command in Emacs or Xemacs (doesn't matter). If at first the command fails, try 3 more times.

I have encountered the following error before:

shell-command-to-string: Creating process pipe: no error

Not sure what to do about it but try again.

\(fn CMD)" nil nil)

(autoload 'esn-prompt "esn-fun" "\
Prompts for something that optionally is forced to match reg

\(fn PROMPT &optional REG ALTPROMPT DEFAULT TXT)" nil nil)

(autoload 'esn-mangle-problem-file-name "esn-fun" "\
This changes file names to unique names based on the problem that you are in.  For example sdtab1 is changed to sdtab1_2 in the second problem statement.

\(fn NAME PROBLEM)" nil nil)

(autoload 'esn-format "esn-fun" "\
* Provides esn-format which formats everything the same way as format with the exception of %1% %2%, etc.  Those are replaced with the first format argument, and then the second, and so on.

\(fn TEXT &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (esn-hide-header) "esn-hide" "lisp/esn-hide.el"
;;;;;;  (19902 59654))
;;; Generated autoloads from lisp/esn-hide.el

(autoload 'esn-hide-header "esn-hide" "\
Hides non-editable portions of the header.  Makes editable
poritions of buffer highlighted.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-indent-line) "esn-indent" "lisp/esn-indent.el"
;;;;;;  (19902 59998))
;;; Generated autoloads from lisp/esn-indent.el

(autoload 'esn-indent-line "esn-indent" "\
* Indents a line in EsN

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-mode-add-input-variables) "esn-input" "lisp/esn-input.el"
;;;;;;  (19902 60040))
;;; Generated autoloads from lisp/esn-input.el

(autoload 'esn-mode-add-input-variables "esn-input" "\
Defines input variable aliases automatically if not present.

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-iov2 esn-iov esn-occ) "esn-iov" "lisp/esn-iov.el"
;;;;;;  (19902 60100))
;;; Generated autoloads from lisp/esn-iov.el

(autoload 'esn-occ "esn-iov" "\
Puts Occasion in $PK or $PRED block if not present

\(fn)" nil nil)

(autoload 'esn-iov "esn-iov" "\
* Creates IOV definition

\(fn)" t nil)

(autoload 'esn-iov2 "esn-iov" "\
Creates an IOV definition outside of current location

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-make-link-overlays) "esn-link" "lisp/esn-link.el"
;;;;;;  (19902 60174))
;;; Generated autoloads from lisp/esn-link.el

(autoload 'esn-make-link-overlays "esn-link" "\
* Creates an idle timer to create link overlays

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-before-change-functions-hook) "esn-magic-keys"
;;;;;;  "lisp/esn-magic-keys.el" (19917 40128))
;;; Generated autoloads from lisp/esn-magic-keys.el

(autoload 'esn-before-change-functions-hook "esn-magic-keys" "\
Called when in EsN mode on change of buffer.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads (esn-narrow-record esn-is-abbrev-p esn-get-current-rec
;;;;;;  esn-last-record-same-p esn-point-at-bor esn-narrow-to-current-problem)
;;;;;;  "esn-narrow" "lisp/esn-narrow.el" (19896 44506))
;;; Generated autoloads from lisp/esn-narrow.el

(autoload 'esn-narrow-to-current-problem "esn-narrow" "\
Not documented

\(fn)" t nil)

(autoload 'esn-point-at-bor "esn-narrow" "\
Point at beginning of record

\(fn)" nil nil)

(autoload 'esn-last-record-same-p "esn-narrow" "\
Returns true if the last record was the same as the current record.

\(fn &optional FORCE-RECALC)" nil nil)

(autoload 'esn-get-current-rec "esn-narrow" "\
Returns the current record, otherwise returns \"\".

Current Record = Three letters of current record excluding $

For $AES0 and $AESINITIAL return AES0

\(fn &optional FORCE-RECALC)" t nil)

(defalias 'esn-narrow-rec 'esn-narrow-record "\
* Narrows record")

(autoload 'esn-is-abbrev-p "esn-narrow" "\
* Is the current record an abbreviated record?

\(fn &optional ONLY-REC)" nil nil)

(autoload 'esn-narrow-record "esn-narrow" "\
This function narrows the buffer to the current record.
   If outside of the record, esn-narrow-rec does nothing.
Returns the indentation length

\(fn &optional FORCE-RECALCULATE)" t nil)

;;;***

;;;### (autoloads (esn-mode-short-advan) "esn-options" "lisp/esn-options.el"
;;;;;;  (19902 52040))
;;; Generated autoloads from lisp/esn-options.el

(autoload 'esn-mode-short-advan "esn-options" "\
Gets the short description of the advan number.

NUMBER specifies the advan number described.

\(fn NUMBER)" nil nil)

;;;***

;;;### (autoloads (esn-plt-add-wide) "esn-plt" "lisp/esn-plt.el"
;;;;;;  (19903 4308))
;;; Generated autoloads from lisp/esn-plt.el

(autoload 'esn-plt-add-wide "esn-plt" "\
* Add WIDE keyword to $DATA

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-project-get-control-streams def-esn-project-update)
;;;;;;  "esn-project" "lisp/esn-project.el" (19896 44488))
;;; Generated autoloads from lisp/esn-project.el

(autoload 'def-esn-project-update "esn-project" "\
Defines a project update to do on control streams
FUNCTION-NAME is the resulting function.
APPLY-FUNCTION is the function that should be applied to the control stream.

\(fn FUNCTION-NAME &key APPLY-FUNCTION MESSAGE)" nil (quote macro))

(autoload 'esn-project-get-control-streams "esn-project" "\
* Gets the control stream possibilities based on the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-get-fixed-thetas esn-num-problems esn-is-pred-p
;;;;;;  esn-max-theta esn-max-eta esn-max-eps esn-max-what esn-rec
;;;;;;  esn-is-foi-posthoc-p esn-is-foi-p esn-is-fo-posthoc-p esn-is-fo-p
;;;;;;  esn-is-hybrid-p esn-is-focei-p esn-is-lap-p esn-is-foce-p
;;;;;;  esn-est-method-txt esn-model-txt esn-cancel-pending-unneeded-timers
;;;;;;  esn-in-comment-p esn-is-block-p esn-standardize-lines esn-eta-error
;;;;;;  esn-problem-number esn-runname-noext esn-input-data esn-output-tables
;;;;;;  esn-nm-ver>= esn-fixed-theta-p) "esn-properties" "lisp/esn-properties.el"
;;;;;;  (19902 62224))
;;; Generated autoloads from lisp/esn-properties.el

(autoload 'esn-fixed-theta-p "esn-properties" "\
Checks to see if THETA-NUMBER is fixed in the $THETA block.  Only considers the first problem.

\(fn THETA-NUMBER)" nil nil)

(autoload 'esn-nm-ver>= "esn-properties" "\
Is the control stream using NONMEM VERSION or greater?

\(fn VERSION)" nil nil)

(autoload 'esn-output-tables "esn-properties" "\
Gets a list of the output tables in the current control-stream

\(fn)" t nil)

(autoload 'esn-input-data "esn-properties" "\
Provides the current control stream's input dataset

\(fn)" nil nil)

(autoload 'esn-runname-noext "esn-properties" "\
Gets run-name, based on the current buffer.
According to WFN a runname is the file without the extension.
Also accoring to WFN you need to be in the current directory to execute it.
When nospaces is true, replace spaces with _.

\(fn &optional NOSPACES)" nil nil)

(autoload 'esn-problem-number "esn-properties" "\
* Figures out the problem number for the current point.
Problem 1 is the first problem

\(fn &optional POINT)" nil nil)

(autoload 'esn-eta-error "esn-properties" "\
* Returns the errors for ETAs

\(fn &optional THE OME SIG)" t nil)

(autoload 'esn-standardize-lines "esn-properties" "\
* Returns standardize equations.

\(fn &optional THE OME SIG DO-THETA)" nil nil)

(autoload 'esn-is-block-p "esn-properties" "\
Determines if:
 1. We are in a OMEGA/SIGMA blocks
 2. We are in a BLOCK(#)

\(fn)" nil nil)

(autoload 'esn-in-comment-p "esn-properties" "\
Returns if the the point is in the comment or string.  Also
preserves match data.

\(fn &optional MSL)" nil nil)

(autoload 'esn-cancel-pending-unneeded-timers "esn-properties" "\
* Cancels unneeded times when the current record changes

\(fn)" nil nil)

(autoload 'esn-model-txt "esn-properties" "\
* Returns a Text Rendition of the Model used

\(fn)" nil nil)

(autoload 'esn-est-method-txt "esn-properties" "\
* Returns a Text rendition of the Estimation method

\(fn)" nil nil)

(autoload 'esn-is-foce-p "esn-properties" "\
* Returns if the current control stream uses FOCE estimation (but NOT FOCEI)

\(fn)" nil nil)

(autoload 'esn-is-lap-p "esn-properties" "\
* Returns if the current control stream uses Laplacian estimation

\(fn)" nil nil)

(autoload 'esn-is-focei-p "esn-properties" "\
* Returns if the current control stream uses FOCEI estimation

\(fn)" nil nil)

(autoload 'esn-is-hybrid-p "esn-properties" "\
* Returns if the current control stream uses Hybrid estimation.

\(fn)" nil nil)

(autoload 'esn-is-fo-p "esn-properties" "\
* Returns if the current control stream is FO.

\(fn)" nil nil)

(autoload 'esn-is-fo-posthoc-p "esn-properties" "\
 * Returns if the current control stream is FO w/POSTHOC step (and NO interaction step)

\(fn)" nil nil)

(autoload 'esn-is-foi-p "esn-properties" "\
 * Returns if the current control stream is FOI w/POSTHOC step

\(fn)" nil nil)

(autoload 'esn-is-foi-posthoc-p "esn-properties" "\
 * Returns if the current control stream is FOI w/POSTHOC step

\(fn)" nil nil)

(autoload 'esn-rec "esn-properties" "\
* This returns if a record is present. If cont is true, return its contents.
If prob-one, only the first problem is considered.
If destructive is true, remove the records after obtaining them.

\(fn REC &optional CONT PROBONE DESTRUCTIVE)" nil nil)

(autoload 'esn-max-what "esn-properties" "\
Gets the maximum value of the variable what

\(fn WHAT)" nil nil)

(autoload 'esn-max-eps "esn-properties" "\
Not documented

\(fn &optional NO-EXTENDED)" nil nil)

(autoload 'esn-max-eta "esn-properties" "\
Not documented

\(fn &optional NO-EXTENDED)" nil nil)

(autoload 'esn-max-theta "esn-properties" "\
Not documented

\(fn &optional NO-EXTENDED)" nil nil)

(autoload 'esn-is-pred-p "esn-properties" "\
Determines if the current control stream is a pred control stream.  Looks for the $PRED record.

\(fn)" nil nil)

(autoload 'esn-num-problems "esn-properties" "\
Returns number of problems in control stream.

\(fn)" nil nil)

(autoload 'esn-get-fixed-thetas "esn-properties" "\
Gets the thetas that are fixed.  It will be cached until the user modifies the $THETA block.

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-switch-rpt esn-get-rpt) "esn-rpt-mode" "lisp/esn-rpt-mode.el"
;;;;;;  (19902 62426))
;;; Generated autoloads from lisp/esn-rpt-mode.el

(autoload 'esn-get-rpt "esn-rpt-mode" "\
Function that gets the name of the NONMEM output based on the current file.

FILE-NAME is an optional argument that changes the file name from
the current buffer file name to whatever is supplied.

\(fn &optional FILE-NAME)" nil nil)

(autoload 'esn-switch-rpt "esn-rpt-mode" "\
* Function to switch to output listing

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-desc-subroutines) "esn-space" "lisp/esn-space.el"
;;;;;;  (19903 216))
;;; Generated autoloads from lisp/esn-space.el

(autoload 'esn-desc-subroutines "esn-space" "\
Adds descriptive text to subroutines ADVAN/TRANS combinations.

\(fn &rest IGNORED)" t nil)

;;;***

(autoload 'esn-update-get-version "esn-update" "\
Not documented

\(fn)" nil nil)

;;;### (autoloads (esn-table-split-count) "esn-tab-pred" "lisp/esn-tab-pred.el"
;;;;;;  (19903 290))
;;; Generated autoloads from lisp/esn-tab-pred.el

(autoload 'esn-table-split-count "esn-tab-pred" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-table-name-extension) "esn-tables" "lisp/esn-tables.el"
;;;;;;  (19903 8074))
;;; Generated autoloads from lisp/esn-tables.el

(autoload 'esn-table-name-extension "esn-tables" "\
Defines a table name based on the current control stream by dropping an extension and adding a new extension.  If the buffer is not saved, return nil.

\(fn EXTENSION)" nil nil)

;;;***

;;;### (autoloads (esn-add-toolbar) "esn-toolbar" "lisp/esn-toolbar.el"
;;;;;;  (19896 44456))
;;; Generated autoloads from lisp/esn-toolbar.el

(autoload 'esn-add-toolbar "esn-toolbar" "\
Add the ESN toolbar to a particular mode.

\(fn MAP)" nil nil)

;;;***

;;;### (autoloads (esn-scale esn-get-time-units) "esn-units" "lisp/esn-units.el"
;;;;;;  (19903 4516))
;;; Generated autoloads from lisp/esn-units.el

(autoload 'esn-get-time-units "esn-units" "\
Gets the time or prompts for a time variable.

\(fn &optional PROMPT)" nil nil)

(autoload 'esn-scale "esn-units" "\
* Gets scale factor from control stream

\(fn &optional NO-INSERT)" t nil)

;;;***

;;;### (autoloads (esn-update-save-variables esn-update-get-version)
;;;;;;  "esn-update" "lisp/esn-update.el" (19903 3288))
;;; Generated autoloads from lisp/esn-update.el

(autoload 'esn-update-get-version "esn-update" "\
Not documented

\(fn)" nil nil)

(autoload 'esn-update-save-variables "esn-update" "\
* This updates the save variables upon  opening a file.

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-edit) "esn-vc" "lisp/esn-vc.el" (19917 41510))
;;; Generated autoloads from lisp/esn-vc.el

(autoload 'esn-edit "esn-vc" "\
* Defines the function that is used to start an editing of a repository.

\(fn)" t nil)

;;;***

;;;### (autoloads (esn-current-record-options-help) "esn-version-switch"
;;;;;;  "lisp/esn-version-switch.el" (19902 60452))
;;; Generated autoloads from lisp/esn-version-switch.el

(autoload 'esn-current-record-options-help "esn-version-switch" "\
* Gets the current record options help vector

\(fn &optional RECORD VERSION)" nil nil)

;;;***

;;;### (autoloads (esn-use-xpose-p esn-use-nmqual-p esn-use-pirana-p
;;;;;;  esn-use-plt-p esn-use-pdx-p esn-use-wfn-p esn-use-census-p
;;;;;;  esn-use-psn-p esn-use-xmind-p esn-use-plt-archive-p) "esn-which"
;;;;;;  "lisp/esn-which.el" (19903 3418))
;;; Generated autoloads from lisp/esn-which.el

(autoload 'esn-use-plt-archive-p "esn-which" "\
A function saying if this is a plt tools archive or not.

FILE-NAME is an optional argument to check if the supplied file
is in the PLT tools archive or not.

\(fn &optional FILE-NAME)" nil nil)

(autoload 'esn-use-xmind-p "esn-which" "\
Determines if you may use Xmind

\(fn)" nil nil)

(autoload 'esn-use-psn-p "esn-which" "\
Use Perl Speaks NONMEM?

\(fn)" nil nil)

(autoload 'esn-use-census-p "esn-which" "\
Figures out if this directory is under Census control

\(fn &rest IGNORE)" nil nil)

(autoload 'esn-use-wfn-p "esn-which" "\
Returns if Wings for NONMEM is installed and setup for use with EsN.

\(fn)" nil nil)

(autoload 'esn-use-pdx-p "esn-which" "\
* Determines if we should use PDx-Pop

\(fn)" nil nil)

(autoload 'esn-use-plt-p "esn-which" "\
* A function saying if one should use PLT tools or not.

\(fn)" nil nil)

(autoload 'esn-use-pirana-p "esn-which" "\
Figures out if this directory is under Pirana control

\(fn)" nil nil)

(autoload 'esn-use-nmqual-p "esn-which" "\
* Check to see if nmqual is installed

\(fn)" nil nil)

(autoload 'esn-use-xpose-p "esn-which" "\
Determines if EsN uses Xpose

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-xmind-strip-project esn-add-update-topics
;;;;;;  esn-xmind-checkout esn-xmind-start-map esn-xmind-blank-map
;;;;;;  esn-xmind-update-ctl-map esn-xmind-default-individual-map
;;;;;;  esn-xmind-default-map-name) "esn-xmind" "lisp/esn-xmind.el"
;;;;;;  (19917 41988))
;;; Generated autoloads from lisp/esn-xmind.el

(autoload 'esn-xmind-default-map-name "esn-xmind" "\
* Defines the default map name for Xmind files.  PLT stores the mind map under USERSCRIPTS

\(fn)" nil nil)

(autoload 'esn-xmind-default-individual-map "esn-xmind" "\
* Defines the default individual Xmind map.  
When using PLT, EsN stores the mind-map under [WORKINGDIRECTORYNAME]-xmind/[CONTROLSTREAMNAME,NO EXTENSION].xmind
Otherwise stores under [CONTROLSTREAMNAME,NOEXTENSION].xmind

\(fn &optional CONTROL-FILE-FULL-PATH EXT)" nil nil)

(autoload 'esn-xmind-update-ctl-map "esn-xmind" "\
* Updates the Control Stream Map based on what is currently going on in the control stream.

\(fn)" nil nil)

(autoload 'esn-xmind-blank-map "esn-xmind" "\
* Create blank map.

\(fn FILE &optional ROOT-NAME ROOT-LABEL SHEET-TITLE TIMESTAMP)" nil nil)

(autoload 'esn-xmind-start-map "esn-xmind" "\
* Creates a default map

\(fn &optional FILE)" nil nil)

(autoload 'esn-xmind-checkout "esn-xmind" "\
* Checks out an Xmind file.

\(fn &optional XMIND-FILE)" nil nil)

(autoload 'esn-add-update-topics "esn-xmind" "\
* Starts an idle timer to add or update a parent topic.

\(fn)" nil nil)

(autoload 'esn-xmind-strip-project "esn-xmind" "\
* Strip Project of Labels, and Makers

\(fn &optional FILE)" t nil)

;;;***

;;;### (autoloads (esn-xpose-save esn-xpose-get-input-line) "esn-xpose"
;;;;;;  "lisp/esn-xpose.el" (19903 4412))
;;; Generated autoloads from lisp/esn-xpose.el

(autoload 'esn-xpose-get-input-line "esn-xpose" "\
* Gets a regular expression denoting all the acceptable input
line parameters.  This is done for each problem.  If no input
line is present, then return \"\"

When current is non-nil, return information from the current problem.

If no-reg is non-nil, then return the input line list.

\(fn &optional CURRENT NO-REG)" t nil)

(autoload 'esn-xpose-save "esn-xpose" "\
* Makes sure that xpose naming conventions for model files are correct. Returns if additional update hooks should be stopped.

\(fn)" nil nil)

;;;***

;;;### (autoloads (esn-build-yas esn-yas-after esn-yas-before) "esn-yas"
;;;;;;  "lisp/esn-yas.el" (19903 3686))
;;; Generated autoloads from lisp/esn-yas.el

(autoload 'esn-yas-before "esn-yas" "\
* Hook for Before YAS completion

\(fn)" nil nil)

(autoload 'esn-yas-after "esn-yas" "\
* Hook for After YAS completion

\(fn)" nil nil)

(autoload 'esn-build-yas "esn-yas" "\
* Build Yasnippets for Esn-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("esn-autoloads.el" "esn-pkg.el" "esn-start.el"
;;;;;;  "lisp/esn-80.el" "lisp/esn-ac.el" "lisp/esn-align.el" "lisp/esn-autoloads-gen.el"
;;;;;;  "lisp/esn-coding.el" "lisp/esn-company.el" "lisp/esn-cui.el"
;;;;;;  "lisp/esn-cwres.el" "lisp/esn-ess.el" "lisp/esn-exec.el"
;;;;;;  "lisp/esn-extended.el" "lisp/esn-fill.el" "lisp/esn-fontlock.el"
;;;;;;  "lisp/esn-fun.el" "lisp/esn-hide.el" "lisp/esn-indent.el"
;;;;;;  "lisp/esn-input.el" "lisp/esn-iov.el" "lisp/esn-link.el"
;;;;;;  "lisp/esn-magic-keys.el" "lisp/esn-narrow.el" "lisp/esn-options.el"
;;;;;;  "lisp/esn-plt.el" "lisp/esn-project.el" "lisp/esn-properties.el"
;;;;;;  "lisp/esn-rpt-mode.el" "lisp/esn-space.el" "lisp/esn-tab-pred.el"
;;;;;;  "lisp/esn-toolbar.el" "lisp/esn-units.el" "lisp/esn-update.el"
;;;;;;  "lisp/esn-vc.el" "lisp/esn-version-switch.el" "lisp/esn-which.el"
;;;;;;  "lisp/esn-xmind.el" "lisp/esn-xpose.el" "lisp/esn-yas.el")
;;;;;;  (19896 43582 468000))

;;;***

(provide 'esn-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esn-autoloads.el ends here
