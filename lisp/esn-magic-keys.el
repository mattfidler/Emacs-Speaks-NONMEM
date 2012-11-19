;;; esn-magic-keys.el --- Emacs Speaks NONMEM's smart keys
;;
;; Filename: esn-magic-keys.el
;; Description: Emacs Speaks NONMEM's mart keys
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Filder
;; Created: Thu Jan 28 11:50:57 2010 (-0600)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 1077
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This includes the smart key features of Emacs Speaks NONMEM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 20-Jan-2011    Matthew L. Fidler  
;;    Fixed tab when orgtbl-comment-mode is enabled. 
;; 22-Dec-2010    Matthew L. Fidler  
;;    Added modification hooks to each record.
;; 11-Dec-2010    Matthew L. Fidler  
;;    Changed post-command-hook mechanism to run esn-{REC}-post-command-hook where {REC} reperesents the record that the cursor is currently in.
;; 08-Dec-2010    Matthew L. Fidler  
;;    Bug Fix for magic-$
;; 30-Nov-2010    Matthew L. Fidler  
;;    Changed magic-wrap-actual to check that the wrapping is occuring within esn-mode.
;; 30-Nov-2010    Matthew L. Fidler
;;    Made post-command-hook only work in esn-mode.  That way when changing modes, it doesn't try to wrap regions, etc.
;; 04-Nov-2010      
;;    Changed esn help key.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Bugfix to enter and exit hook condition-case statements.
;; 02-Nov-2010    Matthew L. Fidler
;;    Added exit record hook and enter record hook
;; 26-Oct-2010    Matthew L. Fidler  
;;    Added more debugging information to post-command hook
;; 20-Sep-2010    Matthew L. Fidler  
;;    Only try to start tab completion if completion engine isn't running.
;; 17-Sep-2010    Matthew L. Fidler

;;    Bug fix for (1) moving point to end of symbol (2) pressing return, (3)
;;    going to first part of line... Didn't decompose symbol when moving
;;    mouse....

;; 17-Sep-2010    Matthew L. Fidler  
;;    Added bug-fix for YaSnippet.
;; 17-Sep-2010    Matthew L. Fidler  
;;    Added back esn-align-input-bind to INPUT records.
;; 14-Sep-2010    Matthew L. Fidler  

;;    Changed esn-upcase-buffer and esn-downcase-buffer to ignore format
;;    specifications starting with s and t.  Otherwise captilize or insert a
;;    capital character.

;; 14-Sep-2010    Matthew L. Fidler  
;;
;;    Changed pre-command hook to decompose regions based on text-properties
;;    instead of regular expressions (speed increase)
;;
;; 13-Sep-2010    Matthew L. Fidler  
;;    Changed magic return to be a simple newline and indent.
;; 10-Sep-2010    Matthew L. Fidler  
;;    Bug fix for upcase and downcase buffer.
;; 08-Sep-2010    Matthew L. Fidler  
;;    Added Bug-fixes for Completion-UI...
;; 07-Sep-2010    Matthew L. Fidler  
;;    Added "TAB" key to start completion
;; 01-Sep-2010    Matthew L. Fidler  
;;    Updated upcase-char-insert.  Also updated upcase and downcase buffer.
;; 30-Aug-2010    Matthew L. Fidler
;;    Moved advices into esn-advices
;; 23-Aug-2010    Matthew L. Fidler  

;;    Made newline a defalias instead of an explicit key (makes auto-complete
;;    mode return function correctly).
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize record regular expressions. 
;; 27-Jul-2010    Matthew L. Fidler  
;;    Added magic ? that opens help when esn-nm-help-dir is defined.
;; 21-Jul-2010    Matthew L. Fidler  
;;    Changed backward-delete-word to change (x FIXED) to either (0 x) when positive or (x) when negative or zero.
;; 12-Jul-2010    Matthew L. Fidler
;;    Broke out the seed generation to use in completion options such as SEED=x
;; 04-May-2010    Matthew L. Fidler
;;    Added asterisk bug-fix
;; 04-May-2010    Matthew L. Fidler
;;    Made compact $PROBLEM statements return to comments and have *, tab, and
;;    backspace special features.
;; 04-May-2010    Matthew L. Fidler
;;    Changed return behavior after $PROBLEM statement to create a new line (optionally)
;; 22-Apr-2010    Matthew L. Fidler
;;    Added Error Handling
;; 19-Apr-2010    Matthew L. Fidler
;;    Added bug-fix for deleting THETA(x)
;; 17-Feb-2010    Matthew L. Fidler
;;    Took out arglist for pre-command-hook-a
;; 11-Feb-2010    Matthew L. Fidler
;;    Changed counting table to an idle timer.
;; 09-Feb-2010    Matthew L. Fidler
;;    Added Magic wrap idle timer
;; 01-Feb-2010    Matthew L. Fidler
;;    Added wrap to be called when the next line is shorter than the current line.
;; 01-Feb-2010    Matthew L. Fidler
;;    Change wrap to be only called when the current line is over the default
;;    wrapping length.
;; 28-Jan-2010    Matthew L. Fidler
;;    Allow Xmind completion and also allow smart capitalization revisions.
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
                                        ;(require 'esn-fun)
                                        ;(require 'esn-which)
                                        ;(eval-when-compile
                                        ;  (require 'esn-version-switch))
                                        ;(require 'esn-start)
;;(declare-fucntion esn-rec3 "esn-fun")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-last-record-same-p "esn-narrow")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-current-rec "esn-narrow")
(declare-function esn-indent-line "esn-indent")
(declare-function esn-after-completion "esn-completion")
(declare-function esn-is-abbrev-p "esn-narrow")
(declare-function esn-80-clear "esn-80")
(declare-function esn-80-show "esn-80")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-number-est "esn-align")
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-nm-help "esn-help")
(declare-function esn-error "esn-exec")
(declare-function esn-space-input "esn-input")
(declare-function esn-desc-subroutines "esn-space")
(declare-function esn-fill-record "esn-fill")

;; esn-number-estimates, esn-mode-use-cov-type-labels,
;; esn-update-editable-regions, esn-update-modification-line

(eval-when-compile
  (require 'esn-which)
  (require 'esn-fun))
(random 't)
(require 'esn-rec-hooks)
(require 'esn-vars)
(require 'esn-options)
(require 'esn-autoloads)

(defvar esn-enter-record-hook '()
  "* Hook called when entering a record")
(defvar esn-exit-record-hook '()
  "* Hook called when exiting a record")

(defvar esn-always-post-command-hook '()
  "Hooks that are always called.")


(make-variable-buffer-local 'esn-last-record-start)
(make-variable-buffer-local 'esn-last-record-name)

(require 'esn-rec-hooks)

(require 'esn-macros)
;; Call default wrapping on all known records except abbreviated, and table, input, bind, theta, omega and sigma.
(esn-not-abbrev-modification-hook-2 'esn-wrap ("tab" "inp" "bin" "the" "ome" "sig"))


;; Fast keys

;;;###autoload
(defun esn-before-change-functions-hook (beg end)
  "Called when in EsN mode on change of buffer."
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (unless esn-run-save-fn
      (condition-case error
          (save-excursion
            (save-restriction 
              (when beg
                (goto-char beg)
                (let* ((rec (esn-get-current-record t))
                       (mark-active mark-active)
                       (rec-hook (concat "esn-" (downcase rec) "-modification-hook"))
                       (hook (intern-soft rec-hook)))
                  (when hook
                    (condition-case error
                        (run-hooks hook)
                      (error
                       (message "Error running `%s': %s" rec-hook (error-message-string error))))))
                (goto-char end)
                (unless (esn-last-record-same-p t)
                  (let* ((rec (esn-get-current-record))
                         (rec-hook (concat "esn-" (downcase rec) "-modification-hook"))
                         (hook (intern-soft rec-hook)))
                    (when hook
                      (condition-case error
                          (run-hooks hook)
                        (error
                         (message "Error running `%s': %s" rec-hook (error-message-string error))))))))))
        (error
         (message "Error in `esn-before-change-functions-hook': %s" (error-message-string error)))))))

(defun esn-post-command-hook (&rest ignore)
  "Define a timer to run the esn-post-command-hook so that it doesn't incessantly redraw if not needed."
  (interactive)
  ;;  (message "Last Command: %s" last-command)
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (let ((inhibit-read-only 't)
          (inhibit-point-motion-hooks 't)
          (mark-active mark-active))
      (when (and (eq major-mode 'esn-mode)
                 (not esn-run-save-fn)
                 (not (memq last-command '(tabbar-select-tab-callback
                                           ignore
                                        ;mouse-drag-region
                                           ))))
        (setq esn-var-names '())
        (let ((exit-hook esn-exit-record-hook) (enter-hook esn-enter-record-hook))
          (condition-case error
              (progn
                (esn-get-current-rec)
                (when (or
                       (and esn-last-record-start (not esn-get-current-record-start))
                       (and (not esn-last-record-start) esn-get-current-record-start)
                       (and esn-last-record-start esn-get-current-record-start
                            (not (= esn-last-record-start esn-get-current-record-start))))
                  ;; Run Exit record hook first.
                  (condition-case error
                      (progn
                        (when (and esn-last-record-start esn-last-record-name
                                   (not (string= (esn-current-rec) esn-last-record-name)))
                          (save-excursion
                            (let (p1
                                  p2
                                  no-match)
                              (goto-char esn-last-record-start)
                              (when (re-search-backward (esn-reg-record-exp esn-last-record-name) nil 't)
                                (setq p1 (match-end 0)))
                              (goto-char esn-last-record-start)
                              (when (re-search-forward (esn-reg-record-exp esn-last-record-name) nil 't)
                                (setq p2 (match-end 0)))
                              (cond
                               ( (and p1 p2 (> (- p2 esn-last-record-start) (- esn-last-record-start p1)))
                                 ;; Distance between p2 and last record location is greater than
                                 ;; the distance between p1 and last record location.  p1 is the best point.
                                 (goto-char p1))
                               ( (and p1 p2)
                                 (goto-char p2))
                               ( p1
                                 (goto-char p1))
                               ( p2
                                 (goto-char p2))
                               ( 't
                                 (setq no-match 't)))
                              (unless no-match
                                (skip-chars-forward " \t\n")
                                (run-hooks 'esn-exit-record-hook))))))
                    (error
                     (message "ESN: Exit Record Hook Error: %s; Hooks: %s" (error-message-string error) exit-hook)))
                  (unless (string= "" (esn-get-current-rec))
                    (run-hooks 'esn-enter-record-hook))))
            (error
             (message "ESN: Enter Record Hook Error: %s; Hooks: %s" (error-message-string error) enter-hook))))
        (condition-case err
            (save-excursion
              (when esn-was-at-indent-keyword
                (goto-char esn-was-at-indent-keyword)
                (unless (or 
                         (looking-at esn-mode-deindent-keyword-regexp)
                         (looking-at esn-mode-deindent-indent-keyword-regexp)
                         (looking-at esn-mode-indent-keyword-regexp))
                  (esn-indent-line))))
          (error (message "Error while fixing indentation, post: %s" (error-message-string err))))
        (let* ((rec (esn-get-current-record))
               (rec-hook (concat "esn-" (downcase rec) "-post-command-hook"))
               (hook (intern-soft rec-hook)))
          (if hook
              (condition-case error
                  (progn
                    (run-hooks hook))
                (error
                 (message "Error running `%s': %s" rec-hook (error-message-string error))))))
        (condition-case error
            (progn
              (run-hooks 'esn-always-post-command-hook))
          (error
           (message "Error running persistent post-command hooks: %s" (error-message-string error))))
        (condition-case err
            (cond
             ( (and last-command-event (memq last-command-event (list ?\t 'tab)))
               ;; Finish tabs.
               (when (and esn-pre-command-point (= esn-pre-command-point (point)))
                 (when (looking-back "\\<[$]?[A-Za-z0-9_]*[ \t=]*")
                   (esn-after-completion (match-string 0))))))
          (error
           (message "Post Tab/Return Error: %s" (error-message-string err))))))))

;;
;;(if esn-use-hyperlinks
;;    (esn-make-link-overlays)
;;  (esn-rm-link-overlays))
(defvar esn-pre-command-point nil
  "* Defines point for pre-command-hook"
  )
(make-variable-buffer-local 'esn-pre-command-point)
(defvar esn-was-at-indent-keyword nil
  "* Defines if the line was at a indent keyword."
  )
(make-variable-buffer-local 'esn-was-at-indent-keyword)

(defvar esn-last-file-name nil
  "Last file name of command")

(defun esn-pre-command-hook ()
  "Esn's pre command hook"
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (unless esn-run-save-fn
      (setq esn-last-file-name (buffer-file-name))
      (setq esn-last-record-name (esn-get-current-rec))
      (setq esn-last-record-start esn-get-current-record-start)
      (setq esn-pre-command-point (point))
      (esn-pre-command-hook-run))))

(defun esn-pre-command-hook-run ()
  "* Wrapped around an error handler"
  (setq esn-font-lock-bad-mus-save nil)
  (setq esn-font-lock-bad-mus-cached nil)
  (condition-case err
      (save-excursion
        (if (not (save-excursion (skip-chars-backward " \t\n") (esn-is-abbrev-p)))
            (setq esn-was-at-indent-keyword nil)
          (goto-char (point-at-bol))
          (skip-chars-forward " \t")
          (if (or 
               (looking-at esn-mode-deindent-keyword-regexp)
               (looking-at esn-mode-deindent-indent-keyword-regexp)
               (looking-at esn-mode-indent-keyword-regexp))
              (progn
                (setq esn-was-at-indent-keyword (point))
                (esn-indent-line)))))
    (error
     "Error while fixing indentation: " (error-message-string err)))
  (condition-case err
      (progn
        (unless (memq last-command '(
                                     set-mark
                                     cua-set-mark
                                     down-list
                                     up-list
                                     end-of-defun
                                     beginning-of-defun
                                     backward-sexp
                                     forward-sexp
                                     backward-up-list
                                     forward-paragraph
                                     backward-paragraph
                                     end-of-buffer
                                     beginning-of-buffer
                                     backward-word
                                     forward-word
                                     mwheel-scroll
                                     backward-word
                                     forward-word
                                     move-end-of-line
                                     move-beginning-of-line
                                     backward-char
                                     forward-char
                                     scroll-up
                                     scroll-down
                                     scroll-left
                                     scroll-right
                                     next-buffer
                                     previous-buffer))
          (when esn-use-symbols
            (unless (= (point) (point-min))
              (let ((tmp (get-text-property (- (point) 1) 'composition)))
                (when tmp
                  (decompose-region (- (point) (nth 1 tmp)) (point))))))
          (setq esn-skip-purpose nil)
          (setq esn-var-names '())))
    (error
     (message "EsN pre-command hook error: %s" (error-message-string err)))))

;;
(defun esn-window-scroll-functions (window display-start)
  "update scroll for ESN"
  (let (
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (buffer-undo-list 't)
        )
    (esn-80-clear)
    (setq esn-80-last-window-start nil)
    (setq esn-80-last-window-height nil)
    (setq esn-80-last-line nil)
    (setq esn-80-last-buffer-name nil)
    (when esn-highlight-over
      (esn-80-show))))

(defun esn-window-size-change-functions (frame)
  (let (
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (buffer-undo-list 't)
        )
    (esn-80-clear)
    (setq esn-80-last-window-start nil)
    (setq esn-80-last-window-height nil)
    (setq esn-80-last-line nil)
    (setq esn-80-last-buffer-name nil)
    (when esn-highlight-over
      (esn-80-show))))

(defun esn-magic-adjust-cur-rec-bounds (&optional pre)
  "* Adjusts the current record bound on character insert"
  (interactive "p")
  (when (and  esn-get-current-record-start
              esn-get-current-record-stop
              esn-get-current-record-val
              (>= (point) esn-get-current-record-start)
              (<= (point) esn-get-current-record-stop)
              )
    (setq esn-get-current-record-stop (+ (or pre 1) esn-get-current-record-stop))
    (setq esn-get-current-record-eof (+ (or pre 1) esn-get-current-record-eof))))

(defun esn-gen-seed ()
  "Generate NONMEM Seed based on uniform random selection"
  ;; Note in the NONMEM manual, it states the maximum seed number is
  ;; 21,474,836,447.

  ;; However, in C and C++, the maximum signed intger number is
  ;; 2,147,483,647.  Perhaps they repeated the 4?

  ;; Tested actual number is 2,147,483,647.

  ;; To be safe, this generates seeds from 0 - 2,147,483,647.
  ;; They are not quite uniformly distributed, though.  However, every
  ;; seed is possible.

  ;; The range of values for an integer depends on the machine. The minimum
  ;; range is -134,217,728 to 134,217,727 (28 bits; i.e., to but some machines
  ;; may provide a wider range.
  ;; A safe seed  to give numbers from 0 - 99,999,999 is 100,000,000

  ;; This is a two-step process (due to number liminations in Emacs)

  ;; First generate a number from 0-21

  ;; If that number is 21 then
  ;; Generate a number from 0-47,483,677, concatenate the two.
  
  ;; Otherwise generate a number from 0-99,999,999.  Again, concatenate the two.

  ;; Assuming that the numbers are uniform, the 0-20 should be selected 97.788% of the time, and 21 should be selected 2.212% of the time.
  ;; This is different from the uniform distribution of  ~4% per selection.

  ;; To overcome this, a random number from 0-100,000 will be generated.  If
  ;; this number is smaller than 97,788, generate a number from 0-20,
  ;; otherwise use 21.  This will give a close approximation to uniformly
  ;; selecting 0 - 2,147,483,647, though emacs can't represent numbers that
  ;; high.

  ;; To give the lower seed range,
  ;; The second section 99,999,999
  (let (
        (rand1 (random 100001))
        (rand2 nil)
        (str "")
        )
    (if (< rand1 97788)
        (progn
          (setq rand1 (random 21))
          (setq rand2 (number-to-string (random 100000000)))
          (if (> 8  (length rand2))
              (setq rand2 (concat (make-string (- 8 (length rand2)) ?0)
                                  rand2))
            )

          (setq str (concat (number-to-string rand1) rand2))
          )
      (setq rand1 21)
      (setq rand2 (number-to-string (random 47483648)))
      (if (> 8  (length rand2))
          (setq rand2 (concat
                       (make-string (- 8 (length rand2)) ?0)
                       rand2))
        )
      (setq str (concat (number-to-string rand1) rand2))
      )
    (when (string-match "^0+" str)
      (setq str (replace-match "" nil nil str))
      )
    (symbol-value 'str)))

(defun esn-magic-start-paren (&optional pre)
  " Magic ( function.  Adds a random seed for the $SIMULATION record when inside, otherwise, just insert self."
  (interactive "p")
  ;;(esn-magic-adjust-cur-rec-bounds pre)
  (if pre
      (let (
            (i 0))
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-start-paren)))
    (if (and overwrite-mode (not (looking-at "$")))
        (delete-char 1))
    (let (
          (double nil))
      (if (or (not esn-sim-add-seed)
              (save-excursion
                (let (
                      (ret 't)
                      )
                  (when (string= (esn-get-current-record) "SIM")
                    (re-search-backward (eval-when-compile (esn-reg-record-exp "SIM" 't)) nil t)
                    (if (re-search-backward (eval-when-compile (esn-reg-record-exp "SIM" 't)) nil t)
                        (setq double 't)
                      (setq ret nil)
                      )
                    )
                  (symbol-value ret))))
          (if double
              (insert "(-1")
            (if (and (featurep 'completion-ui) auto-completion-mode)
                (auto-completion-self-insert (string-to-char "("))
              (insert "(")))
        (if (and overwrite-mode (not (looking-at "$")))
            (delete-char 1))
        (insert (concat "(" (esn-gen-seed)))))))

(defun esn-magic-semi (&optional pre)
  "Magic semi-colon function.  Appropriately skips on automatic numbering and spacing.  Also if on blank line deletes preceeding crap."
  (interactive "p")
  (if pre
      (let (
            (i 0))
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-semi)))
    ;;  (esn-magic-adjust-cur-rec-bounds)
    (save-restriction
      (if (and overwrite-mode (not (looking-at "$")))
          (delete-char 1))
      (let (
            (rec (esn-get-current-record))
            (double nil)
            (nothing nil))
        ;; Put at beginning of line
        (save-excursion
          (when (and nil (re-search-backward "^[ \t]*\\=" nil t))
            (replace-match ";")
            (setq nothing 't)))
        (unless nothing
          (save-excursion
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq double (re-search-forward "\\<SAME\\>" nil t))
            (widen))
          (if double
              (insert ";;")
            (insert ";"))
          (when (and
                 (esn-number-estimates)
                 (string= rec "THE"))
            (esn-number-est)
            (re-search-forward "\\=[Cc;]* *\\(?:THETA(\\)?[0-9]+\\(?:)\\)? *- *\\[? *" nil t)
            (when (and (esn-use-pdx-p) esn-mode-auto-generate-brackets-for-theta)
              (re-search-forward "\\=[ \t]*\\[" nil t)))
          (when (and (or
                      (esn-number-estimates)
                      (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels))
                     (string= rec "OME"))
            (esn-number-est "OME")
            (when (and
                   (esn-use-pdx-p)
                   esn-mode-use-omega-sigma-var-type-labels)
              (re-search-forward "\\=[Cc;]*[ \t]*\\[[APFapf]\\]"))
            (re-search-forward "\\=[Cc;]*[ \t]*\\(?:ETA(\\)?[0-9]+\\(?:)\\)?[ \t]*-[ \t]*" nil t))
          (when (and (or
                      (esn-number-estimates)
                      (and esn-mode-use-pdx esn-mode-use-omega-sigma-var-type-labels))
                     (string= rec "SIG"))
            (esn-number-est "SIG")
            (when (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels)
              (re-search-forward "\\=[Cc;]*\\[[APFapf]\\]"))
            (re-search-forward "\\=[Cc;]* *\\(?:EPS(\\)?[0-9]+\\(?:)\\)? *- *" nil t))
          (when (esn-mode-use-cov-type-labels)
            (re-search-forward "\\= *COV\\> *" nil t)))))))

(defun esn-magic-? (&optional pre)
  "Magic ? function; Produces Help when outside of comments"
  (interactive "p" )
  (if pre
      (let (
            (i 0)
            )
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-?)))
    (if (or 
         (not esn-use-magic-help-key)
         (esn-in-comment-p)
         (save-excursion
           (re-search-backward "[ \t]*\".*\\=" nil t)))
        (progn
          (if (and overwrite-mode (not (looking-at "$")))
              (delete-char 1))
          (if (and (featurep 'completion-ui) auto-completion-mode)
              (auto-completion-self-insert (string-to-char "?"))
            (insert "?")))
      (esn-nm-help))))

(defun esn-magic-quote (&optional pre)
  "Magic Quote function"
  (interactive "p")
  (esn-magic-$ pre "\""))

(defun esn-magic-$ (&optional pre char)
  "Magic Dollar Sign function"
  (interactive "p")
  (if pre
      (let (
            (i 0))
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-$ nil char)))
    (let (
          (dollar (or char "$"))
          )
      ;;    (esn-magic-adjust-cur-rec-bounds)
      (if esn-fix-records
          (save-excursion
            (cond
             ( (re-search-backward "^[ \t]*\\=" nil t)
               (replace-match ""))
             ( (esn-in-comment-p)
               ;; Do nothing
               )
             ( (string= dollar "$")
               ;; Add Return.
               (setq dollar "\n$")))))
      (if (and overwrite-mode (not (looking-at "$")))
          (delete-char 1))
      (if (and (featurep 'completion-ui) auto-completion-mode)
          (auto-completion-self-insert (string-to-char dollar))
        (insert dollar)))))

(defun esn-magic-! (&optional pre)
  "Magic ! function.  Appropriately indents WFN autocovariate model records ;!"
  (interactive "p")
  (if pre
      (let (
            (i 0)
            )
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-!))
        ;;  (esn-magic-adjust-cur-rec-bounds)
        (save-excursion
          (if (re-search-backward "^[ \t]*;\\=" nil t)
              (replace-match ";"))))
    (if (and overwrite-mode (not (looking-at "$")))
        (delete-char 1))
    (if (and (featurep 'completion-ui) auto-completion-mode)
        (auto-completion-self-insert (string-to-char "!"))
      (insert "!"))))

(defun esn-magic-bs-ok (pt)
  "Returns if a point is read-only intangible, or invisable"
  (save-excursion
    (or (get-char-property pt 'read-only)
        (get-char-property pt 'intangible)
        (get-char-property pt 'invisible)
        )))

(defun esn-magic-bs-del ()
  (interactive)
  (if (memq last-input-event '(delete \C-d 4 deletechar \M-f) )
      (if (or (and (boundp 'delete-key-deletes-forward) delete-key-deletes-forward)
              (and (boundp 'normal-erase-is-backspace) normal-erase-is-backspace))
          (esn-magic-bs 't)
        (esn-magic-bs)
        )
    (esn-magic-bs)))

(defun esn-magic-asterisk (&optional pre)
  "Magic Asterisk Key, allows easy \"bulleting\" in comments"
  (interactive "p")
  ;;  (esn-magic-adjust-cur-rec-bounds pre)
  (if pre
      (let (
            (i 0)
            )
        (while (< i pre)
          (setq i (+ i 1))
          (esn-magic-asterisk)))
    (if (and overwrite-mode (not (looking-at "$")))
        (delete-char 1))
    (let (
          (tmp1 "")
          any-pro
          edit-begin
          edit-end)
      (setq edit-begin
            (concat "^\\( *;[;C]* *\\)"
                    (regexp-opt
                     (mapcar
                      (lambda(x)
                        (nth 0 x)
                        )
                      (esn-update-editable-regions))
                     't)))
      (setq edit-end
            (concat "^\\( *;[;C]* *\\)"
                    (regexp-opt
                     (mapcar
                      (lambda(x)
                        (if (string= "$PRO" (nth 0 x))
                            (setq any-pro (nth 1 x))
                          )
                        (nth 1 x)
                        )
                      (esn-update-editable-regions))
                     't)))
      (if (not (and (save-excursion
                      (re-search-backward "\n *;[;Cc]* *\\=" nil t)
                      )
                    (not (looking-at " *[*]"))
                    (or
                     (esn-is-between edit-begin edit-end)
                     (and any-pro
                          (esn-is-between (eval-when-compile (esn-reg-record-exp "PRO" 't))
                                          (regexp-quote any-pro))))))
          (if (and (featurep 'completion-ui) auto-completion-mode)
              (auto-completion-self-insert (string-to-char "*"))
            (insert "*"))
        (skip-chars-backward " \t")
        (when (looking-at "[ \t]*")
          (replace-match ""))
        (setq tmp1 (esn-update-modification-line))
        (when (string-match "\\( +\\)[*]" tmp1)
          (setq tmp1 (match-string 1 tmp1)))
        (insert tmp1)
        (insert "*")
        (insert " ")))))

(defun esn-magic-bs (&optional forward)
  "* Magic backspace record that makes sure wrapping is invoked."
  (let (
        (case-fold-search 't)
        (last nil)
        (pt (point))
        (pt2 nil)
        (len1 nil)
        (len2 nil)
        (looking-at-tos nil)
        (move nil)
        (at-beg-or-end nil)
        (rec (esn-get-current-record))
        edit-begin
        edit-end
        any-pro)
    (setq edit-begin
          (concat "^\\( *;[;C]* *\\)"
                  (regexp-opt
                   (mapcar
                    (lambda(x)
                      (nth 0 x)
                      )
                    (esn-update-editable-regions))
                   't)))
    (setq edit-end
          (concat "^\\( *;[;C]* *\\)"
                  (regexp-opt
                   (mapcar
                    (lambda(x)
                      (if (string= "$PRO" (nth 0 x))
                          (setq any-pro (nth 1 x))
                        )
                      (nth 1 x)
                      )
                    (esn-update-editable-regions))
                   't)))
    (if (and (not forward)
             (save-excursion
               (re-search-backward "\n *;[;Cc]* *\\=" nil t)
               )
             (or
              (esn-is-between edit-begin edit-end)
              (and any-pro
                   (esn-is-between (eval-when-compile (esn-reg-record-exp "PRO" 't))
                                   (regexp-quote any-pro)))))
        (save-excursion
          (when (re-search-backward "\n *;[;Cc]* *\\=" nil t)
            (replace-match "")))
      (if (and forward
               (looking-at "\n *;[;Cc]* *")
               (or
                (esn-is-between edit-begin edit-end)
                (and any-pro
                     (esn-is-between (eval-when-compile (esn-reg-record-exp "PRO" 't))
                                     (regexp-quote any-pro)))))
          (save-excursion
            (if (looking-at "\n *;[;Cc]* *\\([^ ]\\)")
                (replace-match " \\1")
              (looking-at "\n *;[;Cc]* * ")
              (replace-match "")))
        (unless (or (and (or
                          (esn-number-estimates)
                          (and (esn-use-pdx-p) esn-mode-use-omega-sigma-var-type-labels)
                          (esn-mode-use-cov-type-labels))
                         (string-match "\\(THE\\|OME\\|SIG\\)" rec)
                         (save-excursion
                           (when (esn-mode-use-cov-type-labels)
                             (when (and (not forward) (re-search-backward
                                                       "[^;]\\(;[ \t]*COV\\>\\)\\=" nil t))
                               (forward-char 1)
                               (delete-char (length (match-string 1)))))
                           (when (and (not forward) (re-search-backward "[^;]\\(;\\[\\=\\|;[;C]*\\([ \t]*\\[[APFapf]\\]\\)?[ \t]*\\(?:\\(?:THETA\\|EPS\\|ETA\\)(\\)?[0-9]+\\(?:)\\)?[ \t]*\\([=-]\\)?\\(?:[ \t]*\\[\\)?\\(?:[ \t]*\\][ \t]*\\)?\\=\\)" nil t))
                             (forward-char 1)
                             (let ((tmp (match-string 1)))
                               (message "%s" (substring tmp -1))
                               (delete-char (length tmp))
                               (when (and (string=
                                           (substring tmp -1) "[")
                                          (looking-at "[ \t]*\\]"))
                                 (replace-match "")))
                             't)))
                    ;; Delete ;C or ;; in records.
                    (save-restriction
                      (save-excursion
                        (let ((what nil))
                          (when (and (not forward)
                                     (progn
                                       (when (re-search-backward ";[C;]\\=" nil t)
                                         (setq what (match-string 0))
                                         't))
                                     (save-excursion
                                       (when (re-search-backward "^[^;]*\\=" nil t)
                                         (setq what (format " \\{%s\\}%s" (length (match-string 0)) what))
                                         't))
                                     (esn-narrow-rec))
                            ;; Now Delete
                            (re-search-forward "\\=;[C;]" nil t)
                            (backward-char 1)
                            (delete-char 1)
                            (end-of-line)
                            (skip-chars-forward " \t\n")
                            (beginning-of-line)
                            (while (re-search-forward what nil t)
                              (backward-char 1)
                              (delete-char 1)
                              (end-of-line)
                              (skip-chars-forward " \t\n")
                              (beginning-of-line))
                            't)))))
          (if (or (and (not forward) (bobp))
                  (and forward (eobp)))
              (progn
                (esn-error "Can't delete anymore.")
                )
            (save-excursion
              (if forward
                  (progn
                    (forward-char 1)
                    (setq at-beg-or-end (eobp))
                    )
                (backward-char 1)
                (setq at-beg-or-end (bobp))))
            (if (esn-magic-bs-ok (+ (point) (if forward 0 -1)))
                (progn
                  (esn-error "Can't delete unwritable region of buffer.")
                  )
              (if at-beg-or-end nil
                (if (esn-magic-bs-ok (+ (point) (if forward 1 -2)))
                    (progn
                      (let (
                            (inhibit-point-motion-hooks 't)
                            (inhibit-read-only 't)
                            )
                        (if forward
                            (progn
                              )
                          (backward-char 2)
                          (if (looking-at ":")
                              (progn
                                (forward-char 1)
                                (if (looking-at "[ \t]")
                                    (progn
                                      (insert " ")
                                      (message "Cannot delete the extra space.")))
                                (forward-char 1))
                            (forward-char 1)
                            (if (looking-at "\n")
                                (progn
                                  (insert "\n")
                                  (message "Cannot delete the extra return.")))
                            (forward-char 1)))))))))
          (save-excursion
            (if (re-search-backward "\\<\\(THETA\\|ETA\\|EPS\\)\\>([0-9]+)\\=" nil t)
                (setq looking-at-tos (point))))
          (if looking-at-tos
              (decompose-region looking-at-tos (point)))
          (if esn-delete-comment-if-deleting-item
              (save-excursion
                (if forward
                    (forward-char 1))
                (if (not (looking-at " *;")) nil
                  (backward-char 1)
                  (if (looking-at "[^ \n;]")
                      (setq last (re-search-backward "^ *\\=" nil t))))))
          (if (not last)
              (if forward
                  (delete-char 1)
                (delete-backward-char 1))
            (save-excursion
              (if forward
                  (forward-char 1))
              (skip-chars-forward " \t")
              (setq pt2 (point))
              (beginning-of-line)
              (setq len1 (- pt (point)))
              (setq len2 (- pt2 (point)))
              (setq len2 (+ len2 (- len2 len1))))
            (if forward
                (forward-char 1))
            (delete-backward-char 1)
            (beginning-of-line)
            (while (looking-at (format " \\{%s,%s\\};.*\n" len1 len2))
              (replace-match ""))
            (skip-chars-backward "\n"))
          (setq last nil)
          (if forward
              (progn
                (save-excursion
                  (if (looking-at " *;")
                      (setq last (re-search-backward "^ *\\=" nil t)))
                  )
                (if last
                    (skip-chars-backward " "))))
          (if (not move) nil
            (if forward
                (forward-char 1)
              (backward-char 1))))))))
(defun esn-magic-del ()
  "Magic delete record, where wrapping is invoked."
  (interactive)
  (esn-magic-bs 't))

(defun esn-wrap ()
  (interactive)
  "* Call record wrap"
  (esn-magic-wrap))

(defun esn-magic-wrap (&optional char rec force)
  "* Magic Wrap Timer"
  (if esn-magic-wrap-timer
      (cancel-timer esn-magic-wrap-timer))
  (if force
      (esn-magic-wrap-actual char rec force)
    (if rec
        (esn-magic-wrap-actual char rec force)
      (if char
          (esn-magic-wrap-actual char rec force)
        (setq esn-magic-wrap-timer (run-with-timer 0.25 nil 'esn-magic-wrap-actual))))))

(defun esn-magic-wrap-actual (&optional char rec force)
  "*Wrapping function called on spaces, returns, tabs and characters."
  (interactive)
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (unless esn-skip-wrap
      (when (eq major-mode 'esn-mode)
        (if (or (string= (esn-current-rec) "TAB")
                (string= (esn-current-rec) "INP")
                (string= (esn-current-rec) "BIN"))
            (progn
              (save-restriction
                (esn-narrow-rec)
                (message "%s" (buffer-string)))
              (esn-align-tab-hook))
          
          ;; Wrapping when:
          ;; (1) Current column is greater than fill column
          ;; (2) End of line is greater than fill column
          ;; (3) This column is smaller than next column, and not the beginning of a new record.
          (let (end-column-1 end-column-2)
            (when (or
                   (> (current-column) fill-column)
                   (> (save-excursion (end-of-line) (setq end-column-1 (current-column)) (symbol-value 'end-column-1))
                      fill-column)
                   (< end-column-1 (save-excursion (forward-line 1) (beginning-of-line)
                                                   (if (looking-at "[ \t]$") (symbol-value 'end-column-1)
                                                     (end-of-line) (setq end-column-2 (current-column)) (symbol-value 'end-column-2)))))
              (let (
                    (case-fold-search 't)
                    (rec (or rec (esn-get-current-record)))
                    (comment nil)
                    (updated nil)
                    ;; comment wrapping
                    (tmp nil)
                    (semi nil)
                    (point1 (point))
                    (point2 nil)
                    (spaced nil)
                    (ext (regexp-opt (append
                                      (mapcar (lambda (what)
                                                (substring what 1 (length what))
                                                ) esn-default-extension)
                                      (list
                                       (substring esn-table-extension 1 (length esn-table-extension))
                                       (substring esn-msfo-est 1 (length esn-msfo-est))
                                       (substring esn-msfo-non 1 (length esn-msfo-non)))))))
                                        ;    (message "%s" rec)
                (setq point1 nil)
                (setq point2 nil)
                (if esn-fix-records
                    (save-excursion
                      (cond
                       (
                        (save-excursion
                          (beginning-of-line)
                          (looking-at (eval-when-compile (format "^\\([ \t]+\\)%s" (esn-reg-records 't))))
                          )
                        (if (looking-at "[ \t]+")
                            (replace-match ""))
                        )
                       ( (re-search-backward "[ \t]+[$].*?\\=" nil t)
                         (when (looking-at "[ \t]+")
                           (replace-match "\n"))))))
                ;; Space the following before wrap.
                (when (string= "INP" rec)
                  (setq spaced (esn-space-input rec))
                  )
                (when (string= "SUB" rec)
                  (setq spaced (or spaced (esn-desc-subroutines rec)))
                  )
                (save-excursion
                  (if (re-search-backward
                       (format "\\(;+\\(?:%s\\|!\\)? *\\)?.*\\=" esn-sub-begin) nil t)
                      (setq comment (match-string 1))))
                (if char
                    (insert char))
                (if (or (and
                         esn-wrapping-of-records
                         (or force
                             (not
                              (string-match
                               (concat "^\\(\\|"
                                       (regexp-opt  esn-records-not-wrapped)
                                       "\\)$") rec))))
                        (or
                         (and esn-update-theta (string= rec "THE"))
                         (and esn-update-omega (string= rec "OME"))))
                    (progn
                      (setq updated (esn-fill-record rec))))
                (if (and comment (not updated))
                    (save-excursion
                      (beginning-of-line)
                      (if (not (looking-at "^.*$")) nil
                        (if (<= (length (match-string 0)) esn-character-limit)
                            (progn
                              )
                          (if (looking-at "^\\([^;\n]*?\\);")
                              (progn
                                (setq tmp (make-string (length (match-string 1)) ? ))
                                )
                            (setq tmp ""))
                          (save-excursion
                            (beginning-of-line)
                            (setq point1 (point)))
                          (forward-char esn-character-limit)
                          (if (not (re-search-backward "[/\\\\][^\n ]*\\=" nil t))
                              (esn-backward-w)
                            (skip-chars-forward "/\\"))
                          (skip-chars-backward " \t")
                          (if (not (looking-at ext)) nil
                            (skip-chars-backward ".")
                            (if (not (re-search-backward "[/\\\\][^\n ]*\\=" nil t))
                                (esn-backward-w)
                              ;;(backward-word)
                              (skip-chars-forward "/\\\\")))
                          (save-excursion
                            (beginning-of-line)
                            (setq point2 (point)))
                          (if (not (= point1 point2)) nil
                            (if (looking-at "[ \t]*")
                                (replace-match ""))
                            (insert (concat "\n" tmp comment " "))
                            (if (looking-at "[ \t]*")
                                (replace-match "")))))))))))))))

(defun esn-upcase-char-self-insert (&optional pre)
  "Inserts uppercase when appropriate."
  (interactive "p")
  ;;  (esn-magic-adjust-cur-rec-bounds pre)
  (esn-upcase-char-insert (make-string 1 last-input-event) pre))

(defvar esn-upcase-char-insert-sub-reg-insert (eval-when-compile 
                                                (let (
                                                      (sub (esn-current-record-options-help "SUB" "7"))
                                                      (lst '())
                                                      ret)
                                                  (mapc (lambda(x)
                                                          (when (and (eq (type-of (nth 1 x)) 'string) (string-match (regexp-quote "(usersub)") (nth 1 x)))
                                                            (add-to-list 'lst (esn-rec3 (nth 0 x)))))
                                                        sub)
                                                  (setq ret (regexp-opt lst))
                                                  (when (string-match "=" ret)
                                                    (setq ret (replace-match "[A-Za-z0-9_]*[ \t=]+" nil nil ret)))
                                                  (setq ret (concat "\\<" ret esn-reg-filename-partial))
                                                  (symbol-value 'ret)))
  "* Subroutines regular expression for upcase char insert.")

(defvar esn-upcase-char-insert-sub-reg-toggle (eval-when-compile 
                                                (let (
                                                      (sub (esn-current-record-options-help "SUB" "7"))
                                                      (lst '())
                                                      ret)
                                                  (mapc (lambda(x)
                                                          (when (and (eq (type-of (nth 1 x)) 'string) (string-match (regexp-quote "(usersub)") (nth 1 x)))
                                                            (add-to-list 'lst (esn-rec3 (nth 0 x)))))
                                                        sub)
                                                  (setq ret (regexp-opt lst 'words))
                                                  (when (string-match "=" ret)
                                                    (setq ret (replace-match "" nil nil ret)))
                                                  (symbol-value 'ret)))
  "* Subroutines regular expression for toggle case"
  )

(defun esn-upcase-char-insert (char &optional pre)
  "* Inserts a character and makes it upper case when needed."
  (interactive)
  (if pre
      (let ((i 0))
        (while (< i pre)
          (setq i (+ i 1))
          (esn-upcase-char-insert char)))
    (let (
          (case-fold-search 't)
          lower)
      (cond 
       ( (esn-in-comment-p) ; Lower case possible
         (setq lower 't))
       ( (and (boundp 'org-table-comment-editing)
              org-table-comment-editing)
         (setq lower t))
       ( (looking-back "[$][^ \t\n]*"))
       ( (and (esn-is-abbrev-p) (not (looking-back "^[ \t]*\".*")))  
         ;; No file names defined (other than quoted)
         )
       ( (looking-back (eval-when-compile (format "\\<FIL[A-Za-z0-9_]*[ \t=]+%s" esn-reg-filename-partial))) ;; FILE
         (setq lower 't))
       ( (and (string= "EST" (esn-current-rec)) ;; MSFO
              (looking-back (eval-when-compile (format "\\<MSF[A-Za-z0-9_]*[ \t=]+%s" esn-reg-filename-partial))))
         (setq lower 't))
       ( (and (string= "SUB" (esn-current-rec)) 
              (looking-back esn-upcase-char-insert-sub-reg-insert))
         (setq lower 't))
       ( (and (string= "DAT" (esn-current-rec))
              (looking-back "\\<IGN[A-Z0-9_a-z]*[ \t=]+['\"]?"))
         (setq lower 't))
       ( (looking-back (eval-when-compile (esn-reg-data-rec '("DAT" "MSF" "INC") 't))) ; File records
         (setq lower 't))
       ( (and (looking-back "\\<FOR[A-Za-z0-9_]*[ \t=]+")
              (string-match "[st]" char))
         (setq lower 't)))
      (if (and (featurep 'completion-ui) auto-completion-mode
               (eq 'auto-completion-self-insert (key-binding "A")))
          (if lower
              (auto-completion-self-insert (string-to-char char) ?w)
            (auto-completion-self-insert (string-to-char (upcase char)) ?w)
            )
        (if lower
            (insert char)
          (insert (upcase char)))))))

(defalias 'esn-upcase-buffer 'esn-change-case-buffer)
(defun esn-downcase-buffer ()
  (interactive)
  (esn-change-case-buffer 't))

(defun esn-change-case-buffer (&optional downcase)
  "* Converts appropriate regions to uppercase."
  (interactive)
  (save-excursion
    (save-match-data
      (save-restriction
        (let* (
               (p1 nil)
               (p2 nil)
               (comment-reg "\\(?:;\\|\\<\\$PRO[a-zA-Z]*\\|^[ \t]*C[ \t]+[^=\n \t]\\)")
               (record-reg (eval-when-compile (esn-reg-record-exp '("DAT" "MSF" "INC") 't)))
                                        ;       (option-reg "\\(FILE?\\|OTH\\(?:ER?\\)?\\|PK\\|ERROR?\\|MODEL?\\|DES\\|AES\\|TOL\\|INFN?\\|MSFO?\\) *[= \t]*")
                                        ;       (option-reg "\\(FILE\\|MSFO?\\)")
               (sub-reg esn-upcase-char-insert-sub-reg-toggle)
               (option-reg (format "\\<\\(FIL[A-Z0-9a-z_]*\\|MSF[A-Z0-9a-z_]*\\|%s\\)" sub-reg))
               (ignore-reg "\\<IGN[A-Za-z0-9_]*")
               (format-reg "\\<FOR[A-Za-z0-9_]*")
               (file-reg "\\<\\(?:[.]\\{1,2\\}[/\\\\]\\|[A-Z]:[/\\\\]\\|[/\\\\]\\)")
               (find-reg (format "\\(%s\\|%s\\|%s\\|%s\\|%s\\|%s\\)" comment-reg record-reg
                                 option-reg ignore-reg file-reg
                                 format-reg
                                 ))
               (found "")
               (rec "")
               (inhibit-read-only 't)
               (inhibit-point-motion-hooks 't)
               (case-fold-search 't)
               )
          (save-excursion
            (goto-char (point-min))
            (setq p1 (point))
            (while (re-search-forward find-reg nil t)
              (setq found (match-string 1))
              (setq p2 (point))
              (if downcase
                  (downcase-region p1 p2)
                (upcase-region p1 p2)
                )
              (cond
               ( (string-match comment-reg found)
                 (end-of-line)
                 )
               ( (string-match format-reg found)
                 (skip-chars-forward " \t,st=")
                 )
               ( (string-match record-reg found)
                 (re-search-forward (eval-when-compile (format "\\=[ \t\n]+%s" esn-reg-filename)) nil t)
                 )
               ( (string-match option-reg found)
                 (cond
                  (
                   (and (string-match sub-reg found)
                        (string= rec "SUB")
                        )
                   (message "Found Sub Option: %s" found)
                   (re-search-forward (eval-when-compile (format "\\=[ \t\n=]+%s" esn-reg-filename)) nil t)
                   )
                  ( 't
                    (message "Found Option %s" found)
                    (re-search-forward (eval-when-compile (format "\\=[ \t\n=]+%s" esn-reg-filename)) nil t)
                    )
                  )
                 )
               ( (string-match ignore-reg found)
                 (re-search-forward "\\= *[= \t] *['\"]?.['\"]?" nil t)
                 )
               ;; Don't fix anything in the the records below, otherwise assume its a file.
               ( (not (esn-is-abbrev-p))
                 (re-search-forward "\\=[^ \n]*" nil t)
                 )
               )
              (setq p1 (point))
              )
            (if downcase
                (downcase-region p1 (point-max))
              (upcase-region p1 (point-max)))))))))


;; Subroutines user routines

(defun esn-end-of-line ()
  "Goes to the end of line, unless in a boxed comment, then go to end of the comment line."
  (interactive)
  (end-of-line)
  )
(defun esn-magic-backtab ()
  "NONMEM's backtab function"
  (interactive)
  (let (
        edit-begin
        edit-end
        any-pro
        (tmp1 "")
        )
    (setq edit-begin
          (concat "^\\( *;[;C]* *\\)"
                  (regexp-opt
                   (mapcar
                    (lambda(x)
                      (nth 0 x)
                      )
                    (esn-update-editable-regions))
                   't)
                  )
          )
    (setq edit-end
          (concat "^\\( *;[;C]* *\\)"
                  (regexp-opt
                   (mapcar
                    (lambda(x)
                      (if (string= "$PRO" (nth 0 x))
                          (setq any-pro (nth 1 x))
                        )
                      (nth 1 x)
                      )
                    (esn-update-editable-regions))
                   't)))
    
    (if (or
         (esn-is-between edit-begin edit-end)
         (and any-pro
              (esn-is-between (eval-when-compile (esn-reg-record-exp "PRO" 't))
                              (regexp-quote any-pro))))
        (if (save-excursion
              (re-search-backward ";[;C]* *\\* *\\=" nil t)
              )
            ;;(esn-magic-return)
            (newline-and-indent)
          (save-excursion
            (beginning-of-line)
            (let (
                  (cur-comment nil)
                  (ln 0)
                  (tmp "")
                  )
              (when (looking-at "[ \t]*;[;C]* *\\*")
                (setq cur-comment (match-string 0))
                (save-match-data
                  (setq tmp cur-comment)
                  (when (string-match "^[ \t]*;[;C]*" tmp)
                    (setq tmp (replace-match "" nil nil tmp)))
                  (when (string-match "\\*$" tmp)
                    (setq tmp (replace-match "" nil nil tmp)))
                  (setq ln (length tmp))
                  (while (looking-at (format "[ \t]*;[;C]* \\{%s,\\}\\*?" ln))
                    (forward-line -1)
                    (beginning-of-line)
                    )
                  (looking-at "[ \t]*;[;C]* *\\*?")
                  (setq cur-comment (match-string 0))
                  )
                (replace-match cur-comment 't 't))))))))
                                        ;newline-and-indent  -- Return
                                        ;newline -- Return
(provide 'esn-magic-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-magic-keys.el ends here
