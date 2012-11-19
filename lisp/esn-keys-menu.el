;;; esn-keys-menu.el --- Menu Functionality and Key functionality
;;
;; Filename: esn-keys-menu.el
;; Description: Menu Functionality & Key Functionality
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Jan 26 14:41:13 2010 (-0600)
;; Version:  0.1
;; Last-Updated: Mon May  2 10:18:35 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 565
;; URL: http://esnm.sourceforge.net/
;; Keywords: keys, menu
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines the keys used for Emacs Speaks NONMEM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 10-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Dec  8 22:47:28 2010 (-0600) #560 (Matthew L. Fidler)
;;    Bugfix to `esn-plt-submit' menu
;; 08-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Dec  8 22:43:07 2010 (-0600) #555 (Matthew L. Fidler)
;;    Cleaned up esn-versions-menu.  No longer an evaulation buffer.  Took out "normal keys" advice.
;; 07-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Dec  7 14:02:49 2010 (-0600) #518 (Matthew L. Fidler)
;;    Changed "normal keys" to be an advice.  Helps with auto-completion modes.
;; 30-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 30 09:24:12 2010 (-0600) #497 (Matthew L. Fidler)
;;    Changed "Tools" menu to "EsN" menu.  Silly, There was already a tools menu.
;; 15-Nov-2010    MatthewL. Fidler
;;    Last-Updated: Mon Nov 15 11:21:39 2010 (-0600) #434 (Matthew L. Fidler)
;;    Changed to support only ONE keymap.  Fixes some external problems.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 14:29:00 2010 (-0500) #408 (Matthew L. Fidler)
;;    Added toggle variable for bug fix.  Need to track down where this comes from.
;; 27-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 27 12:26:09 2010 (-0500) #391 (Matthew L. Fidler)
;;    Made header menu part of the automation menu.
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 12:12:32 2010 (-0500) #389 (Matthew L. Fidler)
;;    Remove smart tab
;; 14-Sep-2010    Matthew L. Fidler
;;    Last-Updated: Tue Sep 14 10:30:39 2010 (-0500) #385 (Matthew L. Fidler)
;;    Changed esn-symbol menu.
;; 10-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 10 13:01:02 2010 (-0500) #359 (Matthew L. Fidler)
;;    Changed fontlock customizations to jump to customize buffer.
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 12:24:14 2010 (-0500) #345 (Matthew L. Fidler)
;;    Bug-fix.  Added Extended menu to Xpose-type menus.
;; 03-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep  3 17:30:32 2010 (-0500) #250 (Matthew L. Fidler)
;;    Added different menus depending on if PLT is active or PDX pop.
;; 24-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 24 11:57:50 2010 (-0500) #136 (Matthew L. Fidler)
;;
;;    Made switching the assumed version from the menu switch regular
;;    expressions.
;;
;; 13-Jul-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Jul 13 09:46:14 2010 (-0500) #103 (Matthew L. Fidler)
;;    Split out Header menu
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 13:39:52 2010 (-0500) #52 (Matthew L. Fidler)
;;    Added PLT WIDE option (for $DATA)
;; 14-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Mon Jun 14 10:09:07 2010 (-0500) #50 (Matthew L. Fidler)
;;    Removed option esn-mirror-problem-purpose.  Need to add it again
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 11:12:58 2010 (-0500) #47 (Matthew L. Fidler)
;;    Added support to .eta and .par tables in PDx pop.
;; 16-Apr-2010    Matthew L. Fidler
;;    Last-Updated: Fri Apr 16 10:51:36 2010 (-0500) #32 (Matthew L. Fidler)
;;    Added switch to output to the NONMEM menu and keys.
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 10:52:57 2010 (-0600) #28 (Matthew L. Fidler)
;;    Made Linking to PLT tools msfo files not active when msfo.outputfile is used.
;; 01-Feb-2010    Matthew L. Fidler
;;    Last-Updated: Mon Feb  1 10:45:18 2010 (-0600) #26 (Matthew L. Fidler)
;;    Added PLT tools MSFO=msfo.outputfile option.
;; 28-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jan 28 15:46:20 2010 (-0600) #24 (Matthew L. Fidler)
;;    Added Xmind individual launch.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 16:38:39 2010 (-0600) #17 (Matthew L. Fidler)
;;    Add Refresh of mode when changing supported programs.  Updates tool-bar
;;    and mode-line
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 15:00:25 2010 (-0600) #11 (Matthew L. Fidler)
;;    Added Xmind launcher.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 14:57:57 2010 (-0600) #7 (Matthew L. Fidler)
;;    Made Xmind active only with zip and unzip are in the path.
;; 26-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Tue Jan 26 14:42:12 2010 (-0600) #2 (Matthew L. Fidler)
;;    Added Xmind to menus.
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
(require 'esn-start)
(setq toggle nil)
;; Do version control and admin stuff.  Then menu should be FINISHED...
(require 'esn-options)

(defun esn-menu-def (keymap menu-name menu-lst)
  "* Adds menu to keymap (from menu-list)"
  (easy-menu-define menu-name keymap "EsN menu" menu-lst)
  (easy-menu-add menu-name keymap))

(defvar esn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'esn-renumber-theta)
    (define-key map (kbd "C-c C-n") 'esn-renumber-eta)
    (define-key map (kbd "C-c C-e") 'esn-renumber-eps)
    (define-key map (kbd "C-c C-r") 'esn-renumber)
    (define-key map (kbd "C-c C-x") 'esn-undo-numbering)
    (define-key map (kbd "C-c C-w") 'esn-fix-numbering)
    (define-key map (kbd "C-c C-q") 'esn-nmqual-submit)
    (define-key map (kbd "C-c C-f") 'esn-nm-submit)
    (define-key map (kbd "C-c C-c") 'esn-switch-rpt)
    (define-key map (kbd "<f1>") 'esn-nm-help)
    (define-key map (kbd "\"") 'esn-magic-quote)
    (define-key map (kbd "$") 'esn-magic-$)
    (define-key map (kbd "*") 'esn-magic-asterisk)
    (define-key map (kbd "!") 'esn-magic-!)
    (define-key map (kbd "?") 'esn-magic-?)
    (define-key map (kbd ";") 'esn-magic-semi)
    (define-key map (kbd "(") 'esn-magic-start-paren)
    (define-key map (kbd "<backtab>") 'esn-magic-backtab)
    (define-key map [remap delete-backward-char] 'esn-delete-backward-char)
    (define-key map [remap delete-char-untabify] 'esn-delete-char-untabify)
    (define-key map [remap delete-char] 'esn-delete-char)
    (define-key map [remap backward-kill-word] 'esn-backward-kill-word)
    (mapc (lambda(x)
            (define-key map x 'esn-upcase-char-self-insert))
          (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
    map)
  "Esn mode map")

(defmacro esn-define-keys (map &optional skip-emacsish-keys &rest body)
  (list 'defvar map
        (list 'let (list
                    (list map (list 'make-sparse-keymap)))
              ;;
              ;; Keys
              ;;
              (if skip-emacsish-keys 
                  (list 'progn )
                (list 'progn
                      (list 'define-key map (list 'kbd "C-c C-t") ''esn-renumber-theta)
                      (list 'define-key map (list 'kbd "C-c C-n") ''esn-renumber-eta)
                      (list 'define-key map (list 'kbd "C-c C-e") ''esn-renumber-eps)
                      (list 'define-key map (list 'kbd "C-c C-r") ''esn-renumber)
                      (list 'define-key map (list 'kbd "C-c C-x") ''esn-undo-numbering)
                      (list 'define-key map (list 'kbd "C-c C-w") ''esn-fix-numbering)
                      (list 'define-key map (list 'kbd "C-c C-q") ''esn-nmqual-submit)
                      (list 'define-key map (list 'kbd "C-c C-f") ''esn-nm-submit)
                      (list 'define-key map (list 'kbd "C-c C-c") ''esn-switch-rpt)))
              (list 'define-key map (list 'kbd "<f1>") ''esn-nm-help)
              (list 'define-key map (list 'kbd "\"") ''esn-magic-quote)
              (list 'define-key map (list 'kbd "$") ''esn-magic-$)
              (list 'define-key map (list 'kbd "*") ''esn-magic-asterisk)
              (list 'define-key map (list 'kbd "!") ''esn-magic-!)
              (list 'define-key map (list 'kbd "?") ''esn-magic-?)
              (list 'define-key map (list 'kbd ";") ''esn-magic-semi)
              (list 'define-key map (list 'kbd "(") ''esn-magic-start-paren)
              (list 'define-key map (list 'kbd "<backtab>") ''esn-magic-backtab)
              (append (list 'progn ) (mapcar (lambda(x)
                                               (list 'define-key map (list 'kbd x) ''esn-upcase-char-self-insert))
                                             (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                                                   "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
                      body)
              map)
        "Keymap for EsN major mode"))

;; Define the major emacs key bindings.
(esn-define-keys esn-mode-map)
(setq esn-versions-menu nil
      esn-xpose-menu nil
      esn-tools-menu nil
      esn-run-menu nil
      esn-pdx-menu nil
      esn-plt-menu nil
      esn-extended-menu nil
      esn-display-menu  nil
      esn-automation-menu nil)

(defun esn-mode--keymap ()
  "* Setup the appropriate keymap based on submode."
  (interactive)
  (let ((newmap (make-sparse-keymap)))
    (when (boundp 'viper-mode)
      (viper-modify-major-mode 'esn-mode 'emacs-state esn-mode-map)
      (viper-modify-major-mode 'esn-mode 'insert-state esn-mode-map)
      (viper-change-state-to-vi))
    
    (set-keymap-parent newmap (current-local-map))
    
    (esn-menu-def newmap esn-versions-menu  esn-versions-lst)
    (when (esn-use-xpose-p)
      (esn-menu-def newmap esn-xpose-menu  esn-xpose-lst))
    
    (esn-menu-def newmap esn-tools-menu  esn-tools-lst)
    (esn-menu-def newmap esn-run-menu  esn-run-lst)
    (when (esn-use-pdx-p)
      (esn-menu-def newmap esn-pdx-menu  esn-pdx-lst))
    (when (esn-use-plt-p)
      (esn-menu-def newmap esn-plt-menu  esn-plt-lst))
    (esn-menu-def newmap esn-extended-menu  esn-extended-lst)
    (esn-menu-def newmap esn-display-menu  esn-display-lst)
    (esn-menu-def newmap esn-automation-menu  esn-automation-lst)
    (esn-add-toolbar newmap)
    (use-local-map newmap)
    (setq imenu-generic-expression esn-imenu-generic-expression)
    (imenu-add-to-menubar "->")))

(defun esn-customize-nonmem ()
  "Customize Nonmem"
  (interactive)
  (customize-group "esn-mode"))

(defun esn-mode-toggle-sticky-var (var &rest extra)
  "* Toggle sticky var"
  (interactive)
  (let ((nvar (if (stringp var) var (symbol-name 'var)))
        (small (if esn-header-small "small-" "")))
    (when (string-match "esn-\\(pdx\\|plt\\|pirana\\|census\\|def\\)-\\(small-\\)?" nvar)
      (setq nvar (replace-match "esn-" 't 't nvar)))
    (cond 
     ( (esn-use-pdx-p)
       (when (string-match "esn-" nvar)
         (setq nvar (replace-match (concat "esn-pdx-" small) 't 't nvar))))
     
     ( (esn-use-plt-p)
       (when (string-match "esn-" nvar)
         (setq nvar (replace-match (concat "esn-plt-" small) 't 't nvar))))
     
     ( (esn-use-plt-p)
       (when (string-match "esn-" nvar)
         (setq nvar (replace-match (concat "esn-pirana-" small) 't 't nvar))))
     
     ( (esn-use-census-p)
       (when (string-match "esn-" nvar)
         (setq nvar (replace-match (concat "esn-census-" small) 't 't nvar))))
     ( 't
       (when (string-match "esn-" nvar)
         (setq nvar (replace-match (concat "esn-def-" small) 't 't nvar)))))
    (set (intern nvar) (not (symbol-value (intern nvar))))
    (customize-save-variable (intern nvar) (symbol-value (intern nvar)))
    (customize-save-customized)))


(defmacro esn-mode-toggle-var (var &rest extra)
  "* Macro for toggling variables"
  (list
   (list 'lambda (list )
         (list 'interactive)
         (list 'setq var (list 'not var))
         (list 'customize-save-variable (list 'quote var) var)
         (list 'customize-save-customized)
         (append
          (list 'progn)
          extra))))

(defmacro esn-mode-radio-var (var val &rest extra)
  "* Macro for toggling radio buttons"
  (list
   (list 'lambda (list )
         (list 'interactive)
         (list 'setq var val)
         (list 'customize-save-variable (list 'quote var) var)
         (list 'customize-save-customized)
         (append
          (list 'progn)
          extra))))

(defvar esn-versions-lst
  (append
   '("Use"
     "Assumed NONMEM version")
   (mapcar
    (lambda(x)
      `[,(concat "Assume NONMEM " (cadr x)) (esn-mode-radio-var esn-assumed-version ,(car x))
        :style radio
        :selected (string= ,(car x) esn-assumed-version)])
    esn-mode-nm-versions)
   '(["Customize NONMEM versions" (customize-variable 'esn-mode-nm-versions)]
     "--"
     ["Customize Executable Locations" (lambda() (interactive) (customize-group "esn-exec"))]
     "--"
     "Support")
   (mapcar
    (lambda(var)
      (let ((toggle-var (concat "esn-mode-use-"
                                (downcase (nth 0 (split-string var))))))
        (when (string-match "xpose" toggle-var)
          (setq toggle-var "esn-xpose"))
        (setq toggle-var (intern toggle-var))
        `[,var (esn-mode-toggle-var ,toggle-var (esn-mode))
               :style toggle
               :selected ,toggle-var]))
    '("Census"
      "PDx Pop"
      "Pirana"
      "PLT Tools"
      "Xmind"
      "Xpose"))
   `(
     "--"
     ["Use ? to access EsN's NONMEM help" (esn-toggle-var esn-use-magic-help-key)
      :style toggle
      :selected esn-use-magic-help-key]
     "--"
     ,(format "EsN %s" esn-mode-ver)))
  "Versions menu for EsN."
  )
(defvar esn-xpose-lst
  '("Xpose"
    ["Turn on Xpose Support"
     (esn-mode-toggle-var esn-xpose (esn-mode))
     :style toggle
     :selected esn-xpose
     ]
    "--"
    "Output Table Options"
    ["Generate Xpose tables"
     (esn-mode-toggle-var esn-xpose-generate-tables)
     :active (esn-use-xpose-p)
     :style toggle
     :selected esn-xpose-generate-tables
     ]
    ["Only generate Xpose tables for files named runX.mod"
     (esn-mode-toggle-var esn-xpose-tables-for-non-xpose-ctl-streams)
     :active esn-xpose
     :style toggle
     :selected (not esn-xpose-tables-for-non-xpose-ctl-streams)
     ]
    ["Use smaller output table names (e.g. sd instead of sdtab)"
     (esn-mode-toggle-var  esn-xpose-small-table-file-names)
     :active (esn-use-xpose-p)
     :style toggle
     :selected esn-xpose-small-table-file-names
     ]
    ["Customize Variables assigned to Xpose tables" (customize-group "esn-xpose-tables")
     :active (esn-use-xpose-p)]
    "--"
    "Naming Convention"
    ["Force Xpose's naming convention of run#.mod, no padding with zeros"
     (lambda()
       (interactive)
       (setq esn-xpose-choose-file-name 't)
       (setq esn-xpose-choose-file-name-padding nil)
       (setq esn-xpose-choose-file-name-no-run nil)
       (setq esn-xpose-choose-file-name-no-run-padding nil)
       
       (customize-save-variable 'esn-xpose-choose-file-name esn-xpose-choose-file-name)
       (customize-save-variable 'esn-xpose-choose-file-padding esn-xpose-choose-file-name-padding)
       (customize-save-variable 'esn-xpose-choose-file-name-no-run esn-xpose-choose-file-name-no-run)
       (customize-save-variable 'esn-xpose-choose-file-no-run-padding esn-xpose-choose-file-name-no-run-padding)
       (customize-save-customized))
     :active (and esn-xpose (not (esn-use-plt-p)) (not (esn-use-pdx-p)))
     :style radio
     :selected esn-xpose-choose-file-name
     ]
    ["Force Xpose's naming convention of run###.mod, padding with zeros"
     (lambda()
       (interactive)
       (setq esn-xpose-choose-file-name nil)
       (setq esn-xpose-choose-file-name-padding 't)
       (setq esn-xpose-choose-file-name-no-run nil)
       (setq esn-xpose-choose-file-name-no-run-padding nil)
       
       (customize-save-variable 'esn-xpose-choose-file-name esn-xpose-choose-file-name)
       (customize-save-variable 'esn-xpose-choose-file-padding esn-xpose-choose-file-name-padding)
       (customize-save-variable 'esn-xpose-choose-file-name-no-run esn-xpose-choose-file-name-no-run)
       (customize-save-variable 'esn-xpose-choose-file-no-run-padding esn-xpose-choose-file-name-no-run-padding)
       (customize-save-customized))
     :active (and esn-xpose (not (esn-use-plt-p)) (not (esn-use-pdx-p)))
     :style radio
     :selected esn-xpose-choose-file-name-padding
     ]
    ["Force naming convention of #.mod, no padding with zeros"
     (lambda()
       (interactive)
       (setq esn-xpose-choose-file-name nil)
       (setq esn-xpose-choose-file-name-padding nil)
       (setq esn-xpose-choose-file-name-no-run 't)
       (setq esn-xpose-choose-file-name-no-run-padding nil)
       
       (customize-save-variable 'esn-xpose-choose-file-name esn-xpose-choose-file-name)
       (customize-save-variable 'esn-xpose-choose-file-padding esn-xpose-choose-file-name-padding)
       (customize-save-variable 'esn-xpose-choose-file-name-no-run esn-xpose-choose-file-name-no-run)
       (customize-save-variable 'esn-xpose-choose-file-no-run-padding esn-xpose-choose-file-name-no-run-padding)
       (customize-save-customized))
     :active (and esn-xpose (not (esn-use-plt-p)) (not (esn-use-pdx-p)))
     :style radio
     :selected esn-xpose-choose-file-name-no-run
     ]
    ["Force naming convention of ###.mod, padding with zeros"
     (lambda()
       (interactive)
       (setq esn-xpose-choose-file-name nil)
       (setq esn-xpose-choose-file-name-padding nil)

       (setq esn-xpose-choose-file-name-no-run nil)
       (setq esn-xpose-choose-file-name-no-run-padding 't)

       (customize-save-variable 'esn-xpose-choose-file-name esn-xpose-choose-file-name)
       (customize-save-variable 'esn-xpose-choose-file-padding esn-xpose-choose-file-name-padding)
       (customize-save-variable 'esn-xpose-choose-file-name-no-run esn-xpose-choose-file-name-no-run)
       (customize-save-variable 'esn-xpose-choose-file-no-run-padding esn-xpose-choose-file-name-no-run-padding)
       (customize-save-customized)
       )
     :active (and esn-xpose (not (esn-use-plt-p)) (not (esn-use-pdx-p)))
     :style radio
     :selected esn-xpose-choose-file-name-no-run-padding
     ]
    ["Do not force a naming convention for Xpose"
     (lambda()
       (interactive)
       (setq esn-xpose-choose-file-name nil)
       (setq esn-xpose-choose-file-name-padding nil)
       (setq esn-xpose-choose-file-name-no-run nil)
       (setq esn-xpose-choose-file-name-no-run-padding nil)

       (customize-save-variable 'esn-xpose-choose-file-name esn-xpose-choose-file-name)
       (customize-save-variable 'esn-xpose-choose-file-padding esn-xpose-choose-file-name-padding)
       (customize-save-variable 'esn-xpose-choose-file-name-no-run esn-xpose-choose-file-name-no-run)
       (customize-save-variable 'esn-xpose-choose-file-no-run-padding esn-xpose-choose-file-name-no-run-padding)
       (customize-save-customized)
       )
     :active (and esn-xpose (not (esn-use-plt-p)) (not (esn-use-pdx-p)))
     :style radio
     :selected (not (or esn-xpose-choose-file-name-padding
                        esn-xpose-choose-file-name
                        esn-xpose-choose-file-name-no-run-padding
                        esn-xpose-choose-file-name-no-run
                        ))
     ]
    "--"
    "R script summary"
    ["Automatically generate summary R script for Xpose." (esn-mode-toggle-var esn-mode-create-xpose-summary-script)
     :style toggle
     :selected esn-mode-create-xpose-summary-script
     :active (esn-use-xpose-p)]
    ["Try to run windows macro to convert RTF to password-protected word document"
     (esn-mode-toggle-var esn-mode-use-macro)
     :style toggle
     :selected esn-mode-use-macro
     ]
    ;; ["Customize Summary Document" (customize-variable 'esn-mode-xpose-summary-document)
    ;;  :active (esn-use-xpose-p)]
    )
  "Xpose menu for use in `esn-mode'."
  )

(defvar esn-tools-lst
  '("EsN"
    "Renumbering/Changing Variables"
    ["Change THETA,ETA,EPS to extended variables"
     esn-undo-numbering
     :active (not esn-wfn-extended)
     ]
    ["Change extended variables to THETA,ETA,EPS" 
     esn-fix-numbering
     :active (not esn-wfn-extended)
     ]
    ("Renumber Items"
     ["Renumber THETA(x)"
      esn-renumber-theta
      ]
     ["Renumber ETA(x)"
      esn-renumber-eta
      ]
     ["Renumber EPS(x)"
      esn-renumber-eps
      ]
     ["Renumber ERR(x)"
      esn-renumber-err
      ]
     "--"
     ["Renumber A(x)"
      esn-renumber-a
      ]
     ["Renumber A_0(x)"
      esn-renumber-a_0
      ]
     ["Renumber CDEN_(x)"
      esn-renumber-cden_
      ]
     ["Renumber DADT(x)"
      esn-renumber-dadt
      ]
     ["Renumber MTIME(x)"
      esn-renumber-mtime
      ]
     ["Renumber MNEXT(x)"
      esn-renumber-mnext
      ]
     ["Renumber MPAST(x)"
      esn-renumber-mpast
      ]
     ["Renumber P(x)"
      esn-renumber-p
      ]
     ["Renumber THSIMP(x)"
      esn-renumber-thsimp
      ]
     ["Renumber OMSIMP(x)"
      esn-renumber-omsimp
      ]
     "--"
     ["Renumber Arbitrary Variable"
      esn-renumber
      ]
     )
    "--"
    "Version Control"
    ["Differences from last version" (call-interactively 'esn-vc-diff) ]
    ["Revision History" vc-print-log ]
    "--"
    "Misc"
    ["Fill in Estimate values from last run" esn-update-est-based-on-output
     :active (esn-get-nonmem-output)]
    ["Switch to NONMEM output" esn-switch-rpt
     ]
    ["Generate Between Subject Variability Exploration Control Streams" esn-mode-gen-bsv-files]
    ["Generate Random Variability Exploration Control Streams" esn-rv-form]
    "--"
    ["Update Xmind project files based on what is in the current directory"
     esn-xmind-update-all-files-in-current-directory]
    )
  "Tools menu for use in `esn-mode'."
  )

(defvar esn-run-lst
  (append 
   '("Run")
   (when (esn-use-psn-p)
     '(
       "Perl Speaks NONMEM"
       ["Bootstrap (via PsN)" esn-psn-bs
        :active esn-mode-psn-install-val]
       ["Case Deletion Diagnostics (via PsN)" esn-psn-cdd
        :active esn-mode-psn-install-val]
       ["Execute NONMEM run (via PsN)" esn-psn-execute
        :active esn-mode-psn-install-val]
       ["Log Liklihood Profiling (via PsN)" esn-psn-llp
        :active esn-mode-psn-install-val]
       ["Summarize NONMEM output (via PsN)" esn-psn-sumo
        :active esn-mode-psn-install-val]
       ["Visual Predictive Check (via PsN)" esn-psn-vpc
        :active esn-mode-psn-install-val]
       ["Numerical Predictive Check (via PsN)" esn-psn-npc
        :active esn-mode-psn-install-val]
       ("PsN Options"
        "Bootstrapping"
        ["Use BCa method" (esn-mode-toggle-var esn-mode-psn-bs-bca)
         
         :style toggle

         :selected esn-mode-psn-bs-bca
         :active esn-mode-psn-install-val]
        ["Generate Matlab Scripts for Plots"
         (esn-mode-toggle-var esn-bs-mplots)
         
         :style toggle
         
         :selected esn-bs-mplots
         :active esn-mode-psn-install-val]
        ["Generate R Scripts for Plots"
         (esn-mode-toggle-var esn-bs-rplots)
         
         :style toggle
         
         :selected esn-bs-rplots
         :active esn-mode-psn-install-val]
        ["Skip NONMEM runs where covariance step terminated"
         (esn-mode-toggle-var esn-skip-covariance-step-terminated)
         
         :style toggle
         
         :selected esn-skip-covariance-step-terminated
         :active esn-mode-psn-install-val]
        ["Skip NONMEM runs where estimate is near boundary"
         (esn-mode-toggle-var esn-skip-estimate-near-boundary)
         
         :style toggle
         
         :selected esn-skip-estimate-near-boundary
         :active esn-mode-psn-install-val]
        ["Skip NONMEM runs with Covariance Warnings"
         (esn-mode-toggle-var esn-skip-with-covstep-warnings)
         
         :style toggle
         
         :selected esn-skip-with-covstep-warnings
         :active esn-mode-psn-install-val]
        ["Customize All Bootstrapping options"
         (customize-group "esn-mode-psn-bootstrap")
         :active esn-mode-psn-install-val]
        "--"
        "Case Deletion Diagnostics"
        ["Generate R Scripts for Plots"
         (esn-mode-toggle-var esn-mode-psn-cdd-rplots)
         
         :style toggle
         
         :selected esn-mode-psn-cdd-rplots
         :active esn-mode-psn-install-val]
        ["Factors selected for exclusion are drawn randomly instead of consecutively"
         (esn-mode-toggle-var esn-mode-psn-cdd-use-random-instead-of-consecutive)
         
         :style toggle
         
         :selected esn-mode-psn-cdd-use-random-instead-of-consecutive
         :active esn-mode-psn-install-val]
        ["Do Crossvalidation"
         (esn-mode-toggle-var esn-mode-psn-cdd-xv)
         
         :style toggle
         
         :selected esn-mode-psn-cdd-xv
         :active esn-mode-psn-install-val]
        ["Customize All Case Deletion Diagnostics options"
         (customize-group "esn-mode-psn-cdd")
         :active esn-mode-psn-install-val]
        "--"
        "Executing NONMEM"
        ["Rerun with tweaked inits if run fails"
         (esn-mode-toggle-var esn-mode-psn-tweak-inits)
         
         :style toggle
         
         :selected esn-mode-psn-tweak-inits
         :active esn-mode-psn-install-val]
        ["Widen criteria for \"failed run\""
         (esn-mode-toggle-var esn-mode-psn-picky)
         
         :style toggle
         
         :selected esn-mode-psn-picky
         :active esn-mode-psn-install-val]
        ["Summarize Output"
         (esn-mode-toggle-var esn-mode-psn-ex-summarize)
         
         :style toggle
         
         :selected esn-mode-psn-ex-summarize
         :active esn-mode-psn-install-val]
        ["Quick Summary of Output"
         (esn-mode-toggle-var esn-mode-psn-quick-summary)
         
         :style toggle
         
         :selected esn-mode-psn-quick-summary
         :active esn-mode-psn-install-val]
        ["Calculate Shrinkage"
         (esn-mode-toggle-var esn-mode-psn-calculate-shrinkage)
         
         :style toggle
         
         :selected esn-mode-psn-calculate-shrinkage
         :active esn-mode-psn-install-val]
        ["Calcuate CWRES"
         (esn-mode-toggle-var esn-mode-psn-calculate-cwres)
         
         :style toggle
         
         :selected esn-mode-psn-calculate-cwres
         :active esn-mode-psn-install-val]
        ["Wrap Data"
         (esn-mode-toggle-var esn-mode-psn-wrap-data)
         
         :style toggle
         
         :selected esn-mode-psn-wrap-data
         :active esn-mode-psn-install-val]
        ["Stochastic Differential Equation Model"
         (esn-mode-toggle-var esn-mode-psn-sde)
         
         :style toggle
         
         :selected esn-mode-psn-sde
         :active esn-mode-psn-install-val]
        ["Save space and memory by reducing PsN's internal dataset"
         (esn-mode-toggle-var esn-mode-psn-drop-dropped)
         
         :style toggle
         
         :selected esn-mode-psn-drop-dropped
         :active esn-mode-psn-install-val]
        ["Customize All NONMEM execution options"
         (customize-group "esn-mode-psn-execute")
         :active esn-mode-psn-install-val]
        "--"
        "Log Liklihood Profiling"
        ["Generate Matlab scripts"
         (esn-mode-toggle-var esn-mode-psn-llp-mplots)
         
         :style toggle
         
         :selected esn-mode-psn-llp-mplots
         :active esn-mode-psn-install-val]
        ["Generate R scripts"
         (esn-mode-toggle-var esn-mode-psn-llp-rplots)
         
         :style toggle
         
         :selected esn-mode-psn-llp-rplots
         :active esn-mode-psn-install-val
         ]
        ["Customize All Log Liklihood Profiling options"
         (customize-group "esn-mode-psn-llp")
         :active esn-mode-psn-install-val
         ]
        )
       "--"
       )
     )
   '(
     "PLT Tools"
     ["Run using PLT Tools" 'esn-plt-submit
      :active (esn-use-plt-p)
      ]
     ["Change execution Type" (customize-variable 'esn-plt-exec-type)
      :active (esn-use-plt-p)
      ]
     "--"
     )
   (when (esn-use-wfn-p)
     '(
       "Wings for NONMEM"
       ["Submit run to NONMEM (via Wings)" (and esn-wfn-submit (not (string= "" esn-wfn-submit)))
        :active (esn-use-wfn-p)]
       ["Bootstrap NONMEM model (via Wings)" (and esn-wfn-bs (not (string= "" esn-wfn-bs)))
        :active (esn-use-wfn-p)]
       ["Randomization Test for NONMEM model (via Wings)" (and esn-wfn-rt (not (string= "" esn-wfn-rt)))
        :active (esn-use-wfn-p)]
       ["Change/Find Wings For NONMEM executable" (customize-variable 'esn-mode-wfn-bat) ]
       "--"
       )
     )
   '(
     "Submit Run"
     ["Run using NMQual" esn-nmqual-submit
      :active (esn-use-nmqual-p)
      ]
     ["Run using NONMEM nmfe script" esn-nm-submit
      :active (and esn-exec-nm (not (string= "" esn-exec-nm)) (file-exists-p esn-exec-nm))
      ]
     "--"
     "Launch Run Managers"
     ["Launch Census"
      esn-census-start
      :active (and esn-census-exe (not (string= "" esn-census-exe)) (file-exists-p esn-census-exe))
      ]
     ["Launch Pirana"
      esn-pirana-start
      :active (and esn-pirana-exe (not (string= "" esn-pirana-exe)) (file-exists-p esn-pirana-exe))
      ]
     ["Launch PDx-Pop"
      esn-PDxPop-start
      :active (and esn-PDxPop-exe (not (string= "" esn-PDxPop-exe)) (file-exists-p esn-PDxPop-exe))
      ]
     "--"
     "Mind Map Tools"
     ["Launch Xmind's Project File"
      esn-xmind-project
      :active (esn-use-xmind-p)
      ]
     ["Launch Xmind's Individual File"
      esn-xmind-ctl
      :active (esn-use-xmind-p)
      ]
     )
   )
  "Run menu for use in `esn-mode'."
  )

(defvar esn-pdx-lst
  '("PDx"
    ["Use PDx-pop"
     (esn-mode-toggle-var esn-mode-use-pdx (esn-mode))
     :style toggle
     :selected esn-mode-use-pdx
     ]
    "--"
    "Variable Labels"
    ["Generate and Update [P], [A], and [F] labels in $OMEGA and $SIGMA blocks"
     (esn-mode-toggle-var esn-mode-use-omega-sigma-var-type-labels)
     :style toggle
     :selected esn-mode-use-omega-sigma-var-type-labels
     :active (esn-use-pdx-p)
     ]
    ["Automatically Generate brackets for THETA labels"
     (esn-mode-toggle-var esn-mode-auto-generate-brackets-for-theta)
     :style toggle
     :selected esn-mode-auto-generate-brackets-for-theta
     :active (esn-use-pdx-p)
     ]
    "--"
    "Naming Convention"
    ["Use naming convention of #.ctl, no padding with zeros"
     (lambda ()
       (interactive)
       (setq esn-pdx-choose-file-name 't)
       (setq esn-pdx-choose-file-name-padding nil)

       (customize-save-variable 'esn-pdx-choose-file-name esn-pdx-choose-file-name)
       (customize-save-variable 'esn-pdx-choose-file-name-padding esn-pdx-choose-file-name-padding)
       (customize-save-customized)
       )
     :active (esn-use-plt-p)
     :style radio
     :selected esn-pdx-choose-file-name
     ]
    ["Use naming convention of ###.ctl, padding with zeros"
     (lambda()
       (interactive)
       (setq esn-pdx-choose-file-name nil)
       (setq esn-pdx-choose-file-name-padding 't)

       (customize-save-variable 'esn-pdx-choose-file-name esn-pdx-choose-file-name)
       (customize-save-variable 'esn-pdx-choose-file-name-padding esn-pdx-choose-file-name-padding)
       (customize-save-customized)
       )
     :active (esn-use-plt-p)
     :style radio
     :selected esn-pdx-choose-file-name-padding
     ]
    ["Do not use a naming convention"
     (lambda()
       (interactive)
       (setq esn-pdx-choose-file-name nil)
       (setq esn-pdx-choose-file-name-padding nil)

       (customize-save-variable 'esn-pdx-choose-file-name esn-pdx-choose-file-name)
       (customize-save-variable 'esn-pdx-choose-file-name-padding esn-pdx-choose-file-name-padding)
       (customize-save-customized)
       )
     :active (esn-use-plt-p)
     :style radio
     :selected (not (or esn-pdx-choose-file-name-padding esn-pdx-choose-file-name))
     ]
    "--"
    "Tables"
    ["Generate PDx-Pop's tables"
     (esn-mode-toggle-var esn-pdx-generate-tables)
     :style toggle
     :selected esn-pdx-generate-tables
     ]
    ["Generate Xpose tables in PDx-Pop control streams"
     (esn-mode-toggle-var esn-pdx-gen-xpose)
     :selected esn-pdx-gen-xpose
     :style toggle
     :active (and esn-pdx-generate-tables (esn-use-pdx-p))
     ]
    ["Generate single table (.tab) tables in PDx-Pop control streams"
     (esn-mode-toggle-var esn-pdx-gen-one-table)
     :selected esn-pdx-gen-one-table
     :style toggle
     :active (and esn-pdx-generate-tables (esn-use-pdx-p))
     ]
    ["Generate eta parameter tables (.eta) in PDx-Pop control streams"
     (esn-mode-toggle-var esn-pdx-gen-eta-table)
     :selected esn-pdx-gen-eta-table
     :style toggle
     :active (and esn-pdx-generate-tables (esn-use-pdx-p))
     ]
    ["Generate parameter tables (.par) in PDx-Pop control streams"
     (esn-mode-toggle-var esn-pdx-gen-par-table)
     :selected esn-pdx-gen-par-table
     :style toggle
     :active (and esn-pdx-generate-tables (esn-use-pdx-p))
     ]
    "--"
    "Run"
    ["Launch PDx-Pop"
     esn-PDxPop-start
     :active (and esn-PDxPop-exe (file-exists-p esn-PDxPop-exe))
     ]
    )
  "PDx-POP menu for use in `esn-mode'."
  )

(defvar esn-plt-lst
  '("PLT"
    ["Use PLT tools"
     (esn-mode-toggle-var esn-mode-use-plt (esn-mode))
     :style toggle
     :selected esn-mode-use-plt
     ]
    "--"
    "Graphics Script"
    ["Generate Graphics Script from Control Stream"
     (esn-mode-toggle-var esn-plt-gen-graphics)
     :style toggle
     :selected esn-plt-gen-graphics
     ]
    ("Customize Covariates"
     ["Change Assumed Age Covariates"
      (customize-variable 'esn-plt-age-covs)
      ]
     ["Change Assumed Race Covariates"
      (customize-variable 'esn-plt-race-covs)
      ]
     ["Change Assumed Gender Covariates"
      (customize-variable 'esn-plt-gender-covs)
      ]
     ["Change Assumed Height Covariates"
      (customize-variable 'esn-plt-height-covs)
      ]
     ["Change Assumed Dose Covariates"
      (customize-variable 'esn-plt-dose-covs)
      ]
     ["Change Assumed Fed Covariates"
      (customize-variable 'esn-plt-fed-covs)
      ]
     ["Change Assumed Formulation Covariates"
      (customize-variable 'esn-pt-formulation-covs)
      ]
     )
    "--"
    "UserDefinedScript.R"
    ["Use EsN's UserDefinedScript.R"
     (esn-mode-toggle-var esn-plt-use-userscript)
     :style toggle
     :selected esn-plt-use-userscript
     ]
    ["Try to run windows macro to convert RTF to password-protected word document"
     (esn-mode-toggle-var esn-mode-use-macro)
     :style toggle
     :selected esn-mode-use-macro
     ]
    "--"
    "Tables"
    ["Generate PLT tools' tables"
     (esn-mode-toggle-var esn-plt-generate-tables)
     :style toggle
     :selected esn-plt-generate-tables 
     ]
    ["Generate Xpose tables in addition to PLT tools' tables"
     (esn-mode-toggle-var esn-plt-gen-xpose)
     :style toggle
     :active esn-plt-generate-tables
     :selected esn-plt-gen-xpose
     ]
    ["Generate One conglomerate table in addition to PLT tools' tables"
     (esn-mode-toggle-var esn-plt-gen-one-table)
     :style toggle
     :active esn-plt-generate-tables
     :selected esn-plt-gen-one-table
     ]
    ("Table Options"
     "AllRecords.txt"
     ["Change variables automatically added to AllRecords.txt"
      (customize-variable 'esn-plt-allrecords)
      :active (and esn-plt-generate-tables esn-mode-use-plt)
      ]
     ["Change variables that can be added to AllRecords.txt, but not done by default"
      (customize-variable 'esn-plt-no-allrecords)
      :active (and esn-plt-generate-tables esn-mode-use-plt)
      ]
     "FirstRecords.txt"
     ["Change parameter variables added to FirstRecords.txt by default (other than parameter values)"
      (customize-variable 'esn-plt-firstrecords)
      :active (and esn-plt-generate-tables esn-mode-use-plt)
      ]
     ["Change parameter variables that can be  added to FirstRecords.txt, not done by default"
      (customize-variable 'esn-plt-no-firstrecords)
      :active (and esn-plt-generate-tables esn-mode-use-plt)
      ]
     )
    "--"
    "Xpose Integration"
    ["Generate Xpose Summary R script as well as UserDefinedScript.R"
     (esn-mode-toggle-var esn-plt-gen-xpose-r-script)
     :style toggle
     :selected esn-plt-gen-xpose-r-script
     :active esn-plt-gen-xpose
     ]
    ["Generate Xpose graphics/objects from PLT outputted tables (Requires Graphics Script)"
     (esn-mode-toggle-var esn-plt-gen-xpose-plt)
     :style toggle
     :selected esn-plt-gen-xpose-plt
     ]
    ("Xpose-PLT Options"
     ;; ["Customize graphics exported to Xpose"
     ;;  (customize-variable 'esn-mode-xpose-summary-document)
     ;;  ]
     ["Customize comparison graphics exported to Xpose"
      (customize-variable 'esn-mode-xpose-summary-document-comparisons)
      ]
     )
    "--"
    "Misc"
    ["Use MSFO=msfo.outputfile, as requested by PLT tools"
     (esn-mode-toggle-var esn-plt-use-msfo-outputfile)
     :style toggle
     :selected esn-plt-use-msfo-outputfile
     ]
    ["Put WIDE in $DATA record, as requested by PLT tools with large datasets."
     (esn-mode-toggle-var esn-plt-add-wide-to-data)
     :style toggle
     :selected esn-plt-add-wide-to-data
     ]
    "--"
    "Launching Options"
    ["Under windows, open the plta file instead of launching PLT tools directly"
     (esn-mode-toggle-var esn-plt-win-open)
     :style toggle
     :selected esn-plt-win-open
     :active (and esn-w32 (fboundp 'w32-browser))
     ]
    ["Quit PLT tools after the automation script has been run"
     (esn-mode-toggle-var esn-plt-plta-quit)
     :style toggle
     :selected esn-plt-plta-quit
     ]
    "--"
    ("Opening Files (Requires Windows & w32-browser)"
     "Standard"
     ["Graphics" esn-plt-graphics-pdf :active (and esn-w32 (fboundp 'w32-browser))]
     ["Brief Summary" esn-plt-brief-summary-pdf :active (and esn-w32 (fboundp 'w32-browser))]
     ["Summary" esn-plt-summary-pdf :active (and esn-w32 (fboundp 'w32-browser))]
     ["Results" esn-plt-results-pdf :active (and esn-w32 (fboundp 'w32-browser))]
     ["Statistics" esn-plt-statistics :active (and esn-w32 (fboundp 'w32-browser))]
     ["All" esn-plt-all :active (and esn-w32 (fboundp 'w32-browser))]
     "--"
     "Bootstrap"
     ["Bootstrap Parameter Values (CSV)" esn-plt-bootstrap-csv :active (and esn-w32 (fboundp 'w32-browser))]
     ["Bootstrap Summary (CSV)" esn-plt-bootstrap-summary-csv :active (and esn-w32 (fboundp 'w32-browser))]
     ["Bootstrap Graphics (PDF)" esn-plt-graphics-bootstrap-pdf :active (and esn-w32 (fboundp 'w32-browser))]
     ["All Bootstrap Files" esn-plt-bootstrap :active (and esn-w32 (fboundp 'w32-browser))]
     )
    )
  "PLT menu for use in `esn-mode'."
  )

(defvar esn-extended-lst
  '("Extended"
    "Display Options"
    ["Highlight extended control stream variables" (esn-mode-toggle-var esn-wfn-color-extended)
     :style toggle
     :selected esn-wfn-color-extended]
    "--"
    "Editing"
    ["Update variable names in model blocks when updating labels in estimates"
     (esn-mode-toggle-var esn-update-wfn-labels-when-editing)
     :style toggle
     :selected esn-update-wfn-labels-when-editing]
    "--"
    "Getting Variable Names"
    ["Ignore initial C in ;C and initial THETA/OMEGA/SIGMA/ETA/EPS/ERR for variable labels"
     (esn-mode-toggle-var esn-wfn-var-label-skip-tos)
     :style toggle
     :selected esn-wfn-var-label-skip-tos
     ]
    ["Ignore initial [P] [F] or [A] for variable labels"
     (esn-mode-toggle-var esn-wfn-var-label-skip-afp)
     :style toggle
     :selected esn-wfn-var-label-skip-afp
     ]
    "--"
    "Translation"
    ["Translate Extended Control Streams to Normal Control Streams; Open Extended" (esn-mode-toggle-var esn-wfn-extended)
     :style toggle
     :selected esn-wfn-extended]
    ["Translate extended variable names in ZERO=(x)"
     (esn-mode-toggle-var esn-wfn-extended-zero)
     :style toggle
     :selected esn-wfn-extended-zero]
    ["When opening extended, prefer records in lower case" (esn-mode-toggle-var esn-wfn-prefer-lc)
     :style toggle
     :selected esn-wfn-prefer-lc]
    ["Use mu-variable translation"
     :style toggle
     :selected esn-wfn-mu-var
     :active (lambda()
               (let ((nm-ver (esn-update-get-version)))
                 (when (string= "-1" nm-ver)
                   (setq nm-ver esn-assumed-version))
                 (>= (string-to-number nm-ver) 7)
                 )
               )
     ]
    "--"
    "Tools"
    ["Change THETA,ETA,EPS to extended variables" esn-undo-numbering
     :active (not esn-wfn-extended)]
    ["Change extended variables to THETA,ETA,EPS" esn-fix-numbering
     :active (not esn-wfn-extended)])
  "Extended CTLs for `esn-mode'.")

(defvar esn-display-lst
  '("Display"
    "Character Limit"
    ["Show lines over 80 characters"
     (esn-mode-toggle-var esn-highlight-over)
     
     :style toggle

     :selected esn-highlight-over
     ]
    ["Show 80 character limit"
     (lambda() (interactive)
       (when esn-highlight-80-col-even-if-not-over
         (esn-80-clear)
         )
       (esn-mode-toggle-var esn-highlight-80-col-even-if-not-over)
       )
     :style toggle
     :selected esn-highlight-80-col-even-if-not-over
     ]
    "--"
    "Hidden/Read Only Sections"
    ["Hide CWRES generated code"
     (lambda ()
       (interactive)
       (esn-mode-toggle-var esn-hide-cwres-generated-code)
       (esn-hide-header)
       )
     :style toggle
     :selected esn-hide-cwres-generated-code
     :active (or esn-set-read-only esn-hide-uneditable-header-components)
     ]
    ["Portions of header read-only" (lambda ()
                                      (interactive)
                                      (setq esn-set-read-only (not esn-set-read-only))
                                      (customize-save-variable 'esn-set-read-only esn-set-read-only)
                                      (customize-save-customized)
                                      (esn-hide-header)
                                      )
     :style toggle
     :selected esn-set-read-only
     ]
    ["Hide portions of header (requires them to be read-only)"
     (lambda ()
       (interactive)
       (setq esn-hide-uneditable-header-components (not esn-hide-uneditable-header-components))
       (customize-save-variable 'esn-hide-uneditable-header-components esn-hide-uneditable-header-components)
       (esn-hide-header)
       (customize-save-variable 'esn-hide-uneditable-header-components esn-hide-uneditable-header-components)
       (customize-save-customized)
       )
     :style toggle
     :active esn-set-read-only
     :selected esn-hide-uneditable-header-components]
    "--"
    "Symbols"
    ["Display symbols instead of text." 
     (esn-mode-toggle-var esn-use-symbols 
                          (decompose-region (point-min) (point-max))
                          (save-excursion
                            (esn-compose-region (window-start) 
                                                (progn (goto-char (window-start))
                                                       (forward-line (window-height))
                                                       (end-of-line) 
                                                       (point)))))

     :style toggle
     :selected esn-use-symbols
     ]
    ("Options"
     ["Show DADT(#) as differential"
      (esn-mode-toggle-var esn-symbols-dadt 
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-symbols-dadt
      ]
     ["Display superscripts"
      (esn-mode-toggle-var esn-superscripts
                           (decompose-region (point-min) (point-max)) 
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-superscripts
      ]
     ["Display subscripts"
      (esn-mode-toggle-var esn-subscripts
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-subscripts
      ]
     ["Display symbols for operators"
      (esn-mode-toggle-var esn-operators
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point))))
                           )
      :style toggle
      :selected esn-operators
      ]
     ["Display symbols for THETA(#), LAMBDA, etc."
      (esn-mode-toggle-var esn-symbol-words
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-symbol-words
      ]
     ["Display Subscripts for A(#), A_0(#), etc."
      (esn-mode-toggle-var esn-symbols-a_0
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-symbols-a_0
      ]
     ["Display the characters \"()_\" in symbols"
      (esn-mode-toggle-var esn-symbols-expanded
                           (decompose-region (point-min) (point-max))
                           (save-excursion
                             (esn-compose-region (window-start) 
                                                 (progn (goto-char (window-start))
                                                        (forward-line (window-height))
                                                        (end-of-line) 
                                                        (point)))))
      :style toggle
      :selected esn-symbols-expanded
      ]
     )
    "--"
    "Highlighting & Hyperlinking"
    ["Customize file color coding/highlighting" (lambda() (interactive) (customize-group "esn-font-lock"))]
    ["Create Hyperlinks" (esn-mode-toggle-var esn-use-hyperlinks)
     :style toggle
     :selected esn-use-hyperlinks
     ]
    ("Options"
     ["Generate Hyperlink to use MSFO as MSFI in new control stream"
      (esn-mode-toggle-var esn-link-msfo)
      :style toggle
      :selected esn-link-msfo
      :active (not (and esn-plt-use-msfo-outputfile (esn-use-plt-p)))
      ]
     ["Link Output Files"
      (esn-mode-toggle-var esn-link-outfiles)
      :style toggle
      :selected esn-link-outfiles
      ]
     ["Link Reference Models"
      (esn-mode-toggle-var esn-link-ref-models)
      :style toggle
      :selected esn-link-ref-models
      ]
     ["Link ;INCLUDE files"
      (esn-mode-toggle-var esn-link-include-files)
      :type toggle
      :group esn-link-include-files
      ]
     )
    )
  "Display menu for use in `esn-mode'."
  )

(defvar esn-automation-lst
  '("Automation"
    ("Changes On Save"
     "Tables and Outputs"
     ["Update Table Names" (esn-mode-toggle-var esn-update-table)
      :style toggle
      :selected esn-update-table]
     ["Update MSFO Output Names" (esn-mode-toggle-var esn-update-msfo)
      :style toggle
      :selected esn-update-msfo]
     ["Update FORTRAN output through OPEN statments" (esn-mode-toggle-var esn-update-fortran)
      :style toggle
      :selected esn-update-fortran]
     ["Generate Single Table"
      (esn-mode-toggle-var esn-plt-gen-one-table)
      :selected esn-generate-one-table
      :style toggle
      ]
     ["Generate Xpose tables"
      (esn-mode-toggle-var esn-xpose-generate-tables)
      :active (esn-use-xpose-p)
      :style toggle
      :selected esn-xpose-generate-tables
      ]
     ("Update Single table output (if not using Xpose)"
      ["Add continuous covariates to output (Variables defined in Xposes' cotab)"
       (esn-mode-toggle-var esn-update-table-continuous-covariates)
       :style toggle
       :selected esn-update-table-continuous-covariates
       ]
      ["Add categorical covariates to output (Variables defined in Xposes' cotab)"
       (esn-mode-toggle-var esn-update-table-categorical-covariates)
       :style toggle
       :selected esn-update-table-categorical-covariates
       ]
      ["Add Standard variables to table (Variables defined in sdtab)"
       (esn-mode-toggle-var esn-update-table-sdtab)
       :style toggle
       :selected esn-update-table-sdtab
       ]
      ["Add parameters to the table"
       (esn-mode-toggle-var esn-update-table-patab)
       :style toggle
       :selected esn-update-table-patab
       ]
      ["Add CWRES parameters to the table"
       (esn-mode-toggle-var esn-update-table-cwtab)
       :style toggle
       :selected esn-update-table-cwtab
       ]
      ["Add Multiple response variables to output (Variables defined in Xposes mutab)"
       (esn-mode-toggle-var esn-update-table-mutab)
       :style toggle
       :selected esn-update-table-mutab
       ]
      ["Add Extra Variables (Varibles defined in Xposes extra)"
       (esn-mode-toggle-var esn-update-table-extra)
       :style toggle
       :selected esn-update-table-extra
       ]
      ["Add miscellaneous variables defined in Xposes' xptab"
       (esn-mode-toggle-var esn-update-xptab)
       :style toggle
       :selected esn-update-table-xptab]
      ["Add miscellaneous variables defined in Xposes' mytab"
       (esn-mode-toggle-var esn-update-mytab)
       :style toggle
       :selected esn-update-table-mytab
       ]
      ["Customize Variables assigned to Xpose tables" (lambda ()
                                                        (interactive)
                                                        (customize-group "esn-xpose-tables")
                                                        )]
      )
     "--"
     "Hidden changes for NONMEM"
     ("Change File Coding System"
      ["Don't change the file coding system on save."
       (lambda()
         (interactive)
         (setq esn-save-coding-system 1)
         (customize-save-variable 'esn-save-coding-system esn-save-coding-system)
         (customize-save-customized)
         )
       :style toggle
       :selected (= 1 esn-save-coding-system)
       ]
      ["Change file-coding system to Windows if edited there, otherwise use Unix"
       (lambda()
         (interactive)
         (setq esn-save-coding-system 2)
         (customize-save-variable 'esn-save-coding-system esn-save-coding-system)
         (customize-save-customized)
         )
       :style toggle
       :selected (= 2 esn-save-coding-system)
       ]
      ["Use Windows/DOS encoding"
       (lambda()
         (interactive)
         (setq esn-save-coding-system 3)
         (customize-save-variable 'esn-save-coding-system esn-save-coding-system)
         (customize-save-customized)
         )
       :style toggle
       :selected (= 3 esn-save-coding-system)
       ]
      ["Use Linux/Unix encoding"
       (lambda()
         (interactive)
         (setq esn-save-coding-system 4)
         (customize-save-variable 'esn-save-coding-system esn-save-coding-system)
         (customize-save-customized)
         )
       :style toggle
       :selected (= 4 esn-save-coding-system)
       ]
      "--"
      ["Use \\ instead of / for Windows/DOS files"
       (esn-mode-toggle-var esn-fix-windows-slashes)
       :style toggle
       :selected esn-fix-windows-slashes]
      )
     ["Capitalize buffer on save (currently required for NONMEM)"
      (esn-mode-toggle-var esn-wfn-caps)
      :style toggle
      :selected esn-wfn-caps
      ]
     "--"
     "Misc."
     ["Compress path lengths"
      (esn-mode-toggle-var esn-compress-paths)
      :style toggle
      :selected esn-compress-paths
      ]
     ["Drop $INPUT aliases that interfere with table header output."
      (esn-mode-toggle-var esn-drop-aliases)
      :style toggle
      :selected esn-drop-aliases
      ]
     ["Update Xmind map upon save."
      (esn-mode-toggle-var esn-mode-use-xmind)
      :style toggle
      :selected esn-mode-use-xmind
      :active (esn-xmind-components-present)
      ]
     )
    ("Changes While Editing"
     "Alignment"
     ["Align Code at = and comments"
      (esn-mode-toggle-var esn-align-at-equals)
      :style toggle
      :selected esn-align-at-equals]
     ["Align THETA, OMEGA & SIGMA" (esn-mode-toggle-var esn-align-matrices)
      :style toggle
      :selected esn-align-matrices]
     ["Automatically Indent" (esn-mode-toggle-var esn-mode-auto-indent)
      :style toggle
      :selected esn-mode-auto-indent
      ]
     ("Alignment Options"
      ["Add comma between THETA estimates (requires THETA alignment)"
       (esn-mode-toggle-var esn-align-add-comma)
       :style toggle
       :selected esn-align-add-comma
       :active esn-align-matrices
       ]
      "--"
                                        ;       ["Customize records not indented to record-length" (customize-variable 'esn-mode-recs-not-indented-to-record-length)]
      ["Customize records with no indentation" (customize-variable 'esn-mode-auto-indent-force-zero-indent)]
      )
     "---"
     "Key Behaviors"
     ["Smart capitalization"
      (esn-mode-toggle-var esn-caps-tool)
      :style toggle
      :selected esn-caps-tool]
     ["Generate Seed with \"(\" in $SIM record, and TAB in SEED= options"
      (esn-mode-toggle-var esn-sim-add-seed)
      :style toggle
      :selected esn-sim-add-seed
      ]
     "--"
     "Completion"
     ["No completion" (esn-mode-radio-var esn-completion-type 0
                                          (esn-completion-off))
      :style radio
      :selected (= esn-completion-type 0)
      ]
     ["Use Auto-Complete Mode (if available)" (esn-mode-radio-var esn-completion-type 2
                                                                  (esn-ac-start))
      :style radio
      :selected (= esn-completion-type 2)
      ]
     ["Use Company-Mode (if available)" (esn-mode-radio-var esn-completion-type 1
                                                            (esn-company-start))
      :style radio
      :selected (= esn-completion-type 1)
      ]

     ["Use Completion UI (if available)" (esn-mode-radio-var esn-completion-type 3
                                                             (esn-cui-start))
      :style radio
      :selected (= esn-completion-type 3)
      ]

     ["Use Emacs Built-in completion" (esn-mode-radio-var esn-completion-type 999)
      :style radio
      :selected (= esn-completion-type 999)
      ]
                                        ;    ["Automatic completion" (esn-mode-toggle-var esn-mode-tab-completion)
                                        ;     :style toggle
                                        ;     :selected esn-mode-tab-completion]
     ("Options"
      ["Use ESN's tab completion to fill in INPUT with CSV files" (esn-mode-toggle-var esn-mode-tab-complete-input)
       
       :active (not (= 0 esn-completion-type))
       
       :style toggle
       :selected esn-mode-tab-complete-input]
      ["After INPUT is tab completed, move $DATA before $INPUT and go to end of $DATA" (esn-mode-toggle-var esn-mode-tab-complete-input-after-data)

       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)

       :style toggle
       :selected esn-mode-tab-complete-input-after-data]
      ["When completing $INPUT, comment about variable translation" (esn-mode-toggle-var esn-update-input-add-comment)
       :style toggle
       :selected esn-update-input-add-comment
       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)]
      "---"
      ["Open dialog box under windows for blank files" (esn-mode-toggle-var esn-mode-tab-completion-browse-files-w32)
       :style toggle
       :selectd esn-mode-tab-completion-browse-files-w32
       :active esn-w32
       ]
      ["Complete GRD with DDDDS when sigma-type THETAS are in ERROR block" (esn-mode-toggle-var esn-grd-complete)
       :style toggle
       :selected esn-grd-complete
       ]
      ["Complete scale factor and calculate units." (esn-mode-toggle-var esn-scale-complete)
       :style toggle
       :selected esn-scale-complete
       ]
      "--"
      ["Customize the comments for of the input variable translation" (customize-variable 'esn-variable-labels )
       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)]
      ["Customize assumed DV variables" (customize-variable 'esn-mode-dv-vars )
       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)]
      ["Customize assumed Time variables" (customize-variable 'esn-mode-time-vars )
       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)]
      ["Customize assumed variables that are kept by default" (customize-variable 'esn-mode-reserved-vars )
       :active (and (not (= 0 esn-completion-type)) esn-mode-tab-complete-input)]
      )
     "-"
     "Wrapping/Indentation"
     ["Automatic Wrapping of Records"  (esn-mode-toggle-var esn-wrapping-of-records)
      :style toggle
      :selected esn-wrapping-of-records]
     ["Use ESN's indentation rules"  (esn-mode-toggle-var esn-mode-auto-indent)
      :style toggle
      :selected esn-mode-auto-indent]
     ["Customize records not wrapped" (customize-variable 'esn-records-not-wrapped)]
     "--"
     "While Typing"
     ["Number Estimates*"
      (esn-mode-toggle-sticky-var "esn-number-estimates")
      :style toggle
      :selected (esn-number-estimates)
      ]
     ["Update $SUBROUTINE's ADVAN/TRANS descriptions." (esn-mode-toggle-var esn-update-sub)
      :style toggle
      :selected esn-update-sub]
     ["Update $TABLE's number of PRED variables" (esn-mode-toggle-var esn-table-pred-display)
      :style toggle
      :selected esn-table-pred-display
      ]
     ["Update $INPUT's number of variables" (esn-mode-toggle-var esn-update-input)
      :style toggle
      :selected esn-update-input
      ]
     ["Update extended variable names in model blocks when updating labels in estimates"
      (esn-mode-toggle-var esn-update-wfn-labels-when-editing)
      :style toggle
      :selected esn-update-wfn-labels-when-editing]
     ["Update Variable labels to have COV for covariance initial estimate*"
      (esn-mode-toggle-sticky-var "esn-mode-use-cov-type-labels")
      :style toggle
      :selected (esn-mode-use-cov-type-labels)
      ]
     ("Options"
      "ADVAN/TRANS update options"
      ["Use small $SUBROUTINE ADVAN/TRANS descriptions." (esn-mode-toggle-var esn-update-sub-small)
       :style toggle
       :selected esn-update-sub-small]
      "--"
      "Estimate Options"
      ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *" (esn-mode-toggle-sticky-var "esn-full-numbering")
       :style toggle
       :selected (esn-full-numbering)
       :active (esn-number-estimates)
       ]
      ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
       (esn-mode-toggle-sticky-var "esn-tos-comments-use-c")
       :style toggle
       :selected (esn-tos-comments-use-c)
       :active (and (esn-number-estimates) (not (esn-use-pdx-p)))
       ]
      )
     )
    "--"
    "Version Control"
    ["Automatically Version Control Files" (esn-mode-toggle-var esn-use-version-control)
     :style toggle
     :selected esn-use-version-control
     ]
    ["Automatic Description of Edit (based on time-stamp, otherwise prompt for version notes)"
     (esn-mode-toggle-var esn-vc-auto-commit)
     :style toggle
     :selected esn-vc-auto-commit
     ]
    ["Add new model version with new modification line"
     (esn-mode-toggle-var esn-vc-upon-new-modification-log-line)
     :style toggle
     :selected esn-vc-upon-new-modification-log-line
     ]
    ["Create new version of model, input and output files." (esn-vc-commit) ]
    ["Differences from last version" (call-interactively 'esn-vc-diff) ]
    ["Revision History" (vc-print-log) ]
    "--"
    "Code Generation"
    ["Generate CWRES code when appropriate."
     (esn-mode-toggle-var esn-automatically-generate-cwres)
     :style toggle
     :selected esn-automatically-generate-cwres
     ]
    ["Create multiple control streams to output more generated varaibles than NONMEM can handle."
     (esn-mode-toggle-var esn-table-split-pred)
     :style toggle
     :selected esn-table-split-pred
     ]
    ["On save, generate derived paramters like duration, AUC, etc., when known"
     (esn-mode-toggle-var esn-mode-generate-secondary-parameters)
     :style toggle
     :selected esn-mode-generate-secondary-parameters
     ]
    ("Options"
     "CWRES options"
     ["PsN compatability -- Use ../../ for .est files"
      (esn-mode-toggle-var esn-cwres-psn)
      :style toggle
      :selected esn-cwres-psn
      ]
     ["Automatically Remove CWRES code for unsuppored models."
      (esn-mode-toggle-var esn-cwres-remove)
      :style toggle
      :selected esn-cwres-remove
      ]
     "--"
     ["Generate CWRES for FO posthoc models."
      (esn-mode-toggle-var esn-cwres-fo-posthoc)
      :style toggle
      :selected esn-cwres-fo-posthoc]
     ["Generate CWRES for FOI posthoc models (NM6)"
      (esn-mode-toggle-var esn-cwres-foi-posthoc)
      :style toggle
      :selected esn-cwres-foi-posthoc]
     ["Generate CWRES for FOCE models."
      (esn-mode-toggle-var esn-cwres-foce)
      :style toggle
      :selected esn-cwres-foce
      ]
     ["Generate CWRES for FOCEI models."
      (esn-mode-toggle-var esn-cwres-focei)
      :style toggle
      :selected esn-cwres-focei
      ]
     ["Generate CWRES for Hybrid models."
      (esn-mode-toggle-var esn-cwres-hybrid)
      :style toggle
      :selected esn-cwres-hybrid
      ]
     ["Generate CWRES for Laplacian models."
      (esn-mode-toggle-var esn-cwres-lap)

      :style toggle

      :selected esn-cwres-lap
      ]
     ["Customize templates and other CWRES items" (lambda ()
                                                    (interactive)
                                                    (customize-group "esn-mode-cwres")
                                                    )
      t]

     )
    "--"
    "Header"
    ["On save, Add header if not present"
     (esn-mode-toggle-var esn-update-add-header-if-not-present)
     :style toggle
     :selected esn-update-add-header-if-not-present
     ]
    ["Add header with new files"
     (esn-mode-toggle-var esn-update-add-header-on-create)
     :style toggle
     :selected esn-update-add-header-on-create
     ]
    ["Use small headers"
     (esn-mode-toggle-var esn-header-small)
     :style toggle
     :selected esn-header-small
     ]
    "--"
    ("Large Header Program Specific Options"
     "Census"
     ["Number Estimates"
      (esn-mode-var "esn-census-number-estimates")
      :style toggle
      :selected esn-census-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-census-mirror-problem-purpose)
      :style toggle
      :selected esn-census-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-census-mode-use-cov-type-labels)
      :style toggle
      :selected esn-census-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-census-full-numbering)
      :style toggle
      :selected esn-census-full-numbering
      :active esn-census-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-census-tos-comments-use-c)
      :style toggle
      :selected esn-census-tos-comments-use-c
      :active (and esn-census-number-estimates (not (esn-use-pdx-p)))
      ]

     "--"
     "PDx Pop"
     ["Number Estimates"
      (esn-mode-var "esn-pdx-number-estimates")
      :style toggle
      :selected esn-pdx-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-pdx-mirror-problem-purpose)
      :style toggle
      :selected esn-pdx-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-pdx-mode-use-cov-type-labels)
      :style toggle
      :selected esn-pdx-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *" (esn-mode-toggle-var esn-pdx-full-numbering)
      :style toggle
      :selected esn-pdx-full-numbering
      :active esn-pdx-number-estimates
      ]
     "--"
     "PLT Tools"
     ["Number Estimates"
      (esn-mode-var "esn-plt-number-estimates")
      :style toggle
      :selected esn-plt-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-plt-mirror-problem-purpose)
      :style toggle
      :selected esn-plt-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-plt-mode-use-cov-type-labels)
      :style toggle
      :selected esn-plt-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-plt-full-numbering)
      :style toggle
      :selected esn-plt-full-numbering
      :active esn-plt-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-plt-tos-comments-use-c)
      :style toggle
      :selected esn-plt-tos-comments-use-c
      :active (and esn-plt-number-estimates (not (esn-use-pdx-p)))
      ]
     "--"
     "Pirana"
     ["Number Estimates"
      (esn-mode-var "esn-pirana-number-estimates")
      :style toggle
      :selected esn-pirana-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-pirana-mirror-problem-purpose)
      :style toggle
      :selected esn-pirana-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-pirana-mode-use-cov-type-labels)
      :style toggle
      :selected esn-plt-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-pirana-full-numbering)
      :style toggle
      :selected esn-pirana-full-numbering
      :active esn-pirana-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-pirana-tos-comments-use-c)
      :style toggle
      :selected esn-pirana-tos-comments-use-c
      :active (and esn-pirana-number-estimates (not (esn-use-pdx-p)))
      ]
     "--"
     "Other/Default"
     ["Number Estimates"
      (esn-mode-var "esn-def-number-estimates")
      :style toggle
      :selected esn-def-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-def-mirror-problem-purpose)
      :style toggle
      :selected esn-def-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-def-mode-use-cov-type-labels)
      :style toggle
      :selected esn-def-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-sticky-var "esn-def-full-numbering")
      :style toggle
      :selected esn-def-full-numbering
      :active esn-def-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-def-tos-comments-use-c)
      :style toggle
      :selected esn-def-tos-comments-use-c
      :active (and esn-def-number-estimates (not (esn-use-pdx-p)))
      ]
     )
    ("Small Header Program Specific Options"
     "Census"
     ["Number Estimates"
      (esn-mode-var "esn-census-small-number-estimates")
      :style toggle
      :selected esn-census-small-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-census-small-mirror-problem-purpose)
      :style toggle
      :selected esn-census-small-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-census-small-mode-use-cov-type-labels)
      :style toggle
      :selected esn-census-small-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-census-small-full-numbering)
      :style toggle
      :selected esn-census-small-full-numbering
      :active esn-census-small-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-census-small-tos-comments-use-c)
      :style toggle
      :selected esn-census-small-tos-comments-use-c
      :active (and esn-census-small-number-estimates (not (esn-use-pdx-p)))
      ]
     
     "--"
     "PDx Pop"
     ["Number Estimates"
      (esn-mode-var "esn-pdx-small-number-estimates")
      :style toggle
      :selected esn-pdx-small-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-pdx-small-mirror-problem-purpose)
      :style toggle
      :selected esn-pdx-small-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-pdx-small-mode-use-cov-type-labels)
      :style toggle
      :selected esn-pdx-small-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *" (esn-mode-toggle-var esn-pdx-small-full-numbering)
      :style toggle
      :selected esn-pdx-small-full-numbering
      :active esn-pdx-small-number-estimates
      ]
     "--"
     "PLT Tools"
     ["Number Estimates"
      (esn-mode-var "esn-plt-small-number-estimates")
      :style toggle
      :selected esn-plt-small-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-plt-small-mirror-problem-purpose)
      :style toggle
      :selected esn-plt-small-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-plt-small-mode-use-cov-type-labels)
      :style toggle
      :selected esn-plt-small-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-plt-small-full-numbering)
      :style toggle
      :selected esn-plt-small-full-numbering
      :active esn-plt-small-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-plt-small-tos-comments-use-c)
      :style toggle
      :selected esn-plt-small-tos-comments-use-c
      :active (and esn-plt-small-number-estimates (not (esn-use-pdx-p)))
      ]
     "--"
     "Pirana"
     ["Number Estimates"
      (esn-mode-var "esn-pirana-small-number-estimates")
      :style toggle
      :selected esn-pirana-small-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-pirana-small-mirror-problem-purpose)
      :style toggle
      :selected esn-pirana-small-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-pirana-small-mode-use-cov-type-labels)
      :style toggle
      :selected esn-plt-small-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-var esn-pirana-small-full-numbering)
      :style toggle
      :selected esn-pirana-small-full-numbering
      :active esn-pirana-small-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-pirana-small-tos-comments-use-c)
      :style toggle
      :selected esn-pirana-small-tos-comments-use-c
      :active (and esn-pirana-small-number-estimates (not (esn-use-pdx-p)))
      ]
     "--"
     "Other/Default"
     ["Number Estimates"
      (esn-mode-var "esn-def-small-number-estimates")
      :style toggle
      :selected esn-def-small-number-estimates
      ]
     ["Update $PROBLEM to match header purpose"
      (esn-mode-toggle-var esn-def-small-mirror-problem-purpose)
      :style toggle
      :selected esn-def-small-mirror-problem-purpose
      ]
     ["Update Variable labels to have COV for covariance initial estimate"
      (esn-mode-toggle-var esn-def-small-mode-use-cov-type-labels)
      :style toggle
      :selected esn-def-small-mode-use-cov-type-labels
      ]
     ["Use full labels for numbering (i.e. instead of \"1 -\", use \"THETA(1) -\") *"
      (esn-mode-toggle-sticky-var "esn-def-small-full-numbering")
      :style toggle
      :selected esn-def-small-full-numbering
      :active esn-def-small-number-estimates
      ]
     ["Use ;C for numbered comments (i.e. \";C 1 -\" or \";C THETA(1) -\") *"
      (esn-mode-toggle-var esn-def-small-tos-comments-use-c)
      :style toggle
      :selected esn-def-small-tos-comments-use-c
      :active (and esn-def-number-estimates (not (esn-use-pdx-p)))
      ]
     )
    )
  "Automation menu for use in `esn-mode'."
  )

(provide 'esn-keys-menu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-keys-menu.el ends here
