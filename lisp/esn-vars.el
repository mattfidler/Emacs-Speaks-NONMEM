;;; esn-vars.el --- Esn Variables
;; 
;; Filename: esn-vars.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon Aug 30 08:33:17 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 18:58:29 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 130
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Taken out of Esn-mode so there is no recursive requiring for esn-mode.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 09:28:13 2010 (-0500) #107 (Matthew L. Fidler)
;;    Moved a few variables from esn-options.el here.
;; 30-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Mon Aug 30 10:27:21 2010 (-0500) #27 (Matthew L. Fidler)

;;    
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
;; Local Variables
(require 'esn-reg)

(defvar esn-complete-declare-operators '(
                                         "BLOCK"
                                         "BACKSPACE"
                                         "CALL"
                                         "COMMON"
                                         "CONTINUE"
                                         "DDIM"
                                         "DIM" "DIMENSION"
                                         "DOUBLE"
                                         "ENTRY"
                                         "EQUIVALENCE" "EXIT" "GOTO"
                                         "IMPLICIT"
                                         "INDEX" "INQUIRE" "INTEGER" "INTRINSIC" "IOSTAT"
                                         "ISIGN" "LE" "LGE" "LGT" "LLE" "LLT" "EXTERNAL"
                                         "FILE" "FLOAT" "FORMAT" "LOGICAL" "OPEN"  "PARAMETER" "PAUSE" "PRECISION" "PRINT" "PROGRAM" "READ"
                                         "REAL" "REAL" "RETURN" "REWIND" "SAVE" 
                                         "STATUS" "STOP" "SUBROUTINE"   "UNIT" "WRITE"
                                         "BTEST"
                                         "EOF" "GETDAT" "GETTIM" "HFIX" "IAND" "IEOR" "INTERFACE"
                                         "LOCKING" "LOCNEAR"  "SETTIM"  "ERR" "LACFAR"
                                         )
  "* Fortran operators"
  )
(defvar esn-complete-logic-operators '("AND"
                                       "EQ" "GE" "FALSE"  "GT"
                                       "NE" "NOT"  "OR" "LT" "TRUE" "NOT"  "THEN" 
                                       )
  "Logic Operators"
  )
(defvar esn-complete-operators '("ABS" "ACOS" "AIMAG" "AINT" "ALOG"
                                 "ALOG10" "AMAX0" "AMAX1" "AMIN0" "AMIN1" "AMOD"
                                 "ANINT" "ASIN" "ATAN" "ATAN2" 
                                 "CABS"  "CCOS" "CEXP" "CHAR" "CHARACTER"
                                 "CLOG" "CLOSE" "CMPLX"  "COMPLEX" "CONJG" 
                                 "COS" "COSH" "CSIN" "CSQRT" "DBLE" "DCOS" "DCOSH"  "DEXP"
                                 "DINT" "DLOG" "DMAX1" "DMIN1" "DMOD" "DNINT" 
                                 "DPROD" "DSIGN" "DSIN" "DSINH" "DSQRT" "DTAN" "DTANH"  
                                 "EXP"  
                                 "FUNCTION"   "IABS" "ICHAR" "IDIM" "IDINT" "IDNINT"
                                 "IFIX"  
                                 "LEN"  "LOG" "LOG10" 
                                 "MAX" "MAX0" "MAX1" "MIN" "MIN0" "MIN1" "MOD"  "NINT" 
                                 "SIGN" "SIN" "SINH" "SNGL"
                                 "SQRT" "TAN" "TANH"  "CDABS" "CDCOS" "CDEXP" "CDLOG" "CDSIN" "CDSQRT" "COTAN" "DCMPLX"
                                 "DCONJG" "DCOTAN" "DIMAG" "DREAL" 
                                 "IBCHNG" "IBCLR" "IBSET"  "IMAG" "INT" "INT1" "INT2" "INT4" "INTC"
                                 "IOR" "ISHA" "ISHC" "ISHFT" "ISHL" "JFIX"  "LOC"
                                 "DABS" "DACOS" "DASIN" "DATA" "DATAN"
                                 "DATAN2""ETDAT")
  "* List of operators for NONMEM"
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xpose
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar esn-use-xpose-saved nil)
(defvar esn-use-xpose-save nil)
(defvar esn-inhibit-xpose nil
  "* Defines if xpose functions are inhibited"
  )
(defvar esn-xpose-get-input-line-save nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WFN
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-wfn-runname-last-bfn nil
  "* Local variable telling the last buffer file name.  If the current buffer file name is nil, use this."
  )
;; Excluded Variables 

(defvar esn-wfn-excluded-variables 
  (eval-when-compile
    (with-temp-buffer
      (insert
       (regexp-opt (append (mapcar (lambda(x) (concat "~" x "!")) (append esn-complete-operators esn-complete-logic-operators esn-complete-declare-operators))
                           '(
                             ;; If/Then
                             "~CALL!" "~CALLFL!" "~DO!" "~ELSE!" "~ELSEIF!" "~ENDIF!"
                             "~EXIT!"
                             "~IF!"  "~MODE!"  "~PASS!" "~RANDOM!" "~RETURN!" "~SIMEPS!" "~SIMETA!"
                             "~SQRT!" "~SUPP!" "~WHILE!" "~ENDDO!"
                             "~THEN!"

                             ;; Non-word operators

                             ".AND." ".EQ." ".GE." ".GT." ".LE." ".LT." ".NE." ".OR." ".AND." ".TRUE."
                             ".FALSE." "==" "/=" ">" ">=" "<" "<="
                             )) 't))
      (goto-char (point-min))
      (while (re-search-forward "~" nil t)
        (replace-match "\\\\<"))
      (goto-char (point-min))
      (while (re-search-forward "!" nil t)
        (replace-match "\\\\>"))
      (buffer-substring (point-min) (point-max))
      )
    )
  "Regular expression that shows the excluded names for WFN variables."
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-force-plt nil
  "* Variable to force PLT tools to be used.  Please do not modify directly. It will have undesired consequences."
  )
(defvar esn-force-no-plt nil
  "* Variable to force PLT tools  not to be used.  Please do not modify directly. It will have undesired consequences."
  )
(defvar esn-use-plt-saved nil
  "* Is the PLT state saved?"
  )
(defvar esn-use-plt-save nil
  "* PLT state"
  )
(defvar esn-plt-auto-timer nil
  "* Create PLT auto timer now."
  )
(defvar esn-plt-list-last-buffer nil)

(defvar esn-plt-exec-queue '()
  "* Defines a queue of runs that are slated for execution."
  )
(defvar esn-plt-list-font-lock-keywords nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xmind
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar esn-xmind-checkout-time nil
  "* Defines the time-stamp of the xmind file that is checked out."
  )
(defvar esn-xmind-checkout-file nil
  "* Defines the file name of the xmind file that is checked out."
  )
(defvar esn-xmind-checkout-dir nil
  "* Defines the directory of the xmind-file that is checked out."
  )
(defvar esn-use-xmind-saved nil
  "* Xmind use saved?"
  )
(defvar esn-use-xmind-save nil
  "* Xmind use"
  )
(defvar esn-xmind-ctl-xml
  "<children><topics type=\"attached\"><topic branch=\"folded\" id=\"26i7agq08s4eug0m6qhlv3g57m\" structure-class=\"org.xmind.ui.spreadsheet\" ><title>Properties</title><children><topics type=\"attached\"><topic id=\"5hql31jdsct7kugrko83q2th2d\"><title>Model</title><children><topics type=\"attached\"><topic id=\"7fsloob92b8100e4ta75nht33q\"><title>Desc</title><labels><label>Value</label></labels></topic></topics></children></topic><topic id=\"2f4ko2vjlhds2v7dubpaaq7sq0\"><title>Estimation</title><children><topics type=\"attached\"><topic id=\"102phlsfaibf4admhl8isrkgqg\"><title>?</title><labels><label>Value</label></labels></topic></topics></children></topic><topic id=\"335it18bfcscb3o2rgimnh81bp\"><title>Data</title><children><topics type=\"attached\"><topic id=\"7tdqp442439rbg374m5k0fa9sb\" xlink:href=\"file:./\"><title>?</title><labels><label>Value</label></labels></topic></topics></children></topic><topic id=\"63uughk40gafobjrcjbjmsanru\"><title># Thetas</title><children><topics type=\"attached\"><topic id=\"37f5o0dbiub07vlbh4q714etkf\"><title>4</title><labels><label>Value</label></labels></topic></topics></children></topic><topic id=\"3v9tmi35lqfuddo776i40d834b\"><title># Etas</title><children><topics type=\"attached\"><topic id=\"7r0kphqa619tejvlopkmjumpku\"><title>1</title><labels><label>Value</label></labels></topic></topics></children></topic><topic id=\"41josv7du4lock04i711mvvfpq\"><title># Eps</title><children><topics type=\"attached\"><topic id=\"2remne6814trm5jdi5722bspqd\" timestamp=\"1264711105383\"><title>1</title><labels><label>Value</label></labels></topic></topics></children></topic></topics></children></topic></topics></children>"
  "* Xmind template for xml"
  )

(defvar esn-xmind-ctl-est ""
  "* Estimate method for current buffer"
  )
(defvar esn-xmind-ctl-ntheta ""
  "* Number of thetas for current buffer"
  )
(defvar esn-xmind-ctl-neta ""
  "* Number of etas for current buffer"
  )
(defvar esn-xmind-ctl-neps ""
  "* Number of eps for current buffer"
  )
(defvar esn-xmind-ctl-mdesc ""
  "* Model description for current buffer"
  )
(defvar esn-xmind-ctl-data ""
  "* Data for current buffer"
  )
(defvar esn-xmind-default-tree-children
  "<topic id=\"%s\" timestamp=\"%s\"><title>%s</title><children><topics type=\"attached\">%s</topics></children></topic>"
  "* Default Topic for Tree structure creation -- children
 #1 -- Topic ID
 #2 -- Timestamp
 #3 -- Short Title
 #4 -- children"
  )
(defvar esn-xmind-default-tree
  "<topic id=\"%s\" timestamp=\"%s\"><title>%s</title></topic>"
  "* Default Topic for Tree structure creation -- no children
 #1 -- Topic ID
 #2 -- Timestamp
 #3 -- Short Title
")
(defvar esn-xmind-default-tree-child
  "<topic id=\"%s\" timestamp=\"%s\"><title>%s</title><labels><label>%s</label></labels></topic>"
  "* Default Topic for Tree structure creation -- no children
 #1 -- Topic ID
 #2 -- Timestamp
 #3 -- Short Title
")
(defvar esn-xmind-default-topic
  "<topic id=\"%s\" timestamp=\"%s\" xlink:href=\"file:%s\"><title>%s</title><marker-refs><marker-ref marker-id=\"other-question\"/></marker-refs><labels><label>%s</label></labels></topic>"
  "Default Control Stream Topic without children
 (
 #1 -- Topic ID
 #2 -- Timestamp
 #3 -- Model Link
 #4 -- Short Title
 #5 -- Model Label
 )"
  )

(defvar esn-xmind-last-parent-topic-time nil
  "* Last Parent Topic Timestamp;  Initialized to nil to say not taken.")
(defvar esn-xmind-last-parent-topic nil
  "* Last Parent Topic ID;  Initialized to nil to say not taken.")
(defvar esn-add-update-topics-timer nil
  "*  Update Topics Time (lets xmind not HAVE to update as soon as you save)."
  )
(defvar esn-xmind-last-topic-title ""
  "* Defines the last Topic title"
  )

(defvar esn-xmind-list-of-completions-id nil
  "* List of completions"
  )
(defvar esn-xmind-list-of-completions-topic nil
  "* List of completions"
  )
(defvar esn-xmind-list-of-completions-titles nil
  "* List of completions"
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Switch
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Units
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-prefix-units
  '(
    ("Y" 24) ;; Yotta 10^24
    ("Z" 21) ;; Zetta 10^21
    ("E" 18) ;; Exa 10^18
    ("P" 15) ;; Peta 10^15
    ("T" 12) ;; Tera 10^12
    ("G" 9) ;; Giga 10^9
    ("M" 6) ;; Mega 10^6
    ("k" 3) ;; Kilo 10^3
    ("h" 2) ;; Hecto 10^2
    ("da" 1) ;; Deka 10^1
    ("d" -1) ;; Deci 10^-1
    ("c" -2) ;; Centi 10^-2
    ("m" -3) ;; Milli 10^-3
    ("u" -6) ;; Micro 10^-6 (should be mu)
    ("mu" -6);;
    ("n" -9) ;; nano 10^-9
    ("p" -12) ;; pico
    ("f" -15) ;; femto
    ("a" -18) ;; atto
    ("z" -21) ;; zepto
    ("y" -24) ;; yocoto
    ("" 0) ;; Nothing.
    )
  )

(defvar esn-time-units
  '(
    ("sec" "sec")
    ("secs" "sec")
    ("second" "sec")
    ("seconds" "sec")
    ("hour" "hr")
    ("hours" "hr")
    ("hrs" "hr")
    ("hr" "hr")
    ("day" "day")
    ("days" "day")
    ("dys" "day")
    ("dy" "day")
    )
  "* Time Units"
  )
(defvar esn-time-regexp
  (eval-when-compile
    (concat "\\(?:" (regexp-opt (append
                                 (mapcar (lambda(x)
                                           (upcase x)
                                           )
                                         (all-completions ""  esn-time-units)
                                         )
                                 (mapcar (lambda(x)
                                           (downcase x)
                                           )
                                         (all-completions ""  esn-time-units)
                                         )
                                 )
                                ) "\\)")))
(defvar esn-prefix-regexp
  (eval-when-compile
    (concat "\\(?:" (regexp-opt (all-completions ""  esn-prefix-units)) "\\)")))


(defvar esn-mode-scale-translations
  '(
    ("1" (
          ("C" "Central")
          ("O" "Output")
          ("0" "Output")
          ("1" "Central")
          ("2" "Output")
          ))
    ("2" (
          ("C" "Central")
          ("O" "Output")
          ("0" "Output")
          ("2" "Central")
          ("3" "Output")
          ))
    ("3" (
          ("C" "Central")
          ("O" "Output")
          ("0" "Output")
          ("1" "Central")
          ("3" "Output")
          ))
    ("4" (
          ("C" "Central")
          ("O" "Output")
          ("0" "Output")
          ("2" "Central")
          ("4" "Output")))
    ("10" (
           ("C" "Central")
           ("O" "Output")
           ("0" "Output")
           ("1" "Central")
           ("2" "Output")))
    ("11" (
           ("C" "Central")
           ("O" "Output")
           ("0" "Output")
           ("1" "Central")
           ("4" "Output")
           ))
    ("12" (
           ("C" "Central")
           ("O" "Output")
           ("0" "Output")
           ("2" "Central")
           ("5" "Output")))
    )
  "* Defines the translation number for the scale factor."
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toolbar
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-icon-directory
  (expand-file-name (concat (file-name-as-directory esn-path) "etc/icons"))
  "*Location for ESN icons.
This variable should be set automatically by the ESN install process.
Icons should be found in ESN/etc/icons/ directory.
If `esn-icon-directory' is invalid, please report a bug.")


(defvar esn-toolbar nil
  "Toolbar items to be added to ESN editing buffers.")


(defvar esn-toolbar-plt-archive nil
  "Toolbar items to be added to ESN read-only buffers.")



(defvar esn-toolbar-plt nil
  "Toolbar items to be added to ESN editing buffers.")

(defvar esn-toolbar-pdx nil
  "Toolbar items to be added to ESN editing buffers.")

(defvar esn-toolbar-pirana nil
  "Toolbar items to be added to ESN editing buffers.")

(defvar esn-toolbar-census nil
  "Toolbar items to be added to ESN editing buffers.")
(defvar esn-toolbar-simple nil
  "Toolbar items to be added to ESN editing buffers.")
(defvar esn-toolbar-xemacs-general
  (list
   [toolbar-file-icon toolbar-open t "Open a file"]
   [toolbar-disk-icon toolbar-save t "Save buffer"]
   [toolbar-printer-icon generic-print-buffer t "Print buffer"]
   [toolbar-cut-icon toolbar-cut t "Kill region"]
   [toolbar-copy-icon toolbar-copy t "Copy region"]
   [toolbar-paste-icon toolbar-paste t "Paste from clipboard"]
   [toolbar-undo-icon toolbar-undo t "Undo edit"]
   [toolbar-replace-icon toolbar-replace t "Search & Replace"]
   [:style 3d]
   )
  "General Xemacs icons to be added iff `esn-toolbar-own-icons' is non-nil.
These toolbar items were taken from the list that John Fox's code provided.
Each vector is of length four specifying: 1 - icon; 2 - function to call;
3 - whether to activate; 4 - doc string.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Known table options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar esn-table-pred-tab-opts
  (eval-when-compile
    (format "\\(%s\\(?: *=? *%s*\\)?\\)"
            (regexp-opt '(
                          "APPEND"
                          "CONDITIONAL"
                          "FILE"
                          "FIRSTONLY"
                          "FORWARD"
                          "NOAPPEND"
                          "NOFORWARD"
                          "NOHEADER"
                          "NOPRINT"
                          "OMITTED"
                          "ONEHEADER"
                          "PRINT"
                          "UNCONDITIONAL"
                          ) 'words) 
            esn-reg-filename
            )))

(defvar esn-table-split-pt nil
  "* Placeholder for the current position before splitting the tables."
  )
(defvar esn-table-split-count-timer nil
  "* Timer to call esn-table-split")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Space Thetas
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar esn-separator-between-eta-number-and-description " "
  "* Separator between eta number and description")

(defvar esn-separator-between-variable-and-description " "
  "* Separator between eta and description")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RV
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar esn-mode-rv-do-add 't)
(defvar esn-mode-rv-do-prop 't)
(defvar esn-mode-rv-do-addprop 't)
(defvar esn-mode-rv-do-logn 't)

(defvar esn-mode-rv-add-ini "1")
(defvar esn-mode-rv-prop-ini "1")
(defvar esn-mode-rv-addprop-add-ini "1")
(defvar esn-mode-rv-addprop-prop-ini "1")
(defvar esn-mode-rv-logn-ini "1")
(defvar esn-mode-rv-cb "")
(defvar esn-mode-rv-fn "")
(defvar esn-mode-rv-is-log nil)
(defvar esn-mode-rv-is-norm nil)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pirana
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-use-pirana-saved nil
  "* Esn Use Pirana Saved?"
  )
(defvar esn-use-pirana-save nil
  "* Esn Pirana?"
  )
(defvar esn-force-pirana nil
  "* Variable to force Pirana to be used.  Please do not modify directly. It will have undesired consequences."
  )
(defvar esn-force-no-pirana nil
  "* Variable to force Pirana not to be used.  Please do not modify directly. It will have undesired consequences."
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PDX
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar esn-force-pdx nil
  "* Variable to force PDX pop to be used.  Please do not modify directly. It will have undesired consequences.")
(defvar esn-force-no-pdx nil
  "* Variable to force PDX pop not to be used.  Please do not modify directly. It will have undesired consequences.")
(defvar esn-use-pdx-saved nil
  "* Use PLT saved?"
  )
(defvar esn-use-pdx-save nil
  "* Use PLT save."
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Narrow
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-get-current-record-start nil
  "* Starting point for current record"
  )
(defvar esn-get-current-record-stop nil
  "* Stopping point for current record"
  )
(defvar esn-get-current-record-stop2 nil
  "* Stopping point for current record (outside of record but before next)"
  )
(defvar esn-get-current-record-eof nil
  "* EOF for current record.  If this is off, then the saved value is off."
  )
(defvar esn-get-current-record-val nil
  )
(defvar esn-get-current-record-len nil
  )
(defvar esn-get-current-record-abbrev nil
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magic Keys variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar esn-run-save-fn nil
  "* Defines if running save functions."
  )

(defvar esn-post-command-hook-timer nil
  "Post command hook timer."
  )
(defvar esn-command-hook-lag-time 0.125
  "Reqired number of seconds for readraw of with command."
  )
(defvar esn-use-lag-time nil
  "Use a lag time to redraw the post command.")

(defvar esn-skip-wrap nil
  "* Defines if one should skip wrapping for the present time.")

(defvar esn-magic-wrap-timer nil
  "* Magic Wrap Timer")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-input-number-of-vars 0
  "* A variable to store if the input is over 20 (50 with NONMEM 7) variables."
  )
(defvar esn-mode-input-comments ""
  "* Defines the input mode comments.")

(defvar esn-font-lock-input-number nil)
(defvar esn-font-lock-input-data "")

(defvar esn-number-input-begin 0
  )
(defvar esn-number-input-end 0
  )
(defvar esn-number-input-warning-reg nil
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-invisible-areas-list ()
  "List of invisible overlays used by ESN.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function (esn-fun)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-lastlst '())
(defvar esn-lastpoint nil)
(defvar esn-abs-dir-hash '()
  "* Variable saving calculated absolute directories."
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-font-lock-operators-reg
  (eval-when-compile
    (with-temp-buffer
      (insert
       (regexp-opt (append
                    (mapcar (lambda(x) (concat "~" x "!")) (append esn-complete-operators esn-complete-logic-operators esn-complete-declare-operators))
                    '(
                      ;; If/Then
                      "~CALL!" "~CALLFL!" "~DO!" "~ELSE!" "~ELSEIF!" "~ENDIF!"
                      "~EXIT!"
                      "~IF!"  "~MODE!"  "~PASS!" "~RANDOM!" "~RETURN!" "~SIMEPS!" "~SIMETA!"
                      "~SQRT!" "~SUPP!" "~WHILE!" "~ENDDO!"
                      "~THEN!"

                      ;; Non-word operators

                      ".AND." ".EQ." ".GE." ".GT." ".LE." ".LT." ".NE." ".OR." ".AND." ".TRUE."
                      ".FALSE." "==" "/=" ">" ">=" "<" "<="
                      )) 't))
      (goto-char (point-min))
      (while (re-search-forward "~" nil t)
        (replace-match "\\\\<"))
      (goto-char (point-min))
      (while (re-search-forward "!" nil t)
        (replace-match "\\\\>"))
      (buffer-substring (point-min) (point-max))
      )
    )
  "* Operators defined by NONMEM"
  )


(defvar esn-font-lock-keywords nil
  "* Font lock keywords for Emacs Speaks NONMEM")

(defvar esn-font-lock-input-number nil)
(defvar esn-font-lock-input-data "")
(defvar esn-font-lock-bad-mus-cached nil
  "* Cached bad mus?"
  )
(make-variable-buffer-local 'esn-font-lock-bad-mus-cached)
(defvar esn-font-lock-bad-mus-save nil
  "* Bad mus save variable"
  )
(make-variable-buffer-local 'esn-font-lock-bad-mus-save)
(defvar esn-last-point-max nil
  "* Last maximum point"
  )
(make-variable-buffer-local 'esn-last-point-max)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-current-run nil
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-current-completions  nil
  "* Temporary variable to save completions")
(defvar esn-completing-upcase nil
  "*  A Flag for lightning completion.  When upcase is true, then forces uppercase completion, otherwise when nil, any character can complete.  All completion is upper-case except file-name completion?")
(defvar esn-completing-smart-case nil
  "*  A flag for lightning completion.  When this is true, try to figure out the best completion character to insert, be it upper case or lower case.")

(defvar esn-completing-covariates nil
  "* Determines if the theta is completing covariates or not.")

(defvar esn-reg-complete-msg "^`\\([^']*\\)'")

(defvar esn-list-completions '(
                               ("DAT" (
                                       ("TRA" ("TIME/24" "TIME/24.000" "TIME/24.00" "II/24" "II/24.000" "II/24.00"))
                                       )
                                )
                               )
  "* List of option list completions"
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Census
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-use-census-saved nil)
(defvar esn-use-census-save nil)
(defvar esn-force-census nil
  "* Variable to force Census to be used.  Please do not modify directly. It will have undesired consequences."
  )
(defvar esn-force-no-census nil
  "* Variable to force Census not to be used.  Please do not modify directly. It will have undesired consequences."
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar esn-commit-last-version nil
  "* Local variable telling the previous version should be added to version control."
  )
(defvar esn-from-new-file nil
  "* Local variable telling EsN if this was a new file (suppresses automatic version control on first save).")
(defvar esn-mode nil
  "* Defines if esn-mode is activated.")

(defvar esn-update-cp-save-vals '()
  "* Defines the saves for the copied buffer.")
(defvar esn-update-cp-purpose-save ""
  "* Defines the purpose for the copied file"
  )
(defvar esn-mode-hook nil)
(defvar esn-before-update-hook nil)
(defvar esn-default-directory ""
  "* String of the default directory for this file."
  )
(defconst xemacs (string-match "XEmacs" emacs-version)
  "non-nil iff XEmacs, nil otherwise")
(defvar esn-last-small-name nil
  "* A variable that indicates the last saved small name (to be replaced on problem line).")
(defvar esn-save-last-full-name nil
  "* A variable that indicates the last full saved name on a save-as operation in emacs."
  )
(defvar esn-last-full-name nil
  "* A variable that indicates the last full saved name from `buffer-file-name'"
  )
(defvar esn-save-copy-stat nil
  "Defines if a copy status is saved yet."
  )
(defvar esn-save-copy-var nil
  "Defines if the copy status")

(defvar esn-last-purpose nil
  "* A variable that indicates the last purpose.")
(defvar esn-advan-trans-completions-pk-flag nil
  "* A varaible that stores the pk completion flag for the current advan/trans combination.")
(defvar esn-advan-trans-completions-pk
  '()
  "* An alist that stores the pk record completions for a currrent advan/trans combination.")
(defvar esn-omega-last-defined-diagonal 0
  " * A marker for the last defined diagonal element in omega.")
(defvar esn-sigma-last-defined-diagonal 0
  " * A marker for the last defined diagonal element in omega.")

(defvar esn-mode-echo-initially nil)
(defvar esn-viper-mode-map nil)

(defvar esn-delete-comment-if-deleting-item nil)
(defvar esn-use-units nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PsN
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-mode-psn-check-install nil
  "Variable for checking if PsN is installed")
(defvar esn-mode-psn-install-val nil
  "Variable to store if PsN is installed")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-vc-committing-buffer ""
  "* Defines the buffer that is being committed before a save."
  )
(defvar esn-buffer-should-be-read-only nil
  "* Defines if the buffer should be opened for editing (nil) or should be read only (non-nil)")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yas variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar esn-yas-start nil
  "* Yas candidate"
  )
(defvar esn-yas-last-theta nil
  "* Last theta"
  )

(make-variable-buffer-local 'esn-yas-last-theta)
(defvar esn-yas-added-2-thetas '()
  "* A Yas eta-selection step added 2 thetas instead of just one"
  )
(make-variable-buffer-local 'esn-yas-added-2-thetas)

(defvar esn-yas-last-unit nil
  "*Last Unit defined"
  )
(make-variable-buffer-local 'esn-yas-last-unit)
(defvar esn-yas-last-tv nil
  "*Last TV"
  )
(make-variable-buffer-local 'esn-yas-last-tv)

(defvar esn-yas-last-theta-value 0
  "* Last Theta Defined"
  )
(make-variable-buffer-local 'esn-yas-last-theta-value)

(defvar esn-yas-pending-thetas nil
  "* List of pending thetas"
  )
(make-variable-buffer-local 'esn-yas-pending-thetas)
(defvar esn-yas-pending-omegas nil
  "* List of pending omegas"
  )
(make-variable-buffer-local 'esn-yas-pending-omegas)
(defvar esn-yas-pending-snippets nil
  "* List of pending snippets"
  )
(make-variable-buffer-local 'esn-yas-pending-snippets)
(defvar esn-yas-pending-snippet-timer nil
  "* Timer for Yas pending snippets"
  )
(defvar esn-yas-pre-point nil
  "* The pre-point that the snippet should return to after completion"
  )
(defvar esn-yas-theta-post-snippets nil
  "* Variable for storing post-snippets for $THETA insertion"
  )

(defvar esn-symbol-words-lst
  '(
    ("INF" 8734)
    ;; 1/4, 1/2, 3/4
    ("1/4" 188 )
    ("1/2" 189 )
    ("3/4" 190 )
    ;; (c) and (R)
    ("(R)" 174)
    ("ALPHA" 945)
    ("BETA" 946)
    ("GAMMA" 947)
    ("DELTA" 948)
    ("ERR" 949)
    ("EPS" 949)
    ("ZETA" 950)
    ("ETA" 951)
    ("THETA" 952)
    ("IOTA" 953)
    ("KAPPA" 954)
    ("LAMBDA" 955)
    ("MU" 956)
    ("NU" 957)
    ("XI" 958)
    ("PI" 960)
    ("RHO" 961)
    ("SIGMA" 963)
    ("TAU" 964)
    ("UPSILON" 965)
    ("PHI" 966)
    ("CHI" 967)
    ("PSI" 968)
    ("OMEGA" 969)
    ;; Hebrew
    ("ALEF" 1488))
  "* EsN symbol words"
  )

(defvar esn-symbol-non-words
  '(
    (".EQ." 61)
    ("==" 61)
    (".NE." 8800)
    ("/=" 8800)
    (".GT." 62)
    (".LT." 62)
    (".LE." 8804)
    ("<=" 8804)
    (".GE." 8805);
    (">=" 8805)
    ("*" 183))
  "* EsN non-word symbols"
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(make-variable-buffer-local 'font-lock-keywords)
(make-variable-buffer-local 'font-lock-defaults)
(make-variable-buffer-local 'esn-wfn-runname-last-bfn)
(make-variable-buffer-local 'esn-number-input-begin)
(make-variable-buffer-local 'esn-number-input-end)
(make-variable-buffer-local 'esn-number-input-warning-reg)
(make-variable-buffer-local 'esn-number-theta-timer)
(make-variable-buffer-local 'esn-number-eta-timer)
(make-variable-buffer-local 'esn-number-eps-timer)
(make-variable-buffer-local 'esn-xpose-get-input-line-save)
(make-variable-buffer-local 'esn-mode-tried-ini)
(make-variable-buffer-local 'esn-table-split-count-timer)
(make-variable-buffer-local 'esn-align-matrix-timer)
(make-variable-buffer-local 'esn-get-current-record-eof)
(make-variable-buffer-local 'esn-get-current-record-start)
(make-variable-buffer-local 'esn-get-current-record-stop)
(make-variable-buffer-local 'esn-get-current-record-stop2)
(make-variable-buffer-local 'esn-get-current-record-val)
(make-variable-buffer-local 'esn-get-current-record-len)
(make-variable-buffer-local 'esn-get-current-record-abbrev)
(make-variable-buffer-local 'esn-align-equals-fun-timer)
(make-variable-buffer-local 'esn-magic-wrap-timer)
(make-variable-buffer-local 'esn-make-link-overlays-timer)
(make-variable-buffer-local 'esn-mode-xpose-gen-summary-timer)
(make-variable-buffer-local 'esn-plt-auto-timer)
(make-variable-buffer-local 'esn-add-update-topics-timer)
(make-variable-buffer-local 'esn-vc-first-prompt)
(make-variable-buffer-local 'esn-last-small-name)
(make-variable-buffer-local 'esn-last-full-name)
(make-variable-buffer-local 'esn-save-copy-stat)
(make-variable-buffer-local 'esn-save-copy-var)
(make-variable-buffer-local 'esn-last-purpose)
(make-variable-buffer-local 'esn-time-unit)
(make-variable-buffer-local 'esn-volume-unit)
(make-variable-buffer-local 'esn-update-cp-save-vals)
(make-variable-buffer-local 'esn-default-directory)
(make-variable-buffer-local 'esn-completing-upcase)
(make-variable-buffer-local 'esn-mode)
(make-variable-buffer-local 'esn-inhibit-xpose)
(make-variable-buffer-local 'max-lisp-eval-depth)
(make-variable-buffer-local 'esn-wfn-dups)
(make-variable-buffer-local 'esn-var-names)
(make-variable-buffer-local 'esn-wfn-last-kill-item)
(make-variable-buffer-local 'esn-commit-last-version)
(make-variable-buffer-local 'esn-buffer-should-be-read-only)
(make-variable-buffer-local 'esn-mode-current-dv)
(make-variable-buffer-local 'esn-from-new-file)
(make-variable-buffer-local 'esn-from-new-file)
(make-variable-buffer-local 'ess-nuke-trailing-whitespace-p)
(make-variable-buffer-local 'esn-xmind-last-parent-topic)
(make-variable-buffer-local 'esn-xmind-last-parent-topic-time)
(make-variable-buffer-local 'esn-xmind-list-of-completions-id)
(make-variable-buffer-local 'esn-xmind-list-of-completions-topic)
(make-variable-buffer-local 'esn-xmind-list-of-completions-titles)
(make-variable-buffer-local 'esn-xmind-last-topic-title)
(make-variable-buffer-local 'esn-xmind-ctl-est)
(make-variable-buffer-local 'esn-xmind-ctl-ntheta)
(make-variable-buffer-local 'esn-xmind-ctl-neta)
(make-variable-buffer-local 'esn-xmind-ctl-neps)
(make-variable-buffer-local 'esn-xmind-ctl-mdesc)
(make-variable-buffer-local 'esn-xmind-ctl-data)
(make-variable-buffer-local 'esn-abs-dir-hash)
(make-variable-buffer-local 'esn-use-plt-saved)
(make-variable-buffer-local 'esn-use-plt-save)
(make-variable-buffer-local 'esn-use-pdx-saved)
(make-variable-buffer-local 'esn-use-pdx-save)
(make-variable-buffer-local 'esn-use-pirana-save)
(make-variable-buffer-local 'esn-use-pirana-saved)
(make-variable-buffer-local 'esn-use-census-saved)
(make-variable-buffer-local 'esn-use-census-save)
(make-variable-buffer-local 'esn-use-xpose-save)
(make-variable-buffer-local 'esn-use-xpose-saved)
(provide 'esn-vars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-vars.el ends here
