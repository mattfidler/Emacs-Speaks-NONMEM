;;; esn-tables.el --- Create/Delete Tables
;; 
;; Filename: esn-tables.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 20 08:29:58 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Dec 21 10:06:38 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 1294
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
;; 21-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 29 10:38:15 2011 (-0600) #1292 (Matthew L. Fidler)
;;    Added `esn-get-inputs'  It was somehow deleted.
;; 06-Jun-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Jun  6 11:11:21 2011 (-0500) #1289 (Matthew L. Fidler)
;;    Bug fix for esn-tables saving when the add/require is just a string.
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

(declare-function esn-fix-numbering "esn-extended")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-narrow-rec "esn-narrow")
(eval-when-compile
  (require 'cl)
  (require 'esn-options)
  (require 'esn-magic-keys))

(defmacro esn-to-bin (&rest args)
  "Take nil and true arguments and change into a binary number"
  (let ((ret (make-symbol "ret"))
        (n (make-symbol "n")))
    (setq ret 0)
    (setq n 0)
    (mapc
     (lambda(arg)
       (when arg
         (setq ret (+ ret (lsh 1 n))))
       (setq n (+ n 1)))
     (reverse args))
    ret))

;; Macro that reads etc/variables.txt into a hash table with binary
;; flags. On compile no reference to the file exists.
(defmacro esn/make/var/hash ()
  "Makes variable hasn"
  (let ((tmp (make-symbol "tmp"))
        (lst (make-symbol "lst"))
        (hash (make-symbol "hash"))
        (advan (make-symbol "advan"))
        (trans (make-symbol "trans")))
    ;; Transform and grab etc/variables.txt
    (with-temp-buffer
      (insert-file-contents (concat esn-path "etc/variables.txt"))
      (goto-char (point-min))
      (when (looking-at ".*\n")
        (setq tmp (match-string 0))
        (replace-match ""))
      (while (re-search-forward "\"" nil t)
        (replace-match ""))
      (goto-char (point-min))
      ;; Variable, Variable Name, Units, Values
      (while (re-search-forward "^\\(.*?\\)\t\\(.*?\\)\t\\(.*?\\)\t\\(.*?\\)\t" nil t)
        (replace-match "(\"\\1\" \"\\2\" \"\\3\" \"\\4\"\t"))
      (goto-char (point-min))
      (while (re-search-forward "\t$" nil t)
        (replace-match "\t0\t" t t))
      (goto-char (point-min))
      (while (re-search-forward "\t\t" nil t)
        (replace-match "\t0\t")
        (goto-char (match-beginning 0)))
      (goto-char (point-min))
      (while (re-search-forward "\\([01]\\)\t" nil t)
        (replace-match "\\1"))
      (goto-char (point-min))
      (while (re-search-forward "\t\\([01]\\)" nil t)
        (replace-match "\t#b\\1")
        (end-of-line))
      (goto-char (point-min))
      (while (re-search-forward "\\([01]\\)$" nil t)
        (replace-match (concat (match-string 1) "0000)")))
      (goto-char (point-min))
      (insert "(setq lst '(")
      (goto-char (point-max))
      (insert "))")
      (eval-buffer))
    (setq tmp (split-string tmp "[\t\n]" t))
    (pop tmp)
    (pop tmp)
    (pop tmp)
    (pop tmp)
    (setq tmp
          (mapcar
           (lambda(x)
             (make-symbol (downcase x)))
           (append  tmp (list "tv" "trans-only" "trans" "advan"))))
    ;; Now add advan and trans variables from already defined
    ;; `esn-advan-trans-vars'
    
    ;; Don't include descriptions; They will be queried from
    ;; `esn-advan-trans-vars'
    (mapc (lambda(x)
            (mapc (lambda (y)
                    ;; ADVAN+TRANS = 1 + 2 = 3
                    
                    ;; Advan 5 defines ALL variables in ADVAN, though
                    ;; usually the Ks and TVKs come from the TRANS
                    ;; statement.  Adjust accordingly.
                    (add-to-list 'lst
                                 (list (nth 0 y) "" "" ""
                                       (if (string-match "^TV" (nth 0 y))
                                           15
                                         (if (string-match "^K[0-9]+$" (nth 0 y))
                                             7
                                           3)))))
                  (nth 3 x))
            (mapc
             (lambda(y)
               (mapc
                (lambda(z)
                  ;; ADVAN+TRANS+TRANS_ONLY = 1 + 2 + 4 = 7
                  ;; ADVAN+TRANS+TRANS_ONLY+TV = 1 + 2 + 4 + 8 = 15
                  (add-to-list 'lst (list (nth 0 z) "" "" "" (if (string-match "^TV" (nth 0 z)) 15 7))))
                (nth 3 y)))
             (nth 4 x)))
          esn-advan-trans-vars)
    (setq hash (make-hash-table :size (length lst) :test 'equal))
    (mapc (lambda(x)
            (puthash (car x) (cdr x) hash))
          lst)
    `(progn
       (setq esn-variable-hash ,hash) ;"Hash table for known NONMEM variables"
       (defmacro* esn-var-prop-num (&rest rest &key,@tmp &allow-other-keys)
         "Gets the property number based on keys"
         (list 'esn-to-bin ,@tmp)))))

(defmacro esn-prop-num-p (var prop)
  "Gets If the property is contained in the variable var in the
hash table `esn-variable-hash' (uses bitwise comparisons)."
  `(let ((ret (make-symbol "ret")))
     (setq ret (gethash ,var esn-variable-hash))
     (if (not ret)
         ret
       (not (= 0 (logand ,prop (nth 3 ret)))))))

(defvar esn-tables-hash (make-hash-table :test 'equal)
  "Hash table for Table Properties.  Hashed on property number.")

(setq esn-tables-hash (make-hash-table :test 'equal))

(defmacro esn-subset-table-num (prop)
  "Subsets table to variables that are contained in the binary number prop."
  `(let ((ret (make-symbol "ret")))
     (setq ret (gethash ,prop esn-tables-hash))
     (if ret
         (symbol-value 'ret)
       (maphash
        (lambda(key lst)
          (when (not (= 0 (logand ,prop (nth 3 lst))))
            (push key ret)))
        esn-variable-hash)
       (setq ret (append (list (replace-regexp-in-string "[*]" ".*" (replace-regexp-in-string "#" "[0-9]+" (regexp-opt ret 'words)))) ret))
       (puthash ,prop ret esn-tables-hash)
       (symbol-value 'ret))))

(defmacro* esn-subset-table (&rest keys &allow-other-keys)
  "Subset table to specified keys"
  `(esn-subset-table-num (esn-var-prop-num ,@keys)))

(defmacro* esn-subset-regexp (&rest keys &allow-other-keys)
  "Get regular expression corresponding to the specified keys."
  `(nth 0 (esn-subset-table ,@keys)))

(defmacro* esn-subset-vars (&rest keys &allow-other-keys)
  "Get list of variables corresponding to the specified keys."
  `(cdr (esn-subset-table ,@keys)))

(esn/make/var/hash)

(defmacro esn-&keys-filter (args filter)
  "Filter and change arguments for Deftable."
  (let ((ret (make-symbol "ret"))
        (last-table-name nil))
    (setq ret
          (mapcar
           (lambda(x)
             (if (and (symbolp x)
                      (string-match (concat "^" (regexp-quote (symbol-name filter))) (symbol-name x)))
                 (intern (replace-match ":" t t (symbol-name x)))
               (if (functionp x)
                   `(funcall ',x)
                 x)))
           args))
    ret))

(defcustom esn-table-update-hook '()
  "Defines tables to update"
  :type 'hook
  :group 'esn-automatically-fill-in-tables)

;;;###autoload
(defun esn-table-name-extension (extension)
  "Defines a table name based on the current control stream by dropping an extension and adding a new extension.  If the buffer is not saved, return nil."
  (if (buffer-file-name)
      (concat (esn-runname-noext 't) extension)
    nil))

(defmacro* esn-deftable (name &rest rest
                              &key
                              table-name
                              (delete t)
                              (table-options "NOPRINT")
                              (delete-orphans nil)
                              condition
                              filter-add
                              group
                              add-extra
                              require-extra
                              &allow-other-keys)
  "Defines a table for use with EsN.

:TABLE-NAME defines either the table name as a string or a function
that returns the table name as a string.

:GROUP defines a single group where table customizations are done.

:DELETE defines if undefined variables should be deleted. The
default is to delete. Set this to nil if you wish to ignore
undefined variables.

:DELETE-ORPHANS defines if variables included in the table, but
would not be included in the table if the table was generated
from scratch, should be deleted.  The default is to keep these values.

:TABLE-OPTIONS defines what table options should be defined for
this table.  By default this is NOPRINT.  NOAPPEND/APPEND are
calculated based on variables added to the table.

:FILTER-ADD is a function that filters what should be added.

:CONDITION -- Condition when table is added.  By default the
table is NEVER added on save.  Used when adding function to
`esn-table-update-hook'

To create tables of NONMEM generated parameters, the following options are used:

First the required variables; the variables required to create a table:

:REQUIRE-POPULATION t -- Require all population based parameters (Like TVCL)
:REQUIRE-POPULATION regexp -- Require population based parameters (Like TVCL) matching regular expression

:REQUIRE-INDIVIDUAL t -- Require all individual parameters (Like CL)
:REQUIRE-INDIVIDUAL regexp -- Require individual parameters (Like CL) matching regular expression

:REQUIRE-ERROR t includes ALL error variables (Variables associated with EPS)
:REQUIRE-ERROR regexp includes error variables matching regular expression (Variables associated with EPS)

:REQUIRE-PRED -- Require PRED type variables
:REQUIRE-PAR-RES -- Require Residual type variables
:REQUIRE-PAR-IOV -- Require IOV type variables.

:REQUIRE-OTHER t includes all variables that are not defined above
:REQUIRE-OTHER regexp includes variables that are not defined above matching a regular expression.

:REQUIRE-ETA t -- Requires all ETA parameters (ETA1 ETA2 ETA3, etc)

:INCLUDE-IOV t -- Include IOV-based etas in table output.

Next are the variables to add if any required variables are found:

:ADD-POPULATION t -- Add all population based parameters (Like TVCL)
:ADD-POPULATION regexp -- Add population based parameters (Like TVCL) matching regular expression

:ADD-INDIVIDUAL t -- Add all individual parameters (Like CL)
:ADD-INDIVIDUAL regexp -- Add individual parameters (Like CL) matching regular expression

:ADD-PRED
:ADD-PAR-RES                            
:ADD-PAR-IOV

:ADD-ERROR t includes ALL error variables (Variables associated with EPS)
:ADD-ERROR regexp includes error variables matching regular expression (Variables associated with EPS)

:ADD-OTHER t includes all variables that are not population, individual error or ETA variables
:ADD-OTHER regexp includes variables that are not population, individual error or ETA variables matching a regular expression.

:ADD-ETA t -- Adds all ETA parameters (ETA1 ETA2 ETA3, etc)

:ADD-EXTRA list -- defines a list of extra variables that will be added.  This is customizable.

:REQUIRE-EXTRA list --defines a list of extra variables that will be added.  If present this table will be added.  This is customizable.

All other ADD/REQUIRE statements refer to variables defined in the $INPUT record, like ID.

For example, defining the PLT Tools Table AllRecords.txt, you may use:



"
  (declare (indent 1))
  (let ((req-arg (make-symbol "req-arg"))
        (add-arg (make-symbol "add-arg")))
    (setq req-arg (macroexpand `(esn-&keys-filter ,rest :require-)))
    (setq add-arg (macroexpand `(esn-&keys-filter ,rest :add-)))
    `(progn
       (defcustom  ,(intern (concat (symbol-name name) "-add-table")) ,add-extra
         ,(concat "Defines extra variables added when `" (symbol-name name)"-table' is called (if present).  These are only added if there is at least some required variables.")
         :type '(repeat
                 (string :tag "Variable to add"))
         :group ,group)
       (defcustom  ,(intern (concat (symbol-name name) "-req-table")) ,require-extra
         ,(concat "Defines extra variables added when `" (symbol-name name)"-table' is called (if present). These variables are added to the required list")
         :type '(repeat
                 (string :tag "Variable to add"))
         :group ,group)
       (add-hook 'esn-table-update-hook (lambda() (when ,condition (,(intern (concat (symbol-name name) "-table"))))))
       (defun ,(intern (concat (symbol-name name) "-table")) ()
         ,(if (functionp table-name)
              (concat "A function to create/update tables with the table-name produced by `" (symbol-name table-name) "'.  Generated by `esn-deftable'")
            (concat "A function to create/update the " table-name " table. Generated by `esn-deftable'"))
         (interactive)
         (let* ((debug-on-error t)
                (debug-on-quit t)
                (time (float-time))
                req-reg add-reg req-p add-p all-p inputs
                (table-options ,table-options)
                (tab-name ,(if (functionp table-name)
                               `(funcall ',table-name)
                             table-name))
                current-table
                add orphan undef delreg del)
           (when tab-name
             (message "[EsN] Determining contents of %s." tab-name)
             (setq req-reg (esn-subset-regexp ,@req-arg))
             (setq add-reg (esn-subset-regexp ,@add-arg))
             
             (setq req-p (esn-get-parameters ,@req-arg))
             (setq add-p (esn-get-parameters ,@add-arg))
             ;; Add customized content.
             (when ,(intern (concat (symbol-name name) "-req-table"))
               (mapc (lambda(x)
                       (add-to-list 'req-p x))
                     ,(intern (concat (symbol-name name) "-req-table"))))
             
             (when ,(intern (concat (symbol-name name) "-add-table"))
               (mapc (lambda(x)
                       (add-to-list 'add-p x))
                     ,(intern (concat (symbol-name name) "-add-table"))))
             (setq all-p (esn-get-parameters :all t))
             (setq inputs (esn-get-inputs))
             (mapc
              (lambda(x)
                (cond
                 ((and (not (string= "" req-reg)) (string-match req-reg x))
                  (add-to-list 'req-p x))
                 ((and (not (string= "" add-reg)) (string-match add-reg x))
                  (add-to-list 'add-p x))))
              inputs)
             ;; Filter what is added (if needed)
             
             (when (and req-p (< 0 (length req-p)))
               (setq current-table (split-string (esn-get-table-variables tab-name)))
               (cond
                ((not current-table) ;; Just add the table.
                 (esn-add-table tab-name (sort (append add-p req-p) 'string<) table-options))
                (t ;; Modify table -- First determine which are already defined
                 (mapc
                  (lambda(x)
                    (cond
                     ((and (member x add-p)
                           (not (member x current-table)))
                      (add-to-list 'add x))
                     ((and (member x req-p)
                           (not (member x current-table)))
                      (add-to-list 'add x))
                     ((and (member x current-table)
                           (or (member x inputs)
                               (member x all-p))
                           (not (member x add-p))
                           (not (member x req-p)))
                      (add-to-list 'orphan x)
                      ,(if delete-orphans
                           '(delete* x current-table :test 'string=))
                      ,(if delete-orphans
                           '(add-to-list 'del x)))
                     ((and (member x current-table)
                           (not (member x inputs))
                           (not (member x all-p)))
                      (add-to-list 'undef x)
                      ,(if delete
                           '(delete* x current-table :test 'string=))
                      ,(if delete
                           '(add-to-list 'del x)))))
                  (append add-p req-p current-table))
                 ;; OK now actually modify table.
                 (cond
                  ((or (= 0 (length current-table)) ;; nothing's left
                       (and (= (length current-table) (length add-p)) ;; All that left is optional.
                            (equal (sort add-p 'string<) (sort current-table 'string<))))
                   ;; Delete table.  Nothing of note is left.
                   (esn-remove-table tab-name))
                  (t
                   (when del
                     (setq del (regexp-opt del 'words)))
                   (esn-modify-table tab-name (mapconcat (lambda(x) x) add " ") del table-options))))))))))))




(defvar esn-get-parameters-all nil
  "All parameters")
(make-variable-buffer-local 'esn-get-parameters-all)
(defmacro defvar-gpc (&rest pars)
  "Defines the esn-get-parameter-X-cache parameter values"
  (let ((alst '((pop population)
                (ind individual)
                (err error)
                (prd pred)
                (rs par-res)
                (etas eta)
                (iv par-iov)
                (misc other)
                (cw cwtab))))
    (append (list 'progn)
            (mapcar
             (lambda(x)
               `(defvar ,(intern (format "esn-get-parameters-%s-cache" x)) nil))
             pars)
            (mapcar
             (lambda(x)
               `(make-variable-buffer-local (quote ,(intern (format "esn-get-parameters-%s-cache" x)))))
             pars)
            (list 
             `(defun esn-get-parameters-cached-p ()
                "Determines if parameters have been cached."
                (and ,@(mapcar
                        (lambda(x)
                          (intern (format "esn-get-parameters-%s-cache" x)))
                        pars)))
             `(defun esn-get-parameters-set-cache ()
                "Sets parameter cache."
                ,@(mapcar
                   (lambda(x)
                     `(setq ,(intern (format "esn-get-parameters-%s-cache" x)) ,x)
                     )
                   pars))
             `(defun esn-get-parameters-get-cache ()
                "Gets parameter cache."
                ,@(mapcar
                   (lambda(x)
                     `(setq ,x ,(intern (format "esn-get-parameters-%s-cache" x)))
                     )
                   pars))
             `(defun esn-get-parameters-set-ret ()
                "Sets return value without looping (which apparently is taking some time)"
                (setq ret (append ret
                                  ,@(mapcar
                                     (lambda (x)
                                       `(if (or all (equal t ,@(assoc-default x alst))) ,x
                                          (when (stringp ,@(assoc-default x alst))
                                            (delete-if-not
                                             (lambda(y) (string-match ,@(assoc-default x alst) y))
                                             ,x))))
                                     pars))))
             `(defun esn-get-parameters-clear-cache ()
                "Clears parameter cache."
                ,@(mapcar
                   (lambda(x)
                     `(setq ,(intern (format "esn-get-parameters-%s-cache" x)) nil))
                   pars))))))
(defvar-gpc pop ind err prd rs etas iv misc cw)
;;(message "%s" (macroexpand '(defvar-gpc pop ind err prd rs etas iv misc)))
;; Clear cache on abbreviated record modification.
(esn-abbrev-modification-hook 'esn-get-parameters-clear-cache)
(esn-abbrev-modification-hook (lambda() (setq esn-get-parameters-all nil)))

(defun* esn-get-parameters (&rest rest &key population population-fixed individual eta pred include-iov error other par-res par-iov cwtab all &allow-other-keys)
  "Gets a list of defined parameters from the control stream

:population t includes ALL estimated population variables
:population regular-expression includes population variables
matching regular expression

:population-fixed t includes ALL fixed population variables
:population-fixed  regular-expression includes fixed population
variables matching regular expression

:individual t includes ALL individual variables
:individual regular-expression includes individual variables
matching regular expression

:error t includes ALL error variables (Variables associated with EPS)
:error regular-expression includes error variables matching
regular expression (Variables associated with EPS)

:pred t includes ALL PRED variables (IPRED OPRED PPRED etc) and DV
:pred regular-expression includes PRED variables and DV that match a regular expression

:par-res t includes ALL RES variables (IRES ORES PRES etc)
:par-res regular-expression includes RES variables regular expression

:eta t includes ALL eta variables
:include-iov t includes IOV etas in the result

:par-iov t includes ALL IOV variables i.e. IOV1 = ETA(1)*OCC1+ETA(2)*OCC2 includes IOV1.
:par-iov regular-expression includes IOV variables matching a regular expression.

:cwtab t includes ALL CWTAB variables COM(1)=G11 or G11 or
nothing depending on the version of NONMEM used and the verbatim
code.

:other t includes ALL variables not matched above.
:other regular-expression includes variables not matched above
that match the supplied regular expression.


:all t includes all possible variables"
  ;; Should try to save parameters in an overlay that automatically
  ;; updates. For now...
  (let (predpk
        (start2 0)
        (start 0) var val rem-pop
        pop popx ind err iv misc rs
        etas prd est
        iov-etas ret cw)
    ;; Take out extended control stream references.
    (if (and all esn-get-parameters-all)
        (setq ret esn-get-parameters-all)
      (if (esn-get-parameters-cached-p)
          (esn-get-parameters-get-cache)
        (setq predpk (esn-rec (esn-reg-record-exp (append esn-current-abbrev-records '("THE" "OME" "SIG"))) t))
        (with-temp-buffer
          (insert predpk)
          (set-syntax-table esn-mode-syntax-table)
          (esn-fix-numbering)
          (setq predpk (esn-rec (esn-reg-record-exp esn-current-abbrev-records) t)))
        (while (string-match ";.*" predpk start)
          (setq predpk (replace-match "" nil nil predpk))
          (setq start (match-beginning 0)))
        ;; Get cwtab variables.
        (cond
         ((esn-nm-ver>= 7);; Do nothing.  CWRES data item generated automatically. 
          )
         ((and (esn-nm-ver>= 6.2);; Just add the GH components.  No verbatim code is necessary.
               (or
                (and esn-cwres-foce (esn-is-foce))
                (and esn-cwres-focei (esn-is-focei))
                (and esn-cwres-hybrid (esn-is-hybrid))
                (and esn-cwres-fo-posthoc (esn-is-fo-posthoc))
                (and esn-cwres-foi-posthoc (esn-is-foi-posthoc))
                (and esn-cwres-lap (esn-is-lap))))
          (let ((maxeta (esn-max-eta))
                (maxeps (esn-max-eps))
                (i 0))
            (while (< i maxeta)
              (setq i (+ i 1))
              (add-to-list 'cw (format "G%s1" i)))
            (setq i 0)
            (while (< i  maxeps)
              (setq i (+ 1 i))
              (add-to-list 'cw (format "H%s1" i)))))
         ((esn-nm-ver>= 5);; Add the COM and GH components.
          (setq start 0)
          (while (string-match "^\" +COM(\\([0-9]+\\)) *= *\\(G\\|H\\)H?(\\([0-9]+\\) *\\, *1 *)" predpk start)
            (setq start (match-beginning 0))
            (add-to-list 'cw (format "COM(%s)=%s%s1" (match-string 1 predpk) (match-string 2 predpk) (match-string 3 predpk)))
            (setq predpk (replace-match "" nil nil predpk))
            (setq added 't))))
        (setq start 0)
        (while (string-match "^[ \t]*\\([A-Za-z_][^ \t]*\\)[ \t]*=\\([^=]*\\)$" predpk start)
          (setq var (match-string 1 predpk))
          (setq val (match-string 2 predpk))
          (setq predpk (replace-match "" nil nil predpk))
          (setq start (match-beginning 0))
          (with-syntax-table esn-mode-syntax-table
            (cond
             ((string-match (esn-subset-regexp :iov t) var)
              (add-to-list 'iv (format "%s" var))
              ;; IOV ETAs don't add anything.  Should not add them to many
              ;; tables.  Separate them out here.  Assume any ETAs in the
              ;; val statement are IOV etas.
              (setq start2 0)
              (while (string-match "\\<ETA(\\([0-9]+\\))" val start2)
                (setq start2 (match-end 0))
                (add-to-list 'iov-etas (format "ETA%s" (match-string 1 val)))))
             ((string-match "PRED$" var)
              (add-to-list 'prd (format "%s" var)))
             ((string-match "RES$" var)
              (add-to-list 'rs (format "%s" var)))
             ;; Order should be EPS then ETA and then THETA to prefer ERROR, then INdividual then Population.
             ;; That way
             ;; CL=THETA(x)*DEXP(ETA(y)) is an individual parameter instead of a population parameter.
             ((string-match "\\<EPS([0-9]+)" val)
              ;; The practice of setting error
              
              ;; W = THETA(1)
              ;; Y = F+EPS(1)*W
              
              ;; Should have W as an error parameter not a population
              ;; parameter.  Therefore any variable previously considered
              ;; population variables that are located in the line with
              ;; EPS(x) values, assume they are error values.
              (setq start2 0)
              (while (string-match (regexp-opt pop 'words) val start2)
                (setq start2 (match-end 0))
                (add-to-list 'err (match-string 0 val))
                (add-to-list 'rem-pop (match-string 0 val)))
              (add-to-list 'err (format "%s" var)))
             ((string-match "\\<ETA([0-9]+)" val)
              (add-to-list 'ind (format "%s" var)))
             ((string-match "\\<THETA([0-9]+)" val)
              ;; Check for FIXED vs UNFIXED
              (setq est nil)
              (setq start2 0)
              (while (and (not est)
                          (string-match "\\<THETA(\\([0-9]+\\))" val start2))
                (setq est (not (esn-fixed-theta-p (match-string 1 val))))
                (setq start2 (match-end 0)))
              (if est
                  (add-to-list 'pop (format "%s" var))
                (add-to-list 'popx (format "%s" var))))
             
             (t
              (add-to-list 'misc (format "%s" var))))))
        ;; Add parameters that may be generated by NONMEM.
        (add-to-list 'misc "MDV") ; Required by CWRES table
        (add-to-list 'prd "DV")
        (add-to-list 'prd "PRED")
        (add-to-list 'rs "RES")
        (add-to-list 'rs "WRES")
        ;; Doesn't support MC methods yet.
        (when (esn-nm-ver>= 7)
          (cond
           ((esn-is-foce-p)
            (add-to-list 'rs "CRES")
            (add-to-list 'rs "CWRES")
            (add-to-list 'prd "CPRED"))
           ((esn-is-focei-p)
            (add-to-list 'rs "CRESI")
            (add-to-list 'rs "CWRESI")
            (add-to-list 'prd "CPREDI"))
           ((esn-is-foi-p)
            (add-to-list 'rs "RESI")
            (add-to-list 'rs "WRESI")
            (add-to-list 'prd "PREDI"))))
        
        ;; I prefer the actual parameter value to be output as CL instead of TVCL
        ;;
        ;; Therefore, I would prefer population parameter to be CL in this instance:
        ;;
        ;; TVCL = THETA(1)
        ;; CL = TVCL
        ;;
        ;; Currently CL is in the Misc category.
        
        ;; Residual components should be in error.  Anything matching RES$
        ;; is considered a residual.  Currently these are in Misc
        (delete-if (lambda(var)
                     (if (not (member (concat "TV" var) pop))
                         (if (not (string-match "RES$" var )) nil
                           (add-to-list 'err (format "%s" var))
                           t)
                       (add-to-list 'rem-pop (concat "TV" var)) ;; Add to removed list
                       (add-to-list 'pop (format "%s" var))
                       t)) misc)
        (delete-if (lambda(var) (member var rem-pop)) pop)
        (dotimes (i (esn-max-eta t))
          (unless (member (format "ETA%s" (+ i 1)) iov-etas)
            (add-to-list 'etas (format "ETA%s" (+ i 1)))))
        ;; Save to cache.
        (esn-get-parameters-set-cache))
      ;; Cache ALL parameters.  Probably will be used MULTIPLE times.
      (when (or include-iov all)
        (setq etas (append etas iov-etas)))
      (esn-get-parameters-set-ret)
      (when all
        (setq esn-get-parameters-all ret)))
    ret))


;; Create a get inputs cache.  Flush the cache whenever $INPUT is modified.
(defvar esn-get-inputs-cache (make-hash-table :test 'equal)
  "Hash to cache $INPUT lookups.")
(make-variable-buffer-local 'esn-get-inputs-cache)
(esn-rec-modification-hook "input" (lambda() (setq esn-get-inputs-cache (make-hash-table :test 'equal))))


;;;###autoload
(defun* esn-get-inputs (&rest rest &key (keep 'valid) (alias 'list) (return 'items) &allow-other-keys)
  "Gets a list of defined inputs from the control stream.

INPUT represent the input statements (possibly cached from other functions..?)

:keep defines how to handle DROP and SKIP records

:keep 'alias changes the input record as follows:

original $INPUT record:

 $INPUT ONE TWO=DROP THREE=SKIP DROP SKIP

to a list:

 (\"ONE\" \"TWO\" \"THREE\" \"DROP\" \"SKIP\")

:KEEP 'drop changes input record as follows:

original $INPUT record:

 $INPUT ONE TWO=DROP THREE=SKIP DROP SKIP

to a list:

 (\"ONE\" \"DROP\" \"SKIP\" \"DROP\" \"SKIP\")

:KEEP 'valid change input record as follows:

original $INPUT record:

 $INPUT ONE TWO=DROP THREE=SKIP DROP SKIP

to a list:

 (\"ONE\")

The handling of aliases have three possible methods:


:ALIAS 'first

Changes the original $INPUT from

 $INPUT FIRST=SECOND THIRD=FOURTH FIFTH

to a list:

 (\"FIRST\" \"THIRD\" \"FIFTH\")

:ALIAS 'second

Changes the original $INPUT from

 $INPUT FIRST=SECOND THIRD=FOURTH FIFTH

to a list:

 (\"SECOND\" \"FOURTH\" \"FIFTH\")

:ALIAS 'list

Changes the original $INPUT from

 $INPUT FIRST=SECOND THIRD=FOURTH FIFTH

to a list:

 ((\"FIRST\" \"SECOND\") (\"THIRD\" \"FOURTH\") \"FIFTH\")


The return value can be either:

:RETURN 'list, which gives a list as stated above

:RETURN 'items, which gives a list with no sublists contained in
it.  For example:

The record

 $INPUT FIRST=SECOND THIRD=FOURTH FIFTH

Would return the following list:

 (\"FIRST\" \"SECOND\" \"THIRD\" \"FOURTH\" \"FIFTH\")



:RETURN 'regexp, which gives a regular expression of the kept variables.

"
  (if (gethash (list keep alias return) esn-get-inputs-cache)
      (gethash (list keep alias return) esn-get-inputs-cache)
    (let ((inp (esn-rec "INP" 't))
          ret
          (start 0))
      ;; Drop $INPUT record statements. 
      (while (string-match (eval-when-compile (esn-reg-record-exp "INP" nil)) inp start)
        (setq start (match-beginning 0))
        (setq inp (replace-match "" nil nil inp)))
      ;; Delete comments
      (setq start 0)
      (while (string-match ";.*" inp start)
        (setq start (match-beginning 0))
        (setq inp (replace-match "" nil nil inp)))
      ;; Replace newlines
      (setq start 0)
      (while (string-match "\n" inp start)
        (setq start (match-beginning 0))
        (setq inp (replace-match " " nil nil inp)))
      ;; Change DROP and SKIP
      ;; DROP = VAR
      (setq start 0)
      (while (string-match "\\(DROP\\|SKIP\\)[ \t]*=[ \t]*\\([A-Za-z][^ \t]*\\)" inp start)
        (cond
         ((eq keep 'alias) ;; Keep the alias instead.
          (setq start (+ (match-beginning 0) (length (match-string 2))))
          (setq inp (replace-match "\\2" t nil inp)))
         ((eq keep 'drop) ; Keep the Drop or skip statement
          (setq start (+ (match-beginning 0) (length (match-string 1))))
          (setq inp (replace-match "\\1" t nil inp)))
         ((eq keep 'valid) ; remove DROP and SKIP statements
          (setq start (match-beginning 0))
          (setq inp (replace-match "" nil nil inp)))))
      ;; VAR = DROP
      (setq start 0)
      (while (string-match "\\<\\([A-Za-z][^ \t]*\\)[ \t]*=[ \t]*\\(DROP\\|SKIP\\)" inp start)
        (cond
         ((eq keep 'alias) ;; Keep the alias.
          (setq start (+ (match-beginning 0) (length (match-string 1))))
          (setq inp (replace-match "\\1" t nil inp)))
         ((eq keep 'drop) ;; Keep the Drop/skip statement
          (setq start (+ (match-beginning 0)) (length (match-string 2)))
          (setq inp (replace-match "\\2" t nil inp)))
         ((eq keep 'valid) ;Remove Drop and Skip statements
          (setq start (match-beginning 0))
          (setq inp (replace-match "" nil nil inp)))
         (t
          (error ":keep option not specified appropriately in `esn-get-inputs'")
          (setq start (match-end 0)))))
      ;; Remaining DROP and SKIP statements.
      (unless (eq keep 'valid)
        (setq start 0)
        (while (string-match "\\(DROP\\|SKIP\\)" inp start)
          (setq inp (replace-match "" nil nil inp))
          (setq start (match-beginning 0))))
      ;; Now deal with aliases
      (setq start 0)
      (while (string-match "\\<\\([A-Za-z][^ \t]*\\)[ \t]*=[ \t]*\\([A-Za-z][^ \t]*\\)\\>" inp start)
        (cond
         ((and (eq alias 'list) (eq return 'regexp))
          (setq start (+ 1 (match-beginning 0) (length (match-string 1)) (length (match-string 2))))
          (setq inp (replace-match "\\1 \\2" t nil inp)))
         ((eq alias 'first)
          (setq inp (replace-match "\\1" t nil inp))
          (setq start (+ (match-beginning 0) (length (match-string 1)))))
         ((eq alias 'second)
          (setq inp (replace-match "\\2" t nil inp))
          (setq start (+ (match-beginning 0) (length (match-string 2)))))
         ((eq alias 'list)
          (setq start (+ 1 (match-beginning 0) (length (match-string 1)) (length (match-string 2))))
          (setq inp (replace-match "\\1=\\2" t nil inp)))
         (t
          (error ":alias option not specified appropriately in `esn-get-inputs'"))))
      (setq ret (split-string inp (if (or (eq return 'regexp)
                                          (eq return 'items))
                                      "[ \t=]+" nil) t))
      (cond
       ((eq return 'list)
        (seq ret (mapcar (lambda(x) (split-string x "[=]+" t)))))
       ((eq return 'regexp)
        (setq ret (regexp-opt ret 'words))))
      (puthash (list keep alias return) ret esn-get-inputs-cache)
      (symbol-value 'ret))))

(defun esn-add-table (name var opt)
  "Adds table NAME  with variables VAR and options OPT to the control stream."
  (let (tab (new-var var))
    ;; Add/Remove NOAPPEND and DV, PRED, WRES, RES variables.
    (if (and (member "DV" var)
             (member "PRED" var)
             (member "WRES" var)
             (member "RES" var)
             )
        (setq new-var (delete-if (lambda(x) (member x '("DV" "PRED" "WRES" "RES" "NOAPPEND"))) new-var))
      (setq new-var (append new-var '("NOAPPEND"))))
    ;; Create table text
    (setq tab (format "$TABLE %s %s FILE=%s\n" (mapconcat (lambda(x) x) new-var " ")
                      opt name))
    (save-excursion
      (esn-insert-after tab (append esn-insert-table-after (list "SIM" "EST" "NON")) 't))))

(defun esn-modify-table (name add del opt)
  "Modify Table NAME adding ADD deleting the regular expression DEL and adding OPT if not present."
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward (format "\\<FILE?\\>.*?%s" (regexp-quote name)) nil t)
               (string= "TAB" (esn-get-current-rec)))
      (let ((f (match-beginning 0)) o)
        (save-restriction
          (esn-narrow-rec)
          (when opt
            (mapc
             (lambda(x)
               (goto-char (point-min))
               (unless (re-search-forward (format "\\<%s\\>" (regexp-quote x)) nil t)
                 (add-to-list 'o x)))
             (split-string opt))
            (goto-char f)
            (insert " " (mapconcat (lambda(x) x) o " ") " "))
          (goto-char (point-min))
          (re-search-forward (eval-when-compile (esn-reg-record-exp "TAB" t t)) nil t)
          (insert " " add " ")
          (when del
            (while (re-search-forward del nil t)
              (replace-match "")))
          (goto-char (point-min))
          (while (re-search-forward "  +" nil t)
            (replace-match " ")))))))

(defun esn-remove-table (name)
  "Removes the output table NAME"
  (save-restriction
    (let ((case-fold-search 't)
          (n name))
      (when (string-match "[0-9]+" n)
        (setq n (replace-match "[0-9]+" nil nil n)))
      (save-excursion
        (goto-char (point-min))
        (if (not (re-search-forward (format "\\<FILE *[= \t] *[^ ]*\\(?:\\.\\.[/\\\\]\\)*%s\\>" n) nil t)) nil
          (esn-narrow-rec)
          (delete-region (point-min) (point-max))
          (widen)
          (if (looking-at "\n")
              (replace-match "")))))))

(defun esn-get-table-variables (name)
  "Gets the variables output in the table NAME."
  (save-restriction
    (let (
          (tab "")
          (case-fold-search 't)
          (n name))
      (when (string-match "[0-9]+" n)
        (setq n (replace-match "[0-9]+" nil nil n)))
      (save-excursion
        (goto-char (point-min))
        (if (not (re-search-forward (format "\\<FILE *[= \t] *[^ ]*%s\\>" n) nil t)) nil
          (esn-narrow-rec)
          (setq tab (buffer-substring-no-properties (point-min) (point-max)))
          (widen)
          (if (or (string-match "\\<APPE[A-Z]+\\>" tab)
                  (not (string-match "\\<NOAP[A-Z]+\\>" tab)))
              (setq tab (concat tab " DV PRED WRES RES")))
          (while (string-match (format "\\<%s[A-Z]*\\>"
                                       (regexp-opt '("APPE"
                                                     "COND"
                                                     "FORW"
                                                     "NOAP"
                                                     "NOFO"
                                                     "NOPR"
                                                     "OMIT"
                                                     "ONEH"
                                                     "PRIN"
                                                     "UNCO"
                                                     "FIRS"
                                                     "$TAB"
                                                     ) 't)) tab)
            (setq tab (replace-match "" nil nil tab)))
          (while (string-match 
                  (format "\\<%s[A-Z]* *[= \t] *[^ \n]+\\(?:\\.\\.[/\\\\]\\)*"
                          (regexp-opt '("FILE") 't))
                  tab)
            (setq tab (replace-match "" nil nil tab)))
          (while (string-match (eval-when-compile
                                 (format "%s[A-Z]* *[= \t] *[^ \t]*"
                                         (regexp-opt '("ESA" "SEE" "FOR") 't)))
                               tab)
            ;; Remove SEED=, ESAMPLE=, and FORMAT= statements.
            (setq tab (replace-match "" nil nil tab)))
          (while (string-match "\n" tab)
            (setq tab (replace-match " " nil nil tab)))))
      (symbol-value 'tab))))

(provide 'esn-tables)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-tables.el ends here
