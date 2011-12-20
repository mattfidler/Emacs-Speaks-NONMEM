;;; esn-code.el --- Produces NONMEM code for specific things
;; 
;; Filename: esn-code.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Jul 28 10:45:20 2010 (-0500)
;; Version: 0.1 
;; Last-Updated: Thu Apr 28 00:57:52 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 88
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
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 09:43:52 2010 (-0500) #84 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
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

(declare-function esn-narrow-rec "esn-narrow")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the number of differential equations needed before NONMEM fails.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NONMEM 6 PRCOMG
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; ___________________________________________________________________
 ;; |                                                                 |
 ;; |                                PRCOMG                           |
 ;; |_________________________________________________________________|

 ;; MEANING: PREDPP common
 ;; CONTEXT: For use with PREDPP

 ;; This feature is not fully documented.  The interested user may be able  L
 ;; to  obtain  more  information  by studying the appropriate sections of  L
 ;; PREDPP code.                                                            L

 ;; USAGE:
 ;; $PK
 ;; "FIRST
 ;; " COMMON /PRCOMG/ MITER,METH,IMAX,ISTFLG,INTFLG
 ;; " INTEGER MITER,METH,IMAX,ISTFLG,INTFLG
 ;; " IMAX=200000

 ;; DISCUSSION:
 ;; The common allows the user to over-ride certain  default  settings  in
 ;; ADVAN6, ADVAN8, ADVAN9, and SS6.

 ;; MITER and METH

 ;;   PREDPP sets MITER and METH to default values with every new  indivi-
 ;;   dual or reset record.

 ;;   ADVAN8
 ;;        Always use METH=2.  If user set MITER, use his  value.   Other-
 ;;        wise, use MITER=2.

 ;;   ADVAN9
 ;;        If user set METH, use MF=METH*10 in call to LSODI.   Otherwise,
 ;;        use MF=20

 ;;        If user did not set MITER, then set MITER to 1 (if  no  popula-
 ;;        tion etas) or 2 (if population etas).

 ;;        If user did set MITER, use his value.

 ;; IMAX

 ;;   ADVAN6, ADVAN8, ADVAN9, SS6
 ;;        The variable MAXCAL gives the maximum number of calls  to  FCN1
 ;;        (ADVAN6,  ADVAN8,  SS6)  or  RES (ADVAN9) during an integration
 ;;        interval.
 ;;        Each of the above routines sets MAXCAL to the  value  given  by  |
 ;;        MAXFCN (a parameter in the include file SIZES) at the start of   |
 ;;        each integration interval unless the user supplies a  value  in
 ;;        IMAX, in which case the user's value is used.

 ;; ISTFLG

 ;;   ADVAN9
 ;;        ISTFLG controls how ADVAN9 calls LSODI1 and  what  it  does  if
 ;;        LSODI1  returns  and  indicates  that  an  integration  failed.
 ;;        ISTFLG is set to 0 (default) at ICALL=0.   If  changed  by  the
 ;;        user,  it  retains  the changed value until the user changes it
 ;;        again.  ISTATE is a variable that  is  passed  from  ADVAN9  to
 ;;        LSODI1.   ISTATE=1  indicates that the integration is starting.
 ;;        ISTATE=2 indicates that this call  is  a  continuation  from  a
 ;;        prior  successful  integration.   Default: Use ISTATE=1 for the
 ;;        first integration, and ISTATE=2 for a continuation when nothing
 ;;        external  to  ADVAN9  has  changed.   In  case  of failure with
 ;;        ISTATE=2, restore original inputs and try again with ISTATE=1.

 ;;   ISTFLG=1
 ;;        Never try ISTATE=2, always use ISTATE=1.

 ;;   ISTFLG=2
 ;;        Never retry (only try ISTATE=2).

 ;; INTFLG

 ;;   ADVAN6, SS6, ADVAN8

 ;;   INTFLG stands for "Integration Flag" and affects the number of calls
 ;;   to  the  integrating  subroutine during each advance.  It is only of
 ;;   interest when second derivatives of the state vector are calculated.

 ;;   It is present because there may be some trade-off between  run  time
 ;;   and  accuracy  of computation.  More calls to the integrator, with a
 ;;   smaller number of derivatives obtained with  each  call,  result  in
 ;;   longer  run times, but might also produce more accurate derivatives.
 ;;   Also, it might provide more consistent computations when the  number
 ;;   of comparments and/or etas is to be changed.

 ;;   Default is -1.  User may set to any other value in user-written code
 ;;   (e.g., with verbatim code in $PK).

 ;;   PREDPP examines this value after the first call to PK for an indivi-
 ;;   dual  record,  so  that it can be set on an individual-by-individual
 ;;   basis.  PREDPP examines INTFLG when NEWIND=0  or  1  (i.e.,  at  the
 ;;   start  of  an individual's data).  Presumably, this is the only time
 ;;   that NONMEM might change the LVOUT array in ROCM15.

 ;;   When INTFLG is set to any other value than -1, ADVAN6 and SS6 calcu-
 ;;   late second derivatives "one group at a time".

 ;;   E.g., suppose etas 1, 2, 3 are active.

 ;;        1) call DVERK to obtain 2nd derivatives 1,1
 ;;        2) call DVERK to obtain 2nd derivatives 1,2 and 2,2
 ;;        3) call DVERK to obtain 2nd derivatives 1,3 and 2,3 and 3,3

 ;;        (Each calculation involves the integration of the state vector,
 ;;        augmented  by  the  relevant first derivative(s) and the second
 ;;        derivatives for one eta.)

 ;;        Thus, the maximum number of differential equations  that  would
 ;;        ever be integrated at one time is

 ;;            PW=2*PE*PM+PM
 ;;        Where:  (PE is the maximum number of etas; currently 10)
 ;;        (PM is the maximum number of compartments - 1; currently 9)
 ;;        PW is the size defined for various work arrays  in  the  source
 ;;        code.
 ;;        It is currently defined as above (189).

 ;;   When INTFLG is -1, ADVAN6/SS6 makes the most efficient  use  of  the
 ;;   work  arrays when computing second derivatives, to reduce the number
 ;;   of calls to DVERK.

 ;;   At NEWIND=0 or 1, PREDPP looks at the number of active etas for this
 ;;   individual  and  the  number of user-defined compartments defined by
 ;;   the MODEL subroutine (NCM), and creates a  scheme  to  calculate  as
 ;;   many  groups  of 2nd. derivatives at once as it can.  (Although com-
 ;;   partments may be turned on and off within the data set, PREDPP  does
 ;;   not revise the scheme of integration each time.)

 ;;        E.g., with the current values of PE, PM, and PW, here are  some
 ;;        schemes  of  integration.  Under "nth. call" are the etas whose
 ;;        second derivatives are computed with that call to DVERK.

 ;;        # compts.  1st. call     2nd. call    3d. call  4th. call
 ;;           9       etas 1 - 5    etas 6 - 7   eta 8     eta 9
 ;;           8       etas 1 - 5    etas 6 - 7   eta 8     eta 9
 ;;           7       etas 1 - 5    etas 6 - 7   eta 8 - 9
 ;;           6       etas 1 - 6    etas 7 - 8   eta 9
 ;;           5       etas 1 - 7    etas 8 - 9
 ;;           4       etas 1 - 8    etas 9
 ;;           3       etas 1 - 9

 ;;   Changing the size of the work arrays:

 ;;   If the system is large enought that integration involves  more  than
 ;;   one call to DVERK, and the user would like all second derivatives to
 ;;   be computed with a single call to DVERK, the  source  code  must  be
 ;;   changed to define a larger value for PW.

 ;;   To integrate the maximum number of  etas  and  compartments  in  one
 ;;   call, set:

 ;;     PW=PM*(1+PE+PE*(PE+1)/2)

 ;;   With the current values of PE and PM, PW=594

 ;;   Suppose PW is changed, but by accident  is  made  smaller  than  the
 ;;   default  (2*PE*PM+PM=189).   Then for problems with large numbers of
 ;;   compartments and/or etas, the work arrays will not be large  enough,
 ;;   with either INTFLG=-1 or INTFLG!=-1.

 ;;   A new error message exists in PREDPP, for which PRED exit code is  2
 ;;   (always abort).

 ;;   WORK ARRAYS ARE TOO SMALL FOR 2ND. DERIVS.  INCREASE PW, OR DECREASE
 ;;   NO. OF. COMPTS AND/OR ETAS, OR USE DERIV2=NO

 ;;   Again, this message cannot occur unless the source code of PREDPP is
 ;;   changed incorrectly.

 ;;   REFERENCES: None. 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NONMEM 7 Differential Equation Solver
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; +--------------------------------------------------------------------+
 ;; |                                                                    |
 ;; |                      DIFF EQ SOLVER SETTINGS                       |
 ;; |                                                                    |
 ;; +--------------------------------------------------------------------+

 ;; MEANING: PREDPP global variables
 ;; CONTEXT: For use with PREDPP

 ;; This feature is not fully documented.  The interested user may be able
 ;; to obtain more information by studying  the  appropriate  sections  of
 ;; PREDPP code.

 ;; USAGE:
 ;; $PK
 ;; "FIRST
 ;; " USE PRCOM_INT, ONLY: MITER,METH,IMAX,ISTFLG,INTFLG
 ;; "MAIN
 ;; " IMAX=200000

 ;; DISCUSSION:
 ;; These  variables allows the user to over-ride certain default settings
 ;; in ADVAN6, ADVAN8, ADVAN9, ADVAN13, and SS6.

 ;; MITER and METH

 ;;      PREDPP sets MITER and METH to default values with every new indi-
 ;;      vidual or reset record.

 ;;      ADVAN8
 ;;           Always  use METH=2.  If user set MITER, use his value.  Oth-
 ;;           erwise, use MITER=2.

 ;;      ADVAN9 and ADVAN13
 ;;           If user set METH, use MF=METH*10 in  call  to  LSODI  (LSODA
 ;;           with ADVAN13).  Otherwise, use MF=20

 ;;           If  user did not set MITER, then set MITER to 1 (if no popu-
 ;;           lation etas) or 2 (if population etas).

 ;;           If user did set MITER, use his value.

 ;; IMAX

 ;;      ADVAN6, ADVAN8, ADVAN9, ADVAN13, SS6
 ;;           The variable MAXCAL gives the maximum  number  of  calls  to
 ;;           FCN1  (ADVAN6,  ADVAN8, SS6) or RES (ADVAN9, ADVAN13) during
 ;;           an integration interval.  Each of the  above  routines  sets
 ;;           MAXCAL to the value given by MAXFCN (a parameter in the MOD-
 ;;           ULE SIZES) at the start of each integration interval  unless
 ;;           the  user supplies a value in IMAX, in which case the user's
 ;;           value is used.

 ;; ISTFLG

 ;;      ADVAN9 and ADVAN13
 ;;           ISTFLG controls how ADVAN9 (or  ADVAN13)  calls  LSODI1  (or
 ;;           LSODA)  and  what  it does if LSODI1 (or LSODA)  returns and
 ;;           indicates that an integration failed.  ISTFLG is  set  to  0
 ;;           (default)  at  ICALL=0.   If changed by the user, it retains
 ;;           the changed value until the user changes it  again.   ISTATE
 ;;           is  a  variable  that  is  passed  from the ADVAN to LSODI1.
 ;;           ISTATE=1  indicates  that  the  integration   is   starting.
 ;;           ISTATE=2  indicates  that this call is a continuation from a
 ;;           prior successful integration.  Default: Use ISTATE=1 for the
 ;;           first  integration,  and  ISTATE=2  for  a continuation when
 ;;           nothing external to the ADVAN has changed.  In case of fail-
 ;;           ure  with  ISTATE=2,  restore  original inputs and try again
 ;;           with ISTATE=1.

 ;;      ISTFLG=1
 ;;           Never try ISTATE=2, always use ISTATE=1.

 ;;      ISTFLG=2
 ;;           Never retry (only try ISTATE=2).

 ;; INTFLG

 ;;      ADVAN6, SS6, ADVAN8

 ;;      INTFLG stands for "Integration Flag" and affects  the  number  of
 ;;      calls  to  the integrating subroutine during each advance.  It is
 ;;      only of interest when second derivatives of the state vector  are
 ;;      calculated.

 ;;      It  is  present  because  there may be some trade-off between run
 ;;      time and accuracy of computation.  More calls to the  integrator,
 ;;      with  a  smaller  number  of derivatives obtained with each call,
 ;;      result in longer run times, but might also produce more  accurate
 ;;      derivatives.  Also, it might provide more consistent computations
 ;;      when the number of compartments and/or etas is to be changed.

 ;;      Default is -1.  User may set to any other value  in  user-written
 ;;      code (e.g., with verbatim code in $PK).

 ;;      PREDPP  examines  this  value  after  the first call to PK for an
 ;;      individual record, so that it can be  set  on  an  individual-by-
 ;;      individual  basis.   PREDPP  examines  INTFLG  when NEWIND=0 or 1
 ;;      (i.e., at the start of an individual's data).   Presumably,  this
 ;;      is  the  only  time  that  NONMEM  might  change  the LVOUT array
 ;;      (See non-active eta list for pred).

 ;;      When INTFLG is set to any other value than  -1,  ADVAN6  and  SS6
 ;;      calculate second derivatives "one group at a time".

 ;;      E.g., suppose etas 1, 2, 3 are active.

 ;;           1) call DVERK to obtain 2nd derivatives 1,1
 ;;           2) call DVERK to obtain 2nd derivatives 1,2 and 2,2
 ;;           3) call DVERK to obtain 2nd derivatives 1,3 and 2,3 and 3,3

 ;;           (Each calculation involves the integration of the state vec-
 ;;           tor, augmented by the relevant first derivative(s)  and  the
 ;;           second derivatives for one eta.)

 ;;           Thus,  the  maximum  number  of  differential equations that
 ;;           would ever be integrated at one time is

 ;;               PW=2*PE*PM+PM
 ;;           Where: (PE is the maximum number of etas; currently 10)
 ;;           (PM is the maximum number of compartments - 1; currently 9)
 ;;           PW is the size defined for various work arrays in the source
 ;;           code.
 ;;           It is currently defined as above (189).

 ;;      When INTFLG is -1, ADVAN6/SS6 makes the most efficient use of the
 ;;      work arrays when computing second derivatives, to reduce the num-
 ;;      ber of calls to DVERK.

 ;;      At  NEWIND=0  or 1, PREDPP looks at the number of active etas for
 ;;      this individual  and  the  number  of  user-defined  compartments
 ;;      defined  by  the  MODEL subroutine (NCM), and creates a scheme to
 ;;      calculate as many groups of 2nd. derivatives at once as  it  can.
 ;;      (Although  compartments  may be turned on and off within the data
 ;;      set, PREDPP does not revise the scheme of integration each time.)

 ;;           E.g.,  with  the  current values of PE, PM, and PW, here are
 ;;           some schemes of integration.  Under "nth. call" are the etas
 ;;           whose  second  derivatives  are  computed  with that call to
 ;;           DVERK.

 ;;           # compts.  1st. call     2nd. call    3d. call  4th. call
 ;;              9       etas 1 - 5    etas 6 - 7   eta 8     eta 9
 ;;              8       etas 1 - 5    etas 6 - 7   eta 8     eta 9
 ;;              7       etas 1 - 5    etas 6 - 7   eta 8 - 9
 ;;              6       etas 1 - 6    etas 7 - 8   eta 9
 ;;              5       etas 1 - 7    etas 8 - 9
 ;;              4       etas 1 - 8    etas 9
 ;;              3       etas 1 - 9

 ;;      Changing the size of the work arrays:

 ;;      If the system is large enough that integration involves more than
 ;;      one call to DVERK, and the user would like all second derivatives
 ;;      to be computed with a single call to DVERK, the source code  must
 ;;      be changed to define a larger value for PW.

 ;;      To  integrate  the maximum number of etas and compartments in one
 ;;      call, set:

 ;;        PW=PM*(1+PE+PE*(PE+1)/2)

 ;;      With the current values of PE and PM, PW=594

 ;;      Suppose PW is changed, but by accident is made smaller  than  the
 ;;      default  (2*PE*PM+PM=189).   Then for problems with large numbers
 ;;      of compartments and/or etas, the work arrays will  not  be  large
 ;;      enough, with either INTFLG=-1 or INTFLG!=-1.

 ;;      A new error message exists in PREDPP, for which PRED exit code is
 ;;      2 (always abort).

 ;;      WORK ARRAYS ARE TOO SMALL  FOR  2ND.  DERIVS.   INCREASE  PW,  OR
 ;;      DECREASE NO. OF. COMPTS AND/OR ETAS, OR USE DERIV2=NO

 ;;      Again, this message cannot occur unless the source code of PREDPP
 ;;      is changed incorrectly.

 ;; Location prior to NONMEM 7: prcomg

 ;; REFERENCES: None.

(defun esn-is-diff-eq ()
  "* Determines if control stream has differential equation solvers associated with it."
  ;; NM6
 ;; The common allows the user to over-ride certain  default  settings  in
 ;; ADVAN6, ADVAN8, ADVAN9, and SS6.
  ;; NM7
 ;; These  variables allows the user to over-ride certain default settings
 ;; in ADVAN6, ADVAN8, ADVAN9, ADVAN13, and SS6.
  )

(defun esn-fix-diffeq (&optional add-code)
  (let (
        (nm6 "\" COMMON /PRCOMG/ MITER,METH,IMAX,ISTFLG,INTFLG\n\" INTEGER MITER,METH,IMAX,ISTFLG,INTFLG")
        (nm6-reg1 "^[ \t]*\"[ \t]*COMMON[ \t]+/PRCOMG/[ \t]+MITER[ \t]*,[ \t]*METH[ \t]*,[ \t]*IMAX[ \t]*,[ \t]*ISTFLG[ \t]*,[ \t]*INTFLG[ \t]*$")
        (nm6-reg2 "^[ \t]*\"[ \t]*INTEGER[ \t]+MITER[ \t]*,[ \t]*METH[ \t]*,[ \t]*IMAX[ \t]*,[ \t]*ISTFLG[ \t]*,[ \t]*INTFLG[ \t]*$")
        (nm7 "\"\" USE PRCOM_INT, ONLY: MITER,METH,IMAX,ISTFLG,INTFLG")
        (nm7-reg1 "^[ \t]*\"\"[ \t]*USE[ \t]+PRCOM_INT[ \t]*,[ \t]*ONLY[ \t]*:[ \t]*MITER[ \t]*,[ \t]*METH[ \t]*,[ \t]*IMAX[ \t]*,[ \t]*ISTFLG[ \t]*,[ \t]*INTFLG[ \t]*$")
        (advan (esn-advan))
        (should-have-diffeq nil)
	(nm-ver (esn-update-get-version))
	)
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (setq nm-ver (string-to-number nm-ver))
    (setq should-have-diffeq (or (= advan 6)
                                 (= advan 8)
                                 (= advan 9)
                                 (and (>= nm-ver 7) (= advan 13))
                                 ))
    ;; need to determine ss6
    (unless should-have-diffeq
      ;; need to determine ss6
      (save-excursion
       (save-match-data
         (save-restriction
           (goto-char (point-min))
           (when (re-search-forward 
                  (eval-when-compile
                    (esn-reg-record-exp "SUB")
                    ) 
                  nil t)
             (esn-narrow-rec)
             (goto-char (point-min))
             (when (re-search-forward "\\<SS6\\>" nil t)
               (setq should-have-diffeq 't)
               )
             )
           )
         )
       )
      )
    (save-excursion
      (save-match-data
        (save-restriction
          (cond
           ( (not should-have-diffeq)
             ;; Remove differential equation settings code of ANY type
             
            )
           ( (>= nm-ver  7)
             (goto-char (point-min))
             (cond
              ( (re-search-forward nm6-reg1 nil t) ; NONMEM 6 code is present, though NONMEM 7 should be  
                (replace-match nm7 't 't)
                (when (re-search-forward nm6-reg2 nil t)
                  (replace-match "")
                  (when (looking-at "\n")
                    (replace-match "")
                    )
                  )
                ;; Now move " IMAX = ### and other such code to the bottom of block and in "MAIN
                )
              ( (re-search-forward nm7-reg1 nil t)
                ;; Nothing needs to be done;  NONMEM 7 code currently present.
                )
              ( 't
                ;; Need to add differential equation code?
                )
              )
             )
           ( 't
             )
           )
          )
        )
      )
    )
  )

(provide 'esn-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-code.el ends here
