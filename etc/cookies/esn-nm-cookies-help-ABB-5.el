;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-ABB-5 '(
		("DES=" (
			("FULL" "DES=FULL\n Arrays of the DES routine are stored in non-compact form. With $ESTIMATION METHOD=COND LAPLACIAN, the option  NUMERICAL  is also required. DES=FULL is the default with ADVAN9.")
			("COMPACT" "DES=COMPACT\n Arrays of the DES routine are stored in compact form. Required with Laplacian method` optional otherwise.  This is  the default, except with ADVAN9.")
))
		("DERIV2=" (
			("NO" "DERIV2=NO\n Prevents the computation of second derivatives, which are  needed only for the Laplacian method. With $ESTIMATION METHOD=COND LAPLACIAN, this option  may  not  be specified.")
			("NOCOMMON" "DERIV2=NOCOMMON\n Permits the computation of these derivatives, but  prevents  them from being stored in the global common NMPRD4. $ESTIMATION METHOD=COND LAPLACIAN may be specified, but variables representing  second derivatives are not stored in common NMPRD4. Therefore, they cannot be displayed in tables  and  scatterplots. In addition, no variables computed in the $PK block may be referenced in the $ERROR block.  This is true  whether  or  not  these variables  happen  to have second derivatives, and whether or not the Laplacian method is used.")
))
		("COMSAV=" "COMSAV=n2 ('common save')\n This option has the effect of instructing NONMEM (see below) that the  first  n2  positions of common NMPRD4 are to receive special treatment when COMACT=1 and COMACT=2.  (COMACT is a  variable  in common  NMPRD3  which  is  set by NONMEM to values > 0 when it is copying values from NMPRD4  for  tables  and  scatterplots.)  The first  n2  positions are referred to as the save region of common NMPRD4. They are initialized by NONMEM before each call  to  PRED according to: At COMACT=1, initialized to 0. At COMACT=2, re-initialized to what they were set to by  PRED  at COMACT=1. (The remaining positions remain untouched  by  NONMEM  from  data record  to  data  record  and  are  under the full control of the user.) n2 may not exceed n1. When PREDPP is used,  inserts code in the  PK  routine  to store  the value of COMSAV in NMPRD3 at ICALL=1.  If no $PK block is present, the code is  inserted  in  the  ERROR  routine.  When PREDPP  is not used, the generated or library PRED routine stores the value of COMSAV in NMPRD3 at ICALL<=1.")
		("COMRES=" "COMRES=n1  ('common reserve')\n COMRES gives instructions to . Values of n1:  -1  Do not store any variables in the global common NMPRD4   0  Store variables in NMPRD4 with no  reserved  positions  (the default) n1>0  Store variables in NMPRD4, but reserve the first  n1  positions With abbreviated code, the Ith position in NMPRD4  is  referenced by COM(I). This option is intended for advanced users of NONMEM, e.g.,  when abbreviated  code  is  combined with user-supplied subroutines or verbatim code. If the user-supplied code makes use of  (say)  the first  40  positions  in  the  COM array, the option COMRES=40 is needed to instruct  to skip  these  positions`  the  first position used by  is COM(41). $TABLE and $SCATTER may explicitly list reserved positions COM(1) through COM(n1), in addition to listing by name variables defined in abbreviated code which are stored by  in  higher  positions. Individual blocks of abbreviated  code  may  always  include  the pseudo-statement  COMRES=-1, which excludes all variables defined in that particular block from the common.")
)

"* $ABBREVIATED help for NONMEM 5")

(provide 'esn-records-help-ABB-5)

(provide 'esn-nm-cookies-help-ABB-5)
