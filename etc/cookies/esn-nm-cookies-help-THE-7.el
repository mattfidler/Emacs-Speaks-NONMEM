;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-THE-7 '(
		("-INF" "-INF\n-Infinity")
		("INF" "INF\n+Infinity")
		("FIXED" "FIXED\nFix an estimate value")
		("NOABORTFIRST" "NOABORTFIRST\n Same as NOABORT option, but also applies to the  first  value  of the  theta  vector  that  is  tried.  It cannot be shortened` all characters must be coded.  May be used with the  NOABORT  option, in  which case the stronger condition (NOABORTFIRST) takes precedence.")
		("NOABORT" "NOABORT\n During the Initial Estimates step, NONMEM  is  simply  to  ignore values  of the theta vector that result in PRED error return code 1.  (Ordinarily the first value of  the  theta  vector  is  never ignored.)  These will not be feasible values for an initial estimate.")
		("ABORT" "ABORT\n During the Initial Estimates step, NONMEM is to abort  when  PRED sets  the  error return code to 1.  (The PRED error return code n is set by the statement \"EXIT n [k]\" in abbreviated code,  or  by the  statement  IERPRD=n in user-supplied code, or by PREDPP when it detects an error.)  This is the default.")
		("NUMBERPOINTS=" "NUMBERPOINTS=n\n During  NONMEM's  search  for  an  initial  estimate, a number of points will be  examined.   This  number  will  be  automatically determined  by  NONMEM,  or it can be specified with this option. ")
		("NUMPTS=" "NUMPTS=n\n During  NONMEM's  search  for  an  initial  estimate, a number of points will be  examined.   This  number  will  be  automatically determined  by  NONMEM,  or it can be specified with this option. ")
		("NUMPOINTS=" "NUMPOINTS=n\n During  NONMEM's  search  for  an  initial  estimate, a number of points will be  examined.   This  number  will  be  automatically determined  by  NONMEM,  or it can be specified with this option. ")
		("NUMBERPTS=" "NUMBERPTS=n\n During  NONMEM's  search  for  an  initial  estimate, a number of points will be  examined.   This  number  will  be  automatically determined  by  NONMEM,  or it can be specified with this option. ")
)

"* $THETA help for NONMEM 7")

(provide 'esn-records-help-THE-7)

(provide 'esn-nm-cookies-help-THE-7)
