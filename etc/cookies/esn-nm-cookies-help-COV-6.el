;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-COV-6 '(
		("CONDITIONAL" "CONDITIONAL\n The Covariance Step is implemented, but only when the  Estimation Step  terminates  successfully (in this run or in a run continued via $MSFI).  This is the default.")
		("OMITTED" "OMITTED\n The Covariance Step is not implemented.")
		("NOSLOW" "NOSLOW\n Requests a faster method of computation.   This  is  the  default (but see SLOW).")
		("SLOW" "SLOW\n Requests a slower method of computation.  Required when either  a mixture  model  was  used along with CENTERING on the $ESTIMATION record, or NUMERICAL was used on the $ESTIMATION record.  If  not present,  the  option will be automatically supplied in these two cases.")
		("COMPRESS" "COMPRESS\n Covariance Step arrays are printed in compressed format, even  if their  size  is such that NONMEM would normally print them in the usual format.")
		("PRINT=" "PRINT=[E][R][S]\n Additional outputs will be  printed  besides  the  defaults.   E: print  the  eigenvalues  of the correlation matrix.  R: print the matrix .5*R.  S: print the matrix .25*S.  PRINT=R (or S)  is  not needed with MATRIX=R (or S).")
		("MATRIX=" "MATRIX=c\n Specifies that the covariance matrix will be different  from  the default  (R  sup  -1 S R sup -1).  MATRIX=R requests that 2 times the inverse R matrix be used.  MATRIX=S requests that 4 times the inverse  S  matrix be used.  (R and S are two matrices from  statistical  theory,   the  Hessian   and   Cross-Product   Gradient matrices, respectively.)  MATRIX=R should not be used with option SPECIAL.")
		("SPECIAL" "SPECIAL\n The special computation will be used in the Covariance Step  with a recursive PRED subroutine.  A recursive PRED subroutine is such that, with single-subject data, the PRED computation with a  data record depends on information passed to it with any of the previ--ous data records.  This is the default when PREDPP is used.")
)

"* $COVARIANCE help for NONMEM 6")

(provide 'esn-records-help-COV-6)

(provide 'esn-nm-cookies-help-COV-6)
