;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-COV-5 '(
		("OMITTED" "OMITTED\n The Covariance Step is not implemented.")
		("CONDITIONAL" "CONDITIONAL\n The Covariance Step is implemented only when the Estimation  Step terminates  successfully  (in  this run or in a run continued via $MSFI).  This is the default.")
		("COMPRESS" "COMPRESS\n Covariance Step arrays are printed in compressed format, even  if their  size  is such that NONMEM would normally print them in the usual format.")
		("PRINT=" "PRINT=[E][R][S]\n Additional outputs should be printed  besides  the  defaults.   E requests the eigenvalues of the covariance matrix` R requests the R matrix` S requests the S matrix.  PRINT=R (or S) is not  needed with MATRIX=R (or S).")
		("MATRIX=" "MATRIX=c\n Specifies that the covariance matrix should be different from the default  (R  sup  -1  S  R  sup  -1).  MATRIX=R requests that the inverse R matrix be used.  MATRIX=S requests that the  inverse  S matrix  be  used.   (R  and  S are two matrices from  statistical theory,  the Hessian and Cross-Product Gradient matrices, respectively.) MATRIX=R should not be used with option SPECIAL.")
		("SPECIAL" "SPECIAL\n The special computation should be used in  the  Covariance  Step. Use  with single-subject data and a recursive PRED subroutine.  A recursive PRED subroutine is such that the PRED computation  with a  data  record  depends on the PRED computation(s) with previous data records.  This is the default when PREDPP is  used  and  the data are single-subject data.")
)

"* $COVARIANCE help for NONMEM 5")

(provide 'esn-records-help-COV-5)

(provide 'esn-nm-cookies-help-COV-5)
