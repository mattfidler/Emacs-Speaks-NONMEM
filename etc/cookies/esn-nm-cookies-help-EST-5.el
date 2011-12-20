;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-EST-5 '(
		("OMITTED" "OMITTED\n The Estimation Step is not implemented.")
		("REPEAT" "REPEAT\n The search is repeated with initial estimates that are the  final estimates of the revised scaled transformed parameters (STP) from the first search, rescaled to 0.1.")
		("NOREPEAT" "NOREPEAT\n The estimate obtained at the end of the  minimization  search  is taken to be the final parameter estimate. This is the default.")
		("MSFO=" "MSFO=filename\n A Model Specification File is output to a  file  with  the  given filename.  Filename may not contain embedded spaces.  If filename contains commas, semicolons, or parentheses, then it must be surrounded by quotes (' or \"). Filename may also contain  equal  signs  if  it  is  enclosed  in quotes.  If filename is the same as any option of the $ESTIMATION record, it must be enclosed in quotes. Default: No MSF is output.  iteration  estimates  may  also  be seen  in the original parameterization for those iterations whose summaries appear in intermediate printout.  These  estimates  may be found in file INTER, which will exist only during the duration of the run. See also model specification file.")
		("NOABORT" "NOABORT\n During  the  Estimation  Step,  NONMEM  should  implement  thetarecovery,  i.e., attempt to avoid values of theta which result in PRED error return code 1.")
		("ABORT" "ABORT\n During the Estimation Step, NONMEM should  not  implement  theta recovery  when  PRED  sets the error return code to 1.  (The PRED error return code n is set by  the  statement  \"EXIT  n  [k]\"  in abbreviated  code,  or by the statement IERPRD=n in user-supplied code, or by PREDPP when it detects errors.) This is the default.")
		("PRINT=" "PRINT=n3\n Iteration summaries are printed for the 0th,  every  n3th  iteration,  and  last iteration.  When n3=0, no summaries are printed. Default: 9999 (so that summaries are printed  for  0th  and  last iterations).")
		("MAXEVALS=" "MAXEVALS=n2\n Maximum allowable number of evaluations of the objective function during  the  Estimation Step.  Default: a generous number.  (Each evaluation of the objective function requires  one  pass  through the  data  set.)  This is also referred to as a \"function evaluation.\") MAXEVAL=0 omits the Estimation Step` it  is  useful  with POSTHOC.   MAXEVALS=-1  may  be  specified when a $MSFI record is present.  It requests that NONMEM re-use the value from the  previous run, and is the default with $MSFI.")
		("SIGDIGITS=" "SIGDIGITS=n1\n Number of significant digits  required  in  the  final  parameter estimate.  Default: 3.  ")
		("NSIGDIGITS=" "NSIGDIGITS=n1\n Number of significant digits  required  in  the  final  parameter estimate.  Default: 3.  ")
		("NOPOSTHOC" "NOPOSTHOC\n Etas are not estimated.  This is the default with METHOD=0.   May not be used with METHOD=1.")
		("POSTHOC" "POSTHOC\n This option may be used when the FO method is  used.   After  the Estimation Step terminates, the eta values are estimated for each individual.  To estimate the etas based on the initial  estimates of THETA, OMEGA, and SIGMA (found either in the control stream or in a model specification file),  also  specify  MAXEVAL=0  (which omits the Estimation Step).")
		("ZERO=" "ZERO=list\n Required with METHOD=HYBRID.  A list of indices  for  etas  which are  fixed  to  zero during the Estimation Step.  \"list\" contains one or more integers.  If more than one, they must be  surrounded by parentheses.  The list must be contained entirely on one line. The indices may be separated by commas or spaces.")
		("NONUMERICAL" "NONUMERICAL\n Requests that second eta-derivatives for the Laplacian method  be computed by PRED.")
		("NUMERICAL" "NUMERICAL\n Requests that second eta-derivatives for the Laplacian method  be obtained numerically.")
		("NOCENTERING" "NOCENTERING\n Requests that the average conditional estimates of each  eta  not be constrained.  This is the default.")
		("CENTERING" "CENTERING\n Requests that the average conditional estimates of  each  eta  be constrained  to  be  close to 0.  May only be used with METHOD=1. Not permitted with INTERACTION.")
		("NOFO" "NOFO\n Requests that the First-Order Model not be used with METHOD=1 and CENTERING.  This is the default.")
		("FO" "FO\n Requests that the First-Order Model be  used  with  METHOD=1  and CENTERING.  Can not be used with LAPLACIAN.")
		("-2LOGLIKELIHOOD" "-2LOGLIKELIHOOD\n Indicates that Y (with  abbreviated code)  or  F  (with  a user-supplied  PRED  or  ERROR  code)  is  a -2 log (conditional) likelihood.  All remarks for LIKELIHOOD apply.  ")
		("-2LLIKELIHOOD" "-2LLIKELIHOOD\n Indicates that Y (with  abbreviated code)  or  F  (with  a user-supplied  PRED  or  ERROR  code)  is  a -2 log (conditional) likelihood.  All remarks for LIKELIHOOD apply.  ")
		("LIKELIHOOD" "LIKELIHOOD\n This is designed mainly, but not exclusively, for use  with  noncontinuous  observed responses (\"odd-type data\").  Indicates that Y (with  abbreviated code) or F (with a user-supplied PRED or  ERROR  code) will be set to a (conditional) likelihood.  Upon simulation it will be ignored, and the DV data item will  be  set directly  to  the  simulated  value  in abbreviated or user code. Also etas, if any, are understood to be population etas.  Epsilon variables  and  the  $SIGMA  record may not be used.  The L2 data item may not be used. The CONTR and CCONTR options of  the  $SUBROUTINES  record  may not be used.  NONMEM cannot obtain the initial estimate for omega.  If the data are  population  and  MAXEVALS=0 is not coded, then METHOD=1 LAPLACE is required.")
		("PREDICTION" "PREDICTION\n Indicates that Y (with  abbreviated code)  or  F  (with  a user-supplied  PRED  or  ERROR  code)  will serve as a prediction variable, i.e., it will be set to a prediction.  Upon  simulation it  will  be  set  to a simulated value.  Also, etas (if any) are population etas only  if  epsilons  also  appear.   This  is  the default.")
		("NOLAPLACIAN" "NOLAPLACIAN\n Do not use the Laplacian method.  This is the default.")
		("LAPLACIAN" "LAPLACIAN\n Use the  Laplacian  method,  in  which  second  derivatives  with respect  to  eta  are used.  May only be specified with METHOD=1. Can not be used with INTERACTION or $ABBREVIATED DERIV2=NO.")
		("NOINTERACTION" "NOINTERACTION\n Always set etas to 0 during the  computation  of  the  model  for intraindividual random error.  This is the default.")
		("INTERACTION" "INTERACTION\n The dependence on etas of the model for  intra-individual  random error  is preserved in the computation of the objective function. May only be used with METHOD=1.  Cannot be used with LAPLACIAN or CENTERING.")
		("METHOD=" (
			("0" "METHOD=[0,ZERO]\n Always set etas to 0 during the computation of the objective function.  Also  called  the \"first order (FO) method.\" (The default.)")
			("ZERO" "METHOD=[0,ZERO]\n Always set etas to 0 during the computation of the objective function.  Also  called  the \"first order (FO) method.\" (The default.)")
			("1" "METHOD=[1,CONDITIONAL]\n Use conditional estimates for the etas during  the  computation  of  the  objective  function.   METHOD=1  (without the LAPLACIAN option) is also called  the  \"first  order  conditional estimation (FOCE) method.\"")
			("CONDITIONAL" "METHOD=[1,CONDITIONAL]\n Use conditional estimates for the etas during  the  computation  of  the  objective  function.   METHOD=1  (without the LAPLACIAN option) is also called  the  \"first  order  conditional estimation (FOCE) method.\"")
			("HYBRID" "METHOD=HYBRID\n Use conditional estimates for the etas during  the  computation  of the objective function, with the exception of those etas listed  in  the  ZERO  option.   Cannot  be  used  with INTERACTION, LAPLACIAN, or CENTERING.")
))
)

"* $ESTIMATION help for NONMEM 5")

(provide 'esn-records-help-EST-5)

(provide 'esn-nm-cookies-help-EST-5)
