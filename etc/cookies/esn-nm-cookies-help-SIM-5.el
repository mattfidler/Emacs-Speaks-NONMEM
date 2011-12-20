;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-SIM-5 '(
		("OMITTED" "OMITTED\n The Simulation Step is not implemented.")
		("NOPREDICTION" "NOPREDICTION\n Permitted with ONLYSIM, but is not required. Indicates  that  the DV  data  item  will  be  set  directly to the simulated value in abbreviated or user-supplied code (as  when  simulating  odd-type data).   Also,  etas (if any) are always population etas, even if epsilons do not appear.")
		("PREDICTION" "PREDICTION\n Permitted with ONLYSIM, but is not  required.  Indicates  that  Y (with    abbreviated code) or F (with a user-supplied PRED or ERROR routine) will be set to a simulated value.   Also,  etas (if  any) are population etas only if epsilons also appear.  This is the default.")
		("REQUESTSECOND" "REQUESTSECOND\n NONMEM normally sets flags informing PRED that it need  not  compute second eta derivatives during the Simulation Step. With this option, the second  derivative  flag  is  set  to  \"do  compute\". REQUESTSECOND implies REQUESTFIRST.")
		("REQUESTFIRST" "REQUESTFIRST\n NONMEM normally sets flags informing PRED that it need  not  compute  first eta derivatives during the Simulation Step. With this option, the first derivative flag is set to \"do compute\".")
		("SUBPROBLEMS=" "SUBPROBLEMS=n\n Requests that the entire NONMEM problem is to be repeated n times in  succession  (including  all NONMEM steps: simulation, estimation, covariance, table, scatterplot). Each  subproblem  includes the  Simulation Step, but the random sources are simply continued from subproblem to subproblem.  If n=0 or n=1, there is only  one subproblem`  this  is  the  default.  ")
		("NSUBPROBS=" "NSUBPROBS=n\n Requests that the entire NONMEM problem is to be repeated n times in  succession  (including  all NONMEM steps: simulation, estimation, covariance, table, scatterplot). Each  subproblem  includes the  Simulation Step, but the random sources are simply continued from subproblem to subproblem.  If n=0 or n=1, there is only  one subproblem`  this  is  the  default.  ")
		("NSUBPROBLEMS=" "NSUBPROBLEMS=n\n Requests that the entire NONMEM problem is to be repeated n times in  succession  (including  all NONMEM steps: simulation, estimation, covariance, table, scatterplot). Each  subproblem  includes the  Simulation Step, but the random sources are simply continued from subproblem to subproblem.  If n=0 or n=1, there is only  one subproblem`  this  is  the  default.  ")
		("SUBPROBS=" "SUBPROBS=n\n Requests that the entire NONMEM problem is to be repeated n times in  succession  (including  all NONMEM steps: simulation, estimation, covariance, table, scatterplot). Each  subproblem  includes the  Simulation Step, but the random sources are simply continued from subproblem to subproblem.  If n=0 or n=1, there is only  one subproblem`  this  is  the  default.  ")
		("ONLYSIMULATION" "ONLYSIMULATION\n NONMEM performs only the simulation step and  does  not  evaluate the  objective  function. This has two effects.  (1) PRED-defined data items in tables and scatterplots are  computed  using  simulated  etas  and  initial  thetas.  (2) WRES values in tables and scatterplots are 0. $ESTIM and $COV cannot be used with ONLYSIMULATION. See the NOPREDICTION option.")
		("NEW" "NEW\n If the NEW option  is  used,  the  vector  of  eta's  (epsilon's) changes  with  each  call to SIMETA (SIMEPS), rather than only at the start of the next individual record (next level-two record).")
		("UNIFORM" "UNIFORM\n The random numbers of the source are to be pseudo-uniform on  the interval [0,1].")
		("NORMAL" "NORMAL\n The random numbers of the source are  to  be  pseudo-normal  with mean 0 and variance 1 (unless the source is the first and used to generate  eta  and  epsilon  realizations,  in  which  case   the variance-covariance  of  these variables is that specified in the $OMEGA and $SIGMA records).  This is the default.")
)

"* $SIMULATION help for NONMEM 5")

(provide 'esn-records-help-SIM-5)

(provide 'esn-nm-cookies-help-SIM-5)
