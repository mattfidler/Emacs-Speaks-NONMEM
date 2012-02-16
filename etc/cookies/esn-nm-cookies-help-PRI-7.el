;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-PRI-7 '(
		("DISPLAY=" "DISPLAY[=ALL|CNT]\n     Optional.  The PRIOR subroutine will contain code to print items  of    interest in the NONMEM report.  This is to assist the user in checking that the $PRIOR record is working correctly.    DISPLAY=ALL is the default when only DISPLAY is present. Lines  such    as the following are printed with every call to PRIOR:    PRIOR ICALL,IPROB,IREP,CNT:  2  2  0   -41.898951681785.    With  DISPLAY=CNT, lines such as the following are printed only when    PRIOR is called for simulation or estimation.    PRIOR CNT:    -41.898951681785  ICMAX=n    Optional.  The PRIOR subroutine will set the given  value  in  ICMAX    prior to calling the subroutine.  (See PRIOR Simulation: ICMAX).  Subroutine arguments    Optional.  The arguments are described in the help entries for NWPRI    and TNPRI.  They must be coded excatly as shown, i.e., no  abbreviations.  Any argument that is omitted defaults to 0.  ITYP,NSAM,ISS,PLEV,CNT      Arguments for both NWPRI and TNPRI  NTHETA,NETA,NEPS,NTHP,NETP,NEPP,NPEXP      Arguments unique to NWPRI  IFND,MODE,IVAR      Arguments unique to for TNPRI")
		("PROBLEM=" "PROBLEM=n\n Specifies  the  problem  for which the subroutine is to be called. May also be specified as PROBLEM=n or  PROBLEM.EQ.n  or  PROBLEMn. PROBLEM  may  also  be  coded as IPROB.  Instead of=, .EQ. may be used. Other permitted tests are .NE., .LT., .LE., .GT., and .GE.")
		("SIMULATION" "SIMULATION\n Specifies the NONMEM task  for  which  the  subroutine  is  to  be called.   If omitted, PRIOR is called for all tasks (i.e., for all values of ICALL).  ESTIMATION and SIMULATION may not both be specified  in  the same conditional clause.  ESTIMATION may be spelled ESTIMATE,ESTM` SIMULATION may be spelled SIMULATE,SIML. May also be specified as ICALL=n, ICALL.EQ.n, or ICALLn,  where  n is 2 (ESTIMATION) or 4 (SIMULATION).")
		("ESTIMATION" "ESTIMATION\n Specifies the NONMEM task  for  which  the  subroutine  is  to  be called.   If omitted, PRIOR is called for all tasks (i.e., for all values of ICALL).  ESTIMATION and SIMULATION may not both be specified  in  the same conditional clause.  ESTIMATION may be spelled ESTIMATE,ESTM` SIMULATION may be spelled SIMULATE,SIML. May also be specified as ICALL=n, ICALL.EQ.n, or ICALLn,  where  n is 2 (ESTIMATION) or 4 (SIMULATION).")
		("NWPRI" "NWPRI\n Required.  Either  TNPRI  or NWPRI.  The following options and arguments apply to calls to this subroutine.  Another subroutine  option (or another $PRIOR record) may follow, with a new set of options and arguments.")
		("TNPRI" "TNPRI\n Required.  Either  TNPRI  or NWPRI.  The following options and arguments apply to calls to this subroutine.  Another subroutine  option (or another $PRIOR record) may follow, with a new set of options and arguments.")
)

"* $PRIOR help for NONMEM 7")

(provide 'esn-records-help-PRI-7)

(provide 'esn-nm-cookies-help-PRI-7)
