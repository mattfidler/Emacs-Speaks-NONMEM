;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-MSF-6 '(
		("ONLYREAD" "ONLYREAD\n This option should be used if and only if the model specification file  is  being  used to convey prior information to a succeeding problem in the control stream (See TNPRI).  No task  records  may appear  in a problem specification containing a $MSFI record with this option.")
		("NPOPETAS=" "NPOPETAS[=n]\n If the data are to be  understood  to  be  population  data,  the option  NPOPETAS  may be needed, and then the part \"=n\", n>0, may also need to be coded.  The integer n should  be  the  number  of population  eta  variables.   The  part \"=n\" is required when the $ERROR block describes a model that changes the value of  F,  and there is no $PK block. It is also required when variables  A(n)  are  used  in  the  $PK abbreviated code. If the data are to be understood to be single-subject  data,  the option NPOPETAS=0 may be needed. ")
		("POPETAS=" "POPETAS[=n]\n If the data are to be  understood  to  be  population  data,  the option  NPOPETAS  may be needed, and then the part \"=n\", n>0, may also need to be coded.  The integer n should  be  the  number  of population  eta  variables.   The  part \"=n\" is required when the $ERROR block describes a model that changes the value of  F,  and there is no $PK block. It is also required when variables  A(n)  are  used  in  the  $PK abbreviated code. If the data are to be understood to be single-subject  data,  the option NPOPETAS=0 may be needed. ")
		("RESCALE" "RESCALE\n The search is to be restarted with initial estimates taken to  be the final estimates from the previous run, so that a UCP value of 0.1 now corresponds to a final estimate from  the  previous  run. This  option may be used only if the search with the previous run terminated successfully and all the options used on the  $ESTIMATION  record  coincide  with  those  used  with  the previous run (except the MAXEVAL option).")
		("NORESCALE" "NORESCALE\n If a search is continued, it is continued just as it  would  have been  in the previous run. This option has no effect if the Estimation Step is not implemented or if the step is implemented, but using different options on the $ESTIMATION record from those used with the previous run.  This is the default option.")
)

"* $MSFI help for NONMEM 6")

(provide 'esn-records-help-MSF-6)

(provide 'esn-nm-cookies-help-MSF-6)
