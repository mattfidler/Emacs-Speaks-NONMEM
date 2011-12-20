;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-SCA-7 '(
		("OMITTED" "OMITTED\n The Scatterplot Step is not implemented.")
		("UNCONDITIONAL" "UNCONDITIONAL\n The Scatterplot Step is always implemented.  This is the default.")
		("CONDITIONAL" "CONDITIONAL\n The Scatterplot Step is implemented only when the Estimation Step terminates successfully or  is  not  implemented.   This  is  the default.")
		("TO=" "TO=n2\n n2  is  the number of the last data record which may \"contribute\" to the scatterplot.  Default:  There  is  no  upper  limit.   All appropriate  records  will  contribute.  To restore the NONMEM VI behavior, use TO=n1+899.")
		("FROM=" "FROM=n1\n n1 is the number of the first data record which may  \"contribute\" to the scatterplot.  Default: n1 is 1.")
		("OBSONLY" "OBSONLY\n The scatterplot will only use  data  records  with  MDV=0.   This option  applies  independently of FIRSTONLY.  It is not necessary when either DV, RES, or WRES is plotted.")
		("FIRSTONLY" "FIRSTONLY\n Only the first data record from each individual record  may  contribute   a   point  to  the  scatterplot.   May  also  be  coded FIRSTRECORDONLY or FIRSTRECONLY.")
		("NOABS0" "NOABS0\n Prevents a zero line from being superimposed on the abscissa axis of  the  scatterplots.   May  also  be  coded  NOABSZERO, NOABS0, NOABZERO, NOAB0. If neither of ABS0  and NOABS0 is  present,  NONMEM  automatically superimposes  a zero line on the abscissa axis if it is appropriate for the type of data item.")
		("ABS0" "ABS0\n A  zero line is superimposed on the abscissa axis of the scatterplots.  ")
		("ABZERO" "ABZERO\n A  zero line is superimposed on the abscissa axis of the scatterplots.  ")
		("AB0" "AB0\n A  zero line is superimposed on the abscissa axis of the scatterplots.  ")
		("ABSZERO" "ABSZERO\n A  zero line is superimposed on the abscissa axis of the scatterplots.  ")
		("NOORD0" "NOORD0\n Prevents a zero line from being superimposed on the ordinate axis of the scatterplots.   If  neither  ORD0   nor  NOORD0  is  present, NONMEM automatically superimposes a zero line on the ordinate axis if it is  appropriate for the type of data item.")
		("NOORDZERO" "NOORDZERO\n Prevents a zero line from being superimposed on the ordinate axis of the scatterplots.   If  neither  ORD0   nor  NOORD0  is  present, NONMEM automatically superimposes a zero line on the ordinate axis if it is  appropriate for the type of data item.")
		("ORD0" "ORD0\n A  line  through  the zero value on the ordinate axis is superimposed on the scatterplots.  ")
		("ORDZERO" "ORDZERO\n A  line  through  the zero value on the ordinate axis is superimposed on the scatterplots.  ")
		("UNIT" "UNIT\n A line of unit slope is superimposed on the scatterplots.")
)

"* $SCATTERPLOT help for NONMEM 7")

(provide 'esn-records-help-SCA-7)

(provide 'esn-nm-cookies-help-SCA-7)
