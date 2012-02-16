;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-NON-7 '(
		("OMITTED" "OMITTED\n The Nonparametric Step is not implemented.")
		("UNCONDITIONAL" "UNCONDITIONAL\n The Nonparametric Step is always implemented.")
		("CONDITIONAL" "CONDITIONAL\n The Nonparametric Step is implemented only  when  the  Estimation Step  terminates  successfully  or  is not implemented (i.e., the $ESTIMATION record specifies MAXEVAL=0).  This is the default.")
		("RECOMPUTE" "RECOMPUTE\n Requests that the nonparametric density estimate occurring in  an input MSF should be ignored` the nonparametric estimate should be recomputed.")
		("MSFO=" "MSFO=filename\n A Model Specification File  is  output  to  the  given  filename. Filename  may  not contain embedded spaces.  If filename contains commas, semicolons, equal  signs,  or  parentheses,  it  must  be enclosed  in  quotes  ('  or \").  Filename may contain at most 71 characters.  If filename is the same as any option of  the  $NONPARAMETRIC  record,  it  must  be  enclosed  in quotes.  The MSFO option may appear without a file name.  More  precisely,  if  the $ESTIMATION  record  is  also  present, and it also specifies the MSFO option, then the filename is required only on one of the two records,  $ESTIMATION  or  $NONPARAMETRIC,  whichever one appears first in the control stream.  If the filename appears present  on both  records, it must be the same on both records.  If the filename is omitted on the second of the two records, the MSF  option must be the final option on that record.  Default: No MSF is output.")
		("ETAS" "ETAS\n Requests that conditional (nonparametric) estimates of eta values be obtained. (Also called the CNPE).")
		("MARGINALS" "MARGINALS\n Requests that marginal cumulatives  be  obtained  (the  default). These values are found in NONMEM global variables (See Nonparametric Density: DEN_,CDEN_)")
)

"* $NONPARAMETRIC help for NONMEM 7")

(provide 'esn-records-help-NON-7)

(provide 'esn-nm-cookies-help-NON-7)
