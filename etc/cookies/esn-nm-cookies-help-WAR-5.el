;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-WAR-5 '(
		("NORESET" "NORESET\n Counts of how many times a given message has been  displayed  are accumulated  over problems.  (A new $WARNING record always resets all counters, but they will not be automatically reset again with subsequent problems.)")
		("RESET" "RESET\n All counts reset to 0 with each new problem. This is the default.")
		("ERRORMAXIMUM=" "ERRORMAXIMUM=n\n Refers to data error messages.    Each type of data error message will be displayed no more than n times (n>=0).  The default  is  20.   Data  error  messages  can  never  be  totally suppressed.  With n=0 and n=1,  displays  the  first  data error message and then terminates. Example:  $WARNING EMAX=99999 Sets the maximum number of data error messages displayed from  20 to  a much higher value, so that  will not terminate until all data error conditions have been identified.")
		("EMAX=" "EMAX=n\n Refers to data error messages.    Each type of data error message will be displayed no more than n times (n>=0).  The default  is  20.   Data  error  messages  can  never  be  totally suppressed.  With n=0 and n=1,  displays  the  first  data error message and then terminates. Example:  $WARNING EMAX=99999 Sets the maximum number of data error messages displayed from  20 to  a much higher value, so that  will not terminate until all data error conditions have been identified.")
		("EMAXIMUM=" "EMAXIMUM=n\n Refers to data error messages.    Each type of data error message will be displayed no more than n times (n>=0).  The default  is  20.   Data  error  messages  can  never  be  totally suppressed.  With n=0 and n=1,  displays  the  first  data error message and then terminates. Example:  $WARNING EMAX=99999 Sets the maximum number of data error messages displayed from  20 to  a much higher value, so that  will not terminate until all data error conditions have been identified.")
		("ERRMAXIMUM=" "ERRMAXIMUM=n\n Refers to data error messages.    Each type of data error message will be displayed no more than n times (n>=0).  The default  is  20.   Data  error  messages  can  never  be  totally suppressed.  With n=0 and n=1,  displays  the  first  data error message and then terminates. Example:  $WARNING EMAX=99999 Sets the maximum number of data error messages displayed from  20 to  a much higher value, so that  will not terminate until all data error conditions have been identified.")
		("DATAMAXIMUM" "DATAMAXIMUM\n Refers to data warning messages.    Forms  and default are identical to those for the WARNINGMAXIMUM option.")
		("DATA" "DATA\n Refers to data warning messages.    Forms  and default are identical to those for the WARNINGMAXIMUM option.")
		("DMAX" "DMAX\n Refers to data warning messages.    Forms  and default are identical to those for the WARNINGMAXIMUM option.")
		("DMAXIMUM" "DMAXIMUM\n Refers to data warning messages.    Forms  and default are identical to those for the WARNINGMAXIMUM option.")
		("WARNINGMAXIMUM" "WARNINGMAXIMUM\n Refers to general (non-data) warning messages.   WARNINGMAXIMUM=NONE      Suppress warning messages. WARNINGMAXIMUM=n      Each type of warning message will be displayed no more  than      n times  (n >=0).  The default is 20. Example: $WARNING     WMAX=1 Sets the maximum for all non-data messages to 1.")
		("WARN" "WARN\n Refers to general (non-data) warning messages.   WARNINGMAXIMUM=NONE      Suppress warning messages. WARNINGMAXIMUM=n      Each type of warning message will be displayed no more  than      n times  (n >=0).  The default is 20. Example: $WARNING     WMAX=1 Sets the maximum for all non-data messages to 1.")
		("WMAX" "WMAX\n Refers to general (non-data) warning messages.   WARNINGMAXIMUM=NONE      Suppress warning messages. WARNINGMAXIMUM=n      Each type of warning message will be displayed no more  than      n times  (n >=0).  The default is 20. Example: $WARNING     WMAX=1 Sets the maximum for all non-data messages to 1.")
		("WARNMAXIMUM" "WARNMAXIMUM\n Refers to general (non-data) warning messages.   WARNINGMAXIMUM=NONE      Suppress warning messages. WARNINGMAXIMUM=n      Each type of warning message will be displayed no more  than      n times  (n >=0).  The default is 20. Example: $WARNING     WMAX=1 Sets the maximum for all non-data messages to 1.")
		("NONE" "NONE\n Suppress all warning messages.")
)

"* $WARNINGS help for NONMEM 5")

(provide 'esn-records-help-WAR-5)

(provide 'esn-nm-cookies-help-WAR-5)
