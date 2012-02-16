;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-MOD-6 '(
		("COMPARTMENT=" "COMPARTMENT=([name] [attribute1] [attribute2] ...)\n Each COMPARTMENT option defines a single  compartment.   Compartments  are  numbered in the order in which they are defined.  The name option gives the name for the compartment.  The name  option may  not be one of the compartment attributes below, unless it is enclosed in quotes (' or \"). If the name option is not used, then  the  compartment  is  named \"COMPn,\" where n is the compartment number. Attributes are chosen from the following list.  When an attribute is not chosen, its opposite is the default.   INITIALOFF - Compartment is initially off.   NOOFF - Compartment may not be turned on or off.   NODOSE - Compartment may not receive a dose.   EQUILIBRIUM - Compartment is an equilibrium compartment (ADVAN9   only` implies NODOSE).   EXCLUDE - Compartment amount is excluded from  the  computation   of  the  amount  (Amt) of the output compartment (ADVAN9 only).   [Amt=(total amount of drug thus far input  into  system)  minus   (total amount remaining in the system)] The following two attributes have no opposites.   DEFOBSERVATION - Compartment is the  default  observation  compartment.   DEFDOSE - Compartment is the default dose compartment.   If no compartment has the DEFOBSERVATION attribute, the default   is  the first compartment with the name CENTRAL` otherwise, the   first compartment.  If no compartment has  the  DEFDOSE  attribute,  the default is the first compartment with the name DEPOT   which may receive doses` otherwise, the first compartment which   may receive doses.")
		("NPARAMETERS=" "NPARAMETERS=n3\n Number of basic PK parameters.  Default: The number of  basic  PK parameters  defined  in  the  $PK  abbreviated code.    May be  0  with  the  general  non-linear  models (ADVAN6, ADVAN8, and ADVAN9).")
		("NPARAMS=" "NPARAMS=n3\n Number of basic PK parameters.  Default: The number of  basic  PK parameters  defined  in  the  $PK  abbreviated code.    May be  0  with  the  general  non-linear  models (ADVAN6, ADVAN8, and ADVAN9).")
		("NEQUILIBRIUM=" "NEQUILIBRIUM=n2\n Number of equilibrium compartments.  Default: 0")
		("NCOMPARTMENTS=" "NCOMPARTMENTS=n1\n Total number of compartments other than the  output  compartment. Default:  the  number  of COMPARTMENT options.  ")
		("NCOMPS=" "NCOMPS=n1\n Total number of compartments other than the  output  compartment. Default:  the  number  of COMPARTMENT options.  ")
		("NCM=" "NCM=n1\n Total number of compartments other than the  output  compartment. Default:  the  number  of COMPARTMENT options.  ")
)

"* $MODEL help for NONMEM 6")

(provide 'esn-records-help-MOD-6)

(provide 'esn-nm-cookies-help-MOD-6)
