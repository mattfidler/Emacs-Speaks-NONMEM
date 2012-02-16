;; -*-no-byte-compile: t; -*-

(defvar esn-exact-records-reg-6 "\\<\\(\\$\\(?:A\\(?:BBREVIATED\\|ES\\(?:0\\|INITIAL\\)?\\)\\|BIND\\|CO\\(?:NTR\\|V\\(?:ARIANCE\\|R\\)\\)\\|D\\(?:ATA\\|ES\\)\\|E\\(?:RROR\\|ST\\(?:IMAT\\(?:E\\|ION\\)\\|M\\)\\)\\|IN\\(?:CLUDE\\|D\\(?:EX\\(?:ES\\)?\\|XS\\)\\|FN\\|PUT\\)\\|M\\(?:IX\\|ODEL\\|SFI\\)\\|NONPARAMETRIC\\|OM\\(?:EGA\\|IT\\)\\|P\\(?:K\\|R\\(?:ED\\|IOR\\|OBLEM\\)\\)\\|S\\(?:CATTER\\(?:GRAMS\\|PLOT\\|S\\)\\|I\\(?:GMA\\|M\\(?:L\\|ULAT\\(?:E\\|ION\\)\\)\\)\\|U\\(?:B\\(?:\\(?:ROUTINE\\)?S\\)\\|PER\\)\\)\\|T\\(?:ABLE\\|HETA\\|OL\\)\\|WARNINGS\\|include\\)\\|INCLUDE\\)\\>"
"Exact regular expression of ALL known records for NONMEM 6")

(defvar esn-records-reg-6 "\\<\\(\\$\\(?:A\\(?:BB\\(?:REVIATED\\|[A-Z0-9_]*\\)\\|ES\\(?:INITIAL\\|[A-Z0-9_]*\\)?\\)\\|BIN[A-Z0-9_]*\\|CO\\(?:N\\(?:TR\\|[A-Z0-9_]*\\)\\|V\\(?:ARIANCE\\|[A-Z0-9_]*\\)\\)\\|D\\(?:AT[A-Z0-9_]*\\|ES[A-Z0-9_]*\\)\\|E\\(?:RR\\(?:OR\\|[A-Z0-9_]*\\)\\|ST\\(?:IMAT\\(?:E\\|ION\\)\\|[A-Z0-9_]*\\)\\)\\|IN\\(?:C\\(?:LUDE\\|[A-Z0-9_]*\\)\\|D\\(?:EX\\(?:ES\\)?\\|XS\\|[A-Z0-9_]*\\)\\|F[A-Z0-9_]*\\|P\\(?:UT\\|[A-Z0-9_]*\\)\\)\\|M\\(?:IX[A-Z0-9_]*\\|OD\\(?:EL\\|[A-Z0-9_]*\\)\\|SF[A-Z0-9_]*\\)\\|NON\\(?:PARAMETRIC\\|[A-Z0-9_]*\\)\\|OM\\(?:E\\(?:GA\\|[A-Z0-9_]*\\)\\|I[A-Z0-9_]*\\)\\|P\\(?:K\\|R\\(?:E[A-Z0-9_]*\\|I\\(?:OR\\|[A-Z0-9_]*\\)\\|O\\(?:BLEM\\|[A-Z0-9_]*\\)\\)\\)\\|S\\(?:CA\\(?:TTER\\(?:GRAMS\\|PLOT\\|S\\)\\|[A-Z0-9_]*\\)\\|I\\(?:G\\(?:MA\\|[A-Z0-9_]*\\)\\|M\\(?:ULAT\\(?:E\\|ION\\)\\|[A-Z0-9_]*\\)\\)\\|U\\(?:B\\(?:ROUTINES\\|[A-Z0-9_]*\\)\\|P\\(?:ER\\|[A-Z0-9_]*\\)\\)\\)\\|T\\(?:AB\\(?:LE\\|[A-Z0-9_]*\\)\\|HE\\(?:TA\\|[A-Z0-9_]*\\)\\|OL[A-Z0-9_]*\\)\\|WAR\\(?:NINGS\\|[A-Z0-9_]*\\)\\)\\|INCL\\(?:UDE\\|[A-Z0-9_]*\\)\\)\\>"
"* Regular expression of ALL known records for NONMEM 6")

(defvar esn-records-word-reg-6 "\\(\\<\\(?:\\$\\(?:A\\(?:BB\\(?:REVIATED\\|[A-Z0-9_]*\\)\\|ES\\(?:INITIAL\\|[A-Z0-9_]*\\)?\\)\\|BIN[A-Z0-9_]*\\|CO\\(?:N\\(?:TR\\|[A-Z0-9_]*\\)\\|V\\(?:ARIANCE\\|[A-Z0-9_]*\\)\\)\\|D\\(?:AT[A-Z0-9_]*\\|ES[A-Z0-9_]*\\)\\|E\\(?:RR\\(?:OR\\|[A-Z0-9_]*\\)\\|ST\\(?:IMAT\\(?:E\\|ION\\)\\|[A-Z0-9_]*\\)\\)\\|IN\\(?:C\\(?:LUDE\\|[A-Z0-9_]*\\)\\|D\\(?:EX\\(?:ES\\)?\\|XS\\|[A-Z0-9_]*\\)\\|F[A-Z0-9_]*\\|P\\(?:UT\\|[A-Z0-9_]*\\)\\)\\|M\\(?:IX[A-Z0-9_]*\\|OD\\(?:EL\\|[A-Z0-9_]*\\)\\|SF[A-Z0-9_]*\\)\\|NON\\(?:PARAMETRIC\\|[A-Z0-9_]*\\)\\|OM\\(?:E\\(?:GA\\|[A-Z0-9_]*\\)\\|I[A-Z0-9_]*\\)\\|P\\(?:K\\|R\\(?:E[A-Z0-9_]*\\|I\\(?:OR\\|[A-Z0-9_]*\\)\\|O\\(?:BLEM\\|[A-Z0-9_]*\\)\\)\\)\\|S\\(?:CA\\(?:TTER\\(?:GRAMS\\|PLOT\\|S\\)\\|[A-Z0-9_]*\\)\\|I\\(?:G\\(?:MA\\|[A-Z0-9_]*\\)\\|M\\(?:ULAT\\(?:E\\|ION\\)\\|[A-Z0-9_]*\\)\\)\\|U\\(?:B\\(?:ROUTINES\\|[A-Z0-9_]*\\)\\|P\\(?:ER\\|[A-Z0-9_]*\\)\\)\\)\\|T\\(?:AB\\(?:LE\\|[A-Z0-9_]*\\)\\|HE\\(?:TA\\|[A-Z0-9_]*\\)\\|OL[A-Z0-9_]*\\)\\|WAR\\(?:NINGS\\|[A-Z0-9_]*\\)\\)\\|INCL\\(?:UDE\\|[A-Z0-9_]*\\)\\)\\>\\|\\<[A-Z][A-Z0-9_]*\\(?:([ \t]*[0-9]+[ \t]*)\\)?\\>\\)"
"* Regular expression of all known records or Variables")
(defvar esn-record-options-reg-6 '(

	("INC" "\\<\\(\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("WAR" "\\<\\(D\\(?:AT\\(?:AMAXIMUM\\|[A-Z0-9_]*\\)\\|MA\\(?:XIMUM\\|[A-Z0-9_]*\\)\\)\\|E\\(?:MA\\(?:XIMUM\\|[A-Z0-9_]*\\)\\|RR\\(?:MAXIMUM\\|ORMAXIMUM\\|[A-Z0-9_]*\\)\\)\\|LIS[A-Z0-9_]*\\|NO\\(?:N[A-Z0-9_]*\\|R\\(?:ESET\\|[A-Z0-9_]*\\)\\)\\|RES\\(?:ET\\|[A-Z0-9_]*\\)\\|W\\(?:AR\\(?:N\\(?:\\(?:ING\\)?MAXIMUM\\)\\|[A-Z0-9_]*\\)\\|MA[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("THE" "\\<\\(-INF\\|ABO\\(?:RT\\|[A-Z0-9_]*\\)\\|FIX\\(?:ED\\|[A-Z0-9_]*\\)\\|INF\\|N\\(?:OA\\(?:BORT\\(?:FIRST\\)?\\|[A-Z0-9_]*\\)\\|UM\\(?:BERP\\(?:\\(?:OIN\\)?TS\\)\\|P\\(?:\\(?:OIN\\)?TS\\)\\|[A-Z0-9_]*\\)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("TAB" "\\<\\(APP\\(?:END\\|[A-Z0-9_]*\\)\\|CON\\(?:DITIONAL\\|[A-Z0-9_]*\\)\\|F\\(?:I\\(?:L[A-Z0-9_]*\\|R\\(?:ST\\(?:\\(?:REC\\(?:ORD\\)?\\)?ONLY\\)\\|[A-Z0-9_]*\\)\\)\\|OR\\(?:WARD\\|[A-Z0-9_]*\\)\\)\\|NO\\(?:A\\(?:PPEND\\|[A-Z0-9_]*\\)\\|F\\(?:ORWARD\\|[A-Z0-9_]*\\)\\|H\\(?:EADER\\|[A-Z0-9_]*\\)\\|P\\(?:RINT\\|[A-Z0-9_]*\\)\\)\\|O\\(?:MI\\(?:TTED\\|[A-Z0-9_]*\\)\\|NE\\(?:HEADER\\|[A-Z0-9_]*\\)\\)\\|PRI\\(?:NT\\|[A-Z0-9_]*\\)\\|UNC\\(?:ONDITIONAL\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("SUP" "\\<\\(ITE\\(?:RATIONS\\|[A-Z0-9_]*\\)\\|N\\(?:IT\\(?:ERATIONS\\|[A-Z0-9_]*\\)\\|OP\\(?:RINT\\|[A-Z0-9_]*\\)\\)\\|PRI\\(?:NT\\|[A-Z0-9_]*\\)\\|SCO\\(?:PE\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("SUB" "\\<\\(A\\(?:DV\\(?:AN\\(?:1[012]\\|[1-9]\\)?\\|[A-Z0-9_]*\\)\\|ES[A-Z0-9_]*\\)\\|C\\(?:CO\\(?:NTR\\|[A-Z0-9_]*\\)\\|ON\\(?:TR\\|[A-Z0-9_]*\\)\\|RI[A-Z0-9_]*\\)\\|DES[A-Z0-9_]*\\|ERR\\(?:OR\\|[A-Z0-9_]*\\)\\|IN\\(?:F[A-Z0-9_]*\\|L\\(?:ETA\\|[A-Z0-9_]*\\)\\)\\|M\\(?:IX[A-Z0-9_]*\\|OD\\(?:EL\\|[A-Z0-9_]*\\)\\)\\|OTH\\(?:ER\\|[A-Z0-9_]*\\)\\|P\\(?:K\\|R\\(?:E[A-Z0-9_]*\\|I\\(?:OR\\|[A-Z0-9_]*\\)\\)\\)\\|S\\(?:PT\\(?:WO\\|[A-Z0-9_]*\\)\\|S\\(?:1[012]\\|[1-9]\\)?\\|UB\\(?:ROUTINES\\|[A-Z0-9_]*\\)\\)\\|T\\(?:OL[A-Z0-9_]*\\|RA\\(?:NS[1-6]?\\|[A-Z0-9_]*\\)\\)\\|USM\\(?:ETA\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("SIM" "\\<\\(N\\(?:EW\\|O\\(?:N\\(?:PARAMETRIC\\|[A-Z0-9_]*\\)\\|P\\(?:REDICTION\\|[A-Z0-9_]*\\)\\|R\\(?:MAL\\|[A-Z0-9_]*\\)\\)\\|SU\\(?:BPROB\\(?:\\(?:LEM\\)?S\\)\\|[A-Z0-9_]*\\)\\)\\|O\\(?:MI\\(?:TTED\\|[A-Z0-9_]*\\)\\|NL\\(?:YSIMULATION\\|[A-Z0-9_]*\\)\\)\\|PRE\\(?:DICTION\\|[A-Z0-9_]*\\)\\|REQ\\(?:UEST\\(?:FIRST\\|SECOND\\)\\|[A-Z0-9_]*\\)\\|SUB\\(?:PROB\\(?:\\(?:LEM\\)?S\\)\\|[A-Z0-9_]*\\)\\|TRU[A-Z0-9_]*\\|UNI\\(?:FORM\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("SIG" "\\<\\(BLO\\(?:CK\\|[A-Z0-9_]*\\)\\|DIA\\(?:GONAL\\|[A-Z0-9_]*\\)\\|FIX\\(?:ED\\|[A-Z0-9_]*\\)\\|SAM[A-Z0-9_]*\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("SCA" "\\<\\(AB\\(?:0\\|S\\(?:ZERO\\|[A-Z0-9_]*\\)\\|Z\\(?:ERO\\|[A-Z0-9_]*\\)\\)\\|CON\\(?:DITIONAL\\|[A-Z0-9_]*\\)\\|F\\(?:IR\\(?:ST\\(?:\\(?:REC\\(?:ORD\\)?\\)?ONLY\\)\\|[A-Z0-9_]*\\)\\|RO[A-Z0-9_]*\\)\\|NO\\(?:A\\(?:BS0\\|[A-Z0-9_]*\\)\\|O\\(?:RD\\(?:0\\|ZERO\\)\\|[A-Z0-9_]*\\)\\)\\|O\\(?:BS\\(?:ONLY\\|[A-Z0-9_]*\\)\\|MI\\(?:TTED\\|[A-Z0-9_]*\\)\\|RD\\(?:ZERO\\|[A-Z0-9_]*\\)\\)\\|TO\\|UN\\(?:C\\(?:ONDITIONAL\\|[A-Z0-9_]*\\)\\|I[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("PRI" "\\<\\(DIS\\(?:PLAY\\|[A-Z0-9_]*\\)\\|EST\\(?:IMATION\\|[A-Z0-9_]*\\)\\|IPR\\(?:OB\\|[A-Z0-9_]*\\)\\|NWP\\(?:RI\\|[A-Z0-9_]*\\)\\|PRO\\(?:BLEM\\|[A-Z0-9_]*\\)\\|SIM\\(?:ULATION\\|[A-Z0-9_]*\\)\\|TNP\\(?:RI\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("OME" "\\<\\(BLO\\(?:CK\\|[A-Z0-9_]*\\)\\|DIA\\(?:GONAL\\|[A-Z0-9_]*\\)\\|FIX\\(?:ED\\|[A-Z0-9_]*\\)\\|SAM[A-Z0-9_]*\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("NON" "\\<\\(CON\\(?:DITIONAL\\|[A-Z0-9_]*\\)\\|ETA[A-Z0-9_]*\\|M\\(?:AR\\(?:GINALS\\|[A-Z0-9_]*\\)\\|SF[A-Z0-9_]*\\)\\|OMI\\(?:TTED\\|[A-Z0-9_]*\\)\\|REC\\(?:OMPUTE\\|[A-Z0-9_]*\\)\\|UNC\\(?:ONDITIONAL\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("MSF" "\\<\\(N\\(?:OR\\(?:ESCALE\\|[A-Z0-9_]*\\)\\|PO\\(?:PETAS\\|[A-Z0-9_]*\\)\\)\\|ONL\\(?:YREAD\\|[A-Z0-9_]*\\)\\|POP\\(?:ETAS\\|[A-Z0-9_]*\\)\\|RES\\(?:CALE\\|[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("MOD" "\\<\\(COM\\(?:PARTMENT\\|[A-Z0-9_]*\\)\\|N\\(?:C\\(?:M[A-Z0-9_]*\\|O\\(?:MP\\(?:\\(?:ARTMENT\\)?S\\)\\|[A-Z0-9_]*\\)\\)\\|EQ\\(?:UILIBRIUM\\|[A-Z0-9_]*\\)\\|PA\\(?:RAM\\(?:\\(?:ETER\\)?S\\)\\|[A-Z0-9_]*\\)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("INP" "\\<\\(DRO[A-Z0-9_]*\\|NON\\(?:MEM\\|[A-Z0-9_]*\\)\\|SKI[A-Z0-9_]*\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("EST" "\\<\\(-2L\\(?:L\\(?:IKELIHOOD\\|[A-Z0-9_]*\\)\\|O\\(?:GLIKELIHOOD\\|[A-Z0-9_]*\\)\\)\\|ABO\\(?:RT\\|[A-Z0-9_]*\\)\\|CEN\\(?:TERING\\|[A-Z0-9_]*\\)\\|ETA\\(?:BARCHECK\\|[A-Z0-9_]*\\)\\|FO\\|GRI[A-Z0-9_]*\\|INT\\(?:ERACTION\\|[A-Z0-9_]*\\)\\|L\\(?:AP\\(?:LACIAN\\|[A-Z0-9_]*\\)\\|IK\\(?:ELIHOOD\\|[A-Z0-9_]*\\)\\)\\|M\\(?:AX\\(?:EVALS\\|[A-Z0-9_]*\\)\\|ET\\(?:HOD\\|[A-Z0-9_]*\\)\\|SF[A-Z0-9_]*\\)\\|N\\(?:O\\(?:A\\(?:BORT\\|[A-Z0-9_]*\\)\\|C\\(?:ENTERING\\|[A-Z0-9_]*\\)\\|E\\(?:TABARCHECK\\|[A-Z0-9_]*\\)\\|F[A-Z0-9_]*\\|I\\(?:NTERACTION\\|[A-Z0-9_]*\\)\\|L\\(?:APLACIAN\\|[A-Z0-9_]*\\)\\|N\\(?:UMERICAL\\|[A-Z0-9_]*\\)\\|O\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|MEGABOUNDTEST\\|[A-Z0-9_]*\\)\\|P\\(?:OSTHOC\\|[A-Z0-9_]*\\)\\|R\\(?:EPEAT[12]?\\|[A-Z0-9_]*\\)\\|S\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|IGMABOUNDTEST\\|LOW\\|ORT\\|[A-Z0-9_]*\\)\\|T\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|HETABOUNDTEST\\|[A-Z0-9_]*\\)\\)\\|SI\\(?:GDIGITS\\|[A-Z0-9_]*\\)\\|UM\\(?:ERICAL\\|[A-Z0-9_]*\\)\\)\\|OM\\(?:E\\(?:GABOUNDTEST\\|[A-Z0-9_]*\\)\\|I\\(?:TTED\\|[A-Z0-9_]*\\)\\)\\|P\\(?:OS\\(?:THOC\\|[A-Z0-9_]*\\)\\|R\\(?:E\\(?:DICTION\\|[A-Z0-9_]*\\)\\|I\\(?:NT\\|[A-Z0-9_]*\\)\\)\\)\\|REP\\(?:EAT[12]?\\|[A-Z0-9_]*\\)\\|S\\(?:B\\(?:O\\(?:UNDTEST\\|[A-Z0-9_]*\\)\\|T\\)\\|IG\\(?:DIGITS\\|MABOUNDTEST\\|[A-Z0-9_]*\\)\\|LO[A-Z0-9_]*\\|OR[A-Z0-9_]*\\|TI\\(?:ELTJES\\|[A-Z0-9_]*\\)\\)\\|T\\(?:B\\(?:O\\(?:UNDTEST\\|[A-Z0-9_]*\\)\\|T\\)\\|HE\\(?:TABOUNDTEST\\|[A-Z0-9_]*\\)\\)\\|ZER[A-Z0-9_]*\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("DAT" "\\<\\(ACC\\(?:EPT\\|[A-Z0-9_]*\\)\\|BLA\\(?:NKOK\\|[A-Z0-9_]*\\)\\|IGN\\(?:ORE\\|[A-Z0-9_]*\\)\\|L\\(?:AS\\(?:T20\\|[A-Z0-9_]*\\)\\|RE\\(?:CL\\|[A-Z0-9_]*\\)\\)\\|N\\(?:O\\(?:O\\(?:PEN\\|[A-Z0-9_]*\\)\\|R\\(?:EWIND\\|[A-Z0-9_]*\\)\\|W\\(?:IDE\\|[A-Z0-9_]*\\)\\)\\|UL[A-Z0-9_]*\\)\\|REW\\(?:IND\\|[A-Z0-9_]*\\)\\|TRA\\(?:NSLATE\\|[A-Z0-9_]*\\)\\|WID[A-Z0-9_]*\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("COV" "\\<\\(CO\\(?:M\\(?:PRESS\\|[A-Z0-9_]*\\)\\|N\\(?:DITIONAL\\|[A-Z0-9_]*\\)\\)\\|MAT\\(?:RIX\\|[A-Z0-9_]*\\)\\|NOS\\(?:LOW\\|[A-Z0-9_]*\\)\\|OMI\\(?:TTED\\|[A-Z0-9_]*\\)\\|PRI\\(?:NT\\|[A-Z0-9_]*\\)\\|S\\(?:LO[A-Z0-9_]*\\|PE\\(?:CIAL\\|[A-Z0-9_]*\\)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>")
	("BIN" "\\<\\(D\\(?:OS[A-Z0-9_]*\\|RO[A-Z0-9_]*\\)\\|LAS[A-Z0-9_]*\\|NEX[A-Z0-9_]*\\|SKI[A-Z0-9_]*\\|[\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)-]\\)\\>")
	("ABB" "\\<\\(COM\\(?:RES\\|SAV\\|[A-Z0-9_]*\\)\\|DE\\(?:R\\(?:IV2\\|[A-Z0-9_]*\\)\\|S[A-Z0-9_]*\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>"))
"* Regular expressions for Options by Record OR next record for NONMEM 6")

(defvar esn-records-with-options-reg-6 "\\<\\(\\$\\(?:ABB\\|BIN\\|COV\\|DAT\\|EST\\|IN[CP]\\|M\\(?:OD\\|SF\\)\\|NON\\|OME\\|PRI\\|S\\(?:CA\\|I[GM]\\|U[BP]\\)\\|T\\(?:AB\\|HE\\)\\|WAR\\)\\|INC\\)[A-Z0-9_]*\\>"
"* Regular expression of Records with Options")

(defvar esn-records-all-options-6 "\\<\\(-\\(?:2L\\(?:L\\(?:IKELIHOOD\\|[A-Z0-9_]*\\)\\|O\\(?:GLIKELIHOOD\\|[A-Z0-9_]*\\)\\)\\|INF\\)?\\|A\\(?:B\\(?:0\\|O\\(?:RT\\|[A-Z0-9_]*\\)\\|S\\(?:ZERO\\|[A-Z0-9_]*\\)\\|Z\\(?:ERO\\|[A-Z0-9_]*\\)\\)\\|CC\\(?:EPT\\|[A-Z0-9_]*\\)\\|DV\\(?:AN\\(?:1[012]\\|[1-9]\\)?\\|[A-Z0-9_]*\\)\\|ES[A-Z0-9_]*\\|PP\\(?:END\\|[A-Z0-9_]*\\)\\)\\|BL\\(?:A\\(?:NKOK\\|[A-Z0-9_]*\\)\\|O\\(?:CK\\|[A-Z0-9_]*\\)\\)\\|C\\(?:CO\\(?:NTR\\|[A-Z0-9_]*\\)\\|EN\\(?:TERING\\|[A-Z0-9_]*\\)\\|O\\(?:M\\(?:P\\(?:ARTMENT\\|RESS\\)\\|RES\\|SAV\\|[A-Z0-9_]*\\)\\|N\\(?:DITIONAL\\|TR\\|[A-Z0-9_]*\\)\\)\\|RI[A-Z0-9_]*\\)\\|D\\(?:AT\\(?:AMAXIMUM\\|[A-Z0-9_]*\\)\\|E\\(?:R\\(?:IV2\\|[A-Z0-9_]*\\)\\|S[A-Z0-9_]*\\)\\|I\\(?:A\\(?:GONAL\\|[A-Z0-9_]*\\)\\|S\\(?:PLAY\\|[A-Z0-9_]*\\)\\)\\|MA\\(?:XIMUM\\|[A-Z0-9_]*\\)\\|OS[A-Z0-9_]*\\|RO[A-Z0-9_]*\\)\\|E\\(?:MA\\(?:XIMUM\\|[A-Z0-9_]*\\)\\|RR\\(?:MAXIMUM\\|OR\\(?:MAXIMUM\\)?\\|[A-Z0-9_]*\\)\\|ST\\(?:IMATION\\|[A-Z0-9_]*\\)\\|TA\\(?:BARCHECK\\|[A-Z0-9_]*\\)\\)\\|F\\(?:I\\(?:L[A-Z0-9_]*\\|R\\(?:ST\\(?:\\(?:REC\\(?:ORD\\)?\\)?ONLY\\)\\|[A-Z0-9_]*\\)\\|X\\(?:ED\\|[A-Z0-9_]*\\)\\)\\|O\\(?:R\\(?:WARD\\|[A-Z0-9_]*\\)\\)?\\|RO[A-Z0-9_]*\\)\\|GRI[A-Z0-9_]*\\|I\\(?:GN\\(?:ORE\\|[A-Z0-9_]*\\)\\|N\\(?:F[A-Z0-9_]*\\|L\\(?:ETA\\|[A-Z0-9_]*\\)\\|T\\(?:ERACTION\\|[A-Z0-9_]*\\)\\)\\|PR\\(?:OB\\|[A-Z0-9_]*\\)\\|TE\\(?:RATIONS\\|[A-Z0-9_]*\\)\\)\\|L\\(?:A\\(?:P\\(?:LACIAN\\|[A-Z0-9_]*\\)\\|S\\(?:T20\\|[A-Z0-9_]*\\)\\)\\|I\\(?:K\\(?:ELIHOOD\\|[A-Z0-9_]*\\)\\|S[A-Z0-9_]*\\)\\|RE\\(?:CL\\|[A-Z0-9_]*\\)\\)\\|M\\(?:A\\(?:R\\(?:GINALS\\|[A-Z0-9_]*\\)\\|T\\(?:RIX\\|[A-Z0-9_]*\\)\\|X\\(?:EVALS\\|[A-Z0-9_]*\\)\\)\\|ET\\(?:HOD\\|[A-Z0-9_]*\\)\\|IX[A-Z0-9_]*\\|OD\\(?:EL\\|[A-Z0-9_]*\\)\\|SF[A-Z0-9_]*\\)\\|N\\(?:C\\(?:M[A-Z0-9_]*\\|O\\(?:MP\\(?:\\(?:ARTMENT\\)?S\\)\\|[A-Z0-9_]*\\)\\)\\|E\\(?:Q\\(?:UILIBRIUM\\|[A-Z0-9_]*\\)\\|W\\|X[A-Z0-9_]*\\)\\|IT\\(?:ERATIONS\\|[A-Z0-9_]*\\)\\|O\\(?:A\\(?:B\\(?:ORT\\(?:FIRST\\)?\\|S0\\)\\|PPEND\\|[A-Z0-9_]*\\)\\|C\\(?:ENTERING\\|[A-Z0-9_]*\\)\\|E\\(?:TABARCHECK\\|[A-Z0-9_]*\\)\\|F\\(?:ORWARD\\|[A-Z0-9_]*\\)\\|H\\(?:EADER\\|[A-Z0-9_]*\\)\\|I\\(?:NTERACTION\\|[A-Z0-9_]*\\)\\|L\\(?:APLACIAN\\|[A-Z0-9_]*\\)\\|N\\(?:MEM\\|PARAMETRIC\\|UMERICAL\\|[A-Z0-9_]*\\)\\|O\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|MEGABOUNDTEST\\|PEN\\|RD\\(?:0\\|ZERO\\)\\|[A-Z0-9_]*\\)\\|P\\(?:OSTHOC\\|R\\(?:EDICTION\\|INT\\)\\|[A-Z0-9_]*\\)\\|R\\(?:E\\(?:PEAT[12]?\\|S\\(?:CALE\\|ET\\)\\|WIND\\)\\|MAL\\|[A-Z0-9_]*\\)\\|S\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|IGMABOUNDTEST\\|LOW\\|ORT\\|[A-Z0-9_]*\\)\\|T\\(?:B\\(?:\\(?:OUNDTES\\)?T\\)\\|HETABOUNDTEST\\|[A-Z0-9_]*\\)\\|W\\(?:IDE\\|[A-Z0-9_]*\\)\\)\\|P\\(?:A\\(?:RAM\\(?:\\(?:ETER\\)?S\\)\\|[A-Z0-9_]*\\)\\|O\\(?:PETAS\\|[A-Z0-9_]*\\)\\)\\|S\\(?:I\\(?:GDIGITS\\|[A-Z0-9_]*\\)\\|U\\(?:BPROB\\(?:\\(?:LEM\\)?S\\)\\|[A-Z0-9_]*\\)\\)\\|U\\(?:L[A-Z0-9_]*\\|M\\(?:BERP\\(?:\\(?:OIN\\)?TS\\)\\|ERICAL\\|P\\(?:\\(?:OIN\\)?TS\\)\\|[A-Z0-9_]*\\)\\)\\|WP\\(?:RI\\|[A-Z0-9_]*\\)\\)\\|O\\(?:BS\\(?:ONLY\\|[A-Z0-9_]*\\)\\|M\\(?:E\\(?:GABOUNDTEST\\|[A-Z0-9_]*\\)\\|I\\(?:TTED\\|[A-Z0-9_]*\\)\\)\\|N\\(?:E\\(?:HEADER\\|[A-Z0-9_]*\\)\\|L\\(?:Y\\(?:READ\\|SIMULATION\\)\\|[A-Z0-9_]*\\)\\)\\|RD\\(?:ZERO\\|[A-Z0-9_]*\\)\\|TH\\(?:ER\\|[A-Z0-9_]*\\)\\)\\|P\\(?:K\\|O\\(?:P\\(?:ETAS\\|[A-Z0-9_]*\\)\\|S\\(?:THOC\\|[A-Z0-9_]*\\)\\)\\|R\\(?:E\\(?:DICTION\\|[A-Z0-9_]*\\)\\|I\\(?:NT\\|OR\\|[A-Z0-9_]*\\)\\|O\\(?:BLEM\\|[A-Z0-9_]*\\)\\)\\)\\|RE\\(?:C\\(?:OMPUTE\\|[A-Z0-9_]*\\)\\|P\\(?:EAT[12]?\\|[A-Z0-9_]*\\)\\|Q\\(?:UEST\\(?:FIRST\\|SECOND\\)\\|[A-Z0-9_]*\\)\\|S\\(?:CALE\\|ET\\|[A-Z0-9_]*\\)\\|W\\(?:IND\\|[A-Z0-9_]*\\)\\)\\|S\\(?:AM[A-Z0-9_]*\\|B\\(?:O\\(?:UNDTEST\\|[A-Z0-9_]*\\)\\|T\\)\\|CO\\(?:PE\\|[A-Z0-9_]*\\)\\|I\\(?:G\\(?:DIGITS\\|MABOUNDTEST\\|[A-Z0-9_]*\\)\\|M\\(?:ULATION\\|[A-Z0-9_]*\\)\\)\\|KI[A-Z0-9_]*\\|LO[A-Z0-9_]*\\|OR[A-Z0-9_]*\\|P\\(?:E\\(?:CIAL\\|[A-Z0-9_]*\\)\\|T\\(?:WO\\|[A-Z0-9_]*\\)\\)\\|S\\(?:1[012]\\|[1-9]\\)?\\|TI\\(?:ELTJES\\|[A-Z0-9_]*\\)\\|UB\\(?:PROB\\(?:\\(?:LEM\\)?S\\)\\|ROUTINES\\|[A-Z0-9_]*\\)\\)\\|T\\(?:B\\(?:O\\(?:UNDTEST\\|[A-Z0-9_]*\\)\\|T\\)\\|HE\\(?:TABOUNDTEST\\|[A-Z0-9_]*\\)\\|NP\\(?:RI\\|[A-Z0-9_]*\\)\\|O\\(?:L[A-Z0-9_]*\\)?\\|R\\(?:A\\(?:NS\\(?:LATE\\|[1-6]\\)?\\|[A-Z0-9_]*\\)\\|U[A-Z0-9_]*\\)\\)\\|U\\(?:N\\(?:C\\(?:ONDITIONAL\\|[A-Z0-9_]*\\)\\|I\\(?:FORM\\|[A-Z0-9_]*\\)\\)\\|SM\\(?:ETA\\|[A-Z0-9_]*\\)\\)\\|W\\(?:AR\\(?:N\\(?:\\(?:ING\\)?MAXIMUM\\)\\|[A-Z0-9_]*\\)\\|ID[A-Z0-9_]*\\|MA[A-Z0-9_]*\\)\\|ZER[A-Z0-9_]*\\)\\>"
"* Regular expression of ALL options")

(defvar esn-records-options-val-reg-6 '(
("WAR"
	("ERR" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("EMA" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("THE"
	("NUM" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("TAB"
	("FIL" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
)
("SUP"
	("ITE" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NIT" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("SCO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("SUB"
	("OTH" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("INF" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("AES" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("DES" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("MOD" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("ERR" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("PK" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("INL" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("SPT" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("USM" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("CCO" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("CON" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("PRI" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("PRE" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("MIX" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("CRI" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("TRA" "\\<\\(TRANS[1-6]\\)\\>")
	("SS" "\\<\\(SS\\(?:1[012]\\|[1-9]\\)\\)\\>")
	("ADV" "\\<\\(ADVAN\\(?:1[012]\\|[1-9]\\)\\)\\>")
	("TOL" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\|\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("SUB" "\\<\\(DP\\|LIB\\(?:RARY\\|[A-Z0-9_]*\\)\\)\\>")
)
("SIM"
	("TRU" "\\<\\(FIN\\(?:AL\\|[A-Z0-9_]*\\)\\|INI\\(?:TIAL\\|[A-Z0-9_]*\\)\\|PRI\\(?:OR\\|[A-Z0-9_]*\\)\\)\\>")
	("SUB" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NSU" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("SCA"
	("TO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("FRO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("PRI"
	("DIS" "\\<\\(ALL[A-Z0-9_]*\\|CNT[A-Z0-9_]*\\)\\>")
	("PRO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("IPR" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("as" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("NON"
	("MSF" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
)
("MSF"
	("NPO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("POP" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("MOD"
	("COM" "\\<\\((nameattribute1attribute2\\.\\.\\.)\\)\\>")
	("NPA" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NEQ" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NCO" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NCM" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("EST"
	("MSF" "\\<\\(\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)\\)\\>")
	("PRI" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("MAX" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("SIG" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NSI" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("ZER" "\\<\\(\\(?:[0-9]+\\|([ \t]*\\(?:[0-9]+[ \t,]+\\)?[ \t]*)\\)\\)\\>")
	("SLO" "\\<\\([12]\\)\\>")
	("GRI" "\\<\\(([ \t]*[0-9.]+[ \t,]+[0-9.]+[ \t,]+[0-9.]+[ \t,]+[0-9.]+[ \t]*)\\)\\>")
	("MET" "\\<\\(CON\\(?:DITIONAL\\|[A-Z0-9_]*\\)\\|HYB\\(?:RID\\|[A-Z0-9_]*\\)\\|ZER[A-Z0-9_]*\\|[01]\\)\\>")
)
("DAT"
	("TRA" "\\<\\(([ \t]*\\(?:II\\|TIME\\)/24\\(?:\\|[.]00\\|[.]000\\)\\(?:[ \t,]+\\(?:II\\|TIME\\)/24\\(?:\\|[.]00\\|[.]000\\)\\)?[ \t]*)\\)\\>")
	("LAS" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
	("NUL" "\\<\\([\"']?.+?[\"']?\\)\\>")
	("IGN" "\\<\\([\"']?.+?[\"']?\\)\\>")
	("LRE" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
("COV"
	("PRI" "\\<\\([ERS]\\)\\>")
	("MAT" "\\<\\([RS]\\)\\>")
)
("ABB"
	("DES" "\\<\\(COM\\(?:PACT\\|[A-Z0-9_]*\\)\\|FUL[A-Z0-9_]*\\)\\>")
	("DER" "\\<\\(NO\\(?:C\\(?:OMMON\\|[A-Z0-9_]*\\)\\)?\\)\\>")
	("COM" "\\<\\([+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\|[.][0-9]+\\)\\)\\>")
)
)
"* Regular expressions for Option Values by Record and Option for NONMEM 6")

(defvar esn-records-which-options-have-val-reg-6 '(
	("WAR" "\\<\\(\\(?:E\\(?:MA\\|RR\\)\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("THE" "\\<\\(\\(?:NUM\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("TAB" "\\<\\(\\(?:FIL\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("SUP" "\\<\\(\\(?:ITE\\|NIT\\|SCO\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("SUB" "\\<\\(\\(?:A\\(?:DV\\|ES\\)\\|C\\(?:CO\\|ON\\|RI\\)\\|DES\\|ERR\\|IN[FL]\\|M\\(?:IX\\|OD\\)\\|OTH\\|P\\(?:K\\|R[EI]\\)\\|S\\(?:PT\\|S\\|UB\\)\\|T\\(?:OL\\|RA\\)\\|USM\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("SIM" "\\<\\(\\(?:NSU\\|SUB\\|TRU\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("SCA" "\\<\\(\\(?:\\(?:FR\\|T\\)O\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("PRI" "\\<\\(\\(?:DIS\\|IPR\\|PRO\\|as\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("NON" "\\<\\(\\(?:MSF\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("MSF" "\\<\\(\\(?:NPO\\|POP\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("MOD" "\\<\\(\\(?:COM\\|N\\(?:C[MO]\\|EQ\\|PA\\)\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("EST" "\\<\\(\\(?:GRI\\|M\\(?:AX\\|ET\\|SF\\)\\|NSI\\|PRI\\|S\\(?:IG\\|LO\\)\\|ZER\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("DAT" "\\<\\(\\(?:ACC\\|IGN\\|L\\(?:AS\\|RE\\)\\|NUL\\|TRA\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("COV" "\\<\\(\\(?:MAT\\|PRI\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
	("ABB" "\\<\\(\\(?:COM\\|DE[RS]\\)[A-Z0-9_]*\\|\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)[A-Z0-9_]**\\>\\)\\>")
)
"* Regular expressions for which Options have Values in NONMEM 6")

(defvar esn-records-which-have-val-reg-6 "\\<\\(\\$\\(?:ABB\\|COV\\|DAT\\|EST\\|M\\(?:OD\\|SF\\)\\|NON\\|PRI\\|S\\(?:CA\\|IM\\|U[BP]\\)\\|T\\(?:AB\\|HE\\)\\|WAR\\)\\)[A-Z0-9_]*\\>"
"* Regular expression for Records with OPTION=VALUE for NONMEM 6")
(defvar esn-abbrev-records-regexp-6 "\\<\\(\\$\\(?:AES\\(?:INITIAL\\|[A-Z0-9_]*\\)?\\|DES[A-Z0-9_]*\\|ERR\\(?:OR\\|[A-Z0-9_]*\\)\\|INF[A-Z0-9_]*\\|MIX[A-Z0-9_]*\\|P\\(?:K\\|RE[A-Z0-9_]*\\)\\|TOL[A-Z0-9_]*\\)\\)\\>"
"* Regular expression for Abbreviated Records in NONMEM 6")
(defvar esn-input-data-items-6 "\\<\\(A\\(?:DDL\\|MT\\)\\|C\\(?:ALL\\|\\(?:M\\|ON\\)T\\)\\|D\\(?:AT[123E]\\|V\\)\\|EVID\\|I[DI]\\|L[12]\\|M\\(?:DV\\|RG_\\)\\|PCMT\\|R\\(?:A\\(?:TE\\|W_\\)\\|PT_\\)\\|SS\\|TIME\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>"
"* Regular expression for reserved NONMEM 6 input keywords.")
(defvar esn-abbrev-lhs-6 '(
	("TOL" "\\<\\(NRD\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PRE" "\\<\\(\\(?:[Y]\\|INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PK" "\\<\\(A\\(?:DVAN\\>\\|L\\(?:\\(?:AG[0-9]+\\|PHA\\)\\>\\)\\|OB\\>\\|_0([ \t]*[0-9]+[ \t]*)\\)\\|BETA\\>\\|CL\\>\\|D[0-9]+\\>\\|F\\(?:\\(?:[0-9]+\\|[O]\\)\\>\\)\\|GAMMA\\>\\|I_SS\\>\\|K\\(?:[AM]?\\>\\)\\|MTIME\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|P([ \t]*[0-9]+[ \t]*)\\|\\(?:R[0-9]+\\|S\\(?:[0-9]+\\|[C]\\)\\|T\\(?:RANS\\|SCALE\\)\\|V\\(?:M\\|SS\\)\\|XSCALE\\|\\(?:[QV]\\|INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("MIX" "\\<\\(A[0-9]+\\>\\|I\\(?:\\(?:[0-9]+\\|CALL\\)\\>\\)\\|MIXP\\>\\|NSPOP\\>\\|P\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("INF" "\\<\\(\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("ERR" "\\<\\(\\(?:COMRES\\|\\(?:[Y]\\|INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("DES" "\\<\\(DADT([ \t]*[0-9]+[ \t]*)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("AES0" "\\<\\(A\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:INIT\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("AES" "\\<\\(E\\(?:([ \t]*[0-9]+[ \t]*)\\)?\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")

)
"* Left Handed Regular Expressions")
(defvar esn-abbrev-rhs-6 '(
	("TOL" "\\<\\(NRD([ \t]*[0-9]+[ \t]*)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PRE" "\\<\\(E\\(?:\\(?:PS\\|RR\\|TA\\)([ \t]*[0-9]+[ \t]*)\\)\\|ICALL\\>\\|L[0-9]+\\>\\|N\\(?:\\(?:EWIND\\|ONMEM\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|THETA([ \t]*[0-9]+[ \t]*)\\|\\(?:[Y]\\|INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PK" "\\<\\(A\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|DOS\\(?:REC\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|TIM\\>\\)\\|E\\(?:TA([ \t]*[0-9]+[ \t]*)\\|VTREC\\>\\)\\|I\\(?:\\(?:CALL\\|II\\|\\(?:[0-9]+\\|[D]\\)\\)\\>\\)\\|N\\(?:\\(?:EWIND\\|ONMEM\\|POP\\|VNT\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|T\\(?:HETA([ \t]*[0-9]+[ \t]*)\\|\\(?:IM\\|STAT\\)E\\>\\)\\|\\(?:VI\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("MIX" "\\<\\(C\\(?:\\(?:ALL\\(?:FL\\)?\\|OMRES\\)\\>\\)\\|D\\(?:\\(?:ATA\\|O\\)\\>\\)\\|E\\(?:\\(?:NDDO\\|XIT\\)?\\>\\)\\|ICALL\\>\\|NONMEM\\>\\|PREDPP\\>\\|STDY\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|T\\(?:EMPLT\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:WHILE\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("INF" "\\<\\(\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("ERR" "\\<\\(A\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|COMRES\\>\\|E\\(?:PS([ \t]*[0-9]+[ \t]*)\\|RR\\(?:([ \t]*[0-9]+[ \t]*)\\|OR\\>\\)\\|TA([ \t]*[0-9]+[ \t]*)\\)\\|F\\>\\|ICALL\\>\\|L[0-9]+\\>\\|N\\(?:\\(?:EWIND\\|ONMEM\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|THETA([ \t]*[0-9]+[ \t]*)\\|\\(?:[Y]\\|INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("DES" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|D\\(?:\\(?:ES\\|OS\\(?:REC\\|TIM\\)?\\)\\>\\)\\|ISFINL\\>\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\|IM\\>\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("AES0" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("AES" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|DOS\\(?:\\(?:REC\\|TIM\\)?\\>\\)\\|ISFINL\\>\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\|IM\\>\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")

)
"* Right Handed Regular Expressions")
(defvar esn-abbrev-rhs-norec-6 '(
	("TOL" "\\<\\(NRD([ \t]*[0-9]+[ \t]*)\\)")
	("PRE" "\\<\\(E\\(?:\\(?:PS\\|RR\\|TA\\)([ \t]*[0-9]+[ \t]*)\\)\\|ICALL\\>\\|L[0-9]+\\>\\|N\\(?:\\(?:EWIND\\|ONMEM\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|THETA([ \t]*[0-9]+[ \t]*)\\|Y\\>\\)")
	("PK" "\\<\\(A\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|DOS\\(?:REC\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|TIM\\>\\)\\|E\\(?:TA([ \t]*[0-9]+[ \t]*)\\|VTREC\\>\\)\\|I\\(?:\\(?:CALL\\|II\\|\\(?:[0-9]+\\|[D]\\)\\)\\>\\)\\|N\\(?:\\(?:EWIND\\|ONMEM\\|POP\\|VNT\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|T\\(?:HETA([ \t]*[0-9]+[ \t]*)\\|\\(?:IM\\|STAT\\)E\\>\\)\\|VI\\>\\)")
	("MIX" "\\<\\(C\\(?:\\(?:ALL\\(?:FL\\)?\\|OMRES\\)\\>\\)\\|D\\(?:\\(?:ATA\\|O\\)\\>\\)\\|E\\(?:\\(?:NDDO\\|XIT\\)?\\>\\)\\|ICALL\\>\\|NONMEM\\>\\|PREDPP\\>\\|STDY\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|T\\(?:EMPLT\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\)\\|WHILE\\>\\)")
	("ERR" "\\<\\(A\\(?:\\>\\|([ \t]*[0-9]+[ \t]*)\\)\\|COMRES\\>\\|E\\(?:PS([ \t]*[0-9]+[ \t]*)\\|RR\\(?:([ \t]*[0-9]+[ \t]*)\\|OR\\>\\)\\|TA([ \t]*[0-9]+[ \t]*)\\)\\|F\\>\\|ICALL\\>\\|L[0-9]+\\>\\|N\\(?:\\(?:EWIND\\|ONMEM\\)\\>\\)\\|PRED\\(?:\\(?:PP\\)?\\>\\)\\|THETA([ \t]*[0-9]+[ \t]*)\\|Y\\>\\)")
	("DES" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|D\\(?:\\(?:ES\\|OS\\(?:REC\\|TIM\\)?\\)\\>\\)\\|ISFINL\\>\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\|IM\\>\\)\\)")
	("AES0" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\)\\)")
	("AES" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|DOS\\(?:\\(?:REC\\|TIM\\)?\\>\\)\\|ISFINL\\>\\|NONMEM\\>\\|P\\(?:([ \t]*[0-9]+[ \t]*)\\|REDPP\\>\\)\\|T\\(?:\\>\\|HETA([ \t]*[0-9]+[ \t]*)\\|IM\\>\\)\\)")

)
"* Right Handed Regular Expressions without records")
(defvar esn-abbrev-no-6 '(
	("TOL" "\\<\\(\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PRE" "\\<\\(A([ \t]*[0-9]+[ \t]*)\\|DA\\(?:DT([ \t]*[0-9]+[ \t]*)\\|TREC\\>\\)\\|E([ \t]*[0-9]+[ \t]*)\\|G\\>\\|H\\>\\|INDXS\\>\\|P([ \t]*[0-9]+[ \t]*)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("PK" "\\<\\(DADT([ \t]*[0-9]+[ \t]*)\\|E\\(?:([ \t]*[0-9]+[ \t]*)\\|PS([ \t]*[0-9]+[ \t]*)\\|VTREC\\>\\)\\|\\(?:GG\\|I\\(?:DEF\\|NDXS\\|R\\(?:EV\\|GG\\)\\)\\|N\\(?:ETAS\\|VNT\\)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("MIX" "\\<\\(\\(?:COM\\|E\\(?:PS\\|RR\\|TA\\)\\|NEWIND\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("INF" "\\<\\(\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("ERR" "\\<\\(DADT([ \t]*[0-9]+[ \t]*)\\|E\\(?:([ \t]*[0-9]+[ \t]*)\\|VTREC\\>\\)\\|G\\>\\|HH\\>\\|I\\(?:\\(?:DEF\\|NDXS\\|REV\\)\\>\\)\\|NVNT\\>\\|P([ \t]*[0-9]+[ \t]*)\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\>\\)")
	("DES" "\\<\\(D\\(?:[AP]\\>\\)\\|E\\(?:\\(?:PS\\|RR\\|TA\\)?([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:IR\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("AES0" "\\<\\(D\\(?:[APT]\\>\\)\\|E\\(?:\\(?:PS\\|RR\\|TA\\)?([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:IR\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")
	("AES" "\\<\\(D\\(?:[APT]\\>\\)\\|E\\(?:\\(?:PS\\|RR\\|TA\\)([ \t]*[0-9]+[ \t]*)\\)\\|\\(?:IR\\|\\(?:INC[A-Z0-9_]*\\|[$][A-Z][A-Z0-9_]*\\)\\)\\>\\)")

)
"* Forbidden Regular Expressions")

(provide 'esn-nm-cookies-6)
