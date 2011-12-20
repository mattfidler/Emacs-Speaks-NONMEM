;; -*-no-byte-compile: t; -*-
(defvar esn-records-help-TAB-6 '(
		("OMITTED" "OMITTED\n The Table Step is not implemented.")
		("CONDITIONAL" "CONDITIONAL\n The Table Step is implemented only when the Estimation Step  terminates successfully or is not implemented.")
		("UNCONDITIONAL" "UNCONDITIONAL\n The Table Step is always implemented.  This is the default.")
		("NOAPPEND" "NOAPPEND\n Requests that items DV, PRED, RES, WRES not appear automatically. When  this  is specified, the number of labels (i.e., user-chosen item types) that may appear in the table can be as  large  as  12 (rather  than  8) for a printed table, and as large as 54 (rather than 50) for a table file. If items PRED, RES, and/or WRES are explicitly  coded  in  list1, then they appear in the table, exactly as listed.  (Previously to NONMEM VI 2.0, they could be  included  in  the  list,  but  were suppressed  from  the  portion of the table described by list1 in favor of the automatically-generated items.)")
		("APPEND" "APPEND\n Items DV, PRED, RES, WRES appear  automatically  as  the  last  4 columns of the table.  This is the default.")
		("FORWARD" "FORWARD\n Used only with the FILE option.  When a table file is opened during a given (sub)problem, it is forwarded to the end of the file. Moreover, if in the same (sub)problem the $TABLE record  is  followed  by  a  contiguous  succession of additional $TABLE records having the same filename as the given record,  then  even  though some  of  these additional records may have the NOFORWARD option, or have neither the NOFORWARD nor the FORWARD options,  the  FORWARD option will apply to each of the records in the succession.")
		("NOFORWARD" "NOFORWARD\n Used only with the FILE option.  When the table  file  is  opened during a given (sub)problem, it is positioned at the start of the file.   This is the default.  However, when  there  are  multiple $TABLE  records  within  the  same  problem  and  having the same filename, the situation is a little  more  complicated`  see  the text describing the FORWARD option.")
		("FIRSTONLY" "FIRSTONLY\n Only information corresponding to the first data record from each individual  record  appears in the table.  ")
		("FIRSTRECONLY" "FIRSTRECONLY\n Only information corresponding to the first data record from each individual  record  appears in the table.  ")
		("FIRSTRECORDONLY" "FIRSTRECORDONLY\n Only information corresponding to the first data record from each individual  record  appears in the table.  ")
		("ONEHEADER" "ONEHEADER\n Used only with the FILE option.  Only the first  900-record  segment of the table is preceded by header lines.")
		("NOHEADER" "NOHEADER\n Used only with the FILE option.  No header lines are included  in the table file.")
		("FILE=" "FILE=filename\n The table is written to the given file in character  form,  e.g., ASCII  or  EBCDIC,  according to the hardware platform.  Filename may not contain embedded spaces.  If it  contains  commas,  semicolons,  or  parentheses,  then  it  must be surrounded by single quotes ' or double quotes \".  Filename  may  also  contain  equal signs if it is enclosed in quotes. Filename may contain at most 71 characters. If filename is the same as any option of the  $TABLE  record,  it must  be  enclosed in quotes.  Filename can differ between $TABLE records. Default: No table file is output.  Required with NOPRINT.")
		("NOPRINT" "NOPRINT\n No printed table appears in the NONMEM output.")
		("PRINT" "PRINT\n A printed table appears  in  the  NONMEM  output.   This  is  the default.")
)

"* $TABLE help for NONMEM 6")

(provide 'esn-records-help-TAB-6)

(provide 'esn-nm-cookies-help-TAB-6)
