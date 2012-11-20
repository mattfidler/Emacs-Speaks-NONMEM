$PROBLEM test

;@ Data 24 (=24) @;
;@ Variables 14/20 @;
$INPUT C=DROP  TIME  ID         DV         CMT        RBBI=DROP  TO=DROP    DAY
       AMT     II    ADDL       STDY=DROP  HR=DROP    SBID=DROP  OCC        AGE
       ETHN    RACE  IRIS=DROP  SEX        FREQ=DROP  DSCN=DROP  HMAN=DROP  MAQ
       
$DATA data-f-h-m.csv IGNORE=C  ACCEPT=(CMT.EQ.5) WIDE
      
$PRED
  TVIC50 = THETA(1)
  IC50   = TVIC50*DEXP(ETA(1))
  
  GAM    = THETA(2)
  
  TR     = DLOG(THETA(3)/(1-THETA(3)))
  TVIMAX = DEXP(TR)/(1+DEXP(TR))
  IMAX   = DEXP(TR+ETA(2))/(1+DEXP(TR+ETA(2))) ; Logit
  
  F      = 1-IMAX*(MAQ**GAM)/(MAQ**GAM+IC50**GAM)
  
  IPRED  = F
  IRES   = DV-IPRED
  
  W      = THETA(4)
  DEL    = 0
  IF (W.EQ.0) THEN
    DEL = 1
  ENDIF
  
  IWRES  = IRES/(W+DEL)
  Y      = IPRED + EPS(1)*W
  IDV    = MAQ
  
$THETA (0, 1)  ;C THETA(1) - IC50
       (0, 1)  ;C THETA(2) -  Gamma
       (0, 1)  ;C THETA(3) - Imax
       (0, 1)  ;C THETA(4) - Additive Error
       
$OMEGA 1  ;C ETA(1) - IC50
       1  ;C ETA(2) - Imax
$SIGMA 1  FIXED
       

$ESTIMATION METHOD=COTAB999 NOLAPLACIAN MAXEVALS=9999 SIGDIGITS=3 NOABORT
            PRINT=1 MSFO=run999.nsf
            
$COV PRINT=E
     
;@ Pred Variables 16/20 @;
$TABLE DAY  ID  OCC  NOAPPEND  NOPRINT  ONEHEADER  FILE=catab999
$TABLE DAY  ID  OCC  NOAPPEND  NOPRINT  ONEHEADER  FILE=cotab999
$TABLE ETA1    ETA2  DAY       GAM      IC50       ID  IMAX  OCC  TR
       TVIC50  W     NOAPPEND  NOPRINT  ONEHEADER  FILE=patab999
$TABLE RES        WRES  DAY  ID  IRES  IWRES  OCC  NOAPPEND  NOPRINT
       ONEHEADER  FILE=sdtab999
