;;; esn-test.el --- Tests for functions
;; 
;; Filename: esn-test.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue Sep 21 12:37:54 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 18:58:30 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 142
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;(defalias 'e 'esn-sub-test)
(defun esn-sub-test ()
  (interactive)
  (let ((debug-on-error 't)
        (esn-update-sub-small nil))
    (with-temp-buffer
      (insert "$PRO ok\n$SUB ADVAN1 TRANS1\n")
      (message "%s" (esn-desc-subroutines)))))

;;(defalias 'e 'esn-yas-esn-snippet-test)
(defun esn-yas-esn-snippet-test ()
  (interactive)
  (message (esn-yas-esn-snippet "$SUBROUTINES ADVAN1 TRANS2
$PK

  TVCL =THETA(Volume/Time)

  TVV  = THETA(Volume)

  $0

  S1   = SCALE")))

;;(defalias 'e 'esn-yas-current-theta-test)
(defun esn-yas-current-theta-test ()
  (interactive)
  (let ((esn-yas-last-theta 1)
        (esn-yas-added-2-thetas '(1 3 4)))
    (message "1:%s 2:%s 3:%s 4:%s 5:%s 6:%s"
             (esn-yas-current-theta 1)
             (esn-yas-current-theta 2)
             (esn-yas-current-theta 3)
             (esn-yas-current-theta 4)
             (esn-yas-current-theta 5)
             (esn-yas-current-theta 6))))

;;(defalias 'e 'esn-yas-par-test)
(defun esn-yas-par-test ()
  (interactive)
  (let ((debug-on-error 't)
        (esn-yas-last-theta 1)
        (esn-yas-mu-referencing-type 1))
    (message "%s" (esn-yas-par 1 "TVCL" "Volume/Time" "0")))
  )
;;(defalias 'e 'esn-yas-eta-types-transform-test)
(defun esn-yas-eta-types-transform-test ()
  (interactive)
  (let (
        (debug-on-error 't)
        (esn-yas-last-tv "TVCL")
        (esn-yas-last-theta 1)
        (esn-yas-last-theta-value 1)
        )
    (message "%s" (esn-yas-eta-types-transform "Log-normal"))
    (message "%s" (esn-yas-eta-types-transform "Additive"))
    (message "%s" (esn-yas-eta-types-transform "Proportional"))
    (message "%s" (esn-yas-eta-types-transform "No Between Subject Error"))
    (message "%s" (esn-yas-eta-types-transform "Logit"))
    (message "%s" (esn-yas-eta-types-transform "Constrained Log-normal (Theta > C)"))))

;;(defalias 'e 'esn-yas-pending-units-test)
(defun esn-yas-pending-units-test ()
  (interactive)
  (let (
        (debug-on-error 't)
        (esn-yas-pending-thetas '((2 "TVV+ETA(2) ; Additive" "Volume" "0")
                                  (1 "TVCL*DEXP(ETA(1) ; Lognormal" "Volume/Time" "0")))
        (esn-yas-pending-omegas '((2 "TVV+ETA(2) ; Additive" "Volume" "0")
                                  (1 "TVCL*DEXP(ETA(1) ; Lognormal" "Volume/Time" "0"))))                         
    (message "%s" (esn-yas-fix-pending-units "V/1000            ; Dose: mg; Volume: L; Conc: ng/mL"))
    (message "%s,%s" esn-yas-pending-thetas esn-yas-pending-omegas)))

(defun esn-yas-mu-trf-test (&optional test)
  "* Tests esn-yas-mu-trf function"
  (interactive)
  (let ((esn-yas-mu-referencing-type (or test 1)) 
        (debug-on-error 't))
    (message "%s,Additive,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) ; Additive" nil "TVCL"))
    (message "%s,Additive,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) " nil "TVCL"))
    
    (message "%s,Log-normal,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal " nil "TVCL"))
    (message "%s,Log-normal,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1))" nil "TVCL"))
    
    (message "%s,Log-normal,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal
  MU_1 = DLOG(TVCL)" nil "TVCL"))
    
    
    (message "%s,,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "" nil "TVCL"))
    (message "%s, ,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf " " nil "TVCL"))
    
    (message "================================================================================")
    (message "%s,Additive,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) ; Additive" 1 1))
    (message "%s,Additive,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) " 1 1))
    
    (message "%s,Log-normal,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal " 1 1))
    (message "%s,Log-normal,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1))" 1 1))
    
    (message "%s,Log-normal,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal
  MU_1 = DLOG(TVCL)" 1 1))
    
    
    (message "%s,,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "" 1 1))
    (message "%s, ,1:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf " " 1 1))
    
    (message "================================================================================")
    (message "%s,Additive,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) ; Additive" 2 1))
    (message "%s,Additive,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL+ETA(1) " 2 1))
    
    
    (message "%s,Log-normal,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal " 2 1))
    (message "%s,Log-normal,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1))" 2 1))
    
    (message "%s,Log-normal,nil:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "TVCL*DEXP(ETA(1)) ; Log-normal
  MU_1 = DLOG(TVCL)" 2 1))
    
    (message "%s,,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf "" 2 1))
    (message "%s, ,2:%s" esn-yas-mu-referencing-type  (esn-yas-mu-trf " " 2 1))
    
    (message "================================================================================")
    (unless test
      (esn-yas-mu-trf-test 2)
      (esn-yas-mu-trf-test 3))))
;;(defalias 'e 'esn-test-plt-timestamp-function)
(defun esn-test-plt-timestamp-function ()
  "* Finds timestamps associated with this file"
  (interactive)
  (esn-message "Timestamps:%s" (esn-plt-find-timestamps)))

(defalias 'e 'esn-yas-test)
(defun esn-yas-test ()
  (interactive)
  (when (fboundp 'yas/read-and-eval-string)
    (yas/read-and-eval-string "(esn-yas-par 2 \"TVCL\" \"Volume/Time\")"))
  (when (fboundp 'yas/eval-lisp)
    (yas/eval-lisp "(esn-yas-par 2 \"TVKA\" \"1/Time\")")
    (yas/eval-lisp "(esn-yas-par 2 \"TVCL\" \"Volume/Time\")")))

;;(defalias 'e 'esn-iov-get-occ-test)
(defun esn-iov-get-occ-test ()
  (interactive)
  (message "%s" (esn-iov-get-occ)))

                                        ;(defalias 'e 'esn-iov-number-test)
(defun esn-iov-number-test ()
  (interactive)
  (message "%s" (esn-iov-number 1)))

;; Now test esn-use-pirana

;;(defalias 'e 'esn-test-use-pirana)
(defun esn-test-use-pirana ()
  (interactive)
  (let (esn-use-pirana-saved)
    (message "Use Pirana: %s" (esn-use-plt-p))))

(defalias 'e 'esn-test-templates)
(defun esn-test-templates ()
  (interactive)
  (let ((debug-on-error t) (debug-on-quit t))
    (message "%s" (esn-parse-template "######################################################################
# PLTTools Graphics Script 
# Program Version 3.3.1 build 2449 EsN $esn-mode-ver
# Saved %Y-%m-%d %H:%M:%S
# Path $(if (not (buffer-file-name)) \"\" (file-name-directory (buffer-file-name)))
######################################################################
# INITIAL SETUP
####################
# Calculate Half Lives
####################
CalculateHalfLives       <- c()
NumberOfCompartments     <- c()
####################
# THETAS
####################
THETANUMBER      <- $(esn-R-seq (esn-max-theta) t)
PARAMNONMEM      <- c()
HALFLIVES        <- c()
PARAMNAMES       <- c()
THETASCREEN      <- c()
####################
# ETAS
####################
ETANUMBERS       <- c()
ETAPARAMS        <- c()
EXPETA   <- c()
USECORRESPONDING         <- c()
####################
# SIGMAS
####################
SIGMANUMBER      <- c()
SIGMANAME        <- c()
####################
# COVARIATES
####################
COVARColNames    <- c()
COVARNONMEM      <- c()
COVARDescriptors         <- c()
COVARType        <- c()
COVARGLM  <- c()
####################
# DemoFile% %
####################
DEMOEXISTS       <- c()
IDALIAS  <- c()
DEMOFILENAME     <- c()
DEMODELIMITER    <- c()
####################
# Select Graphics
####################
BySubjectGraphics        <- c()
SpaghettiGraphics        <- c()
ETAdomain        <- c()
expETAdomain     <- c()
PostHocDomain    <- c()
ETAvsETA         <- c()
LogLogCp         <- c()
LinearLinearCp   <- c()
ResidualsCp      <- c()
####################
# Additional Options
####################
CalculateStatistics      <- c()
DisplayOutlierIDs        <- c()
StratifyCovariate        <- c()
####################
# Graphics Options
####################
BlackWhite       <- c()
GlobalHeader     <- c()
####################
# Units
####################
AMTunit  <- c()
TIMEUnitsTABLE   <- c()
TIMEUnitsGRAPHICS        <- c()
####################
# Scaling
####################
IDCorrection     <- c()
DVconversion     <- c()
AMTconversion    <- c()
####################
# Number of Dependent Variables
####################
NumberofDependentVariables       <- c()
####################
# Single DV
####################
LOQ      <- c()
DVUnits  <- c()
####################
# Multiple DVs (optional)
####################
IndicatorColumn  <- c()
FlagValues       <- c()
DVNames  <- c()
LOQValues        <- c()
MultipleDVUnits  <- c()
####################
# By-Subject Graphics
####################
BySubject01      <- list()
####################
# Options
####################
####################
# Options
####################
BySubjectRows    <- c()
DosesTextField   <- c()
LabelDoseOptions         <- c()
####################
# Race (optional)
####################
RaceCodes        <- c()
RaceText         <- c()
####################
# Gender (optional)
####################
GenderCodes      <- c()
GenderText       <- c()
####################
# By-Subject Grouping Variable
####################
BySubjectGroupCodes      <- c()
BySubjectGroupText       <- c()
####################
# By-Subject Grouping
####################
BySubjectGroupVariable   <- c()
BySubjectGroupLabel      <- c()
####################
# By-Subject MultiplePeriods
####################
BySubjectPeriodVariable  <- c()
BySubjectPeriodLabel     <- c()
####################
# Spaghetti Graphics
####################
Spaghetti01      <- list()
####################
# Display Formulation (optional)
####################
FormulationStatusName    <- c()
DisplayFormulationCodes  <- c()
DisplayFormulationText   <- c()
####################
# Display Fed Status (optional)
####################
FedStatusName    <- c()
DisplayFedCodes  <- c()
DisplayFedText   <- c()
####################
# Spaghetti Grouping
####################
SpaghettiGroupVariable   <- c()
SpaghettiGroupLabel      <- c()
####################
# Spaghetti Grouping Variable
####################
SpaghettiGroupCodes      <- c()
SpaghettiGroupText       <- c()
####################
# Spaghetti MultiplePeriods
####################
SpaghettiPeriodVariable  <- c()
SpaghettiPeriodLabel     <- c()
####################
# Grouping 3
####################
Group3Variable   <- c()
Group3Label      <- c()
####################
# Grouping Variable 3
####################
Group3Codes      <- c()
Group3Text       <- c()
####################
# MultiplePeriods 3
####################
Group3PeriodVariable     <- c()
Group3PeriodLabel        <- c()
########################################
# PLTsoft END
########################################
"))
    ))
;;;###autoload
(defun esn-test ()
  "Test Specific Issues that have come up."
  (interactive)
  (message "Issue #1: %s" (esn-test-1)))

(defvar esn-test-control-3
  "$PROBLEM  THEOPHYLLINE   SINGLE SUBJECT DATA
$INPUT  DOSE=AMT TIME CP=DV
$DATA  DATA3
$SUBROUTINES  ADVAN2

$PK
CALLFL=1
KA=THETA(1)
K=THETA(2)
SC=THETA(3)

$ERROR
Y=F+ERR(1)

$THETA  (0,1.7)  (0,.102)  (0,29)

$ESTIMATION  MAXEVAL=240  PRINT=2
$COVR
$TABLE TIME
$SCAT    CP VS TIME
$SCAT    PRED VS TIME
$SCAT    RES VS TIME
$SCAT    PRED VS CP  UNIT
")

(defvar esn-test-control-4
  "$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      ID DOSE TIME CP=DV WT
$DATA       THEO

$PRED
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;DOSE=WT-ADJUSTED DOSE (MG/KG)
;DS=NON-WT-ADJUSTED DOSE (MG)
   IF (DOSE.NE.0) THEN
      DS=DOSE*WT
      W=WT
   ENDIF
   KA=THETA(1)+ETA(1)
   KE=THETA(2)+ETA(2)
   CL=THETA(3)*W+ETA(3)
   D=EXP(-KE*TIME)-EXP(-KA*TIME)
   E=CL*(KA-KE)
   F=DS*KE*KA/E*D
   Y=F+EPS(1)

$THETA  (.1,3,5) (.008,.08,.5) (.004,.04,.9)
$OMEGA BLOCK(3)  6 .005 .0002 .3 .006 .4
$SIGMA  .4

$EST     MAXEVAL=450  PRINT=5
$COV
$TABLE          ID DOSE WT TIME
$SCAT           (RES WRES) VS TIME BY ID
")

(defvar esn-test-control-5
  "$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      ID DOSE=AMT TIME CP=DV WT
$DATA       THEOPP

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED
   CALLFL=1
   KA=THETA(1)+ETA(1)
   K=THETA(2)+ETA(2)
   CL=THETA(3)*WT+ETA(3)
   SC=CL/K/WT

$THETA  (.1,3,5) (.008,.08,.5) (.004,.04,.9)
$OMEGA BLOCK(3)  6 .005 .0002 .3 .006 .4

$ERROR
   Y=F+EPS(1)

$SIGMA  .4

$EST     MAXEVAL=450  PRINT=5
$COV
$TABLE          ID DOSE WT TIME
$SCAT           (RES WRES) VS TIME BY ID
")



(defun esn-test-1 ()
  "Tests Issue #1"
  (let ((esn-var-names '())
        var-names ret)
    (with-temp-buffer
      (insert "$OMEGA BLOCK(2)
0.1 ; 1 – E_V1
0.2 0.3 ; 2 – E_V2

$OMEGA
0.3 ; 3 – E_V3
")
      (setq var-names (esn-get-variable-names "OME")))
    (setq ret (equal var-names '("E_V1" "E_V2" "E_V3")))
    (message "%s = (E_V1 E_V2 E_V3); %s" var-names ret)))

(provide 'esn-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-test.el ends here
