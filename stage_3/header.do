/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

X - Header
 # 

Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/19/17

********************************************************************************
********************************************************************************/

set more off
set maxiter 1000, permanently

* global PROJECT_ROOT "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
global PROJECT_ROOT "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

adopath ++ `"$PROJECT_ROOT\ado"'

* First close any open log file
capture log close

/********************************************************************************
********************************************************************************/
