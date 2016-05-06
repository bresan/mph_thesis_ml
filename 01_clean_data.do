/* 
Author: Grant Nguyen
Purpose: Clean raw PRISM data to prepare for imputation and finally analysis
	1. Drop extraneous variables
	2. Standardize variable formatting
	3. Recode missing observations in a standard format
	4. Convert variables to categorical indicators
	5. Rename variables intelligibly
	6. Output data
*/

********************************************************
** Set locals etc.
clear all
set more off

if "`c(os)'" == "Windows" {
	global data_dir = "H:/Thesis/data"
} 
else {
	global data_dir = "/Users/Grant/Desktop/Thesis"
}

********************************************************
** Bring in Data
use "$data_dir/IP data base Nov 2015_Grant Nguyen_Version 2013.dta", clear


********************************************************
** Drop extra variables
// Convert prescription date variable to days since admission variables
// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?

forvalues i = 1/10 {
	gen p_lag_`i' = date(Prescriptiondate`i',"YMD###") - dateadmit
	replace p_lag_`i' = 0 if p_lag_`i' == .
	// replace p_lag_`i' = 0 if p_lag_`i' < 0 // Fair assumption to make about those with neg values?
}




// Drop ID and time variables -- not needed, time variables are missing
drop InPatientNo TimeAdmission *hr* *min* *am_pm* date* // If needed, calculate the days of admission before dropping here

// Drop ID variables 
drop *ID* LabtestNumber *clinician*

// Drop indicators with observed missingness
drop immunization pulse1 BP Respirations oxygen height MUAC sicklecell Rbloodsugar otherspecify*

// Drop other variables not needed for anaysis
drop Death1 Death2 Disability


********************************************************
** Recode variables with multiple possibilities
// For  Diagnoses, generate a variable for "if diagnosed at admission, irrespective of order". 
// Do the same for Final diagnoses
	qui { 
		levelsof AdmissionDiag1, local(diags) c
		foreach diag in `diags' {
			gen admit_diag_`diag' = 0
			gen final_diag_`diag' = 0
			forvalues i = 1/10 {
				replace admit_diag_`diag' = 1 if AdmissionDiag`i' == 1
			}
			forvalues i = 1/6 {
				replace final_diag_`diag' = 1 if FinalDiag`i' == 1
			}
		}
	}
// Keep only the primary diagnosis at admission and end -- since ordering probably matters less after the first
	forvalues i = 2/10 {
		drop AdmissionDiag`i'
	}
	forvalues i = 2/6 {
		drop FinalDiag`i'
	}


********************************************************
** Standardize variable formatting/labels
// Encode/decode certain variables


// Standardize yes/nos and other categorizations


// Drop all variable and value labels


********************************************************
** Recode missing observations to a standard format
// Get list of numeric variables
preserve
describe, replace clear
levelsof _____ if type == "numeric" 
restore

// Recode all 9 and 9999 values to missing (treat do not know as missing)
foreach var of varlist `num_vars' {
	replace `var' = . if inlist(`var',9,9999)
}

// For certain variables, it may be ok to record missing variables as 0s
// If the space for drug prescription #5 is missing, but it isn't for #1, we can probably assume that drug #5 was probably not prescribed at all
forvalues i = 2/5 {
	replace drugprescribed`i' = 0 if drugprescribed`i' == . & drugprescribed1 != . 
}

// Drop all variables with over half missing values (don't want to even consider imputing them)
foreach var of varlist `num_vars' {
	qui count if `var' == . 
	if `r(N)' > 50000 drop `var' 
}

********************************************************
** Convert variables from numeric to categorical indicators
// 


********************************************************
** Rename variables intelligibly
// Rename signs and symptoms variables to ss_*
local ss_vars = "a b c d "
local ss_vars = "`ss_vars' e f g h"


// Rename treatment variables to tr_*


// Rename testing variables to te_*


// Rename diagnosis variables to dx_*


// Rename covariates to cv_*
// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?


// Rename outcome variable to death



********************************************************
** Output data

cd "$data_dir"
export delimited using "cleaned_data.csv", delimit(",") replace
