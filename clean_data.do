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

if `c(os)' == "Windows" {
	global data_dir = ""
} 
else {
	global data_dir = ""
}

********************************************************
** Bring in Data
use "$data_dir/ifjsifjs.dta", clear


********************************************************
** Drop extra variables
// Drop time variables -- not needed
drop *time* *min*

// Drop something else


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



********************************************************
** Convert variables to categorical indicators
// 


********************************************************
** Rename variables intelligibly
// Rename signs and symptoms variables to ss_*


// Rename treatment variables to tr_*


// Rename testing variables to te_*


// Rename diagnosis variables to dx_*


// Rename outcome variable to death



********************************************************
** Output data

