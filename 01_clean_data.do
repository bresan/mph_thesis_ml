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
// Drop ID and time variables -- not needed, time variables are missing
	drop InPatientNo TimeAdmission *hr* *min* *am_pm* 

// Drop ID variables 
	drop VisitID parishID villageID LabtestNumber *clinician*

// Drop indicators with observed missingness
	drop immunization pulse1 BP Respirations oxygen height MUAC sicklecell Rbloodsugar otherspecify* 

// Drop other variables not needed for anaysis
	drop Death1 Death2 Disability InpatientNo *blood* reason_notgiven Other Date* labtestmissing 

	foreach var of varlist date* {
		if "`var'" != "dateadmit" drop `var' 
	}
	
	// What is Pulse2? caprefil?
	drop Pulse2 caprefil BCS NoHBwhy BloodGroup BldRH  // Pulse2 13,277 non-missing, caprefil 38,654 non-missing, BCS (Blantyre Coma Score) 27,207 non-missing, BloodGroup 14,542 non-missing, BldRH 11,510 non-missing
	
// Drop unusable variables (red on variable sheet?)
	drop Prostration Coma Respiratorydistress AbnormalBleeding Jaudice Haemoglobinuria Convulsions3more circulatorycollapse Pulmonaryoedema ///
		Acidosis RenalImpairment Hyperlactataemia Hyperparasitemia vomitingeverything Convulsion2less Inabilitytodrinkorbreastfeed lethargicorunconscious /// 
		coomorbidity CantaccessACTs ACToutofstock unabletoreturn moderatelysick parentalrequest  


********************************************************
** Recode variables with multiple possibilities
// For  Diagnoses, generate a variable for "if diagnosed at admission, irrespective of order". 
// Do the same for Final diagnoses
	qui { 
		levelsof AdmissionDiag1, local(diags) c
		gen dx_match = 0 // Was the primary final diagnosis included in any of the admission diagnoses?
		foreach diag in `diags' {
			gen dx_admit_`diag' = 0
			gen dx_final_`diag' = 0
			forvalues i = 1/10 {
				replace dx_admit_`diag' = 1 if AdmissionDiag`i' == 1
				replace dx_match = 1 if AdmissionDiag`i' == FinalDiag1
			}
			forvalues i = 1/6 {
				replace dx_final_`diag' = 1 if FinalDiag`i' == 1
			}
		}
		// Do we recode diag_match here for 77/88/99 (diagnosis not clear, missing, or other)?
	}
	
	// How do the diagnoses stack up against death?
	tab AdmissionDiag1 anydeath, row
	tab FinalDiag1 anydeath, row
	tab dx_match anydeath, row
	
// Keep only the primary diagnosis at admission and discharge -- since ordering probably matters less after the first
	forvalues i = 2/10 {
		drop AdmissionDiag`i'
	}
	forvalues i = 2/6 {
		drop FinalDiag`i'
	}

	
// What treatment was given in the hospital? Was it given at admission or not?
// Treathosp1/8, TreatAdm1/11, Drugprescribed

// Convert prescription date variable to days since admission variables
// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?

// Use date* variable instead of Prescriptiondate variable to get the date that the medicine was administered, rather than prescribed? 
// date* has more missingness than Prescribed -- due to lack of administration, or something else?
	forvalues i = 1/15 {
		gen p_lag_`i' = date(Prescriptiondate`i',"YMD###") - dateadmit
		replace p_lag_`i' = 0 if p_lag_`i' == .
		replace p_lag_`i' = 0 if p_lag_`i' < 0 // Fair assumption to make about those with neg values?
		drop Prescriptiondate`i'
	}

qui { 
	levelsof TreatmentAdm1, local(treatments) c
	foreach treat in `treatments' {
		gen tr_admit_`treat' = 0
		gen tr_hosp_`treat' = 0
		forvalues i = 1/11 {
			replace tr_admit_`treat' = 1 if TreatmentAdm`i' == `treat' 
		}
		forvalues i = 1/10 {
			replace tr_admit_`treat' = 1 if drugprescribed`i' == `treat' & p_lag_`i' == 0 // If the treatment was given the same day as admission/ consider it treatment at admission
			replace tr_hosp_`treat' = 1 if drugprescribed`i' == `treat' & p_lag_`i' > 0 // Otherwise, consider it in-hospital treatment
		}
		forvalues i = 1/8 {
			replace tr_hosp_`treat' = 1 if Treathosp`i' == `treat'
		}
	}
}

drop p_lag_* Treathosp* drugprescribed* TreatmentAdm* 

// Results of BS and/or RDT? and/or Hb6? Or just use labtestresult variable for the test result variable?


********************************************************
** Standardize variable formatting/labels
// Encode/decode certain variables
	decode SiteID, gen(site_name)
	
// Fix ages -- variable age is in months, along with significant clumping to major age values
// How to solve this clumping?
// Other age variables available -- year, month, day
	gen age_new = (AgeYrs * 365 + AgeMths* 30.5 + AgeDays) / 30.5 // Age in months
	sum age
	sum age_new
	drop age

// Drop all recorded cases over 5 years of age
	keep if age < 60
	// kdensity age
	// kdensity age, n(87137) gen(test1 test2) bwidth(6)
	// replace test1 = 0 if test1 < 0

********************************************************
** Recode missing observations to a standard format

// Recode all 9 and 9999 values to missing (treat do not know as missing)
	preserve
	describe, replace clear
	levelsof name if isnumeric == 1, local(num_vars) c
	restore

	qui {
		foreach var in `num_vars' {
			replace `var' = . if inlist(`var',99,9999)
			replace `var' = . if `var' == 9 & !inlist("`var'","Temp","Weight","MUAC","AdmissionDiag1","BS1admit","RDTadmit","HIV") 
		}
	}
	
// Drop all variables with over half missing values (don't want to even consider imputing them)
/*
foreach var of varlist `num_vars' {
	qui count if `var' == . 
	if `r(N)' > 50000 drop `var' 
}
*/

********************************************************
** Convert binary variables from numeric to categorical indicators
	// Need to figure out how to subset for binary variables only
	/*
	preserve
	keep `num_vars'
	collapse (max) `num_vars'
	local binary_vars = ""
	foreach var of varlist * {
		qui count if `var' > 1 & `var' != .
		if `r(N)' == 0 local binary_vars = "`binary_vars' `var'"
	}
	restore
	
	label define binary 1 "Yes" 0 "No"
	label values `binary_vars' binary
	foreach var in `binary_vars' {
		decode `var', gen(str_var)
		drop `var'
		rename str_var `var'
	}
	*/
	
// Turn all variables to lowercase
	rename *,lower

********************************************************
** Output preliminary variable list for use in variable pruning
	cd "$data_dir"
	preserve
	describe, replace clear
	export delimited using cleaned_vars.csv, delimit(",") replace
	restore


********************************************************
** Rename variables intelligibly
// Make variable labels a bit more self-explanatory
	rename (bs1admit rdtadmit hblevel admissiondiag1 finaldiag1) (bs_admit rdt_admit hb_level dx_1_admission dx_1_final)

// Rename signs and symptoms variables to ss_*
	local ss_vars = "fever cough cough2weeks diffbreath convulsions altconsciousness vomiting unabledrink diarrhea diarrhea2wks bldydiarrhea teaurine"
	local ss_vars = "`ss_vars' temp weight pallor severe_wasting sunken_eyes skin_pinch edema jaundice dpbreath flarnostril icrecession subcostal airway wheezing"
	local ss_vars = "`ss_vars' crackles unconscious lethargy unablesit bgfontanelle unablepain stiffneck kerning" 

// Rename treatment variables to tr_*
// No need to recode tr_admit and tr_hosp variables because they're already correctly formatted
	local tr_vars = ""

// Rename testing variables to te_*
	local te_vars = "bs_admit rdt_admit hb_desired hb_done hb_level"
	local te_vars = "`te_vars'"

// Rename diagnosis variables to dx_*
// No need to recode because they're already correctly formatted
	local dx_vars = ""

// Rename covariates to cv_*
	local cv_vars = "readmission gender days " // Is days LOS? 
	local cv_vars = "`cv_vars'"

// Enact renames
	foreach vartype in ss te cv {
		foreach var in ``vartype'_vars' {
			rename `var' `vartype'_`var'
		}
	}

// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?


// Rename outcome variable to death
rename anydeath death


********************************************************
** Output data

cd "$data_dir"
export delimited using "cleaned_data.csv", delimit(",") replace
