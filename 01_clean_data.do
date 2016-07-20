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
// First, bring in inpatient data dictionary, to use in assigning possible treatments and diagnoses
	import excel using "$data_dir/inpatient data dictionary 1 Jan 14.xlsx", clear firstrow sheet("Diagnosis")
	rename (code ADMISSIONDIAGNOSIS) (diag_code diagnosis)
	tempfile diagnoses
	save `diagnoses'
	
	import delimited using "$data_dir/treatment_map.csv", clear
	rename code tr_code
	levelsof parent_drug, local(treatment_bins) c
	tempfile treatment
	save `treatment'

// Now, bring in the actual dataset and save a tempfile for easy interactive reference
	use "$data_dir/IP data base Nov 2015_Grant Nguyen_Version 2013.dta", clear
	tempfile master
	save `master'


********************************************************
** Drop extra variables
// Drop ID and time variables -- not needed, time variables are missing
	drop InPatientNo TimeAdmission *hr* *min* *am_pm* sqldate

// Drop ID variables 
	drop VisitID parishID villageID LabtestNumber *clinician*

// Drop indicators with observed missingness
	drop immunization pulse1 BP Respirations oxygen height MUAC sicklecell Rbloodsugar otherspecify* 

// Drop other variables not needed for analysis
	drop Death1 Death2 Disability InpatientNo *blood* reason_notgiven Other labtestmissing  
	
	// What is Pulse2? caprefil?
	drop Pulse2 caprefil BCS NoHBwhy BloodGroup BldRH  // Pulse2 13,277 non-missing, caprefil 38,654 non-missing, BCS (Blantyre Coma Score) 27,207 non-missing, BloodGroup 14,542 non-missing, BldRH 11,510 non-missing
	
	// Other extra date variables
	drop weekyear 
	
// Drop unusable variables (red on variable sheet -- only done after 2012 or 2013)
	drop Prostration Coma Respiratorydistress AbnormalBleeding Jaudice Haemoglobinuria Convulsions3more circulatorycollapse Pulmonaryoedema ///
		Acidosis RenalImpairment Hyperlactataemia Hyperparasitemia vomitingeverything Convulsion2less Inabilitytodrinkorbreastfeed lethargicorunconscious /// 
		coomorbidity CantaccessACTs ACToutofstock unabletoreturn moderatelysick parentalrequest

// Remove additional variables with significant systematic missingness prior to or after 2012 or 2013
	drop HB_desired HB_done skin_pinch Edema severe_wasting sunken_eyes Unablepain ///
	Lethargy Severeanaemia Hypoglycemia Rhonchi


********************************************************
** Recode variables with multiple possibilities
// For  Diagnoses, generate a variable for "if diagnosed at admission, irrespective of order". 
// Do the same for Final diagnoses
	qui { 
		levelsof AdmissionDiag1, local(diags) c
		gen dx_match = 0 // Are there any matching diagnoses between the diagnosis at admission and death?
		foreach diag in `diags' {
			gen dx_admit_`diag' = 0
			gen dx_final_`diag' = 0
			forvalues i = 1/10 {
				replace dx_admit_`diag' = 1 if AdmissionDiag`i' == `diag'
			}
			forvalues i = 1/6 {
				replace dx_final_`diag' = 1 if FinalDiag`i' == `diag'
			}
			replace dx_match = dx_match + 1 if dx_admit_`diag' == 1 & dx_final_`diag' == 1
		}
		
		egen dx_admit_count = rowtotal(dx_admit_*)
		egen dx_final_count = rowtotal(dx_final_*)
		// Calculate the Jacquard distance between admission and final diagnoses -- essentially,
		// Number of matching diagnoses divided by number of diagnoses present only in admission or final
		gen dx_match_dist = dx_match / (dx_admit_count + dx_final_count - dx_match)
		replace dx_match_dist = 0 if dx_match_dist == . // If the denominator is 0, this means that there were no diagnoses at admission or discharge
		sum dx_match_dist
		
		// Do we recode diag_match here for 77/88/99 (diagnosis not clear, missing, or other)?
	}
	
	drop AdmissionDiag* FinalDiag*
	
// Keep only the top 20 diagnoses for admission and final, since they make up a significant portion of the total diagnoses given
	preserve
	gen total_admissions = 1
	drop *count
	collapse (sum) total_admissions dx_admit* dx_final*
	reshape long dx_, i(total_admissions) j(diag_var) string
	rename dx_ dx_admissions
	gen admit_type = "admit" if regexm(diag_var,"admit")
	replace admit_type = "final" if regexm(diag_var,"final")
	gsort admit_type -dx_admissions
	by admit_type: gen diag_order = _n 
	replace diag_var = "dx_" + diag_var
	levelsof diag_var if diag_order > 20, local(drop_vars) c // If it's not one of the top-20 diagnoses, drop it
	levelsof diag_var if diag_order <= 10 & admit_type =="admit", local(admit_10) c
	levelsof diag_var if diag_order <= 10 & admit_type =="final", local(final_10) c
	gen diag_rate = dx_admissions/total_admissions
	keep diag_var dx_admissions total_admissions diag_rate diag_order
	order diag_var dx_admissions total_admissions diag_rate diag_order
	export delimited using "$data_dir/01_diagnosis_counts.csv", delimit(",") replace
	restore 
	
	// Make an indicator variable if one of the top 10 variables is "missed" in the initial diagnosis stage, or if there is a mis-diagnosis in the original
	local admit_10 = subinstr("`admit_10'","dx_admit_","",.)
	local final_10 = subinstr("`final_10'","dx_final_","",.)
	
	foreach var in `admit_10' {
		gen dx_misdiag_`var' = 0
		replace dx_misdiag_`var' = 1 if dx_admit_`var' == 1 & dx_final_`var' == 0
	}
	
	foreach var in `final_10' {
		gen dx_admiss_`var' = 0
		replace dx_admiss_`var' = 1 if dx_admit_`var' == 0 & dx_final_`var' == 1
	}
	
	drop `drop_vars'
	
	
// What treatment was given in the hospital? Was it given at admission or not?
// Treathosp1/8, TreatAdm1/11, Drugprescribed

// Convert prescription date variable to days since admission variables
// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?

// Use Prescriptiondate variable to get the date that the medicine was prescribed 
// There is also a date* variable, for date of drug administration, but that has much more missingness

// Test out generating a variable seeing if (if there was a positive malaria test), 
// how long treatment was delayed after the positive test
// Test dates only have 10K non-missing, how/why?
	gen date_firstpositive = . // Date of first positive malaria test
	gen date_first_tr_mal = . // Date of first treatment for malaria
	forvalues i = 1/6 {
		replace date_firstpositive = date(Date`i',"YMD###") if (BS`i' == 1 | RDT`i' == 1) & date_firstpositive == .
	}

	forvalues i = 1/15 {
		gen p_lag_`i' = date(Prescriptiondate`i',"YMD###") - dateadmit
		replace p_lag_`i' = 0 if p_lag_`i' == .
		replace p_lag_`i' = 0 if p_lag_`i' < 0 // If date is negative, we assume it to be at admission
		replace p_lag_`i' = 0 if p_lag_`i' == 365 // Year miscoding: If a year away, we assume it to be at date of admission and treat it as such
		replace date_first_tr_mal = date(date1`i',"YMD###") if date1`i' != "" & (date_first_tr_mal == . | date_first_tr_mal < date(date1`i',"YMD###")) & inlist(drugprescribed`i',1,2,3) // Get list of antimalarials to plug in here
		drop Prescriptiondate`i'
	}
	gen lag_tr_mal = date_first_tr_mal - date_firstpositive
	sum lag_tr_mal
	drop lag_tr_mal // seems like there may not be enough coverage of testing dates for this to work -- although we still need to test with true antimalarial list!

qui { 
	// levelsof TreatmentAdm1, local(treatments) c
	foreach treat_bin in `treatment_bins' {
		di "Processing `treat_bin'"
		preserve
		// Grab the numeric code for all specific treatments within the broader treatment bins
		use `treatment' if parent_drug == "`treat_bin'", clear
		levelsof tr_code, local(treatment_codes) c
		restore
		
		gen tr_admit_`treat_bin' = 0
		gen tr_hosp_`treat_bin' = 0
		
		foreach treat in `treatment_codes' {
			forvalues i = 1/11 {
				replace tr_admit_`treat_bin' = 1 if TreatmentAdm`i' == `treat' 
			}
			forvalues i = 1/15 {
				replace tr_admit_`treat_bin' = 1 if drugprescribed`i' == `treat' & p_lag_`i' == 0 // If the treatment was given the same day as admission/ consider it treatment at admission
				replace tr_hosp_`treat_bin' = 1 if drugprescribed`i' == `treat' & p_lag_`i' > 0 // Otherwise, consider it in-hospital treatment
			}
			forvalues i = 1/8 {
				replace tr_hosp_`treat_bin' = 1 if Treathosp`i' == `treat'
			}
		}
	}
}
drop p_lag_* Treathosp* drugprescribed* TreatmentAdm* 

// For a subset of diagnoses (malaria, malnutrition, etc.), if they received an initial diagnosis, were they provided with "expected" medicine?
preserve
import delimited using "$data_dir/rec_dx_treatments.csv", clear
levelsof diag_code, local(all_diags) c

foreach diag in `all_diags' {
	levelsof parent_drug if type == "primary" | type == "general", local(tr_pr_`diag') c
	levelsof parent_drug if type == "supportive", local(tr_sec_`diag') c
}
restore

foreach diag in `all_diags' {
	gen tr_match_`diag' = "No"
	// Categorize treated cases
	foreach treat in `tr_pr_`diag'' {
		replace tr_match_`diag' = "Yes" if dx_admit_`diag' == 1 & (tr_hosp_`treat' == 1 | tr_admit_`treat' == 1)
	}
	// Categorize cases that only got supportive care
	if "`tr_sec_`diag'" != "" {
		foreach treat in `tr_sec_`diag'' {
			replace tr_match_`diag' = "Support Only" if dx_admit_`diag' == 1 & tr_match_`diag' == "No" & (tr_hosp_`treat' == 1 | tr_admit_`treat' == 1)
			
			// If there are fewer than 10 observations in the support only column, call all of them general
			// This is to avoid test/train split issues later on, where the training set may not contain any observations that include a "Support Only" value
			//	and is asked to predict on the test dataset that does have some of those values
			qui count if tr_match_`diag' == "Support Only" 
			if `r(N)' < 10 replace tr_match_`diag' = "Yes" if tr_match_`diag' == "Support Only"
		}
	}
}

********************************************************
** Standardize variable formatting/labels
// Encode/decode certain variables
	decode SiteID, gen(site_name)
	drop SiteID 
	rename site_name site_id
	
// Determine the length of stay, using date of discharge and admission
	replace Datedischarge = subinstr(Datedischarge,"301","201",.) // If year is coded as 3013
	replace Datedischarge = subinstr(Datedischarge,"2044","2011",.)
	replace Datedischarge = subinstr(Datedischarge,"2021","2012",.)
	replace Datedischarge = subinstr(Datedischarge,"2020","2010",.)
	replace Datedischarge = subinstr(Datedischarge,"1930","2012",.)
	replace Datedischarge = subinstr(Datedischarge,"1931","2012",.)

	gen admit_duration = date(Datedischarge,"YMD###") - dateadmit
	replace admit_duration = . if admit_duration < 0 | admit_duration > 365 // Consider these outliers and treat them as missing
	
	// Examine differences in Days length-of-stay variable and admit_duration LOS variable
	sum Days admit_duration if Days != . & admit_duration != .
	count if Days != admit_duration & Days != .
	count if Days == 0
	count if admit_duration == 0
	
	drop Days admit_duration // Drop the LOS variables for now since they are quite discrepant
	
// Recode sex into a character variable
	gen sex = ""
	replace sex = "Male" if Gender == 0
	replace sex = "Female" if Gender == 1
	replace sex = "Missing" if Gender == 9
	drop Gender
	rename sex Gender
	
// Fix ages -- variable age is in months, along with significant heaping to major age values
// Other age variables available -- year, month, day
	/*
	gen age_new = (AgeYrs * 365 + AgeMths* 30.5 + AgeDays) / 30.5 // Age in months
	sum age
	sum age_new
	drop age_new
	*/
	
// Drop all recorded cases over 5 years of age
	keep if age < 60
	hist age
	graph export "$data_dir/../graphs/age_dist.pdf", replace
	cap graph close _all
	// kdensity age
	// kdensity age, n(87137) gen(test1 test2) bwidth(6)
	// replace test1 = 0 if test1 < 0
	
// Recode ages to a categorical to address age heaping
	egen age_cat = cut(age), at(0 2 4 7 12 24 36 48 60) // Cut at 0-1 months, 2-3 months, 4-6 months, 7-11, then annual after
	gen age_end = age_cat + 12
	replace age_end = 1 if age_cat == 0
	replace age_end = 4 if age_cat == 2
	replace age_end = 7 if age_cat == 4
	tostring age_end, replace
	tab age_cat
	tostring age_cat, replace
	replace age_cat = age_cat + " to " + age_end + " months"
	drop age age_end
	rename age_cat age

// Generate a string variable for anti-malarial treatment
	gen tr_anti_malarial = ""
	replace tr_anti_malarial = "none" if quinineother == 0
	replace tr_anti_malarial = "quinine" if quinineother == 1 
	replace tr_anti_malarial = "other" if quinineother == 2
	drop anyantimalarial otherantimalarial quinine*
	
// Generate a string variable for malaria test result (and then complicated or not)
	decode malaria, gen(malaria_test)
	replace malaria_test = "Complicated" if compmalaria == 1 
	replace malaria_test = "Uncomplicated" if malaria_test == "Yes"
	replace malaria_test = "None" if malaria_test == "No"
	drop compmalaria malaria labtestresult
	rename clinicalmalaria malaria_final // This is the final diagnosis of malaria, rather than the malaria test

// Generate a string variable for temperature
// Choose under 35.5 as the indicator to mirror the Mpimbaza risk score paper
	replace Temp = . if Temp < 15 | Temp > 50 // These temperatures are impossible (and 99 and 99.9 are missing)
	gen temp_under35p5 = "No"
	replace temp_under35p5 = "Yes" if Temp <= 35.5 & Temp != .
	replace temp_under35p5 = "Missing" if Temp == .
	drop Temp
	
// Fix Jaundice coding
	replace Jaundice = 9 if Jaundice > 9 | (Jaundice >= 2 & Jaundice <= 8) // Supposed to be a 0/1/9 variable
	
// Generate a string variable for HB level
// Use under 70 g/L as the cutoff for severe anemia in children 6-59 months of age according to WHO
	gen hb_under7 = "No"
	replace hb_under7 = "Yes" if HBlevel < 7
	replace hb_under7 = "Missing" if HBlevel == 31
	drop HBlevel
	
// Generate a string variable for Pallor
	gen pallor_string = ""
	replace pallor_string = "None" if Pallor == 0
	replace pallor_string = "Mild or Moderate" if Pallor == 1
	replace pallor_string = "Severe" if Pallor == 2
	replace pallor_string = "Missing" if Pallor == . // For some reason it has a lot of over-2 numbers, assume missing
	drop Pallor
	rename pallor_string Pallor
	
// Generate a string variable for Airway
	tostring Airway, replace
	replace Airway = "Clear" if inlist(Airway,"0","1") // Assume that 0 means clear airway, and was just miscoded
	replace Airway = "Strider" if inlist(Airway,"2")
	replace Airway = "Missing" if inlist(Airway,"9")
	
// Recode random missings
	replace AltConsciousness = . if AltConsciousness == 11


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
			replace `var' = . if `var' == 9 & !inlist("`var'","Weight","MUAC","AdmissionDiag1","BS1admit","RDTadmit","HIV") 
		}
	}
	

********************************************************
** Convert binary variables from numeric to categorical indicators	
	preserve
	keep `num_vars'
	collapse (max) `num_vars'
	local binary_vars = ""
	foreach var of varlist * {
		qui count if `var' > 1 & `var' != .
		if `r(N)' == 0 & substr("`var'",1,2) != "dx" & substr("`var'",1,2) != "tr" local binary_vars = "`binary_vars' `var'"
	}
	restore
	
	label define binary 1 "Yes" 0 "No" 9 "Missing"
	label values `binary_vars' binary
	foreach var in `binary_vars' {
		replace `var' = 9 if `var' == .
		decode `var', gen(str_var)
		drop `var'
		rename str_var `var'
	}
	

********************************************************
** Output preliminary variable list for use in variable pruning	
// dateadmit is the admission date (already formatted in Stata dates)
// Also have monthyear, year, and weekyear variables -- possible covariates?
	rename dateadmit admit_date
	drop date* Date*
	rename admit_date dateadmit

// Turn all variables to lowercase
	rename *,lower	
	
// Drop some treatment variables with very low fill-out rates, or covered by other variables
	drop *transfus* *bldtran* tranfusdate 
	drop amodiaquine artemetherim artesunateiv artesunateoral artesunaterectal chloroquine al sp arco dp
	
// Preliminary: To drop or keep this set of variables that are all bunched together and included/not?
	drop oxygen ivfluids nutrition tepidspong kpw nasogastric leftlateral

// Drop macro age variables
	drop ageyrs agemths agedays
	
	cd "$data_dir"
	preserve
	describe, replace clear
	export delimited using "prelim_vars.csv", delimit(",") replace
	restore


********************************************************
** Rename variables intelligibly

// Make variable labels a bit more self-explanatory
	rename (bs1admit rdtadmit) (bs_admit rdt_admit)

// Rename signs and symptoms variables to ss_*
	local ss_vars = "fever cough cough2weeks diffbreath convulsions altconsciousness vomiting unabledrink diarrhea diarrhea2wks bldydiarrhea teaurine"
	local ss_vars = "`ss_vars' temp_under35p5 weight pallor jaundice dpbreath flarnostril icrecession subcostal airway wheezing"
	local ss_vars = "`ss_vars' crackles unconscious unablesit bgfontanelle stiffneck kerning" 

// Rename treatment variables to tr_*
// No need to recode tr_admit and tr_hosp variables because they're already correctly formatted
	local tr_vars = "" // Duplicative of existing treatment data?

// Rename testing variables to te_*
	local te_vars = "bs_admit rdt_admit hb_under7 hiv malaria_test" 
	local te_vars = "`te_vars'"

// Rename extra diagnosis variables to dx_*
	local dx_vars = "malaria_final"

// Rename covariates to cv_*
	local cv_vars = "readmission gender monthyear year age" 
	local cv_vars = "`cv_vars' site_id "

// Enact renames
	foreach vartype in ss te dx cv {
		foreach var in ``vartype'_vars' {
			rename `var' `vartype'_`var'
		}
	}


// Drop bs and rdt test variables (after the admit variables have been renamed) 
	drop bs* rdt* hb*

// Rename outcome variable to death (also have malariadeath as another potential outcome)
	rename anydeath death
	
// Drop malariadeath and disposition variables (these are outcomes, but not the primary ones of interest)
	drop disposition malariadeath
	
// Drop dateadmit because it's too wide -- will rely on monthyear
	drop dateadmit
	
// Output a cleaned variable list
	preserve
	describe, replace clear
	export delimited using "cleaned_vars.csv", delimit(",") replace
	restore

********************************************************
** Output data
cd "$data_dir"
export delimited using "01_cleaned_data.csv", delimit(",") replace

********************************************************
** Generate cross-tabs of treatment and diagnosis for committee to examine
// Generate means
	drop dx_malaria_final tr_anti_malarial dx_misdiag* dx_admiss* tr_match*
	collapse (mean) tr_* dx_*
	rename * mean_*
	gen id = 1
	reshape long mean_, i(id) j(var_name) string
	
// Merge on layperson labels for diagnoses
	split var_name, parse("_")
	rename var_name3 diag_code
	replace diag_code = "" if !regexm(var_name,"dx_")
	destring diag_code, replace force
	drop var_name1 var_name2 var_name4
	merge m:1 diag_code using `diagnoses', keep(1 3) nogen
	drop id
	
// Format and export
	sort diag_code var_name
	export delimited using "01_tr_dx_tab.csv", delimit(",") replace
