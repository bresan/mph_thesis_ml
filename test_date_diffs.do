// Figure out what differences there are between the Length of Stay variable and a user-generated version
cd "$data_dir"
use "IP data base Nov 2015_Grant Nguyen_Version 2013.dta", clear

// Determine the length of stay, using date of discharge and admission

	gen admit_duration = date(Datedischarge,"YMD###") - dateadmit
	replace admit_duration = . if admit_duration < 0 | admit_duration > 365 // Consider these outliers and treat them as missing
	
	// Examine differences in Days length-of-stay variable and admit_duration LOS variable
	sum Days admit_duration if Days != . & admit_duration != .
	count if Days != admit_duration & Days != .
	count if Days == 0
	count if admit_duration == 0
	
	br Datedischarge dateadmit Days admit_duration if Days != . & admit_duration != .
	