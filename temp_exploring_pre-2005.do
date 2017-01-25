import delimited "C:\Users\slevine2\Dropbox (Stanford Law School)\NIOSH\NIOSH-Analysis\data\1_cleaned\clean_accidents.csv", delimiter("|") 

gen date = accidentdate

replace date = substr(date,-4,.)

gen pre = inlist(date, "2000", "2001", "2002", "2003", "2004", "2005")

tab accidenttype if pre
tab accidenttype if !pre

tab mineractivity if pre
tab mineractivity if !pre

tab accidentclassification if pre
tab accidentclassification if !pre

tab degreeofinjury if pre
tab degreeofinjury if !pre

tab sourceofinjury if pre
tab sourceofinjury if !pre

tab natureofinjury if pre
tab natureofinjury if !pre

tab occupation if pre
tab occupation if !pre

foreach var of varlist accidenttype mineractivity accidentclassification degreeofinjury sourceofinjury natureofinjury occupation {
	tostring `var', replace
	replace `var' = . if inlist(`var', "no value found", "nec", "miscellaneous,nec", "other", "unknown", "other, nec", "unclassified, insufficient data")

}
