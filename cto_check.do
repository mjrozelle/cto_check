*! cto_check.ado - Stata module to perform hfcs on SurveyCTO data
*! Author: Michael Rozelle <michael.rozelle@wur.nl>
*! Version 0.0.1  Modified:  March 2023

// Drop the cto_read program if it already exists
cap program drop cto_check

// Define the cto_read program, which takes three arguments
program define cto_check, rclass
// instrument, then dataset, then dofile
syntax, ///
	INSTname(string) ///
	DIRECTORY(string) ///
	DOfile(string) ///
	ENUM(string) ///
	UID(string) ///
	DURATION(string) ///
	INTERESTvars(namelist) ///
	HFCDIRectory(string)
	
version 17

// Create a quiet environment to prevent unnecessary output
qui { 

// Reset any previously defined frames
frames reset

*===============================================================================
* 	Import XLSforms
*===============================================================================

/* We're going to work in two frames - there's the "survey" sheet containing the 
questions, enablement conditions, and so on, as well as the "choices" sheet,
which gives us all the value labels. Rather than open and close a million datasets,
frames let us work on both these levels simultaneously.
*/

local instname "/Users/michaelrozelle/Library/CloudStorage/Dropbox/Farm_Flock_Fork/3.instruments/yield_measurement/yield_measurement.xlsx"

*===============================================================================
* 	Choices
*===============================================================================

// Import the "choices" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(choices)

// Rename the "listname" variable to "list_name" for consistency
cap rename listname list_name 

// Keep only the "list_name", "name", and "label" variables
keep list_name name label 

// Remove any empty rows from the dataset
missings dropobs, force 

// Remove any rows where the "name" variable is not a number (i.e. non-standard labeling)
drop if !regexm(name, "^[0-9]+$") 

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n 

// Create a clone of the "name" variable for programming purposes
clonevar name1 = name 

// Replace any minus signs in the "name" variable with underscores, to match how SurveyCTO handles value labels
replace name = subinstr(name, "-", "_", 1) 

// Replace any dollar signs in the "label" variable with pound signs, to prevent Stata from parsing them as macros
replace label = subinstr(label, "$", "#", .)

// Remove any double quotes from the "label" variable
replace label = subinstr(label, `"""', "", .)

// Remove any spaces from the "list_name" variable
replace list_name = subinstr(list_name, " ", "", .)

// Create a local macro called "brek" containing a line break character
local brek = char(10) 

// Remove any line breaks from the "name" and "name1" variables
foreach var of varlist name name1 {
	replace `var' = subinstr(`var', "`brek'", "", .)
}

tempfile choices
save `choices'

*===============================================================================
* 	The Instrument
*===============================================================================

// Import the "survey" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(survey) 

cap confirm variable label 

if !_rc {
	
	rename label labelEnglishen
	clonevar labelStata = labelEnglishen
	
}
else {
	
	cap tostring labelStata, replace
	
}

// Keep only relevant variables in the instrument
keep type name labelEnglishen labelStata calculation

drop if missing(name)

// Replace any dollar signs in the variables with hash signs
foreach v of varlist labelEnglishen labelStata {
	replace `v' = subinstr(`v', "$", "#", .)
}

// Replace any missing Stata labels with the English labels
replace labelStata = "" if labelStata == "."
replace labelStata = labelEnglishen if missing(labelStata)

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n

// Create a local macro called "brek" containing a line break character
local brek = char(10) 

// Remove any line breaks, dollar signs, and double quotes from variables
foreach var of varlist labelEnglishen labelStata {
	replace `var' = subinstr(`var', "`brek'", "", .)
	replace `var' = subinstr(`var', "$", "#", .) 
	replace `var' = subinstr(`var', `"""', "", .)
	/* Stata will intepret dollar signs as globals, and as a result, we won't 
	be able to see the questions being referenced for constraints (or even 
	inside the question itself). By changing the dollar sign to a #, we can 
	still see these references in the variable labels and notes */
}

// Replace any full stops in variable names with an empty string
replace name = subinstr(name, ".", "", .) 

// Convert all variable names to lower case
replace name = lower(name) 

// Split the "type" variable into two variables, "type" and "type2"
split type 
// Note that "type2" will contain the value label name for categorical variables.

*===============================================================================
* 	Repeat Groups
*===============================================================================

/* 
These next few loops tell us which groups each question belongs to. This helps
 us tell if a question exists within a loop, or a specific block of questions 
 with an enablement condition, etc.
*/

foreach jump in begin end { // identify the begin and end repeat sections

	count if type=="`jump'_repeat"
	local repeat_groups = `r(N)' // number of repeat sections
	levelsof order if type=="`jump'_repeat", local(`jump') separate( ) // the rows where those sections are in the instrument
	tokenize "``jump''", parse( ) // create numbered macros containing where those rows are
	forvalues num = 1/`repeat_groups' {
		
		local `jump'_`num' ``num'' // this will be useful in the next loop
		
	}
	
}

forvalues i = 1/`repeat_groups' {
	
	local taken_`i' = 0 // this local will indicate whether an "end_repeat" has already been assigned to a specific repeat group
	local name_repeat_`i' = labelStata[`begin_`i'']
	
}

forvalues i = `repeat_groups'(-1)1 { // for each begin_repeat

	forvalues j = `repeat_groups'(-1)1 { // for each end_repeat 
	
		local true = (`end_`j'' > `begin_`i'') & `taken_`j'' == 0 // evaluates to 1 if the end_repeat is after the begin_repeat and hasn't already been taken
		if `true' {
			
			local k = `j' // potential match. But we will try lower values of repeat blocks to check if there is another end_repeat that is closer
			
		}
		
	}
	
	local taken_`k' = 1 // we have assigned this end_repeat
	gen repeat_group_`i' = 1  in `begin_`i''/`end_`k'' // gen dummy=1 if question is inside repeat block
		
	sort order
	
}

*------------------------------------------------------------------
*	Question Types
*------------------------------------------------------------------

gen preloaded=regexm(calculation, "^pulldata") // If the variable is preloaded, we may want to be alerted to that fact
gen note = type=="note" // Notes are useless to us, good to identify them here

label define question_type_M 1 "String" 2 "Select One" 3 "Select Multiple" ///
	4 "Numeric" 5 "Date" 6 "Datetime" -111 "Not Relevant for HFC"
gen question_type=.
label values question_type question_type_M
replace question_type = 1 if type == "text"
replace question_type = 2 if word(type, 1)=="select_one"
replace question_type = 3 if word(type, 1)=="select_multiple"
replace question_type = 4 if !inlist(type, "date", "text") & missing(question_type)
replace question_type = 5 if inlist(type, "date", "today")
replace question_type = 6 if inlist(type, "start", "end", "submissiondate")
replace question_type= 7 if type == "geopoint"
replace question_type = -111 if ///
	inlist(type, "begin_group", "end_group", "begin_repeat", ///
	"end_repeat", "text audit", "deviceid", "image") ///
	| note == 1 | preloaded == 1
replace question_type=-111 if missing(question_type)

/* 
The above tries to assign a question type to every row of the instrument. 
If the question type is negative (see value label) the dofile will just skip 
that variable. In the majority of cases this should be benign, and you'll know
pretty quickly if a variable needs attention.
*/

drop if question_type < 0

reshape long repeat_group_@, i(name) j(repeat_num)
replace repeat_num = . if missing(repeat_group_)
bysort name: egen repeat_group = max(repeat_num)
keep if repeat_group == repeat_num
bysort name: keep if _n == 1
drop repeat_num repeat_group_

gen dataset = ""
replace dataset = "survey" if missing(repeat_group)

forvalues i = 1/`repeat_groups' {
	
	replace dataset = "`name_repeat_`i''" if repeat_group == `i'
	
}

drop repeat_group preloaded note calculation

tempfile instrument
save `instrument'

*===============================================================================
* 	Open File
*===============================================================================

// Now we're about to write the instructions to a dofile. Buckle up

file open myfile using "`dofile'", write text replace

file write myfile ///
	"/*" ///
	_n "Title: HFC Dofile for `macval(instname)'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Note: " ///
	_n "*/" _n(3) ///
	"quietly {" _n(2) ///
	"*===============================================================================" ///
	_n "* 	Setup" _n /// 
	"*===============================================================================" ///
	_n(3) "clear all" _n "version 17" _n "set more off" _n "set graphics off" _n ///
	"set maxvar 30000" _n "cap log close" _n "set trace off" ///
	_n "set linesize 200" _n(3) ///
	"*===============================================================================" ///
	_n "* 	Macros" _n /// 
	"*===============================================================================" ///
	_n(3) ///
	"local" _tab `"today = date(c(current_date), "`datestyle'")"' _n ///
	"local" _tab `"todaystr=string(\`today', "%td")"' _n(3) ///
	"*===============================================================================" ///
	_n "* 	Load Datasets" _n /// 
	"*===============================================================================" _n(2) ///
	"frame rename default survey" _n `"use "`directory'/survey_level.dta""' _n(2)
	
levelsof dataset, local(datasets)
foreach dataset in `datasets' {
	
	if "`dataset'" == "survey" {
		
		continue
		
	}
	
	file write myfile "frame create `dataset'" _n ///
	`"frame `dataset': use "`directory'/`dataset'_level.dta""' _n(2)
	
}
	

*===============================================================================
* 	Variables
*===============================================================================

sort order
local v = 1
local brek = char(10)
local tab = char(9)

forvalues i = 0(1)`repeat_groups' {
	
	if `i' == 0 {
		
		local section "survey"
		
	}
	else {
		
		local section = "`repeat_name_`i''"
		
	}
	
	local title = proper("`section'")
	
	file write myfile "*===============================================================================" ///
	_n "* 	`title'" _n /// 
	"*===============================================================================" _n(2) ///
	"cwf `section'" _n(2)
	
	levelsof name if dataset == "`section'" & question_type == 4, local(number_variables)
	quietly foreach var in `number_variables' {
		
		local this_q if name == "`var'"
		sum order `this_q'
		local order = `r(max)' // order of questions in the survey
		levelsof labelStata `this_q', local(stlabel) // Stata label
		levelsof labelEnglishen `this_q', local(enlabel) // How question appeared in the survey
		
		file write myfile "*------------------------------------------------------------------" ///
		_n "* 	`stlabel'" _n ///
		"* 	`labelEnglishen'" _n /// 
		"* 	QUESTION TYPE: Number" _n /// 
		"*------------------------------------------------------------------" _n(2) ///
		"sum `var', detail" ///
		_n "histogram `var', kdensity xline(\`r(p5)') xline(\`r(p95)')" _n ///
		`"graph export "`hfcdirectory'/`var'_histogram.png", as(png) replace"' _n(2) ///
		"regress `var' i.`enum', vce(robust)" _n(2)
	}
	
	levelsof name if dataset == "`section'" & question_type == 1, local(string_variables)
	quietly foreach var in `string_variables' {

		local this_q if name == "`var'"
		sum order `this_q'
		local order = `r(max)' // order of questions in the survey
		
		cap levelsof type2 `this_q', local(vallabel) clean // value label
		
		levelsof labelStata `quest', local(stlabel) // Stata label
		
		levelsof labelEnglishen `quest', local(enlabel) // How question appeared in the survey
		
	}
	
	
}

	
file close myfile
	
	
}

end
