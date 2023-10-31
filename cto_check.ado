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
	OUTPUT(string) ///
	DIRECTORY(string) ///
	ENUM(name) ///
	SUCCESSCONDITION(string)

clear
version 17
frames reset
	
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
	local varname_repeat_`i' = name[`begin_`i'']
	
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

frame rename default instrument
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

*------------------------------------------------------------------
*	Get all relevant variables
*------------------------------------------------------------------

local name_repeat_0 survey
local varname_repeat_0 survey

forvalues i = 1/6 {
	
	forvalues j = 0/`repeat_groups' {
		
		levelsof name if question_type == `i' ///
			& dataset == "`name_repeat_`j''", local(`j'_v`i') clean
	
	}
	
}

*===============================================================================
* 	Open File
*===============================================================================

local brek = char(10)
local tab = char(9)

frame create hfc
cwf hfc 

insobs 1
gen command = ""

forvalues j = 0/`repeat_groups' {
	
	replace command = command + ///
		"*==============================================================================="  ///
		+ "`brek'* 	`name_repeat_`j''`brek'" + ///
		"*===============================================================================" ///
		+ "`brek'`brek'" + ///
		`"use "`macval(directory)'/`varname_repeat_`j''.dta", clear`brek'`brek'"'
	
	// STRING VARIABLES
	if "``j'_v1'" != "" { 
		
		replace command = command + ///
			"// string variables`brek'local string_vars ``j'_v1'`brek'`brek'"
		
	}
	
	// SELECT ONE VARIABLES
	if "``j'_v2'" != "" { 
		
		replace command = command + ///
			"// select one variables`brek'local sel_one_vars ``j'_v2'`brek'`brek'"
		
	}
	
	// SELECT MULTIPLE VARIABLES
	if "``j'_v3'" != "" { 
		
		replace command = command + ///
			"// select multiple variables`brek'local sel_mult_vars ``j'_v3'`brek'`brek'"
		
	}
	
	// NUMERIC VARIABLES
	if "``j'_v4'" != "" { 
		
		replace command = command + ///
			"// numeric variables`brek'local numeric_vars ``j'_v4'`brek'`brek'"
		
	}
	
	// DATE VARIABLES
	if "``j'_v5'" != "" { 
		
		replace command = command + ///
			"// date variables`brek'local date_vars ``j'_v5'`brek'`brek'"
		
	}
	
	// DATETIME VARIABLES
	if "``j'_v6'" != "" { 
		
		replace command = command + ///
			"// datetime variables`brek'local datetime_vars ``j'_v6'`brek'`brek'"
		
	}
	
	
	
	
}

file open myfile using "`output'", write replace

file write myfile ///
	"/*" ///
	_n "Title: HFC Dofile for `macval(instname)'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Note: " ///
	_n "*/" _n(2) ///
	"quietly {" _n(2) ///
	"*===============================================================================" ///
	_n "* 	Setup" _n /// 
	"*===============================================================================" ///
	_n(2) "clear all" _n "version 17" _n "set more off" _n "set maxvar 30000" ///
	_n "cap log close" _n "set trace off" _n "set linesize 200" _n(2) ///
	"*===============================================================================" ///
	_n "* 	Macros" _n /// 
	"*===============================================================================" ///
	_n(2) ///
	"local" _tab `"today = date(c(current_date), "`datestyle'")"' _n ///
	"local" _tab `"todaystr = string(\`today', "%td")"' _n(2) ///
	"*===============================================================================" ///
	_n "* 	Program Definitions" _n /// 
	"*===============================================================================" ///
	_n(2) "prog define dispersion_check" _n(2) ///
	"syntax varlist" _n ///
	"foreach var of varlist \`varlist' {" _n(2) ///
	_tab "sum \`var'" _n _tab "local obs = \`r(N)'" _n _tab ///
	"local mean = \`r(mean)'" _n _tab "local sd = \`r(sd)'" _n _tab ///
	"local max = \`r(max)'" _n _tab ///
	"local digits = ceil(log(\`max')/log(10) )+ 3" _n(2) _tab ///
	"count if \`var' < 0" _n _tab "if \`r(N)' == 0 local lower_bound = 0" ///
	_n _tab "else local lower_bound : display %-\`digits'.2f \`mean' - (`strictness' * \`sd')" ///
	_n(2) _tab "count if \`var' < \`lower_bound'" _n _tab ///
	"local lowers = \`r(N)'" _n _tab ///
	`"local lower_bound = strtrim("\`lower_bound'")"' _n(2) _tab ///
	"local upper_bound : display %-\`digits'.2f \`mean' + (\`strictness' * \`sd')" ///
	_n _tab "count if \`var' > \`upper_bound' & !missing(\`var')" _n _tab ///
	"local uppers = \`r(N)'" _n _tab ///
	`"local upper_bound = strtrim("\`upper_bound'")"' _n(2) _tab ///
	"histogram \`var', kdensity xline(\`upper_bound' \`lower_bound') ///" ///
	_n _tab(2) ///
	`"note("Suggested lower constraint: \`lower_bound' (\`lowers' offending observations). Suggested upper constraint: \`upper_bound' (\`uppers' offending observations).", size(vsmall)) ///"' ///
	_n _tab(2) `"title("{bf}\`: variable label \`var''", pos(11) size(2.75)) ///"' ///
	_n _tab(2) `"subtitle("\`obs' observations", pos(11) size(2.5)) ///"' ///
	_n _tab(2) `"ylabel(, grid gmax) ///"' _n _tab(2) ///
	`"xmlabel(\`upper_bound' \`lower_bound', labsize(*1.5) tlength(medium)) ///"' ///
	_n _tab(2) "name(\`var')" _n(2) "}" _n(2) "end" _n(2) ///
	`"prog define duration_graph"' _n(2) ///
	`"preserve"' _n `"keep if `successcondition'"' _n ///
	`"replace duration = duration * 1e2"' _n(2) ///
	`"collapse (mean) duration, by(`enum')"' _n(2) ///
	`"egen double duration_z = std(duration)"' _n ///
	`"generate above = (duration_z >= 0)"' _n ///
	`"sort duration_z, stable"' _n ///
	`"generate rank_des = _n * 2"' _n ///
	`"cap sdecode `enum', replace"' _n ///
	`"labmask rank_des, value(`enum')"' _n ///
	`"generate zero = 0"' _n ///
	`"tostring duration, gen(duration_lab) force format(%tCMM:SS)"' _n ///
	`"count"' _n `"local max = \`r(N)' * 2"' _n(2) ///
	`"twoway  (rspike zero duration_z rank_des, horizontal) ///"' _n ///
	_tab `"(scatter rank_des duration_z, msize(6) mlabel(duration_lab) ///"' ///
	_n _tab `"mlabsize(1.6) mlabposition(0)), ///"' _n _tab ///
	`"xlabel(-3(0.5)3, nolabels noticks) ylabel(2(2)\`max', ///"' _n _tab ///
	`"valuelabel labsize(2)) legend(off) ///"' _n _tab ///
	`"ytitle("Enumerator") ///"' _n _tab ///
	`"title("{bf}Average Survey Duration", size(2.75) pos(11)) ///"' _n _tab ///
	`"subtitle("When `successcondition'", size(2.5) pos(11)) ///"' _n _tab ///
	`"scheme(white_tableau) ysize(8)"' _n(2) `"restore"' _n(2) `"end"' _n(2)
	
file write myfile (command)
file write myfile _n "}"
file close myfile

}

end 


// gen dummy = 1 if consented == 1
//
// foreach var of varlist `string_vars' `sel_one_vars' `sel_mult_vars' `numeric_vars' {
//	
// 	if ustrregexm("`var'", "^reserved_name_") continue
//	
// 	local v_`var' : variable label `var'
// 	local type : type `var'
// 	if ustrregexm("`type'", "^str") {
//		
// 		gen NO_`var' = `var' == ""
//		
// 		local col_cmd `col_cmd' (sum) NO_`var'
//		
// 	}
// 	else {
//		
// 		gen NO_`var' = `var' == .
// 		gen REF_`var' = cond(NO_`var' == 0, `var' == .r, .)
// 		gen DK_`var' = cond(NO_`var' == 0, `var' == .d, .)
//		
// 		local col_cmd `col_cmd' (sum) NO_`var' REF_`var' DK_`var'
//		
// 	}
//	
// }
//
// collapse (sum) dummy `col_cmd', by(enum_name)
//
// foreach var in `string_vars' `sel_one_vars' `sel_mult_vars' `numeric_vars' {
//	
// 	label variable NO_`var' "`v_`var''"
// 	cap label variable REF_`var' "`v_`var''"
// 	cap label variable DK_`var' "`v_`var''"
//	
// }
//
// reshape long NO_ REF_ DK_, i(enum_name) j(variable) string
// renvars *_, postsub("_" "")
//
// bysort enum_name: egen median = median(NO)
// egen category = axis(median enum_name), label(enum_name)
//
// expand dummy
//
// spineplot NO category, ///
// xtitle("Proportion of all Questions", size(small) axis(1)) ///
// xtitle("enum_name", size(small) axis(2)) ///
// title("{bf}Number of Missing Responses per Surveyor", pos(11) size(2.75)) ///
// subtitle("Survey-level variables which are completely missing", pos(11) size(2.5)) ///
// ytitle("Proportion within enum_name", axis(2) size(small)) ///
// xlabel(, angle(45) axis(2) labsize(1.7)) xsize(7)
