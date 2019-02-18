*****																		*****
*****																		*****
*****			CALL for period fertility data in the early 2000s			*****
*****			Register-Based Fertility Research Network					*****
*****																		*****

/*
		Note: time t is a chosen cross-sectional measurement time of age, gender, education, nuts, foreign. This is preferably the year 2001.
		Note: the period [t+1;t+4] is the observation window in which we observe fertility, given characteristics measured in year t


***		REQUIRED VARIABLES IN INDIVIDUAL DATASET	***

id 					anonymous unique numeric person identifier

weight_t			individual weight (equals 1 for every individual in case of simple random sample)

nuts_t				numeric indicator for region of residence, measured at year t (preferably the last day of the year t)
					(please use NUTS2. If this is not possible or does not seem the best option, please let us know)

education_t 		education, measured at year t (preferably the last day of the year t)
					(0) enrolled (full-time) in education 
					(1) not in education and low attainment(1997 ISCED 0-2) (Level 0 - Pre-primary education / Level 1 - Primary education or first stage of basic education / Level 2 - Lower secondary or second stage of basic education) 
					(2) not in education and medium attainment (1997 ISCED 3-4)  (Level 3 - (Upper) secondary education / Level 4 - Post-secondary non-tertiary education)
					(3) not in education and high attainment (1997 ISCED 5-6) Level 5 - First stage of tertiary education / Level 6 - Second stage of tertiary education 
					(9999) unknown

foreign_t 			country of birth, measured at year t (preferably the last day of the year t)
					(0) born in country of residence, 
					(1) born in other country
					(9999) unknown

gender_t 			gender, measured at year t (preferably the last day of the year t)
					(1) men 
					(2) women
					(9999) unknown

age_t				age (years) at year t (preferably the last day of the year t)
					(9999) unknown

birthyear_child1	year of birth first biological child, measured at year t+5 or later
					(dates up to year t are used to calculate parity in year t, dates between t+1 and t+4 are used to assess fertility by level of education)
					(9999) no first biological child or unknown date
					In case of multiple births (e.g. twins), these are treated as subsequent births in the input data (analyses will treat them as one event, while parity will treat them as multiple births)

birthyear_child2	year of birth of the second biological child, measured at year t+5 or later
					(dates up to year t are used to calculate parity in year t, dates between t+1 and t+4 are used to assess fertility by level of education)
					(9999) no second biological child or unknown date
					In case of multiple births (e.g. twins), these are treated as subsequent births in the input data (analyses will treat them as one event, while parity will treat them as multiple births)

birthyear_child3	year of birth of the third biological child, measured at year t+5 or later
					(dates up to year t are used to calculate parity in year t, dates between t+1 and t+4 are used to assess fertility by level of education)
					(9999) no third biological child or unknown date
					In case of multiple births (e.g. twins), these are treated as subsequent births in the input data (analyses will treat them as one event, while parity will treat them as multiple births)

death 				If death in period t+1 to t+4, year of death, measured at year t+5 or later
					(9999) no death in period t+1 to t+4 or unknown

emigration 			If emigration (abroad) in period t+1 to t+4, year of emigration, measured at year t+5 or later
					(9999) no emigration in period t+1 to t+4 or unknown

*/ 

*** INPUT  ***

* set working directory (please insert correct path)
cd "K:\UA\"
log using "aggregate data - Register-Based Fertility Research Network.smcl" , replace
use "Example data.dta", clear

* time_1 (please insert correct year t)
gen time_1 = 2001


*** CHECK ALL VARIABLES ***
codebook id
tab age_t ,miss
tab age_t ,miss nol

tab gender_t ,miss
tab gender_t ,miss nol

tab education_t ,miss
tab education_t ,miss nol

tab nuts_t ,miss
tab nuts_t ,miss nol

tab foreign_t ,miss
tab foreign_t ,miss nol

tab death ,miss
tab death ,miss nol

tab emigration ,miss
tab emigration ,miss nol

sum weight_t


*** Creation parity progression variables ***

* time 2
gen time_2 = time_1 + 4

* birth dates: create parity, lastbirth and birth_child
capture ssc install rowsort
rowsort birthyear_child1 birthyear_child2 birthyear_child3 , gen (RKIDBY1 RKIDBY2 RKIDBY3)
drop birthyear_child1 birthyear_child2 birthyear_child3

replace RKIDBY1 = . if RKIDBY1 == 9999
replace RKIDBY2 = . if RKIDBY2 == 9999
replace RKIDBY3 = . if RKIDBY3 == 9999

gen RKIDBY1_temp = RKIDBY1
replace RKIDBY1_temp = . if RKIDBY1 > time_1
gen RKIDBY2_temp = RKIDBY2
replace RKIDBY2_temp = . if RKIDBY2 > time_1
gen RKIDBY3_temp = RKIDBY3
replace RKIDBY3_temp = . if RKIDBY3 > time_1
egen parity_t = rownonmiss(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
egen lastbirth = rowmax(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
drop if parity_t > 2
drop RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp
tab lastbirth parity ,miss

gen RKIDBY1_temp = RKIDBY1
replace RKIDBY1_temp = . if RKIDBY1 <= time_1
replace RKIDBY1_temp = . if RKIDBY1 > time_2
gen RKIDBY2_temp = RKIDBY2
replace RKIDBY2_temp = . if RKIDBY2 <= time_1
replace RKIDBY2_temp = . if RKIDBY2 > time_2
gen RKIDBY3_temp = RKIDBY3
replace RKIDBY3_temp = . if RKIDBY3 <= time_1
replace RKIDBY3_temp = . if RKIDBY3 > time_2
egen birth_child = rowmin(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
drop RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp
replace birth_child = 9999 if birth_child == .
tab birth_child ,miss


*** continuous-time approach *** 

gen birth = 0
replace birth = 1 if birth_child ~= 9999

gen bday50 = ((time_1 - age_t) + 50)
egen temp = rowmin(birth_child death emigration time_2 bday50)
gen timetocens = 9999
replace timetocens = (temp - ((time_1 - age_t) + 15)) + 1 if parity_t == 0
replace timetocens = (temp - lastbirth) + 1 if parity_t > 0
drop bday50 temp

stset timetocens /*
*/ if age_t < 50 & timetocens >= 1 /* 
*/, id(id) failure(birth==1) 

sts list , by(parity_t)
sts list , by(education_t)

* micro model: piecewise constant exponential
stsplit timetocens_split, at(0, 4, 9, 14, 19, 24) 
streg  /*
*/ i.parity_t /*
*/, dist(exponential) nohr
estimates store model1

drop timetocens_split
stjoin

* prepare aggregated event/risk-time data
recode age_t (0/19=1) (20/24=2) (25/29=3) (30/34=4) (35/39=5) (40/44=6) (45/49=7) , gen(age_t_7cat)

collapse (sum) N_size = weight_t /*
*/ if age_t < 50 & timetocens >= 1 /*
*/ , by(nuts_t parity_t gender_t education_t foreign_t age_t_7cat timetocens birth)
sort nuts_t parity_t gender_t education_t foreign_t age_t_7cat timetocens birth

/* Aggregate-level analysis: Fit a Poisson regression model */
gen id = _n
stset timetocens , id(id) failure(birth==1) 
stsplit timetocens_split, at(0, 4, 9, 14, 19, 24) 

poisson birth /*
*/ i.parity_t /*
*/ [fweight = N_size], exposure(timetocens)

estimates store model2
* estimates from individual and aggregate model should be identical
estimates table model1 model2 

drop   timetocens_split _est_model2
stjoin

* save data and send to Belgian co-authors
save "aggregate continuous-time data - Register-Based Fertility Research Network.dta" ,replace






use "Example data.dta", clear

* time_1 (please insert correct year t)
gen time_1 = 2001

*** Creation parity progression variables ***

* time 2
gen time_2 = time_1 + 4

* birth dates: create parity, lastbirth and birth_child
capture ssc install rowsort
rowsort birthyear_child1 birthyear_child2 birthyear_child3 , gen (RKIDBY1 RKIDBY2 RKIDBY3)
drop birthyear_child1 birthyear_child2 birthyear_child3

replace RKIDBY1 = . if RKIDBY1 == 9999
replace RKIDBY2 = . if RKIDBY2 == 9999
replace RKIDBY3 = . if RKIDBY3 == 9999

gen RKIDBY1_temp = RKIDBY1
replace RKIDBY1_temp = . if RKIDBY1 > time_1
gen RKIDBY2_temp = RKIDBY2
replace RKIDBY2_temp = . if RKIDBY2 > time_1
gen RKIDBY3_temp = RKIDBY3
replace RKIDBY3_temp = . if RKIDBY3 > time_1
egen parity_t = rownonmiss(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
egen lastbirth = rowmax(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
drop if parity_t > 2
drop RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp
tab lastbirth parity ,miss

gen RKIDBY1_temp = RKIDBY1
replace RKIDBY1_temp = . if RKIDBY1 <= time_1
gen RKIDBY2_temp = RKIDBY2
replace RKIDBY2_temp = . if RKIDBY2 <= time_1
gen RKIDBY3_temp = RKIDBY3
replace RKIDBY3_temp = . if RKIDBY3 <= time_1
egen birth_child = rowmin(RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp)
drop RKIDBY1_temp RKIDBY2_temp RKIDBY3_temp
replace birth_child = 9999 if birth_child == .
tab birth_child ,miss

*** discrete-time approach ***
expand (time_2 - time_1)
sort id
by id: gen pp_id = _n

gen year_tv = time_1 + pp_id
gen age_tv = age + pp_id

gen event = 0
replace event = 1 if year_tv == birth_child

drop if year_tv > death
drop if year_tv > emigration
drop if year_tv > birth_child
drop if age_tv > 49
drop if age_tv < 15

tab age_tv year_tv ,miss
recode age_tv (15/19=1) (20/24=2) (25/29=3) (30/34=4) (35/39=5) (40/44=6) (45/49=7) , gen(age_tv_7cat)

sort id year_tv
gen exposure = 9999
replace exposure = age_tv - 15 if parity_t == 0
replace exposure = (year_tv - lastbirth) - 1 if parity_t > 0

* observed discrete-time fertility schedules
by nuts_t parity_t , sort : tabulate exposure, summarize(event) nostandard noobs
by nuts_t parity_t gender_t , sort : tabulate exposure, summarize(event) nostandard noobs
by nuts_t parity_t gender_t education_t , sort : tabulate exposure, summarize(event) nostandard noobs
by nuts_t parity_t gender_t education_t foreign_t , sort : tabulate exposure, summarize(event) nostandard noobs

* individual-level discrete-time model
logit event c.exposure c.exposure#c.exposure /*
*/ i.parity_t /*
*/ , or
estimates store m_ind

* prepare aggregated event/risk-time data
collapse (sum) N_size = weight_t , by(nuts_t parity_t gender_t education_t foreign_t exposure age_tv_7cat event)
sort nuts_t parity_t gender_t education_t foreign_t age_tv_7cat exposure event

logit event c.exposure c.exposure#c.exposure /*
*/ i.parity_t /*
*/ [fweight = N_size] , or
estimates store m_agg

* estimates from individual and aggregate model should be identical
estimates table m_ind m_agg 

* save data and log file and send to Belgian co-authors
drop _*

tab event /*
*/ [fweight = N_size] ,miss
tab exposure event /*
*/ [fweight = N_size] ,miss nofreq row
save "aggregate discrete-time data - Register-Based Fertility Research Network.dta" ,replace


gen exposure_v2 = trunc(exposure/2)*2
collapse (sum) N_size_v2 = N_size  , by(nuts_t parity_t gender_t education_t foreign_t exposure_v2 age_tv_7cat event)

tab event /*
*/ [fweight = N_size_v2] ,miss
tab exposure event /*
*/ [fweight = N_size_v2] ,miss nofreq row
save "aggregate discrete-time data V2 - Register-Based Fertility Research Network.dta" ,replace

gen exposure_v3 = exposure_v2
replace exposure_v3 = 26 if exposure_v2 > 26
collapse (sum) N_size_v3 = N_size_v2  , by(nuts_t parity_t gender_t education_t foreign_t exposure_v3 age_tv_7cat event)

tab event /*
*/ [fweight = N_size_v3] ,miss
tab exposure_v3 event /*
*/ [fweight = N_size_v3] ,miss nofreq row
save "aggregate discrete-time data V3 - Register-Based Fertility Research Network.dta" ,replace



use "aggregate discrete-time data - Register-Based Fertility Research Network.dta" ,clear

recode exposure (0/4 = 1) (5/9 = 2) (10/14 = 3) (15/19 = 4) (20/24 = 5) (25/29 = 6) (30/100 = 7) ,gen(exp_first) 
recode exposure (0=0) (1=1) (2=2) (3=3) (4/5 = 4) (6/7 = 5) (8/9 = 6) (10/14 = 7) (15/24 = 8) (25/100 = 9) ,gen(exp_higher) 

gen parity_exp_cat = 9999
replace parity_exp_cat = parity_t * 100 + exp_first if parity_t == 0
replace parity_exp_cat = parity_t * 100 + exp_higher if parity_t ~= 0

collapse (sum) N_size_v4 = N_size  , by(nuts_t gender_t education_t foreign_t parity_exp_cat age_tv_7cat event)

tab  event /*
*/ [fweight = N_size_v4] ,miss
tab parity_exp_cat event /*
*/ [fweight = N_size_v4] ,miss
save "aggregate discrete-time data V4 - Register-Based Fertility Research Network.dta" ,replace

log close








