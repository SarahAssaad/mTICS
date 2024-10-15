//Dr Sarah Assaad 2023
**********************************************************************
*Creating the mTICS score from wave 8 data 

use "ELSA wave 8", clear //change the data file name as appropriate

****************************************************************
**code used and shared by Dr Dorina Cadar (HCAP1)

*******creating mtics - method based on hrs 
***************************************************************
*immediate & delayed words recall (0-10)
****************************************************************
tab1 cflisen cflisd, nol 
* recode refusal and not applicable to missing
recode cflisen cflisd (-9=.) (-1=.)
tab1 cflisen cflisd
hist cflisen
hist cflisd

****************************************************************
*serial 7's test (subtractions)
*****************************************************************
******** based on hrs - each substraction scored independently 
**don't know (dk) responses were coded as incorrect.  
* recode refusal and not applicable to missing

tab1 cfsva cfsvb cfsvc cfsvd cfsve, nol
local  7s cfsva cfsvb cfsvc cfsvd cfsve
foreach v in `7s'{
recode `v'(-8=0)(-9=.) (-1=.), gen (`v'_o)
} 

gen ser7_w8=.
replace ser7_w8=1 if cfsva_o ==93
replace ser7_w8=0 if cfsva_o !=93 & cfsva_o!=.
replace ser7_w8=ser7_w8+1 if ((cfsva_o-cfsvb_o)==7) 
replace ser7_w8=ser7_w8+1 if ((cfsvb_o-cfsvc_o)==7) 
replace ser7_w8=ser7_w8+1 if ((cfsvc_o-cfsvd_o)==7) 
replace ser7_w8=ser7_w8+1 if ((cfsvd_o-cfsve_o)==7)

egen ser7miss = rowmiss(cfsva_o cfsvb_o cfsvc_o cfsvd_o cfsve_o)
egen ser7dk = anycount(cfsva cfsvb cfsvc cfsvd cfsve),v(-8)
*respondents who refused to perform the test at the outset were assigned missing values
replace ser7_w8=. if ser7miss==5

**********************************************
*counting backward
************************************************
***original 
tab1 cfc20frst cfc20fscnd, nol

gen cfbkcount =.
recode cfbkcount (.=2) if cfc20frst==1
recode cfbkcount (.=0) if cfc20frst==2
recode cfbkcount (.=0) if cfc20frst==3
recode cfbkcount (.=1) if cfc20fscnd==1
recode cfbkcount (0=1) if cfc20fscnd==1
recode cfbkcount (.=0) if cfc20fscnd==2
recode cfbkcount (.=0) if cfc20fscnd==-8
recode cfbkcount (.=0) if cfc20frst==-8
//-8 is code for 'dont know'
tab cfbkcount, m 
tab cfc20frst cfc20fscnd, m nol

********dates ***********************
tab1 cfdatd cfdatm cfdaty cfday 

tab1 cfdatd cfdatm cfdaty cfday, nol 

recode cfdatd (2=0) (-8=0) (-9=.) (-1=.) 
recode cfdatm (2=0) (-8=0) (-9=.) (-1=.) 
recode cfdaty (2=0) (-8=0) (-9=.) (-1=.) 
recode cfday (2=0) (-8=0) (-9=.) (-1=.) 

tab1 cfdatd cfdatm cfdaty cfday 

**************************
tab1 cfnmsc cfnmca cfnmus cfnmp

tab1 cfnmsc cfnmca cfnmus cfnmp, nol

recode cfnmsc (2=0) (-8=0) (-9=.) (-1=.) 
recode cfnmca (2=0) (-8=0) (-9=.) (-1=.) 
recode cfnmus (2=0) (-8=0) (-9=.) (-1=.) 
recode cfnmp (2=0) (-8=0) (-9=.) (-1=.) 

tab1 cfnmsc cfnmca cfnmus cfnmp


**** hrs strategy (mtics-35) ************************************
egen w8mtics35qa=rowtotal(cfdatd cfdatm cfdaty cfday cflisen cflisd cfbkcount ser7_w8 cfnmsc cfnmca cfnmus cfnmp) 
egen w8nmissis35qa = rowmiss(cfdatd cfdatm cfdaty cfday cflisen cflisd cfbkcount ser7_w8  cfnmsc cfnmca cfnmus cfnmp)
replace w8mtics35qa = . if w8nmissis35qa 


**** hrs strategy (mtics-27)
egen w8mtics27qa = rowtotal(cflisen cflisd cfbkcount ser7_w8)
egen w8nmissisqa = rowmiss(cflisen cflisd cfbkcount ser7_w8)
replace w8mtics27qa = . if w8nmissisqa

hist w8mtics35qa
hist w8mtics27qa

*******************************************************************
**END of Dr Dorina Cadar stata code 

****************************************************************
***code developed by Dr Sarah Assaad

//categorisation of the mTICS27 score 
* <= 6 low cognition 
* 7 to 11 moderate cognition
* >= 12 normal cognition 

recode w8mtics27qa (0/6=1 "low cognition") (7/11 = 2 "moderate cognition") (12/max = 3 "normal cognition"), gen(w8mtics27qa_cat)

sort w8mtics27qa_cat
by w8mtics27qa_cat: sum w8mtics27qa

****************************************************************

//Compute the cognition variables

*dementia/AD variables to use
tab hedbwde hedibde, m nol // there are n = 48 cases with dementia diagnosis fed forward and n = 135 of newly reported dementia diagnoses
 
tab hedbwad hedibad, m nol // there are n = 8 cases with AD diagnosis fed forward and n = 56 of newly reported AD diagnoses

//compute the summary variable for dementia or Alzheimer's disease

gen demAD=.
label var demAD "Dementia or AD diagnosis new or fed forward w7_8"
replace demAD=1 if hedbwde==9 | hedbwad==8 | hedibde==1 | hedibad==1
recode demAD (.=5) // for all other cases with no dementia/AD new or previous
tab demAD hcap1
tab demAD HCAP_Eligible
tab  demAD w8mtics27qa_cat, row // mtics classification has high specificity 85.6% but lower sensitivity 74.2%. This is the reason why we are using mtics for classification of cases in the absence of dementia/AD diagnosis

tab demAD w8mtics27qa_cat, row m // we notice that 39% of cases with dementia in the total dataset N = 8587 have a missing score for mtics and the specificity is reduced to 53%

**********************************************************************
*generating the cognition level variables from the dementia/ AD diagnoses (new variable demAD) and mtics-27 scores 

gen cog_corr = .
replace cog_corr=1 if demAD==1 // Dementia or AD diagnoses
recode cog_corr (.=1) if w8mtics27qa_cat==1 // No Dementia/AD and low cognition based on mtics-27
recode cog_corr (.=2) if w8mtics27qa_cat==2 // No Dementia/AD and moderate cognition
recode cog_corr (.=3) if w8mtics27qa_cat==3 // No Dementia/AD and normal cognition

tab cog_corr demAD, m
tab cog_corr w8mtics27qa_cat, m

tab cog_corr, m

recode cog_corr (.=4), generate(cog_corr_miss)
label define cog_corr_label 1 "Dementia/AD or low cognition mtics-27" 2 "No Dementia/AD and moderate cognition mtics-27" 3 "No Dementia/AD and normal cognition mtics-27" 4 "No Demetia/AD and missing mtics-27"
label values cog_corr cog_corr_miss cog_corr_label

tab1 cog_corr cog_corr_miss, m

save, replace
**********************************************************************
**********************************************************************
//if we want to use mTICS27 score alone 

//generating a variable to address missing data in mtics-27 score based on scores in contributing items

recode w8mtics27qa (0/6=1 "low cognition") (7/11 = 2 "moderate cognition") (12/max = 3 "normal cognition"), gen(w8mtics27qa_cat)

sort w8mtics27qa_cat
by w8mtics27qa_cat: sum w8mtics27qa

egen w8mtics27qa_inc = rowtotal(cflisen cflisd cfbkcount ser7_w8)

gen w8mtics27qa_cat_nomiss = w8mtics27qa_cat
recode w8mtics27qa_cat_nomiss (.=1) if w8mtics27qa_inc==0 & HCAP_Issued==1 & w8mtics27qa_cat==. // low cognition
recode w8mtics27qa_cat_nomiss (.=1) if w8mtics27qa_inc==3 & HCAP_Issued==1 & w8mtics27qa_cat==. // low cognition
recode w8mtics27qa_cat_nomiss (.=1) if w8mtics27qa_inc==6 & HCAP_Issued==1 & w8mtics27qa_cat==. // low cognition
recode w8mtics27qa_cat_nomiss (.=3) if w8mtics27qa_inc>=7 & HCAP_Issued==1 & w8mtics27qa_cat==. // normal cognition 
label values w8mtics27qa_cat_nomiss w8mtics27qa_cat

*End of code.

