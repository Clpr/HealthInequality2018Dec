/* UTF-8*/
/* DATA CLEANING & AGGREGATION OF HARMONIZED CHARLS DATA */
/*Tianhao Dec 2018*/
/*********************************************/

/********************************************************/
/*1: DREAD IN DATA TO THE WORK LIBRARY (READ FROM A SPSS DATASET BECAUSE FUCKING STATA CANNOT DO ANYTHING NOT ANNOYING!)*/
PROC IMPORT DATAFILE = "E:\CHARLS_DB\harmonized 2018\H_CHARLS_C_Data.sav" OUT = WORK.HCHARLS REPLACE; RUN;

/********************************************************/
/*2. SELECT SUB-SETS OF EACH WAVE*/
/*note: 
	becuase charls has 4 waves data, but the harmonized dataset make the 4 waves together but did not use a year tag;
	we separately create 4 sub-dataset for each wave; every one has the same data structure and is added with a year column;
	and, we use the following principles:
	1. only those who responded in current wave are selected
	2. only those who were still alive when interviewed are selected
*/
PROC SQL;
	/*WAVE 1 (2011)*/
	CREATE TABLE WORK.W1 AS SELECT
		2011 AS YEAR, *
		FROM WORK.HCHARLS
		WHERE INW1 = 1 AND R1IWSTAT = 1; /*INWx marks if the agent took part in the current wave; 1 == TRUE; RwIWSTAT marks if the agent were alive in this wave & responded, 1 == TRUE */
	/*WAVE 2 (2013)*/
	CREATE TABLE WORK.W2 AS SELECT
		2013 AS YEAR, *
		FROM WORK.HCHARLS
		WHERE INW1 = 1 AND R2IWSTAT = 1;
	/*WAVE 3 (2014)*/
	CREATE TABLE WORK.W3 AS SELECT
		2014 AS YEAR, *
		FROM WORK.HCHARLS
		WHERE INW1 = 1 AND R3IWSTAT = 1;
	/*WAVE 4 (2015)*/
	CREATE TABLE WORK.W4 AS SELECT
		2015 AS YEAR, *
		FROM WORK.HCHARLS
		WHERE INW1 = 1 AND R4IWSTAT = 1;


/********************************************************/
/*3. SELECT CANDIDATE VARIABLES IN EACH SUB-SET*/
/*note;
	in this section, we select candidate variables from each sub-set;
	because the time dimension of the harmonized dataset was fucked to multi-columns,
	we have no way but to separately operate on 3 sub-sets. (because the Wave 3 did not report the HEALTH SECTION)

	We select the following candidate variables (# is the number of wave):
	1. Demography & tags:
		1. ID, HOUSEHOLDID, COMMUNITYID
		2. (city, province etc need to be added later, according to another mapping of community information)
		3. R#AGEY (age at interview), S#GENDER (gender at interview, 1 for male, 2 for female)
		4. S#EDUCL (harnomized education level, if need names, use S#EDUC_C)
		5. R#MSTAT (marital status, 1 is married, 3 is parterned), R#MCURLN (length of current marriage, pls note: Wave1,Wave2,Wave3 are recorded, but Wave4 = Wave3 + 1(approximately equal))
		6. R#HUKOU (hukou (residence type) status, 1 for agricultural, 2 for non-agricultural), H#RURAL (if the COMMUNITY is urban, 0 for urban, 1 for rural)
	2. Health (DOCTER SAID, EVER HAD) note: we do not care if the respondant changed their answer at last interview
		1. R#HIBPE (high blood pressure, 1 for yes 0 for no)
		2. R#DIABE (diabetes)
		3. R#CANCRE (cancer)
		4. R#LUNGE (lung disease)
		5. R#HEARTE (heart problem)
		6. R#STROKE (stroke)
		7. R#PSYCHE (psych problem)
		8. R#ARTHRE (arthritis)
		9. R#DYSLIPE (dyslipidemia)
		10. R#LIVERE (liver disease)
		11. R#KIDNEYE (kidney disease)
		12. R#DIGESTE (stomach/digestive disease)
		13. R#ASTHMAE (asthma)
	3. Health (Other)
		1. R#BMI (BMI index, body mass index, larger -> fatter, = weight(km) / height^2(m^2)
		2. R#DRINKL (ever drinked in the last year, 0 for no, 1 for yes)
		3. R#SMOKEV (smoke ever), R#SMOKEN (smoke now), R#SMOKEF (cigarettes per day)
	4. Health Care & Insurance
		1. R#HOSP1Y (flag: hospital stay last year, 1 for yes, 0 for no), R#HSPTIM1Y (how many times in hospital), R#HSPNITE (how many nights in hospital of most recent in-hospital)
		2. R#DOCTOR1M (flag: doctor visit/outpatient last month, no Chinese medicine), R#DOCTIM1M (how many times of these visits)
		3. R#OOPHOS1Y (out-of-pocket expenditure of hospitalization last year, in yuan),
		4. R#OOPDOC1M (out-of-pocket expenditure of outpatient last month, in yuan)
		5. R#HIGOV (if covered by a public (health) insurance plan, 1 for yes, 0 for no), R#HIPRIV (if covered by a private (health) insurance plan, 1 for yes, 0 for no), R#HIOTHP (if covered by any other plan)
	5. Financial and Housing Wealth
		1. H#ACHCK (Individual value of  CASH & SAVINGS, cash + checking + saving, in yuan)
		2. H#ASTCK (Individual value of stocks and mutual funds)
		3. H#ABOND (Individual value of government bonds)
		4. H#AOTHR (Individual value of all other savings)
		5. HH1ATOTB/HH2ATOTB/H4ATOTB (household Total Wealth) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!(DIFF AMONG WAVES!)
	6. Income
		1. R#ITEARN (earned income last year, wage + bonus)
		2. R#ITSEMP (self-employment after tax last year)
		3. R#ITREST (financial assets income before tax, last year)
		4. R#IPEN (pension income last year)
		5. HH#IGXFR (governemnt/public transfer income of the HOUSEHOLD)
		6. R#IOTHR  (other income)
		7. HH#IOTHHH (income from other houshold members)
		8. HH#ITOT (total household income)
	7. Consumption
		1. HH#CFOOD (household food consumption, last 7 days)
		2. HH#CTOT (total household consumption, recalculated as annual)
	8. Family Structure
		1. H#CORESD (whether any child co-resides with respondent, 1 for yes, 0 for no)
		2. H#LVNEAR (whether any child live near, 1 for yes, 0 for no)
		3. R#SOCWK (participate in social activities, 1 for yes, 0 for no)
		4. H#FCANY (flag, any transfer from children/grandchildren, 1 for yes, 0 for no)
		5. H#FCAMT (amount, amount of transfers from children/grandchildren in the past year)
		6. H#FREC (total amount of transfers received in the past year)
	9. Employment history
		1. R#WORK (flag, still working, 1 for yes, 0 for no)
		2. R#LBRF_C (labor force status: 1 for agri, 2 for non-agri empolyed, 3 for non-agri self-employed, 4 for non-agri family business, 5 for unemployed, 6 for retired, 7 for never work)
	10. Retirement
		1. R#RETEMP (flag, whether retired, 1 for yes, 0 for no)
		2. R#FRET_C (flag, whether FORMALLY retired, 1 for yes, 0 for no)
	11. Pension
		1. (non included, for now)


*/
PROC SQL;
	/*WAVE 1 (2011)*/
	CREATE TABLE WORK.SUBW1 AS SELECT
		YEAR,
		/**** demography & background ****/
		ID, HOUSEHOLDID, COMMUNITYID,
		R1AGEY AS AGE, /*AGE AT INTERVIEW*/
		RAGENDER AS GENDER,  /*GENDER, 1 FOR MALE, 2 FOR FEMALE*/
		S1EDUCL AS EDU,  /*HARMONIZED EDUCATION LEVEL (NUMBER)*/
		R1MSTAT AS MARITAL, /*marital status*/
		R1MCURLN AS LEN_MARITAL, /*length of current marriage*/
		R1HUKOU AS HUKOU,  /*HUKOU STATUS*/
		H1RURAL AS URBAN,  /*if the community is defined as urban, 0 for uabn, 1 for rural*/
		/**** chronic diseases (ever had, told by doctor) ****/
		R1HIBPE AS EVER_HIBPE, R1DIABE AS EVER_DIABE, R1CANCRE AS EVER_CANCRE,
		R1LUNGE AS EVER_LUNGE, R1HEARTE AS EVER_HEARTE, R1STROKE AS EVER_STROKE,
		R1PSYCHE AS EVER_PSYCHE, R1ARTHRE AS EVER_ARTHRE, R1DYSLIPE AS EVER_DYSLIPE,
		R1LIVERE AS EVER_LIVERE, R1KIDNEYE AS EVER_KIDNEYE, R1DIGESTE AS EVER_DIGESTE, R1ASTHMAE AS EVER_ASTHMAE,
		/******** other health indicators ***********/
		R1BMI AS BMIIDX,  /*BMI INDEX*/
		R1DRINKL AS DRINK1Y_FLAG, /*flag, drinked last year? 1 for yes, 0 for no*/
		R1SMOKEV AS SMOKE_EVER, R1SMOKEN AS SMOKE_NOW, R1SMOKEF AS SMOKE_NUM,  /*smoking*/
		/******** Health Care & Insurance **********/
		R1HOSP1Y AS HOSP1Y_FLAG, R1HSPTIM1Y AS HOSP1Y_NUM, R1HSPNITE AS HOSPNOX_LAST,  /*in hospital*/
		R1DOCTOR1M AS OUTP1M_FLAG, R1DOCTIM1M AS OUTP1M_NUM,  /*outpatient*/
		R1OOPHOS1Y AS HOSP1Y_REALEXP, R1OOPDOC1M AS OUTP1M_REALEXP,  /*outpatient & in-hospital expenditure (out-of-pocket)*/
		R1HIGOV AS INSGOV_FLAG, R1HIPRIV AS INSPRI_FLAG, R1HIOTHP AS INSOTH_FLAG, /*if covered by any public or private or any other insurance plan, e.g. UE-BMI*/
		/******** Financial and Housing Wealth **********/
		H1ACHCK AS INDIASSET_CASHSAVE,  /*individual cash & saving*/
		H1ASTCK AS INDIASSET_CAPITAL,  /*stock & mutual funds*/
		H1ABOND AS INDIASSET_GOVBOND, /*gov bond*/
		H1AOTHR AS INDIASSET_OTHER,  /*other kinds of asset*/
		HH1ATOTB AS HOUSASSET_TOTAL,  /*househodl total wealth  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!(DIFF AMONG WAVES!)  */
		/******** Income *******************/
		R1ITEARN AS INCOME_EARN,   /*earned income*/
		R1ITSEMP AS INCOME_SELF,  /*self-employed*/
		HH2ICAP AS HOUSINCOME_CAPT,  /*total household capital income*/
		R1IPEN AS INCOME_PENSION,  /*total pension income*/
		HH1IGXFR AS INCOME_TRANS,  /*gov/public transfer payments*/
		R1IOTHR AS INCOME_OTHER,  /*other incomes*/
		HH1IOTHHH AS INCOME_FAMILY,  /*income from other household members*/
		HH1ITOT AS HOUSINCOME_TOTAL,  /*total household income*/
		/******** Consumption *******************/
		HH1CFOOD AS EXP1W_FOOD,  /*food consumption last week*/
		HH1CTOT AS EXP1Y_TOTAL,  /*household total consumption, (including medical), 1 year*/
		/******** Family Structure *****************/
		H1CORESD AS CORESD_FLAG,  /*whether any child co-resides with respondent*/
		H1LVNEAR AS LVNEAR_FLAG,  /*whther a child lives near, 1 for yes, 0 for no*/
		R1SOCWK AS SOCWK_FLAG,   /*participate in social activities, 1 for yes, 0 for no*/
		H1FCANY AS TRANSCHILD_FLAG, /*flag, any transfer from children/grandchildren, 1 for yes, 0 for no*/
		H1FCAMT AS TRANSCHILD_AMT, /*amount, amount of transfers from children/grandchildren*/
		H1FREC AS TRANSPAY_TOTAL,  /*total amount of transfers received in the past year*/
		/******* Employment history ***********/
		R1WORK AS WORK_FLAG,  /*still work? 1 for yes, 0 for no*/
		R1LBRF_C AS JOBSTATUS,  /*types of jobs, category*/
		/******* Retirement *****************/
		R1RETEMP AS RETIRE_FLAG,  /*retired? 1 for yes, 0 for no*/
		R1FRET_C AS RETIRE_FORMAL_FLAG  /*formally retired? 1 for yes, 0 for no*/

		FROM WORK.W1;

	/*WAVE 2 (2012)*/
	CREATE TABLE WORK.SUBW2 AS SELECT
		YEAR,
		/**** demography & background ****/
		ID, HOUSEHOLDID, COMMUNITYID,
		R2AGEY AS AGE, /*AGE AT INTERVIEW*/
		RAGENDER AS GENDER,  /*GENDER, 1 FOR MALE, 2 FOR FEMALE*/
		S2EDUCL AS EDU,  /*HARMONIZED EDUCATION LEVEL (NUMBER)*/
		R2MSTAT AS MARITAL, /*marital status*/
		R2MCURLN AS LEN_MARITAL, /*length of current marriage*/
		R2HUKOU AS HUKOU,  /*HUKOU STATUS*/
		H2RURAL AS URBAN,  /*if the community is defined as urban, 0 for uabn, 1 for rural*/
		/**** chronic diseases (ever had, told by doctor) ****/
		R2HIBPE AS EVER_HIBPE, R2DIABE AS EVER_DIABE, R2CANCRE AS EVER_CANCRE,
		R2LUNGE AS EVER_LUNGE, R2HEARTE AS EVER_HEARTE, R2STROKE AS EVER_STROKE,
		R2PSYCHE AS EVER_PSYCHE, R2ARTHRE AS EVER_ARTHRE, R2DYSLIPE AS EVER_DYSLIPE,
		R2LIVERE AS EVER_LIVERE, R2KIDNEYE AS EVER_KIDNEYE, R2DIGESTE AS EVER_DIGESTE, R2ASTHMAE AS EVER_ASTHMAE,
		/******** other health indicators ***********/
		R2BMI AS BMIIDX,  /*BMI INDEX*/
		R2DRINKL AS DRINK1Y_FLAG, /*drinked last year*/
		R2SMOKEV AS SMOKE_EVER, R2SMOKEN AS SMOKE_NOW, R2SMOKEF AS SMOKE_NUM,  /*smoking*/
		/******** Health Care & Insurance **********/
		R2HOSP1Y AS HOSP1Y_FLAG, R2HSPTIM1Y AS HOSP1Y_NUM, R2HSPNITE AS HOSPNOX_LAST,  /*in hospital*/
		R2DOCTOR1M AS OUTP1M_FLAG, R2DOCTIM1M AS OUTP1M_NUM,  /*outpatient*/
		R2OOPHOS1Y AS HOSP1Y_REALEXP, R2OOPDOC1M AS OUTP1M_REALEXP,  /*outpatient & in-hospital expenditure (out-of-pocket)*/
		R2HIGOV AS INSGOV_FLAG, R2HIPRIV AS INSPRI_FLAG, R2HIOTHP AS INSOTH_FLAG, /*if covered by any public or private or any other insurance plan, e.g. UE-BMI*/
		/******** Financial and Housing Wealth **********/
		H2ACHCK AS INDIASSET_CASHSAVE,  /*individual cash & saving*/
		H2ASTCK AS INDIASSET_CAPITAL,  /*stock & mutual funds*/
		H2ABOND AS INDIASSET_GOVBOND, /*gov bond*/
		H2AOTHR AS INDIASSET_OTHER,  /*other kinds of asset*/
		HH2ATOTB AS HOUSASSET_TOTAL,  /*househodl total wealth  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!(DIFF AMONG WAVES!)  */
		/******** Income *******************/
		R2ITEARN AS INCOME_EARN,   /*earned income*/
		R2ITSEMP AS INCOME_SELF,  /*self-employed*/
		HH2ICAP AS HOUSINCOME_CAPT,  /*total household capital income*/
		R2IPEN AS INCOME_PENSION,  /*total pension income*/
		HH2IGXFR AS INCOME_TRANS,  /*gov/public transfer payments*/
		R2IOTHR AS INCOME_OTHER,  /*other incomes*/
		HH2IOTHHH AS INCOME_FAMILY,  /*income from other household members*/
		HH2ITOT AS HOUSINCOME_TOTAL,  /*total household income*/
		/******** Consumption *******************/
		HH2CFOOD AS EXP1W_FOOD,  /*food consumption last week*/
		HH2CTOT AS EXP1Y_TOTAL,  /*household total consumption, (including medical), 1 year*/
		/******** Family Structure *****************/
		H2CORESD AS CORESD_FLAG,  /*whether any child co-resides with respondent*/
		H2LVNEAR AS LVNEAR_FLAG,  /*whther a child lives near, 1 for yes, 0 for no*/
		R2SOCWK AS SOCWK_FLAG,   /*participate in social activities, 1 for yes, 0 for no*/
		H2FCANY AS TRANSCHILD_FLAG, /*flag, any transfer from children/grandchildren, 1 for yes, 0 for no*/
		H2FCAMT AS TRANSCHILD_AMT, /*amount, amount of transfers from children/grandchildren*/
		H2FREC AS TRANSPAY_TOTAL,  /*total amount of transfers received in the past year*/
		/******* Employment history ***********/
		R2WORK AS WORK_FLAG,  /*still work? 1 for yes, 0 for no*/
		R2LBRF_C AS JOBSTATUS,  /*types of jobs, category*/
		/******* Retirement *****************/
		R2RETEMP AS RETIRE_FLAG,  /*retired? 1 for yes, 0 for no*/
		R2FRET_C AS RETIRE_FORMAL_FLAG  /*formally retired? 1 for yes, 0 for no*/

		FROM WORK.W2;

	/*WAVE 4 (2015)*/
	CREATE TABLE WORK.SUBW4 AS SELECT
		YEAR,
		/**** demography & background ****/
		ID, HOUSEHOLDID, COMMUNITYID,
		R4AGEY AS AGE, /*AGE AT INTERVIEW*/
		RAGENDER AS GENDER,  /*GENDER, 1 FOR MALE, 2 FOR FEMALE*/
		S4EDUCL AS EDU,  /*HARMONIZED EDUCATION LEVEL (NUMBER)*/
		R4MSTAT AS MARITAL, /*marital status*/
		R3MCURLN + 2 AS LEN_MARITAL, /*length of current marriage, based on Wave3, add 2 to convert it from Wave3(2014) to Wave4(2015)*/
		R4HUKOU AS HUKOU,  /*HUKOU STATUS*/
		H4RURAL AS URBAN,  /*if the community is defined as urban, 0 for uabn, 1 for rural*/
		/**** chronic diseases (ever had, told by doctor) ****/
		R4HIBPE AS EVER_HIBPE, R4DIABE AS EVER_DIABE, R4CANCRE AS EVER_CANCRE,
		R4LUNGE AS EVER_LUNGE, R4HEARTE AS EVER_HEARTE, R4STROKE AS EVER_STROKE,
		R4PSYCHE AS EVER_PSYCHE, R4ARTHRE AS EVER_ARTHRE, R4DYSLIPE AS EVER_DYSLIPE,
		R4LIVERE AS EVER_LIVERE, R4KIDNEYE AS EVER_KIDNEYE, R4DIGESTE AS EVER_DIGESTE, R4ASTHMAE AS EVER_ASTHMAE,
		/******** other health indicators ***********/
		R4BMI AS BMIIDX,  /*BMI INDEX*/
		R4DRINKL AS DRINK1Y_FLAG, /*drinked last year*/
		R4SMOKEV AS SMOKE_EVER, R4SMOKEN AS SMOKE_NOW, R4SMOKEF AS SMOKE_NUM,  /*smoking*/
		/******** Health Care & Insurance **********/
		R4HOSP1Y AS HOSP1Y_FLAG, R4HSPTIM1Y AS HOSP1Y_NUM, R4HSPNITE AS HOSPNOX_LAST,  /*in hospital*/
		R4DOCTOR1M AS OUTP1M_FLAG, R4DOCTIM1M AS OUTP1M_NUM,  /*outpatient*/
		R4OOPHOS1Y AS HOSP1Y_REALEXP, R4OOPDOC1M AS OUTP1M_REALEXP,  /*outpatient & in-hospital expenditure (out-of-pocket)*/
		R4HIGOV AS INSGOV_FLAG, R4HIPRIV AS INSPRI_FLAG, R4HIOTHP AS INSOTH_FLAG, /*if covered by any public or private or any other insurance plan, e.g. UE-BMI*/
		/******** Financial and Housing Wealth **********/
		H4ACHCK AS INDIASSET_CASHSAVE,  /*individual cash & saving*/
		H4ASTCK AS INDIASSET_CAPITAL,  /*stock & mutual funds*/
		H4ABOND AS INDIASSET_GOVBOND, /*gov bond*/
		H4AOTHR AS INDIASSET_OTHER,  /*other kinds of asset*/
		H4ATOTB AS HOUSASSET_TOTAL,  /*househodl total wealth  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!(DIFF AMONG WAVES!)  */
		/******** Income *******************/
		R4ITEARN AS INCOME_EARN,   /*earned income*/
		R4ITSEMP AS INCOME_SELF,  /*self-employed*/
		HH4ICAP AS HOUSINCOME_CAPT,  /*total household capital income*/
		R4IPEN AS INCOME_PENSION,  /*total pension income*/
		HH4IGXFR AS INCOME_TRANS,  /*gov/public transfer payments*/
		R4IOTHR AS INCOME_OTHER,  /*other incomes*/
		HH4IOTHHH AS INCOME_FAMILY,  /*income from other household members*/
		HH4ITOT AS HOUSINCOME_TOTAL,  /*total household income*/
		/******** Consumption *******************/
		HH4CFOOD AS EXP1W_FOOD,  /*food consumption last week*/
		HH4CTOT AS EXP1Y_TOTAL,  /*household total consumption, (including medical), 1 year*/
		/******** Family Structure *****************/
		H4CORESD AS CORESD_FLAG,  /*whether any child co-resides with respondent*/
		H4LVNEAR AS LVNEAR_FLAG,  /*whther a child lives near, 1 for yes, 0 for no*/
		R4SOCWK AS SOCWK_FLAG,   /*participate in social activities, 1 for yes, 0 for no*/
		H4FCANY AS TRANSCHILD_FLAG, /*flag, any transfer from children/grandchildren, 1 for yes, 0 for no*/
		H4FCAMT AS TRANSCHILD_AMT, /*amount, amount of transfers from children/grandchildren*/
		H4FREC AS TRANSPAY_TOTAL,  /*total amount of transfers received in the past year*/
		/******* Employment history ***********/
		R4WORK AS WORK_FLAG,  /*still work? 1 for yes, 0 for no*/
		R4LBRF_C AS JOBSTATUS,  /*types of jobs, category*/
		/******* Retirement *****************/
		R4RETEMP AS RETIRE_FLAG,  /*retired? 1 for yes, 0 for no*/
		R4FRET_C AS RETIRE_FORMAL_FLAG  /*formally retired? 1 for yes, 0 for no*/

		FROM WORK.W4;

/*******************************************************************************/
/*4. DELETE OLD TABLES*/
PROC SQL;
	DROP TABLE WORK.W1, WORK.W2, WORK.W3, WORK.W4;


/*******************************************************************************/
/*5. MERGE TO A PANEL TABLE*/
DATA WORK.W124;
	SET WORK.SUBW1 WORK.SUBW2 WORK.SUBW4;
RUN;

	
/*******************************************************************************/
/*6. DATA PROCESS: VARIABLE BY VARIABLE*/

DATA WORK.CHARLS_W124;
	SET WORK.W124;
/*1. GENDER: now we let 1 for male, 0 for female**************/
	IF GENDER = 1 THEN GENDER = 1;
	IF GENDER = 2 THEN GENDER = 0;
	FORMAT GENDER $8. ; /*drop text tags (for R using)************/
/*2. EDU: format it to integer, delete tags*/
	FORMAT EDU $8. ;
/*3. MARITAL: drop tags, make (1 married & 3 parterned) to 1, else to 0*******/
	FORMAT MARITAL $8. ;
	IF MARITAL <=3 THEN MARITAL = 1;
	IF MARITAL >3 THEN MARITAL = 0;  /*two single if clauses to avoid affecting missing values!*/
/*4. HUKOU: drop tags, make (2 non-agri) to 1, else (1 agri, 3 undefied, 4 no hukou) to 0**********/
	FORMAT HUKOU $8. ;
	IF HUKOU = 2 THEN HUKOU = 1;
	IF (HUKOU = 1 | HUKOU = 3 | HUKOU = 4) THEN HUKOU = 0;
/*5. URBAN: drop tags, make (0 urban) to 1, make (1 rural) to 0*/
	FORMAT URBAN $8. ;
	URBAN = 1 - URBAN;
/*6. CHRONIC: combine all 13 kinds of chronic diseases to 1 single flag variable: CHRONIC_FLAG, then, drop the 13 single variables*/
	/*the new variable CHRONIC_FLAG indicates whether a respondent has any of the 13 kinds of chornic diseases*/
	/*and another new vairable CHRONIC_NUM indicates how many kinds of chronic diseases a respondent has*/
	/*note: but first, we make missing values in the 13 variables as 0 (we assume if respondents did not report one, they did not have the disease)*/
	IF MISSING(EVER_HIBPE) = 1 THEN EVER_HIBPE = 0;
	IF MISSING(EVER_DIABE) = 1 THEN EVER_DIABE = 0;
	IF MISSING(EVER_CANCRE) = 1 THEN EVER_CANCRE = 0;
	IF MISSING(EVER_LUNGE) = 1 THEN EVER_LUNGE = 0;
	IF MISSING(EVER_HEARTE) = 1 THEN EVER_HEARTE = 0;
	IF MISSING(EVER_STROKE) = 1 THEN EVER_STROKE = 0;
	IF MISSING(EVER_PSYCHE) = 1 THEN EVER_PSYCHE = 0;
	IF MISSING(EVER_ARTHRE) = 1 THEN EVER_ARTHRE = 0;
	IF MISSING(EVER_DYSLIPE) = 1 THEN EVER_DYSLIPE = 0;
	IF MISSING(EVER_LIVERE) = 1 THEN EVER_LIVERE = 0;
	IF MISSING(EVER_KIDNEYE) = 1 THEN EVER_KIDNEYE = 0;
	IF MISSING(EVER_DIGESTE) = 1 THEN EVER_DIGESTE = 0;
	IF MISSING(EVER_ASTHMAE) = 1 THEN EVER_ASTHMAE = 0;
	CHRONIC_NUM = SUM( EVER_HIBPE, EVER_DIABE, EVER_CANCRE, EVER_LUNGE, EVER_HEARTE, EVER_STROKE, EVER_PSYCHE, EVER_ARTHRE, EVER_DYSLIPE, EVER_LIVERE, EVER_KIDNEYE, EVER_DIGESTE, EVER_ASTHMAE );
	IF CHRONIC_NUM > 0 THEN CHRONIC_FLAG = 1; ELSE CHRONIC_FLAG = 0; /*because we filled missing values of every sub-question with 0, then the new variable must have no missing value*/
	DROP EVER_HIBPE EVER_DIABE EVER_CANCRE EVER_LUNGE EVER_HEARTE EVER_STROKE EVER_PSYCHE EVER_ARTHRE EVER_DYSLIPE EVER_LIVERE EVER_KIDNEYE EVER_DIGESTE EVER_ASTHMAE; /*drop sub-questions*/
/*7. DRINK1Y_FLAG: drop tags, now 1 for yes, 0 for no*/
	FORMAT DRINK1Y_FLAG $8. ;
/*8. SMOKE_EVER, SMOKE_NOW: drop tags, now 1 for yes, 0 for no*/
	FORMAT SMOKE_EVER $8. SMOKE_NOW $8. ;
/*9. HOSP1Y_FLAG, OUTP1M_FLAG, INSGOV_FLAG, INSPRI_FLAG, INSOTH_FLAG: drop tags, now 1 for yes, 0 for no*/
	FORMAT HOSP1Y_FLAG $8. OUTP1M_FLAG $8. INSGOV_FLAG $8. INSPRI_FLAG $8. INSOTH_FLAG $8. ;
/*10. INSGOV_FLAG, INSPRI_FLAG, INSOTH_FLAG: define a new variable INSURANCE_FLAG to indicate whether a respondent is covered by any kind of health insurances, 1 for yes, 0 for no */
	/*if any of the 3 vars missing, make them 0 (we assume the respondents are not covered by this kind of insurances)*/
	IF MISSING(INSGOV_FLAG) = 1 THEN INSGOV_FLAG = 0;
	IF MISSING(INSPRI_FLAG) = 1 THEN INSPRI_FLAG = 0;
	IF MISSING(INSOTH_FLAG) = 1 THEN INSOTH_FLAG = 0;
	INSURANCE_FLAG = (INSGOV_FLAG + INSPRI_FLAG + INSOTH_FLAG) > 0;
/*11.INDIASSET_CASHSAVE, INDIASSET_CAPITAL, INDIASSET_GOVBOND, INDIASSET_OTHER: define a new variable INDIASSET_TOTAL, total individual assets */
	/*if any of the 3 vars missing, make them 0*/
	IF MISSING(INDIASSET_CASHSAVE) = 1 THEN INDIASSET_CASHSAVE = 0;
	IF MISSING(INDIASSET_CAPITAL) = 1 THEN INDIASSET_CAPITAL = 0;
	IF MISSING(INDIASSET_GOVBOND) = 1 THEN INDIASSET_GOVBOND = 0;
	IF MISSING(INDIASSET_OTHER) = 1 THEN INDIASSET_OTHER = 0;
	INDIASSET_TOTAL = SUM(INDIASSET_CASHSAVE, INDIASSET_CAPITAL, INDIASSET_GOVBOND, INDIASSET_OTHER);
/*12.INCOME_EARN, INCOME_SELF, INCOME_PENSION, INCOME_TRANS, INCOME_OTHER, INCOME_FAMILY: aggregate them to a new variable INDIINCOME_TOTAL, the total individual incomes in the last year */
	IF MISSING(INCOME_EARN) = 1 THEN INCOME_EARN = 0;
	IF MISSING(INCOME_SELF) = 1 THEN INCOME_SELF = 0;
	IF MISSING(INCOME_PENSION) = 1 THEN INCOME_PENSION = 0;
	IF MISSING(INCOME_TRANS) = 1 THEN INCOME_TRANS = 0;
	IF MISSING(INCOME_OTHER) = 1 THEN INCOME_OTHER = 0;
	IF MISSING(INCOME_FAMILY) = 1 THEN INCOME_FAMILY = 0;
	INDIINCOME_TOTAL = SUM( INCOME_EARN, INCOME_SELF, INCOME_PENSION, INCOME_TRANS, INCOME_OTHER, INCOME_FAMILY );
/*13. CORESD_FLAG, LVNEAR_FLAG: define a new variable CHILDCARE_FLAG to indicate whether the respondent receives daily care from any child live together or nearby*/
	IF MISSING(CORESD_FLAG) = 1 THEN CORESD_FLAG = 0;
	IF MISSING(LVNEAR_FLAG) = 1 THEN LVNEAR_FLAG = 0;
	CHILDCARE_FLAG = (CORESD_FLAG + LVNEAR_FLAG) > 0;
/*14. SOCWK_FLAG, TRANSCHILD_FLAG: drop tags*/
	FORMAT SOCWK_FLAG $8. TRANSCHILD_FLAG $8. ;
/*15. WORK_FLAG: drop tags, 1 for working, 0 for retired or unemployed*/
/*16. RETIRE_FLAG, RETIRE_FORMAL_FLAG: drop tags, 1 for retired, 0 for working*/
	FORMAT WORK_FLAG $8. RETIRE_FLAG $8. RETIRE_FORMAL_FLAG $8. ;
/*17 JOB_STATUS: (labor force status: 1 for agri, 2 for non-agri empolyed, 3 for non-agri self-employed, 4 for non-agri family business, 5 for unemployed, 6 for retired, 7 for never work)*/
	


RUN;




/**********************************************************************/
/*7. MERGE: COMMUNITY ID WITH COMMUNITY INFORMATION */
/*note:
	in this section, we add information of community (e.g. city, province, county) to the dataset
*/
/*7.1 read in id tables in different years*/
PROC IMPORT DATAFILE = "E:\CHARLS_DB\harmonized 2018\id_2012.xlsx" OUT = WORK.IDW12 REPLACE;RUN; /*the fucking ID contains 0 at the 1st digit; requires much extra work*/
PROC IMPORT DATAFILE = "E:\CHARLS_DB\harmonized 2018\id_2013.xlsx" OUT = WORK.IDW34 REPLACE;RUN; /*string ID, good*/
DATA WORK.IDW12;
	SET WORK.IDW12;
	FORMAT communityID z7.0;
	communityID2 = INPUT(communityID, $14. );  /*convert number ID to characters (cosistent with other datasets)*/
	TMP = LENGTH(communityID2);
	IF TMP = 6 THEN communityID2 = "0" || communityID2;  /*make up*/
	DROP communityID TMP;
	RENAME communityID2 = communityID;
RUN;
/*7.2 merge the two id tables, drop duplicates (if applicable)*/
DATA WORK.IDW124;
	SET WORK.IDW12 WORK.IDW34;
RUN;





		

		
/**********************************************************************/
/*8. MERGE: ID INFORMATION WITH CHARLS_W124 */
PROC SQL;
	CREATE TABLE WORK.CHARLS_W124 AS SELECT DISTINCT
		* FROM WORK.CHARLS_W124 LEFT JOIN WORK.IDW124
		ON CHARLS_W124.communityID = IDW124.communityID;
	DROP TABLE WORK.SUBW1, WORK.SUBW2, WORK.SUBW4, WORK.W124, WORK.IDW12, WORK.IDW34;  /*drop intermediate tables*/



/*well, every observation is well matched. no observation lost*/





/**********************************************************************/
/*8. AGGREGATION: COMMUNITY LEVEL */
/*note:
	in this section, we aggregate the dataset CHARLS_w124 at community level, output to CHARLS_COMM (still a panel);
	note: all missing values are not counted, e.g. avg([1,miss,2]) = 1.5 rather than 1
*/
PROC SQL;
	CREATE TABLE WORK.CHARLS_COMM AS SELECT DISTINCT
		YEAR, COMMUNITYID, CITY, PROVINCE,  /*coding information & background information*/
		/***** demography ******/
		AVG(AGE) AS AVGAGE,  /*AVERAGE AGE*/
		AVG(GENDER) AS MALE_RATIO,  /*RATIO OF MALE POPULATION*/
		AVG(EDU) AS AVGEDU,  /*AVERAGE EDU LEVEL*/
		AVG(MARITAL) AS MARITAL_RATIO,  /*RATIO OF MARRIED OR PARTERNERED POPULATION*/
		AVG(LEN_MARITAL) AS MARITAL_AVELEN,  /*AVERAGE YEARS OF CURRENT OR LATEST MARRIAGE*/
		AVG(HUKOU) AS NONAGRIHUKOU_RATIO,  /*RATIO OF THE POPULATION WITH NON-AGRICULTURAL HUKOU (LEGAL RESIDENCE STATUS)*/
		AVG(URBAN) AS URBAN_RATIO,  /*RATIO OF THE POPULATION LIVING IN AN URBAN AREA/COMMUNITY*/
		/***** health ********/
		AVG(BMIIDX) AS AVGBMI,  /*AVERAGE LEVEL OF THE RESPONDENTS' BMI INDEX (BODY MASS INDEX)*/
		AVG(DRINK1Y_FLAG) AS DRINK1Y_RATIO,  /*RATIO OF THE POPULATION WHO EVER DROKE AT LEAST ONE TIME IN THE PAST YEAR*/
		AVG(SMOKE_EVER) AS SMOKEEVER_RATIO,  /*RATIO FO THE POPULATION WHO EVER SMOKED*/
		AVG(SMOKE_NOW) AS SMOKENOW_RATIO, /*RATIO OF THE POPULATIOIN WHO SMOKE NOW*/
		AVG(SMOKE_NUM) AS AVGSMOKENUM,  /*AVERAGE NUMBER OF CIGARETTES SMOKED PER DAY*/
		/**** chronic diseases ******/
		AVG(CHRONIC_FLAG) AS CHRONIC_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ON OF THE 13 KINDS OF CHRONIC DISEASES*/
		AVG(CHRONIC_NUM) AS AVGCHRONIC_NUM,  /*AVERAGE TYPES OF CHRONIC DISEASES SUFFERED BY RESPONDENTS, PER CAPITA*/
		/**** medical cost *****/
		AVG(HOSP1Y_FLAG) AS HOSP1Y_RATIO, /*RATIO OF THE POPULATION WHO EVER RECEIVED IN-HOSPITAL TREATMENT IN THE PAST 1 YEAR*/
		AVG(HOSP1Y_NUM) AS AVGHOSP1Y_TIMES, /*AVERAGE TIMES OF RECEIVING IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(HOSPNOX_LAST) AS AVGHOSPNOX_LAST,  /*AVERAGE OF NIGHTS IN HOSPITAL WHEN RECEIVING THE MOST RECENT IN-HOSPITAL TREATMENT*/
		AVG(OUTP1M_FLAG) AS OUTP1M_RATIO, /*RATIO OF THE POPULATION WHO EVER VISITED DOCTORS OR RECEIVED OUTPATIENT TREATMENTS IN THE LAST 1 MONTH*/
		AVG(OUTP1M_NUM) AS AVGOUTP1M_TIMES, /*AVERAGE TIMES OF RECEIVING OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		AVG(HOSP1Y_REALEXP) AS AVGHOSP1Y_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(OUTP1M_REALEXP) AS AVGOUTP1M_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		/**** health insurance plans ****/
		AVG(INSURANCE_FLAG) AS INSURANCE_RATIO,  /*RATIO OF THE POPULATION COVERED BY ANY KIND OF HEALTH INSURANCE PLANS*/
		AVG(INSGOV_FLAG) AS INSGOV_RATIO,  /*RATIO OF THE POPULATION COVERED BY GOVERNEMNT OR PUBLIC HEALTH INSURANCE PLANS*/
		AVG(INSPRI_FLAG) AS INSPRI_RATIO,  /*RATIO OF THE POPULATION COVERED BY PRIVATE HEALTH INSURANCE PLANS*/
		AVG(INSOTH_FLAG) AS INSOTH_RATIO,  /*RATIO OF THE POPULATION COVERED BY OTHER KINDS OF HEALTH INSURANCE PLANS*/
		/**** asset ****/
		AVG(HOUSASSET_TOTAL) AS AVGHOUSASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD'S ASSETS, RMB*/
		AVG(INDIASSET_TOTAL) AS AVGINDIASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL INDIVIDUAL'S ASSETS, RMB*/
		AVG(INDIASSET_CASHSAVE) AS AVGINDIASSET_CASHSAVE,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CASH & SAVINGS*/
		AVG(INDIASSET_CAPITAL) AS AVGINDIASSET_CAPITAL,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CAPITALS*/
		AVG(INDIASSET_GOVBOND) AS AVGINDIASSET_GOVBOND,  /*AVERAGE AMOUNT OF INDIVIDUAL'S GOVERNMENT BONDS*/
		AVG(INDIASSET_OTHER) AS AVGINDIASSET_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL'S OTHER KINDS OF ASSETS*/
		/**** income ****/
		AVG(HOUSINCOME_TOTAL) AS AVGHOUSINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD INCOMES*/
		AVG(INDIINCOME_TOTAL) AS AVGINDIINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL PERSONAL INCOMES*/
		AVG(HOUSINCOME_CAPT) AS AVGHOUSINCOME_CAPITAL,  /*AVERAGE AMOUNT OF THE HOUSEHOLD CAPITAL INCOMES*/
		AVG(INCOME_EARN) AS AVGINDIINCOME_EARN,  /*AVERAGE AMOUNT OF INDIVIDUAL EARNED INCOMES SUCH AS SALARIES*/
		AVG(INCOME_SELF) AS AVGINDIINCOME_SELF,  /*AVERAGE AMOUNT OF INDIVIDUAL SELF-EMPLOYED INCOMES*/
		AVG(INCOME_PENSION) AS AVGINDIINCOME_PENSION,  /*AVERAGE AMOUNT OF INDIVIDUAL PENSION INCOMES*/
		AVG(INCOME_TRANS) AS AVGINDIINCOME_TRANS,  /*AVERAGE AMOUNT OF INDIVIDUAL TRANSFER-PAYMENT INCOMES*/
		AVG(INCOME_OTHER) AS AVGINDIINCOME_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL OTHER KINDS OF INCOMES*/
		AVG(INCOME_FAMILY) AS AVGINDIINCOME_FAMILY,  /*AVERAGE AMOUNT OF INDIVIDUAL INCOMES FROM OTHER FAMILY MEMBERS*/
		/**** consumption ****/
		AVG(EXP1W_FOOD) AS AVGEXP1W_FOOD,  /*AVERAGE 1-WEEK CONSUMPTIONS ON FOOD*/
		AVG(EXP1Y_TOTAL) AS AVGEXP1Y_TOTAL,  /*AVERAGE 1-YEAR TOTAL CONSUMPTIONS*/
		/**** family care & daily activities ****/
		AVG(CHILDCARE_FLAG) AS CHILDCARE_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ONE CHILD LIVING TOGETHER OR NEARBY*/
		AVG(CORESD_FLAG) AS CHILDCORESD_RATIO,  /*RATIO OF THE POPULATION LIVING TOGETHER WITH AT LEAST ONE CHILD*/
		AVG(LVNEAR_FLAG) AS CHILDLVNEAR_RATIO,  /*RATIO OF THE POPULATION WHOSE AT LEAST ONE CHILD LIVING NEARBY*/
		AVG(SOCWK_FLAG) AS SOCWK_RATIO,  /*RATIO OF THE POPULATION PARTICIPATING IN SOCIAL ACTIVITIES NOW*/
		AVG(TRANSCHILD_FLAG) AS TRANSCHILD_RATIO,  /*RATIO OF THE POPULATION RECEIVING ANY TRANSFER PAYMENT FROM CHILDREN*/
		AVG(TRANSCHILD_AMT) AS AVGTRANSCHILD_AMT,  /*AVERAGE AMOUNT OF THE MONEY RECEIVED FROM CHILDREN IN THE PAST YEAR*/
		AVG(TRANSPAY_TOTAL) AS AVGTRANSPAY_TOTAL,  /*AVERAGE AMOUNT OF TOTAL TRANSFER PAYMENT RECEIVED IN THE PAST YEAR*/
		/**** retirement ****/
		AVG(WORK_FLAG) AS WORK_RATIO,  /*RATIO OF THE POPULATION STILL WORKING*/
		AVG(RETIRE_FLAG) AS RETIRE_RATIO,  /*RATIO OF THE POPULATION HAVING RETIRED*/
		AVG(RETIRE_FORMAL_FLAG) AS RETIREFORMAL_RATIO, /*RATIO OF THE POPULATION HAVING FORMALLY RETIRED OR REGISTERED AS RETIRED*/
		AVG(JOBSTATUS = 1) AS JOBSTATUS_AGRI_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS AGRICULTURE*/
		AVG(JOBSTATUS = 2) AS JOBSTATUS_NAGE_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL EMPLOYED*/
		AVG(JOBSTATUS = 3) AS JOBSTATUS_NAGS_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL SELF-EMPLOYED*/
		AVG(JOBSTATUS = 4) AS JOBSTATUS_NAGF_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL FAMILY BUSINESS*/
		AVG(JOBSTATUS = 5) AS JOBSTATUS_UNEM_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS UN-EMPLOYED*/
		AVG(JOBSTATUS = 6) AS JOBSTATUS_RETI_RATIO,  /*RATIO OF THE POUPLATION WHOSE JOB STATUS MARKED AS RETIRED*/
		AVG(JOBSTATUS = 7) AS JOBSTATUS_NEWK_RATIO  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NEVER WORKED*/
		/******************************/
		FROM WORK.CHARLS_W124
		GROUP BY YEAR, COMMUNITYID;  /*primarily group by year, then aggregate within individual groups*/


 


/**********************************************************************/
/*9. AGGREGATION: CITY LEVEL */
/*note:
	in this section, we aggregate the dataset CHARLS_w124 at city level, output to CHARLS_CITY (still a panel);
	note: all missing values are not counted, e.g. avg([1,miss,2]) = 1.5 rather than 1
*/
PROC SQL;
	CREATE TABLE WORK.CHARLS_CITY AS SELECT DISTINCT
		YEAR, CITY, PROVINCE,  /*coding information & background information*/
		/***** demography ******/
		AVG(AGE) AS AVGAGE,  /*AVERAGE AGE*/
		AVG(GENDER) AS MALE_RATIO,  /*RATIO OF MALE POPULATION*/
		AVG(EDU) AS AVGEDU,  /*AVERAGE EDU LEVEL*/
		AVG(MARITAL) AS MARITAL_RATIO,  /*RATIO OF MARRIED OR PARTERNERED POPULATION*/
		AVG(LEN_MARITAL) AS MARITAL_AVELEN,  /*AVERAGE YEARS OF CURRENT OR LATEST MARRIAGE*/
		AVG(HUKOU) AS NONAGRIHUKOU_RATIO,  /*RATIO OF THE POPULATION WITH NON-AGRICULTURAL HUKOU (LEGAL RESIDENCE STATUS)*/
		AVG(URBAN) AS URBAN_RATIO,  /*RATIO OF THE POPULATION LIVING IN AN URBAN AREA/COMMUNITY*/
		/***** health ********/
		AVG(BMIIDX) AS AVGBMI,  /*AVERAGE LEVEL OF THE RESPONDENTS' BMI INDEX (BODY MASS INDEX)*/
		AVG(DRINK1Y_FLAG) AS DRINK1Y_RATIO,  /*RATIO OF THE POPULATION WHO EVER DROKE AT LEAST ONE TIME IN THE PAST YEAR*/
		AVG(SMOKE_EVER) AS SMOKEEVER_RATIO,  /*RATIO FO THE POPULATION WHO EVER SMOKED*/
		AVG(SMOKE_NOW) AS SMOKENOW_RATIO, /*RATIO OF THE POPULATIOIN WHO SMOKE NOW*/
		AVG(SMOKE_NUM) AS AVGSMOKENUM,  /*AVERAGE NUMBER OF CIGARETTES SMOKED PER DAY*/
		/**** chronic diseases ******/
		AVG(CHRONIC_FLAG) AS CHRONIC_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ON OF THE 13 KINDS OF CHRONIC DISEASES*/
		AVG(CHRONIC_NUM) AS AVGCHRONIC_NUM,  /*AVERAGE TYPES OF CHRONIC DISEASES SUFFERED BY RESPONDENTS, PER CAPITA*/
		/**** medical cost *****/
		AVG(HOSP1Y_FLAG) AS HOSP1Y_RATIO, /*RATIO OF THE POPULATION WHO EVER RECEIVED IN-HOSPITAL TREATMENT IN THE PAST 1 YEAR*/
		AVG(HOSP1Y_NUM) AS AVGHOSP1Y_TIMES, /*AVERAGE TIMES OF RECEIVING IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(HOSPNOX_LAST) AS AVGHOSPNOX_LAST,  /*AVERAGE OF NIGHTS IN HOSPITAL WHEN RECEIVING THE MOST RECENT IN-HOSPITAL TREATMENT*/
		AVG(OUTP1M_FLAG) AS OUTP1M_RATIO, /*RATIO OF THE POPULATION WHO EVER VISITED DOCTORS OR RECEIVED OUTPATIENT TREATMENTS IN THE LAST 1 MONTH*/
		AVG(OUTP1M_NUM) AS AVGOUTP1M_TIMES, /*AVERAGE TIMES OF RECEIVING OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		AVG(HOSP1Y_REALEXP) AS AVGHOSP1Y_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(OUTP1M_REALEXP) AS AVGOUTP1M_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		/**** health insurance plans ****/
		AVG(INSURANCE_FLAG) AS INSURANCE_RATIO,  /*RATIO OF THE POPULATION COVERED BY ANY KIND OF HEALTH INSURANCE PLANS*/
		AVG(INSGOV_FLAG) AS INSGOV_RATIO,  /*RATIO OF THE POPULATION COVERED BY GOVERNEMNT OR PUBLIC HEALTH INSURANCE PLANS*/
		AVG(INSPRI_FLAG) AS INSPRI_RATIO,  /*RATIO OF THE POPULATION COVERED BY PRIVATE HEALTH INSURANCE PLANS*/
		AVG(INSOTH_FLAG) AS INSOTH_RATIO,  /*RATIO OF THE POPULATION COVERED BY OTHER KINDS OF HEALTH INSURANCE PLANS*/
		/**** asset ****/
		AVG(HOUSASSET_TOTAL) AS AVGHOUSASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD'S ASSETS, RMB*/
		AVG(INDIASSET_TOTAL) AS AVGINDIASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL INDIVIDUAL'S ASSETS, RMB*/
		AVG(INDIASSET_CASHSAVE) AS AVGINDIASSET_CASHSAVE,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CASH & SAVINGS*/
		AVG(INDIASSET_CAPITAL) AS AVGINDIASSET_CAPITAL,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CAPITALS*/
		AVG(INDIASSET_GOVBOND) AS AVGINDIASSET_GOVBOND,  /*AVERAGE AMOUNT OF INDIVIDUAL'S GOVERNMENT BONDS*/
		AVG(INDIASSET_OTHER) AS AVGINDIASSET_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL'S OTHER KINDS OF ASSETS*/
		/**** income ****/
		AVG(HOUSINCOME_TOTAL) AS AVGHOUSINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD INCOMES*/
		AVG(INDIINCOME_TOTAL) AS AVGINDIINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL PERSONAL INCOMES*/
		AVG(HOUSINCOME_CAPT) AS AVGHOUSINCOME_CAPITAL,  /*AVERAGE AMOUNT OF THE HOUSEHOLD CAPITAL INCOMES*/
		AVG(INCOME_EARN) AS AVGINDIINCOME_EARN,  /*AVERAGE AMOUNT OF INDIVIDUAL EARNED INCOMES SUCH AS SALARIES*/
		AVG(INCOME_SELF) AS AVGINDIINCOME_SELF,  /*AVERAGE AMOUNT OF INDIVIDUAL SELF-EMPLOYED INCOMES*/
		AVG(INCOME_PENSION) AS AVGINDIINCOME_PENSION,  /*AVERAGE AMOUNT OF INDIVIDUAL PENSION INCOMES*/
		AVG(INCOME_TRANS) AS AVGINDIINCOME_TRANS,  /*AVERAGE AMOUNT OF INDIVIDUAL TRANSFER-PAYMENT INCOMES*/
		AVG(INCOME_OTHER) AS AVGINDIINCOME_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL OTHER KINDS OF INCOMES*/
		AVG(INCOME_FAMILY) AS AVGINDIINCOME_FAMILY,  /*AVERAGE AMOUNT OF INDIVIDUAL INCOMES FROM OTHER FAMILY MEMBERS*/
		/**** consumption ****/
		AVG(EXP1W_FOOD) AS AVGEXP1W_FOOD,  /*AVERAGE 1-WEEK CONSUMPTIONS ON FOOD*/
		AVG(EXP1Y_TOTAL) AS AVGEXP1Y_TOTAL,  /*AVERAGE 1-YEAR TOTAL CONSUMPTIONS*/
		/**** family care & daily activities ****/
		AVG(CHILDCARE_FLAG) AS CHILDCARE_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ONE CHILD LIVING TOGETHER OR NEARBY*/
		AVG(CORESD_FLAG) AS CHILDCORESD_RATIO,  /*RATIO OF THE POPULATION LIVING TOGETHER WITH AT LEAST ONE CHILD*/
		AVG(LVNEAR_FLAG) AS CHILDLVNEAR_RATIO,  /*RATIO OF THE POPULATION WHOSE AT LEAST ONE CHILD LIVING NEARBY*/
		AVG(SOCWK_FLAG) AS SOCWK_RATIO,  /*RATIO OF THE POPULATION PARTICIPATING IN SOCIAL ACTIVITIES NOW*/
		AVG(TRANSCHILD_FLAG) AS TRANSCHILD_RATIO,  /*RATIO OF THE POPULATION RECEIVING ANY TRANSFER PAYMENT FROM CHILDREN*/
		AVG(TRANSCHILD_AMT) AS AVGTRANSCHILD_AMT,  /*AVERAGE AMOUNT OF THE MONEY RECEIVED FROM CHILDREN IN THE PAST YEAR*/
		AVG(TRANSPAY_TOTAL) AS AVGTRANSPAY_TOTAL,  /*AVERAGE AMOUNT OF TOTAL TRANSFER PAYMENT RECEIVED IN THE PAST YEAR*/
		/**** retirement ****/
		AVG(WORK_FLAG) AS WORK_RATIO,  /*RATIO OF THE POPULATION STILL WORKING*/
		AVG(RETIRE_FLAG) AS RETIRE_RATIO,  /*RATIO OF THE POPULATION HAVING RETIRED*/
		AVG(RETIRE_FORMAL_FLAG) AS RETIREFORMAL_RATIO, /*RATIO OF THE POPULATION HAVING FORMALLY RETIRED OR REGISTERED AS RETIRED*/
		AVG(JOBSTATUS = 1) AS JOBSTATUS_AGRI_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS AGRICULTURE*/
		AVG(JOBSTATUS = 2) AS JOBSTATUS_NAGE_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL EMPLOYED*/
		AVG(JOBSTATUS = 3) AS JOBSTATUS_NAGS_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL SELF-EMPLOYED*/
		AVG(JOBSTATUS = 4) AS JOBSTATUS_NAGF_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL FAMILY BUSINESS*/
		AVG(JOBSTATUS = 5) AS JOBSTATUS_UNEM_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS UN-EMPLOYED*/
		AVG(JOBSTATUS = 6) AS JOBSTATUS_RETI_RATIO,  /*RATIO OF THE POUPLATION WHOSE JOB STATUS MARKED AS RETIRED*/
		AVG(JOBSTATUS = 7) AS JOBSTATUS_NEWK_RATIO  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NEVER WORKED*/
		/******************************/
		FROM WORK.CHARLS_W124
		GROUP BY YEAR, CITY;






/**********************************************************************/
/*10. AGGREGATION: PROVINCE LEVEL */
/*note:
	in this section, we aggregate the dataset CHARLS_w124 at city level, output to CHARLS_PROV (still a panel);
	note: all missing values are not counted, e.g. avg([1,miss,2]) = 1.5 rather than 1
*/
PROC SQL;
	CREATE TABLE WORK.CHARLS_PROV AS SELECT DISTINCT
		YEAR, PROVINCE,  /*coding information & background information*/
		/***** demography ******/
		AVG(AGE) AS AVGAGE,  /*AVERAGE AGE*/
		AVG(GENDER) AS MALE_RATIO,  /*RATIO OF MALE POPULATION*/
		AVG(EDU) AS AVGEDU,  /*AVERAGE EDU LEVEL*/
		AVG(MARITAL) AS MARITAL_RATIO,  /*RATIO OF MARRIED OR PARTERNERED POPULATION*/
		AVG(LEN_MARITAL) AS MARITAL_AVELEN,  /*AVERAGE YEARS OF CURRENT OR LATEST MARRIAGE*/
		AVG(HUKOU) AS NONAGRIHUKOU_RATIO,  /*RATIO OF THE POPULATION WITH NON-AGRICULTURAL HUKOU (LEGAL RESIDENCE STATUS)*/
		AVG(URBAN) AS URBAN_RATIO,  /*RATIO OF THE POPULATION LIVING IN AN URBAN AREA/COMMUNITY*/
		/***** health ********/
		AVG(BMIIDX) AS AVGBMI,  /*AVERAGE LEVEL OF THE RESPONDENTS' BMI INDEX (BODY MASS INDEX)*/
		AVG(DRINK1Y_FLAG) AS DRINK1Y_RATIO,  /*RATIO OF THE POPULATION WHO EVER DROKE AT LEAST ONE TIME IN THE PAST YEAR*/
		AVG(SMOKE_EVER) AS SMOKEEVER_RATIO,  /*RATIO FO THE POPULATION WHO EVER SMOKED*/
		AVG(SMOKE_NOW) AS SMOKENOW_RATIO, /*RATIO OF THE POPULATIOIN WHO SMOKE NOW*/
		AVG(SMOKE_NUM) AS AVGSMOKENUM,  /*AVERAGE NUMBER OF CIGARETTES SMOKED PER DAY*/
		/**** chronic diseases ******/
		AVG(CHRONIC_FLAG) AS CHRONIC_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ON OF THE 13 KINDS OF CHRONIC DISEASES*/
		AVG(CHRONIC_NUM) AS AVGCHRONIC_NUM,  /*AVERAGE TYPES OF CHRONIC DISEASES SUFFERED BY RESPONDENTS, PER CAPITA*/
		/**** medical cost *****/
		AVG(HOSP1Y_FLAG) AS HOSP1Y_RATIO, /*RATIO OF THE POPULATION WHO EVER RECEIVED IN-HOSPITAL TREATMENT IN THE PAST 1 YEAR*/
		AVG(HOSP1Y_NUM) AS AVGHOSP1Y_TIMES, /*AVERAGE TIMES OF RECEIVING IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(HOSPNOX_LAST) AS AVGHOSPNOX_LAST,  /*AVERAGE OF NIGHTS IN HOSPITAL WHEN RECEIVING THE MOST RECENT IN-HOSPITAL TREATMENT*/
		AVG(OUTP1M_FLAG) AS OUTP1M_RATIO, /*RATIO OF THE POPULATION WHO EVER VISITED DOCTORS OR RECEIVED OUTPATIENT TREATMENTS IN THE LAST 1 MONTH*/
		AVG(OUTP1M_NUM) AS AVGOUTP1M_TIMES, /*AVERAGE TIMES OF RECEIVING OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		AVG(HOSP1Y_REALEXP) AS AVGHOSP1Y_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF IN-HOSPITAL TREATMENTS IN THE PAST 1 YEAR*/
		AVG(OUTP1M_REALEXP) AS AVGOUTP1M_REALEXP,  /*AVERAGE OUT-OF-POCKET EXPENDITURE OF OUT-PATIENT TREATMENTS IN THE PAST 1 MONTH*/
		/**** health insurance plans ****/
		AVG(INSURANCE_FLAG) AS INSURANCE_RATIO,  /*RATIO OF THE POPULATION COVERED BY ANY KIND OF HEALTH INSURANCE PLANS*/
		AVG(INSGOV_FLAG) AS INSGOV_RATIO,  /*RATIO OF THE POPULATION COVERED BY GOVERNEMNT OR PUBLIC HEALTH INSURANCE PLANS*/
		AVG(INSPRI_FLAG) AS INSPRI_RATIO,  /*RATIO OF THE POPULATION COVERED BY PRIVATE HEALTH INSURANCE PLANS*/
		AVG(INSOTH_FLAG) AS INSOTH_RATIO,  /*RATIO OF THE POPULATION COVERED BY OTHER KINDS OF HEALTH INSURANCE PLANS*/
		/**** asset ****/
		AVG(HOUSASSET_TOTAL) AS AVGHOUSASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD'S ASSETS, RMB*/
		AVG(INDIASSET_TOTAL) AS AVGINDIASSET_TOTAL,  /*AVERAGE AMOUNT OF TOTAL INDIVIDUAL'S ASSETS, RMB*/
		AVG(INDIASSET_CASHSAVE) AS AVGINDIASSET_CASHSAVE,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CASH & SAVINGS*/
		AVG(INDIASSET_CAPITAL) AS AVGINDIASSET_CAPITAL,  /*AVERAGE AMOUNT OF INDIVIDUAL'S CAPITALS*/
		AVG(INDIASSET_GOVBOND) AS AVGINDIASSET_GOVBOND,  /*AVERAGE AMOUNT OF INDIVIDUAL'S GOVERNMENT BONDS*/
		AVG(INDIASSET_OTHER) AS AVGINDIASSET_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL'S OTHER KINDS OF ASSETS*/
		/**** income ****/
		AVG(HOUSINCOME_TOTAL) AS AVGHOUSINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL HOUSEHOLD INCOMES*/
		AVG(INDIINCOME_TOTAL) AS AVGINDIINCOME_TOTAL,  /*AVERAGE AMOUNT OF TOTAL PERSONAL INCOMES*/
		AVG(HOUSINCOME_CAPT) AS AVGHOUSINCOME_CAPITAL,  /*AVERAGE AMOUNT OF THE HOUSEHOLD CAPITAL INCOMES*/
		AVG(INCOME_EARN) AS AVGINDIINCOME_EARN,  /*AVERAGE AMOUNT OF INDIVIDUAL EARNED INCOMES SUCH AS SALARIES*/
		AVG(INCOME_SELF) AS AVGINDIINCOME_SELF,  /*AVERAGE AMOUNT OF INDIVIDUAL SELF-EMPLOYED INCOMES*/
		AVG(INCOME_PENSION) AS AVGINDIINCOME_PENSION,  /*AVERAGE AMOUNT OF INDIVIDUAL PENSION INCOMES*/
		AVG(INCOME_TRANS) AS AVGINDIINCOME_TRANS,  /*AVERAGE AMOUNT OF INDIVIDUAL TRANSFER-PAYMENT INCOMES*/
		AVG(INCOME_OTHER) AS AVGINDIINCOME_OTHER,  /*AVERAGE AMOUNT OF INDIVIDUAL OTHER KINDS OF INCOMES*/
		AVG(INCOME_FAMILY) AS AVGINDIINCOME_FAMILY,  /*AVERAGE AMOUNT OF INDIVIDUAL INCOMES FROM OTHER FAMILY MEMBERS*/
		/**** consumption ****/
		AVG(EXP1W_FOOD) AS AVGEXP1W_FOOD,  /*AVERAGE 1-WEEK CONSUMPTIONS ON FOOD*/
		AVG(EXP1Y_TOTAL) AS AVGEXP1Y_TOTAL,  /*AVERAGE 1-YEAR TOTAL CONSUMPTIONS*/
		/**** family care & daily activities ****/
		AVG(CHILDCARE_FLAG) AS CHILDCARE_RATIO,  /*RATIO OF THE POPULATION WITH AT LEAST ONE CHILD LIVING TOGETHER OR NEARBY*/
		AVG(CORESD_FLAG) AS CHILDCORESD_RATIO,  /*RATIO OF THE POPULATION LIVING TOGETHER WITH AT LEAST ONE CHILD*/
		AVG(LVNEAR_FLAG) AS CHILDLVNEAR_RATIO,  /*RATIO OF THE POPULATION WHOSE AT LEAST ONE CHILD LIVING NEARBY*/
		AVG(SOCWK_FLAG) AS SOCWK_RATIO,  /*RATIO OF THE POPULATION PARTICIPATING IN SOCIAL ACTIVITIES NOW*/
		AVG(TRANSCHILD_FLAG) AS TRANSCHILD_RATIO,  /*RATIO OF THE POPULATION RECEIVING ANY TRANSFER PAYMENT FROM CHILDREN*/
		AVG(TRANSCHILD_AMT) AS AVGTRANSCHILD_AMT,  /*AVERAGE AMOUNT OF THE MONEY RECEIVED FROM CHILDREN IN THE PAST YEAR*/
		AVG(TRANSPAY_TOTAL) AS AVGTRANSPAY_TOTAL,  /*AVERAGE AMOUNT OF TOTAL TRANSFER PAYMENT RECEIVED IN THE PAST YEAR*/
		/**** retirement ****/
		AVG(WORK_FLAG) AS WORK_RATIO,  /*RATIO OF THE POPULATION STILL WORKING*/
		AVG(RETIRE_FLAG) AS RETIRE_RATIO,  /*RATIO OF THE POPULATION HAVING RETIRED*/
		AVG(RETIRE_FORMAL_FLAG) AS RETIREFORMAL_RATIO, /*RATIO OF THE POPULATION HAVING FORMALLY RETIRED OR REGISTERED AS RETIRED*/
		AVG(JOBSTATUS = 1) AS JOBSTATUS_AGRI_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS AGRICULTURE*/
		AVG(JOBSTATUS = 2) AS JOBSTATUS_NAGE_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL EMPLOYED*/
		AVG(JOBSTATUS = 3) AS JOBSTATUS_NAGS_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL SELF-EMPLOYED*/
		AVG(JOBSTATUS = 4) AS JOBSTATUS_NAGF_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NON-AGRICULTURAL FAMILY BUSINESS*/
		AVG(JOBSTATUS = 5) AS JOBSTATUS_UNEM_RATIO,  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS UN-EMPLOYED*/
		AVG(JOBSTATUS = 6) AS JOBSTATUS_RETI_RATIO,  /*RATIO OF THE POUPLATION WHOSE JOB STATUS MARKED AS RETIRED*/
		AVG(JOBSTATUS = 7) AS JOBSTATUS_NEWK_RATIO  /*RATIO OF THE POPULATION WHOSE JOB STATUS MARKED AS NEVER WORKED*/
		/******************************/
		FROM WORK.CHARLS_W124
		GROUP BY YEAR, PROVINCE;






/*10. output the city dataset ***/
PROC EXPORT DATA = WORK.CHARLS_CITY OUTFILE = "E:\CHARLS_DB\harmonized 2018\CHARLS_CITY.xlsx";RUN;
