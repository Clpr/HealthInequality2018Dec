<!-- encoding UTF8 -->
# HealthInequality2018Dec
<!-- author: Tianhao Zhao -->
<!-- data: Dec 2018 -->
----------------------------------

Programs for a paper authored by Jiang, Zheng & Zhao in Dec 2018.
The paper discusses the correslations between the inequality of health outcomes & socioeconomic status.
It uses data from NHSS (1998, 2003, 2008) & CHARLS (2011, 2013, 2015) to do empirical studies.
I published the R language programs to my website.
Readers may download it and run it to repeat our work.
Documentations & comments are well-organized.
If you have any concern or require more information, please let me know.

This paper has been published whose citation is:
> Jiang, Y., Zheng, H., & Zhao, T. (2019). Socioeconomic status and morbidity rate inequality in China: based on NHSS and CHARLS data. International journal of environmental research and public health, 16(2), 215.

Because I expect there is no more updating in the future, I archived this project.
(But still open to discussion, please let me know through email)

Tianhao Zhao (Clpr)

Apr 11 2019

--------------------------------------

## Directory structure
1. Root
   1. **main.r**: a portal file, you'll find complete information about dependency, encoding, source files etc.
   2. **/data**: data files
      1. **/CHARLSdat**: documentations, index files & the SAS code to clean harmonized CHARLS dataset. Explain later
      2. **/ChinaMapDat**: shape & GIS data of Chinese provinces, cities & counties
      3. **CHARLS_CITY.xlsx**: aggregated city-level CHARLS panel dataset; please use UTF-8 encoding; Chinese characters there.
      4. **NHSSdat.xlsx**: county-level NHSS panel dataset; please use UTF-8 encoding; Chinese characters there.
   3. **/docs**: external documentaion of empirical studies
   4. **/output**: please go there to find figures, tables etc.
   5. **/scripts**: analysis programs, important
   6. **/src**: source files of functions
      1. **mapplots.r**: tools to plot colored maps, based on ggplot2
      2. **mathtools.r**: general functions to do general jobs
      3. **paneltools.r**: functions for panel data models; based on *plm* package
      4. **plottools.r**: functions of user-friendly plotting


## CHARLS dataset

In April 2018, CHARLS project published a harmonized dataset ([hyperlink](http://charls.pku.edu.cn/en/page/data/harmonized_charls)).
It solves some ... uhnnnnn .... *tricky* problems in data aggregation process.

Users may use a provided SAS program (./data/CHARLSdat/H_CHARLS_Clean.sas) to easily reproduce our city-level dataset.
The program is fully documentationed & commented in English.
It was written in SAS 9.4 & used SAS/SQL module (I know some SAS releases do not provide this module, so, I specially figure out it here).

Meanwhile, one thing important:
because CHARLS always publish their datasets ONLY in Stata13 format, and SAS 9.4 cannot directly read it (it supports Stata12 format, though),
I firstly converted the original .dta file to a SPSS file (.sav) then read it into SAS 9.4. SPSS format can keep all tags, formats and labels of the original data file; and if you try to directly convert .dta -> .sas7bdat, there are usually some problems of variable names & labels (unpredictable sometimes......).
I did this job with the R package *haven*.

And, I will not attach the original CHARLS dataset in this project.
One, because of the copyrights (you may freely register an account on the CHALRS website then apply for the dataset, quick and safe!);
Another one, it is ... too big and useless for a github project (.dta about 70MB, .sav about 300MB, but the original .zip downloaded is only 12MB! magic ...)









