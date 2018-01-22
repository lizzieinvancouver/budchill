Understanding the bud chill repo
Started 11 Jan 2018

TO DO:

budchill_prep.R defines bb as stage 3 or greater (line 113) and leafout as stage 6 or greater (line 116). What version of BBCH does this agree with?! Note that this is the same as in Pheno budburst prep.R (and in the manuscript we say we used code 7 and code 11 in Finn et al. 2007). 

+++++++++++++++++++++++++++++++++

+++++++++++++++++++++++++++++++++

- Remember that Dan puts files created from R scripts in the INPUT folder 


<><><><><><><><><><><><><>
Overview of files in analyses/
<><><><><><><><><><><><><>
budchill_prep.R 
	- This file reads in the raw experimental data and formats it.
	- day1/day2/day3 are taking the Date column and setting it to days since start of forcing conditions, with day 1 being the no additional chill treatment, day 2 being the 1 set of additional chill and day 3 being the 2 set of additional chill
