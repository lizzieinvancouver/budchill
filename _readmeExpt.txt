# budchill
Chilling Experiment 2015/2016

README started 14 December 2017
By Lizzie

General: The bud chill experiment was done in winter 2015-2016 (meaning December 2015 to whatever month in 2016) and led by Dan Flynn with help from Tim Savas. 

Based on my best available knowledge:
- Field collection happened at Harvard Forest on 18 December 2015 (see Budchill experiment 2016 log.pdf)
- Forcing conditions were the same for everything coming out of chilling: So forcing day/night was 20/10 deg C, with 12 h photoperiod. 
- Chilling photoperiod: from Dan Flynn on 3 Nov 2017: I believe we kept them dark the whole time, as in a refrigerator study. The walk-cold room (in the E lab) just has fluorescent lights and no timer on the lights, so it would have not been possible to have the same light conditions across the chilling temperatures. Temps were the same day/night.
- We then had:
	- WLO (warm-long day-zero additional chilling)
	- chill 1 (1 degree day and night)
	- chill 2 (2 degrees day and night)
	- chill 4 (4 degrees day and night)
	- chill 8 (8 degrees day and night)	
	- We varied how long chilling lasted (3 different time points)… 
		- Based on Budchill prep.R and Budchill experiment 2016 log.pdf we think we had:
		- 1 January (no chill)
		- 16 January (16 days)
		- 2 February (32 days)
- Humidity was set at 85% and C02 is noted as 400 ppm, but I am not sure if we had it on.
- We worked on 7 species (I believe):
	(1) ACESAC
	(2) BETALL
	(3) HAMVIR
	(4) ILEMUC
	(5) QUERUB
	(6) VIBCAS
	(7) FAGGRA


Also, a note on the BBCH scale we used: We used a numeric form that is converted in this file: Budburst Datasheet 2015-05-15.xlsx/BBCHscale tab. You call also find it in Budburst Chill Datasheet 2016-01-04.xlsx or most other of the datasheets here. 

<><><><><><><><><><><><><><><><><><><><>
Chill portions & the experimental design
<><><><><><><><><><><><><><><><><><><><>
Dan designed the experiment so that we obtained the same chill portions under different temperatures. Based on budchill_prep.R I think it goes like this:

time 1 has no chilling, so all treatments are the same
time 2: 
	1 C: 45.5 cp (chill portions)
	2 C: 49.8 cp
	4 C: 51.4 cp
	8 C: 53.5 cp
time 3: 
	1 C: 51.8 cp 
	2 C: 60.3 cp
	4 C: 63.0 cp
	8 C: 66.7 cp

In the code we save chill portions from the Dynamic model (note I did find he was originally pulling chilling hours, but I think this was a mistake as his natural and storage chilling to me look to be in chill portions). Try ?chilling for more beyond this:

Chill Portions are calculated according to Fishman et al. (1987a,b). More honestly, they are calculated according to an Excel sheet produced by Amnon Erez and colleagues, which converts the complex equations in the Fishman papers into relatively simple Excel functions. These were translated into R. References to papers that include the full functions are given below. Growing Degree Hours are calculated according to Anderson et al. (1986), using the default values they suggest.

Here's some examples (using Dan's bud chill prep code) by Lizzie:

16 d at 2 C: 9.6 chill portions (cp); 384 chill hours (ch)
16 d at 4 C: 11.1 cp; 384 ch
16 d at 8 C: 13.2 cp; 384 ch
