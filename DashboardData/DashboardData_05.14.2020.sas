
*================= CODE TO IMPORTING Datasets FOR COUNTY DASHBOARD ==================;

* Data set list
1. ACS 2018 5yr
2. ACS 2018 1yr
3. CDC Diabetes Surveillance
4. CDC SVI
5. SAHIE
;

*Daesung's Emory Virtual desktop;
%let datadir = H:\COVID-Dashboard\Data;

*Data directory - shivani; 
*%let datadir = C:\Users\spate41\Box Sync\COVID19_data_shared;

*NOTE: PLEASE WRITE THE CODE TO READ IN EACH DATASET INTO THE WORK FOLDER IN ONE FILE;
*NOTE: COUNTY IS A CODE, STATE IS A CODE, AND FIPS IS A SEPARATE CODE. PLEASE WRITE THAT CODE;
*NOTE: COUNTY IS CODED AS 0 FOR STATE LEVEL OBSERVATIONS
*NOTE: STATE IS CODED AS 0 FOR NATIONAL LEVEL OBERVATIONS


*ACS 2018 5 year estimates data;
	Data acs2018_5yr_county; 
	set "&datadir.\ACS 2018 5yr\acs2018_5yr_county"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county=(substr(fips,3,3))+0;
	state=(substr(fips,1,2))+0;
	keep county state popden age65over  male  female black hispanic minority groupquater college hhincome poverty;
	run;

	Data acs2018_5yr_state; 
	set "&datadir.\ACS 2018 5yr\acs2018_5yr_state"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 
	
	county = .;
	state = (substr(fips,1,2))+0;
	keep state county popden age65over  male  female black hispanic minority groupquater college hhincome poverty;
	run;

	Data acs2018_5yr_nation; 
	set "&datadir.\ACS 2018 5yr\acs2018_5yr_nation"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county = .;
	state = .;
	keep state county popden age65over  male  female black hispanic minority groupquater college hhincome poverty;
	run;

	data acs2018_5yr_all1;
	set acs2018_5yr_county  acs2018_5yr_state acs2018_5yr_nation;
	statex=input(state,5.0);
	countyx=input(county,5.0);
	drop state county;
	run;

	data acs2018_5yr_all;
	set acs2018_5yr_all1;
	state=statex;
	county=countyx;
	drop statex countyx;
	run;



*CDC's Social Vulnerability Index 2018 data;
	DATA svi2018_us_county1;	
	SET "&datadir.\CDC SVI\svi2018_us_county";
	fipscode=put(FIPS,z5.);

	countyx=(substr(fipscode,3,3))+0;
	statex=(substr(fipscode,1,2))+0;
	*countyname=county;
	*statename=state;
	*county=(substr(fipscode,3,3))+0;
	*state=ST;
	*if state in (66 72 78) then delete;

	*fips = fipscode;
	*statex=input(state,5.0);
	*countyx=input(county,5.0);

	keep RPL_THEME1 RPL_THEME2 RPL_THEME3 RPL_THEME4 countyx statex ;

	run;

	DATA svi2018_us_county;	
	SET svi2018_us_county1;
	county = countyx;
	state = statex;
	drop countyx statex;
	run;


*CDC diabetes Surveilance data;
	DATA diabetescounty1;	
	SET "&datadir.\CDC Diabetes Surveillance\county";
	fipscode=put(CountyFIPS,z5.);
	countyx=(substr(fipscode,3,3))+0;
	statex=fips;
	drop county state;
	run;
	DATA diabetescounty;	
	SET diabetescounty1;
	state=statex ;
	county=countyx;
	drop statex countyx CountyFIPS fips fipscode;
	run;


	DATA diabetesstate1;	
	SET "&datadir.\CDC Diabetes Surveillance\state"; *Need to remove the territories;
	county = 0;
	statex=put(fips, 2.0);
	drop state;
	run;

	DATA diabetesstate;	
	SET diabetesstate1; 
	state=input(statex, 5.0);
	drop statex;
	run;


	DATA diabetesnation;	
	SET "&datadir.\CDC Diabetes Surveillance\nation";
	county =0;
	state = 0;
	run;
	
	data CDCdiabetes;
	set diabetescounty  diabetesstate diabetesnation;
	if fips in (66 72 78) then delete;
	drop fips;
	run;


* Small Area Health Insurance Estimates_2018;
	data sahie_20181;
	set "&datadir.\SAHIE\sahie_2018_new";
	statex=put(statefips,8.);
	countyx=put(countyfips,8.);
	keep statex countyx PCTUI PCTIC;
	run;

	data sahie_2018;
	set sahie_20181;
	state=input(statex,8.0);
	county=input(countyx,8.0);
	drop statex countyx;
	run;



*Merging the data sources;
		proc sort data=acs2018_5yr_all; by state county; run;
		proc sort data=svi2018_us_county; by state county; run;
		proc sort data=CDCdiabetes; by state county; run;
		proc sort data=sahie_2018; by state county; run; 

		data merged;
		merge acs2018_5yr_all svi2018_us_county CDCdiabetes sahie_2018;
		by state county;
		if state in (66 72 78) then delete;
		run;

proc contents data=merged;
run;











*NOTE: PLEASE PUT ALL VARIABLE LABELS IN ONE FORMAT FILE;

proc format;
value $ varlabels
    
"popden" = "Population Density (Per Sq. Mile)"
"age65over"  = "% Age 65 and over"
"male"  = "% Male population"     
"female"  =  "% Female population"       
"black"  =  "% Not Hispanic or Latino: Black or African American Alone"  
"hispanic" = "% Hispanic or Latino"
"minority"  = "% Other than not Hispanic or Latino, White Alone"
"groupquater"  = "% Population in group quarter" 
"college"  = "% Bachelor's Degree or Better, age 25 and over"
"hhincome"  = "Average Household Income (In 2018 Inflation Adjusted Dollars)"
"poverty"  = "% Population under 1.00 poverty income ratio"

"RPL_THEME1" = "Percentile ranking for Socioeconomic theme summary"
"RPL_THEME2" = "Percentile ranking for Household Composition theme summary"
"RPL_THEME3" = "Percentile ranking for Minority Status/Language theme"
"RPL_THEME4" = "Percentile ranking for Housing Type/ Transportation theme"
;
