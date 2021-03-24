
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


*Note ... only one data directory needed ... the rest can go into the folder name;
*Data directory - shivani; 
%let datadir = C:\Users\spate41\Box Sync\COVID19_data_shared;



*NOTE: PLEASE WRITE THE CODE TO READ IN EACH DATASET INTO THE WORK FOLDER IN ONE FILE;
*NOTE: COUNTY IS A CODE, STATE IS A CODE, AND FIPS IS A SEPARATE CODE. PLEASE WRITE THAT CODE;


*Urban rural codes;
      PROC IMPORT OUT= WORK.urbancodes                         
                        DATAFILE= "&datadir.\CDC_Urban_Rural\CDC_NCHS_2006UrbanizationCodes.xlsx" 
                        DBMS=EXCEL REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;

data urbancodes2;
set urbancodes;
	fips=put(county_code,z5.);
	countyx=(substr(fips,3,3))+0;
	statex=(substr(fips,1,2))+0;;
drop county fips;
run;

data urbancodes3;
set urbancodes2;
county=countyx;
state=statex;
drop statex countyx;
run;


*ACS data;
	Data acs2018_5yr_county; 
	set "&datadir.\ACS 2018 5yr\acs2018_5yr_county"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A10008_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	if B12001_004 ne 0 then college =     B12001_001/B12001_004 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	countyx=(substr(fips,3,3))+0;
	statex=(substr(fips,1,2))+0;;
	keep countyx statex popden age65over  male  female black hispanic minority groupquater college hhincome poverty;
	run;

	data acs;
	set acs2018_5yr_county;
	county=countyx;
	state=statex;
	run;


*SVI data;
	DATA svi2018_us_county;	
	SET "&datadir.\CDC SVI\svi2018_us_county";
	fipscode=put(FIPS,z5.);

	*countyname=county;
	*statename=state;
	countyx=(substr(fipscode,3,3))+0;
	statex=ST;
	drop county state countyfips;
	if st in (66 72 78) then delete;
	run;

	data SVI;
	set svi2018_us_county;
	county=countyx;
	state=statex;
	drop countyx statex;
	run;




*CDC diabetes data;
		DATA diabetescounty;	
		SET "&datadir.\CDC Diabetes Surveillance\county";
		run;

		DATA diabetesstate;	
		SET "&datadir.\CDC Diabetes Surveillance\state"; *Need to remove the territories;
		run;

		DATA diabetesnation;	
		SET "&datadir.\CDC Diabetes Surveillance\nation";
		run;
	
		data diabetesall;
		set diabetescounty  diabetesstate diabetesnation;

		fipscode=put(CountyFIPS,z5.);

		*countyname=county;
		*statename=state;
		countyx=(substr(fipscode,3,3))+0;
		statex=fips;
		drop county state countyfips;
		if fips in (66 72 78) then delete;
		run;


		data diabetesall2;
		set diabetesall;
		county=countyx;
		state=statex;
		drop countyx statex;
		run;							


*Health insurance files;
		data sahie_2018_new;
		set "&datadir.\SAHIE\sahie_2018_new";
		state=statefips;
		county=countyfips;
		run;


*Merging the data sources;
		proc sort data=urbancodes3; by state county; run;
		proc sort data=acs; by state county; run;
		proc sort data=svi; by state county; run;
		proc sort data=diabetesall2; by state county; run;
		proc sort data=sahie_2018_new; by state county; run; 

		data mergedsocial;
		merge urbancodes3 acs svi diabetesall2 sahie_2018_new;
		by state county;
		run;

data "&datadir.\mergedsocial";
set mergedsocial;
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
