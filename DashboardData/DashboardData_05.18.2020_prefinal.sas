
*Month and day of current downloand;
%let month=5;
%let day=28;

*================= CODE TO IMPORTING Datasets FOR COUNTY DASHBOARD ==================;

* Data set list
*NYtimes
*NCHS - CDC wonder
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



*================= COVID DEATHS =======================================;
*data inputs: NYTimes COVID deaths by states and county - as of May 2;
*data can be found at:
	*https://github.com/nytimes/covid-19-data;

		filename usny temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
		 method="GET"
		 out=usny;
		run;
		proc import
		  file=usny
		  out=work.USnytimes replace
		  dbms=csv;
		run;


		filename statesny temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
		 method="GET"
		 out=statesny;
		run;
		proc import
		  file=statesny
		  out=work.statesnytimes replace
		  dbms=csv;
		run;


		filename countyny temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
		 method="GET"
		 out=countyny;
		run;
		proc import
		  file=countyny
		  out=work.countiesnytimes replace
		  dbms=csv;
		run;

			data covid_all0;
			length state $35.;
			set Countiesnytimes statesnytimes USnytimes (in=n);
			where  month(date)=&month. and day(date)=&day.;
			fips_code=put(fips,z5.);
			countyx=(substr(fips_code,3,3))+0;
			statex=(substr(fips_code,1,2))+0;
			if n=1 then nation=1; *indicator for nation;
			if statex in (66 69 72 78) then delete;
			drop state county fips_code;
			run;

		*Cumulative deaths due to covid by county, state, and nation;
			data covid_all;
			set covid_all0;
			state=statex;
			county=countyx;
			drop statex countyx;
			run;

proc sort data=covid_all; by order; run;
PROC EXPORT DATA= WORK.covid_all 
            OUTFILE= "&datadir.\covid_all.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;





*=========================== daily cases data;
		proc sort data=Countiesnytimes; by state county date; run;
		data county_daily; 
		set Countiesnytimes;
		by state county;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if first.county then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;

		if first.county then mean7daydeaths=0;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));

		if first.county then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if first.county then mean7daycases=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

		zero=0;
		countycode =put(fips, z5.);
		countyx=(substr(countycode,3,3))+0;
		statex=(substr(countycode,1,2))+0;
		if countyx=. and statex=. then delete; 
		drop state county;
		run;

		proc sort data=statesnytimes; by state date; run;
		data state_daily; 
		set statesnytimes;
		by state;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if first.state then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;

		if first.state then mean7daydeaths=.;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));

		if first.state then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if first.state then mean7daycases=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

		zero=0;
		
		statex=fips;
		if statex in (66 69 72 78) then delete;
		drop state ;
		run;

	proc sort data=usnytimes; by  date; run;
		data us_daily; 
		set usnytimes;
		by date;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if _n_=1 then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;


		if _n_=1 then mean7daydeaths=0;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));


		if _n_=1 then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if _n_=1 then mean7day=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

		zero=0;
        nation=1;
		run;

		data mean7day_current;
		set state_daily county_daily us_daily;
		where  month(date)=&month. and day(date)=&day.;

  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		keep state county nation date dailydeaths mean7daydeaths dailycases mean7daycases zero;
		run;

		data covidtimeseries;
		set state_daily county_daily us_daily;
		where month(date)>3;
  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		*keep state county nation date dailydeaths mean7daydeaths dailycases mean7daycases zero;
		drop statex countyx;
		run;




*****************************************************************
*Population denominator and 2013 Urban rural codes - From CDC Wonder
*https://www.cdc.gov/nchs/data_access/urban_rural.htm#update;
*****************************************************************;
      PROC IMPORT OUT= WORK.urbancodes                         
                        DATAFILE= "&datadir.\CDC_Urban_Rural\Urban_Rural_2013_Classification.xlsx" 
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
				statex=(substr(fips,1,2))+0;
				countyname=county;
			drop county fips;
			run;

			data urbancodes3;
			set urbancodes2;
			county=countyx;
			state=statex;

			countynum=county_code+0;
			countycode=put(countynum, z5.);
			statecode=substr(countycode,1,2);

			annualdeaths2018=deaths;
			annualmortality2018= crude_rate;
			urbanrural=compress(_013_Urbanization_Code||_013_Urbanization);
			if _013_Urbanization_Code = . then urbanrural= " ";
			drop statex countyx;
			run;


 				PROC IMPORT OUT= WORK.statepopulation                         
                        DATAFILE= "&datadir.\CDC_Urban_Rural\State_2018_pop.xlsx" 
                        DBMS=EXCEL REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;


			data statepopulation2;
			set statepopulation;
				statex=state_code;
				statename=state;
			drop state;
			run;

			data statepopulation3;
			set statepopulation2;
				state=state_code;
			drop statex state_code;
			run;

		proc means data=statepopulation3 sum ;
		ods output summary=nationalpop;
		run;

		data nationalpop2;
		set nationalpop;
		population=Population_Sum;
		state = .;
		county =.;
		nation=1;
		keep  population state nation county;
		run;


			data population_all;
			set urbancodes3 statepopulation3 nationalpop2;
			deaths_allcause=deaths;
			drop deaths;
			run;

*****************************************************************************
			State level hospitalization and testing data
*****************************************************************************;
	PROC IMPORT OUT= WORK.testhosp 
            DATAFILE= "C:\Users\spate41\Box Sync\COVID19_data_shared\Hospitalizations and testing\state_level.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


	data testhosp2;
	length hospitalTot 30.;
	set testhosp;
	county=.;
	sasdate=input(date,YYMMDD10.);
	if hospitalizations ne "NA" then hospitalTot=hospitalizations+0;
	if hospitalizationRate ne "NA" then hospitalRate=hospitalizationRate+0;
	drop hospitalizations hospitalizationRate nation date;
	run;

	proc sort data=testhosp2; by state sasdate; run;
	data testhosp3;
	set testhosp2;
	by state sasdate;
	nation=.;
	date=sasdate;
	if last.state=1 and last.sasdate=1 then output;
	drop sasdate;
	format date YYMMDD10.;
	run;


*****************************************************************************
			COVID outcome data with population denominator 
*****************************************************************************;

			
	proc sort data=population_all; by nation state county ; run;
	proc sort data=covid_all; by nation state county ; run;
	proc sort data=mean7day_current; by nation state county ; run;
	proc sort data=testhosp3; by nation state county ; run;

	data covid_pop;
	merge population_all covid_all mean7day_current testhosp3;
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;

	refzero=0;
    if state=0 or (nation=. and state=. and county=.) then delete;
	run;



	proc sort data=population_all; by nation state county ; run;
	proc sort data=covidtimeseries; by nation state county ; run;
	data covidtimeseries_pop;
	merge population_all covidtimeseries;
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;

	refzero=0;
	level=compress(state||county||nation);
    if state=0 or (nation=. and state=. and county=.) then delete;
	run;


PROC EXPORT DATA= WORK.Covidtimeseries_pop 
            OUTFILE= "&datadir.\Yubin\covidtimeseries.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






*************************************************************;
* Date: May 2020
* Data: American Community Survey 2014-2018 5-Year Estimates, Nation, State and County
* Data Source: Social explorer 
*************************************************************;
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
	
	county = " ";
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

	county = " ";
	state = " ";
	nation=1;
	keep state nation county popden age65over  male  female black hispanic minority groupquater college hhincome poverty;
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
	if state in (66 72 78) then delete;
	run;



*****************************************************************
CDC SVI 2018 Documentation - 1/31/2020
*****************************************************************;
	DATA svi2018_us_county;	
	SET "&datadir.\CDC SVI\svi2018_us_county";
	fipscode=put(FIPS,z5.);

	*countyname=county;
	*statename=state;
	countyx=(substr(fipscode,3,3))+0;
	statex=ST;
	if st in (66 72 78) then delete;
	keep RPL_THEME1 RPL_THEME2 RPL_THEME3 RPL_THEME4 countyx statex ;
	run;

	data SVI;
	set svi2018_us_county;
	county=countyx;
	state=statex;
	nation=.;
	drop countyx statex;
	run;



***************************************************************;
*CDC diabetes Atlas data;
***************************************************************;
		
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
	county = .;
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
	county =.;
	state = .;
	nation=1;
	run;
	
	data CDCdiabetes;
	set diabetescounty  diabetesstate diabetesnation;
	if state in (66 72 78) then delete;
	drop fips;
	run;					

*******************************************************************;
*SAIHE Health insurance files;
*******************************************************************;
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
	nation=.;
	drop statex countyx;
	run;

proc means data=sahie_2018 stackods;
class state;
var PCTUI;
ods output summary=sahie_2018_state;
run;

data sahie_2018_state2;
set sahie_2018_state;
county=.;
nation=.;
PCTUI=mean;
keep state county nation PCTUI;
run;



proc means data=sahie_2018 stackods;
var PCTUI;
ods output summary=sahie_2018_nat;
run;

data sahie_2018_nat2;
set sahie_2018_nat;
county=.;
state=.;
nation=1;
PCTUI=mean;
keep state county nation PCTUI;
run;

data sahie_2018_merge;
set sahie_2018 sahie_2018_state2 sahie_2018_nat2;
run;


*Merging the data sources;
		proc sort data=acs2018_5yr_all; by state county nation; run;
		proc sort data=svi; by state county nation;  run;
		proc sort data=CDCdiabetes; by state county nation; run;
		proc sort data=sahie_2018_merge;  by state county nation; run; 
		proc sort data=Covid_pop;  by state county nation; run; 

		data mergedsocial;
		merge acs2018_5yr_all svi CDCdiabetes sahie_2018_merge Covid_pop;
		by state county nation;
		run;

		data "&datadir.\mergedsocial";
		set mergedsocial;
		run;


		PROC EXPORT DATA= WORK.mergedsocial 
		            OUTFILE= "&datadir.\Yubin\nationalraw.csv" 
		            DBMS=CSV REPLACE;
		     PUTNAMES=YES;
		RUN;



proc format;
value $ varlabels
    
"age65over"  = "% Over 65 y"
"male"  = "% Male"     
"black"  =  "% African American"  
"poverty"  = "% in Poverty"
"PCTUI" = "% Uninsured"
"diabetes"="% Diabetes"
"obesity"="% Obese"
"groupquater"  = "% in Group Quarters" 

"RPL_THEME1" = "Socioeconomic Vulnerability"
"RPL_THEME2" = "Household Composition Vulnerability"
"RPL_THEME3" = "Minority/Language Vulnerability"
"RPL_THEME4" = "Housing/Transportaion Vulnerability"
"popden" = "Population Density (Per Sq. Mile)"
"_013_Urbanization_Code"="Urban-Rural Status"
"_013_Urbanization"="Urban-Rural Status"

"cases"="Total COVID-19 Cases"
"deaths"="Total COVID-19 Deaths"
"mean7daycases"="Daily Cases (7-day Average)"
"mean7daydeaths"="Daily Deaths (7-day Average)"

"covidmortality"="Total Deaths per 100,000"
"caserate"="Total Cases per 100,000"

"covidmortality7day"="Daily Deaths per 100,000 (7-day Average)"
"caserate7day"="Daily Cases per 100,000 (7-day Average)"

"hospitalTot"="Total Hospitalizations" 
"hospitalRate"="Hospitalizations per 100,000"
"tests" ="Total Tests"
"testingRate"="Tests per 100,000"

"female"  =  "% Female population"       
"hispanic" = "% Hispanic or Latino"
"minority"  = "% Other than not Hispanic or Latino, White Alone"
"college"  = "% Bachelor's Degree or Better, age 25 and over"
"hhincome"  = "Average Household Income (In 2018 Inflation Adjusted Dollars)"

;


proc means data=mergedsocial stackods;
		var _013_Urbanization_Code black poverty PCTUI diabetes obesity age65over groupquater male hhincome college RPL_THEME1 RPL_THEME2 RPL_THEME3 RPL_THEME4
		cases  deaths mean7daycases mean7daydeaths covidmortality caserate covidmortality7day caserate7day hospitalTot hospitalRate tests testingRate;
		ods output summary=names;
		run;

		data order;
		set names;
		order=_n_;
		run;

		proc sort data=names; by variable; run;
		proc sort data=order; by variable; run;

		data names2;
		merge names order;
		by variable;
		varname=variable;
		varlabel=put(variable,$varlabels.);
		if variable=_013_Urbanization_Code then varlabel="Urban-Rural Code";
		if variable=_013_Urbanization_Code then varname="urbanrural";
		description="Description for "||compress(varname)||" will go here.";
		keep varname varlabel description order;
		run;

proc sort data=names2; by order; run;
PROC EXPORT DATA= WORK.names2 
            OUTFILE= "&datadir.\Yubin\datadescription.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;







**********************************************************************************
*                                DOUBLING TIME 
*********************************************************************************;

*doubling time = (number of days passed*ln(2))/(ln(end day cases/ beginning day total cases));

proc sort data=covidtimeseries; by state county date; run;
data dt0;
set covidtimeseries;
where month(date)=5 and day(date)>=10;
by state county date;
begincases=lag6(cases);
endcases=cases;
if last.county=1 then dt= (6*log(2)) / (log(endcases/begincases));
if dt=. or county =. then delete;
keep fips state county date cases deaths begincases endcases dt;
run;


data "C:\Users\spate41\Box Sync\COVID19_data_shared\Doubling_time\shivanidt";
set dt0;
run;








*=============== Test graphics;


ods pdf file ="&datadir.\ExampleOutputs\TestPlots_18May2020.pdf" style=pearl;
*Time series ============;
proc sort data=covidtimeseries_pop; by date state county; run;
proc sgplot data=covidtimeseries_pop ;
where (state=13 and county =095) or (state=13 and county =.) or nation=1;
Title "7-day Moving average of cases per population: County, state, nation";
*highlow x=date low=zero high=caserate7day/group=county LINEATTRS=(color="#b4cd95" thickness=5);
series x=date y=caserate7day/group=level  LINEATTRS=( thickness=4);
label caserate7day="Cases per 100,000"  date="Date";
*format excessdeaths percent7.;
run;

*Time series ============ Testing a weird county;
proc sort data=covidtimeseries_pop; by date state county; run;
proc sgplot data=covidtimeseries_pop ;
where (state=13 and county =091) or (state=13 and county =.) or nation=1;
Title "7-day Moving average of cases per population:Dodge County, state, nation";
*highlow x=date low=zero high=caserate7day/group=county LINEATTRS=(color="#b4cd95" thickness=5);
series x=date y=caserate7day/group=level  LINEATTRS=( thickness=4);
label caserate7day="Cases per 100,000"  date="Date";
*format excessdeaths percent7.;
run;

proc sort data=covidtimeseries_pop; by date state county; run;
proc sgplot data=covidtimeseries_pop ;
where (state=13 and county =091) or (state=13 and county =.) or nation=1;
Title "7-day Moving average of deaths per population: County, state, nation";
*highlow x=date low=zero high=caserate7day/group=county LINEATTRS=(color="#b4cd95" thickness=5);
series x=date y=covidmortality7day/group=level  LINEATTRS=( thickness=4);
label covidmortality7day="Deaths per 100,000"  date="Date";
*format excessdeaths percent7.;
run;

proc sort data=covidtimeseries_pop; by date state county; run;
proc sgplot data=covidtimeseries_pop ;
where (state=13 and county =095) or (state=13 and county =.) or nation=1;
Title "Total deaths per population: County, state, nation";
*highlow x=date low=zero high=caserate7day/group=county LINEATTRS=(color="#b4cd95" thickness=5);
series x=date y=covidmortality/group=level  LINEATTRS=( thickness=4);
label covidmortality="Cumulative deaths per 100,000"  date="Date";
*format excessdeaths percent7.;
run;




*============= plots by quartile;
%macro hbarplot(data=, exp=,outcome=,xlabel=,ylabel=);
proc sgplot data=&data. noautolegend aspect=0.7;
where county ne .;
	hbar &exp.  /response=&outcome. stat=mean fillattrs=(color="#007698") nooutline;
  label &exp.="&xlabel." &outcome.="&ylabel.";
  run;
%mend hbarplot;

title "County factors X Covid Outcomes";
%hbarplot(data=Mergedsocial, exp=urbanrural, outcome=covidmortality7day, xlabel=Metropolitan status, ylabel=COVID-19 7-day mortality);
%hbarplot(data=Mergedsocial, exp=urbanrural, outcome=caserate7day, xlabel=Metropolitan status, ylabel=COVID-19 7-day incidence);

ods pdf close;






%hbarplot(data=countyquintile,  exp=black, xlabel=Quintile % Black, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=black, xlabel=Quintile % Black, outcome=caseratecounty,  ylabel=COVID-19 incidence);

%hbarplot(data=countyquintile,  exp=minority, xlabel=Quintile % Minority, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=minority, xlabel=Quintile % Minority, outcome=caseratecounty,  ylabel=COVID-19 incidence);


%hbarplot(data=countyquintile,  exp=age65over, xlabel=Quintile % >65y, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=age65over, xlabel=Quintile % >65y, outcome=caseratecounty,  ylabel=COVID-19 incidence);

%hbarplot(data=countyquintile,  exp=groupquater, xlabel=Quintile % Group quarter, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=groupquater, xlabel=Quintile % Group quarter, outcome=caseratecounty,  ylabel=COVID-19 incidence);

%hbarplot(data=countyquintile,  exp=diabetes, xlabel=Quintile % diabetes, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=diabetes, xlabel=Quintile % diabetes, outcome=caseratecounty,  ylabel=COVID-19 incidence);

%hbarplot(data=countyquintile,  exp=hhincome, xlabel=Quintile HH Income, outcome=covidmortalitycounty,  ylabel=COVID-19 mortality);
%hbarplot(data=countyquintile,  exp=hhincome, xlabel=Quintile HH Income, outcome=caseratecounty,  ylabel=COVID-19 incidence);


proc print data=mergedsocial;
where countyname="Wayne County, MI";
run;





*==================================== MAPS ===============================;
	pattern1 c='#A7A5C6'; 
	pattern2 c='#6699CC';
	pattern3  c='#88AB75';
	pattern4 v=s c='#A6A57A';
	pattern5 v=s c='#FFF275';
	pattern6 v=s c='#FF8C42';
	pattern7 v=s c='#A51122';
			
*http://colorspace.r-forge.r-project.org/articles/hcl_palettes.html#flexible-diverging-palettes";
*https://blogs.sas.com/content/sastraining/2017/04/12/your-mapping-toolkit-tip-3-removing-internal-borders/;


		Title "COVID Cases - Revised Palette From Jeff";
		proc gmap 
			map=maps.uscounty
			data=Mergedsocial ;
			id state county;
			choro cases/ levels =7  coutline=SAME CEMPTY=lightgray; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;












pattern1 c='#3b99b1'; 
	pattern2 c='#56b29e';
	pattern3  c='#9fc095';
	pattern4 v=s c='#eacb2b';
	pattern5 v=s c='#38a419';
	pattern6 v=s c='#e87700';
	pattern7 v=s c='#f4191c';
			
*http://colorspace.r-forge.r-project.org/articles/hcl_palettes.html#flexible-diverging-palettes";
*https://blogs.sas.com/content/sastraining/2017/04/12/your-mapping-toolkit-tip-3-removing-internal-borders/;


		Title "COVID Cases - Palette Zissou1";
		proc gmap 
			map=maps.uscounty
			data=Mergedsocial ;
			id state county;
			choro cases/ levels =7  coutline=SAME CEMPTY=lightgray; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;







* color explore ;
*===================== MAPS;

ods pdf file="&datadir.\maps\colors2.pdf";

	pattern1 c='#3b99b1'; 
	pattern2 c='#56b29e';
	pattern3  c='#9fc095';
	pattern4 v=s c='#eacb2b';
	pattern5 v=s c='#38a419';
	pattern6 v=s c='#e87700';
	pattern7 v=s c='#f4191c';
			
*http://colorspace.r-forge.r-project.org/articles/hcl_palettes.html#flexible-diverging-palettes";
*https://blogs.sas.com/content/sastraining/2017/04/12/your-mapping-toolkit-tip-3-removing-internal-borders/;


		Title "COVID Cases - Palette Zissou1";
		proc gmap 
			map=maps.uscounty
			data=countymap2 ;
			id state county;
			choro cases/ levels =7  coutline=SAME CEMPTY=lightgray; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;




	pattern1 c='cx2a9d8f'; 
	pattern2 c='cxe9c46a';
	pattern3  c='cxf4a261';
	pattern4 v=s c='cxe76f51';
	*pattern5 v=s c='black' ;
		Title "COVID Cases - Palette 1";
			proc gmap 
			map=maps.uscounty
			data=countymap2 ;
			id state county;
			choro cases/ levels =4   coutline=SAME CEMPTY=lightgray ; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;


	pattern1 c='cx48c8a1'; 
	pattern2 c='cxf3ffbd';
	pattern3  c='cxe9c46a';
	pattern4 v=s c='cxe76f51';
	*pattern5 v=s c='black' ;
		Title "COVID Cases - Palette 2";
			proc gmap 
			map=maps.uscounty
			data=countymap2 ;
			id state county;
			choro cases/ levels =4   coutline=SAME CEMPTY=lightgray ; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;



	pattern1 c='#D7AD3A'; 
	pattern2 c='#1FA36E';
	pattern3  c='#BADFA5';
	pattern4 v=s c='#F6E8AD';
	pattern5 v=s c='#D7AD3A';
	pattern6 v=s c='#E60B09' ;
		Title "COVID Cases - Palette 3";
			proc gmap 
			map=maps.uscounty
			data=countymap2 ;
			id state county;
			choro cases/ levels =6   coutline=SAME CEMPTY=lightgray; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;


pattern1 c='cx61c674'; 
pattern2 c='cxb2dbbf'; 
pattern3 c='cxf3ffbd';
pattern4  c='cxe9c46a';
pattern5 v=s c='cxe76f51';
*pattern5 v=s c='black' ;
	Title "COVID Cases - Palette 4";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		id state county;
		choro cases/ levels =5  coutline=same cempty=lightgray ; 
		label cases = "Covid County Deaths";
		*format deaths map1f.;
		run;
		quit;
ods pdf close;
