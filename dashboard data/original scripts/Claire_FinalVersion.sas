*Edited June 22nd to add % native americans
*Edited June 2nd to fix missing state data in Nationalraw file;
*Edited June 12 to assign counties with no COVID data as -1 ;


*Month and day of current downloand;
%let month=10;
%let day=04;
%let year=2021;
*Data directory - shivani; 

%let onedrive=C:\Users\china\OneDrive - Emory University\CovidHealthEquityDashboard\Data;
%let datadir=C:\Users\china\Downloads\data;  *datadir is your preferred local path;
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
			set Countiesnytimes (in=c) statesnytimes (in=s) USnytimes (in=n);
			where year(date)=&year. and month(date)=&month. and day(date)=&day.;
			fips_code=put(fips,z5.);
			if c=1 then countyx=(substr(fips_code,3,3))+0;
			if c=1 then statex=(substr(fips_code,1,2))+0;
			if s=1 then statex=fips;
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

			proc print data=covid_all;
			where county=.;
			var date state cases deaths;
			run;
*
PROC EXPORT DATA= WORK.covid_all ;
           * OUTFILE= "&datadir.\covid_all.csv" ;
           * DBMS=CSV REPLACE;
    * PUTNAMES=YES;
*RUN; 
*
proc import datafile = "&onedrive.\Processed\iowa_oklahoma.csv" out = iowa
DBMS =  csv replace;
*run;
*proc sort data=iowa;*by county;*run;
*proc sort data=Countiesnytimes;*by county;*run;

*data Countiesnytimes;
* countiesnytimes iowa;
*run;

*PROC PRINT data=iowa;
*run;


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



		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);

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
		
		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);

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

		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);


		zero=0;
        nation=1;
		run;

		data mean7day_current;
		set state_daily county_daily us_daily;
		where  year(date)=&year. and month(date)=&month. and day(date)=&day.;

  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		keep state county nation date dailydeaths mean7daydeaths percent14dayCases percent14dayDailyCases percent14dayDeaths percent14dayDailyDeaths dailycases mean7daycases zero;
		run;

		data covidtimeseries;
		set state_daily county_daily us_daily;
		where month(date)>3 or year(date)=2021;
  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		*keep state county nation date dailydeaths mean7daydeaths dailycases mean7daycases zero;
		drop statex countyx;
		run;
		proc sort data=covidtimeseries; by nation state county date; run;

			proc print data=mean7day_current;
			where county=.;
			var date state dailydeaths mean7daydeaths dailycases;
			run;


*****************************************************************
*Population denominator and 2013 Urban rural codes - From CDC Wonder
*https://www.cdc.gov/nchs/data_access/urban_rural.htm#update;
*****************************************************************;
      PROC IMPORT OUT= WORK.urbancodes                         
                        DATAFILE= "&onedrive.\Raw\CDC_Urban_Rural\Urban_Rural_2013_Classification.xlsx" 
                        DBMS=excel REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;

			/*
proc import datafile = "&onedrive.\Raw\CDC_Urban_Rural\Urban_Rural_2013_Classification.csv" out = urbancodes
DBMS =  csv replace;
guessingrows=32767;
run;
			*/




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
			urbanrural=compress(_013_Urbanization_Code||_013_Urbanization_Code);*changed this line to make the code work;
			if _2013_Urbanization_Code = . then urbanrural= " ";
			drop statex countyx;
			run;


 				PROC IMPORT OUT= WORK.statepopulation                         
                        DATAFILE= "&onedrive.\Raw\CDC_Urban_Rural\State_2018_pop.xlsx" 
                        DBMS=EXCEL REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;

/*proc import datafile = "O:\COVID19_data_shared\CDC_Urban_Rural\State_2018_pop.csv" out = statepopulation
DBMS =  csv replace;
run;*/



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

/*
proc compare data=urbancodes4(obs=0) compare=statepopulation3(obs=0) compare=nationalpop2(obs=0);
run;
data urbancodes4;
set urbancodes3;
Deaths1=input(Deaths,best32.);
Population1=input(Population,best32.);
Crude_Rate1=input(Crude_Rate,best32.);
drop Deaths Population Crude_Rate;
rename Deaths1=Deaths Population1=Population Crude_Rate1=Crude_Rate;
run;
		*/




			data population_all;
			set urbancodes3 statepopulation3 nationalpop2;
			deaths_allcause=deaths;
			drop deaths;
			run;


*****************************************************************************
			COVID outcome data with population denominator 
*****************************************************************************;

			
	proc sort data=population_all; by nation state county ; run;
	proc sort data=covid_all; by nation state county ; run;
	proc sort data=mean7day_current; by nation state county ; run;
	

	data covid_pop;
	merge population_all covid_all (in=covid) mean7day_current ;
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;


	*Revising all the 0's to be -1 for the figures;
	array acovid 		cases 		deaths 		covidmortality 		caserate 	covidmortality7day 		caserate7day;
	array acovidfigs 	casesfig 	deathsfig 	covidmortalityfig 	caseratefig covidmortality7dayfig 	caserate7dayfig;

	do over acovid;
	acovidfigs=acovid;
	if acovid in (0 .) then acovidfigs=-1;
	end;

	refzero=0;
    if state=0 or (nation=. and state=. and county=.) then delete;
	drop fips;
	run;



			proc print data=covid_pop;
			where county=.;
			var date state county nation cases deaths;
			run;



	*data checking;
			proc means data =covid_pop n nmiss min max mean;
			var  deaths   covidmortality7day   deathsfig covidmortalityfig  covidmortality7dayfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where deaths in (0 .);
			var  deaths     deathsfig covidmortalityfig   ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where deaths notin (0 .);
			var  deaths     deathsfig  covidmortalityfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where dailydeaths in (0 .) ;
			var    covidmortality7day  covidmortality7dayfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where dailydeaths notin (0 .);
			var    covidmortality7day    covidmortality7dayfig ;
			run;


			proc means data =covid_pop n nmiss min max mean;
			where cases in (0 .);
			var cases  caserate  casesfig  caseratefig  ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where cases notin (0 .) and caserate ne .;
			var cases  caserate  casesfig  caseratefig  ;
			run;

			*proc means data =covid_popn nmiss min max mean;
			*where dailycases in (0 .) ;
			*var caserate7day caserate7dayfig;
			*run;


			proc print data =covid_pop;
			where casesfig <=1 ;
			var statename covidmortality countyname population cases deaths covidmortality caserate covidmortality7day caserate7day casesfig deathsfig covidmortalityfig caseratefig covidmortality7dayfig caserate7dayfig deaths nation ;
			run;

			*Looking at the distribtuion of the quantiles for plotting;
			proc rank data=covid_pop
			groups=7 out=ranks;
			where covidmortalityfig ne -1;
			var covidmortalityfig;
			ranks covidmortalityfigHept;
			run;

			proc means data=ranks;
			class covidmortalityfigHept;
			var covidmortality;
			run;


	proc sort data=population_all; by nation state county ; run;
	proc sort data=covidtimeseries; by nation state county ; run;
	data covidtimeseries_pop;
	merge population_all covidtimeseries (in=covid);
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	dailycaserate=100000*dailycases/population;
    dailymortality=100000*dailydeaths/population;


	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;

	
	*case fatality ratio;
	cfr=100*deaths/cases;

	*Revising all the 0's to be -1 for the figures;
	array acovid 		cases 		deaths 		covidmortality 		dailycaserate  		dailymortality 		caserate 	covidmortality7day 		caserate7day;
	array acovidfigs 	casesfig 	deathsfig 	covidmortalityfig 	dailycaseratefig  	dailymortalityfig 	caseratefig covidmortality7dayfig 	caserate7dayfig;

	do over acovid;
	acovidfigs=acovid;
	if acovid in (0 .) then acovidfigs=-1;
	end;

	refzero=0;
	level=compress(state||county||nation);
    if state=0 or (nation=. and state=. and county=.) then delete;
	run;

data Covidtimeseries_pop1;
set Covidtimeseries_pop;
length _2013_Urbanization $100.;
run;
proc freq data=Covidtimeseries_pop1;
tables _2013_Urbanization;
run;





PROC EXPORT DATA= WORK.Covidtimeseries_pop 
            OUTFILE= "&onedrive.\Upload\covidtimeseries00.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= WORK.Covidtimeseries_pop 
            OUTFILE= "&datadir.\covidtimeseries00.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


*************************************************************;
* Date: May 2020
* Data: American Community Survey 2014-2018 5-Year Estimates, Nation, State and County
* Data Source: Social explorer 
*************************************************************;
	Data acs2018_5yr_county; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_county"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100;
    natives =   A03001_004/ A00001_001 * 100;
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county=(substr(fips,3,3))+0;
	state=(substr(fips,1,2))+0;
	keep county state popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
	run;

	Data acs2018_5yr_state; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_state"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100; 
	natives =   A03001_004/ A00001_001 * 100; 
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 
	
	county = " ";
	state = (substr(fips,1,2))+0;
	keep state county popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
	run;

	Data acs2018_5yr_nation; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_nation"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100; 
	natives =   A03001_004/ A00001_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county = " ";
	state = " ";
	nation=1;
	keep state nation county popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
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
	SET "&onedrive.\raw\CDC SVI\svi2018_us_county";
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
	SET "&onedrive.\raw\CDC Diabetes Surveillance\county";
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

	proc means data=diabetescounty;
	where state notin (66 72 78);
	class state;
	var diabetes obesity;
	ods output summary=diabetesstate0;
	run;

	proc means data=diabetescounty;
	where state notin (66 72 78);
	var diabetes obesity;
	ods output summary=diabetesnation0;
	run;

	data diabetesnation;
	set diabetesnation0;
	diabetes=diabetes_Mean;
	obesity=obesity_mean;
	county=.;
	state=.;
	nation=1;
	keep diabetes obesity nation county state;
	run;

	data diabetesstate;
	set diabetesstate0;
	diabetes=diabetes_Mean;
	obesity=obesity_mean;
	county=.;
	nation=.;
	keep diabetes obesity nation county state;
	run;


/* these numbers don't align;
	DATA diabetesstate1;	
	SET "&datadir.\CDC Diabetes Surveillance\state"; *Need to remove the territories;
	county = .;
	statex=put(fips, 2.0);
	drop state;
	run;
	DATA diabetesnation;	
	SET "&datadir.\CDC Diabetes Surveillance\nation";
	county =.;
	state = .;
	nation=1;
	run;
*/
	
	data CDCdiabetes;
	set diabetescounty  diabetesstate diabetesnation;
	if state in (66 72 78) then delete;
	drop fips;
	run;					

*******************************************************************;
*SAIHE Health insurance files;
*******************************************************************;
	data sahie_20181;
	set "&onedrive.\raw\SAHIE\sahie_2018_new";
	statex=put(statefips,8.);
	countyx=put(countyfips,8.);
	keep statex countyx PCTUI PCTIC;
	run;

	data sahie_2018;
	set sahie_20181;
	state=input(statex,8.0);
	county=input(countyx,8.0);
	if county=0 then county=.;
	nation=.;
	drop statex countyx;
	run;

/*This file is not needed - state is covered in county;
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
*/


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
set sahie_2018 sahie_2018_nat2;
run;


*Merging the data sources;
		proc sort data=acs2018_5yr_all; by nation state county ; run;
		proc sort data=svi; by nation state county ;  run;
		proc sort data=CDCdiabetes; by nation state county ; run;
		proc sort data=sahie_2018_merge;  by nation state county ; run; 
		proc sort data=Covid_pop;  by nation state county ; run; 

		data mergedsocial;
		merge acs2018_5yr_all svi CDCdiabetes sahie_2018_merge Covid_pop;
		by nation state county ;
		cfr=100*deaths/cases;
		run;

	*data checking;
			proc print data=mergedsocial;
			where county=.;
			var date state cases deaths;
			run;

			proc means data =mergedsocial;
			where cases>0;
			var cases deaths covidmortality caserate covidmortality7day caserate7day cfr;
			run;

			proc print data =mergedsocial;
			where deaths=-1;
			var cases deaths covidmortality caserate covidmortality7day caserate7day;
			run;



		


proc sort data=mergedsocial; by nation state county; run;
		PROC EXPORT DATA= WORK.mergedsocial 
		            OUTFILE= "&onedrive.\Upload\nationalraw0.csv" 
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
"cfr"="Case fatality ratio"

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
		var _013_Urbanization_Code black hispanic poverty PCTUI diabetes obesity age65over groupquater male hhincome college RPL_THEME1 RPL_THEME2 RPL_THEME3 RPL_THEME4
		cases  deaths mean7daycases mean7daydeaths covidmortality caserate covidmortality7day caserate7day cfr; *hospitalTot hospitalRate tests testingRate;
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
            OUTFILE= "&datadir.\datadescription.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*
proc sort data=names2; *by order; *run;
*PROC EXPORT DATA= WORK.names2 
            OUTFILE= "&onedrive.\Upload\datadescription.csv" 
            DBMS=CSV REPLACE;
     *PUTNAMES=YES;
*RUN;



*======================== Hospitalization data from CDC ===============;
%let Hospital = &datadir.;

* Library;
libname rawdata "&Hospital" ;

OPTIONS NOSOURCE;

* Data are imported from COVID-19 Module Data Dashboard;

FILENAME website URL 'https://www.cdc.gov/nhsn/pdfs/covid19/covid19-NatEst.csv';

FILENAME COPY "&Hospital/covid19.csv";

  data _NULL_;

   n=-1;

     infile website recfm=s nbyte=n length=len;

     file copy recfm=n;

     input;

     put _infile_ $varying32767. len;

run;

filename copy clear;



proc import datafile = "&Hospital/covid19.csv" out = rawdata.covid19
DBMS =  csv replace;
run;

data rawdata.covid191;
set rawdata.covid19;
if _n_ < 3 then delete;
keep  statename collectionDate InBedsOccAnyPat__Numbeds_Est InBedsOccCOVID__Numbeds_Est ICUBedsOccAnyPat__N_ICUBeds_Est;
rename InBedsOccAnyPat__Numbeds_Est=bedsAll InBedsOccCOVID__Numbeds_Est=bedsCovid ICUBedsOccAnyPat__N_ICUBeds_Est=bedsIcu;
run;

data rawdata.covid192;
set rawdata.covid191;
if statename ="Alabama" then state=01;
if statename ="Alaska" then state=02;
if statename ="Arizona" then state=04;
if statename ="Arkansas" then state=05;
if statename ="California" then state=06;
if statename ="Colorado" then state=08;
if statename ="Connecticut" then state=09;
if statename ="Delaware" then state=10;
if statename ="District of C" then state=11;
if statename ="Florida" then state=12;
if statename ="Georgia" then state=13;
if statename ="Hawaii" then state=15;
if statename ="Idaho" then state=16;
if statename ="Illinois" then state=17;
if statename ="Indiana" then state=18;
if statename ="Iowa" then state=19;
if statename ="Kansas" then state=20;
if statename ="Kentucky" then state=21;
if statename ="Louisiana" then state=22;
if statename ="Maine" then state=23;
if statename ="Maryland" then state=24;
if statename ="Massachusetts" then state=25;
if statename ="Michigan" then state=26;
if statename ="Minnesota" then state=27;
if statename ="Mississippi" then state=28;
if statename ="Missouri" then state=29;
if statename ="Montana" then state=30;
if statename ="Nebraska" then state=31;
if statename ="Nevada" then state=32;
if statename ="New Hampshire" then state=33;
if statename ="New Jersey" then state=34;
if statename ="New Mexico" then state=35;
if statename ="New York" then state=36;
if statename ="North Carolin" then state=37;
if statename ="North Dakota" then state=38;
if statename ="Ohio" then state=39;
if statename ="Oklahoma" then state=40;
if statename ="Oregon" then state=41;
if statename ="Pennsylvania" then state=42;
if statename ="Rhode Island" then state=44;
if statename ="South Carolin" then state=45;
if statename ="South Dakota" then state=46;
if statename ="Tennessee" then state=47;
if statename ="Texas" then state=48;
if statename ="Utah" then state=49;
if statename ="Vermont" then state=50;
if statename ="Virginia" then state=51;
if statename ="Washington" then state=53;
if statename ="West Virginia" then state=54;
if statename ="Wisconsin" then state=55;
if statename ="Wyoming" then state=56;
if statename ="Puerto Rico" then state=72;

date1 = input(collectionDate,date9.);
format date1 datetime;

date = input(collectionDate,date9.);
format date yymmdd10.;

nation = . ;
county = . ;
run;

proc sort data = rawdata.covid192;
by state date1;
run;

data rawdata.hospitalization_historic;
set rawdata.covid192;
keep statename bedsAll bedsCovid bedsICU state county nation date;
run;

data rawdata.hospitalization_static;
set rawdata.covid192;
by state;
if last.state;
keep statename bedsAll bedsCovid bedsICU state county nation date;
run;

proc export data=rawdata.hospitalization_historic
outfile = "&Hospital/hospitalization_historic.csv"
dbms=csv replace;
run;

proc export data=rawdata.hospitalization_static
outfile = "&Hospital/hospitalization_static.csv"
dbms=csv replace;
run;

proc datasets library=rawdata;
delete covid19 covid191 covid192 ;
run;










proc means data=mergedsocial; 
where caserate7dayfig=-1;
var caserate7dayfig;
run;

proc means data=covidtimeseries_pop; 
var dailycaserate caserate7dayfig caserate;
run;

data attrs;
input id $ value $ linecolor $;
datalines;
level 1395.  #af862c
level 13..   #b2b3b3
level ..1    #d9d9d7
;
run;


proc sort data=covidtimeseries_pop; by descending dailycases; run;
data plots;
set covidtimeseries_pop;
if month(date)=&month and day(date)=&day then casestoday=dailycaserate;
if month(date)=&month and day(date)=&day then deathstoday=covidmortality7day;
if state=13 and county=95 then caserateplot=dailycaserate;
if state=13 and county=95 then deathrateplot=covidmortality7day;
if state=13 and county=95 then groupplot=1;
run;



*Test data;
proc sort data=plots; by date state county; run;
proc sgplot data=plots dattrmap=attrs;;
where (state=13 and county =095) or (state=13 and county =.) or nation=1;
Title "7-day Moving average of cases per population: County, state, nation";
highlow x=date low=zero high=caserateplot/group=groupplot  LINEATTRS=(color="#e6c98a" thickness=2);
series x=date y=caserate7dayfig/group=level  LINEATTRS=( thickness=2)  attrid=level datalabel=casestoday;
label caserate7dayfig="Cases per 100,000"  date="Date";
format casestoday 5.1;
run;


proc sort data=plots; by date state county; run;
proc sgplot data=plots ;
where (state=13 and county =121) or nation=1;
Title "Daily new COVID-19 cases per capita: Fulton County vs USA";
highlow x=date low=zero high=caserateplot/group=groupplot  LINEATTRS=(color="#e6c98a" thickness=2);
series x=date y=caserate7dayfig/group=level  LINEATTRS=( thickness=3)  datalabel=casestoday datalabelattrs=(size=12);
label caserate7dayfig="Cases per 100,000 (7-d average)"  date="Date";
yaxis labelattrs=(size=12) valueattrs=(size=12);
xaxis labelattrs=(size=12) valueattrs=(size=12);
format casestoday 5.1;
run;


proc sort data=plots; by date state county; run;
proc sgplot data=plots ;
where (state=12 and county=.) or nation=1;
Title "Daily new COVID-19 cases per capita: Georgia vs USA";
highlow x=date low=zero high=caserateplot/group=groupplot  LINEATTRS=(color="#e6c98a" thickness=2);
series x=date y=caserate7dayfig/group=level  LINEATTRS=( thickness=3)  datalabel=casestoday datalabelattrs=(size=12);
label caserate7dayfig="Cases per 100,000 (7-d average)"  date="Date";
yaxis labelattrs=(size=12) valueattrs=(size=12);
xaxis labelattrs=(size=12) valueattrs=(size=12);
format casestoday 5.1;
run;

ods pdf file = "&onedrive.\Upload\NationalCurves.pdf";
*Case series for the nation;
proc sort data=plots; by date state county; run;
proc sgplot data=plots dattrmap=attrs noautolegend;
where nation=1;
Title "7-day Moving average of cases per population in the US";
highlow x=date low=zero high=dailycaserate/LINEATTRS=(color="#e6c98a" thickness=2);
series x=date y=caserate7dayfig/LINEATTRS=( thickness=3 color="#af862c" )  attrid=level datalabel=casestoday;
xaxis VALUEATTRS=(family="Lato");
label dailycaserate="Cases per 100,000"  date="Date";
format casestoday 5.1;
run;

*Case series for the nation;
proc sort data=plots; by date state county; run;
proc sgplot data=plots dattrmap=attrs noautolegend;
where nation=1;
Title "7-day Moving average of deaths per population in the US";
highlow x=date low=zero high=dailymortality/LINEATTRS=(color="#e6c98a" thickness=2);
series x=date y=covidmortality7day/LINEATTRS=( thickness=3 color="#af862c" )  attrid=level datalabel=deathstoday;
xaxis VALUEATTRS=(family="Lato");
label dailymortality="Deaths per 100,000"  date="Date";
format casestoday 5.1;
run;

*By county;

data countyrank0;
set plots;
where county ne . and month(date)=&month and day(date)=&day ;
run;

proc sort data=countyrank0; by descending dailycases; run;
data countyrank1;
set countyrank0;
caserank=_n_;
run;

proc sort data=countyrank1; by descending dailydeaths; run;
data countyrank2;
set countyrank1;
deathrank=_n_;
keep county state nation deathrank caserank;
run;

proc sort data=plots; by nation state county; run; 
proc sort data=countyrank2; by nation state county; run; 
data plots2;
merge plots countyrank2;
by   nation state county;
if month(date)=&month and day(date)=&day then display=compress(countyname||":"||put(dailycaserate,5.2));
if month(date)=&month and day(date)=&day then display=compress(countyname||":"||put(covidmortality7day,5.2));
 run;

proc sort data=plots2; by date county; run;
proc sgplot data=plots2 dattrmap=attrs;;
where 1 <= caserank <= 5 and county ne .;
Title "7-day Moving average of cases per population in counties with largest increase in daily cases";
series x=date y=caserate7dayfig/group=level  LINEATTRS=( thickness=2)  attrid=level datalabel=display;
label caserate7dayfig="Cases per 100,000"  date="Date";
format casestoday 5.1;
run;

proc sort data=plots2; by date county; run;
proc sgplot data=plots2 dattrmap=attrs;;
where 1 <= deathrank <= 5 and county ne .;
Title "7-day Moving average of deaths per population in counties with largest increase in daily deaths";
series x=date y=covidmortality7day/group=level  LINEATTRS=( thickness=2)  attrid=level datalabel=display;
label covidmortality7day="Deaths per 100,000"  date="Date";
format casestoday 5.1;
run;



*Example map;
	pattern1 c='#e1dce2'; 
	pattern2 c='#d3b6cd';
	pattern3  c='#bf88b5';
	pattern4 v=s c='#af5194';
	pattern5 v=s c='#99528c';
	pattern6 v=s c='#633c70';


		Title "COVID Cases - Totals to date";
		proc gmap 
			map=maps.uscounty
			data=Mergedsocial ;
			where nation ne 1 and county ne .;
			id state county;
			choro cases/ levels =6  coutline=SAME CEMPTY=lightgray; 
			label cases = "Covid County Cases";
			*format deaths map1f.;
			run;
			quit;

ods pdf close;

data lastdate;
set us_daily nobs=nobs;
if _n_=nobs then output;
run;

proc print data=lastdate;
var date;
run;

/*

**********************************************************************************
*                                DOUBLING TIME 
*********************************************************************************;

*doubling time = (number of days passed*ln(2))/(ln(end day cases/ beginning day total cases));

proc sort data=covidtimeseries; by state county date; run;
data dt0;
set covidtimeseries;
where month(date)=&month. and day(date)=&day.;
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


PROC EXPORT DATA= WORK.dt0 
            OUTFILE=  "C:\Users\spate41\Box Sync\COVID19_data_shared\Doubling_time\shivanidt.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




*Worst and best counties;

title "New Cases per 100,000";
proc sort data=mergedsocial; by descending caserate7day; run;
proc print data=mergedsocial  (obs=10) noobs; 
var countyname caserate7day; 
run;

title "Average daily cases";
proc sort data=mergedsocial; by descending mean7daycases; run;
proc print data=mergedsocial  (obs=10) noobs; 
where county ne .;
var countyname mean7daycases; 
run;

title "New daily cases";
proc sort data=mergedsocial; by descending dailycases; run;
proc print data=mergedsocial  (obs=10) noobs; 
where county ne .;
var countyname dailycases; 
run;

title "daily Deaths per 100,000";
proc sort data=mergedsocial; by descending covidmortality7day; run;
proc print data=mergedsocial  (obs=10) noobs; 
where deaths >50;
var countyname covidmortality7day deaths; 
run;


title " Deaths per 100,000";
proc sort data=mergedsocial; by descending covidmortality; run;
proc print data=mergedsocial  (obs=10) noobs; 
where deaths>50;
var countyname covidmortality deaths; 
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
