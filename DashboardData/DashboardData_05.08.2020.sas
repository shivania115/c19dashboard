*================= CODE TO MERGE AND PROCESS DATA FOR COUNTY DASHBOARD ==================;

*Started March 30,2020;
	*Shivani;
*Updated May 11, 2020;
	*Shivani;




*Month and day of current downloand;
%let month=5;
%let day=13;

*Data directory - shivani;
%let datadir = C:\Users\spate41\Box Sync\COVID19_data_shared;





*Hopkins COVID .... these need a lot of data cleaning;
		filename hopkins temp;
		proc http
		 url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
		 method="GET"
		 out=hopkins;
		run;
		proc import
		  file=hopkins
		  out=work.hopkins replace
		  dbms=csv;
		run;


		proc means data=hopkins mean n sum;
		where Province_State="Georgia" and Country_Region="US";
		run;





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



*============================== 2018 Monthly deaths by state =================================;
PROC IMPORT OUT= WORK.Cdcdeathsstate
            DATAFILE= "&datadir.\NCHS_deaths\CDC2018_deaths_temp.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'2018_State_month_deaths$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;


*============================== 2018 Monthly deaths by county  =================================;
PROC IMPORT OUT= WORK.cdcstates1 
            DATAFILE= "&datadir.\NCHS_deaths\Underlying Cause of Death, 1999-2018_States1_10.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.cdcstates2 
            DATAFILE= "&datadir.\NCHS_deaths\Underlying Cause of Death, 1999-2018_States11_20.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.cdcstates3 
            DATAFILE= "&datadir.\NCHS_deaths\Underlying Cause of Death, 1999-2018_States21_30.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.cdcstates4 
            DATAFILE= "&datadir.\NCHS_deaths\Underlying Cause of Death, 1999-2018_States31_40.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.cdcstates5 
            DATAFILE= "&datadir.\NCHS_deaths\Underlying Cause of Death, 1999-2018_States41_56.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


*============================== 2016 Population by state =================================;
PROC IMPORT OUT= WORK.Cdcpopulation
            DATAFILE= "&datadir.\NCHS_deaths\NCHS2016.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'2016_allcauseDeaths_NCHS$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;


*============================== 2018 Population by county =================================;
/*PROC IMPORT OUT= WORK.countypop 
            DATAFILE= "C:\Users\spate41\Box Sync\COVID_19\NCHS_deaths\Underlying Cause of Death, 1999-2018_county_pop.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
*/



	PROC IMPORT OUT= WORK.countypop
	            DATAFILE= "&datadir.\NCHS_deaths\CDC_County_populationdata.xlsx" 
	            DBMS=EXCEL REPLACE;
	     RANGE="SAS$"; 
	     GETNAMES=YES;
	     MIXED=NO;
	     SCANTEXT=YES;
	     USEDATE=YES;
	     SCANTIME=YES;
	RUN;


*================================== Recoding and county analysis;

*Deaths by county in March 2018;
	data cdccountydeaths;
	set cdcstates1-cdcstates5; *the county data were extracted in batches because of time out errors;
	statecode=state_code;
	if state_code in ("66" "69" "72" "78") then delete;
	countynum=county_code+0;
	countycode=put(countynum, z5.);

	monthlydeaths=deaths;

	if county_code in (" ") then delete;
	if substr(month,1,3)in ("Mar" "Apr") then output;
	drop notes population crude_rate deaths;
	run;


proc means data= cdccountydeaths n sum;
class statecode countycode;
var monthlydeaths;
ods output summary=countymonthlydeaths; *Problem is that some months are supressed ....;
run;

*the daily deaths variable will have to be modified based on the time period;
data countydailydeaths;
set countymonthlydeaths;
if monthlydeaths_N=1 then marchaprildeaths=2*monthlydeaths_Sum;
if monthlydeaths_N=2 then marchaprildeaths=monthlydeaths_Sum;
dailydeaths=marchaprildeaths/61;
run;




*County population in 2018;
	data countypop2;
	length state $35.;
	set countypop;
		countynum=county_code+0;
		countycode=put(countynum, z5.);
		statecode=substr(countycode,1,2);

	annualdeaths2018=deaths;
	annualmortality2018= crude_rate;
	drop county_code state_code deaths crude_rate;
	run;


*Deaths due to covid by county;
	data countycovid;
	length state $35.;
	set Countiesnytimes;
	where  month(date)=&month. and day(date)=&day.;
	countycode =put(fips, z5.);
	statecode=substr(countycode,1,2);
	run;

	proc sort data=countycovid; by countycode; run;
	proc sort data=countydailydeaths; by countycode; run;
	proc sort data=countypop2; by countycode; run;
	data countycovid2;
	merge countycovid countydailydeaths countypop2;
	by countycode;


*expected deaths = number of deaths expected over the day studied based on CDC data from 2018;
	expecteddeaths=dailydeaths*44; *March 1 to April 14;

	excessdeathscounty=deaths/(deaths+expecteddeaths);

	covidmortalitycounty = 100000*deaths/population;

	caseratecounty = 100000*cases/population;

	refzero=0;

	if statecode in ("66" "69" "72" "78") then delete;
	if countycode = "    ." then delete;
		
Countyhotspot=0;
if deaths>10 then Countyhotspot=1;

	countystate=compress(county||","||state);

	format excessdeathscounty percent7.1;
	run;



data countycovid3;
set countycovid2;
countyname=county;
statename=state;
drop county state;
run;

data countymap;
set countycovid3;
county=substr(countycode,3,3)+0;
state=substr(countycode,1,2)+0;
drop fips;
run;


*============================ This is where we will merge in the Social Data =================;
*This is from the PAA 2020 paper on southern states;
data cardio;
set "C:\Users\spate41\Box Sync\COVID_19\ExcessDeathsAnalysis\cardio";
	statex=state_fips;
	countyx=county_fips+0;
	county=countyx;
	state=statex;
run;


proc sort data=countymap; by state county; run;
proc sort data=cardio; by state county; run;
data countymap2;
merge cardio countymap;
by state county;
run;

*these are the new data that Daesung put together;
data mergedsocial;
set "&datadir.\mergedsocial";
where county ne .;
fipscode=put(County_Code,z5.);
run;

proc sort data=countymap; by state county; run;
proc sort data=cardio; by state county; run;
data countymapnew;
merge mergedsocial countymap;
by state county;
if urbanrural="." then urbanrural=" ";
run;

*Quintile dataset;
   proc rank data=mergedsocial groups=5 out=quintile;
   run;

   data quintile2;
   set quintile;
   drop county state;
   run;

   data quintile3;
   set quintile2;
   	county=(substr(fipscode,3,3))+0;
	state=(substr(fipscode,1,2))+0;
	run;

proc sort data=quintile3; by state county; run;
proc sort data=countymap; by state county; run;
data countyquintile;
merge quintile3 countymap;
by state county;
run;




*================= National raw data file ====================;
		proc means data=countymap2 noprint;
		class state_fips;
		output out=statemeans;
		run;

		data statemeans2;
		set statemeans;
		where _STAT_ = "MEAN";
		if state_fips=. then state_fips=0;
		drop county state date;
		run;


		*this is one dataset with nation, state, and county data all in one;
		data nationalraw;
		retain state county;
		set countymap2 statemeans2;

		if fips=" " then state=state_fips;
		if fips=" " then county=.;
		run;
		PROC EXPORT DATA= WORK.nationalraw 
		            OUTFILE= "&datadir.\Yubin\nationalraw.csv" 
		            DBMS=CSV REPLACE;
		     PUTNAMES=YES;
		RUN;







*====================== Percentile file ==========================;
*Converting to percentiles;
   proc rank data=statemeans2 groups=100 out=statepercentile;
   run;

 *Converting to percentiles;
   proc rank data=countymap2 groups=100 out=countypercentile;
   run;


		data nationalpercentile;
		retain state county;
		set countypercentile statepercentile;

		if fips=" " then state=state_fips;
		if fips=" " then county=.;
		run;

PROC EXPORT DATA= WORK.NATIONALPERCENTILE 
            OUTFILE= "&datadir.\Yubin\nationalpercentile.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




*=========================== daily cases data;
proc sort data=Countiesnytimes; by state county date; run;
data countydaily; 
set Countiesnytimes;
by state county;
yesterdaydeaths=lag(deaths);
yesterdaycases=lag(cases);
if first.county then yesterdaydeaths=0;
dailydeaths=deaths-yesterdaydeaths;

if first.county then yesterdaycases=0;
dailycases=cases-yesterdaycases;

if first.county then mean7day=0;
mean7day = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

zero=0;
run;


data mean7day;
set countydaily;
	where  month(date)=&month. and day(date)=&day.;

countycode =put(fips, z5.);
countyx=(substr(countycode,3,3))+0;
statex=(substr(countycode,1,2))+0;

run;

data test;
set mean7day;
county2=county+0;
run;


proc sort data=countydaily; by state county; run;
proc sgplot data=countydaily ;
where state="Georgia" and county in ("DeKalb" "Fulton" "Gwinnett" "Dougherty"  "Cobb");
Title "Daily GA COVID-19 Deaths by date";
series x=date y=mean7day/group=county ;
*label covidmortality="Cases due to COVID-19 per 100,000 adults";
*format excessdeaths percent7.;
run;


proc means data=countydaily ;
where state="Georgia";
class date;
var mean7day;
run;

proc means data=Statesnytimes ;
where state="Georgia";
class date;
var cases;
run;



*=============== Meta data file ===================;
		*This is the file to get labels for the dataset;
proc format;
value $ varlabels
    
"excessdeathscounty"="COVID Proportionate mortality"
"deaths"="Total COVID Deaths"
"cases"="Total COVID Cases"
"covidmortalitycounty"="COVID deaths per 100,000"
"caseratecounty"="COVID cases per 100,000"
"asdr"="Cardiometabolic mortality rates, 2014-2106"
"ttpop_2016"="Population, 2012-2016"
"ttpop_2016x100k"="Population (100,000), 2012-2016"
"POPLOSS00"="Population loss"
"v053_rawvalue"="% Aged 65 years or over, 2017"
"ecotype_2015  "="% Urban"
"PCT_NHBLACK10"="% Blacks"
"PCT_HISP10"="% Hispanics"
"DiagDiabetes_2016  "=" % Diabetes, 2016"
"Obesity_2016 "="% Obesity, 2016"
"v023_rawvalue"="% Unemployed 2006-2010"
"highover_2015"=" % High school education or over, 2012-2016"
"v063_rawvalue"=" Median income, 2017 ($1000)"
"POVRATE10"="% poverty, 2010"
"v004_rawvalue"="Primary care doctors per 100,000, 2016"
"v005_rawvalue"="Preventable hospital stay, 2016"
"v132_rawvalue"="Access to exercise opportunities, 2010 & 2013"
"v125_rawvalue"="Air pollution-particulate matter, 2014"
"v137_rawvalue"="% Long commute- driving alone, 2009-2013"
"v133_rawvalue"="Food environment index, 2012"
;


proc means data=nationalraw stackods;
		var deaths cases excessdeathscounty covidmortalitycounty caseratecounty 
				asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
				POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue;
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
		description="Description for "||compress(varname)||" will go here.";
		keep varname varlabel description order;
		run;

PROC EXPORT DATA= WORK.names2 
            OUTFILE= "&datadir.\Yubin\datadescription.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;























*Example graphics;
*https://blogs.sas.com/content/graphicallyspeaking/2015/06/24/bubble-plots/;
proc means data=countymapnew;
class urbanrural;
var covidmortalitycounty caseratecounty;
run;

proc means data=countymapnew;
class urbanrural;
var caseratecounty;
run;




%macro hbarplot(data=, exp=,outcome=,xlabel=,ylabel=);
proc sgplot data=&data. noautolegend aspect=0.7;
	hbar &exp.  /response=&outcome. stat=mean fillattrs=(color="#007698") nooutline;
  label &exp.="&xlabel." &outcome.="&ylabel.";
  run;
%mend hbarplot;

ods pdf file ="&datadir.\ExampleOutputs\BarPlots.pdf" style=pearl;
title "County factors X Covid Outcomes";
%hbarplot(data=countymapnew, exp=urbanrural, outcome=covidmortalitycounty, xlabel=Metropolitan status, ylabel=COVID-19 mortality);
%hbarplot(data=countymapnew, exp=urbanrural, outcome=caseratecounty, xlabel=Metropolitan status, ylabel=COVID-19 incidence);

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
ods pdf close;




proc sgplot data=nationalraw noautolegend aspect=0.7;
where statename ne " " and covidmortalitycounty > 100;
  bubble x=POVRATE10 y=covidmortalitycounty  size=covidmortalitycounty/ splitchar='-' 
         fillattrs=(color="#007698") nooutline;
  text x=POVRATE10 y=covidmortalitycounty text=countyname / position=center;
  xaxis min=0 max=40 offsetmin=0 offsetmax=0.1 display=(nolabel) grid;
  yaxis min=95 max=400 offsetmin=0 offsetmax=0.1 display=(nolabel) grid;
  run;

proc sgplot data=nationalraw noautolegend aspect=0.7;
where statename ne " " and covidmortalitycounty > 100;
reg x=PCT_NHBLACK10 y=covidmortalitycounty;
bubble x=PCT_NHBLACK10 y=covidmortalitycounty  size=covidmortalitycounty/ splitchar='-' 
      fillattrs=(color="#007698") nooutline;
text x=PCT_NHBLACK10 y=covidmortalitycounty text=countyname / position=center;
xaxis label="% African American" min=0 max=80 offsetmin=0 offsetmax=0.1  grid;
yaxis label ="COVID deaths per 100,000" min=95 max=400 offsetmin=0 offsetmax=0.1 grid;
run;

proc means data=nationalraw ;
where statename ne " " ;
var caseratecounty cases deaths;
run;








proc sort data=countydaily; by state county; run;
proc sgplot data=countydaily noautolegend;
where month(date)>=4;
where state="Georgia" and county in ("DeKalb");
Title "Daily GA COVID-19 cases by date";
highlow x=date low=zero high=dailycases/group=county LINEATTRS=(color="#b4cd95" thickness=5);

series x=date y=mean7day/group=county  LINEATTRS=(color="#007698" thickness=4);
label dailycases="Daily New Cases" mean7day="7-day Average" date="Date";
*format excessdeaths percent7.;
run;












ods graphics on / width=8in;
ods graphics on / height=4in;
*ods pdf file="C:\Users\spate41\Box Sync\COVID_19\ExcessDeathsAnalysis\CountyExcessDeathsMarch29Data.pdf" style=pearl;

proc sort data=countycovid2; by caseratecounty; run;
proc sgplot data=countycovid2;
where caseratecounty>200;
Title "County COVID-19 Cases per 100,000 adults in March 2020";
highlow x=countystate low=refzero high=caseratecounty ;*/ highlabel=excessdeaths;
label covidmortality="Cases due to COVID-19 per 100,000 adults";
*format excessdeaths percent7.;
run;

proc sort data=countycovid2; by covidmortalitycounty; run;
proc sgplot data=countycovid2;
where covidmortalitycounty>10;
Title "County Deaths due to COVID-19 per 100,000 adults in March 2020";
highlow x=countystate low=refzero high=covidmortalitycounty ;*/ highlabel=excessdeaths;
label covidmortalitycounty="Deaths due to COVID-19 per 1,000,000 adults";
*format excessdeaths percent7.;
run;

proc sort data=countycovid2; by excessdeathscounty; run;
proc sgplot data=countycovid2;
where excessdeathscounty>.05;
Title "County excess deaths due to COVID-19 (% of all deaths) in March 2020";
highlow x=countystate low=refzero high=excessdeathscounty ;*/ highlabel=excessdeaths;
label excessdeathscounty="Excess deaths relative to March 2018 mortality";
*format excessdeaths percent7.;
run;

title "New York data: population, 2018 all cause deaths, COVID deaths";
proc report data=countycovid2;
where state="New York";
columns county totalmarchdeaths population annualdeaths2018 annualmortality2018 cases deaths excessdeathscounty covidmortalitycounty;
run;

ods pdf close;

*========================================= Maps;







*================== county hotspot anlaysis;

proc genmod data=countymap2;
class statename;
model countyhotspot (event="1")=asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue statename/
		dist =bin; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
run;


proc genmod data=countymap2;
where countyhotspot=1;
class statename;
model deaths=asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue statename/
		dist =poisson; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
run;


proc genmod data=countymap2;
where countyhotspot=1;
class statename;
model covidmortalitycounty=caseratecounty asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue statename/
		dist =poisson; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
ods output parameterestimates=covidmort_poisson;
run;


title "Outcome: COVID mortality";
proc print data=covidmort_poisson noobs;
var parameter estimate ProbChiSq;
format parameter $varlabels.;
run;



*power law - USA;
proc means data=countymap2 sum;
var cases deaths;
run;

proc means data=countymap2 n sum;
class state county;
var deaths cases;
ods output summary=countysumpowerlaw;
run;

proc sort data=countymap2; by descending cases; run;
proc print data=countymap2;
var statename countyname cases;
run;


proc sort data=countymap2; by descending deaths; run;
proc print data=countymap2;
var statename countyname deaths;
run;







*=============== Power law analysis;
proc means data=countymap2 sum;
where statename="Georgia";
var cases deaths;
run;

proc means data=countymap2 n sum;
class state statename;
var deaths cases;
ods output summary=statesumpowerlaw;
run;

proc sort data=countymap2; by state descending cases; run;
proc print data=countymap2;
where statename="Georgia";
var statename countyname cases;
run;

proc sort data=countymap2; by state descending cases; run;
proc print data=countymap2;
where statename="Louisiana";
var statename countyname cases;
run;

proc sort data=countymap2; by south; run;
proc corr data=countymap2  nomiss;
by south;
var deaths excessdeathscounty covidmortalitycounty caseratecounty 
		asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
ods output pearsoncorr=correlation; 
ods output simplestats=southcomparison;
run;

data order;
set southcomparison;
order=_n_;
if south=1 then output;
run;

proc sort data=southcomparison; by variable; run;
proc transpose data=southcomparison out=southcomparison2;
by variable;
id south;
var mean;
format mean 5.3;
run;

proc sort data=order; by variable; run; 
data southcomparison3;
merge southcomparison2 order;
by variable;
run;

proc sort data=southcomparison3; by order;
proc print data=southcomparison3 noobs label;
var variable _0 _1;
format variable $varlabels.;
label _0="Non-South Counties" _1="South Counties";
run;



proc sort data=countymap2; by south; run;
proc corr data=countymap2 noprob nomiss;
where statename="Georgia";
var deaths excessdeathscounty covidmortalitycounty caseratecounty 
		asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
ods output pearsoncorr=correlation_GA; 
ods output simplestats=GAstats;
run;

proc print data=GAstats noobs label;
format variable $varlabels.;
label _0="Non-South Counties" _1="South Counties";
run;



proc sgplot data=countymap2;
reg x=cases y=deaths;
run;


proc sort data=countymap2; by south; run;
proc corr data=countymap2 noprob nomiss;
where statename="Louisiana";
var deaths excessdeathscounty covidmortalitycounty caseratecounty 
		asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
ods output pearsoncorr=correlation_LA; 
run;

title "All Counties";
proc print data=correlation noobs label;
var variable south deaths excessdeathscounty covidmortalitycounty caseratecounty;* asdr ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue ;
format Variable $varlabels.;
label excessdeathscounty="Excess deaths" covidmortalitycounty="COV Mortality" caseratecounty="Case rate";
run;

title "Correlation between county charactgeristics and number of COVID deaths in GA";
proc print data=correlation_GA noobs label;
var variable deaths excessdeathscounty covidmortalitycounty caseratecounty;
format Variable $varlabels.;
label excessdeathscounty="Excess deaths" covidmortalitycounty="COV Mortality" caseratecounty="Case rate";
run;

title "Correlation between county charactgeristics and number of COVID deaths in Louisiana";
proc print data=correlation_LA noobs label;
var variable deaths excessdeathscounty covidmortalitycounty caseratecounty ;
format Variable $varlabels.;
label excessdeathscounty="Excess deaths" covidmortalitycounty="COV Mortality" caseratecounty="Case rate";
run;






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








	Title "% Excess COVID deaths";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		id state county;
		choro excessdeathscounty/ levels =6  coutline=lightgray ; 
		label excessdeathscounty = "Excess County Deaths";
		*format deaths map1f.;
		run;
		quit;

	Title "Primary Care Doctors in the population";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		id state county;
		choro v004_rawvalue/ levels =6  coutline=lightgray ; 
		label v004_rawvalue = "Primary Care Doctors / 100,000 ";
		format v004_rawvalue 5.2;
		run;
		quit;


	Title "Poverty";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		id state county;
		choro POVRATE10/ levels =6  coutline=lightgray ; 
		label POVRATE10 = "% poverty, 2010";
		format POVRATE10 5.2;
		run;
		quit;

proc sort data=countymap2; by descending deaths; run;
proc print data=countymap2(firstobs=10);
var statename countyname deaths;
run;



proc sort data=countymap2; by statename; run;
proc corr data=countymap2 noprob nomiss;
by statename;
var deaths excessdeathscounty covidmortalitycounty caseratecounty 
		asdr  ttpop_2016 POPLOSS00 v053_rawvalue ecotype_2015 PCT_NHBLACK10 PCT_HISP10 DiagDiabetes_2016 Obesity_2016 v023_rawvalue highover_2015 v063_rawvalue 
		POVRATE10 v004_rawvalue v005_rawvalue v132_rawvalue v125_rawvalue v137_rawvalue v133_rawvalue; *cruderate fertility_2016 foreign_2016 v136_rawvalue;
ods output pearsoncorr=statecorr; 
ods output simplestats=statestats;
run;


proc print data=statecorr noobs label;
by statename;
var variable deaths excessdeathscounty covidmortalitycounty caseratecounty ;
format Variable $varlabels.;
label excessdeathscounty="Excess deaths" covidmortalitycounty="COV Mortality" caseratecounty="Case rate";
run;








 *================= Georgia;


	Title "COVID number of deaths";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		where statename="Georgia";
		id state county;
		choro deaths/ levels =6  coutline=lightgray ; 
		label deaths = "Covid County Deaths";
		*format deaths map1f.;
		run;
		quit;

		Title "COVID excess deaths";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		where statename="Georgia";
		id state county;
		choro excessdeathscounty/ levels =6  coutline=lightgray ; 
		label excessdeathscounty = "Excess Covid County Deaths";
		*format deaths map1f.;
		run;
		quit;

		Title "COVID death rate";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		where statename="Georgia";
		id state county;
		choro covidmortalitycounty/ levels =6  coutline=lightgray ; 
		label covidmortalitycounty = "Covid Deaths / 100000 population";
		*format deaths map1f.;
		run;
		quit;

		Title "COVID 'Incidence' ";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		where statename="Georgia";
		id state county;
		choro caseratecounty/ levels =6  coutline=lightgray ; 
		label caseratecounty = "Covid Cases / 100000 population";
		*format deaths map1f.;
		run;
		quit;

	Title "Diabetes incidence";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		id state county;
		choro DiagDiabetes_2016/ levels =6  coutline=lightgray ; 
		label DiagDiabetes_2016 = "Diabetes, %, 2016";
		*format deaths map1f.;
		run;
		quit;


	Title "Total population";
		proc gmap 
		map=maps.uscounty
		data=countymap2 ;
		where statename="Georgia";
		id state county;
		choro ttpop_2016x100k/ levels =6  coutline=lightgray ; 
		label ttpop_2016x100k = "Total population";
		*format deaths map1f.;
		run;
		quit;




















