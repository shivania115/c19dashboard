*Georgia specific data for inquries;

*Run the national data file, then come here;

%let datadir = C:\Users\spate41\Box\COVID19_data_shared;

data mergedsocial ;
		set "&datadir.\mergedsocial";

		label 
		age65over  = "% Over 65 y"
		male  = "% Male"     
		black  =  "% African American"  
		poverty  = "% in Poverty"
		PCTUI = "% Uninsured"
		diabetes="% Diabetes"
		obesity="% Obese"
		groupquater  = "% in Group Quarters" 

		RPL_THEME1 = "Socioeconomic Vulnerability"
		RPL_THEME2 = "Household Composition Vulnerability"
		RPL_THEME3 = "Minority/Language Vulnerability"
		RPL_THEME4 = "Housing/Transportaion Vulnerability"
		popden = "Population Density (Per Sq. Mile)"
		_013_Urbanization_Code="Urban-Rural Status"
		_013_Urbanization="Urban-Rural Status"

		cases="Total COVID-19 Cases"
		deaths="Total COVID-19 Deaths"
		mean7daycases="Daily Cases (7-day Average)"
		mean7daydeaths="Daily Deaths (7-day Average)"
		covidmortality="Total Deaths per 100,000"
		caserate="Total Cases per 100,000"
		covidmortality7day="Daily Deaths per 100,000 (7-day Average)"
		caserate7day="Daily Cases per 100,000 (7-day Average)"
		cfr="Case fatality ratio"

		hospitalTot="Total Hospitalizations" 
		hospitalRate="Hospitalizations per 100,000"
		tests ="Total Tests"
		testingRate="Tests per 100,000"

		female  =  "% Female population"       
		hispanic = "% Hispanic or Latino"
		minority  = "% Other than not Hispanic or Latino, White Alone"
		college  = "% Bachelor's Degree or Better, age 25 and over"
		hhincome  = "Average Household Income (In 2018 Inflation Adjusted Dollars)";
run;

proc format;
value ranks
0="Bottom Tertile"
1="Middle Tertile"
2="Top Tertile"
;

			proc rank data=mergedsocial
			groups=5 out=georgia;
			where state=13 and county ne .;
			var	 	black 		diabetes 		obesity 	hispanic 		RPL_THEME1		poverty;
			ranks 	blackQ 		diabetesQ 		obesityQ 	hispanicQ 		RPL_THEME1Q 	povertyQ;
			run;


%macro hbarplot(data=, exp=,outcome=,xlabel=,ylabel=);
title "&outcome. by &exp.";
proc sgplot data=&data. noautolegend aspect=0.7;
where county ne .;
	hbar &exp.  /response=&outcome. stat=mean fillattrs=(color="#007698") nooutline datalabel=&outcome.;
  label &exp.="&xlabel." &outcome.="&ylabel.";
  run;
%mend hbarplot;


%hbarplot(data=georgia, exp=urbanrural, outcome=covidmortality7day, xlabel=Metropolitan status, ylabel=COVID-19 7-day mortality);
%hbarplot(data=georgia, exp=urbanrural, outcome=caserate7day, xlabel=Metropolitan status, ylabel=COVID-19 7-day incidence);


%hbarplot(data=georgia, exp=diabetesQ, outcome=covidmortality7day, xlabel=&exp., ylabel=COVID-19 7-day mortality);
%hbarplot(data=georgia, exp=diabetesQ, outcome=caserate7day, xlabel=&exp., ylabel=COVID-19 7-day incidence);


%hbarplot(data=georgia, exp=povertyQ, outcome=covidmortality7day, xlabel=&exp., ylabel=COVID-19 7-day mortality);
%hbarplot(data=georgia, exp=povertyQ, outcome=caserate7day, xlabel=&exp., ylabel=COVID-19 7-day incidence);




%hbarplot(data=georgia, exp=hispanicQ, outcome=covidmortality7day, xlabel=&exp., ylabel=COVID-19 7-day mortality);
%hbarplot(data=georgia, exp=hispanicQ, outcome=caserate7day, xlabel=&exp., ylabel=COVID-19 7-day incidence);





*Totals, not daily;
%hbarplot(data=georgia, exp=urbanrural, outcome=covidmortality, xlabel=Metropolitan status, ylabel=COVID-19 total mortality);
%hbarplot(data=georgia, exp=urbanrural, outcome=caserate, xlabel=Metropolitan status, ylabel=COVID-19 total incidence);


%hbarplot(data=georgia, exp=blackQ, outcome=covidmortality, xlabel=&exp., ylabel=COVID-19 total mortality);
%hbarplot(data=georgia, exp=blackQ, outcome=caserate, xlabel=&exp., ylabel=COVID-19 total incidence);


%hbarplot(data=georgia, exp=povertyQ, outcome=covidmortality, xlabel=&exp., ylabel=COVID-19 total mortality);
%hbarplot(data=georgia, exp=povertyQ, outcome=caserate, xlabel=&exp., ylabel=COVID-19 total incidence);


%hbarplot(data=georgia, exp=RPL_THEME1Q, outcome=covidmortality, xlabel=&exp., ylabel=COVID-19 total mortality);
%hbarplot(data=georgia, exp=RPL_THEME1Q, outcome=caserate, xlabel=&exp., ylabel=COVID-19 total incidence);


