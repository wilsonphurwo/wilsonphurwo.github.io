LIBNAME proj "/home/u42905717/sasuser.v94/Data Project";
option fmtsearch=(work proj);

proc format library=proj;
	value WTfmt 1='Yes'
				other='No';
run;
				   
data proj.gspc;
	infile '/home/u42905717/sasuser.v94/Data Project/^GSPC.csv' firstobs=2 dlm=',';
	input DATE yymmdd10. OPEN HIGH LOW CLOSE ADJ_CLOSE VOLUME;
	format DATE mmddyy10.;
run;
	

PROC CONTENTS DATA=proj.GSPC; RUN;

proc means data=proj.GSPC;
	var _all_;
run;

FILENAME weather '/home/u42905717/sasuser.v94/Data Project/1946457.csv';

data proj.weather (drop=TAVG TSUN FMTM PGTM);
	length NAME $ 30 STATION $12;
	infile '/home/u42905717/sasuser.v94/Data Project/1946457.csv' firstobs=2 dsd missover dlm=',';
	input STATION $ NAME $ DATE :yymmdd12. AWND :comma.8 FMTM PGTM PRCP SNOW SNWD TAVG TMAX
		  TMIN TSUN WDF2 WDF5 WSF2 WSF5 WT01 WT02 WT03 WT04 WT05 WT06 WT07 WT08 WT09 
		  WT11 WT13 WT14 WT16 WT17 WT18 WT19 WT22;
	format DATE mmddyy10. WT01 WT02 WT03 WT04 WT05 WT06 WT07 WT08 WT09 
		  WT11 WT13 WT14 WT16 WT17 WT18 WT19 WT22 $WTfmt.;
	label STATION = "Station Identification Code"
		  AWND = "Average daily wind speed"
		  FMTM = "Time of fastest mile or fastest 1-minute wind"
		  PGTM = "Peak gust time"
		  PRCP = "Precipitation"
		  SNOW = "Snowfall"
		  SNWD = "Snow depth"
		  TMAX = "Maximum temperature"
		  TMIN = "Minimum temperature"
		  TSUN = "Daily total sunshine"
		  WDF2 = "Direction of fastest 2-minute wind"
		  WDF5 = "Direction of fastest 5-second wind"
		  WSF2 = "Fastest 2-minute wind speed"
		  WSF5 = "Fastest 5-second wind speed"
		  WT01 = "Fog, ice fog, or freezing fog (may include heavy fog)"
		  WT02 = "Heavy fog or heaving freezing fog (not always distinguished from fog)"
		  WT03 = "Thunder"
		  WT04 = "Ice pellets, sleet, snow pellets, or small hail"
		  WT05 = "Hail (may include small hail)"
		  WT06 = "Glaze or rime"
		  WT07 = "Dust, volcanic ash, blowing dust, blowing sand, or blowing obstruction"
		  WT08 = "Smoke or haze"
		  WT09 = "Blowing or drifting snow"
		  WT10 = "Tornado, waterspout, or funnel cloud"
		  WT11 = "High or damaging winds"
		  WT12 = "Blowing spray"
		  WT13 = "Mist"
		  WT14 = "Drizzle"
		  WT15 = "Freezing drizzle"
		  WT16 = "Rain (may include freezing rain, drizzle, and freezing drizzle)"
		  WT17 = "Freezing rain"
		  WT18 = "Snow, snow pellets, snow grains, or ice crystals"
		  WT19 = "Unknown source of precipitation"
		  WT22 = "Ice fog or freezing fog";
	
	if AWND = . then delete;
	if WDF5 = . then delete;
	if WT01 = . then WT01 = 0;
	if WT02 = . then WT02 = 0;
	if WT03 = . then WT03 = 0;
	if WT04 = . then WT04 = 0;
	if WT05 = . then WT05 = 0;
	if WT06 = . then WT06 = 0;
	if WT07 = . then WT07 = 0;
	if WT08 = . then WT08 = 0;
	if WT09 = . then WT09 = 0;
	if WT10 = . then WT10 = 0;
	if WT11 = . then WT11 = 0;
	if WT12 = . then WT12 = 0;
	if WT13 = . then WT13 = 0;
	if WT14 = . then WT14 = 0;
	if WT15 = . then WT15 = 0;
	if WT16 = . then WT16 = 0;
	if WT17 = . then WT17 = 0;
	if WT18 = . then WT18 = 0;
	if WT19 = . then WT19 = 0;
	if WT20 = . then WT20 = 0;
	if WT21 = . then WT21 = 0;
	if WT22 = . then WT22 = 0;
	
run;

proc means data=proj.weather; run;

proc freq data=proj.weather (drop=DATE);
	tables _all_;
run;

PROC CONTENTS DATA=proj.weather; RUN;

proc sql;
	create table proj.combined as
	select GSPC.*, weather.*
	from proj.GSPC, proj.weather
	where GSPC.DATE=weather.DATE;
quit;

proc sql;
	create table proj.combined_delta as
	select GSPC.ADJ_CLOSE - GSPC.OPEN as CHANGE, weather.*
	from proj.GSPC, proj.weather (drop=NAME STATION)
	where GSPC.DATE=weather.DATE;
quit;

PROC CONTENTS DATA=proj.combined; RUN;

proc means data=proj.combined;
run;

proc glmselect data=proj.combined plots=all;
	model ADJ_CLOSE = AWND PRCP SNOW SNWD TMAX TMIN WT01-WT22 / selection=forward details=all;
run;

proc reg data=proj.combined;
	model ADJ_CLOSE = AWND PRCP SNOW SNWD TMAX TMIN WT01-WT22 / selection=adjrsq rmse sbc adjrsq;
run;
quit;

proc reg data=proj.combined;
	model ADJ_CLOSE = AWND SNOW SNWD TMAX TMIN WT01 WT03 WT05 WT07 WT13 WT16 WT18 WT19 WT22 / details=all;
run;
quit;

proc corr data=proj.combined nosimple;
	var AWND PRCP SNOW SNWD TMAX TMIN;
	with ADJ_CLOSE;
run;

proc corr data=proj.combined nosimple;
	var WT01-WT22;
	with ADJ_CLOSE;
run;