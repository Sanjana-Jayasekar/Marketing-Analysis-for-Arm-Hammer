/*Project A&H - Liquid Detergent Category*/

data groc;
infile "H:\laundet_groc_1114_1165" firstobs=2;
input IRI_KEY WEEK SY GE VEND ITEM UNITS DOLLARS F $ D PR;
run;

data z1; set groc;
SY_1 = PUT(SY,Z2.);run; /*Adding a 0 in SY*/

data z2; set z1;
GE_1 = PUT(GE,Z2.);run; /*Adding a 0 in GE*/

data z3; set z2;
VEND_1 = PUT(VEND,Z5.);run; /*Adding 0s till 5 digits*/

data z4; set z3;
ITEM_1 = PUT(ITEM,Z5.);run; /*Adding 0s till 5 digits*/

proc print data =z4(obs=10); run;

data groc1; set z4;
UPC =catx('-',SY_1,GE_1,VEND_1,ITEM_1); /*Concatenate SY, GE, VEND and ITEM to get UPC*/
RUN;

proc print data =groc1(obs=10); run; 

proc print data =laundry(obs=10); run; /*Import file prod_laundet.csv*/

proc sql;
create table merged as 
select * 
from laundry 
inner join groc1 
on laundry.UPC = groc1.UPC;
quit;
proc print data=merged(obs=10); run;

proc sql;
create table count1 as 
select count(distinct UPC)as count 
from merged;
quit;
proc print data=count1; run;

/*Top 6 Brands in Liquid Category in terms of dollar sales*/
proc sql;
create table brand_by_dollars as
select L5, sum(DOLLARS) as total_dollars
from merged where FORM='LIQUID'
group by L5;
Quit;

proc sort data=brand_by_dollars;
by descending total_dollars ;
run;

proc print data=brand_by_dollars(obs=6); run;

/*Top Companies owning Liquid Detergent Brands*/
proc sql;
create table companies as
select L4, sum(DOLLARS) as sales
from merged where FORM='LIQUID'
group by L4;
Quit;

proc sort data=companies;
by descending sales ;
run;
proc print data=companies;run;


/*Top Markets selling Liquid Detergents in terms of Dollar Sales*/
data delivery_stores;
infile "H:\Delivery_Stores" firstobs=2;
input IRI_KEY OU $ EST_ACV Market_Name $ 21-45 Open Clsd MskdName $ ;
run;

proc print data=delivery_stores(obs=10); run;

proc sql;
create table Stores as
select Market_Name, sum(DOLLARS) as Sales 
from merged
inner join delivery_stores on delivery_stores.IRI_KEY = merged.IRI_KEY
where merged.FORM='LIQUID'
group by Market_Name;
Quit;

proc sort data=Stores;
by descending Sales ;
run;

proc print data=Stores;run;

/*Importing only A&H Liquid Detergent data*/

proc import datafile = "H:\data.csv"
out = data1
dbms = csv;
run;

proc print data=data1 (obs=10); run;

/*To check whether PR=1 and PR=0 are significantly dirrerent or same?*/
data pr; set data1;
proc ttest data=pr; var DOLLARS; 
class PR; run;

/*Time series */
Proc sql;
create table times as 
select WEEK, sum(DOLLARS) as DOLLARS  
from merged_b 
group by WEEK;
quit;

proc print data=times; run;

title 'Autocorrelated Time Series';
proc sgplot data=times noautolegend;
   series x=WEEK y=DOLLARS / markers;
   reg x=WEEK y=DOLLARS/  lineattrs=(color=black);
run;


ods graphics on;
proc autoreg data=times;
   model DOLLARS = WEEK/dwprob;
run;
/* no auto correlation found. So its only the trend component that is making the series non stationary */

/* Augmented Dickey-Fuller Unit Root Tests */
proc arima data=times;
   identify var=DOLLARS stationarity=(adf=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23));
run;
proc arima data=times;
identify var=DOLLARS(1) nlag=8;
run;
estimate p=(1) noconstant method=uls;
run;
estimate q=(1)(2) noconstant method=uls;
run;
outlier;
run;
forecast lead=5 interval=week id=WEEK out=results2;
run;
quit;





