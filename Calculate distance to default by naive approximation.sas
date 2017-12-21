options ls=70 nodate nocenter;

libname CRSP "Q:\Data-ReadOnly\CRSP\";

libname COMP "Q:\Data-ReadOnly\COMP\";

*there seems a difference between the year(datadate) and fyear;
/*
the value of F is from the last year
*/
data comp_funda_data;
    set COMP.funda (where=(indfmt="INDL" and datafmt="STD" and popsrc="D" and fic="USA" and consol="C" and
        fyear>=1970 and fyear<=2015) keep = cusip fyear dlc dltt indfmt datafmt popsrc fic consol);
	cusip = substr(cusip,1,8);
	dlc = dlc * 1000000;
	dltt = dltt * 1000000;
	F = dlc + 0.5 * dltt;
	year = fyear + 1;
	if F = . then delete;
	keep cusip year F;
run;

/*
the value of annret, sigmae and E is from the last year
*/
proc sql;
    create table stock_data as
	select cusip, year(date)+1 as year, exp(sum(log(1+ret)))-1 as annret,std(ret)*sqrt(250) as sigmae, avg(abs(prc)*shrout*1000) as E
	from CRSP.dsf(where=(1969<=year(date)<=2015))
	group by cusip, year;
quit;

data stock_data;
    set stock_data;
	if annret = . or sigmae = . or E = . then delete;
run;

*Combine the stock data with companies' fundamental data;
proc sql;
    create table funda_stock as
	select *
	from stock_data, comp_funda_data
	where stock_data.cusip = comp_funda_data.cusip and stock_data.year = comp_funda_data.year
	order by stock_data.cusip, stock_data.year;
quit;

/*Calculate DD and PD for each firm per year
and also use differernt naive sigma f;
Delete outliers of DD if it is greater than 30
*/
data funda_stock;
    set funda_stock;
	Naive_Sigma = E/(E+F)*sigmae + F/(E+F)*(0.05+0.25*sigmae);
    Naive_Sigma_2 = E/(E+F)*sigmae + F/(E+F)*(0.05+0.5*sigmae);;
    Naive_Sigma_3 = E/(E+F)*sigmae + F/(E+F)*(0.25*sigmae);;
	DD = (log((E+F)/F) + (annret - (Naive_Sigma**2) / 2)*1)/(Naive_Sigma * sqrt(1));
    DD_2 = (log((E+F)/F) + (annret - (Naive_Sigma_2**2) / 2)*1)/(Naive_Sigma_2 * sqrt(1));
    DD_3 = (log((E+F)/F) + (annret - (Naive_Sigma_3**2) / 2)*1)/(Naive_Sigma_3 * sqrt(1));

	PD = 1 - CDF('NORMAL',DD,0,1);
	PD_2 = 1 - CDF('NORMAL',DD_2,0,1);
	PD_3 = 1 - CDF('NORMAL',DD_3,0,1);

	if DD > 30 then delete;
	if DD_2 > 30 then delete;
	if DD_3 > 30 then delete;
run;

proc sort data = funda_stock;
by year;
run;

/*
Get the dataset of statistics of DD and PD for plotting
*/
proc means data = funda_stock n mean p25 p50 p75 std min max noprint;
    output out = stat_dd(drop = _type_ _freq_) mean = p25 = p50 = p75 = / autoname;
	var DD DD_2 DD_3 PD PD_2 PD_3;
	by year;
run;

*import data of usrec, baaffm and cfsi;
%let in_table = usrec baaffm cfsi;

%macro get_infile(name);
    %local k next;
    %do k = 1 %to %sysfunc(countw(&name));
      %let next = %scan(&name,&k);
        proc import datafile = "P:\assignment 5\5.1\&next..csv"
    	    out = &next dbms = csv;
    	run;
    	data &next;
    	    set &next;
    	    year = year(date);
    	    keep year &next;
    	run;
    %end;
%mend;

%get_infile(&in_table);
run;

/*merge data of usrec into the main dataset.*/
data funda_stock;
    merge funda_stock(in=a) usrec(in=b);
    by year;
    if a;
run;

/*merge data of usrec, baaffm and cfsi into statistical datasets for ploting,
  vlist is the names of reference datasets that will be merged into the statistical datasets*/
%macro get_merge(vlist);
    %local j next_var;
    	%do j = 1 %to %sysfunc(countw(&vlist));
            %let next_var = %scan(&vlist,&j);
    	    data stat_dd;
    	        merge stat_dd(in = a) &next_var(in = b);
    	        by year;
    	        if a;
    	    run;
    	%end;

%mend;

%get_merge(vlist = (usrec baaffm cfsi));
run;

*output;
ods pdf file = "P:\assignment 5\5.1\Report for Assignment-5.1-Yu Lin.pdf";
ods text="DD uses Naive_Sigma_D = 0.05 + 0.25*Sigma_E";
ods text="DD_2 uses Naive_Sigma_D = 0.05 + 0.5*Sigma_E";
ods text="DD_3 uses Naive_Sigma_D = 0.25*Sigma_E";

proc means data = funda_stock n mean p25 p50 p75 std min max ;
	var DD DD_2 DD_3 PD PD_2 PD_3;
	by year;
	title "Descriptive Statistics of DD and PD by year";
run;

proc corr data = funda_stock pearson;
    var DD PD;
	title 'Correlations between DD and PD';
run;
proc corr data = funda_stock pearson;
    var DD_2 PD_2;
	title 'Correlations between DD_2 and PD_2';
run;
proc corr data = funda_stock pearson;
    var DD_3 PD_3;
	title 'Correlations between DD_3 and PD_3';
run;

*Plot each variable along with time;
%macro get_mean_plot(vlist);
    %local k next next_2 next_3;
    %do k = 1 %to (%sysfunc(countw(&vlist)))/3;
        %let next = %scan(&vlist,3*(&k-1)+1);
		%let next_2 = %scan(&vlist,3*(&k-1)+2);
		%let next_3 = %scan(&vlist,3*(&k));
        proc sgplot data = stat_dd nocycleattrs;;
            series x = year y = &next / lineattrs=(color=blue);
            series x = year y = &next_2 / lineattrs=(color=green);
            series x = year y = &next_3 / lineattrs=(color=orange);
            title "&next. over time";
        run;
    %end;
%mend;

%get_mean_plot(DD_Mean DD_2_Mean DD_3_Mean DD_P25 DD_2_P25 DD_3_P25 DD_P50 DD_2_P50 DD_3_P50 DD_P75 DD_2_P75 DD_3_P75);

proc means data = funda_stock(where=(USREC = 1)) n mean p25 p50 p75 std min max;
    var DD DD_2 DD_3 PD PD_2 PD_3;
	title 'Descriptive statistics of DD and PD in recession years';
run;

proc means data = funda_stock(where=(USREC = 0)) n mean p25 p50 p75 std min max;
    var DD DD_2 DD_3 PD PD_2 PD_3;
	title 'Descriptive statistics of DD and PD in non-recession years';
run;

/*use macro to plot DD and PD along with usrec, baaffm and cfsi,
  vlist1 is used to indicate the name of reference argument (usrec baaffm and cfsi)*/
%macro get_plot_2(vlist1);
    %local k next;
    %do k = 1 %to %sysfunc(countw(&vlist1));
        %let next = %scan(&vlist1,&k);
        proc sgplot data = stat_dd nocycleattrs;
    	    series x = year y = DD_Mean / lineattrs=(color=blue);
			series x = year y = DD_2_Mean / lineattrs=(color=green);
			series x = year y = DD_3_Mean / lineattrs=(color=orange);
            series x = year y = &next / y2axis lineattrs=(color=red);
            title "The average of DD along with &next. over years";
        run;

		    proc sgplot data = stat_dd nocycleattrs;
    	    series x = year y = PD_Mean / lineattrs=(color=blue);
			series x = year y = PD_2_Mean / lineattrs=(color=green);
			series x = year y = PD_3_Mean / lineattrs=(color=orange);
            series x = year y = &next / y2axis lineattrs=(color=red);
            title "The average of PD along with &next. over years";
        run;
    %end;
%mend;

%get_plot_2(vlist1 = USREC BAAFFM CFSI);
run;

ods pdf close;
/*
it seems that we do not need to use risk-free rate for method 1
*/
/*




