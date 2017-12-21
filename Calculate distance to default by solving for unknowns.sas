proc printto log = "logfile";
run;

options ls=70 nodate nocenter;

libname CRSP "Q:\Data-ReadOnly\CRSP\";

libname COMP "Q:\Data-ReadOnly\COMP\";

*there seems a difference between the year(datadate) and fyear;
/*
the value of F is from the last year
*/
data comp_funda_data;
    set COMP.funda (where=(indfmt="INDL" and datafmt="STD" and popsrc="D"
        and fic="USA" and consol="C" and fyear>=1970 and fyear<=2015)
        keep = cusip fyear dlc dltt indfmt datafmt popsrc fic consol);
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
	select cusip, year(date)+1 as year, exp(sum(log(1+ret)))-1 as annret,
        std(ret)*sqrt(250) as sigmae, avg(abs(prc)*shrout*1000) as E
	from CRSP.dsf(where=(1969<=year(date)<=2015))
	group by cusip, year;
quit;

data stock_data;
    set stock_data;
	if annret = . or sigmae = . or E = . then delete;
run;

*import the data containing risk free rate;
proc import datafile = "P:\assignment 5\5.2\dailyfed.csv" out = dailyfed dbms = csv replace;
run;

data dailyfed;
    set dailyfed;
	year = year(DATE);
	r = log(1+DTB3/100);
	if r = . then delete;
	keep year r;
run;

proc sort nodupkey data = dailyfed;
    by year;
run;

*Combine the stock data with companies' fundamental data and risk free rate;
proc sql;
    create table funda_stock as
	select *
	from stock_data, comp_funda_data, dailyfed
	where stock_data.cusip = comp_funda_data.cusip and
        stock_data.year = comp_funda_data.year and stock_data.year = dailyfed.year
	order by stock_data.cusip, stock_data.year;
quit;

/*Calculate naive DD and naive PD for each firm per year
Delete outliers of DD if it is greater than 30
*/
data funda_stock;
    set funda_stock;
	Naive_Sigma = E/(E+F)*sigmae + F/(E+F)*(0.05+0.25*sigmae);

	DD_naive = (log((E+F)/F) + (annret - (Naive_Sigma**2) / 2)*1)/(Naive_Sigma * sqrt(1));

	PD_naive = 1 - CDF('NORMAL',DD_naive,0,1);

	if DD_naive > 30 then delete;
run;

*set the initial value of V and sigma_v;
data var_tmp_method;
    set funda_stock;
	ini_V = E+F;
	ini_sigmav = (E/(E+F))*sigmae;
run;

*solve for V and sigma_v;
proc model data = var_tmp_method;
    endogenous ini_V ini_sigmav;
	exogenous r F E sigmae;
	E = ini_V*PROBNORM(log(ini_V/F)+(r+(ini_sigmav*ini_sigmav)/2)/(ini_sigmav))
        - exp(-r)*F*PROBNORM(log(ini_V/F)+(r-(ini_sigmav*ini_sigmav)/2)/(ini_sigmav));
	sigmae = (ini_V/E)*PROBNORM(log(ini_V/F)+(r+(ini_sigmav*ini_sigmav)/2)/(ini_sigmav))*ini_sigmav;
	solve ini_V ini_sigmav / out=solve_result(rename=(ini_V=V ini_sigmav=sigmav)
        drop=_type_ _mode_ _errors_) seidel maxiter = 32000 maxsubit = 2000 converge = 1e-4 noprint;
	by cusip year;
	keep cusip year ini_V ini_sigmav;
run;

*merge the results of v and sigma_v with the main dataset;
data funda_stock;
    merge funda_stock(in = a) solve_result(in = b);
    by cusip year;
    if a and b;
run;

*calculate new DD and PD;
data funda_stock;
    set funda_stock;
    DD = (log(V/F)+(annret - sigmav*sigmav/2)*1)/(sigmav*sqrt(1));
	PD = CDF('NORMAL', -DD, 0 ,1);
run;

proc sort data = funda_stock;
by year;
run;

/*
Get the dataset of statistics of DD and PD for plotting
*/
proc means data = funda_stock n mean p25 p50 p75 std min max noprint;
    output out = stat_dd(drop = _type_ _freq_) mean = p25 = p50 = p75 = / autoname;
	var DD PD;
	by year;
run;

*import data of usrec, baaffm and cfsi;
%let in_table = usrec baaffm cfsi;

%macro get_infile(name);
    %local k next;
    %do k = 1 %to %sysfunc(countw(&name));
      %let next = %scan(&name,&k);
        proc import datafile = "P:\assignment 5\5.2\&next..csv"
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
ods pdf file = "P:\assignment 5\5.2\Report for Assignment5-2.pdf";
ods text="DD_naive uses Naive_Sigma_D = 0.05 + 0.25*Sigma_E";

proc means data = funda_stock n mean p25 p50 p75 std min max ;
	var DD PD;
	by year;
	title "Descriptive Statistics of DD and PD by year";
run;

*calculate the correlation matrix between DD and naive DD;
proc corr data = funda_stock pearson;
    var DD DD_naive;
	title 'Correlations between DD and DD_naive';
run;

proc corr data = funda_stock pearson;
    var PD PD_naive;
	title 'Correlations between PD and PD_naive';
run;

*Plot each variable along with time;
%macro get_mean_plot(vlist);
    %local k next;
    %do k = 1 %to %sysfunc(countw(&vlist));
        %let next = %scan(&vlist,k);
        proc sgplot data = stat_dd nocycleattrs;;
            series x = year y = &next / lineattrs=(color=blue);
            title "&next. over time";
        run;
    %end;
%mend;

%get_mean_plot(DD_Mean DD_P25 DD_P50 DD_P75);

proc means data = funda_stock(where=(USREC = 1)) n mean p25 p50 p75 std min max;
    var DD PD;
	title 'Descriptive statistics of DD and PD in recession years';
run;

proc means data = funda_stock(where=(USREC = 0)) n mean p25 p50 p75 std min max;
    var DD PD;
	title 'Descriptive statistics of DD and PD in non-recession years';
run;

/*use macro to plot financial variables along with usrec, baaffm and cfsi,
  vlist1 is used to indicate the name of reference argument (usrec baaffm and cfsi)*/
%macro get_plot_2(vlist1);
    %local k next;
    %do k = 1 %to %sysfunc(countw(&vlist1));
        %let next = %scan(&vlist1,&k);
        proc sgplot data = stat_dd nocycleattrs;
    	    series x = year y = DD_Mean / lineattrs=(color=blue);
            series x = year y = &next / y2axis lineattrs=(color=red);
            title "The average of DD along with &next. over years";
        run;

		    proc sgplot data = stat_dd nocycleattrs;
    	    series x = year y = PD_Mean / lineattrs=(color=blue);
            series x = year y = &next / y2axis lineattrs=(color=red);
            title "The average of PD along with &next. over years";
        run;
    %end;
%mend;

%get_plot_2(vlist1 = USREC BAAFFM CFSI);
run;

ods pdf close;




