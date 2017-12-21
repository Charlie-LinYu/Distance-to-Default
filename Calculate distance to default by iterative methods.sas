proc printto log = "P:\assignment 5\5.3\logfile";
run;

options ls=70 nodate nocenter;

libname CRSP "Q:\Data-ReadOnly\CRSP\";

libname COMP "Q:\Data-ReadOnly\COMP\";

*there seems a difference between the year(datadate) and fyear;
/*
the value of F is from the last year
*/
data comp_funda_data;
    set COMP.funda (obs = 1000000 where=(indfmt="INDL" and datafmt="STD" and popsrc="D"
        and fic="USA" and consol="C" and fyear>=1970 and fyear<=2015)
        keep = cusip fyear dlc dltt indfmt datafmt popsrc fic consol);
	cusip = substr(cusip,1,8);
	dlc = dlc * 1000000;
	dltt = dltt * 1000000;
	F = dlc + 0.5 * dltt;
	year = fyear + 1;
	real_year = fyear;
	if F = . then delete;
	keep cusip year F real_year;
run;
/*no lag*/
data stock_daily;
    set CRSP.dsf(obs = 1000000 where=(1969<=year(date)<=2015) keep = cusip date prc shrout);
	E = abs(prc) * shrout * 1000;
	real_year = year(date);
	if E = . then delete;
	keep cusip date real_year E;
run;


/*
the value of annret, sigmae and E is from the last year
*/
proc sql;
    create table stock_data as
	select cusip, year(date)+1 as year, year(date) as real_year, exp(sum(log(1+ret)))-1 as annret,
        std(ret)*sqrt(250) as sigmae, avg(abs(prc)*shrout*1000) as E_annual_ave
	from CRSP.dsf(obs = 1000000 where=(1969<=year(date)<=2015))
	group by cusip, year, real_year;
quit;

data stock_data;
    set stock_data;
	if annret = . or sigmae = . or E_annual_ave = . then delete;
run;

*import the data containing risk free rate;
proc import datafile = "P:\assignment 5\5.3\dailyfed.csv" out = dailyfed_orig dbms = csv replace;
run;


*Merge daily stock data, fundamental data and risk free rate for iterations;
proc sql;
    create table stock_daily_kmv as
	select *
	from stock_daily, comp_funda_data, dailyfed_orig
	where stock_daily.real_year = comp_funda_data.real_year
        and stock_daily.cusip = comp_funda_data.cusip
	    and stock_daily.date = dailyfed_orig.date
	order by stock_daily.cusip, stock_daily.date;
quit;

*calculate the yearly risk free rate;
data stock_daily_kmv;
    set stock_daily_kmv;
	r = log(1+DTB3/100);
	if r = . then delete;
	if F = 0 then delete;
	drop DTB3;
run;

*set initial values for iterations;
data stock_daily_kmv(rename=(sigmae=pre_sigmav));
    merge stock_daily_kmv(in=a) stock_data(in=b);
    by cusip real_year;
	if a;
    tmp_sigmav=.;
	V_itr = E+F;
	converged = 0;
	keep cusip date year real_year E F r sigmae tmp_sigmav V_itr converged;
run;

%macro iteration;
%do j = 1 %to 20;
***************;
*calculate V;
proc model data = stock_daily_kmv noprint;
    dependent V_itr;
	independent r F E pre_sigmav;
	E = V_itr*PROBNORM(log(V_itr/F)+(r+(pre_sigmav*pre_sigmav)/2)/(pre_sigmav))
        - exp(-r)*F*PROBNORM(log(V_itr/F)+(r-(pre_sigmav*pre_sigmav)/2)/(pre_sigmav));
	solve V_itr / out=solve_v(drop=_type_ _mode_ _errors_)
        seidel maxiter = 32000 maxsubit = 2000 converge = 0.0001 noprint;
	by cusip date;
	keep cusip date V_itr;
run;

*calculate the daily return of V to further calcualte the sigam V;
data solve_v;
    set solve_v;
	by cusip date V_itr notsorted;
	ret = V_itr/lag(V_itr)-1;
    if first.cusip then ret=.;
run;

*calculate sigma V;
proc sql;
    create table tmp_sigmav as
	select cusip, year(date) as real_year, std(ret)*sqrt(250) as tmp_sigmav
	from solve_v
	group by cusip, real_year;
quit;

data stock_daily_kmv;
    set stock_daily_kmv(drop = tmp_sigmav);
run;

*merge new sigma V into the main dataset for the next iteration;
proc sql;
    create table stock_daily_kmv as
	select *
    from stock_daily_kmv, tmp_sigmav
	where stock_daily_kmv.cusip = tmp_sigmav.cusip
        and stock_daily_kmv.real_year = tmp_sigmav.real_year
    order by stock_daily_kmv.cusip, stock_daily_kmv.date;
quit;

*check whether the sigma V has been converged;
data stock_daily_kmv;
    set stock_daily_kmv;
	if abs(tmp_sigmav - pre_sigmav)<0.001 then converged = 1;
	pre_sigmav = tmp_sigmav;
run;
*********************;

%end;
%mend;

%iteration;

*output the final result of sigma_V and V, and also lag the data;
proc sql;
    create table result_itr as
	select cusip, year(date)+1 as year,
        std(ret)*sqrt(250) as sigmav_itr, avg(V_itr) as V_itr
	from solve_v
	group by cusip, year;
quit;

*get the risk free rate for the naive method and the direct method;
data dailyfed;
    set dailyfed_orig;
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
	Naive_Sigma = E_annual_ave/(E_annual_ave+F)*sigmae + F/(E_annual_ave+F)*(0.05+0.25*sigmae);

	DD_naive = (log((E_annual_ave+F)/F) + (annret - (Naive_Sigma**2) / 2)*1)/(Naive_Sigma * sqrt(1));

	PD_naive = 1 - CDF('NORMAL',DD_naive,0,1);

	if DD_naive > 30 then delete;
run;

*set the initial value of V and sigma_v;
data var_tmp_method;
    set funda_stock;
	ini_V = E_annual_ave+F;
	ini_sigmav = (E_annual_ave/(E_annual_ave+F))*sigmae;
run;

*solve for V and sigma_v using the direct method;
proc model data = var_tmp_method noprint;
    endogenous ini_V ini_sigmav;
	exogenous r F E_annual_ave sigmae;
	E_annual_ave = ini_V*PROBNORM(log(ini_V/F)+(r+(ini_sigmav*ini_sigmav)/2)/(ini_sigmav))
        - exp(-r)*F*PROBNORM(log(ini_V/F)+(r-(ini_sigmav*ini_sigmav)/2)/(ini_sigmav));
	sigmae = (ini_V/E_annual_ave)*PROBNORM(log(ini_V/F)
        + (r+(ini_sigmav*ini_sigmav)/2)/(ini_sigmav))*ini_sigmav;
	solve ini_V ini_sigmav / out=result_direct(rename=(ini_V=V_direct ini_sigmav=sigmav_direct)
        drop=_type_ _mode_ _errors_) seidel maxiter = 32000 maxsubit = 2000 converge = 1e-4 noprint;
	by cusip year;
	keep cusip year ini_V ini_sigmav;
run;

*merge the results of the iterative method and the direct method with the main dataset;
data funda_stock;
    merge funda_stock(in = a) result_direct(in = b) result_itr(in=c);
    by cusip year;
    if a and b and c;
run;

*calculate new DD and PD of the iterative method and the direct method;
data funda_stock;
    set funda_stock;
    DD_direct = (log(V_direct/F)+(annret - sigmav_direct*sigmav_direct/2)*1)/(sigmav_direct*sqrt(1));
	PD_direct = CDF('NORMAL', -DD_direct, 0 ,1);
    DD_itr = (log(V_itr/F)+(annret - sigmav_itr*sigmav_itr/2)*1)/(sigmav_itr*sqrt(1));
	PD_itr = CDF('NORMAL', -DD_itr, 0 ,1);
run;

proc sort data = funda_stock;
by year;
run;

*output;
ods pdf file = "P:\assignment 5\5.3\Report for Assignment5-3-YuLin.pdf";
/*
Get the dataset of statistics of DD and PD for plotting
*/
proc means data = funda_stock n mean p25 p50 p75 std min max;
    output out = stat_dd(drop = _type_ _freq_) mean = p25 = p50 = p75 = / autoname;
	var DD_itr PD_itr DD_direct PD_direct DD_naive PD_naive;
	by year;
	title "Descriptive Statistics of DD and PD by year";
run;

*calculate the correlation matrix between DD and naive DD;
proc corr data = funda_stock pearson;
    var DD_itr DD_direct DD_naive;
	title 'Correlations between DD_itr, DD_direct and DD_naive';
run;

proc corr data = funda_stock pearson;
    var PD_itr PD_direct PD_naive;
	title 'Correlations between PD_itr, PD_direct and PD_naive';
run;

*Plot each variable along with time;
%macro get_plot(vlist);
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

%get_plot(DD_itr_Mean DD_direct_Mean DD_naive_Mean
    DD_itr_P25 DD_direct_P25 DD_naive_P25
    DD_itr_P50 DD_direct_P50 DD_naive_P50
    DD_itr_P75 DD_direct_P75 DD_naive_P75);

ods pdf close;
