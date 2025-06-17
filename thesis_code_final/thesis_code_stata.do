******** Checkpoint 1, May 2025 ********************************************************************************

******** Data list ********************************************************************************

* Commodity index: 1996Q1:2025Q2 = commodity
* Equity index: 1986Q4:2025Q2 = 
* Gov debt to GDP: 2000Q1:2024Q4
* Forecast inflation HICP (PCE forecasts were scarce), 1 year ahead: 1999Q4:2025Q4
* HICP (for GMM as well): 1992Q1:2025Q2
* HLW, external output gap estimate: 1997Q1:2022Q3
* HP Filtered output gap estimate, my calculation (take first 4 years as burner): 1995Q1:2024Q4
* M2 growth Y-o-Y: 1981Q1:2025Q1 
* M3 growth Y-o-Y: 1981Q1:2025Q1
* PCE Y-o-Y delta: 1997Q1:2025Q2
* Shadow rate (Wu-Xia-2016) inserted EURIBOR: 1999Q1:2025Q1
* Bond spread for EEA (10year - 1 year): 2004Q3:2024Q4
* Constructed unemployment gap measures: 1999Q1:2025Q2

******** Variable list ********************************************************************************

** comm_ind_g : Commodity index growth Y-o-Y, quarterly. Euro area.
** stoxx_50_index_g: Euro top 50 stock by capitalization index growth Y-o-Y, quarterly.
** gov_debt: Government debt to GDP ratio, in percentages. Quarterly, Euro area.
** hicp_f_1y: HICP growth forecast, contemporaneous mean estimate. 1 year ahead, Euro area. 
** forecast_gap: hicp_f_1y - 2. ( 2 = inflation target of ECB). In percentages.
** hicp: CPI index (includes food and energy) of euro area, Y-o-Y change. 
** pi_gap: hicp - 2. (2 = inflation target of ECB). In percentages.
** output_gap_hlw: HLW estimates of Euro area output gap.
** m2_g: M2 growth Y-o-Y. Euro area, in percentages.
** m3_g: M3 growth Y-o-Y. Euro area, in percentages.
** pce: Core Inflation (HICPX: Excluding food and energy prices.) of euro area, Y-o-Y change.
** pce_gap: pce - 2. (2 = inflation target of ECB). In percentages.
** pr_1_shadow: Policy rate of the ECB, where between 2004Q1 and 2022Q3 Wu-Xia (2016) shadow rates were inserted to capture ZLB periods.
** eea_bond_spread: 10 year yield - 1 year yield of AAA rated government bonds, euro area. Potential instrument for post 2012.
** u_y0: Unemployment rate, Euro Area.
** u_f_y1: Forecasted unemployment rate, 1 year ahead. Euro area.
** u_f_y2: Forecasted unemployment rate, 2 years ahead. Euro area.
** meanu_gap: Observed  unemployment rate - NAIRU (Non-accelerating inflation rate of unemployment). NAIRU is an non-observable variable.
**** To compute NAIRU (structural estimation is beyond the scope of this thesis), an assumption that takes the mean value of unemployment rates of the whole time period as NAIRU to compute unemployment gap.
** forecastu_gap: u_f_y2 - u_y0. 2 year ahead forecast - observed unemployment. This variable can be used as a potential instrument for output gap.
********************************************************************************

gen tq = quarterly(time, "YQ")
format tq %tq
tsset time_index

** Carvalho (2021)'s instruments: 4 lags of core inflation, output gap, policy rate, m2 growth, bond spread, commodity price inf.
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
     instruments(L(1/4).pce L(1/4).pr_1_shadow ///
                 L(1/4).output_gap_hp L(1/4).m2_g ///
                 L(1/4).comm_ind_g L(1/4)meanu_gap) vce(hac nw 4)
				 
			 			 
* Trying 2/4 due to lag 1 could be problematic. This regression is also very good

** Idea: Take long run average of fisher equation as r*.

gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
     instruments(L(2/4).pce L(2/4).pr_1_shadow ///
                 L(2/4).output_gap_hp L(2/4).m2_g ///
                 L(2/4).comm_ind_g L(2/4)meanu_gap) vce(hac nw 4)

estat overid

** HLW Output gap could be biased, output gap coefficient is negative.

gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hlw), ///
     instruments(L(2/4).pce L(2/4).pr_1_shadow ///
                 L(2/4).output_gap_hlw L(2/4).m2_g ///
                 L(2/4).comm_ind_g L(2/4)meanu_gap) vce(hac nw 4)

estat overid

* New version 

gmm (pr_1_shadow - ///
     {smooth_p}*L.pr_1_shadow - ///
     (1 - {smooth_p})*({constant} + {p_react}*pi_gap + {y_react}*output_gap_hp)), ///
     instruments(L(1/4).pce L(1/4).pr_1_shadow ///
                 L(1/4).output_gap_hp L(1/4).m2_g ///
                 L(1/4).comm_ind_g L(1/4).meanu_gap) ///
     vce(hac nw 4)

	 
	 
***** STORAGE command *********************************************


* GMM1
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(1/4).pce L(1/4).pr_1_shadow L(1/4).output_gap_hp L(1/4).m2_g L(1/4).comm_ind_g L(1/4).meanu_gap) ///
    vce(hac nw 4)

estimates store GMM1

* GMM2
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)

estimates store GMM2

* GMM3
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hlw), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hlw L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
	
estimates store GMM3

esttab GMM1 GMM2 GMM3 using "GMM_regression_results.tex", ///
    replace se star(* 0.10 ** 0.05 *** 0.01) label title("GMM Regression Results")

****************************************
	 
* OLS

* OLS1 with HP output gap
newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
estimates store OLS1

* OLS2 with HP output gap, using 2–4 lags of instruments as in GMM2 (not directly used here but kept consistent)
newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
estimates store OLS2

* OLS3 with HLW output gap
newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hlw, lag(4)
estimates store OLS3


estpost summarize pr_1_shadow pi_gap output_gap_hp output_gap_hlw pce m2_g comm_ind_g meanu_gap
esttab using "summary_stats.tex", ///
    cells("mean sd min max") ///
    title("Summary Statistics") ///
    label replace
	
	

esttab OLS1 OLS2 OLS3 using "OLS_regression_results.tex", ///
    replace se star(* 0.10 ** 0.05 *** 0.01) label title("OLS Regression Results with Newey-West Errors")



******** Checkpoint 2, June 2025 ********************************************************************************

* Checking Taylor Method vs Actual Forecast Data

	* GMM taylor - lag inst 2
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
	
	* GMM taylor - lag inst 4
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(1/4).pce L(1/4).pr_1_shadow L(1/4).output_gap_hp L(1/4).m2_g L(1/4).comm_ind_g L(1/4).meanu_gap) ///
    vce(hac nw 4)
	
	* GMM SPF - lag inst 2 
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)

	* GMM SPF - lag inst 4
gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(1/4).pce L(1/4).pr_1_shadow L(1/4).output_gap_hp L(1/4).m2_g L(1/4).comm_ind_g L(1/4).meanu_gap) ///
    vce(hac nw 4)

	* OLS Taylor
	
newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp, lag(4)


	* OLS SPF
	
newey pr_1_shadow L.pr_1_shadow forecast_gap output_gap_hp, lag(4)


****************************************************************************************************
****************************************************************************************************
****************************************************************************************************
* Structural Break - not implemented in the thesis because of limited space.

* Checking for structural breaks = Before and after 2012 Euro crisis

gen period = .
replace period = 1 if time_index >= 73 & time_index < 125   // Before crisis
replace period = 2 if time_index >= 125 & time_index <= 178 // After crisis

* STRUCTURAL BREAK GMM * 


* BEFORE CRISIS - contemp pi - GMM - lag length instrument 3
preserve
keep if period == 1
eststo GMM1_before_contemp_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* BEFORE CRISIS - contemp pi - GMM - lag length instrument 2
preserve
keep if period == 1
eststo GMM2_before_contemp_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

* BEFORE CRISIS - forward, SPF - GMM, lag length instrument 3
preserve
keep if period == 1
eststo GMM3_before_spf_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* BEFORE CRISIS - forward, SPF - GMM, lag length instrument 2
preserve
keep if period == 1
eststo GMM4_before_spf_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

* BEFORE CRISIS - forward, Taylor 1993 - GMM - lag length instrument 3
preserve
keep if period == 1
eststo GMM5_before_taylor_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* BEFORE CRISIS - forward, Taylor 1993 - GMM - lag length instrument 2
preserve
keep if period == 1
eststo GMM6_before_taylor_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - contemp pi - GMM - lag length instrument 3
preserve
keep if period == 2
eststo GMM7_after_contemp_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - contemp pi - GMM - lag length instrument 2
preserve
keep if period == 2
eststo GMM8_after_contemp_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*pi_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - forward, SPF - GMM, lag length instrument 3
preserve
keep if period == 2
eststo GMM9_after_spf_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - forward, SPF - GMM, lag length instrument 2
preserve
keep if period == 2
eststo GMM10_after_spf_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*forecast_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - forward, Taylor 1993 - GMM - lag length instrument 3
preserve
keep if period == 2
eststo GMM11_after_taylor_3: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(2/4).pce L(2/4).pr_1_shadow L(2/4).output_gap_hp L(2/4).m2_g L(2/4).comm_ind_g L(2/4).meanu_gap) ///
    vce(hac nw 4)
restore

* AFTER CRISIS - forward, Taylor 1993 - GMM - lag length instrument 2
preserve
keep if period == 2
eststo GMM12_after_taylor_2: gmm (pr_1_shadow - {constant} - {smooth_p}*L.pr_1_shadow - {p_react}*rolling_hicp_gap - {y_react}*output_gap_hp), ///
    instruments(L(3/4).pce L(3/4).pr_1_shadow L(3/4).output_gap_hp L(3/4).m2_g L(3/4).comm_ind_g L(3/4).meanu_gap) ///
    vce(hac nw 4)
restore

	  
* final tab   
esttab GMM2_before_contemp_2 GMM4_before_spf_2 GMM6_before_taylor_2 ///
       GMM8_after_contemp_2 GMM10_after_spf_2 GMM12_after_taylor_2 ///
       using "Structural_Break_GMM_Lag2_Table.tex", replace se star(* 0.10 ** 0.05 *** 0.01) label ///
       title("Structural Break GMM (Lag Length = 2): Before and After Crisis") ///
       mtitles("Before-Con PI-2" "Before-SPF-2" "Before-Taylor-2" ///
               "After-Con PI-2" "After-SPF-2" "After-Taylor-2") ///
       keep(smooth_p:_cons p_react:_cons y_react:_cons constant:_cons) ///
       varlabels(smooth_p:_cons "Smoothing Parameter" p_react:_cons "Inflation Gap Reaction Coefficient" y_react:_cons "Output Gap Reaction Coefficient" constant:_cons "Constant")
* final tab  


* STRUCTURAL BREAK OLS * 
************************************************************************************************************

* BEFORE CRISIS - contemp - OLS
preserve 
keep if period == 1
eststo OLS_before_contemp: newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
restore

* BEFORE CRISIS - forward, SPF data - OLS
preserve
keep if period == 1
eststo OLS_before_SPF: newey pr_1_shadow L.pr_1_shadow forecast_gap output_gap_hp, lag(4)
restore

* BEFORE CRISIS - forward, Taylor's method - OLS
preserve 
keep if period == 1
eststo OLS_before_Taylor: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp, lag(4)
restore

* AFTER CRISIS - contemp - OLS
preserve 
keep if period == 2
eststo OLS_before_contemp: newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
restore


* AFTER CRISIS - forward, SPF data - OLS
preserve
keep if period == 2
eststo OLS_after_SPF: newey pr_1_shadow L.pr_1_shadow forecast_gap output_gap_hp, lag(4)
restore

* AFTER CRISIS - forward, Taylor's method - OLS
preserve 
keep if period == 2
eststo OLS_after_Taylor: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp, lag(4)
restore


esttab OLS_before_Taylor OLS_before_SPF OLS_after_Taylor OLS_after_SPF using "OLS_Crisis_Table.tex", ///
    replace se star(* 0.10 ** 0.05 *** 0.01) label ///
    title("OLS Regression Results: Before and After Crisis -- Forward Looking Taylor Rule") ///
    mtitles("Before Crisis - Taylor" "Before Crisis - SPF" "After Crisis - Taylor" "After Crisis - SPF") ///
    keep(L.pr_1_shadow rolling_hicp_gap forecast_gap output_gap_hp _cons)

****************************************************************************************************

* BEFORE CRISIS - contemp - OLS
preserve 
keep if period == 1
eststo OLS_before_contemp: newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
restore

* BEFORE CRISIS - forward, SPF data - OLS
preserve
keep if period == 1
eststo OLS_before_SPF: newey pr_1_shadow L.pr_1_shadow forecast_gap output_gap_hp, lag(4)
restore

* BEFORE CRISIS - forward, Taylor's method - OLS
preserve 
keep if period == 1
eststo OLS_before_Taylor: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp, lag(4)
restore

* AFTER CRISIS - contemp - OLS
preserve 
keep if period == 2
eststo OLS_after_contemp: newey pr_1_shadow L.pr_1_shadow pi_gap output_gap_hp, lag(4)
restore

* AFTER CRISIS - forward, SPF data - OLS
preserve
keep if period == 2
eststo OLS_after_SPF: newey pr_1_shadow L.pr_1_shadow forecast_gap output_gap_hp, lag(4)
restore

* AFTER CRISIS - forward, Taylor's method - OLS
preserve 
keep if period == 2
eststo OLS_after_Taylor: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp, lag(4)
restore

* Export Table
esttab OLS_before_contemp OLS_before_SPF OLS_before_Taylor ///
       OLS_after_contemp OLS_after_SPF OLS_after_Taylor ///
       using "OLS_Crisis_Table12.tex", replace se star(* 0.10 ** 0.05 *** 0.01) label ///
       title("OLS Regression Results: Before and After Crisis -- Forward Looking Taylor Rule") ///
       mtitles("Before - Contemp" "Before - SPF" "Before - Taylor" ///
               "After - Contemp" "After - SPF" "After - Taylor") ///
       keep(L.pr_1_shadow rolling_hicp_gap forecast_gap pi_gap output_gap_hp _cons)
	   
	   
****************************************************************************************************
****************************************************************************************************
****************************************************************************************************
* Structural Break - not implemented in the thesis because of limited space. I will present results during defense.

****************************************************************************************************
* President periods were also omitted in the thesis due to space controls. I will present results during defense.
* PRESIDENTS

eststo clear

* Duisenberg
eststo duisenberg: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp ///
    if inrange(time_index, 81, 101), lag(4)

* Trichet
eststo trichet: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp ///
    if inrange(time_index, 101, 133), lag(4)

* Draghi
eststo draghi: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp ///
    if inrange(time_index, 133, 165), lag(4)

* Lagarde
eststo lagarde: newey pr_1_shadow L.pr_1_shadow rolling_hicp_gap output_gap_hp ///
    if inrange(time_index, 165, 178), lag(4)
	
	
esttab using ecb_president.tex, replace ///
     se star(* 0.10 ** 0.05 *** 0.01) label title("Shadow Rate Regression by ECB President")
