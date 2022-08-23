* Diverging Bars Correlation plot with Confidence Interval

	* Before we start off, lets install the following packages (One time install)
	ssc install schemepack, replace //Scheme Pack Created by Asjad Naqvi
	ssc install colrspace, replace
	ssc install palettes, replace 
	ssc install labutil, replace //Applying labels to numbers (Nick Cox)
	net install pr0041_4.pkg, replace //Correlation Coef with CI (Nick Cox - corrci)

	* Load Dataset
	sysuse auto, clear	
			
	* Only change names of variable in local var_corr. 
	* The code will hopefully do the rest of the work without any hitch
	local var_corr price mpg trunk weight length turn foreign
	local countn : word count `var_corr'
	
	* Use correlation command
	* https://journals.sagepub.com/doi/pdf/10.1177/1536867X0800800307
	
	quietly corrci `var_corr'
	matrix C = r(corr)
	local rnames : rownames C
	matrix LB = r(lb)
	matrix UB = r(ub)
	matrix Z = r(z) //matrix of z = atanh r
	
	egen miss = rowmiss(`var_corr')
	count if miss == 0
	local N = r(N)
	
	* Now to generate a dataset from the Correlation Matrix
	clear
		
	* This will not have the diagonal of matrix (correlation of 1) 
	local tot_rows : display `countn' * `countn'
	set obs `tot_rows'
	
	generate corrname1 = ""
	generate corrname2 = ""
	generate byte y = .
	generate byte x = .
	generate double corr = .
	generate double lb = .
	generate double ub = .
	generate double z = .
	generate double abs_corr = .
	
	local row = 1
	local y = 1
	local rowname = 2
		
	foreach name of local var_corr {
		forvalues i = `rowname'/`countn' { 
			local a : word `i' of `var_corr'
			replace corrname1 = "`name'" in `row'
			replace corrname2 = "`a'" in `row'
			replace y = `y' in `row'
			replace x = `i' in `row'
			replace corr = C[`i',`y'] in `row'
			replace lb = LB[`i',`y'] in `row'
			replace ub = UB[`i',`y'] in `row'
			replace z = Z[`i',`y'] in `row'
			replace abs_corr = abs(C[`i',`y']) in `row'
			
			local ++row
			
		}
		
		local rowname = `rowname' + 1
		local y = `y' + 1
	
	}
		
	drop if missing(corrname1)
	
	* Generating total non missing count and P-Values
	generate N = `N'
	generate double p = min(2 * ttail(N - 2, abs_corr * sqrt(N - 2) / sqrt(1 - abs_corr^2)), 1)
	
	* Generate stars
	generate stars = "*" if p <= 0.1 & p > 0.05
	replace stars = "**" if p <= 0.05 & p > 0.01
	replace stars = "***" if p <= 0.01
	
	* Generating a variable that will contain color codes
	* colorpalette HCL pinkgreen, n(20) nograph intensity(0.75) //Not Color Blind Friendly
	colorpalette CET CBD1, n(20) nograph //Color Blind Friendly option
	generate colorname = ""
	local col = 1
	forvalues colrange = -1(0.1)0.9 {
		replace colorname = "`r(p`col')'" if corr >= `colrange' & corr < `=`colrange' + 0.1'
		replace colorname = "`r(p20)'" if corr == 1
		local ++col
	}	
	
	* Grouped correlation of variables
	generate group_corr = corrname1 + " - " + corrname2
	compress
	
	
	* Sort the plot
	sort corr, stable
	generate rank_corr = _n
	labmask rank_corr, values(group_corr)
	
********************************************************************************	
	
	* Plotting
	* Run the commands ahead in one go if you have reached this point in breaks
	* Saving the plotting code in a local 
	forvalues i = 1/`=_N' {
	
		local barlist "`barlist' (scatteri `=rank_corr[`i']' 0 `=rank_corr[`i']' `=corr[`i']' , recast(line) lcolor("`=colorname[`i']'") lwidth(*6))"
	
	}
	
	* Saving labels for Y-Axis in a local
	levelsof rank_corr, local(yl)
	foreach l of local yl {
	
		local ylab "`ylab' `l'  `" "`:lab (rank_corr) `l''" "'"	
		
	}	
	
	twoway `barlist' ///
			(rspike lb ub rank_corr, horizontal lcolor(white) lwidth(*2)) ///
			(rspike lb ub rank_corr, horizontal lcolor(black*.5)), ///
			legend(off) scheme(white_tableau) ylabel(`ylab', labsize(2.5)) ///
			xlab(, labsize(2.5)) ///
			ytitle("Pairs") xtitle("Correlation Coeff.") ///
			title("Correlation Coefficient with Confidence Interval (Diverging Bar Plot)", size(3) pos(11))

