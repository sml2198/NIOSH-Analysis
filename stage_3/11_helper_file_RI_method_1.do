
/********************************************************************************
11_helper_file_RI_method_1.do
********************************************************************************/

*+- reset locals for counting interations 
local x = 0 // current number in loop
local convergence_count = 0  // number of successful/convergent iterations
 
while (`convergence_count' < $num_iterations & `x' < $max_iterations) {

	*+- first, add one to x (to keep track of number of completed loops/iterations, regardless of outcome - this starts at zero)
	local x = (`x' + 1)
	
	*+- set seed (this needs to be specific to the iteration number) 
		*+- the parallel command automatically creates a new seed based on the cluster number
	local new_seed = (`x' + 625)
	set seed `new_seed'
	
	*+- reset locals 
	local converged ""
	local cov_of_interest ""
	
	*+- drop existing shuffled variables (if this is not the first instance of the loop that iterates)
	capture drop *shuffled

	*+- shuffle all covariates of interest (i.e. randomly sample without replacement)
	foreach var of varlist $covars {
		qui shufflevar `var'
	}
	
	*+- set local for covariates of interest (now we grab the SHUFFLED variables, instead of the unshuffled variables contained in "covars")
	foreach var of varlist *_shuffled {
		local cov_of_interest "`cov_of_interest' `var'"
	}
	pause "complete: variables-of-interest local assigned"
		
	/****** THE MODEL! ********/	
	local cmd "$model $depvar `cov_of_interest' $covariates, $suffix  vce(cl mineid) iter(100)"
	cap noi `cmd'
	pause "model number `x' run"
	
	*+- create a local macro containing a binary value indicating whether or not the regression converged
	local converged = e(converged)
			
	*+- count the numver of covariates/subparts of interest (so we know how many positions in the matrix to look for)
	qui describe `cov_of_interest'
	local count: word count `cov_of_interest' 
	
	*+- sort (stably) on mineid and year so storing the coefficients can be done according to iteration number
	sort mineid year
	
	*+- grab the coefficient on each shuffled variable of interest & replace each corresponding _c var with the proper coefficient
	if "`converged'" == "1" {
		foreach var of varlist `cov_of_interest' {
			qui replace x_`var'_c = _b[`var'] in `x'
		}
		pause "coefficients should now be stored in variables"
		local convergence_count = `convergence_count' + 1
	}
	
	*+- if still in progress
	if "`convergence_count'" != "$num_iterations" noi di "not finished! on iteration `x' with `convergence_count' converged"
	
	*+- if iteration limit has been reached
	if ("`x'" == "$max_iterations") {
		noi di "`x' (max) iterations completed, convergence NOT achieved $num_iterations times"
		keep in 1/`x'
		keep *shuffled_c
	}
	
	*+- if finished in less than the maximum number of iterations
	if "`convergence_count'" == "$num_iterations" {
		noi di "finished! in `x' iterations"
		keep in 1/`x'
		keep *shuffled_c
	}
} // while statement - loop that iterates
