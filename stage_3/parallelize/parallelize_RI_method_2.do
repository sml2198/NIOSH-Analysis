
/********************************************************************************
parallel_iterations_2.do
********************************************************************************/

*+- reset locals for counting interations 
local x = 0 // current number in loop
local convergence_count = 0  // number of successful/convergent iterations

*+- take the global subpart and make it a local (it's easier to manipulate as a variable this way)
local sig_subpart $global_sig_subpart

while (`convergence_count' < $num_iterations & `x' < $max_iterations) {

	pause "beginning iteration number `x' of $num_iterations"
	
	*+- first, add one to x (to keep track of number of completed loops/iterations, regardless of outcome - this starts at zero)
	local x = (`x' + 1)
	
	*+- set seed (this needs to be specific to the iteration number) 
		*+- the parallel command automatically creates a new seed based on the cluster number
	local new_seed = (`x' + 625)
	set seed `new_seed'
	
	*+- reset the subpart/violation of interest (set it equal to its real value)
	replace `sig_subpart' = copy_`sig_subpart'
	local converged ""

	*+- shuffle covariate of interest
	cap drop *_shuffled
	qui shufflevar `sig_subpart'
	
	*+- we should be able to do this in one line, but since sig_subpart is a global now, we can't give it a prefix
		*+- so we use foreach syntax to get around this, even though there's just one variable in the list 
	replace `sig_subpart' = `sig_subpart'_shuffled
	
	/****** THE MODEL! ********/	
	
	local cmd "$model $depvar $covars $covariates, vce(cl mineid) $suffix iter(100)"
	noi di "`cmd'"
	cap noi `cmd'
	pause "model number `x' run"
	
	*+- create a local macro containing a binary value indicating whether or not the regression converged
	local converged = e(converged)
	
	*+- sort (stably) on mineid and year so storing the coefficients can be done according to iteration number
	sort mineid year

	*+- grab the coefficient on each shuffled variable of interest & replace each corresponding _c var with the proper coefficient
		*+- just as earlier, we do to do this in a loop (because we can't add a suffix to a global for the variable names)
	if "`converged'" == "1" {
		qui replace x_`sig_subpart'_c = _b[`sig_subpart'] in `x'
		pause "coefficients should now be stored in vars"
		local convergence_count = `convergence_count' + 1
	}
	
	*+- if still in progress
	if "`convergence_count'" != "$num_iterations" noi di "not finished! on iteration `x' with `convergence_count' converged"

	*+- if iteration limit has been reached
	if ("`x'" == "$max_iterations") {
		noi di "`x' (max) iterations completed, convergence NOT achieved $num_iterations times"
		keep in 1/`x'
		keep *_c
	}
	
	*+- if finished in less than the maximum number of iterations
	if "`convergence_count'" == "$num_iterations" {
		noi di "finished! in `x' iterations"
		keep in 1/`x'
		keep *_c
	}
	
} // while statement - loop that iterates
