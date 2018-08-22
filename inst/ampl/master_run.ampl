
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/

reset;

model master_mod.ampl;    # load model
data  master_dat.ampl;     # load data

# set options
option solver (option_v["solver"]);    # set solver
option relax_integrality  (option_v["relaxed"]);  # relaxed master problem ?
option presolve  (option_v["presolve"]);  # relaxed master problem ?


# set solver options
if (option_v["solver"]) == "amplxpress" then 
	option xpress_options "mipabsstop=0 miprelstop= 0";
 

# compute restricted bounds on Invested capacities
if (option_v["solve_bounds"]) then
{
   for{z in INV_CANDIDATE}
   {
   		solve bound_capacity_min[z] >> out_log.txt;
   		if solve_result = "infeasible" then break;
   		let restrained_lb[z] := bound_capacity_min[z];
   		solve bound_capacity_max[z] >> out_log.txt;
   		if solve_result = "infeasible" then break;
   		let restrained_ub[z] := bound_capacity_max[z];
   } 
}


# solver master problem
if (option_v["solve_master"]) then
{
	solve master >> out_log.txt;
	# relaxed ub constraint if problem is infeasible
	if solve_result = "infeasible" then 
	{
		drop ub;
		solve master >> out_log.txt;
		if solve_result = "infeasible" then printf "error infeasible problem";
	}
	
	# correct Invested_capacity, slight negative values are possible due to constraint tolerances
	let {z in INV_CANDIDATE} Invested_capacity[z] := max(0, Invested_capacity[z]);
	
}



# write results (in the same folder)
if (option_v["solve_master"]) then
{
	printf "" > out_solutionmaster.txt;
	for {z in INV_CANDIDATE}
	{
		printf "%s;%f\n", z, Invested_capacity[z] >> out_solutionmaster.txt;
	}

	printf "%f\n", master >> out_underestimator.txt;
}

if (option_v["solve_bounds"]) then
{
	printf "" > in_out_capacitybounds.txt;
	for {z in INV_CANDIDATE}
	{
		printf "%s %f %f\n", z, restrained_lb[z], restrained_ub[z] >> in_out_capacitybounds.txt;
	}
} 


# write theta

#for {y in YEAR, w in WEEK}
#{
#	printf "%s;%s;%s;%f\n", card(ITERATION), y,w, Theta[y,w] >> out_theta.txt;
#}


