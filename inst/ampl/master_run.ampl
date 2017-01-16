
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/

reset;

model master_mod.ampl;    # load model
data  master_dat.ampl;     # load data

# include options
include in_options.txt;

option solver cbc;    # set solver (should this be an input data which could be set differently ?)

 # solver master problem
solve >> out_log.txt;


# write results (in the same folder)

printf "" > out_solutionmaster.txt;
for {z in INV_CANDIDATE}
{
	printf "%s;%f\n", z, Invested_capacity[z] >> out_solutionmaster.txt;
}

printf "%f\n", master >> out_underestimator.txt;
