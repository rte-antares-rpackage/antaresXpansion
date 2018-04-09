
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/

reset;

model master_multiy_mod.ampl;    # load model
data  master_multiy_dat.ampl;     # load data

# include options
include in_options.txt;

option solver cbc;    # set solver (should this be an input data which could be set differently ?)


 # solver master problem
solve >> out_log.txt;


display Installed_capacity;
display Delta_capa_negative;
display Delta_capa_positive;

# write results (in the same folder)

printf "" > out_solutionmaster.txt;
for {z in INV_CANDIDATE, h in HORIZON}
{
	printf "%s;%d;%f;%f;%f\n", z, h, Installed_capacity[z,h], Delta_capa_positive[z,h], Delta_capa_negative[z,h] >> out_solutionmaster.txt;
}

printf "%f\n", master >> out_underestimator.txt;

printf "" > out_invcost.txt;
for{h in HORIZON}
{
	printf " %d;%f\n", h, actuation_coefficient[h] * sum{z in INV_CANDIDATE}(c_inv[z,h] * Delta_capa_positive[z, h] +  Installed_capacity[z,h] * c_om[z,h]) >>  out_invcost.txt
}

# write theta

for {h in HORIZON, y in YEAR[h], w in WEEK[h]}
{
	printf "%d,%s;%s;%s;%f\n", card(ITERATION), h, y,w, Theta[h,y,w] >> out_theta.txt;
}


## solve the relaxed problem to compute dual variables of the cut

#option presolve 0;
#option relax_integrality 1;

#solve ;

# write dual of the cut

#for {c in CUT : type[c] = "average"}
#{
#	printf "%s;%s;%f\n", card(CUT), c, cut_avg.dual[c] >> out_dualaveragecut.txt;
#}

#for {c in CUT, y in YEAR : type[c] = "yearly"}
#{
#	printf "%s;%s;%s;%f\n", card(CUT),c, y, cut_yearly.dual[c,y] >> out_dualyearlycut.txt;
#}

#for {c in CUT, y in YEAR, w in WEEK : type[c] = "weekly"}
#{
#	printf "%s;%s;%s;%s;%f\n", card(CUT),c, y,w,cut_weekly.dual[c,y,w] >> out_dualweeklycut.txt;
#}