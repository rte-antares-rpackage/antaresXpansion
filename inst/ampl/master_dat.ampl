
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/
#
# all data files are supposed to be located in the same folder


data;

#number of mc years
set YEAR := include in_mc.txt;

#number of weeks
set WEEK := include in_week.txt;

#investment candidates description
param : INV_CANDIDATE : c_inv  unit_size max_unit := include in_candidates.txt ;

#bender cuts 
param : CUT : c0 c0_inv c0_op type := include in_cut.txt ;

#invested capacity in the simulated iterations
param z0 := include in_z0.txt ;

#marg. cost of each investment (as returned by ANTARES)
param lambda_avg := include in_avgrentability.txt ;
param lambda_yearly := include in_yearlyrentability.txt ;
param lambda_weekly := include in_weeklyrentability.txt ;

# yearly production costs (as returned by ANTARES)
param c0_yearly := include in_yearlycosts.txt ;

# weekly production costs (as returned by ANTARES)
param c0_weekly := include in_weeklycosts.txt ;