
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/
#
# all data files are supposed to be located in the same folder


data;

set HORIZON := include in_horizon.txt;

#number of mc years
set YEAR_TEMP := include in_mc.txt;

#number of weeks
set WEEK_TEMP := include in_week.txt;

#iteration
set ITERATION := include in_iterations.txt;

#invested capacity in the simulated iterations
param z0 := include in_z0.txt ;

# discount rate
param  r := include in_r.txt ;
param ref_year := include in_refyear.txt ;

#investment candidates description
param : INV_CANDIDATE : unit_size max_unit lifetime relaxed := include in_candidates.txt ;
param : c_inv  c_om min_installed max_installed  already_installed := include in_candidate_bis.txt ;


#bender cuts 
param : AVG_CUT : c0_avg := include in_avgcuts.txt ;
param : YEARLY_CUT : c0_yearly := include in_yearlycuts.txt ;
param : WEEKLY_CUT : c0_weekly := include in_weeklycuts.txt ;


#marg. cost of each investment (as returned by ANTARES)
param lambda_avg := include in_avgrentability.txt ;
param lambda_yearly := include in_yearlyrentability.txt ;
param lambda_weekly := include in_weeklyrentability.txt ;

