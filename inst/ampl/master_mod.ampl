
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/
#
# this file describes the master problem of the benders decomposition


#-------------
#--- SETS ----
#-------------
param n_mc integer ;
param n_w integer ;

# set of investment candidates
set INV_CANDIDATE ;

# set of MC years
set YEAR := 1..n_mc;

# set of weeks
set WEEK := 1..n_w;

# set of bender cuts
set CUT ;



#-------------------
#--- PARAMETERS ----
#-------------------

param c_inv{INV_CANDIDATE};      	# investment costs 
param unit_size{INV_CANDIDATE};  	# unit of each investment step
param max_unit{INV_CANDIDATE};	 	# max number of units which can be invested

param prob{y in YEAR} := 1/n_mc; 	# probability of occurence of each MC year

param type{CUT} symbolic ;       	# type of the cut (yearly, weekly or average)
param z0{CUT, INV_CANDIDATE} ;      # invested capacity of each candidates for the given cut
param c0{CUT} ;                  	# total costs for the combination of invested capacities from which the cut comes from
param c0_inv{CUT};    				# investment costs (not used here)
param c0_op{CUT};     				# production costs (not used here)

param c0_yearly{CUT, YEAR} ;    	# yearly total costs
param c0_weekly{CUT, YEAR , WEEK} ;   # weekly total costs

param lambda_avg{CUT, INV_CANDIDATE} ;					#  rentability (average value over all MC years)
param lambda_yearly{CUT, INV_CANDIDATE, YEAR} ;         #  rentability (yearly values)
param lambda_weekly{CUT, INV_CANDIDATE, YEAR, WEEK} ;   #  rentability (weekly values)


#------------------
#--- VARIABLES ----
#------------------

var Invested_capacity{INV_CANDIDATE};       # capacity invested
var N_invested{INV_CANDIDATE} integer >=0;  # number of units invested

var Theta{YEAR, WEEK} >=0;


#-----------
#--- LP ----
#-----------

# objective :
minimize master : sum{y in YEAR} ( prob[y] * sum{w in WEEK} Theta[y,w]) ;

# description of invested capacity :
subject to bounds_on_invested_capacity{z in INV_CANDIDATE} : N_invested[z] <= max_unit[z];
subject to integer_constraint{z in INV_CANDIDATE} : Invested_capacity[z] = unit_size[z] * N_invested[z];		 

# bender's cut :
subject to cut_avg{c in CUT : type[c] = "average"} : sum{y in YEAR} ( prob[y] * sum{w in WEEK} Theta[y,w]) >=   c0[c] - sum{z in INV_CANDIDATE}(lambda_avg[c,z] * (Invested_capacity[z] - z0[c,z])) ;
subject to cut_yearly{c in CUT, y in YEAR : type[c] = "yearly"} : sum{w in WEEK} Theta[y,w] >=  c0_yearly[c,y] - sum{z in INV_CANDIDATE} (lambda_yearly[c,z,y] * (Invested_capacity[z] - z0[c,z]));
subject to cut_weekly{c in CUT, y in YEAR, w in WEEK : type[c] = "weekly"} : Theta[y,w] >=  c0_weekly[c,y,w] - sum{z in INV_CANDIDATE} (lambda_weekly[c,z,y,w] * (Invested_capacity[z] - z0[c,z]));

