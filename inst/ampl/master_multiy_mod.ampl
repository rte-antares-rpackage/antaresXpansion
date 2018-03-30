
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

# set of investment candidates
set INV_CANDIDATE ;

# set of simulated years
set HORIZON ordered;

# set of MC years
set YEAR_TEMP dimen 2; # temporary set (to read the data)
set YEAR{h in HORIZON} = setof {(h, year) in YEAR_TEMP} year ; 
set ALL_YEAR = setof{(h, year) in YEAR_TEMP} year;

# set of weeks
set WEEK_TEMP dimen 2; # temporary set (to read the data)
set WEEK{h in HORIZON} = setof {(h, week) in WEEK_TEMP} week ; 
set ALL_WEEK = setof{(h, week) in WEEK_TEMP} week;

#set of benders iterations
set ITERATION ;

# set of average bender cuts
set AVG_CUT within {ITERATION, HORIZON} ;

# set of yearly bender cuts
set YEARLY_CUT within {ITERATION, HORIZON, ALL_YEAR} ;

# set of weekly bender cuts
set WEEKLY_CUT within {ITERATION, HORIZON, ALL_YEAR, ALL_WEEK} ;



#-------------------
#--- PARAMETERS ----
#-------------------

# discount rate
param r; # discount rate
param ref_year; # reference year
param actuation_coefficient {h in HORIZON} :=  1/(1+r)^(h - ref_year);

# investment candidates
param c_inv{INV_CANDIDATE, HORIZON};      	# investment costs 
param c_om{INV_CANDIDATE, HORIZON};      	# operation and maintenance costs

param unit_size{INV_CANDIDATE};  	# unit of each investment step
param max_unit{INV_CANDIDATE};	 	# max number of units which can be invested
param lifetime{INV_CANDIDATE};	 	# max number of units which can be invested

param max_installed{INV_CANDIDATE, HORIZON};	 	# min installed capacity
param min_installed{INV_CANDIDATE, HORIZON};	 	# max installed capacity
param already_installed{INV_CANDIDATE, HORIZON};    # already installed capacity

param relaxed{INV_CANDIDATE} symbolic ;	  # (true or false) is the investment made continuously, or with steps ?

param z0{ITERATION, HORIZON, INV_CANDIDATE} ; # invested capacity of each candidates for the given iteratoin

# average cut
param c0_avg{AVG_CUT} ;                 	# total costs (operation + investment) for the given iteration
param lambda_avg{AVG_CUT, INV_CANDIDATE} ;	#  rentability (average value over all MC years)

# yearly cut
param c0_yearly{YEARLY_CUT} ;    					# yearly total costs
param lambda_yearly{YEARLY_CUT, INV_CANDIDATE} ;    #  rentability (yearly values)

#weekly cut
param c0_weekly{WEEKLY_CUT} ;   					# weekly total costs
param lambda_weekly{WEEKLY_CUT, INV_CANDIDATE} ;    # rentability (weekly values)

# other
param prob{h in HORIZON, y in YEAR[h]} := 1/card(YEAR[h]) ; 	# probability of occurence of each MC year

#------------------
#--- VARIABLES ----
#------------------

var Delta_capa_positive{INV_CANDIDATE, HORIZON} >=0;  # new investment
var Delta_capa_negative{INV_CANDIDATE, HORIZON} >=0;  # new decomissionment
var Installed_capacity{INV_CANDIDATE, HORIZON} >= 0;       # capacity invested
var N_invested{INV_CANDIDATE, HORIZON} integer >=0;  # number of units invested

var Theta{h in HORIZON, YEAR[h], WEEK[h]};


#-----------
#--- LP ----
#-----------

# objective :
minimize master : sum{h in HORIZON}( actuation_coefficient[h] *   
                  sum{z in INV_CANDIDATE} (c_inv[z,h] * Delta_capa_positive[z, h] +  Installed_capacity[z,h] * c_om[z,h]) + 
                  sum{y in YEAR[h], w in WEEK[h]} (prob[h,y] * Theta[h,y,w]));

# description of invested capacity :		 
subject to integer_constraint{z in INV_CANDIDATE, h in HORIZON : relaxed[z] != "true"} : Installed_capacity[z,h] = unit_size[z] * N_invested[z,h];		 

subject to between_min_and_max{z in INV_CANDIDATE, h in HORIZON} : min_installed[z,h] <= Installed_capacity[z,h] <= max_installed[z,h];
subject to evolution_of_capacity{z in INV_CANDIDATE, h in HORIZON : ord(h) > 1} : Installed_capacity[z,h] = Installed_capacity[z,prev(h)] + Delta_capa_positive[z, h] - Delta_capa_negative[z,h];
subject to evolution_of_capacity_0{z in INV_CANDIDATE, h in HORIZON : ord(h) == 1} : Installed_capacity[z,h] = already_installed[z,h] + Delta_capa_positive[z, h] - Delta_capa_negative[z,h];


subject to lifetime_1{z in INV_CANDIDATE, h in HORIZON : ord(h) <= first(HORIZON) + lifetime[z]} : Installed_capacity[z,h] <= already_installed[z,h] + sum{hh in HORIZON : hh <= h} Delta_capa_positive[z,hh];
subject to lifetime_2{z in INV_CANDIDATE, h in HORIZON : ord(h) > first(HORIZON) + lifetime[z]} : Installed_capacity[z,h] <= already_installed[z,h] + sum{hh in HORIZON : hh <= h and hh > h - lifetime[z]} Delta_capa_positive[z,hh];

subject to max_investment{z in INV_CANDIDATE, h in HORIZON} : Delta_capa_positive[z, h] <= max_unit[z] * unit_size[z];

# bender's cut :
subject to cut_avg{(c,h) in AVG_CUT} : sum{y in YEAR[h]} ( prob[h,y] * sum{w in WEEK[h]} Theta[h,y,w]) >=   c0_avg[c,h] - sum{z in INV_CANDIDATE}(lambda_avg[c,h,z] * (Installed_capacity[z,h] - z0[c,h,z])) ;

subject to cut_yearly{(c,h,y) in YEARLY_CUT} : sum{w in WEEK[h]} Theta[h,y,w] >=  c0_yearly[c,h,y] - sum{z in INV_CANDIDATE} (lambda_yearly[c,h,y,z] * (Installed_capacity[z,h] - z0[c,h,z]));

subject to cut_weekly{(c,h,y,w) in WEEKLY_CUT} : Theta[h,y,w] >=  c0_weekly[c,h,y,w] - sum{z in INV_CANDIDATE} (lambda_weekly[c,h,y,w,z] * (Installed_capacity[z,h] - z0[c,h,z]));

