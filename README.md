# antares-rpackageXpansion


![](vignettes/antareslogo.png)

This package works along with RTE's adequacy software ANTARES : https://antares.rte-france.com/

`antaresXpansion` is the package which optimizes the installed capacities of an ANTARES study.
Typical uses of the package are for:

* __long-term scenario building__: build an economically consistent long-term generation mix 
* __transmission expansion planning__ : compute the network developement which maximizes social welfare


The investment decisions are optimized by running ANTARES' simulations iteratively. At each iteration, the installed capacity of the investments are updated, and the simulations are repeated until the total costs have converged to a minimum. The total cost evaluated in this problem are the sum of **the expected operation cost during one year** and **the investment annuity**.

`antaresXpansion` is currently under development. Feel free to submit any issue.



## Definition of investment candidates


### Concept

The user of the package defines investiment candidates. Each investment candidate is characterized with the following properties:

* __name__: name of the investment candidate 
* __link__: link whose capacity will be invested
* __annual cost per MW__: investment cost, per year and per MW
* __unit size__: size, in MW, of an investment unit (e.g. one group of 300 MW)
* __maximum units__: maximum number of units which can be built

Concretely, the investiment decision will affect only the capacity of the ANTARES' links. Investing in interconnections can be made directly with the package, while investing in generation capacity or storage capacity can be made using the so-called concept of "virtual nodes" with ANTARES.

The definition of all the investment candidates is given in a new input file, located in the user folder of the study: `./user/expansion/candidates.ini`. The syntax used within this file is illustrated in the example below.

### Example

An example with two investments candidates, one in semi-base generation and one in network capacity, is given below.

![](vignettes/example2nodes.png)

The invested semi-base generation in area 1 is shifted in the "virtual node" invest_semibase. Within the optimization process, the capacity of the link between area 1 and invest_semibase will be updated with the new invested capacity.

The `candidates.ini` file for this example will be the following one. This file has to be saved in the folder  `user/expansion/`

```txt
[1]
name = semibase
link = area1 - invest_semibase
annual-cost-per-mw = 126000
unit-size = 200
max-units = 5

[2]
name = grid
link = area1 - area2
annual-cost-per-mw = 3000
unit-size = 500
max-units = 4
```

### Distributed generation

For the case of distributed generation and storage, the investment variables can be *continuous*, without steps of several MW. In that case, the properties `unit-size`and `max-units`can be replaced by the property `max-investment`, and the invested capacity will be able to take any real value between `0` and `max-investment` (in MW).


## Method and settings

### Benders decomposition

The method used to perform the optimal expansion is a __benders decomposition__ in which:

  * The slave problem is an ANTARES simulation, where the installed capacities of the investment candidates are fixed,
  * The master problem is a MILP, in which the decision variables are the installed capacities of each investment candidate.
  
The objective function which is minimized, is the sum of the expected annual operation cost (column OV. COST + column HURDLE COST of the outputs of ANTARES ) and the investment annuity (as defined in the `candidates.ini` file).


Concretely, the package will run ANTARES iteratively. Depending on the number of candidates, the convergence to the optimal solution can be quite long (several hundreds iterations, and so several hundreds ANTARES' simulations). Some algorithmic settings are however available and propose different tradeoffs between the accuracy of the solution and the computation time. 


### Settings

The different settings can be modified by the user of the package. All the settings are saved in a file `settings.ini` located in the foler `user/expansion/`. 

  * `optimality_gap`: The optimality gap can take any numeric value. The optimality gap is theoretically the maximum possible distance (in euros) between the solution returned by the method and the optimal solution. 
  
  * `master`: Can take two values, `relaxed` or `integer`. Defines whether or not should integer variables be used for the master problem.
  
  * `uc-type`: The UC (Unit Commitment) type can take two different values, `relaxed_fast` or `accurate`. It defines which mode will be used in ANTARES. `relaxed_fast` correponds to the fast mode of ANTARES in which the minimum up/down and minimum stable power constraints are relaxed.
  

An example of a `settings.ini` file with the appropriate syntax is given below.

```txt
uc_type = relaxed_fast
master = integer
optimality_gap = 0
```
Note that the optimality gap can also be given relatively to be best found solution by entering a `%` after the numeric value of the setting. 


### Which settings should I use for my expansion problem ?



#####      __1. If I have to run several expansion optimizations of different variants of a study and compare them__

In that case, if the optimal solutions are not returned by the package, the comparison of several variants can be tricky as the imprecision of the method might be in the same order of magnitude as the changes brought by the input variations.

It is therefore advised to be as closed as possile from the __optimum__ of the expansion problem. To do so, the two following conditions should __necessarily__ be fulfilled:

  * set the `optimality_gap` to zero.
  * set the `uc_type` to a `relaxed` mode (for now, only the `relaxed_fast`mode exists)
  
[Remark : even with the two conditions mentionned above, the reslult might be slighty different from the optimum due to numeric approximations, this can be partly solved by putting to optimality gap to `-Inf`]



#####      __2. If I'm building one consistent generation/transmission scenario __

As the optimal solution is not more realistic than an approximate solution of the modelled expansion problem. The settings can be less constraining with :

  * an `optimality_gap` of a few million euros
  * a `uc_type` which is not necessarily `relaxed`


## How to use the package ?


Load the package

```r
library(antaresXpansion)
```

Select an ANTARES study using [antaresRead package](https://github.com/rte-antares-rpackage/antares-rpackageRead). As no  outputs are needed, the simulation argument should be put to zero.

```r
setsimulationPath("study_path", simulation = 0)
```

Create the `candidate.ini` and `settings.ini` files as explained above and store them in the directory `study_path/user/expansion`.


Enter the path of the ANTARES solver (for example).

```r
path_solver <- "C:/Program Files/RTE/Antares/6.0.0/bin/antares-6.0-solver.exe"
```

Run the `benders` function.

```r
benders(path_solver, display = TRUE, report = TRUE)
```

The expansion optimization can be quite long, intermediary results can be written in the console using `display = TRUE`. Moreover, if `report = TRUE`, the results of the function will be summarized in a html report saved in the directory `study_path/user/expansion/report`.

## License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com


