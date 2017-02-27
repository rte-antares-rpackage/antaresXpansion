# antares-rpackageXpansion


![](vignettes/antareslogo.png)

This package works along with RTE's adequacy software ANTARES : https://antares.rte-france.com/

`antaresXpansion` is the package which optimizes the installed capacities of an ANTARES study.
Typical uses of the package are for:

* __long-term scenario building__: build an economically consistent long-term generation mix 
* __transmission expansion planning__ : compute the network developement which maximizes social welfare


The investment decisions are optimized by running ANTARES' simulations iteratively. At each iteration, the installed capacity of the investments are updated, and the simulation are repeated until the total costs has converged to a minimum. The total cost evaluated in this problem are the sum of **the expected operation cost during one year** and **the investment annuity**.

`antaresXpansion` is currently under development.



## Definition of investment candidates


#### Concept

The user of the package defines investiment candidates. Each investment candidate is characterized with the following properties:

* __name__: name of the investment candidate 
* __link__: link whose capacity will be invested
* __annual cost per MW__: investment cost, per year and per MW
* __unit size__: size, in MW, of an investment (e.g. one group of 300 MW)
* __maximum units__: maximum number of units which can be built


Concretely, the investiment decision will affect only the capacity of the ANTARES' links. Investing in interconnections can be made directly with the package, while investing in generation capacity or storage capacity can be made using the so-called concept of "virtual nodes"" with ANTARES.

The definition of all the investment candidates is given in a new input file, located in the user folder of the study: `./user/expansion/candidates.ini`. The syntax used within this file is illustrated in the example below.

#### Example

An example with two investments candidates, one in semi-base generation and one in network capacity, is given below.

![](vignettes/example2nodes.png)

The invested semi-base generation in area 1 is shifted in the "virtual node" invest_semibase. Within the optimization process, the capacity of the link between area 1 and invest_semibase will be updated with the new invested capacity.

The `candidates.ini` file for this example will be the following one:

```r
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

## Method and settings

#### Benders decomposition


#### Settings

## License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com


