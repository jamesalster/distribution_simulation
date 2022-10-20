## Simulation of Distribution Attendance

The Problem: How many individual people are served over X distributions, given that: (1) service users come to multiple distributions, (2) the probability of doing so is affected by if they have been before, (3) there is population turnover among service users?

This repo implements a simulation to estimate the answer, given the following parameters that can be set:

* N: starting population
* d: number of distributions ('distro's)
* t: number of people served at each distribution (variable values over time possible)
* n_arrive: number arriving between each distribution (variable values over time possible)
* p_leave: probability of any given person leaving between each distribution(variable values over time possible.
* p_increased: increased relative probability of attending a distribution, if they have already come to at least one distribution.

Variable values over time were added to allow seasonal variation: however, the example given does not take advantage of this functionality.

