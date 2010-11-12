\name{gadget.options}
\alias{gadget.options}
\title{Gagdet options}
\description{This function creates a list of default values of all necessary switches
for the RGadget simulation and file output. The user can then change the
values of the switches and use the changed list as input to RGadget.}
\value{a list of swithes
\item{stocks}{names of the stocks in the simulation}
\item{doeseat}{Does the mature stock eat the immature}
\item{doescatchsurv}{Is there a survey}
\item{doescatchcomm}{Is there a commercial effort}
\item{doesmigrateimm}{Does the immature stock migrate}
\item{doesmigratemat}{Does the mature stock migrate}
\item{migrationP1}{Probability of staying in area 1 (TWO AREA ASSUMPTION)}
\item{migrationP2}{Probability of staying in area 2 (TWO AREA ASSUMPTION)}
\item{doesfuncmigrate}{(migration) pde's used to describe migration.}
\item{diffustion}{(migration) diffusion parameter}
\item{driftx}{(migration) drift in x coordinate}
\item{drifty}{(migration) drift in y coordinate}
\item{doesmove}{Does the immature stock mature into the mature stock}
\item{numofareas}{Number of gadget areas}
\item{probarea}{A vector of proportions in a given area, assumed equal for both stocks}
\item{areasize}{Size of the gadget area (assumed equal for all areas}
\item{area.temperature}{Average temperature of the area}
\item{immminage}{Minimum age of the immmature stock}
\item{immmaxage}{Maximum age of the immmature stock}
\item{matminage}{Minimum age of the mature stock}
\item{matmaxage}{Maximum age of the mature stock}
\item{minlen}{Minimum length of both stocks}
\item{maxlen}{Maximum length of both stocks}
\item{lengthgrouplen}{Size of each lengthgroup. We assume the size of the lengthgroups is the same for both stocks.}
\item{a}{a in the length-weight relationship a*l^b}
\item{b}{b in the length-weight relationship a*l^b}
\item{sigma}{The standard deviation of length at i years old. This vector must the same length as the number of ages.}
\item{murec}{If specified this will be the meanlength of recruits}
\item{lsup}{L-infinity. Bertalanffy growth parameters lsup, and k for the growth function (used for all ages > 1)}
\item{binn}{binn is the maximum updating length}
\item{beta}{Beta for beta-binomial}
\item{numobs}{number of years observed}
\item{numoftimesteps}{number of timesteps in each year}
\item{z}{z is the natural mortality constant used to calculate the size of the initial population for age 2 +}
\item{spalpha}{alpha for the predation suitability function}
\item{spbeta}{beta for the predation suitability function}
\item{spagamma}{gamma for the predation suitability function}
\item{spdelta}{delta for the predation suitability function}
\item{m0}{m0 for the maximum consumption}
\item{m3}{m3 for the maximum consumption}
\item{H}{H for the maximum consumption}
\item{otherfrac}{the fraction of otherfood that is eaten}
\item{otherfood}{maxratioconsumed}{The maximum portion consumed, in Gadget it is 0.95, this is known as understaocking in Gadget}
\item{survstep}{timestep(s) for the survey}
\item{commstep}{timestep(s) for the commercial effort}
\item{salphasurv}{for the suitability function - survey}
\item{sbetasurv}{for the suitability function - survey}
\item{survfleettype}{Fleettype for the survey}
\item{survmultiplicative}{For the fleettype}
\item{Fysurv}{Fishing effort of the survey}
\item{surv.catches}{What stocks does the survey fleet catch from}
\item{salphacomm}{for the suitability function - commerical catch}
\item{sbetacomm}{for the suitability function - commercial catch}
\item{commfleettype}{Fleettype for the commercial catch}
\item{comm.catches}{What stocks does the commercial fleet catch from}
\item{commmultiplicative}{For the fleettype}
\item{Fycomm}{Fishing effort of the commercial catch}
\item{calcindex}{output survey index likelihood file}
\item{calc.in.kilos}{output catches in kilos likelihood file}
\item{estimate.recruits.by.year}{should recruits be estimated by year}
\item{estimate.recruits.by.year.and.area}{should recruits be estimated by year and area}
\item{estimate.recruits.by.year.constant.area}{should recruits be estimated by year with constant by area effect (fewer degrees of freedom used compared to the one above).}
\item{randomised.recruits}{Should recruits be initialised randomly}
\item{bylen}{output survey index likelihood file by length groups, 0 for age aggregation and 1 for length aggregation}
\item{length.groups}{length groups for indices}
\item{calcldist.c}{output length distribution likelihood data for the catch}
\item{calcalk.c}{output age length distribution likelihood data for the catch}
\item{calcldist.s}{output length distribution likelihood data for the survey}
\item{calcalk.s}{output age length distribution likelihood data for the survey}
\item{file.migration.ratio}{Output migration matricies (0) or ratios (1)}
\item{alkagg}{aggregation level for age length distributions, will be used for both}
\item{survey.sigma}{sigma for error in survey indices}
\item{catch.sigma}{sigma for error in catchdistributions}
\item{frac}{fraction of catch that should be put in likelihood files}
\item{doesgrow}{do the stocks grow}
\item{growthfunction}{What growth function should be used, currently only lengthvbsimple is implemented}
\item{optim.params}{What parameters should Gadget optimise}
\item{randomise.all.initial.values}{Should initial values be randomized}}
