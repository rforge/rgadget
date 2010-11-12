\name{Rgadget}
\alias{Rgadget}
\title{Rgadget}
\usage{Rgadget(opt=gadget.options())}
\description{Each year is divided into timesteps of equal length.  Two stocks are
simulated, representing immature and mature stock components, which
live on 2 areas. Each stock grows and is subject to natural mortality
and there is recruitment into the immature stock. Optional processes are:
- 0 to 2 fleets, representing the commercial fleet and a survey.
- Migration.
- Movement between the stocks, representing maturation. 
- Consumption such that the mature stock is a predator of the immature stock.
Fleets are modelled as predators, as in Gadget. The survey takes place
in the first timestep of each each year and the commercial catch takes
place in each timestep for every year. Maturation from stock $A$ to
stock $B$ is modelled by moving the oldest agegroup of $A$ into $B$
with the age increasing if done on the last timestep of the year. This
replicates the Gadget process \emph{doesmove}.
The order of calculations is the same as in Gadget and is as follows:
1. Migration between areas
2. Consumption, including catch by the fleets
3. Natural mortality
4. Growth 
5. Recruitment
6. Remove the stock, here immature, that will move
7. Increase the age
8. Replace the stock, here to the mature, that has moved and increase the age.}
\value{a list of arrays:
\item{Rec}{Recruits for all years}
\item{immStart}{Initial starting population for the immature stock age 2 and older}
\item{matStart}{Initial starting population for the mature stock age 2 and older}
\item{immNumRec}{Immature stock population for all timesteps, areas, ages and lengths}
\item{matNumRec}{Mature stock population for all timesteps, areas, ages and lengths}
\item{immCsurv}{Survey catches of the immature stock}
\item{matCsurv}{Survey catches of the mature stock}
\item{immCcomm}{Commercial catches of the immature stock}
\item{matCcomm}{Commercial catches of the mature stock}
\item{Eat}{Amount consumed of immatures by matures}
\item{GrowthProb}{Growthprobability matrix}
\item{immMort}{Natural mortality for the immature stock}
\item{matMort}{Natural mortality for the mature stock}
\item{opt}{Gadget options list used in the simulation}}
\arguments{\item{opt}{gadget options list}}
