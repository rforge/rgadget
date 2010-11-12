\name{catch}
\alias{catch}
\title{Fleet catches}
\usage{catch(N, timestep, Fy, salpha, sbeta, numperyear, numobs, l)}
\description{Catch is implemented in \texttt{R} using the {\it Linearfleet}
option in Gadget. 
Let \eqn{C_{fleet,prey}(l,a,t)} be the number of age \eqn{a} prey, in
lengthgroup \eqn{l} caught at timestep \eqn{t}, then
\deqn{C_{fleet,prey}(l,a,t) = F_{l,t}N_{prey}(l,a,t)\Delta t}
with \eqn{F_{l,t} = S_{l}F_{y}} where \eqn{F_y} is constant for each year, 
\deqn{S_{l} = \frac{1}{1+e^{-\alpha-\beta l}}}
is the suitability function and \eqn{\alpha} and \eqn{\beta} are constants. ##'}
\value{Total catches of the fleet}
\arguments{\item{N}{number of prey}
\item{timestep}{the timestep of the catch}
\item{Fy}{fishing yield}
\item{salpha}{suitability constant for the fleet}
\item{sbeta}{suitability constant for the fleet}
\item{numperyear}{number of catch timesteps}
\item{numobs}{number of observation years}}
