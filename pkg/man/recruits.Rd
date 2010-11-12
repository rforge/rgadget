\name{recruits}
\alias{recruits}
\title{Recruitment}
\usage{recruits(n, mu, sigma, l, numofareas, probarea, numobs, numoftimesteps)}
\description{The timestep (or timesteps) on which recruitment takes place is
defined by the user. 
Given $n_{t}$ recruits, at timestep $t$, with mean length $\mu$ and
standard deviation of length $\sigma$, the number of recruits in
lengthgroup $i$ is calculated by:
\deqn{N_{i,1,\text{t}} = n_t(\Phi(\frac{l_{i+1}-\mu}{\sigma})-\Phi(\frac{l_{i}-\mu}{\sigma}))}
As for the initial population, the number of recruits in each length
groups is given in the recruit input file (cf page \pageref{first}).}
\value{Recruits by area, length, time}
\arguments{\item{n}{Number of recruited individuals}
\item{mu}{Vector of mean length per age.}
\item{sigma}{Vector of standard deviation of length per age.}
\item{l}{Vector of length groups}
\item{numofareas}{Number of areas}
\item{probarea}{Vector of proportions living in an area.}
\item{numobs}{Number of years.}
\item{numoftimesteps}{Number of observations per year.}}
