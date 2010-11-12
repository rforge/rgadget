\name{suitability}
\alias{suitability}
\title{Prey suitability}
\usage{suitability(salpha, sbeta, sgamma, sdelta, l, L=c(0))}
\description{The suitability function for predation used in the {\texttt R} model is:
\deqn{S_{pred,prey}(L,l) = \frac{\delta}{1+e^{-\alpha-\beta l-\gamma L}}}
With one predator, one prey and otherfood the equation becomes:
\deqn{C_{L,l}&=N_{L}M_{L}\Psi_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA}}
\deqn{=N_{L}M_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA+HA}}
where $O$ is the density of otherfood.}
\value{matrix of suitabilities, columns prey length, lines predator length}
\arguments{\item{salpha}{\alpha for the suitability function.}
\item{sbeta}{\beta for the suitability function.}
\item{sgamma}{\gamma for the suitability function.}
\item{sdelta}{\delta for the suitability function.}
\item{l}{prey length group(s)}
\item{L}{predator length group(s)}}
