\name{adjustconsumption}
\alias{adjustconsumption}
\title{Adjust for overconsumption}
\usage{adjustconsumption(C, S, E, N, maxratioconsumed, numofareas)}
\description{For each prey an upper limit needs to be set on the total amount
consumed by all predators so as not to obtain more consumption than
available biomass.  Consumption is limited to 95\% ($R_M$) of the available
biomass. This is implemented by scaling target consumption by all
predators. 
Let R_{prey}(l) be the Ratio consumed and R_M be the Maximum Ratio Consumed
Then
\deqn{R_{prey}(l)=\frac{\sum_{pred}\sum_{L}C_{pred,prey}(L,l)}{N_{prey}(l)W_{prey}(l)}}
If R_{prey}(l)>R_M consumption is adjusted as follows
\deqn{C_{pred,prey}(L,l)=R_MN_{prey}(l)W_{prey}(l)\frac{C_{pred,prey}(L,l)}{\sum_{pred}C_{pred,prey}(L,l)}}}
\value{a list with adjusted catches/consumption for C, S and E.}
\arguments{\item{C}{is the commercial catch of prey}
\item{S}{is the survey catch of prey}
\item{E}{is the consumption of prey by predator}
\item{N}{is the total number of prey}
\item{opt}{gadget options list}}
