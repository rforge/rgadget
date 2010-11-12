\name{migrate}
\alias{migrate}
\title{Migration}
\usage{migrate(N1, N2, k, P=migrationProb(), opt=gadget.options())}
\description{Migration takes place in every timestep and migration is assumed to be
constant for all years.
Migration at timestep $t$ is defined as a $2\times 2$ transition
matrix $P_t := [p_{ij}]$ where $p_{ij}$ is the proportion moving from
area $j$ to area $i$. {\bf NB:} for $P$ to be a transition matrix
$\sum_ip_{ij} = 1$, for a fixed $j$.  
If $N\textrm{\emph{1}}_t$ is a matrix containing the abundance in
numbers in area 1 at timestep $t$ before migration and
$N\textrm{\emph{2}}_t$ is the same number for area 2, the numbers
after migration will be
\begin{align}
N\textrm{\emph{1}}_t &= p_{11}\cdot N\textrm{\emph{1}}_t + p_{12}\cdot N\textrm{\emph{2}}_t \notag \\
N\textrm{\emph{2}}_t &= p_{21}\cdot N\textrm{\emph{1}}_t + p_{22}\cdot N\textrm{\emph{2}}_t 
\end{align}
As there are only 2 areas and the sum of the transition matrix columns
must be 1 it is only necessary to enter the values of the first line
into the migration file in the \texttt{R} program.  In the
Gadget2.0.03 migration file, the proportion moving must be removed
from the old area and added to the new area.}
\value{a matrix with the new population after migration.}
\arguments{\item{N1}{number in area 1}
\item{N2}{number in area 2}
\item{k}{time step}
\item{P}{migration matrix}
\item{opt}{gadget options list}}
