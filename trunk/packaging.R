library(roxygen)
#source('stock.R')
#source('function.R')
#source('summaryFunc.R')
package.skeleton('Rgadget',code_files=c('stock.R','function.R','summaryFunc.R'),
                 force=TRUE)
roxygenize('Rgadget',roxygen.dir='Rgadget',copy.package=FALSE,
           unlink.target=FALSE)
system('cp DESCRIPTION Rgadget/')
system('R CMD build Rgadget')
