# -------------------------------------------------------------
# file: compare.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: 2017-06-27 11:46:21 d3g096
# -------------------------------------------------------------

source("/home/haqu027/mass1-applications/read.R")
source("/home/haqu027/mass1-applications/calcR2.R")

sim <- Read.MASS1.ts("../ts3129.out")
substart <- strptime("01/01/2016 00:00:00", "%m/%d/%Y %H:%M:%S")
subend <- strptime("12/31/2016 00:00:00", "%m/%d/%Y %H:%M:%S")

 sim <- subset(sim, ((datetime >= substart) &
                      (datetime <= subend)))

obs <- Read.BC("../../bc_files/hydro/flow/PRD_flow.dat", offset=0.5/24)

tempsim <- sim$discharge[sim$timestamp %in% obs$timestamp]*(1.0)
tempobs <- obs$value[obs$timestamp %in% sim$timestamp]*(1.0)

n <- length(tempsim)

if (n <= 0) q()

postscript("../PRD-scatter-QTL.eps",
           width=6.0, height=6.0,
           paper="letter", horizontal=FALSE)

par(font.main=par('font.lab'))
par(cex.main=par('cex.lab'))
par(mar=c(3,3,2,1)+0.1)
par(mgp=c(2,.75,0))
# par(oma=c(0,0,0,0))
par(cex=1.5)

plot(tempobs, tempsim,
     pch = 3, col = 2, cex = 0.5,
     xlab = "Observed Discharge, cfs", 
     ylab = "Simulated Discharge, cfs",
     main = "")
abline(0,1)
dev.off()

cat("PRD QTL", n,
    format(Model.R2(tempobs, tempsim), digits = 2),
    format(Model.Bias(tempobs, tempsim), digits = 2),
    format(Model.RMS(tempobs, tempsim), digits = 2),
    format(Model.AME(tempobs, tempsim), digits = 2),
    format(Model.Estddev(tempobs, tempsim), digits = 2),
    "\n", file="../statistics.dat", append=TRUE)



