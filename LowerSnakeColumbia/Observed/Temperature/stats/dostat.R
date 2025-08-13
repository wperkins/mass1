# -------------------------------------------------------------
# file: dostat.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November  9, 2001 by William A. Perkins
# Last Change: 2018-03-06 14:16:14 d3g096
# -------------------------------------------------------------

# source("/home/perk/src/R/read.R")
# options(chron.year.abb=FALSE)

all  <- read.table(file="@IN@", header=FALSE, as.is = TRUE,
                   col.names = c('date', 'time', 'temp', 'junk'),
                   skip = 1)

substart <- strptime("01/01/2004", "%m/%d/%Y")

junk <- subset(all, (strptime(date,"%m-%d-%Y") >= substart))
all <- junk


max <- by(all$temp, all$date, max)
min <- by(all$temp, all$date, min)
mean <- by(all$temp, all$date, mean)

cat("# Daily Statistics from", "@IN@\n", file="@OUT@", append=FALSE)

out <- paste(attr(mean, "dimnames")$`all$date`, 
             formatC(min, width=8, digits=2, format="f"),
             formatC(mean, width=8, digits=2, format="f"),
             formatC(max, width=8, digits=2, format="f"),
             "/", sep=" ")
f <- pipe("sort -n -k 1.7,1.10 -k 1.1,1.2 -k 1.4,1.5 >>@OUT@", open="w")
writeLines(out, con=f)
close(f)
