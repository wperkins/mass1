# -------------------------------------------------------------
# file: julian-median.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 20, 2001 by William A. Perkins
# Last Change: 2018-03-01 12:53:05 d3g096
# -------------------------------------------------------------

# library(chron)
# options(chron.year.abb=FALSE)

infile <- "@IN@"
outfile <- "@OUT@"

stats <- read.table(infile, header=FALSE, as.is=TRUE,
                    col.names=c('datestr', 'min', 'mean', 'max', 'junk'),
                    skip=1)

stats$date <- strptime(stats$datestr, format="%m-%d-%Y", tz="PST")
stats$month <- stats$date$month
stats$day <- stats$date$day
stats$year <- stats$date$year + 1900
stats$julian <- stats$date$yday + 1

medlen <- by(stats$mean, stats$julian, length)
medmin <- by(stats$min, stats$julian, median)
medmean <- by(stats$mean, stats$julian, median)
medmax <- by(stats$max, stats$julian, median)

out <- file(outfile, open="w")
writeLines(paste("#Julian Count min mean max (medians) from", infile), con=out)
writeLines(paste(formatC(as.integer(attr(medlen, "dimnames")$`stats$julian`), width=5),
                 formatC(medlen, width=5),
                 formatC(medmin, width=8, digits=2, format="f"),
                 formatC(medmean, width=8, digits=2, format="f"),
                 formatC(medmax, width=8, digits=2, format="f"),
                 sep=" "),
           con=out)
close(out)
           
# points(meanmax)
# points(medmean)
# str medmax
# str(medmax)
# str(medmean)
# abline(18,0)
# length(stats$mean > 18.0)
# length(stats$mean)
# length(which(stats$mean > 18.0))
# 400/3623
# length(which(stats$max > 18.0))
# length(which(stats$max > 18.0))/length(stats$mean)
# mean(stats$max[stats$max > 18.0])
# mean(stats$max[stats$max > 18.0] - 18)
# mean(stats$mean[stats$mean > 18.0] - 18)
# library(stepfun)
# plot(ecdf(stats$max))
# plot(ecdf(stats$max),col="red", verticals=TRUE)
# plot(ecdf(stats$max),color="red", verticals=TRUE)
# plot(ecdf(stats$max),col.vert="red", col.horiz="red", verticals=TRUE)
# plot(ecdf(stats$max),col.vert="red", col.hor="red", verticals=TRUE)
# plot(ecdf(stats$max),col.vert="red", col.hor="red", verticals=TRUE, dopoints=FALSE, ylab="Nonexceedance Probability")
# plot(ecdf(stats$max),col.vert="red", col.hor="red", verticals=TRUE, dopoints=FALSE)
# plot(ecdf(stats$max),col.vert="red", col.hor="red", verticals=TRUE, do.points=FALSE)
# q()
# n
