library(data.table)
library(readxl)
library(stringr)
library(xlsx)
library(ggplot2)
library(readr)
library(plotly)
library(zoo)
source("Functions_HC.R")

# Hate Crime analysis from NYPD data 
# we want to look at the change of complaints over time & potentially broken down by gender/race/type 
# read the data 

### complaints: make sure that these are actually complaints? we do

# let's look at the notes 
# let's look at total complaints and by motivation 
complaintfiles <- list.files("data/", pattern = "complaints")
quarters <- complaintfiles[grep("-q", complaintfiles)]

# let's look at complaints by motivation 
inds <- grep("by-motivation", quarters)
motivations <- quarters[inds]

sheets <- list()
lendocs <- c()

for(i in 1:length(motivations)){
name <- motivations[i]
wb <- loadWorkbook(motivations[i])
x <- getSheets(wb)
sheets[[name]] <- x 
lendocs[i] <- length(x)
}

# read in the last sheet based on sheets name 
hccomp <- list()

for(i in 1:length(motivations)){
  x <- read_excel(motivations[i], sheet = lendocs[i])
  setDT(x)
  x[,Q.Yr:= (x[2,1])]
  x <- x[-(1:3), ]
  lastcol <- ncol(x)
  names(x) <- as.character(x[1, ])
  x <- x[-1, ]
  names(x)[lastcol] <- "Qtr.Yr"
  names(x) <- tolower(names(x))
  hccomp[[i]] <- as.data.table(x)
}

combcomp <- rbindlist(hccomp, fill= TRUE)
# make NAs 0
combcomp[is.na(combcomp)] <- 0

# make long to work work with ggplot 
idvars <- c("precinct", "qtr.yr")
inds <- which(!names(combcomp) %in% idvars)
complong <- melt(combcomp, measure.vars = names(combcomp)[inds]) 
complong[, value := as.numeric(value)]
complong[, year := substr(qtr.yr, 12, 16)]
complong[, qtr.yr := gsub("Quarter", "Quarter\n", qtr.yr)]
complong[, qtr.yr := factor(qtr.yr, levels = c("1st Quarter\n 2017", 
                                               "2nd Quarter\n 2017", 
                                               "3rd Quarter\n 2017",
                                               "4th Quarter\n 2017",
                                               "1st Quarter\n 2018", 
                                               "2nd Quarter\n 2018"))]

complong[, sum(value), by = "year"]
complong <- complong[variable != "total", ]
write.csv(complong, "complaints_hc.csv")

#### let's look at total complaints 
# total arrests across time 
complong[, .(total = sum(value)[1]), by = qtr.yr2]
p <- ggplot(complong[, .(total = sum(value)[1]), by = qtr.yr2], aes(x = qtr.yr2, y = total)) +
  geom_col(fill = "seagreen") + xlab("Quarter") + 
  ggtitle("Total Complaints (Hate Crimes)") +
  theme_light()
p

# by motivation - across all quarters 
p2 <- ggplot(complong[, .(N = sum(value)), by = .(variable)], aes(x = reorder(variable, N), y = N)) +
  geom_col(fill = "seagreen") + coord_flip() + theme_light() + xlab("Motivation") + 
  ggtitle("Total Complaints (Hate Crimes) by Motivation") 
p2

p3 <- ggplot(complong[, .(N = sum(value)), by = .(variable, qtr.yr)], aes(x = reorder(variable, N), y = N)) +
  geom_col(fill = "seagreen") + coord_flip() + facet_wrap(~qtr.yr) + theme_light() + xlab("Motivation")
p3

# let's look at the top 10 motivations over time 

###### ARRESTS
## read in arrests the same way & pull all together 

arrestfiles <- list.files("data/", pattern = "arrests")
quarters <- arrestfiles[grep("-q", arrestfiles)]

# let's look at arrests by motivation 
inds <- grep("by-motivation", quarters)
motivations <- quarters[inds]

hcarrest <- list()
sheets <- list()
lendocs <- c()

for(i in 1:length(motivations)){
  name <- motivations[i]
  wb <- loadWorkbook(motivations[i])
  x <- getSheets(wb)
  sheets[[name]] <- x 
  lendocs[i] <- length(x)
}
 

for(i in 1:length(motivations)){
  x <- read_excel(motivations[i], sheet = lendocs[i])
  setDT(x)
  x[,Q.Yr:= (x[3,1])]
  x <- x[-(1:3), ]
  x <- x[-1, ]
  names(x) <- as.character(x[1, ])
  x <- x[-1, ]
  lastcol <- ncol(x)
  names(x)[lastcol] <- "qtr.yr"
  inds <- grep("Bias", names(x))
  names(x)[inds] <- "Motivation"
  x[, Motivation := tolower(Motivation)]
  x[, Number:=NULL]
  x[, Precinct := na.locf(Precinct, fromLast = FALSE)]
  hcarrest[[i]] <- as.data.table(x)
}

combarr <- rbindlist(hcarrest, fill= TRUE)
# combarr[is.na(combarr)] <- 0
combarr[, year := substr(qtr.yr, 12, 16)]
write.csv(combarr, "arrests_hc.csv")
combarr[, N_M_Qy:= .N, by = .(Motivation, qtr.yr)]
combarr[, .N, by = qtr.yr]
combarr[, .N, by = year]
combarr[Motivation %in% "black", Motivation := "anti-black"]
combarr[, .N, by = .(Motivation, year)]
combarr[, year := as.numeric(year)]

# let's look at the annual data and compare  to the combined data from 2017
arr17 <- combarr[year %in% 2017, ]
artot <- read_excel("hate-crime-arrests-by-motivation-annual-2017.xlsx", 
                    sheet = "Arrests")

artot <- artot[-c(1:4), ]
names(artot) <- as.character(artot[1, ])
artot<- artot[-1, ]
names(artot)[6] <- "Motivation" 
artot[,.(N = .N), by = "Motivation"]
arr17[,.(N = .N), by = "Motivation"]

#
combarr[, qtr.yr := gsub("Quarter", "Quarter\n", qtr.yr)]
combarr[, qtr.yr := factor(qtr.yr, levels = c("1st Quarter\n 2017", 
                                              "2nd Quarter\n 2017", 
                                              "3rd Quarter\n 2017",
                                              "4th Quarter\n 2017",
                                              "1st Quarter\n 2018", 
                                              "2nd Quarter\n 2018"))]




combarr[, .(N = .N, year), by = .(Motivation, qtr.yr)]
combarr[, .(N = .N), by = qtr.yr]
arrsubtot <- combarr[, .(N = .N, year), by = .(Motivation)]

# total arrests 
p <- ggplot(combarr[, .(N = .N), by = qtr.yr], aes(x = qtr.yr, y = N)) +
  geom_col(fill = "seagreen") + xlab("Quarter") + 
  ggtitle("Total Hate Crimes For 2017 & 2 Qtr 2018") +
  theme_light()
p

# by motivation - across all quarters 
p2 <- ggplot(combarr[, .(N = .N), by = .(Motivation)], aes(x = reorder(Motivation, N), y = N)) +
  geom_col(fill = "seagreen") + coord_flip() + theme_light() + xlab("Motivation")
p2

p3 <- ggplot(combarr[, .(N = .N), by = .(Motivation, qtr.yr)], aes(x = reorder(Motivation, N), y = N)) +
  geom_col(fill = "seagreen") + coord_flip() + facet_wrap(~qtr.yr) + theme_light() + xlab("Motivation")

p3


# let's look at total arrests over time 
ggplot(combarr, aes(x = qtr.yr, y = N)) + geom_col(fill = "seagreen") + theme_classic()

# let's look at the top motivations 
ggplot(arrlong[, .(total = sum(value, na.rm = TRUE)), by = .(precinct, qtr.yr2)], 
       aes(x = total, y = qtr.yr2, color = precinct, group = precinct)) + geom_line() + theme_classic()














