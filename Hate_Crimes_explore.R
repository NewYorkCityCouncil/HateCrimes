library(data.table)
library(readxl)
library(stringr)
library(xlsx)
require(ggplot2)
source("Functions_HC.R")

# Hate Crime analysis from NYPD data 
# we want to look at the change of complaints over time & potentially broken down by gender/race/type 
# read the data 

### complaints 

# let's look at total complaints and by motivation 
complaintfiles <- list.files(pattern = "complaints")
quarters <- complaintfiles[grep("-q", complaintfiles)]

# let's look at complaints by motivation 
inds <- grep("by-motivation", quarters)
motivations <- quarters[inds]

# motlist2 <- makeNamedList(motlist)
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
  crimedat[[i]] <- as.data.table(x)
}

combcomp <- rbindlist(crimedat, fill= TRUE)

# make long to work work with ggplot 
vars <- names(combhc)[-c(1,15)]
complong <- melt(combhc, measure.vars = vars) # let's take a look 

###### ARRESTS
## read in arrests the same way & pull all together 
arrestfiles <- list.files(pattern = "arrests")
quarters <- complaintfiles[grep("-q", arrestfiles)][-1]

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

hcarrest <- list()

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
  hcarrest[[i]] <- as.data.table(x)
}

combarr <- rbindlist(hcarrest, fill= TRUE)
vars <- names(combarr)[-c(1,15)]
arrlong <- melt(combarr, measure.vars = vars)



