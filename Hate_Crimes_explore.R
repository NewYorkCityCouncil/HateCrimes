library(data.table)
library(readxl)
library(stringr)
library(xlsx)
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

# motlist <- lapply(motivations, read_excel_allsheets)

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
crimedat <- list()

for(i in 1:length(motivations)){
  x <- read_excel(motivations[i], sheet = lendocs[i])
  setDT(x)
  x[,Q.Yr:= as.vector(rep(x[2,1], nrow(x)))]
  x <- x[-(1:3), ]
  names(x) <- as.character(x[1, ])
  crimedat[[i]] <- as.data.table(x)
}


rbindlist(crimedat, fill= TRUE)

## read in arrests 




### arrests 
