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
motlist <- lapply(motivations, read_excel_allsheets)

# motlist2 <- makeNamedList(motlist)
for(i in 1:length(motivations)){
wb <- loadWorkbook(motivations[i])
sheets <- getSheets(wb) 
}

for(i in 1:6){
  motlist[[i]] <- readLines()
  
}

### arrests 
