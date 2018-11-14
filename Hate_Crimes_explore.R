library(data.table)
library(readxl)


source("Functions_HC.R")
# Hate Crime analysis from NYPD data 
# we want to look at the change of complaints over time & potentially broken down by gender/race/type 
# read the data 

# aggregate data 
arrestfiles <- list.files(pattern = "arrests")
arrests <- lapply(arrestfiles, read_excel, col_names = FALSE)


# let's look at total complaints and by motivation 
complaintfiles <- list.files(pattern = "complaints")
quarters <- complaintfiles[grep("-q", complaintfiles)]


# let's look at motivation 
inds <- grep("by-motivation", quarters)
motivations <- quarters[inds]




motlist <- lapply(motivations, read_excel_allsheets)
motlist2 <- makeNamedList(motlist)
  



# let's try to parse the years 



# let's look at totals 
totals <- quarters[!inds]
