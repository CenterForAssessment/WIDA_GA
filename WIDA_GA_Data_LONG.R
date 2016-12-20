### Load Packages

require(SGP)
require(foreign)
require(data.table)


### Load Data

WIDA_GA_Data_LONG <- read.delim("U:/DATA/SGP/ACCESS SGP/ACCESS_2011_2016.txt", sep='|', header = TRUE)


### clean up data

WIDA_GA_Data_LONG <- subset(WIDA_GA_Data_LONG, select=c("SCHOOL_YEAR", "GTID", "STUDENT_LAST_NAME", "STUDENT_FIRST_NAME", "ASSESSMENT_GRADE", "COMPOSITE_SCALE_SCORE", "COMPOSITE_PROF_LEVEL"))
names(WIDA_GA_Data_LONG) <- c("YEAR", "ID", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "PROF_LEVEL")
levels(WIDA_GA_Data_LONG$GRADE) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "0")
WIDA_GA_Data_LONG$VALID_CASE <- "VALID_CASE"
WIDA_GA_Data_LONG$CONTENT_AREA <- "READING"


### Reorder 

WIDA_GA_Data_LONG <- as.data.table(WIDA_GA_Data_LONG)
setcolorder(WIDA_GA_Data_LONG, c(8,9,1,2,3,4,5,6,7))


### Save data

save(WIDA_GA_Data_LONG, file="U:/DATA/SGP/ACCESS SGP/WIDA_GA_Data_LONG.Rdata")

