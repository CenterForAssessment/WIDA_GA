#################################################################################
###
### Data preparation script for WIDA GA data, 2011-2016
###
#################################################################################

### Load Packages

require(SGP)
require(data.table)


### Utility function

strhead <- function (s, n) {
    if (n < 0)
        substr(s, 1, nchar(s) + n)
    else substr(s, 1, n)
}

### Load Data

WIDA_GA_Data_LONG <- as.data.table(read.delim("U:/DATA/SGP/ACCESS SGP/ACCESS_2011_2016.txt", sep='|', header = TRUE))
#WIDA_GA_Data_LONG <- as.data.table(read.delim("Data/Base_Files/ACCESS_2011_2016.txt", sep='|', header = TRUE))

### Clean Up Data

WIDA_GA_Data_LONG <- subset(WIDA_GA_Data_LONG, select=c("SCHOOL_YEAR", "GTID", "STUDENT_LAST_NAME", "STUDENT_FIRST_NAME", "ASSESSMENT_GRADE", "COMPOSITE_SCALE_SCORE", "COMPOSITE_PROF_LEVEL"))
setnames(WIDA_GA_Data_LONG, c("YEAR", "ID", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "PROF_LEVEL"))
levels(WIDA_GA_Data_LONG$GRADE) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "0")
WIDA_GA_Data_LONG[,VALID_CASE := "VALID_CASE"]
WIDA_GA_Data_LONG[,CONTENT_AREA := "READING"]
WIDA_GA_Data_LONG[,YEAR := as.character(WIDA_GA_Data_LONG$YEAR)]
WIDA_GA_Data_LONG[,ID := as.character(WIDA_GA_Data_LONG$ID)]
WIDA_GA_Data_LONG[,GRADE := as.character(WIDA_GA_Data_LONG$GRADE)]
WIDA_GA_Data_LONG[,SCALE_SCORE := as.numeric(WIDA_GA_Data_LONG$SCALE_SCORE)]
WIDA_GA_Data_LONG[,ACHIEVEMENT_LEVEL := as.character(WIDA_GA_Data_LONG$PROF_LEVEL)]
WIDA_GA_Data_LONG[PROF_LEVEL %in% c("", " NA", "A1", "A2", "A3", "P1", "P2"), ACHIEVEMENT_LEVEL := NA]
WIDA_GA_Data_LONG[,ACHIEVEMENT_LEVEL := strhead(ACHIEVEMENT_LEVEL, 1)]


### Invalidate Cases with Scale Score out of Range (PROF_LEVEL in c("", " NA", "A1", "A2", "A3", "P1", "P2"))

WIDA_GA_Data_LONG[PROF_LEVEL %in% c("", " NA", "A1", "A2", "A3", "P1", "P2"), VALID_CASE := "INVALID_CASE"]


### Reorder

setcolorder(WIDA_GA_Data_LONG, c(8,9,1,2,3,4,5,6,10,7))


### Save data

save(WIDA_GA_Data_LONG, file="U:/DATA/SGP/ACCESS SGP/WIDA_GA_Data_LONG.Rdata")
#save(WIDA_GA_Data_LONG, file="Data/WIDA_GA_Data_LONG.Rdata")
