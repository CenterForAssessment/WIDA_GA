################################################################################
###                                                                          ###
###            Data preparation script for WIDA GA data, 2019-2021           ###
###                                                                          ###
################################################################################

### Load Packages
require(SGP)
require(data.table)

###   Load and Combine Data
wida_ga_19 <- fread("./Data/Base_Files/ACCESS_2019_cleaned_NCIEA.csv", colClasses = rep("character", 74))
wida_ga_20 <- fread("./Data/Base_Files/ACCESS_2020_cleaned_NCIEA.csv", colClasses = rep("character", 74))
wida_ga_21 <- fread("./Data/Base_Files/ACCESS_2021_cleaned_NCIEA.csv", colClasses = rep("character", 74))

WIDA_GA_Data_LONG <- rbindlist(list(wida_ga_19, wida_ga_20, wida_ga_21), use.names=TRUE)[,CONTENT_AREA := "READING"]

###   Clean Up Data
wida.ga.names <- c("VALID_CASE", "CONTENT_AREA", "GTID", "SCHOOL_YEAR", "Grade",
    "CompositeOverallScaleScore", "CompositeOverallProficiencyLevel_integer", "CSEMOverall",
    "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName", "StudentLastName", "StudentFirstName",
    "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram")

WIDA_GA_Data_LONG <- subset(WIDA_GA_Data_LONG, select = wida.ga.names)

setnames(WIDA_GA_Data_LONG,
         c("VALID_CASE", "CONTENT_AREA", "ID", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_CSEM",
           "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME",
           "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"))

WIDA_GA_Data_LONG[, GRADE := as.character(as.numeric(GRADE))]
WIDA_GA_Data_LONG[, SCALE_SCORE := as.numeric(SCALE_SCORE)]
WIDA_GA_Data_LONG[, SCALE_SCORE_CSEM := as.numeric(SCALE_SCORE_CSEM)]

WIDA_GA_Data_LONG[ACHIEVEMENT_LEVEL != "", ACHIEVEMENT_LEVEL := paste("WIDA Level", ACHIEVEMENT_LEVEL)]
WIDA_GA_Data_LONG[ACHIEVEMENT_LEVEL == "", ACHIEVEMENT_LEVEL := "NO SCORE"]

# WIDA_GA_Data_LONG <- SGP:::getAchievementLevel(WIDA_GA_Data_LONG, state="WIDA_GA", achievement.level.name="ACH_LEV")
# table(WIDA_GA_Data_LONG[, .(ACHIEVEMENT_LEVEL, VALID_CASE), ACH_LEV], exclude = NULL)

### Check for duplicates

setkey(WIDA_GA_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(WIDA_GA_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
WIDA_GA_Data_LONG[which(duplicated(WIDA_GA_Data_LONG, by=key(WIDA_GA_Data_LONG)))-1, VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, ID)


### Save data
save(WIDA_GA_Data_LONG, file="Data/WIDA_GA_Data_LONG.Rdata")
