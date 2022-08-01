###############################################################################
###                                                                         ###
###              Data preparation script for 2022 WIDA GA data              ###
###                                                                         ###
###############################################################################

### Load Packages
require(SGP)
require(data.table)

###   Read in 2022 base data
WIDA_GA_Data_LONG_2022 <-
  fread(file = "Data/Base_Files/ACCESS_2022_cleaned_NCIEA.csv",
        colClasses = rep("character", 74))

###   Clean Up Data
wida.ga.names <- c(
  "VALID_CASE", "GTID", "SCHOOL_YEAR", "Grade",
  "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
  "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
  "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
  "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram")

##    Subset data
WIDA_GA_Data_LONG_2022 <- subset(WIDA_GA_Data_LONG_2022, select = wida.ga.names)

##    Re-name variables to conform to `SGP` conventions
setnames(WIDA_GA_Data_LONG_2022,
         c("VALID_CASE", "ID", "YEAR", "GRADE",
           "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
           "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
           "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
           "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"))

##    Re-format to conform to `SGP` conventions
WIDA_GA_Data_LONG_2022[, GRADE := as.character(as.numeric(GRADE))]
WIDA_GA_Data_LONG_2022[, SCALE_SCORE := as.numeric(SCALE_SCORE)]
WIDA_GA_Data_LONG_2022[, SCALE_SCORE_CSEM := as.numeric(SCALE_SCORE_CSEM)]

##    Create variables not included in data
WIDA_GA_Data_LONG_2022[, CONTENT_AREA := "READING"]

WIDA_GA_Data_LONG_2022 <-
    SGP:::getAchievementLevel(WIDA_GA_Data_LONG_2022, state = "WIDA_GA")
WIDA_GA_Data_LONG_2022[is.na(ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL := "NO SCORE"]
# table(WIDA_GA_Data_LONG_2022[, .(ACHIEVEMENT_LEVEL, VALID_CASE),
#                                  ACHIEVEMENT_LEVEL_ORIGINAL], exclude = NULL)

##    Format `NATIVE_LANGUAGE` to match previous data
WIDA_GA_Data_LONG_2022[!grepl("[[:alpha:]]", NATIVE_LANGUAGE),
    NATIVE_LANGUAGE := as.character(as.numeric(NATIVE_LANGUAGE))]

##    Add leading 0 for SCHOOL_NUMBER with 3 digits
WIDA_GA_Data_LONG_2022[, ID_CHECK := nchar(SCHOOL_NUMBER)]
WIDA_GA_Data_LONG_2022[ID_CHECK == 3,
                         SCHOOL_NUMBER := paste0("0", SCHOOL_NUMBER)]


###   Invalidate problematic cases

##    BAD IDs
WIDA_GA_Data_LONG_2022[, ID_CHECK := nchar(ID)]
# table(WIDA_GA_Data_LONG_2022[ID_CHECK < 9, ID])
WIDA_GA_Data_LONG_2022[ID_CHECK < 10, VALID_CASE := "INVALID_CASE"]

WIDA_GA_Data_LONG_2022[, ID_CHECK := NULL]

##    Duplicates
setkey(WIDA_GA_Data_LONG_2022,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(WIDA_GA_Data_LONG_2022,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
WIDA_GA_Data_LONG_2022[which(duplicated(WIDA_GA_Data_LONG_2022,
    by = key(WIDA_GA_Data_LONG_2022))) - 1, VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_Data_LONG_2022, VALID_CASE, CONTENT_AREA, YEAR, ID)


### Save data
save(WIDA_GA_Data_LONG_2022, file = "Data/WIDA_GA_Data_LONG_2022.Rdata")
