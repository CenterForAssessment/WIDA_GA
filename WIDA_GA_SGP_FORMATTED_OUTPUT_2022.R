###############################################################################
###                                                                         ###
###          Formatted text output for WIDA_GA from 2022 long data          ###
###                                                                         ###
###############################################################################

require(data.table)
require(cfaTools)

###   Load 2022 Data
load("Data/WIDA_GA_SGP.Rdata")
load("Data/WIDA_GA_SGP_LONG_Data.Rdata")

###   Check SGP_LEVEL
# WIDA_GA_SGP_LONG_Data[!is.na(SGP),
#                           .(MIN = min(SGP), MAX = max(SGP)),
#                       keyby = c("GRADE", "SGP_LEVEL")]
# WIDA_GA_SGP_LONG_Data[!is.na(SGP_BASELINE),
#                           .(MIN = min(SGP_BASELINE), MAX = max(SGP_BASELINE)),
#                       keyby = c("GRADE", "SGP_LEVEL_BASELINE")]

###   Remove level 4.3 from current and lagged variables
WIDA_GA_SGP_LONG_Data[ACHIEVEMENT_LEVEL == "Level 4.3",
                      ACHIEVEMENT_LEVEL := "Level 4"]
WIDA_GA_SGP_LONG_Data[ACHIEVEMENT_LEVEL_PRIOR == "Level 4.3",
                      ACHIEVEMENT_LEVEL_PRIOR := "Level 4"]
# table(WIDA_GA_SGP_LONG_Data[, ACHIEVEMENT_LEVEL, ACHIEVEMENT_LEVEL_ORIGINAL])

###   Remove "BASELINE" Prior Scale Scores (redundant)
# table(WIDA_GA_SGP_LONG_Data[,
#     SCALE_SCORE_PRIOR == SCALE_SCORE_PRIOR_BASELINE], exclude = NULL)
# table(WIDA_GA_SGP_LONG_Data[,
#     SCALE_SCORE_PRIOR_STANDARDIZED == SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE],
#     exclude = NULL)
WIDA_GA_SGP_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE := NULL]
WIDA_GA_SGP_LONG_Data[, SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE := NULL]


###   Create requested lagged variables

##    Change VALID_CASE to allow for correct lagging
##    Save original in separate variable.
WIDA_GA_SGP_LONG_Data[, VC := VALID_CASE]

##    Check for duplicates within GRADE
##    Take highest grade level/score within grade
setkey(WIDA_GA_SGP_LONG_Data,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(WIDA_GA_SGP_LONG_Data,
    VALID_CASE, CONTENT_AREA, YEAR, ID)
dups <- duplicated(WIDA_GA_SGP_LONG_Data, by = key(WIDA_GA_SGP_LONG_Data))

WIDA_GA_SGP_LONG_Data[which(dups) - 1, VALID_CASE := "INVALID_CASE"]

TMP_INVALID_CASES <- WIDA_GA_SGP_LONG_Data[VALID_CASE == "INVALID_CASE"]
WIDA_GA_SGP_LONG_Data <- WIDA_GA_SGP_LONG_Data[VALID_CASE == "VALID_CASE"]

##    Use `getShiftedValues` to lag current values
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(WIDA_GA_SGP_LONG_Data, shift.key)

getShiftedValues(WIDA_GA_SGP_LONG_Data, shift_amount = c(1L, 2L),
    shift_variable = c("YEAR", "GRADE", "ACHIEVEMENT_LEVEL_ORIGINAL"))

##    Clean up - rename according to old conventions
setnames(WIDA_GA_SGP_LONG_Data,
         gsub("LAG_1", "PRIOR_1_YEAR", names(WIDA_GA_SGP_LONG_Data)))
setnames(WIDA_GA_SGP_LONG_Data,
         gsub("LAG_2", "PRIOR_2_YEAR", names(WIDA_GA_SGP_LONG_Data)))
table(WIDA_GA_SGP_LONG_Data[YEAR == "2022",
          ACHIEVEMENT_LEVEL_PRIOR, ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR_1_YEAR],
      exclude = NULL)

##    Re-combine VALID/INVALID data and replace original VALID_CASE
WIDA_GA_SGP_LONG_Data <-
    rbindlist(list(WIDA_GA_SGP_LONG_Data, TMP_INVALID_CASES), fill = TRUE)
WIDA_GA_SGP_LONG_Data[, VALID_CASE := VC]
WIDA_GA_SGP_LONG_Data[, VC := NULL]

table(WIDA_GA_SGP_LONG_Data[,
        YEAR, is.na(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR_1_YEAR)], exclude = NULL)
table(WIDA_GA_SGP_LONG_Data[,
        GRADE, is.na(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR_1_YEAR)], exclude = NULL)

##    Remove 1_YEAR priors for Grade 0 - repeaters
WIDA_GA_SGP_LONG_Data[GRADE == 0 | YEAR == "2019",
    ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR_1_YEAR := NA]


###   Add in CURRENT Projections

proj.var.names <- c("ID", "GRADE",
                    "LEVEL_4_SGP_TARGET_YEAR_1_CURRENT",
                    "P1_PROJ_YEAR_1_CURRENT", "P35_PROJ_YEAR_1_CURRENT",
                    "P66_PROJ_YEAR_1_CURRENT", "P99_PROJ_YEAR_1_CURRENT")

my.projection.table.names <- "READING.2022"

tmp.list.current <- list()

for (i in my.projection.table.names) {
    tmp.list.current[[i]] <- data.table(
        VALID_CASE = "VALID_CASE",
        YEAR = unlist(strsplit(i, "\\."))[2],
        WIDA_GA_SGP@SGP$SGProjections[[i]][, proj.var.names, with = FALSE])
}

###  Merge projection/target data in.
tmp.projections.c <-
    data.table(rbindlist(tmp.list.current), key = c("ID", "GRADE"))

pctl.names <- names(WIDA_GA_SGP_LONG_Data)
pjct.names <- names(tmp.projections.c)

setkeyv(tmp.projections.c, c("VALID_CASE", "YEAR", "ID", "GRADE"))
setkeyv(WIDA_GA_SGP_LONG_Data, c("VALID_CASE", "YEAR", "ID", "GRADE"))

WIDA_GA_SGP_LONG_Data <- tmp.projections.c[WIDA_GA_SGP_LONG_Data]

###  Final arrangement of variables

setcolorder(WIDA_GA_SGP_LONG_Data, union(pctl.names, pjct.names))
setkeyv(WIDA_GA_SGP_LONG_Data, c("VALID_CASE", "YEAR", "GRADE", "ID"))


###   Clean up names
wida.ga.names <- c(
        "VALID_CASE", "CONTENT_AREA", "GTID", "SCHOOL_YEAR", "Grade",
        "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
        "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
        "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
        "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram")

setnames(WIDA_GA_SGP_LONG_Data,
         c("VALID_CASE", "CONTENT_AREA", "ID", "YEAR", "GRADE",
           "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
           "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
           "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
           "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"),
         wida.ga.names)

prior.cpl.names <- grep("ACHIEVEMENT_LEVEL_ORIGINAL",
                        names(WIDA_GA_SGP_LONG_Data), value = TRUE)
setnames(WIDA_GA_SGP_LONG_Data,
         c(prior.cpl.names, "YEAR_PRIOR_1_YEAR", "YEAR_PRIOR_2_YEAR",
           "GRADE_PRIOR_1_YEAR", "GRADE_PRIOR_2_YEAR"),
         c(gsub("ACHIEVEMENT_LEVEL_ORIGINAL",
                "CompositeOverallProficiencyLevel", prior.cpl.names),
           "SCHOOL_YEAR_PRIOR_1_YEAR", "SCHOOL_YEAR_PRIOR_2_YEAR",
           "Grade_PRIOR_1_YEAR", "Grade_PRIOR_2_YEAR"))

setnames(WIDA_GA_SGP_LONG_Data,
        "LEVEL_4_SGP_TARGET_YEAR_1_CURRENT",
        "MIN_EXIT_CRITERIA_SGP_TARGET_YEAR_1_CURRENT")


###   Save results

assign("WIDA_GA_SGP_LONG_Data_2022_FORMATTED",
        WIDA_GA_SGP_LONG_Data[SCHOOL_YEAR == "2022"])

fwrite(WIDA_GA_SGP_LONG_Data_2022_FORMATTED,
       file = "Data/WIDA_GA_SGP_LONG_Data_2022_FORMATTED.txt", sep = "|")

zip(zipfile = "Data/WIDA_GA_SGP_LONG_Data_2022_FORMATTED.txt.zip",
    files = "Data/WIDA_GA_SGP_LONG_Data_2022_FORMATTED.txt")

unlink("Data/WIDA_GA_SGP_LONG_Data_2022_FORMATTED.txt")
