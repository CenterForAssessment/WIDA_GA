###############################################################################
###                                                                         ###
###          Formatted text output for WIDA_GA from 2024 long data          ###
###                                                                         ###
###############################################################################

require(data.table)
require(SGP)

###   Load 2024 Data
load("Data/WIDA_GA_SGP_LONG_Data_2024.Rdata")
assign("WIDA_GA_RES", WIDA_GA_SGP_LONG_Data_2024)
load("Visualizations/studentGrowthPlots/WIDA_Ga_studentGrowthPlot_Data.Rdata")
assign("ISR_Data", WIDA_Ga_studentGrowthPlot_Data)

rm(list = c("WIDA_GA_SGP_LONG_Data_2024", "WIDA_Ga_studentGrowthPlot_Data"))
gc()

current.year <- "2024"
prior.year.1 <- "2023"
prior.year.2 <- "2022"

###   Check SGP_LEVEL
# WIDA_GA_RES[!is.na(SGP),
#         .(MIN = min(SGP), MAX = max(SGP)),
#     keyby = c("GRADE", "SGP_LEVEL")]
# WIDA_GA_RES[!is.na(SGP_BASELINE),
#         .(MIN = min(SGP_BASELINE), MAX = max(SGP_BASELINE)),
#     keyby = c("GRADE", "SGP_LEVEL_BASELINE")]

###   Remove level 4.3 from current and lagged variables
WIDA_GA_RES[
    ACHIEVEMENT_LEVEL == "Level 4.3",
    ACHIEVEMENT_LEVEL := "Level 4"
][
    ACHIEVEMENT_LEVEL_PRIOR == "Level 4.3",
    ACHIEVEMENT_LEVEL_PRIOR := "Level 4"
]

# table(WIDA_GA_RES[, ACHIEVEMENT_LEVEL, ACHIEVEMENT_LEVEL_ORIGINAL])

###   Remove "BASELINE" Prior Scale Scores (redundant)
WIDA_GA_RES[, SCALE_SCORE_PRIOR_BASELINE := NULL]


###   Merge requested lagged variables (from ISRs)
ISR_Data[,
    VALID_CASE := "VALID_CASE"
][, YEAR := current.year
][, SCHOOL_YEAR_PRIOR_1_YEAR := prior.year.1
][, SCHOOL_YEAR_PRIOR_2_YEAR := prior.year.2
][, grep(current.year, names(ISR_Data), value = TRUE) := NULL
][, grep("TRANSFORMED|_LABELS", names(ISR_Data), value = TRUE) := NULL
]

p1.vars <- grep(prior.year.1, names(ISR_Data), value = TRUE)
p2.vars <- grep(prior.year.2, names(ISR_Data), value = TRUE)
setnames(
    ISR_Data,
    c(p1.vars, p2.vars),
    c(gsub(paste0("[.]", prior.year.1), "_PRIOR_1_YEAR", p1.vars),
      gsub(paste0("[.]", prior.year.2), "_PRIOR_2_YEAR", p2.vars))
)
setcolorder(ISR_Data, key(ISR_Data))
setkeyv(WIDA_GA_RES, SGP:::getKey(WIDA_GA_RES))
setkeyv(ISR_Data, SGP:::getKey(ISR_Data))
res.names <- names(WIDA_GA_RES)
isr.names <- names(ISR_Data)

WIDA_GA_RES <- ISR_Data[WIDA_GA_RES]

###  Final arrangement of variables
setcolorder(WIDA_GA_RES, union(res.names, isr.names))
setkeyv(WIDA_GA_RES, c("VALID_CASE", "YEAR", "GRADE", "ID"))


###   Clean up names
wida.ga.names <-
    c("VALID_CASE", "CONTENT_AREA", "GTID", "SCHOOL_YEAR", "Grade",
      "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
      "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
      "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
      "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram",
      "Grade_PRIOR_1_YEAR", "Grade_PRIOR_2_YEAR",
      "CompositeOverallProficiencyLevel_PRIOR_1_YEAR",
      "CompositeOverallProficiencyLevel_PRIOR_2_YEAR")

setnames(
    WIDA_GA_RES,
    c("VALID_CASE", "CONTENT_AREA", "ID", "YEAR", "GRADE",
      "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
      "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
      "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
      "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM",
      "GRADE_PRIOR_1_YEAR", "GRADE_PRIOR_2_YEAR",
      "ACHIEVEMENT_LEVEL_PRIOR_1_YEAR", "ACHIEVEMENT_LEVEL_PRIOR_2_YEAR"),
    wida.ga.names
)

# NOTE: `MIN_EXIT_CRITERIA_SGP_TARGET_YEAR_1_CURRENT` is identical to `SGP_TARGET_1_YEAR_CURRENT`
WIDA_GA_RES[, MIN_EXIT_CRITERIA_SGP_TARGET_YEAR_1_CURRENT := SGP_TARGET_1_YEAR_CURRENT]

###   Save results
fwrite(
    WIDA_GA_RES,
    file = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt",
    sep = "|"
)
zip(zipfile = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt.zip",
    files = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt",
    flags = "-mqj"
)
unlink("Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt")
