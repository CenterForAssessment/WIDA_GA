###############################################################################
###                                                                         ###
###          Formatted text output for WIDA_GA from 2024 long data          ###
###                                                                         ###
###############################################################################

require(data.table)
require(SGP)

###   Load 2024 Data
load("Data/WIDA_GA_SGP.Rdata")
load("Data/WIDA_GA_SGP_LONG_Data_2024.Rdata")
assign("WIDA_GA_2024", WIDA_GA_SGP_LONG_Data_2024)

###   Check SGP_LEVEL
WIDA_GA_2024[!is.na(SGP),
        .(MIN = min(SGP), MAX = max(SGP)),
    keyby = c("GRADE", "SGP_LEVEL")]
WIDA_GA_2024[!is.na(SGP_BASELINE),
        .(MIN = min(SGP_BASELINE), MAX = max(SGP_BASELINE)),
    keyby = c("GRADE", "SGP_LEVEL_BASELINE")]

###   Remove level 4.3 from current and lagged variables
WIDA_GA_2024[
    ACHIEVEMENT_LEVEL == "Level 4.3",
    ACHIEVEMENT_LEVEL := "Level 4"]

# table(WIDA_GA_2024[, ACHIEVEMENT_LEVEL, ACHIEVEMENT_LEVEL_ORIGINAL])

###   Remove "BASELINE" Prior Scale Scores (redundant)
# table(WIDA_GA_2024[,
#     SCALE_SCORE_PRIOR == SCALE_SCORE_PRIOR_BASELINE], exclude = NULL)
# table(WIDA_GA_2024[,
#     SCALE_SCORE_PRIOR_STANDARDIZED == SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE],
#     exclude = NULL)
WIDA_GA_2024[, SCALE_SCORE_PRIOR_BASELINE := NULL]
# ][, SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE := NULL]


###   Create requested lagged variables
WIDA_GA_2024[,
    SCHOOL_YEAR_PRIOR_1_YEAR := ifelse(is.na(SGP), NA, "2023")
][, SCHOOL_YEAR_PRIOR_2_YEAR := ifelse(SGP_ORDER == 2, "2022", NA)
]

WIDA_GA_2024[,
    GRADE := as.integer(GRADE)
][, Grade_PRIOR_1_YEAR := ifelse(!is.na(SGP), as.character(GRADE - 1), NA)
][, Grade_PRIOR_2_YEAR := ifelse(SGP_ORDER == 2, as.character(GRADE - 2), NA)
][, GRADE := as.character(GRADE)
]

###   Create `PRIOR*` achievement variables
##    First split SGP_NORM_GROUP_SCALE_SCORES
score.split <-
    WIDA_GA_2024$SGP_NORM_GROUP_SCALE_SCORES |>
      as.character() |> strsplit("; ")

WIDA_GA_2024[,
    PRIOR_1_SS := as.numeric(sapply(score.split, \(x) rev(x)[2]))
][, PRIOR_2_SS := as.numeric(sapply(score.split, \(x) rev(x)[3]))
]

##    Use function embedded in `SGPstateData` to calculate achievement levels
fullProfLevWIDA <-
    SGPstateData[["WIDA"]][["SGP_Configuration"]][["ss_to_pl_function"]][["value"]]

WIDA_GA_2024[,
    CompositeOverallProficiencyLevel_PRIOR_1_YEAR :=
        fullProfLevWIDA(grade = Grade_PRIOR_1_YEAR, scale_score = PRIOR_1_SS)
][, CompositeOverallProficiencyLevel_PRIOR_2_YEAR :=
        fullProfLevWIDA(grade = Grade_PRIOR_2_YEAR, scale_score = PRIOR_2_SS)
]
# WIDA_GA_2024[YEAR == "2024",
#     ACHIEVEMENT_LEVEL_PRIOR, CompositeOverallProficiencyLevel_PRIOR_1_YEAR
# ] |> table(exclude = NULL)
WIDA_GA_2024[, c("PRIOR_1_SS", "PRIOR_2_SS") := NULL]


###   Add in CURRENT Projections
proj.var.names <-
    c("ID", "GRADE", "LEVEL_4_SGP_TARGET_YEAR_1_CURRENT",
      "P1_PROJ_YEAR_1_CURRENT", "P35_PROJ_YEAR_1_CURRENT",
      "P66_PROJ_YEAR_1_CURRENT", "P99_PROJ_YEAR_1_CURRENT")

my.projection.table.names <- "READING.2024"

tmp.list.current <- list()

for (i in my.projection.table.names) {
    tmp.list.current[[i]] <-
        data.table(
            VALID_CASE = "VALID_CASE",
            YEAR = unlist(strsplit(i, "\\."))[2],
            WIDA_GA_SGP@SGP$SGProjections[[i]][, proj.var.names, with = FALSE]
        )
}

###  Merge projection/target data in.
tmp.projections.c <-
    data.table::data.table(rbindlist(tmp.list.current), key = c("ID", "GRADE"))

pctl.names <- names(WIDA_GA_2024)
pjct.names <- names(tmp.projections.c)

setkeyv(tmp.projections.c, c("VALID_CASE", "YEAR", "ID", "GRADE"))
setkeyv(WIDA_GA_2024, c("VALID_CASE", "YEAR", "ID", "GRADE"))

WIDA_GA_2024 <- tmp.projections.c[WIDA_GA_2024]

###  Final arrangement of variables

setcolorder(WIDA_GA_2024, union(pctl.names, pjct.names))
setkeyv(WIDA_GA_2024, c("VALID_CASE", "YEAR", "GRADE", "ID"))


###   Clean up names
wida.ga.names <-
    c("VALID_CASE", "CONTENT_AREA", "GTID", "SCHOOL_YEAR", "Grade",
      "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
      "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
      "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
      "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram")

setnames(
    WIDA_GA_2024,
    c("VALID_CASE", "CONTENT_AREA", "ID", "YEAR", "GRADE",
      "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
      "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
      "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
      "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"),
    wida.ga.names
)
setnames(
    WIDA_GA_2024,
    "LEVEL_4_SGP_TARGET_YEAR_1_CURRENT",
    "MIN_EXIT_CRITERIA_SGP_TARGET_YEAR_1_CURRENT"
)

###   Save results

fwrite(
    WIDA_GA_2024,
    file = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt",
    sep = "|"
)

zip(zipfile = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt.zip",
    files = "Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt")
unlink("Data/WIDA_GA_SGP_LONG_Data_2024_FORMATTED.txt")
