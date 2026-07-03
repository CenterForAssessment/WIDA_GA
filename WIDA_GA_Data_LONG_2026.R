#+ include = FALSE, purl = FALSE
###############################################################################
###                                                                         ###
###              Data preparation script for 2026 WIDA GA data              ###
###                                                                         ###
###############################################################################

#' ## Data Preparation
#'
#' The data preparation step involves taking data provided by the GaDOE and
#' producing a `.Rdata` file that will subsequently be analyzed using the `SGP`
#' software. This process is carried out annually as new data becomes available
#' from the WIDA ACCESS for ELLs assessment program.
#'
#' For the 2026 Georgia WIDA ACCESS for ELLs data preparation and cleaning, we
#' first subset the raw data to include only the variables that are relevant to
#' the SGP analyses. These variables were then renamed to conform to the `SGP`
#' package conventions.
#'
#' Invalid records were identified based on the following criteria:
#'
#' * Cases with incorrect student ID characteristics (e.g., fewer than 10 characters)
#' * Students with duplicate records. In these instances, a student's highest
#'   scale score is retained as the "valid" case in the analyses.

#+ include = FALSE, purl = FALSE, eval = FALSE

### Load Packages
require(SGP)
require(data.table)

###   Read in 2026 base data
WIDA_GA_Data_LONG_2026 <-
    fread(file = "Data/Base_Files/ACCESS_2026_DataForSGP_cleaned-prepped.csv",
        colClasses = rep("character", 69), na.strings = NULL)

###   Clean Up Data
wida.ga.names <-
    c("VALID_CASE", "GTID", "SCHOOL_YEAR", "Grade",
      "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
      "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
      "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
      "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram"
    )

##    Subset data
WIDA_GA_Data_LONG_2026 <- subset(WIDA_GA_Data_LONG_2026, select = wida.ga.names)

##    Re-name variables to conform to `SGP` conventions
setnames(
    WIDA_GA_Data_LONG_2026,
    c("VALID_CASE", "ID", "YEAR", "GRADE",
      "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
      "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
      "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
      "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"
    )
)

##    Re-format to conform to `SGP` conventions
WIDA_GA_Data_LONG_2026[,
    GRADE := as.character(as.numeric(GRADE))
][, SCALE_SCORE := as.numeric(SCALE_SCORE)
][, SCALE_SCORE_CSEM := as.numeric(SCALE_SCORE_CSEM)
##    Create variables not included in data
][, CONTENT_AREA := "READING"
]

# WIDA_GA_Data_LONG_2026 <-
#     SGP:::getAchievementLevel(
#         WIDA_GA_Data_LONG_2026,
#         state = "WIDA_GA"
#     )[ is.na(ACHIEVEMENT_LEVEL),
#         ACHIEVEMENT_LEVEL := "NO SCORE"]

WIDA_GA_Data_LONG_2026[,
    ACHIEVEMENT_LEVEL := fcase(
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(1, 1.9, by = 0.1)), "Level 1",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(2, 2.9, by = 0.1)), "Level 2",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(3, 3.9, by = 0.1)), "Level 3",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(4, 4.2, by = 0.1)), "Level 4",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(4.3, 4.9, by = 0.1)), "Level 4.3",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% as.character(seq(5, 5.9, by = 0.1)), "Level 5",
        ACHIEVEMENT_LEVEL_ORIGINAL %in% "6", "Level 6")
][is.na(ACHIEVEMENT_LEVEL),
    ACHIEVEMENT_LEVEL := "NO SCORE"
]
# table(WIDA_GA_Data_LONG_2026[,
#     .(ACHIEVEMENT_LEVEL, VALID_CASE),
#     ACHIEVEMENT_LEVEL_ORIGINAL
# ], exclude = NULL)

##    Format `NATIVE_LANGUAGE` to match previous data
WIDA_GA_Data_LONG_2026[
    !grepl("[[:alpha:]]", NATIVE_LANGUAGE),
    NATIVE_LANGUAGE := as.character(as.numeric(NATIVE_LANGUAGE))
]

##    Check tables with Elena's ('NCIEA_Check_Data_Import_2026.docx')
# table(WIDA_GA_Data_LONG_2026[, VALID_CASE])
# WIDA_GA_Data_LONG_2026[, VALID_CASE, GRADE] |>
# # WIDA_GA_Data_LONG_2026[, VALID_CASE, ACHIEVEMENT_LEVEL_ORIGINAL] |>
#     table() |> prop.table() |> round(3)*100


##    Add leading 0 for SCHOOL_NUMBER with 3 digits
WIDA_GA_Data_LONG_2026[,
    ID_CHECK := nchar(SCHOOL_NUMBER)
][ID_CHECK == 3,
    SCHOOL_NUMBER := paste0("0", SCHOOL_NUMBER)
###   Invalidate problematic cases (BAD IDs)
#     table(WIDA_GA_Data_LONG_2026[ID_CHECK < 10, ID])
][, ID_CHECK := nchar(ID)
][ID_CHECK < 10,
    VALID_CASE := "INVALID_CASE" # 26 invalid case added in '26 (1,506 total invalids)
][, ID_CHECK := NULL
]


##    Duplicates
WIDA_GA_Data_LONG_2026 |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE) |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2026,
        by = key(WIDA_GA_Data_LONG_2026)
    )
WIDA_GA_Data_LONG_2026[
    which(duplicated(
        WIDA_GA_Data_LONG_2026,
        by = key(WIDA_GA_Data_LONG_2026)
    )) - 1, # Invalidate lowest score (-1 index)
    VALID_CASE := "INVALID_CASE" # 0 invalid cases ADDED in '26
]

WIDA_GA_Data_LONG_2026 |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, BIRTH_DATE, SCALE_SCORE) |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, BIRTH_DATE)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2026,
        by = key(WIDA_GA_Data_LONG_2026)
    )
sum(dupl) # 26 dups - All invalid already invalid
table(WIDA_GA_Data_LONG_2026[, VALID_CASE] == "INVALID_CASE", dupl)
# WIDA_GA_Data_LONG_2026[
#     c(which(dupl), which(dupl) - 1),
# ][VALID_CASE == "VALID_CASE"] |> setkeyv(key(WIDA_GA_Data_LONG_2026)) |> print() # 0 students in '26 (previous years = switched schools)
# WIDA_GA_Data_LONG_2026[
#     which(dupl) - 1, # Invalidate lowest score (-1 index)
#     VALID_CASE := "INVALID_CASE"
# ]

WIDA_GA_Data_LONG_2026 |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, BIRTH_DATE, SCALE_SCORE) |>
    setkey(VALID_CASE, CONTENT_AREA, YEAR, ID, BIRTH_DATE)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2026,
        by = key(WIDA_GA_Data_LONG_2026)
    )
sum(dupl)  #  29 dups - All invalid already invalid
table(WIDA_GA_Data_LONG_2026[, VALID_CASE] == "INVALID_CASE", dupl)
# WIDA_GA_Data_LONG_2026[
#     which(dupl) - 1, # Invalidate lowest score (-1 index)
#     VALID_CASE := "INVALID_CASE"
# ]
setkey(WIDA_GA_Data_LONG_2026, VALID_CASE, CONTENT_AREA, YEAR, ID)


### Save data
save(WIDA_GA_Data_LONG_2026, file = "Data/WIDA_GA_Data_LONG_2026.Rdata")
