#+ include = FALSE, purl = FALSE
###############################################################################
###                                                                         ###
###              Data preparation script for 2023 WIDA GA data              ###
###                                                                         ###
###############################################################################

#' ## Data Preparation
#'
#' The data preparation step involves taking data provided by the GaDOE and
#' producing a `.Rdata` file that will subsequently be analyzed using the `SGP`
#' software. This process is carried out annually as new data becomes available
#' from the WIDA ACCESS for ELLs assessment program.
#'
#' For the 2023 Georgia WIDA ACCESS for ELLs data preparation and cleaning, we
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

###   Read in 2023 base data
WIDA_GA_Data_LONG_2023 <-
  fread(file = "Data/Base_Files/ACCESS_2023_cleaned_NCIEA.csv",
        colClasses = rep("character", 74), na.strings = NULL)

###   Clean Up Data
wida.ga.names <-
    c("VALID_CASE", "GTID", "SCHOOL_YEAR", "Grade",
      "ScaleScoreOverall", "CSEMOverall", "ProficiencyLevelOverall",
      "DistrictNumber", "DistrictName", "SchoolNumber", "SchoolName",
      "StudentLastName", "StudentFirstName", "BirthDate", "NativeLanguage",
      "Gender", "IEPStatus", "TitleIIIStatus", "LengthofTimeinLEPELLProgram"
    )

##    Subset data
WIDA_GA_Data_LONG_2023 <- subset(WIDA_GA_Data_LONG_2023, select = wida.ga.names)

##    Re-name variables to conform to `SGP` conventions
setnames(
    WIDA_GA_Data_LONG_2023,
    c("VALID_CASE", "ID", "YEAR", "GRADE",
      "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL_ORIGINAL",
      "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
      "LAST_NAME", "FIRST_NAME", "BIRTH_DATE", "NATIVE_LANGUAGE",
      "GENDER", "IEP_STATUS", "TITLE_III_STATUS", "TIME_IN_ELL_PROGRAM"
    )
)

##    Re-format to conform to `SGP` conventions
WIDA_GA_Data_LONG_2023[, 
    GRADE := as.character(as.numeric(GRADE))
][, SCALE_SCORE := as.numeric(SCALE_SCORE)
][, SCALE_SCORE_CSEM := as.numeric(SCALE_SCORE_CSEM)
##    Create variables not included in data
][, CONTENT_AREA := "READING"]

WIDA_GA_Data_LONG_2023 <-
    SGP:::getAchievementLevel(WIDA_GA_Data_LONG_2023, state = "WIDA_GA")
WIDA_GA_Data_LONG_2023[
    is.na(ACHIEVEMENT_LEVEL),
    ACHIEVEMENT_LEVEL := "NO SCORE"
]
# table(WIDA_GA_Data_LONG_2023[,
#     .(ACHIEVEMENT_LEVEL, VALID_CASE),
#     ACHIEVEMENT_LEVEL_ORIGINAL
# ], exclude = NULL)

##    Format `NATIVE_LANGUAGE` to match previous data
WIDA_GA_Data_LONG_2023[
    !grepl("[[:alpha:]]", NATIVE_LANGUAGE),
    NATIVE_LANGUAGE := as.character(as.numeric(NATIVE_LANGUAGE))
]

##    Check tables with Adrienne's ('NCIEA_Check_Data_Import_2023.docx')
# table(WIDA_GA_Data_LONG_2023[, VALID_CASE])
# # WIDA_GA_Data_LONG_2023[, VALID_CASE, sort(as.numeric(GRADE))] |>
# WIDA_GA_Data_LONG_2023[, VALID_CASE, ACHIEVEMENT_LEVEL_ORIGINAL] |>
#     table() |> prop.table() |> round(3)*100


##    Add leading 0 for SCHOOL_NUMBER with 3 digits
WIDA_GA_Data_LONG_2023[,
    ID_CHECK := nchar(SCHOOL_NUMBER)
][ID_CHECK == 3,
    SCHOOL_NUMBER := paste0("0", SCHOOL_NUMBER)
###   Invalidate problematic cases (BAD IDs)
#     table(WIDA_GA_Data_LONG_2023[ID_CHECK < 9, ID])
][, ID_CHECK := nchar(ID)
][ID_CHECK < 10,
    VALID_CASE := "INVALID_CASE" # 57 invalid cases added in 2023
][, ID_CHECK := NULL
]


##    Duplicates
setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE
)
setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE
)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2023,
        by = key(WIDA_GA_Data_LONG_2023)
    )
WIDA_GA_Data_LONG_2023[
    which(duplicated(
        WIDA_GA_Data_LONG_2023,
        by = key(WIDA_GA_Data_LONG_2023)
    )) - 1, # Invalidate lowest score (-1 index)
    VALID_CASE := "INVALID_CASE" # 6 invalid cases added in 2023
]

setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, BIRTH_DATE, SCALE_SCORE
)
setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, BIRTH_DATE
)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2023,
        by = key(WIDA_GA_Data_LONG_2023)
    )
sum(dupl) # 61 dups without `BIRTH_DATE`, 43 with it in
WIDA_GA_Data_LONG_2023[
    c(which(dupl), which(dupl) - 1),
][VALID_CASE == "VALID_CASE"] |> setkeyv(key(WIDA_GA_Data_LONG_2023)) |> print() # 4 students (most switched schools)
WIDA_GA_Data_LONG_2023[
    which(dupl) - 1, # Invalidate lowest score (-1 index)
    VALID_CASE := "INVALID_CASE"
]

setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, BIRTH_DATE, SCALE_SCORE
)
setkey(
    WIDA_GA_Data_LONG_2023,
    VALID_CASE, CONTENT_AREA, YEAR, ID, BIRTH_DATE
)
dupl <-
    duplicated(
        WIDA_GA_Data_LONG_2023,
        by = key(WIDA_GA_Data_LONG_2023)
    )
sum(dupl) # 61 dups without `BIRTH_DATE`, 43 with it in
WIDA_GA_Data_LONG_2023[
    which(dupl) - 1, # Invalidate lowest score (-1 index)
    VALID_CASE := "INVALID_CASE"
]
setkey(WIDA_GA_Data_LONG_2023, VALID_CASE, CONTENT_AREA, YEAR, ID)


### Save data
save(WIDA_GA_Data_LONG_2023, file = "Data/WIDA_GA_Data_LONG_2023.Rdata")
