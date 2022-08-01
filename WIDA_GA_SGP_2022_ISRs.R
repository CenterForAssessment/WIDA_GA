###############################################################################
###                                                                         ###
###          Script for creating 2022 ISRs for Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load SGP package
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_GA_SGP.Rdata")

###   Define 2022 ISR directory
isr_dir_22 <- "Visualizations/studentGrowthPlots/School/2022"

###   Remove some duplicates (wrong GTID!)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
WIDA_GA_SGP@Data[
    which(duplicated(WIDA_GA_SGP@Data, by = key(WIDA_GA_SGP@Data))) - 1,
        VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)

###   Modify data for ISR production
WIDA_GA_SGP@Data <- WIDA_GA_SGP@Data[VALID_CASE == "VALID_CASE", ]

###   Only produce ISRs for students with SGPs
isr.ids <- WIDA_GA_SGP@Data[YEAR == "2022" & (!is.na(SGP) | GRADE == "0"), ID]
WIDA_GA_SGP@Data <- WIDA_GA_SGP@Data[ID %in% isr.ids,]

##    Clean DISTRICT_NUMBER
WIDA_GA_SGP@Data[, DISTRICT_NUMBER := gsub("GA0000", "", DISTRICT_NUMBER)]
WIDA_GA_SGP@Data[, DISTRICT_NUMBER := gsub("GA", "", DISTRICT_NUMBER)]

##    English version Ach Levs
WIDA_GA_SGP@Data$ACHIEVEMENT_LEVEL_ENGLISH <- as.character(NA)
WIDA_GA_SGP@Data[, ACHIEVEMENT_LEVEL_ENGLISH := fcase(
    ACHIEVEMENT_LEVEL == "Level 1",
        paste0("Entering-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 2",
        paste0("Emerging-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 3",
        paste0("Developing-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL %in% c("Level 4", "Level 4.3"),
        paste0("Expanding-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 5",
        paste0("Bridging-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 6",
        paste0("Reaching-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "NO SCORE", "No Score")]

WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_ENGLISH) |
                   ACHIEVEMENT_LEVEL_ENGLISH == "No Score"),
                       ACHIEVEMENT_LEVEL_ENGLISH :=
                           paste0(ACHIEVEMENT_LEVEL_ENGLISH, ".0")]

##    Spanish version Ach Levs
WIDA_GA_SGP@Data$ACHIEVEMENT_LEVEL_SPANISH <- as.character(NA)
WIDA_GA_SGP@Data[, ACHIEVEMENT_LEVEL_SPANISH := fcase(
    ACHIEVEMENT_LEVEL == "Level 1",
        paste0("Entrada-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 2",
        paste0("Emergente-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 3",
        paste0("Desarrollo-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL %in% c("Level 4", "Level 4.3"),
        paste0("Extensi\u{F3}n-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 5",
        paste0("Transformaci\u{F3}n-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 6",
        paste0("Trascendencia-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "NO SCORE", "Sin Escala")]

WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_SPANISH) |
                   ACHIEVEMENT_LEVEL_SPANISH == "Sin Escala"),
                       ACHIEVEMENT_LEVEL_SPANISH :=
                           paste0(ACHIEVEMENT_LEVEL_SPANISH, ".0")]


###   Create "catch-all" directory for school-level catalogs

all_schools <- file.path(isr_dir_22, "All_Schools")
if (!dir.exists(all_schools)) dir.create(all_schools, recursive = TRUE)


#####
###   English Version
#####

SGPstateData[["WIDA_GA"]][["Assessment_Program_Information"]][[
    "Assessment_Abbreviation"]] <- NULL

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ENGLISH"),
         c("ACH_LEV_ACTUAL", "ACHIEVEMENT_LEVEL"))

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2022",
    # sgPlot.schools = c("0604", "0388", "1114", "0625", "0618", "0303", "0294"),
    sgPlot.demo.report = TRUE,
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile = file.path(isr_dir_22, "All_Schools_English.zip"),
    files = file.path(isr_dir_22, "All_Schools/"), flags = "-rmqj1")


#####
###   Spanish Version
#####

SGPstateData[["WIDA_GA"]] <- SGPstateData[["WIDA_GA_SPANISH"]]

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_SPANISH"),
         c("ACHIEVEMENT_LEVEL_ENGLISH", "ACHIEVEMENT_LEVEL"))

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2022",
    sgPlot.schools = c("0604", "0388", "1114", "0625", "0618", "0303", "0294"),
    # sgPlot.demo.report = TRUE,
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile = file.path(isr_dir_22, "All_Schools_Spanish.zip"),
    files = file.path(isr_dir_22, "All_Schools/"), flags = "-rmqj1")


###   Remove district and school "shell" directories that are now empty

unlink(list.files(isr_dir_22, pattern = "[[:digit:]]",
                  include.dirs = TRUE, full.names = TRUE),
       recursive = TRUE)


###  Growth/Achievement and Bubble Plots

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = c("growthAchievementPlot"))#, # "bubblePlot", need to run summarizeSGP first
#     bPlot.output = c("PDF", "SVG"),
#     bPlot.minimum.n = 15,
#     bPlot.anonymize = TRUE,
#     parallel.config = list(
#         BACKEND = "PARALLEL",
#         WORKERS = list(GA_PLOTS = 15)
#     )
# )
