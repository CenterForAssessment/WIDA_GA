###############################################################################
###                                                                         ###
###          Script for creating 2021 ISRs for Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load SGP package
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_GA_SGP.Rdata")

###   Remove some duplicates (wrong GTID!)

setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
WIDA_GA_SGP@Data[which(duplicated(WIDA_GA_SGP@Data, by = key(WIDA_GA_SGP@Data)))-1, VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)

###   Modify data for ISR production

##    Clean DISTRICT_NUMBER
WIDA_GA_SGP@Data[, DISTRICT_NUMBER := gsub("GA0000", "", DISTRICT_NUMBER)]
WIDA_GA_SGP@Data[, DISTRICT_NUMBER := gsub("GA", "", DISTRICT_NUMBER)]

##    English version Ach Levs
WIDA_GA_SGP@Data$ACHIEVEMENT_LEVEL_ENGLISH <- as.character(NA)
WIDA_GA_SGP@Data[, ACHIEVEMENT_LEVEL_ENGLISH := fcase(
    ACHIEVEMENT_LEVEL == "Level 1", paste0("Entering-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 2", paste0("Emerging-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 3", paste0("Developing-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL %in% c("Level 4", "Level 4.3"), paste0("Expanding-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 5", paste0("Bridging-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 6", paste0("Reaching-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "NO SCORE", "No Score")]

WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_ENGLISH) | ACHIEVEMENT_LEVEL_ENGLISH == "No Score"),
    ACHIEVEMENT_LEVEL_ENGLISH := paste0(ACHIEVEMENT_LEVEL_ENGLISH, ".0")]

##    Spanish version Ach Levs
WIDA_GA_SGP@Data$ACHIEVEMENT_LEVEL_SPANISH <- as.character(NA)
WIDA_GA_SGP@Data[, ACHIEVEMENT_LEVEL_SPANISH := fcase(
    ACHIEVEMENT_LEVEL == "Level 1", paste0("Entrada-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 2", paste0("Emergente-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 3", paste0("Desarrollo-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL %in% c("Level 4", "Level 4.3"), paste0("Extensi\u{F3}n-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 5", paste0("Transformaci\u{F3}n-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "Level 6", paste0("Trascendencia-", ACHIEVEMENT_LEVEL_ORIGINAL),
    ACHIEVEMENT_LEVEL == "NO SCORE", "Sin Escala")]

WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_SPANISH) | ACHIEVEMENT_LEVEL_SPANISH == "Sin Escala"),
    ACHIEVEMENT_LEVEL_SPANISH := paste0(ACHIEVEMENT_LEVEL_SPANISH, ".0")]


###   Create "catch-all" directory for school-level catalogs

if (!dir.exists("Visualizations/studentGrowthPlots/School/2021/All_Schools"))
     dir.create("Visualizations/studentGrowthPlots/School/2021/All_Schools", recursive = TRUE)


#####
###   English Version
#####

SGPstateData[["WIDA_GA"]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]] <- NULL

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ENGLISH"),
         c("ACH_LEV_ACTUAL", "ACHIEVEMENT_LEVEL"))

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2021",
    sgPlot.content_areas = "READING",
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile="Visualizations/studentGrowthPlots/School/2021/All_Schools_English.zip",
    files="Visualizations/studentGrowthPlots/School/2021/All_Schools/")


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
    sgPlot.years = "2021",
    sgPlot.content_areas = "READING",
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile="Visualizations/studentGrowthPlots/School/2021/All_Schools_Spanish.zip",
    files="Visualizations/studentGrowthPlots/School/2021/All_Schools/", flags="-rmq1")

###   Remove district and school "shell" directories that are now empty
shells <- list.files("Visualizations/studentGrowthPlots/School/2021/",
                     pattern = "[[:digit:]]", include.dirs=TRUE, full.names=TRUE)
unlink(shells, recursive = TRUE)
