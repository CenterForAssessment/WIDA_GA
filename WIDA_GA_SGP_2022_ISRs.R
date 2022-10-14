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

###   Modify data for ISR production
##    Only produce ISRs for students with SGPs
isr.ids <-
    WIDA_GA_SGP@Data[YEAR == "2022" & !is.na(SGP), ID] # (!is.na(SGP) | GRADE == "0")
WIDA_GA_SGP@Data[!ID %in% isr.ids, VALID_CASE := "INVALID_CASE"]

###   Remove some duplicates (wrong GTID!)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
WIDA_GA_SGP@Data[
    which(duplicated(WIDA_GA_SGP@Data, by = key(WIDA_GA_SGP@Data))) - 1,
        VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)

###   Modify data for ISR production
WIDA_GA_SGP@Data <- WIDA_GA_SGP@Data[VALID_CASE == "VALID_CASE", ]

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
    ACHIEVEMENT_LEVEL == "NO SCORE", "No Score")
]
WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_ENGLISH) |
                   ACHIEVEMENT_LEVEL_ENGLISH == "No Score"),
                       ACHIEVEMENT_LEVEL_ENGLISH :=
                           paste0(ACHIEVEMENT_LEVEL_ENGLISH, ".0")
]

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
    ACHIEVEMENT_LEVEL == "NO SCORE", "Sin Escala")
]
WIDA_GA_SGP@Data[!(grepl("[.]", ACHIEVEMENT_LEVEL_SPANISH) |
                   ACHIEVEMENT_LEVEL_SPANISH == "Sin Escala"),
                       ACHIEVEMENT_LEVEL_SPANISH :=
                           paste0(ACHIEVEMENT_LEVEL_SPANISH, ".0")
]

###   Create "catch-all" directory for school-level catalogs
all_schools <- file.path(isr_dir_22, "All_Schools")
if (!dir.exists(all_schools))
    dir.create(all_schools, recursive = TRUE)


#####
###   English Version
#####

SGPstateData[["WIDA_GA"]][["Assessment_Program_Information"]][[
    "Assessment_Abbreviation"]] <- NULL

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ENGLISH"),
         c("ACH_LEV_ACTUAL", "ACHIEVEMENT_LEVEL")
)

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2022",
    sgPlot.produce.plots=FALSE, # Get wide data for 
    sgPlot.save.sgPlot.data = TRUE,
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile = file.path(isr_dir_22, "All_Schools_English.zip"),
    files = file.path(isr_dir_22, "All_Schools/"),
    flags = "-rmqj1"
)


#####
###   Spanish Version
#####

SGPstateData[["WIDA_GA"]] <- SGPstateData[["WIDA_GA_SPANISH"]]

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_SPANISH"),
         c("ACHIEVEMENT_LEVEL_ENGLISH", "ACHIEVEMENT_LEVEL")
)

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2022",
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 15)
    )
)

zip(zipfile = file.path(isr_dir_22, "All_Schools_Spanish.zip"),
    files = file.path(isr_dir_22, "All_Schools/"),
    flags = "-rmqj1"
)


###   Remove district and school "shell" directories that are now empty

unlink(
    list.files(
        isr_dir_22, pattern = "[[:digit:]]",
        include.dirs = TRUE, full.names = TRUE
    ),
    recursive = TRUE
)


#####
###   Output District Level Datasets
#####

require(openxlsx)

load("WIDA_Ga_studentGrowthPlot_Data.Rdata")
WIDA_GA_ISR_Data <- copy(WIDA_Ga_studentGrowthPlot_Data)

##    Remove redundant and unneeded variables
vars.to.remove <-
    c(grep("CONTENT_AREA", names(WIDA_GA_ISR_Data), value = TRUE),
      grep("TRANSFORMED", names(WIDA_GA_ISR_Data), value = TRUE),
      "GRADE", "GRADE.2019", "GRADE.2020",
      "SCALE_SCORE.2019", "SCALE_SCORE.2020",
      "ACHIEVEMENT_LEVEL.2019", "ACHIEVEMENT_LEVEL.2020",
      "SGP.2019", "SGP.2020", "SGP.2021",
      "SGP_LEVEL.2019", "SGP_LEVEL.2020", "SGP_LEVEL.2021",
      "P50_PROJ_YEAR_1_CURRENT",
      "P1_PROJ_YEAR_1_CURRENT", "P35_PROJ_YEAR_1_CURRENT", # might keep next year
      "P66_PROJ_YEAR_1_CURRENT", "P99_PROJ_YEAR_1_CURRENT" # might keep next year
    )

WIDA_GA_ISR_Data[, (vars.to.remove)  := NULL]
WIDA_GA_ISR_Data[, REPORTED_SCH_YEAR := "2022"]

##    Reorder and rename remaining variables
col.ord <-
    c("REPORTED_SCH_YEAR",
      "DISTRICT_NAME.2022", "DISTRICT_NUMBER.2022",
      "SCHOOL_NAME.2022", "SCHOOL_NUMBER.2022",
      "ID", "LAST_NAME.2022", "FIRST_NAME.2022", "BIRTH_DATE.2022",
      "GRADE.2021", "GRADE.2022",
      "SCALE_SCORE.2021", "SCALE_SCORE.2022",
      "ACHIEVEMENT_LEVEL.2021", "ACHIEVEMENT_LEVEL.2022",
      "SGP.2022", "SGP_LEVEL.2022"
    )

setcolorder(WIDA_GA_ISR_Data, col.ord)

setnames(WIDA_GA_ISR_Data,
         gsub("[.]2022", "", names(WIDA_GA_ISR_Data))
)
setnames(WIDA_GA_ISR_Data,
         gsub("[.]2021", "_PRIOR", names(WIDA_GA_ISR_Data))
)
setnames(WIDA_GA_ISR_Data, "ID", "GTID")

setkey(WIDA_GA_ISR_Data, DISTRICT_NUMBER, SCHOOL_NUMBER)

###   Create workbooks

dist.dat.path <- file.path(isr_dir_22, "District_Data")
if (!dir.exists(dist.dat.path))
    dir.create(dist.dat.path, recursive = TRUE)

##    Define styles for `openxlsx`
hs1 <-
    createStyle(
        halign = "left",
        textDecoration = "Bold",
        fgFill = "#D9D9D9",
        border = "Bottom",
        wrapText = FALSE
    )

##    Create File Layout (base) workbook
file_layout <- read.xlsx("District Data File Layout ACCESS SGP.xlsx")
names(file_layout) <- gsub("[.]", " ", names(file_layout))
layout.tab <- "ACCESS SGP Data File Layout"

base_wb <- createWorkbook(creator = NULL)
addWorksheet(base_wb, layout.tab)
setColWidths(base_wb, layout.tab, cols = c(1,2), widths = c(25, 85))

writeData(base_wb, layout.tab, file_layout, headerStyle = hs1)

for (tmp.dist in unique(WIDA_GA_ISR_Data[, DISTRICT_NUMBER])) {
    tmp.data <-
        WIDA_GA_ISR_Data[DISTRICT_NUMBER == tmp.dist]

    if (grepl("^782|^783", tmp.dist)) {
        tmp.dist <-
            gsub("(\\d{3})(?=\\d{3})", "\\1-", tmp.dist, perl = TRUE)
    }
    tmp.dst.numb <- paste("District", tmp.dist, "Data")
    tmp.dst.name <- tmp.data[, DISTRICT_NAME][1]
    setkey(
        tmp.data,
        SCHOOL_NUMBER,
        GRADE,
        LAST_NAME,
        FIRST_NAME
    )

    tmp.wb.name <-
        paste0(tmp.dist, "_ACCESS_SGP_Sp2022_", tmp.dst.name, "_DATA.xlsx")

    ##    create a workbook
    growth_wb <- copyWorkbook(base_wb)

    ##    Simple - single worksheet
    addWorksheet(growth_wb, tmp.dst.numb)
    setColWidths(growth_wb, tmp.dst.numb, cols = c(2, 4), widths = 20)
    setColWidths(growth_wb, tmp.dst.numb, cols = c(7, 8), widths = 12)
    setColWidths(growth_wb, tmp.dst.numb, cols = c(14:15), widths = 20)
    freezePane(growth_wb, tmp.dst.numb, firstActiveRow = 2, firstActiveCol=9)

    writeData(growth_wb, tmp.dst.numb, tmp.data, headerStyle = hs1)

    saveWorkbook(
        growth_wb,
        file.path(dist.dat.path, tmp.wb.name),
        overwrite = TRUE
    )
}

zip(zipfile = paste0(dist.dat.path, "_2022.zip"),
    files = dist.dat.path,
    flags = "-rmqj1"
)
unlink(dist.dat.path, recursive = TRUE)
