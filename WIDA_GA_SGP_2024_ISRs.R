###############################################################################
###                                                                         ###
###          Script for creating 2024 ISRs for Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load SGP package
require(data.table)
require(SGP)

###   Load Data
# load("Data/WIDA_GA_SGP.Rdata")
load('/Users/avi/SGP Dropbox/Adam Van Iwaarden/SGP/WIDA_GA/Data/WIDA_GA_SGP.Rdata')

###   Define 2024 ISR directory
isr_dir_24 <- "Visualizations/studentGrowthPlots/School/2024"

###   Modify data for ISR production
##    Only produce ISRs for students with SGPs
isr.ids <-
    WIDA_GA_SGP@Data[YEAR == "2024" & !is.na(SGP), ID] # (!is.na(SGP) | GRADE == "0")

WIDA_GA_SGP@Data[!ID %in% isr.ids, VALID_CASE := "INVALID_CASE"]

###   Remove some duplicates (wrong GTID!)
##    One known case to fix in 2024
WIDA_GA_SGP@Data[
    ID == "9274465865" & YEAR == "2023" & BIRTH_DATE == "08/15/2006",
      VALID_CASE := "INVALID_CASE"
]

setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
dupl <-
    duplicated(WIDA_GA_SGP@Data, by = key(WIDA_GA_SGP@Data))
sum(dupl) # 661 CROSS-GRADE duplicates - (take the record with the HIGHEST score)
WIDA_GA_SGP@Data[which(dupl) - 1, VALID_CASE := "INVALID_CASE"]
setkey(WIDA_GA_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)

###   Filter and modify data for ISR production
WIDA_GA_SGP@Data <-
    WIDA_GA_SGP@Data[
        VALID_CASE == "VALID_CASE" &
        YEAR >= 2022,
    ]

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
][!(grepl("[.]", ACHIEVEMENT_LEVEL_ENGLISH) |
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
][!(grepl("[.]", ACHIEVEMENT_LEVEL_SPANISH) |
    ACHIEVEMENT_LEVEL_SPANISH == "Sin Escala"),
        ACHIEVEMENT_LEVEL_SPANISH :=
            paste0(ACHIEVEMENT_LEVEL_SPANISH, ".0")
]

###   Create "catch-all" directory for school-level catalogs
all_schools <- file.path(isr_dir_24, "All_Schools")
if (!dir.exists(all_schools))
    dir.create(all_schools, recursive = TRUE)

###   Create "catch-all" directory for school-level catalogs
dist_schools <- file.path(isr_dir_24, "Schools_by_District")
if (!dir.exists(dist_schools))
    dir.create(dist_schools, recursive = TRUE)

#####
###   English Version
#####

SGPstateData[["WIDA_GA"]][["Student_Report_Information"]][[
    "Achievement_Level_Labels"]] <- list(
        "Level 1\nEntering" = "Level 1",
        "Level 2\nEmerging" = "Level 2",
        "Level 3\nDeveloping" = "Level 3",
        "Level 4\nExpanding" = "Level 4",
        "Level 4\nExpanding" = "Level 4.3",
        "Level 5\nBridging" = "Level 5",
        "Level 6\nReaching" = "Level 6"
    )

SGPstateData[["WIDA_GA"]][["Assessment_Program_Information"]][[
    "Assessment_Abbreviation"]] <- NULL

setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ENGLISH"),
         c("ACH_LEV_ACTUAL", "ACHIEVEMENT_LEVEL")
)

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2024",
    # sgPlot.demo.report = TRUE,
    # sgPlot.produce.plots=FALSE, # Get wide district data ONLY
    # sgPlot.districts = "644", # c("633", "687"),  #  DeKalb :: Cobb and Laurens
    sgPlot.save.sgPlot.data = TRUE,
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 10)
    )
)


##    Checks for missing schools/districs
districts <-
  list.dirs(isr_dir_24, full.names = FALSE, recursive = FALSE) |>
    as.numeric() |> na.omit() |> as.character()
dat.dist <-
  unique(WIDA_GA_SGP@Data[YEAR == "2024" & !is.na(SGP)]$DISTRICT_NUMBER)
miss <- setdiff(dat.dist, districts)
length(miss)  #  No missing districts :-)
m <- WIDA_GA_SGP@Data[!is.na(SGP) & DISTRICT_NUMBER %in% miss]
table(m[, GRADE, CONTENT_AREA])  #  0 missing students

##    Added in 2023:
# XXX_ACCESS_SGP_Sp20YY_EnglishISR_Reports.zip, where XXX is the
# district code and Y is the current year (2024). An example is:
# 661_ACCESS_SGP_Sp2023_EnglishISR_Reports.zip

dist_zipname <- "_ACCESS_SGP_Sp2024_EnglishISR_Reports.zip"
sch.catalogs <- list.files(file.path(isr_dir_24, "All_Schools"))
for (d in districts) {
    tmp.cats <- grep(paste0("^", d, "_"), sch.catalogs, value = TRUE)
    if (nchar(d) > 3) d <- gsub("^(.{3})(.*)$", "\\1-\\2", d)
    zip(zipfile = file.path(dist_schools, paste0(d, dist_zipname)),
        files = file.path(isr_dir_24, "All_Schools", tmp.cats),
        flags = "-qj"
    )
}

zip(zipfile = file.path(isr_dir_24, "All_Schools_English.zip"),
    files = file.path(isr_dir_24, "All_Schools/"),
    flags = "-rmqj1"
)


#####
###   Spanish Version
#####

SGPstateData[["WIDA_GA"]] <- SGPstateData[["WIDA_GA_SPANISH"]]

SGPstateData[["WIDA_GA"]][["Student_Report_Information"]][[
    "Achievement_Level_Labels"]] <- list(
        "1-Nivel\nde entrada" = "Level 1",
        "2-Nivel\nemergente" = "Level 2",
        "3-Nivel\nde desarrollo" = "Level 3",
        "4-Nivel\nde extensi\u{F3}n" = "Level 4",
        "4-Nivel\nde extensi\u{F3}n" = "Level 4.3",
        "5-Nivel\nde transformaci\u{F3}n" = "Level 5",
        "6-Nivel\nde trascendencia" = "Level 6"
    )
setnames(WIDA_GA_SGP@Data,
         c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_SPANISH"),
         c("ACHIEVEMENT_LEVEL_ENGLISH", "ACHIEVEMENT_LEVEL")
)

# source("..../Github_Repos/Packages/SGPstateData/Custom_ISR/WIDA_GA/WIDA_GA_Custom_ISR-text.R")
# SGPstateData[["WIDA_GA"]][["Custom_Student_Report"]][["Report_Text"]] <- Report_Text_SPANISH

visualizeSGP(
    WIDA_GA_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2024",
    # sgPlot.demo.report = TRUE,
    # sgPlot.districts = "644", # c("633", "687"),  #  DeKalb :: Cobb and Laurens
    sgPlot.zip = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 10)
    )
)

##    New for 2024:
# 783-0636_ACCESS_SGP_Sp2024_SpanishISR_Reports.zip

dist_zipname <- "_ACCESS_SGP_Sp2024_SpanishISR_Reports.zip"
sch.catalogs <- list.files(file.path(isr_dir_24, "All_Schools"))
for (d in districts) {
    tmp.cats <- grep(paste0("^", d, "_"), sch.catalogs, value = TRUE)
    if (nchar(d) > 3) d <- gsub("^(.{3})(.*)$", "\\1-\\2", d)
    zip(zipfile = file.path(dist_schools, paste0(d, dist_zipname)),
        files = file.path(isr_dir_24, "All_Schools", tmp.cats),
        flags = "-qj"
    )
}

zip(zipfile = file.path(isr_dir_24, "All_Schools_Spanish.zip"),
    files = file.path(isr_dir_24, "All_Schools/"),
    flags = "-rmqj1"
)


###   Remove district and school "shell" directories that are now empty
unlink(
    list.files(
        isr_dir_24, pattern = "[[:digit:]]",
        include.dirs = TRUE, full.names = TRUE
    ),
    recursive = TRUE
)


#####
###   Output District Level Datasets
#####

require(openxlsx)

load("Visualizations/studentGrowthPlots/WIDA_Ga_studentGrowthPlot_Data.Rdata")
ISR_Data <- copy(WIDA_Ga_studentGrowthPlot_Data)

##    Remove redundant and unneeded variables
years.to.remove <- 2022 # only second prior needs removed - object trimmed above
vars.to.remove <-
    c(grep("CONTENT_AREA", names(ISR_Data), value = TRUE),
      grep("TRANSFORMED", names(ISR_Data), value = TRUE),
      "GRADE", paste0("GRADE.", years.to.remove),
      paste0("SCALE_SCORE.", years.to.remove),
      paste0("ACHIEVEMENT_LEVEL.", years.to.remove),
      paste0("SGP.", years.to.remove),
      paste0("SGP_LEVEL.", years.to.remove),
      "P50_PROJ_YEAR_1_CURRENT",
      "P1_PROJ_YEAR_1_CURRENT", "P35_PROJ_YEAR_1_CURRENT", # might keep next year
      "P66_PROJ_YEAR_1_CURRENT", "P99_PROJ_YEAR_1_CURRENT" # might keep next year
    )

ISR_Data[,
    (vars.to.remove)  := NULL
][, REPORTED_SCH_YEAR := "2024"
]

##    Reorder and rename remaining variables
col.ord <-
    c("REPORTED_SCH_YEAR",
      "DISTRICT_NAME.2024", "DISTRICT_NUMBER.2024",
      "SCHOOL_NAME.2024", "SCHOOL_NUMBER.2024",
      "ID", "LAST_NAME.2024", "FIRST_NAME.2024", "BIRTH_DATE.2024",
      "GRADE.2023", "GRADE.2024",
      "SCALE_SCORE.2023", "SCALE_SCORE.2024",
      "ACHIEVEMENT_LEVEL.2023", "ACHIEVEMENT_LEVEL.2024",
      "SGP.2024", "SGP_LEVEL.2024" # No prior (in 2023 and 2024 formatting)
    )

setcolorder(ISR_Data, col.ord)

setnames(ISR_Data,
         gsub("[.]2024", "", names(ISR_Data))
)
setnames(ISR_Data,
         gsub("[.]2023", "_PRIOR", names(ISR_Data))
)
setnames(ISR_Data, "ID", "GTID")

ISR_Data[, c("SGP_PRIOR", "SGP_LEVEL_PRIOR") := NULL]

setkey(ISR_Data, DISTRICT_NUMBER, SCHOOL_NUMBER)

###   Create workbooks
dist.dat.path <- file.path(isr_dir_24, "District_Data")
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
file_layout <- read.xlsx("Data/ACCESS SGP System Data Layout.xlsx")
# file_layout <- read.xlsx("District Data File Layout ACCESS SGP.xlsx")
names(file_layout) <- gsub("[.]", " ", names(file_layout))
layout.tab <- "ACCESS SGP Data File Layout"

base_wb <- createWorkbook(creator = NULL)
addWorksheet(base_wb, layout.tab)
setColWidths(base_wb, layout.tab, cols = c(1, 2, 5, 6), widths = c(12, 25, 20, 85))
# setColWidths(base_wb, layout.tab, cols = c(1,2), widths = c(25, 85))

writeData(base_wb, layout.tab, file_layout, headerStyle = hs1)

for (tmp.dist in unique(ISR_Data[, DISTRICT_NUMBER])) {
    tmp.data <-
        ISR_Data[DISTRICT_NUMBER == tmp.dist]

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
        paste0(tmp.dist, "_ACCESS_SGP_Sp2024_", tmp.dst.name, "_DATA.xlsx")

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

zip(zipfile = paste0(dist.dat.path, "_2024.zip"),
    files = dist.dat.path,
    flags = "-rmqj1"
)
unlink(dist.dat.path, recursive = TRUE)
