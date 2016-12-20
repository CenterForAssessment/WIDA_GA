
##########################################################################################
### WORLD CLASS INSTRUCTIONAL DESIGN and ASSESSMENT (WIDA) GEORGIA 
##########################################################################################

load("U:/DATA/SGP/ACCESS SGP/WIDA_Knots_Boundaries.Rdata")
SGPstateData[["WIDA_GA"]][["Achievement"]][["Knots_Boundaries"]] <- WIDA_MA_Knots_Boundaries



SGPstateData[["WIDA_GA"]][["Achievement"]][["Cutscores"]] <- list(
  READING =list(
    GRADE_0=c(237, 263, 288, 307, 329),
    GRADE_1=c(249, 277, 303, 321, 344),
    GRADE_2=c(261, 290, 316, 335, 357),
    GRADE_3=c(272, 303, 328, 347, 369),
    GRADE_4=c(283, 314, 340, 359, 380),
    GRADE_5=c(293, 324, 350, 369, 390),
    GRADE_6=c(302, 334, 359, 379, 399),
    GRADE_7=c(311, 342, 368, 386, 407),
    GRADE_8=c(319, 350, 375, 394, 414),
    GRADE_9=c(327, 357, 382, 400, 419),
    GRADE_10=c(333, 363, 387, 405, 424),
    GRADE_11=c(340, 368, 391, 409, 427),
    GRADE_12=c(346, 372, 395, 413, 430)))

SGPstateData[["WIDA_GA"]][["Achievement"]][["Levels"]] <- list(
  Labels=c("WIDA Level 1", "WIDA Level 2", "WIDA Level 3", "WIDA Level 4", "WIDA Level 5", "WIDA Level 6", "NO SCORE"),
  Proficient=c("Not Proficient","Not Proficient","Not Proficient","Not Proficient","Proficient","Proficient",NA))

SGPstateData[["WIDA_GA"]][["Growth"]][["Levels"]] <- c("Low", "Typical", "High")

SGPstateData[["WIDA_GA"]][["Growth"]][["Cutscores"]] <- list(
  Cuts=c(35, 66),
  Labels=c("1st-34th","35th-65th","66th-99th"))

SGPstateData[["WIDA_GA"]][["Growth"]][["System_Type"]] <- "Cohort and Baseline Referenced"

SGPstateData[["WIDA_GA"]][["Assessment_Program_Information"]] <- list(
  Assessment_Name="World Class Instructional Design and Assessment",
  Assessment_Abbreviation="WIDA",
  Organization=list(
    Name="World Class Instructional Design and Assessment",
    Abbreviation="WIDA",
    URL="http://www.wida.us/",
    Contact="1-866-276-7735"),
  Content_Areas="Reading",
  Grades_Tested=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
  Assessment_Years=c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
  Test_Season="Spring",
  Test_Vendor="WIDA")

SGPstateData[["WIDA_GA"]][["Student_Report_Information"]] <- list(
  Vertical_Scale=list(READING=TRUE),
  Content_Areas_Labels=list(READING="Reading"),
  Grades_Reported=list(READING=c(0,1,2,3,4,5,6,7,8,9,10,11,12)),
  Achievement_Level_Labels=list(
    "Entering"="L1",
    "Emerging"="L2",
    "Developing"="L3",
    "Expanding"="L4",
    "Bridging"="L5",
    "Reaching"="L6"))


SGPstateData[["WIDA_GA"]][["SGP_Configuration"]] <- list(
  max.order.for.percentile=2,
  max.order.for.projection=2,
  max.sgp.target.years.forward=5,
  sgp.projections.max.forward.progression.years=7,
  sgPlot.fan.condition="head(Achievement_Levels, 1) %in% paste('Level', 1:4)",
  sgPlot.sgp.targets=c("sgp.projections", "sgp.projections.lagged"),
  sgp.target.types=c("Scale_Score_Targets_CUKU", "Scale_Score_Targets_Current_CUKU"))

load("Baseline_Coefficient_Matrices/WIDA/WIDA_Baseline_Matrices.Rdata")
SGPstateData[["WIDA"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- WIDA_Baseline_Matrices
