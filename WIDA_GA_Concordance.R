#+ include = FALSE, purl = FALSE
###############################################################################
###                                                                         ###
###      WIDA ACCESS Concordance Meta-Data for scale Revisions  -  2026     ###
###                                                                         ###
###############################################################################

#' ### Concordance data preparation and formatting for `SGPstateData`
#'
#' The data preparation step involves taking data provided ...


#+ include = FALSE, purl = FALSE, eval = FALSE
###   Load packages
require(data.table)
require(SGP)


###   Read in concordance table for WIDA ACCESS 2026, and make a long version
WIDA_Concordance_Table_wide <-
    openxlsx::read.xlsx(
        "Data/Base_Files/Test Data 2026/WIDA-ACCESS-Concordance-Table-25-26.xlsx",
        sheet = "Overall",
        sep.names = "_"
    )|> data.table::as.data.table()

first.cols <- seq(1, 52, by = 4)
last.cols  <- seq(4, 52, by = 4)

WIDA_Concordance_Table <-
    lapply(1:13, \(g) {
        WIDA_Concordance_Table_wide[, first.cols[g]:last.cols[g]]
    }) |> data.table::rbindlist() |>
    data.table::setnames(c("GRADE", "SCALE_SCORE_2026", "SCALE_SCORE_2025", "PROFICIENCY_LEVEL"))

WIDA_Concordance_Table[,
    PROFICIENCY_LEVEL := as.character(round(PROFICIENCY_LEVEL, 1))
][,
    ACHIEVEMENT_LEVEL := fcase(
        PROFICIENCY_LEVEL %in% as.character(seq(1, 1.9, by = 0.1)), "1",
        PROFICIENCY_LEVEL %in% as.character(seq(2, 2.9, by = 0.1)), "2",
        PROFICIENCY_LEVEL %in% as.character(seq(3, 3.9, by = 0.1)), "3",
        PROFICIENCY_LEVEL %in% as.character(seq(4, 4.2, by = 0.1)), "4",
        PROFICIENCY_LEVEL %in% as.character(seq(4.3, 4.9, by = 0.1)), "4.3",
        PROFICIENCY_LEVEL %in% as.character(seq(5, 5.9, by = 0.1)), "5",
        PROFICIENCY_LEVEL %in% "6", "6")
]

# table(WIDA_Concordance_Table[, ACHIEVEMENT_LEVEL, PROFICIENCY_LEVEL], exclude = NULL)

###   Cutscores based on concordance table

##    Check table results against existing cutscores:
existing_cuts <-
    WIDA_Concordance_Table[,
        .(min(SCALE_SCORE_2025)),
        by = .(GRADE, ACHIEVEMENT_LEVEL)
    ][!which(ACHIEVEMENT_LEVEL == 1), ] |>
        data.table::dcast(GRADE ~ ACHIEVEMENT_LEVEL, value.var = "V1")

concord_cuts <-
    WIDA_Concordance_Table[,
        .(min(SCALE_SCORE_2026)),
        by = .(GRADE, ACHIEVEMENT_LEVEL)
    ][!which(ACHIEVEMENT_LEVEL == 1), ] |>
        data.table::dcast(GRADE ~ ACHIEVEMENT_LEVEL, value.var = "V1")
concord_cuts[1, `6` := 601]

cuts.text <- c()
for (cs in 1:nrow(concord_cuts)) {
    cuts.text <- c(cuts.text, paste0("GRADE_",
        concord_cuts[cs, GRADE], " = c(", paste(
        concord_cuts[cs, 2:7], collapse = ", "), ")"
    ))
}
# cat(cuts.text, sep = ",\n")  ##  copy/paste output into cutscores list in SGPstateData

# setkey(WIDA_Concordance_Table)

# WIDA_Linkages_2026 <- list()
# WIDA_Equated_Knots_Bounds <- list()
# WIDA_Linkages_2026[["READING.2026"]] <- vector(mode = "list", length = 13)
#     for (g in 1:13) {
#         WIDA_Linkages_2026[["READING.2026"]][[g]][[
#           "PRE_EQUATED"]][["OLD_TO_NEW"]][["concordance"]] <- equate_dt <-
#             WIDA_Concordance_Table[GRADE == g-1][,
#               list(yx = mean(SCALE_SCORE_2026, na.rm = TRUE)), by = SCALE_SCORE_2025] |>
#                 data.table::setnames(c("scale", "yx"))
#         WIDA_Linkages_2026[["READING.2026"]][[g]][[
#           "PRE_EQUATED"]][["OLD_TO_NEW"]][["interpolated_function"]] <- tmp_func <-
#             approxfun(equate_dt[["scale"]], equate_dt[["yx"]], rule = 2)
#         WIDA_Equated_Knots_Bounds <-
#           c(WIDA_Equated_Knots_Bounds,
#             setNames(list(tmp_func(
#                 SGPstateData[["WIDA"]][["Achievement"]][["Knots_Boundaries"]][["READING.2016"]][[paste0("knots_", g-1)]]
#                   )), paste0("knots_", g-1)),
#             setNames(list(c(50, 650)), paste0("boundaries_", g-1)),
#             setNames(list(c(100, 600)), paste0("loss.hoss_", g-1))
#           )
#     }
# names(WIDA_Linkages_2026[["READING.2026"]]) <- paste0("GRADE_", 0:12)
# WIDA_Linkages_2026[["READING.2026"]][["GRADE_3"]][["PRE_EQUATED"]][["OLD_TO_NEW"]] |> names()

# saveRDS(WIDA_Linkages_2026, "Data/WIDA_Linkages_2026.rds")
# saveRDS(WIDA_Equated_Knots_Bounds, "Data/WIDA_Equated_Knots_Bounds.rds")
