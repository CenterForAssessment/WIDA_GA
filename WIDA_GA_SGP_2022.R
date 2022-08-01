###############################################################################
###                                                                         ###
###          Script to calculate SGPs for 2022 Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load required packages
require(SGP)
require(data.table)

###   Load SGP object from 2021 analyses and 2022 cleaned/prepped long data
load("Data/WIDA_GA_SGP.Rdata")
load("Data/WIDA_GA_Data_LONG_2022.Rdata")

###   Add baseline matrices to `SGPstateData`
SGPstateData <- SGPmatrices::addBaselineMatrices(
                               "WIDA", year = "2021",
                               add.matrices.to.which.state = "WIDA_GA")

###   Run updateSGP to produce cohort and baseline referrenced SGPs, etc.
WIDA_GA_SGP <- updateSGP(
        what_sgp_object = WIDA_GA_SGP,
        with_sgp_data_LONG = WIDA_GA_Data_LONG_2022,
        steps = c("prepareSGP", "analyzeSGP",
                  "combineSGP", "visualizeSGP", "outputSGP"),
        sgp.target.scale.scores = TRUE,
        plot.types = "growthAchievementPlot",
        outputSGP.output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data"),
        save.intermediate.results = FALSE,
        parallel.config = list(
            BACKEND = "PARALLEL",
            WORKERS = list(
                PERCENTILES = 12,
                PROJECTIONS = 6,
                LAGGED_PROJECTIONS = 4,
                SGP_SCALE_SCORE_TARGETS = 4)))

###   Save results
save(WIDA_GA_SGP, file = "Data/WIDA_GA_SGP.Rdata")
