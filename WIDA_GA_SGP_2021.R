################################################################################
###                                                                          ###
###       Script for calculating SGPs for 2021 (& 2020) for WIDA-ACCESS      ###
###                                                                          ###
################################################################################

### Load SGP package
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_GA_Data_LONG.Rdata")

### Add baseline matrices
SGPstateData <- SGPmatrices::addBaselineMatrices("WIDA", year="2021", add.matrices.to.which.state = "WIDA_GA")

###   Run updateSGP to produce cohort referrenced SGPs, etc.
WIDA_GA_SGP <- abcSGP(
		sgp_object = WIDA_GA_Data_LONG,
		years = c("2020", "2021"),
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "visualizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=TRUE,
		sgp.projections.baseline=TRUE,
		sgp.projections.lagged.baseline=TRUE,
		sgp.target.scale.scores=TRUE,
		plot.types=c("growthAchievementPlot"),
		outputSGP.output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data"),
		save.intermediate.results=FALSE,
		parallel.config=list(
			BACKEND="PARALLEL",
			WORKERS=list(
				PERCENTILES=8,
				PROJECTIONS=4,
				LAGGED_PROJECTIONS=2,
				SGP_SCALE_SCORE_TARGETS=2)))

###   Save results
save(WIDA_GA_SGP, file="Data/WIDA_GA_SGP.Rdata")
