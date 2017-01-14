##########################################################################################
###
### Script for calculating SGPs for 2010-2011, 2011-2012, 2012-2013 for WIDA/ACCESS GA
###
##########################################################################################

### Load SGP package

require(SGP)
options(error=recover)
#options(warn=2)


### Load Data

load("Data/WIDA_GA_Data_LONG.Rdata")


### Run analyses

WIDA_GA_SGP <- abcSGP(
		WIDA_GA_Data_LONG,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "visualizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=TRUE,
		sgp.projections.baseline=TRUE,
		sgp.projections.lagged.baseline=TRUE,
        get.cohort.data.info=TRUE,
		sgp.target.scale.scores=TRUE,
		plot.types=c("growthAchievementPlot", "studentGrowthPlot"),
		sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=4, BASELINE_PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SGP_SCALE_SCORE_TARGETS=4, GA_PLOTS=1, SG_PLOTS=1)))


### Save results

save(WIDA_GA_SGP, file="Data/WIDA_GA_SGP.Rdata")
