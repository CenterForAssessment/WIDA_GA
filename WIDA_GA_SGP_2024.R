#+ include = FALSE, purl = FALSE, eval = FALSE
###############################################################################
###                                                                         ###
###          Script to calculate SGPs for 2024 Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load required packages
require(data.table)
require(SGP)

###   Load SGP object from 2023 analyses and 2024 cleaned/prepped long data
load("Data/WIDA_GA_SGP.Rdata")
load("Data/WIDA_GA_Data_LONG_2024.Rdata")

###   Add baseline matrices to `SGPstateData`
SGPstateData <-
    SGPmatrices::addBaselineMatrices(
        state = "WIDA",
        year = "2020",
        add.matrices.to.which.state = "WIDA_GA"
    )


###   Run updateSGP to produce cohort and baseline referenced SGPs, etc.
WIDA_GA_SGP <-
    updateSGP(
        what_sgp_object = WIDA_GA_SGP,
        with_sgp_data_LONG = WIDA_GA_Data_LONG_2024,
        steps = c(
            "prepareSGP", "analyzeSGP", "combineSGP",
            "visualizeSGP", "outputSGP"
        ),
        sgp.target.scale.scores = TRUE,
        plot.types = "growthAchievementPlot",
        outputSGP.output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data"),
        save.intermediate.results = FALSE,
        parallel.config = list(
            BACKEND = "PARALLEL",
            WORKERS = list(
                PERCENTILES = 11,
                PROJECTIONS = 11,
                LAGGED_PROJECTIONS = 11,
                SGP_SCALE_SCORE_TARGETS = 11
            )
        )
    )

#+ include = FALSE, purl = FALSE, eval = FALSE
###   Save results
save(WIDA_GA_SGP, file = "Data/WIDA_GA_SGP.Rdata")

###  Re-print SVGs from Mac - bad font in Linux...
# gofPrint(
#     sgp_object = WIDA_GA_SGP,
#     years = "2024",
#     output_format = "SVG",
#     output_path = "Goodness_of_Fit"
# )


#' ### Conduct SGP analyses
#'
#' Georgia currently uses cohort-referenced SGPs as the official student-level
#' English language proficiency growth metric. All SGPs were calculated
#' concurrently using the [`R` Software Environment](http://www.r-project.org)
#' in conjunction with the [`SGP` package](http://sgp.io). Broadly, the Georgia
#' WIDA ACCESS for ELLs analyses were completed in 5 steps.
#'
#' 1. `prepareSGP`
#' 2. `analyzeSGP`
#' 3. `combineSGP`
#' 4. `outputSGP`
#' 5. `visualizeSGP`
#' 
#' Because these steps are almost always conducted simultaneously, the `SGP`
#' package has "wrapper" functions, `abcSGP` and `updateSGP`, that combine
#' the above steps into a single function call and simplify the source code
#' associated with the data analysis. Documentation for all SGP functions are
#' [available online.](https://cran.r-project.org/web/packages/SGP/SGP.pdf)
#' 
#' We use the [`updateSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/updateSGP)
#' function to ***a)*** do the final preparation and addition of the cleaned and
#' formatted new annual data,
#' ([`prepareSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/prepareSGP)
#' step), ***b)*** calculate SGP estimates
#' ([`analyzeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/analyzeSGP)
#' step), ***c)*** merge the results into the master longitudinal data set
#' ([`combineSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/combineSGP)
#' step) and ***d)*** output a pipe delimited version of the complete long data
#' ([`outputSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/outputSGP)
#' step).
#' 
#' #### Visualize results
#' 
#' Once all analyses were completed via `updateSGP`, individual student growth
#' and English language proficiency reports were produced using the
#' [`visualizeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/visualizeSGP)
#' function and a custom template designed for Georgia. English and Spanish
#' language versions of these reports were created, and individual reports
#' and school level catalogs were bundled according to Georgia's specifications.
#'
#' #### Custom data formatting and district output
#' 
#' The 2024 WIDA ACCESS for ELLs SGP results data were submitted to GaDOE with
#' additional formatting to add fields including students' prior language
#' proficiency level.
#' 
#' System level student data sets with fields used to create individual
#' student reports were also submitted to GaDOE in Microsoft Excel format.
