#+ include = FALSE, purl = FALSE, eval = FALSE
###############################################################################
###                                                                         ###
###          Script to calculate SGPs for 2026 Georgia WIDA-ACCESS          ###
###                                                                         ###
###############################################################################

###   Load required packages
require(data.table)
require(SGP)

###   Load SGP results from 2024 and 2025 analyses, and 2026 cleaned/prepped long data
load("Data/Archive/2025/WIDA_GA_SGP_LONG_Data_2024.Rdata")
load("Data/Archive/2025/WIDA_GA_SGP_LONG_Data_2025.Rdata")
load("Data/WIDA_GA_Data_LONG_2026.Rdata")

###   Remove baseline SGP results from prior years
WIDA_GA_SGP_LONG_Data_2024 <-
    WIDA_GA_SGP_LONG_Data_2024[,
      -grep("BASELINE", names(WIDA_GA_SGP_LONG_Data_2024)), with = FALSE
    ]
WIDA_GA_SGP_LONG_Data_2025 <-
    WIDA_GA_SGP_LONG_Data_2025[,
      -grep("BASELINE", names(WIDA_GA_SGP_LONG_Data_2025)), with = FALSE
    ]

WIDA_GA_SGP_LONG_Data <- rbindlist(list(
    WIDA_GA_SGP_LONG_Data_2024,
    WIDA_GA_SGP_LONG_Data_2025,
    WIDA_GA_Data_LONG_2026),
    fill = TRUE
)

###   Run updateSGP to produce cohort and baseline referenced SGPs, etc.

WIDA_GA_SGP <-
    abcSGP(
        sgp_object = WIDA_GA_SGP_LONG_Data,
        years = "2026",
        # sgp.config = WIDA_GA_Config_2026,
        steps = c(
            "prepareSGP", "analyzeSGP", "combineSGP",
            "visualizeSGP", "outputSGP"),
        sgp.percentiles = TRUE,
        sgp.projections = TRUE,
        sgp.projections.lagged = TRUE,
        sgp.percentiles.baseline = FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.percentiles.equated = TRUE,
        sgp.percentiles.equating.method = "pre-equated",
        sgp.target.scale.scores = TRUE,
        plot.types = "growthAchievementPlot",
        outputSGP.output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data"),
        save.intermediate.results = FALSE,
        parallel.config = list(
            WORKERS = list(
                PERCENTILES = 11,
                PROJECTIONS = 11,
                LAGGED_PROJECTIONS = 11,
                SGP_SCALE_SCORE_TARGETS = 11
            )
        )
    )

#+ include = FALSE, purl = FALSE, eval = FALSE
###   Add R session Info & Save results (`cfaDocs` version 0.0-1.12 or later)
source(
    system.file(
        "rmarkdown", "shared_resources", "rmd", "R_Session_Info.R",
        package = "cfaDocs"
    )
)
WIDA_GA_SGP@Version[["session_platform"]][["2026"]] <- session_platform
WIDA_GA_SGP@Version[["attached_pkgs"]][["2026"]]    <- attached_pkgs
WIDA_GA_SGP@Version[["namespace_pkgs"]][["2026"]]   <- namespace_pkgs

###   Save results
save(WIDA_GA_SGP, file = "Data/WIDA_GA_SGP.Rdata")


#' ### Conduct SGP analyses
#'
#' The GaDOE uses cohort-referenced SGPs as the official student-level
#' English language proficiency growth metric. All SGPs are calculated
#' using the [`R` Software Environment](http://www.r-project.org) in
#' conjunction with the [`SGP` package](http://sgp.io). Broadly, the
#' Georgia WIDA ACCESS for ELLs analyses are completed in 5 steps.
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
#' We use the [`updateSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/updateSGP)
#' function to ***a)*** do the final preparation and addition of the cleaned and
#' formatted new annual data,
#' ([`prepareSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/prepareSGP)
#' step), ***b)*** calculate SGP estimates
#' ([`analyzeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/analyzeSGP)
#' step), ***c)*** merge the results into the master longitudinal data set
#' ([`combineSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/combineSGP)
#' step) and ***d)*** output a pipe delimited version of the complete long data
#' ([`outputSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/outputSGP)
#' step).
#' 
#' #### Visualize results
#' 
#' Once all analyses are completed via `updateSGP`, individual student growth
#' and English language proficiency reports are produced using the
#' [`visualizeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.1-0.0/topics/visualizeSGP)
#' function and a custom template designed for Georgia. English and Spanish
#' language versions of these reports are created, and individual reports
#' and school level catalogs are bundled according to Georgia's specifications.
#'
#' #### Custom data formatting and district output
#' 
#' The 2026 WIDA ACCESS for ELLs SGP results data are submitted to GaDOE
#' with additional formatting to add fields including students' prior
#' language proficiency level.
#' 
#' System level student data sets with fields used to create individual
#' student reports are also submitted to GaDOE in Microsoft Excel format.
