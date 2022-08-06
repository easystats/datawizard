# correlation

<details>

* Version: 0.8.1
* GitHub: https://github.com/easystats/correlation
* Source code: https://github.com/cran/correlation
* Date/Publication: 2022-05-20 21:50:02 UTC
* Number of recursive dependencies: 184

Run `revdep_details(, "correlation")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘correlation-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: visualisation_recipe.easycor_test
    > ### Title: Visualisation Recipe for 'correlation' Objects
    > ### Aliases: visualisation_recipe.easycor_test
    > ###   visualisation_recipe.easycormatrix
    > ###   visualisation_recipe.easycorrelation
    > 
    > ### ** Examples
    ...
    +     labs = list(title = "My Plot")
    +   )
    +   plot(layers) + theme_modern()
    + }
    Loading required package: see
    Warning: Argument `colnames_to` is deprecated. Please use `names_to` instead.
    Error in colnames(data)[sapply(data, select)] : 
      invalid subscript type 'list'
    Calls: visualisation_recipe ... visualisation_recipe.easycormatrix -> <Anonymous> -> .select_nse
    Execution halted
    ```

# effectsize

<details>

* Version: 0.7.0
* GitHub: https://github.com/easystats/effectsize
* Source code: https://github.com/cran/effectsize
* Date/Publication: 2022-05-26 13:20:02 UTC
* Number of recursive dependencies: 234

Run `revdep_details(, "effectsize")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       7.       └─effectsize (local) FUN(newX[, i], ...)
       8.         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
      ── Error (test-rankES.R:76:5): kendalls_w ──────────────────────────────────────
      Error in `UseMethod("ranktransform")`: no applicable method for 'ranktransform' applied to an object of class "character"
      Backtrace:
    ...
          ▆
       1. └─effectsize::kendalls_w(M1) at test-rankES.R:76:4
       2.   └─effectsize:::.kendalls_w(data, verbose = verbose)
       3.     └─base::apply(data, 1, .safe_ranktransform, verbose = verbose)
       4.       └─effectsize (local) FUN(newX[, i], ...)
       5.         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
      
      [ FAIL 3 | WARN 3 | SKIP 11 | PASS 494 ]
      Error: Test failures
      Execution halted
    ```

# parameters

<details>

* Version: 0.18.1
* GitHub: https://github.com/easystats/parameters
* Source code: https://github.com/cran/parameters
* Date/Publication: 2022-05-29 05:50:02 UTC
* Number of recursive dependencies: 374

Run `revdep_details(, "parameters")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘parameters-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cluster_analysis
    > ### Title: Cluster Analysis
    > ### Aliases: cluster_analysis
    > 
    > ### ** Examples
    > 
    > set.seed(33)
    > # K-Means ====================================================
    > rez <- cluster_analysis(iris[1:4], n = 3, method = "kmeans")
    Error: Could not find variable '4' in data.
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-cluster_analysis.R:6:3): cluster_analysis, predict, matrix ──────
      Error: Could not find variable '4' in data.
      Backtrace:
          ▆
    ...
       1. └─parameters::cluster_analysis(iris.dat, n = 4, method = "kmeans") at test-cluster_analysis.R:6:2
       2.   ├─parameters::model_parameters(...)
       3.   └─parameters:::model_parameters.kmeans(...)
       4.     └─datawizard::reshape_longer(...)
       5.       └─datawizard:::.select_nse(...)
       6.         └─datawizard:::.evaluate_pattern(...)
      
      [ FAIL 1 | WARN 0 | SKIP 17 | PASS 568 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘datawizard::data_rescale’
    ```

# report

<details>

* Version: 0.5.1
* GitHub: https://github.com/easystats/report
* Source code: https://github.com/cran/report
* Date/Publication: 2022-02-22 13:00:02 UTC
* Number of recursive dependencies: 173

Run `revdep_details(, "report")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘report-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: report.default
    > ### Title: Template to add report support for new objects
    > ### Aliases: report.default report_effectsize.default report_table.default
    > ###   report_statistics.default report_parameters.default
    > ###   report_intercept.default report_model.default report_random.default
    > ###   report_priors.default report_performance.default report_info.default
    > ###   report_text.default
    ...
    > 
    > library(report)
    > 
    > # Add a reproducible example instead of the following
    > model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
    > r <- report(model)
    Error in data_findcols(table, candidates) : 
      could not find function "data_findcols"
    Calls: report ... report_statistics -> report_statistics.lm -> .find_regression_estimate
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    .find_regression_estimate: no visible global function definition for
      ‘data_findcols’
    report_table.MixMod: no visible global function definition for
      ‘data_findcols’
    report_table.anova: no visible global function definition for
      ‘data_findcols’
    report_table.aov: no visible global function definition for
      ‘data_findcols’
    report_table.aovlist: no visible global function definition for
      ‘data_findcols’
    ...
    report_table.merMod: no visible global function definition for
      ‘data_findcols’
    report_table.stanreg: no visible global function definition for
      ‘data_findcols’
    report_table.survreg: no visible global function definition for
      ‘data_findcols’
    report_table.zeroinfl: no visible global function definition for
      ‘data_findcols’
    Undefined global functions or variables:
      data_findcols
    ```

# see

<details>

* Version: 0.7.1
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2022-06-20 14:20:02 UTC
* Number of recursive dependencies: 210

Run `revdep_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘see-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coord_radar
    > ### Title: Radar coordinate system
    > ### Aliases: coord_radar
    > 
    > ### ** Examples
    > 
    > # Create a radar/spider chart with ggplot:
    ...
    Attaching package: ‘poorman’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    Loading required package: ggplot2
    Error in FUN(X[[i]], ...) : object 'Name' not found
    Calls: <Anonymous> ... <Anonymous> -> f -> scales_add_defaults -> lapply -> FUN
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘datawizard::data_rescale’
    ```

# sjPlot

<details>

* Version: 2.8.10
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2021-11-26 16:10:50 UTC
* Number of recursive dependencies: 189

Run `revdep_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_kfold_cv
    > ### Title: Plot model fit from k-fold cross-validation
    > ### Aliases: plot_kfold_cv
    > 
    > ### ** Examples
    > 
    > data(efc)
    ...
     13. │   │ └─base::eval(mf, parent.frame())
     14. │   │   └─base::eval(mf, parent.frame())
     15. │   ├─stats::model.frame(formula = formula, data = ., drop.unused.levels = TRUE)
     16. │   └─stats::model.frame.default(formula = formula, data = ., drop.unused.levels = TRUE)
     17. │     └─base::eval(predvars, data, env)
     18. │       └─base::eval(predvars, data, env)
     19. └─base::.handleSimpleError(...)
     20.   └─dplyr (local) h(simpleError(msg, call))
     21.     └─rlang::abort(...)
    Execution halted
    ```

# statsExpressions

<details>

* Version: 1.3.2
* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Source code: https://github.com/cran/statsExpressions
* Date/Publication: 2022-05-20 19:50:02 UTC
* Number of recursive dependencies: 158

Run `revdep_details(, "statsExpressions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─effectsize:::.kendalls_w(data, verbose = verbose)
        6. │     └─base::apply(data, 1, .safe_ranktransform, verbose = verbose)
        7. │       └─effectsize (local) FUN(newX[, i], ...)
        8. │         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
        9. ├─statsExpressions:::tidy_model_effectsize(.)
    ...
       10. │ ├─dplyr::bind_cols(...)
       11. │ │ └─rlang::list2(...)
       12. │ └─... %>% select(-contains("term"))
       13. ├─dplyr::select(., -contains("term"))
       14. ├─insight::standardize_names(., style = "broom")
       15. └─dplyr::mutate(., effectsize = stats::na.omit(effectsize::get_effectsize_label(colnames(.))))
      
      [ FAIL 2 | WARN 2 | SKIP 52 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

