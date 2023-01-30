# correlation

<details>

* Version: 0.8.3
* GitHub: https://github.com/easystats/correlation
* Source code: https://github.com/cran/correlation
* Date/Publication: 2022-10-09 00:00:02 UTC
* Number of recursive dependencies: 185

Run `revdep_details(, "correlation")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'correlation-Ex.R' failed
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
    Error in vapply(data, x, FUN.VALUE = logical(1L)) : 
      values must be length 1,
     but FUN(X[[1]]) result is length 0
    Calls: visualisation_recipe ... .select_nse -> .eval_expr -> .select_symbol -> which -> vapply
    Execution halted
    ```

# parameters

<details>

* Version: 0.20.2
* GitHub: https://github.com/easystats/parameters
* Source code: https://github.com/cran/parameters
* Date/Publication: 2023-01-27 13:40:06 UTC
* Number of recursive dependencies: 380

Run `revdep_details(, "parameters")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'parameters-Ex.R' failed
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
    Error in ncol(params) : object 'params' not found
    Calls: cluster_analysis ... .eval_call -> .select_context -> eval -> eval -> ncol
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        4.     └─datawizard::reshape_longer(...)
        5.       └─datawizard:::.select_nse(...)
        6.         └─datawizard:::.eval_expr(...)
        7.           └─datawizard:::.eval_call(...)
        8.             └─datawizard:::.select_seq(x, data, ignore_case, regex, verbose)
        9.               └─datawizard:::.eval_expr(...)
       10.                 └─datawizard:::.eval_call(...)
       11.                   └─datawizard:::.select_context(x, data, ignore_case, regex, verbose)
       12.                     └─base::eval(expr, envir = data)
       13.                       └─base::eval(expr, envir = data)
       14.                         └─base::ncol(params)
      
      [ FAIL 1 | WARN 0 | SKIP 33 | PASS 631 ]
      Error: Test failures
      Execution halted
    ```

# performance

<details>

* Version: 0.10.2
* GitHub: https://github.com/easystats/performance
* Source code: https://github.com/cran/performance
* Date/Publication: 2023-01-12 09:00:06 UTC
* Number of recursive dependencies: 258

Run `revdep_details(, "performance")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'performance-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: check_outliers
    > ### Title: Outliers detection (check for influential observations)
    > ### Aliases: check_outliers check_outliers.default check_outliers.numeric
    > ###   check_outliers.data.frame
    > 
    > ### ** Examples
    > 
    ...
    
    Attaching package: 'poorman'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    Error in eval(expr, envir = data) : object 'info' not found
    Calls: %>% ... .eval_expr -> .eval_call -> .select_context -> eval -> eval
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─performance::check_outliers(iris2, method = c("zscore", "iqr")) at test-check_outliers.R:300:6
       2. └─performance:::check_outliers.grouped_df(...)
       3.   └─datawizard::data_relocate(...)
       4.     └─datawizard:::.select_nse(...)
       5.       └─datawizard:::.eval_expr(...)
       6.         └─datawizard:::.eval_call(...)
       7.           └─datawizard:::.select_context(x, data, ignore_case, regex, verbose)
       8.             └─base::eval(expr, envir = data)
       9.               └─base::eval(expr, envir = data)
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 236 ]
      Error: Test failures
      Execution halted
    ```

# report

<details>

* Version: 0.5.5
* GitHub: https://github.com/easystats/report
* Source code: https://github.com/cran/report
* Date/Publication: 2022-08-22 09:00:09 UTC
* Number of recursive dependencies: 161

Run `revdep_details(, "report")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'report-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: report.aov
    > ### Title: Reporting ANOVAs
    > ### Aliases: report.aov report_effectsize.aov report_table.aov
    > ###   report_statistics.aov report_parameters.aov report_model.aov
    > ###   report_info.aov report_text.aov
    > 
    > ### ** Examples
    ...
    > 
    > data <- iris
    > data$Cat1 <- rep(c("A", "B"), length.out = nrow(data))
    > 
    > model <- aov(Sepal.Length ~ Species * Cat1, data = data)
    > r <- report(model)
    Warning: Could not find Sum-of-Squares for the (Intercept) in the ANOVA table.
    Error in if (endsWith(x_dep, "()")) { : the condition has length > 1
    Calls: report ... .select_nse -> .eval_expr -> .eval_call -> .select_context
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─report::report_performance(x)
        5. └─report:::report_performance.lm(x)
        6.   ├─report::report_table(x, ...)
        7.   └─report:::report_table.lm(x, ...)
        8.     └─datawizard::data_remove(...)
        9.       └─datawizard:::.select_nse(...)
       10.         └─datawizard:::.eval_expr(...)
       11.           └─datawizard:::.eval_call(...)
       12.             └─datawizard:::.select_context(x, data, ignore_case, regex, verbose)
      
      [ FAIL 11 | WARN 4 | SKIP 5 | PASS 59 ]
      Error: Test failures
      Execution halted
    ```

# see

<details>

* Version: 0.7.4
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2022-11-26 00:00:02 UTC
* Number of recursive dependencies: 222

Run `revdep_details(, "see")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        4. ├─base::plot(s)
        5. └─correlation:::plot.easycormatrix(s)
        6.   ├─base::plot(visualisation_recipe(x, ...), ...)
        7.   ├─datawizard::visualisation_recipe(x, ...)
        8.   └─correlation:::visualisation_recipe.easycormatrix(x, ...)
        9.     └─datawizard::reshape_longer(...)
       10.       └─datawizard:::.select_nse(...)
       11.         └─datawizard:::.eval_expr(...)
       12.           └─datawizard:::.select_symbol(...)
       13.             ├─base::which(vapply(data, x, FUN.VALUE = logical(1L)))
       14.             └─base::vapply(data, x, FUN.VALUE = logical(1L))
      
      [ FAIL 2 | WARN 5 | SKIP 8 | PASS 30 ]
      Error: Test failures
      Execution halted
    ```

