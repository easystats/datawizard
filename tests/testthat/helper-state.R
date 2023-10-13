testthat::set_state_inspector(function() {
  # sometimes a dependency might add a custom option, so we need to
  # make sure we don't fail because of such additions
  options <- options()

  # Result of `dput(names(options()))`
  base_options <- c(
    "add.smooth", "askpass", "asksecret", "bitmapType", "browser",
    "browserNLdisabled", "buildtools.check", "buildtools.with", "callr.condition_handler_cli_message",
    "CBoundsCheck", "check.bounds", "citation.bibtex.max", "connectionObserver",
    "continue", "contrasts", "defaultPackages", "demo.ask", "deparse.cutoff",
    "deparse.max.lines", "device", "device.ask.default", "digits",
    "download.file.method", "dvipscmd", "echo", "editor", "encoding",
    "example.ask", "expressions", "ggvis.renderer", "help_type",
    "help.search.types", "help.try.all.packages", "HTTPUserAgent",
    "install.packages.compile.from.source", "internet.info", "keep.parse.data",
    "keep.parse.data.pkgs", "keep.source", "keep.source.pkgs", "locatorBell",
    "mailer", "matprod", "max.contour.segments", "max.print", "menu.graphics",
    "na.action", "nwarnings", "OutDec", "page_viewer", "pager", "papersize",
    "PCRE_limit_recursion", "PCRE_study", "PCRE_use_JIT", "pdfviewer",
    "pkgType", "plumber.docs.callback", "plumber.swagger.url", "printcmd",
    "profvis.keep_output", "profvis.print", "profvis.prof_extension",
    "profvis.prof_output", "prompt", "repos", "restart", "reticulate.initialized",
    "reticulate.repl.busy", "reticulate.repl.hook", "reticulate.repl.initialize",
    "reticulate.repl.teardown", "rl_word_breaks", "rsconnect.check.certificate",
    "rstudio.notebook.executing", "RStudioGD.antialias", "RStudioGD.backend",
    "scipen", "shiny.launch.browser", "shinygadgets.showdialog",
    "show.coef.Pvalues", "show.error.messages", "show.signif.stars",
    "showErrorCalls", "showNCalls", "showWarnCalls", "str", "str.dendrogram.last",
    "terminal.manager", "texi2dvi", "timeout", "ts.eps", "ts.S.compat",
    "unzip", "useFancyQuotes", "verbose", "viewer", "warn", "warning.length",
    "warnPartialMatchArgs", "warnPartialMatchAttr", "warnPartialMatchDollar",
    "width"
  )
  options <- options[base_options]

  list(
    attached = search(),
    connections = nrow(showConnections()),
    cwd = getwd(),
    envvars = Sys.getenv(),
    libpaths = .libPaths(),
    locale = Sys.getlocale(),
    options = options,
    packages = .packages(all.available = TRUE),
    NULL
  )
})
