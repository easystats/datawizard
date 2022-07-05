# text formatters respect `width` argument

    Code
      long_text <- paste(rep("abc ", 100), collapse = "")
      cat(format_text(long_text, width = 50))
    Output
       abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc
    Code
      cat(format_text(long_text, width = 80))
    Output
       abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
    Code
      withr::with_options(list(width = 50), code = {
        cat(format_text(long_text))
      })
    Output
       abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc

