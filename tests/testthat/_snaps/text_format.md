# text formatting helpers work as expected

    Code
      format_text(c("A very long First", "Some similar long Second", "Shorter Third",
        "More or less long Fourth", "And finally the Last"), width = 20)
    Output
      [1] "A very long First,\nSome similar long\nSecond, Shorter\nThird, More or less\nlong Fourth and And\nfinally the Last\n"

---

    Code
      format_text(c("A very long First", "Some similar long Second", "Shorter Third",
        "More or less long Fourth", "And finally the Last"), last = " or ", enclose = "`",
      width = 20)
    Output
      [1] "`A very long\nFirst`, `Some\nsimilar long\nSecond`, `Shorter\nThird`, `More or\nless long Fourth`\nor `And finally the\nLast`\n"

# text formatters respect `width` argument

    Code
      long_text <- strrep("abc ", 100)
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

