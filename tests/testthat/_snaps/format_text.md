# text formatting helpers work as expected

    Code
      cat(text_wrap(long_text, width = 50))
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

---

    Code
      cat(text_wrap(long_text, width = 80))
    Output
       abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc
      abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc

