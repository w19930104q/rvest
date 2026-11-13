# dangling header references are reported as problems

    Code
      tr <- html_table(html_element(html, "table"), trace = TRUE)
    Condition
      Warning:
      Table source tracing found 1 problem:
      * Dangling header reference: ghost.

# duplicate ids are reported as problems

    Code
      tr <- html_table(html_element(html, "table"), trace = TRUE)
    Condition
      Warning:
      Table source tracing found 1 problem:
      * Duplicate cell id: dup.

