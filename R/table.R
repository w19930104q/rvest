#' Parse an HTML table into a data frame
#'
#' @description
#' `html_table()` parses a `<table>` element into a data frame, following
#' the same [algorithm](https://html.spec.whatwg.org/multipage/tables.html#forming-a-table)
#' that browsers use to form a table. Cells that span multiple rows or
#' columns (via the `rowspan` and `colspan` attributes) have their values
#' repeated in every cell they cover, and rows with missing cells are
#' padded with `NA`s.
#'
#' `html_table2()` works just like `html_table()` but uses [html_text2()]
#' instead of [html_text()] to extract the text from each cell. This ensures
#' that the cell text more closely resembles what you see in a browser.
#'
#' @inheritParams html_name
#' @param header Use first row as header? If `NA`, will use first row
#'   if it consists of `<th>` tags.
#'
#'   If `TRUE`, column names are left exactly as they are in the source
#'   document, which may require post-processing to generate a valid data
#'   frame.
#' @param trim Remove leading and trailing whitespace within each cell?
#' @param fill `r lifecycle::badge("deprecated")` Missing cells in tables are
#'   now always automatically filled with `NA`.
#' @param dec The character used as decimal place marker.
#' @param na.strings Character vector of values that will be converted to `NA`
#'    if `convert` is `TRUE`.
#' @param convert If `TRUE`, will run [`type.convert()`] to interpret texts as
#'    logical, integer, double, or `NA`.
#' @param trace If `TRUE`, additionally return source provenance for every
#'   cell in the output. The result becomes an `rvest_table_trace` list with
#'   components `data` (the usual tibble), `cells` (a registry of every
#'   source `td`/`th` with a stable in-table id, its coverage, raw text, and
#'   associated header ids), `body` (a matrix of cell ids aligned with the
#'   rows and columns of `data`), and `problems` (a character vector of
#'   diagnostics, e.g. dangling `headers` references or duplicate element
#'   ids). Cell ids have the form `r{row}c{col}` for the anchor position of
#'   each cell; `NA` slots in `body` are padded missing cells. Explicit
#'   `headers` attributes take precedence over `scope`-based association,
#'   and cells with no associated headers contain an empty character vector.
#' @return
#' When applied to a single element, `html_table()` returns a single tibble.
#' When applied to multiple elements or a document, `html_table()` returns
#' a list of tibbles.
#'
#' If `trace = TRUE`, each tibble is instead wrapped in an
#' `rvest_table_trace` list as described in the `trace` argument.
#' @export
#' @examples
#' sample1 <- minimal_html("<table>
#'   <tr><th>Col A</th><th>Col B</th></tr>
#'   <tr><td>1</td><td>x</td></tr>
#'   <tr><td>4</td><td>y</td></tr>
#'   <tr><td>10</td><td>z</td></tr>
#' </table>")
#' sample1 |>
#'   html_element("table") |>
#'   html_table()
#'
#' # Values in merged cells will be duplicated
#' sample2 <- minimal_html("<table>
#'   <tr><th>A</th><th>B</th><th>C</th></tr>
#'   <tr><td>1</td><td>2</td><td>3</td></tr>
#'   <tr><td colspan='2'>4</td><td>5</td></tr>
#'   <tr><td>6</td><td colspan='2'>7</td></tr>
#' </table>")
#' sample2 |>
#'   html_element("table") |>
#'   html_table()
#'
#' # If a row is missing cells, they'll be filled with NAs
#' sample3 <- minimal_html("<table>
#'   <tr><th>A</th><th>B</th><th>C</th></tr>
#'   <tr><td colspan='2'>1</td><td>2</td></tr>
#'   <tr><td colspan='2'>3</td></tr>
#'   <tr><td>4</td></tr>
#' </table>")
#' sample3 |>
#'   html_element("table") |>
#'   html_table()
#'
#' # html_table2() uses html_text2() so often has better output for
#' # text heavy tables
#' sample4 <- minimal_html("<table>
#'   <tr><th>Col A</th><th>Col B</th></tr>
#'   <tr><td>1<br>2</td><td>x<br>y</td></tr>
#' </table>")
#' sample4 |> html_element("table") |> html_table()
#' sample4 |> html_element("table") |> html_table2()
#'
#' # Use trace = TRUE to keep the source provenance of every value
#' sample5 <- minimal_html("<table>
#'   <tr><th scope='col'>a</th><th scope='col'>b</th></tr>
#'   <tr><td rowspan='2'>1</td><td>2</td></tr>
#'   <tr><td>3</td></tr>
#' </table>")
#' traced <- sample5 |>
#'   html_element("table") |>
#'   html_table(trace = TRUE)
#' traced$body
#' traced$cells
html_table <- function(
  x,
  header = NA,
  trim = TRUE,
  fill = deprecated(),
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  check_bool(header, allow_na = TRUE)
  check_bool(trim)
  check_string(dec)
  check_character(na.strings)
  check_bool(convert)
  check_bool(trace)

  UseMethod("html_table")
}

#' @export
html_table.xml_document <- function(
  x,
  header = NA,
  trim = TRUE,
  fill = deprecated(),
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  tables <- xml2::xml_find_all(x, ".//table")
  html_table(
    tables,
    header = header,
    trim = trim,
    fill = fill,
    dec = dec,
    na.strings = na.strings,
    convert = convert,
    trace = trace
  )
}

#' @export
html_table.xml_nodeset <- function(
  x,
  header = NA,
  trim = TRUE,
  fill = deprecated(),
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  lapply(
    x,
    html_table,
    header = header,
    trim = trim,
    fill = fill,
    dec = dec,
    na.strings = na.strings,
    convert = convert,
    trace = trace
  )
}

#' @export
html_table.xml_node <- function(
  x,
  header = NA,
  trim = TRUE,
  fill = deprecated(),
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  if (lifecycle::is_present(fill)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "html_table(fill)",
      details = "An improved algorithm fills by default so it is no longer needed.",
      user_env = caller_env(2) # S3 generic
    )
  }

  table_parse(x, header, trim, dec, na.strings, convert, html_text, trace)
}

#'
#' @rdname html_table
#' @export
html_table2 <- function(
  x,
  header = NA,
  trim = TRUE,
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  check_bool(header, allow_na = TRUE)
  check_bool(trim)
  check_string(dec)
  check_character(na.strings)
  check_bool(convert)
  check_bool(trace)

  UseMethod("html_table2")
}

#' @export
html_table2.xml_document <- function(
  x,
  header = NA,
  trim = TRUE,
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  tables <- xml2::xml_find_all(x, ".//table")
  html_table2(
    tables,
    header = header,
    trim = trim,
    dec = dec,
    na.strings = na.strings,
    convert = convert,
    trace = trace
  )
}

#' @export
html_table2.xml_nodeset <- function(
  x,
  header = NA,
  trim = TRUE,
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  lapply(
    x,
    html_table2,
    header = header,
    trim = trim,
    dec = dec,
    na.strings = na.strings,
    convert = convert,
    trace = trace
  )
}

#' @export
html_table2.xml_node <- function(
  x,
  header = NA,
  trim = TRUE,
  dec = ".",
  na.strings = "NA",
  convert = TRUE,
  trace = FALSE
) {
  table_parse(x, header, trim, dec, na.strings, convert, html_text2, trace)
}

table_parse <- function(
  x,
  header,
  trim,
  dec,
  na.strings,
  convert,
  text_fun,
  trace = FALSE
) {
  ns <- xml2::xml_ns(x)
  rows <- xml2::xml_find_all(x, ".//tr", ns = ns)
  cells <- lapply(rows, xml2::xml_find_all, ".//td|.//th", ns = ns)
  cells <- compact(cells)

  if (length(cells) == 0) {
    if (isTRUE(trace)) {
      return(table_trace_empty())
    }
    return(tibble::tibble())
  }

  if (isTRUE(trace)) {
    # The trace parser owns grid construction so that provenance ids and
    # output values always share one coordinate system (in particular,
    # nested tables never contribute rows or columns).
    traced <- table_trace_parse(x, trim = trim, text_fun = text_fun)
    out <- table_trace_values(traced)

    if (is.na(header)) {
      header <- length(traced$first_row_tags) > 0L &&
        all(traced$first_row_tags == "th")
    }
    if (header) {
      col_names <- out[1, , drop = FALSE]
      out <- out[-1, , drop = FALSE]
    } else {
      col_names <- paste0("X", seq_len(ncol(out)))
    }
    colnames(out) <- col_names
    df <- tibble::as_tibble(out, .name_repair = "minimal")
    if (isTRUE(convert)) {
      df[] <- lapply(df, function(x) {
        utils::type.convert(x, as.is = TRUE, dec = dec, na.strings = na.strings)
      })
    }
    return(table_trace_final(traced, df, header))
  }

  out <- table_fill(cells, trim = trim, text_fun = text_fun)

  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }

  colnames(out) <- col_names
  df <- tibble::as_tibble(out, .name_repair = "minimal")

  if (isTRUE(convert)) {
    df[] <- lapply(df, function(x) {
      utils::type.convert(x, as.is = TRUE, dec = dec, na.strings = na.strings)
    })
  }

  df
}

# Build the text matrix corresponding to a trace grid, looking each cell
# id up in the registry. Empty slots (padded missing cells) become NA,
# and a spanning cell's text is repeated across every slot it covers.
table_trace_values <- function(traced) {
  grid <- traced$grid
  if (length(grid) == 0L) {
    return(matrix(NA_character_, 0L, 0L))
  }
  text <- traced$registry$text
  names(text) <- traced$registry$cell_id
  out <- unname(text[grid])
  dim(out) <- dim(grid)
  out
}

# Assemble the final trace object, aligning the provenance grid with the
# tibble actually returned (dropping the header row when removed).
table_trace_final <- function(traced, df, header) {
  grid <- traced$grid
  if (isTRUE(header) && nrow(grid) > 0L) {
    grid <- grid[-1L, , drop = FALSE]
  }
  # Padding rows generated by trailing rowspans have no <tr>; the grid
  # already covers them, so no adjustment is needed beyond matching the
  # tibble dimensions.
  body <- grid[, seq_len(ncol(df)), drop = FALSE]
  dimnames(body) <- list(NULL, names(df))

  if (length(traced$problems) > 0L) {
    cli::cli_warn(c(
      "Table source tracing found {length(traced$problems)} problem{?s}:",
      stats::setNames(
        paste0(traced$problems, "."),
        rep("*", length(traced$problems))
      )
    ))
  }

  structure(
    list(
      data = df,
      cells = traced$registry,
      body = body,
      problems = traced$problems
    ),
    class = "rvest_table_trace"
  )
}

table_trace_empty <- function() {
  structure(
    list(
      data = tibble::tibble(),
      cells = table_trace_empty_registry(),
      body = matrix(NA_character_, 0L, 0L),
      problems = character()
    ),
    class = "rvest_table_trace"
  )
}

table_trace_empty_registry <- function() {
  tibble::tibble(
    cell_id = character(),
    html_id = character(),
    tag = character(),
    row = integer(),
    col = integer(),
    rowspan = integer(),
    colspan = integer(),
    covers = list(),
    row_group = character(),
    text = character(),
    headers = list()
  )
}

#' @export
print.rvest_table_trace <- function(x, ...) {
  cat("<rvest_table_trace>\n")
  cat(
    nrow(x$cells),
    " source cell",
    if (nrow(x$cells) == 1L) "" else "s",
    ", ",
    nrow(x$body),
    " output row",
    if (nrow(x$body) == 1L) "" else "s",
    "\n",
    sep = ""
  )
  if (length(x$problems) > 0L) {
    cat(
      length(x$problems),
      " problem",
      if (length(x$problems) == 1L) "" else "s",
      " (see $problems)\n",
      sep = ""
    )
  }
  print(x$data)
  invisible(x)
}

# Table fillng algorithm --------------------------------------------------
# Base on https://html.spec.whatwg.org/multipage/tables.html#forming-a-table

table_fill <- function(cells, trim = TRUE, text_fun = html_text) {
  width <- 0
  height <- length(cells) # initial estimate
  values <- vector("list", height)

  # list of downward spanning cells
  dw <- dw_init()

  # https://html.spec.whatwg.org/multipage/tables.html#algorithm-for-processing-rows
  for (i in seq_along(cells)) {
    row <- cells[[i]]
    if (length(row) == 0) {
      next
    }

    rowspan <- as.integer(html_attr(row, "rowspan", default = NA_character_))
    rowspan[is.na(rowspan)] <- 1
    colspan <- as.integer(html_attr(row, "colspan", default = NA_character_))
    colspan[is.na(colspan)] <- 1
    text <- text_fun(row)
    if (isTRUE(trim)) {
      text <- gsub("^[[:space:]\u00a0]+|[[:space:]\u00a0]+$", "", text)
    }

    vals <- rep(NA_character_, width)
    col <- 1
    j <- 1
    while (j <= length(row)) {
      if (col %in% dw$col) {
        cell <- dw_find(dw, col)
        cell_text <- cell$text
        cell_colspan <- cell$colspan
      } else {
        cell_text <- text[[j]]
        cell_colspan <- colspan[[j]]

        if (rowspan[[j]] > 1) {
          dw <- dw_add(dw, col, rowspan[[j]], colspan[[j]], text[[j]])
        }

        j <- j + 1
      }
      vals[col:(col + cell_colspan - 1L)] <- cell_text
      col <- col + cell_colspan
    }

    # Add any downward cells after last <td>
    for (j in seq2(col - 1L, width)) {
      if (j %in% dw$col) {
        cell <- dw_find(dw, j)
        vals[j:(j + cell$colspan - 1L)] <- cell$text
      }
    }

    dw <- dw_prune(dw)
    values[[i]] <- vals

    height <- max(height, i + max(rowspan) - 1L)
    width <- max(width, col - 1L)
  }

  # Add any downward cells after <tr>
  i <- length(values) + 1
  length(values) <- height
  while (length(dw$col) > 0) {
    vals <- rep(NA_character_, width)
    for (col in dw$col) {
      cell <- dw_find(dw, col)
      vals[col:(col + cell$colspan - 1L)] <- cell$text
    }
    values[[i]] <- vals
    i <- i + 1
    dw <- dw_prune(dw)
  }

  values <- lapply(values, `[`, seq_len(width))
  matrix(unlist(values), ncol = width, byrow = TRUE)
}

dw_find <- function(dw, col) {
  match <- col == dw$col
  list(
    col = dw$col[match],
    rowspan = dw$rowspan[match],
    colspan = dw$colspan[match],
    text = dw$text[match]
  )
}

dw_init <- function() {
  list(
    col = integer(),
    rowspan = integer(),
    colspan = integer(),
    text = character()
  )
}

dw_add <- function(dw, col, rowspan, colspan, text) {
  dw$col <- c(dw$col, col)
  dw$text <- c(dw$text, text)
  dw$rowspan <- c(dw$rowspan, rowspan)
  dw$colspan <- c(dw$colspan, colspan)
  dw
}

dw_prune <- function(dw) {
  dw$rowspan <- dw$rowspan - 1L
  keep <- dw$rowspan > 0L

  dw$col <- dw$col[keep]
  dw$text <- dw$text[keep]
  dw$rowspan <- dw$rowspan[keep]
  dw$colspan <- dw$colspan[keep]
  dw
}
