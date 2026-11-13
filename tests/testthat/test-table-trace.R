# Helpers for table source tracing. Extracts the single trace object
# produced by html_table(trace = TRUE) on a one-table document.
trace_table <- function(html, ...) {
  suppressWarnings(html_table(html_element(html, "table"), trace = TRUE, ...))
}

test_that("trace = FALSE leaves the return shape unchanged", {
  html <- minimal_html(
    "
    <table><tr><th>x</th></tr><tr><td>1</td></tr></table>
  "
  )
  expect_s3_class(html_table(html_element(html, "table")), "tbl_df")
  expect_type(html_table(html, trace = FALSE), "list")
  expect_s3_class(html_table(html, trace = FALSE)[[1]], "tbl_df")
})

test_that("trace returns data, cells, body and problems", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th></tr>
      <tr><td>1</td><td>a</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_s3_class(tr, "rvest_table_trace")
  expect_named(tr, c("data", "cells", "body", "problems"))
  expect_s3_class(tr$data, "tbl_df")
  expect_equal(
    names(tr$cells),
    c(
      "cell_id",
      "html_id",
      "tag",
      "row",
      "col",
      "rowspan",
      "colspan",
      "covers",
      "row_group",
      "text",
      "headers"
    )
  )
  expect_type(tr$body, "character")
  expect_type(tr$problems, "character")
})

test_that("cell ids are stable and every body cell resolves", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th></tr>
      <tr><td>1</td><td>a</td></tr>
      <tr><td>2</td><td>b</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_equal(
    tr$cells$cell_id,
    c("r1c1", "r1c2", "r2c1", "r2c2", "r3c1", "r3c2")
  )
  expect_equal(dim(tr$body), dim(as.matrix(tr$data)))
  expect_true(all(is.na(tr$body) | tr$body %in% tr$cells$cell_id))
  # body coordinates point at the cell whose text the data came from
  expect_equal(unname(tr$body[1, 1]), "r2c1")
  expect_equal(
    tr$cells$text[match(tr$body[1, 1], tr$cells$cell_id)],
    "1"
  )
})

test_that("trace data matches the non-trace tibble", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th><th>z</th></tr>
      <tr><td rowspan='2'>1</td><td>2</td><td>3</td></tr>
      <tr><td>4</td><td>5</td></tr>
    </table>
  "
  )
  plain <- html_table(html_element(html, "table"))
  tr <- trace_table(html)
  expect_equal(tr$data, plain)
})

test_that("spanning cells record coverage and repeat across the body", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th><th>z</th></tr>
      <tr><td rowspan='2' colspan='2'>1</td><td>2</td></tr>
      <tr><td>3</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  span <- tr$cells[tr$cells$cell_id == "r2c1", ]
  expect_equal(span$rowspan, 2L)
  expect_equal(span$colspan, 2L)
  expect_equal(span$covers[[1]]$rows, c(2L, 3L))
  expect_equal(span$covers[[1]]$cols, c(1L, 2L))
  expect_equal(unname(tr$body[1, 1]), "r2c1")
  expect_equal(unname(tr$body[2, 2]), "r2c1")
})

test_that("rowspan = 0 extends only to the end of its row group", {
  html <- minimal_html(
    "
    <table>
      <thead><tr><th>a</th><th>b</th></tr></thead>
      <tbody>
        <tr><td rowspan='0'>x</td><td>1</td></tr>
        <tr><td>2</td></tr>
      </tbody>
      <tfoot><tr><td>foot</td><td>end</td></tr></tfoot>
    </table>
  "
  )
  tr <- trace_table(html, header = TRUE)
  body_rows <- nrow(tr$body)
  # the rowspan=0 cell covers both tbody rows and no further
  span <- tr$cells[tr$cells$text == "x", ]
  expect_equal(span$covers[[1]]$rows, c(2L, 3L))
  expect_equal(span$row_group, "tbody")
  foot <- tr$cells[tr$cells$text == "foot", ]
  expect_equal(foot$row_group, "tfoot")
  expect_equal(foot$covers[[1]]$rows, c(4L, 4L))
  expect_equal(body_rows, 3L)
})

test_that("nested tables do not contribute rows or columns", {
  html <- minimal_html(
    "
    <table>
      <tr><th>a</th><th>b</th></tr>
      <tr>
        <td>outer<table><tr><td>inner</td></tr></table></td>
        <td>2</td>
      </tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_equal(nrow(tr$body), 1L)
  expect_equal(ncol(tr$body), 2L)
  # the nested table's cell is not a separate registry entry
  expect_false(any(tr$cells$text == "inner"))
  expect_equal(nrow(tr$cells), 4L)
  expect_equal(unname(tr$body[1, 1]), "r2c1")
  # text extraction still includes nested content, per html_text()
  expect_match(tr$cells$text[tr$cells$cell_id == "r2c1"], "outer")
})

test_that("nested tables are handled at document level", {
  html <- minimal_html(
    "
    <table>
      <tr><th>a</th><th>b</th></tr>
      <tr><td>o<table><tr><td>i</td></tr></table></td><td>2</td></tr>
    </table>
  "
  )
  # every table (outer and inner) traces without error
  out <- suppressWarnings(html_table(html, trace = TRUE))
  expect_length(out, 2L)
  expect_equal(dim(out[[1]]$body), c(1L, 2L))
  expect_equal(dim(out[[2]]$body), c(1L, 1L))
  expect_equal(out[[2]]$cells$text, "i")
})

test_that("print method summarizes the trace", {
  html <- minimal_html(
    "<table><tr><th>a</th></tr><tr><td>1</td></tr></table>"
  )
  tr <- trace_table(html)
  expect_output(print(tr), "rvest_table_trace")
  expect_output(print(tr), "source cells")
  expect_invisible(print(tr))
})

test_that("multi-level headers associate via scope", {
  html <- minimal_html(
    "
    <table>
      <thead>
        <tr>
          <th scope='colgroup' colspan='2'>region</th>
          <th scope='col'>value</th>
        </tr>
        <tr><th scope='col'>north</th><th scope='col'>south</th><th></th></tr>
      </thead>
      <tbody>
        <tr><th scope='row'>alpha</th><td>1</td><td>2</td></tr>
      </tbody>
    </table>
  "
  )
  tr <- trace_table(html)
  find_headers <- function(id) {
    tr$cells$headers[[match(id, tr$cells$cell_id)]]
  }
  # first body data cell: colgroup header, its column header, and row
  expect_equal(find_headers("r3c2"), c("r1c1", "r2c2", "r3c1"))
  # second data cell belongs to the other column, so no "north" header
  expect_false("r2c1" %in% find_headers("r3c3"))
  expect_equal(find_headers("r3c3"), c("r1c2", "r3c1"))
})

test_that("scope = rowgroup and colgroup use section structure", {
  html <- minimal_html(
    "
    <table>
      <colgroup span='2'></colgroup><colgroup></colgroup>
      <tr>
        <th scope='colgroup' colspan='2'>g1</th><th scope='col'>g2</th>
      </tr>
      <tbody>
        <tr><th scope='rowgroup'>rg</th><td>a</td><td>b</td></tr>
        <tr><td>c</td><td>d</td><td>e</td></tr>
      </tbody>
    </table>
  "
  )
  tr <- trace_table(html, header = FALSE)
  find_headers <- function(id) {
    tr$cells$headers[[match(id, tr$cells$cell_id)]]
  }
  # colgroup header covers both columns of its colgroup
  expect_in("r1c1", find_headers("r2c2"))
  expect_in("r1c1", find_headers("r3c1"))
  expect_false("r1c1" %in% find_headers("r2c3"))
  # rowgroup header applies to every cell in its section
  expect_in("r2c1", find_headers("r3c1"))
  expect_in("r2c1", find_headers("r3c2"))
})

test_that("explicit headers take precedence over scope", {
  html <- minimal_html(
    "
    <table>
      <tr><th id='h1' scope='col'>a</th><th id='h2' scope='col'>b</th></tr>
      <tr><td headers='h2'>x</td><td>y</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  # the explicit reference wins even though scope would pick h1
  expect_equal(tr$cells$headers[[3]], "r1c2")
  # the cell without a headers attribute still uses scope
  expect_equal(tr$cells$headers[[4]], "r1c2")
})

test_that("cells with no resolvable header get an empty list", {
  html <- minimal_html(
    "
    <table>
      <tr><th>a</th><th>b</th></tr>
      <tr><td>x</td><td>y</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_equal(tr$cells$headers[[3]], character())
  expect_type(tr$cells$headers[[3]], "character")
})

test_that("dangling header references are reported as problems", {
  html <- minimal_html(
    "
    <table>
      <tr><th id='h1'>a</th></tr>
      <tr><td headers='h1 ghost'>x</td></tr>
    </table>
  "
  )
  expect_snapshot(
    tr <- html_table(html_element(html, "table"), trace = TRUE)
  )
  expect_match(tr$problems, "Dangling header reference: ghost", all = FALSE)
  # the valid reference still resolves
  expect_equal(tr$cells$headers[[2]], "r1c1")
})

test_that("references into nested tables do not resolve to outer cells", {
  html <- minimal_html(
    "
    <table>
      <tr>
        <td>outer<table><tr><td id='inner'>i</td></tr></table></td>
      </tr>
      <tr><td headers='inner'>v</td></tr>
    </table>
  "
  )
  tr <- trace_table(html, header = FALSE)
  expect_match(
    tr$problems,
    "Header reference does not point to a table cell: inner",
    all = FALSE
  )
  expect_equal(tr$cells$headers[[2]], character())
})

test_that("duplicate ids are reported as problems", {
  html <- minimal_html(
    "
    <table>
      <tr><th id='dup'>a</th><th id='dup'>b</th></tr>
      <tr><td headers='dup'>x</td><td>y</td></tr>
    </table>
  "
  )
  expect_snapshot(
    tr <- html_table(html_element(html, "table"), trace = TRUE)
  )
  expect_match(tr$problems, "Duplicate cell id: dup", all = FALSE)
})

test_that("empty headers attribute resolves to nothing, not scope", {
  html <- minimal_html(
    "
    <table>
      <tr><th id='h1' scope='col'>a</th></tr>
      <tr><td headers=''>x</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_equal(tr$cells$headers[[2]], character())
  expect_equal(tr$problems, character())
})

test_that("whitespace trimming matches html_table conventions", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th></tr>
      <tr><td>  a  </td></tr>
    </table>
  "
  )
  trimmed <- trace_table(html)
  expect_equal(trimmed$cells$text[2], "a")
  raw <- trace_table(html, trim = FALSE)
  expect_equal(raw$cells$text[2], "  a  ")
})

test_that("html_table2 trace keeps text2 cell text", {
  html <- minimal_html(
    "
    <table><tr><th>x</th></tr><tr><td>a<br>b</td></tr></table>
  "
  )
  t1 <- trace_table(html)
  t2 <- suppressWarnings(
    html_table2(html_element(html, "table"), trace = TRUE)
  )
  expect_equal(t1$cells$text[2], "ab")
  expect_equal(t2$cells$text[2], "a\nb")
  expect_equal(t2$data, html_table2(html_element(html, "table")))
})

test_that("coordinates stay aligned after type conversion and NA", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th></tr>
      <tr><td>1</td><td>NA</td></tr>
      <tr><td>2</td><td>z</td></tr>
    </table>
  "
  )
  tr <- trace_table(html)
  expect_type(tr$data$x, "integer")
  expect_true(is.na(tr$data$y[1]))
  # the NA cell still points at its source
  expect_equal(unname(tr$body[1, 2]), "r2c2")
  expect_equal(
    tr$cells$text[match("r2c2", tr$cells$cell_id)],
    "NA"
  )
  unclassed <- trace_table(html, convert = FALSE)
  expect_type(unclassed$data$x, "character")
  expect_equal(dim(unclassed$body), dim(as.matrix(unclassed$data)))
})

test_that("coordinates stay aligned after header removal", {
  html <- minimal_html(
    "
    <table>
      <tr><th>x</th><th>y</th></tr>
      <tr><td>1</td><td>2</td></tr>
    </table>
  "
  )
  kept <- trace_table(html, header = FALSE)
  expect_equal(nrow(kept$body), 2L)
  expect_named(kept$data, c("X1", "X2"))
  removed <- trace_table(html, header = TRUE)
  expect_equal(nrow(removed$body), 1L)
  expect_equal(unname(removed$body[1, ]), c("r2c1", "r2c2"))
})

test_that("trace works on documents and nodesets", {
  html <- minimal_html(
    "
    <table><tr><td>1</td></tr></table>
    <table><tr><td>2</td></tr></table>
  "
  )
  out <- suppressWarnings(html_table(html, trace = TRUE))
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_s3_class(out[[1]], "rvest_table_trace")
  expect_equal(out[[1]]$data[[1]][1], 1L)
  expect_equal(out[[2]]$data[[1]][1], 2L)
})

test_that("trace handles empty tables", {
  html <- minimal_html("<table></table>")
  tr <- suppressWarnings(html_table(html_element(html, "table"), trace = TRUE))
  expect_s3_class(tr, "rvest_table_trace")
  expect_equal(nrow(tr$cells), 0L)
  expect_equal(dim(tr$body), c(0L, 0L))
  expect_equal(tr$problems, character())
  expect_equal(tr$data, tibble::tibble())
})

test_that("trace validates its argument", {
  html <- minimal_html("<table><tr><td>1</td></tr></table>")
  expect_error(
    html_table(html_element(html, "table"), trace = "yes"),
    class = "rlang_error"
  )
})
