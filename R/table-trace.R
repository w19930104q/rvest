# Table source tracing ---------------------------------------------------
# Builds a provenance record for every cell of a table, following the
# table model from https://html.spec.whatwg.org/multipage/tables.html
# (grid construction, spanning cells, row groups). Only used when
# `html_table(trace = TRUE)` is requested, so the default code path is
# unchanged.

# Parse a table into a trace object: a registry of every source cell,
# an id matrix aligned with the (pre-header-removal) table grid, and a
# character vector of diagnostic problems.
table_trace_parse <- function(x, trim, text_fun) {
  ns <- xml2::xml_ns(x)

  sections <- table_sections(x, ns)
  cells <- table_direct_cells(x, sections, ns)
  problems <- table_duplicate_ids(cells)

  n_rows <- length(cells)
  grid_rows <- vector("list", n_rows)
  width <- 0L

  if (n_rows > 0L) {
    dw <- dw_init_trace()
    for (i in seq_len(n_rows)) {
      row <- cells[[i]]
      vals <- rep(NA_character_, width)

      if (length(row$nodes) > 0L) {
        col <- 1L
        j <- 1L
        while (j <= length(row$nodes)) {
          if (col %in% dw$col) {
            cell <- dw_find_trace(dw, col)
            cell_span <- cell$colspan
            cell_id <- cell$id
          } else {
            cell_span <- row$colspan[[j]]
            cell_id <- row$id[[j]]
            if (row$rowspan[[j]] == 0L) {
              # rowspan = 0 extends to the end of the current row group.
              dw <- dw_add_trace(
                dw,
                col,
                row$group_end[[j]],
                cell_span,
                cell_id
              )
            } else if (row$rowspan[[j]] > 1L) {
              dw <- dw_add_trace(
                dw,
                col,
                i + row$rowspan[[j]] - 1L,
                cell_span,
                cell_id
              )
            }
            j <- j + 1L
          }
          length(vals) <- max(length(vals), col + cell_span - 1L)
          vals[col:(col + cell_span - 1L)] <- cell_id
          col <- col + cell_span
        }

        # Downward cells that extend past the last cell in this row.
        for (k in seq2(col - 1L, length(vals))) {
          if (k %in% dw$col) {
            cell <- dw_find_trace(dw, k)
            vals[k:(k + cell$colspan - 1L)] <- cell$id
          }
        }
      }
      dw <- dw_prune_trace(dw, i)
      grid_rows[[i]] <- vals
      width <- max(width, length(vals))
    }

    # Trailing rowspans add padding rows beyond the last non-empty <tr>,
    # mirroring table_fill().
    while (length(dw$col) > 0L) {
      vals <- rep(NA_character_, width)
      for (col in dw$col) {
        cell <- dw_find_trace(dw, col)
        vals[col:(col + cell$colspan - 1L)] <- cell$id
      }
      grid_rows[[length(grid_rows) + 1L]] <- vals
      dw <- dw_prune_trace(dw, length(grid_rows))
    }
  }

  grid_rows <- compact(grid_rows)
  n_rows <- length(grid_rows)
  grid <- matrix(NA_character_, nrow = n_rows, ncol = width)
  for (i in seq_len(n_rows)) {
    if (length(grid_rows[[i]]) > 0L) {
      grid[i, seq_along(grid_rows[[i]])] <- grid_rows[[i]]
    }
  }

  # Annotate each cell with the coverage it occupies in the grid.
  cell_index <- table_cell_index(cells)
  for (id in names(cell_index)) {
    loc <- which(grid == id, arr.ind = TRUE)
    if (nrow(loc) > 0L) {
      entry <- cell_index[[id]]
      entry$r1 <- min(loc[, "row"])
      entry$r2 <- max(loc[, "row"])
      entry$c1 <- min(loc[, "col"])
      entry$c2 <- max(loc[, "col"])
      cell_index[[id]] <- entry
    }
  }

  col_groups <- table_col_groups(x, ns, width)
  first_row_tags <- if (length(cells) > 0L) {
    vapply(cells[[1L]]$nodes, xml2::xml_name, character(1))
  } else {
    character()
  }
  registry <- table_cell_registry(
    cell_index,
    col_groups,
    ns,
    trim,
    text_fun,
    problems
  )
  problems <- registry$problems
  registry <- registry$registry

  list(
    grid = grid,
    registry = registry,
    problems = unique(problems),
    first_row_tags = first_row_tags
  )
}

# Sections (thead/tbody/tfoot, including implicit tbody sections for
# loose rows) and the direct <tr> rows belonging to each. Rows are kept
# in document order so a <tr> sitting outside any section element is not
# silently dropped.
table_sections <- function(table, ns) {
  kids <- xml2::xml_find_all(table, "./thead|./tbody|./tfoot|./tr", ns)
  if (length(kids) == 0L) {
    return(list(list(kind = "tbody", rows = list(), end = 0L)))
  }

  out <- list()
  row_start <- 1L
  loose <- list()
  flush_loose <- function() {
    if (length(loose) > 0L) {
      n <- length(loose)
      out[[length(out) + 1L]] <<- list(
        kind = "tbody",
        rows = loose,
        end = row_start + n - 1L
      )
      row_start <<- row_start + n
      loose <<- list()
    }
  }

  for (kid in kids) {
    if (xml2::xml_name(kid) == "tr") {
      loose[[length(loose) + 1L]] <- kid
    } else {
      flush_loose()
      rows <- table_direct_rows(table, kid, ns)
      n <- length(rows)
      out[[length(out) + 1L]] <- list(
        kind = xml2::xml_name(kid),
        rows = rows,
        end = row_start + n - 1L
      )
      row_start <- row_start + n
    }
  }
  flush_loose()
  out
}

# Whether `node` belongs directly to `table`, i.e. its nearest table
# ancestor is `table` itself. Path comparison works even when `table` is
# itself nested inside another table.
table_is_direct <- function(table, node, ns) {
  ancestors <- xml2::xml_find_all(node, "ancestor::table", ns)
  length(ancestors) > 0L &&
    xml2::xml_path(ancestors[[length(ancestors)]]) == xml2::xml_path(table)
}

# <tr> elements that are not inside a table nested within `table`.
table_direct_rows <- function(table, x, ns) {
  rows <- xml2::xml_find_all(x, ".//tr", ns)
  Filter(
    function(row) table_is_direct(table, row, ns),
    rows
  )
}

# Direct td/th cells of every row, i.e. cells whose nearest table
# ancestor is `x`. Cells inside nested tables are excluded so a nested
# table can never contribute rows or columns to the outer table. Empty
# rows are skipped, matching table_fill()'s compact() call.
table_direct_cells <- function(table, sections, ns) {
  out <- list()
  row_index <- 0L
  for (section in sections) {
    section_rows <- list()
    for (row in section$rows) {
      nodes <- Filter(
        function(cell) table_is_direct(table, cell, ns),
        xml2::xml_find_all(row, ".//td|.//th", ns)
      )
      if (length(nodes) == 0L) {
        next
      }
      section_rows[[length(section_rows) + 1L]] <- nodes
    }

    group_end <- row_index + length(section_rows)
    for (nodes in section_rows) {
      row_index <- row_index + 1L
      out[[row_index]] <- list(
        nodes = nodes,
        rowspan = table_parse_span(nodes, "rowspan"),
        colspan = table_parse_span(nodes, "colspan"),
        id = table_cell_id(row_index, seq_along(nodes)),
        group_end = rep(group_end, length(nodes)),
        group = section$kind,
        row_index = row_index
      )
    }
  }
  out
}

# Parse rowspan/colspan attributes following the HTML spec: missing or
# non-numeric values default to 1, 0 is allowed (rowspan only), and
# values are clamped to 1000.
table_parse_span <- function(nodes, attr) {
  if (length(nodes) == 0L) {
    return(integer())
  }
  raw <- suppressWarnings(
    as.integer(html_attr(nodes, attr, default = NA_character_))
  )
  if (attr == "rowspan") {
    is_zero <- !is.na(raw) & raw == 0L
    raw[is.na(raw) | raw < 0L] <- 1L
    raw[is_zero] <- 0L
  } else {
    raw[is.na(raw) | raw < 1L] <- 1L
  }
  pmin(raw, 1000L)
}

# Column ranges covered by each <colgroup>, for scope = "colgroup".
table_col_groups <- function(x, ns, width) {
  groups <- xml2::xml_find_all(x, "./colgroup", ns)
  if (length(groups) == 0L || width == 0L) {
    return(list())
  }
  spans <- suppressWarnings(
    as.integer(html_attr(groups, "span", default = NA_character_))
  )
  spans[is.na(spans) | spans < 1L] <- 1L
  spans <- pmin(spans, 1000L)

  out <- vector("list", length(groups))
  start <- 1L
  for (i in seq_along(groups)) {
    end <- min(start + spans[[i]] - 1L, width)
    out[[i]] <- start:end
    start <- end + 1L
  }
  out
}

table_duplicate_ids <- function(cells) {
  ids <- unlist(lapply(cells, function(row) {
    if (length(row$nodes) == 0L) {
      return(character())
    }
    html_attr(row$nodes, "id", default = NA_character_)
  }))
  if (length(ids) == 0L) {
    return(character())
  }
  dupes <- unique(ids[!is.na(ids) & duplicated(ids)])
  if (length(dupes) == 0L) {
    return(character())
  }
  paste0("Duplicate cell id: ", dupes)
}

# Flatten rows into a lookup keyed by stable cell id.
table_cell_index <- function(cells) {
  index <- list()
  for (row in cells) {
    for (j in seq_along(row$nodes)) {
      id <- row$id[[j]]
      index[[id]] <- list(
        node = row$nodes[[j]],
        group = row$group,
        r1 = NA_integer_,
        r2 = NA_integer_,
        c1 = NA_integer_,
        c2 = NA_integer_
      )
    }
  }
  index
}

# Build one registry record per source cell, including the associated
# header ids and any diagnostics raised while resolving them.
table_cell_registry <- function(
  cell_index,
  col_groups,
  ns,
  trim,
  text_fun,
  problems
) {
  n <- length(cell_index)
  registry <- tibble::tibble(
    cell_id = names(cell_index),
    html_id = character(n),
    tag = character(n),
    row = integer(n),
    col = integer(n),
    rowspan = integer(n),
    colspan = integer(n),
    covers = vector("list", n),
    row_group = character(n),
    text = character(n),
    headers = vector("list", n)
  )

  for (k in seq_along(cell_index)) {
    id <- names(cell_index)[[k]]
    entry <- cell_index[[id]]
    node <- entry$node

    text <- text_fun(node)
    if (isTRUE(trim)) {
      text <- gsub("^[[:space:]\u00a0]+|[[:space:]\u00a0]+$", "", text)
    }

    header_result <- table_headers_for(
      node = node,
      entry = entry,
      cell_index = cell_index,
      col_groups = col_groups,
      ns = ns
    )
    problems <- c(problems, header_result$problems)

    registry$html_id[[k]] <- xml2::xml_attr(node, "id")
    registry$tag[[k]] <- xml2::xml_name(node)
    registry$row[[k]] <- entry$r1
    registry$col[[k]] <- entry$c1
    registry$rowspan[[k]] <- entry$r2 - entry$r1 + 1L
    registry$colspan[[k]] <- entry$c2 - entry$c1 + 1L
    registry$covers[[k]] <- list(
      rows = c(entry$r1, entry$r2),
      cols = c(entry$c1, entry$c2)
    )
    registry$row_group[[k]] <- entry$group
    registry$text[[k]] <- text
    registry$headers[[k]] <- header_result$ids
  }

  list(registry = registry, problems = problems)
}

# Stable in-table identifier derived from the cell's anchor position.
table_cell_id <- function(row, col) {
  paste0("r", row, "c", col)
}

# Resolve the headers associated with a single cell. Explicit `headers`
# references always take precedence; otherwise association is derived
# from the `scope` attributes of the table's header cells: a header
# applies when its scope covers the target cell's position.
table_headers_for <- function(node, entry, cell_index, col_groups, ns) {
  explicit <- table_explicit_headers(node, cell_index, ns)
  if (!is.null(explicit)) {
    return(explicit)
  }

  hits <- character()
  for (id in names(cell_index)) {
    h <- cell_index[[id]]
    if (xml2::xml_name(h$node) != "th") {
      next
    }
    scope <- xml2::xml_attr(h$node, "scope")
    if (is.na(scope)) {
      next
    }
    # Headers inside <thead> describe body cells, not other header rows.
    if (h$group == "thead" && entry$group == "thead") {
      next
    }
    covered <- switch(
      scope,
      col = h$c2 >= entry$c1 && h$c1 <= entry$c2,
      row = h$r2 >= entry$r1 && h$r1 <= entry$r2,
      colgroup = table_in_same_colgroup(h, entry, col_groups),
      rowgroup = h$group == entry$group,
      FALSE
    )
    if (isTRUE(covered)) {
      hits <- c(hits, id)
    }
  }
  list(ids = unique(hits), problems = character())
}

# Whether a colgroup-scoped header and a target cell share at least one
# <colgroup>. If there are no colgroups, fall back to column overlap.
table_in_same_colgroup <- function(header, entry, col_groups) {
  if (length(col_groups) == 0L) {
    return(header$c2 >= entry$c1 && header$c1 <= entry$c2)
  }
  header_cols <- header$c1:header$c2
  entry_cols <- entry$c1:entry$c2
  for (range in col_groups) {
    if (any(header_cols %in% range) && any(entry_cols %in% range)) {
      return(TRUE)
    }
  }
  FALSE
}

# Returns NULL when the cell has no `headers` attribute; otherwise the
# resolved (possibly empty) header id vector plus diagnostics for
# dangling or invalid references.
table_explicit_headers <- function(node, cell_index, ns) {
  raw <- xml2::xml_attr(node, "headers")
  if (is.na(raw)) {
    return(NULL)
  }
  tokens <- strsplit(trimws(raw), "[[:space:]]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0L) {
    return(list(ids = character(), problems = character()))
  }

  table <- xml2::xml_find_first(node, "ancestor::table", ns)
  problems <- character()
  ids <- character()
  for (token in tokens) {
    matches <- xml2::xml_find_all(
      table,
      paste0(".//*[@id=", xpath_quote(token), "]"),
      ns
    )
    if (length(matches) == 0L) {
      problems <- c(problems, paste0("Dangling header reference: ", token))
      next
    }
    target <- table_find_cell(matches[[1]], cell_index, ns)
    if (is.null(target)) {
      problems <- c(
        problems,
        paste0("Header reference does not point to a table cell: ", token)
      )
      next
    }
    ids <- c(ids, target)
  }
  list(ids = unique(ids), problems = problems)
}

# Map an arbitrary table node back to the stable id of the direct cell
# that contains it (the node itself, or an ancestor within the table).
# Returns NULL if the walk reaches a nested table boundary first, since a
# cell in a nested table is not a cell of this table.
table_find_cell <- function(node, cell_index, ns) {
  paths <- vapply(
    cell_index,
    function(entry) xml2::xml_path(entry$node),
    character(1)
  )
  node_path <- xml2::xml_path(node)
  if (node_path %in% paths) {
    return(names(paths)[match(node_path, paths)])
  }
  # Nested elements inside a cell: walk up until we reach either a direct
  # cell or another (nested) table boundary.
  repeat {
    parent <- xml2::xml_parent(node)
    if (inherits(parent, "xml_missing") || is.na(xml2::xml_path(parent))) {
      return(NULL)
    }
    node <- parent
    if (xml2::xml_name(node) == "table") {
      return(NULL)
    }
    node_path <- xml2::xml_path(node)
    if (node_path %in% paths) {
      return(names(paths)[match(node_path, paths)])
    }
  }
}

xpath_quote <- function(x) {
  if (!grepl("\"", x, fixed = TRUE)) {
    return(paste0("\"", x, "\""))
  }
  parts <- strsplit(x, "\"", fixed = TRUE)[[1]]
  paste0("concat(", paste0("\"", parts, "\"", collapse = ", '\"', "), ")")
}

# Downward spanning cells used by the trace grid parser. These track
# cell ids rather than text, and remember the row at which the span
# ends.
dw_init_trace <- function() {
  list(
    col = integer(),
    end_row = integer(),
    colspan = integer(),
    id = character()
  )
}

dw_add_trace <- function(dw, col, end_row, colspan, id) {
  dw$col <- c(dw$col, col)
  dw$end_row <- c(dw$end_row, end_row)
  dw$colspan <- c(dw$colspan, colspan)
  dw$id <- c(dw$id, id)
  dw
}

dw_find_trace <- function(dw, col) {
  match <- col == dw$col
  list(
    col = dw$col[match],
    end_row = dw$end_row[match],
    colspan = dw$colspan[match],
    id = dw$id[match]
  )
}

dw_prune_trace <- function(dw, current_row) {
  keep <- dw$end_row > current_row
  dw$col <- dw$col[keep]
  dw$end_row <- dw$end_row[keep]
  dw$colspan <- dw$colspan[keep]
  dw$id <- dw$id[keep]
  dw
}
