# Regenerates generated/panel-xmap-classes-simple-links.{png,pdf} -- the xmap
# classes panel used as @fig-xmap-classes in the JCGS paper (issue #6).
#
# Styled after the 2023 ASC poster panel (screenshots/asc-poster.png), minus
# its mock-up of apply_xmap(). Inputs stack in argument order (.data above
# .xmap), and key columns are tinted as in diagram_crossmap-transform: source
# keys blue, target keys green. Every console block is captured from the real
# objects rather than typed, and the apply_xmap() call is parsed, highlighted
# and evaluated from the same text, so the panel always matches what xmap
# does. simple_xmap and simple_stats are built as in
# vignette("applying-crossmaps").
#
# Usage, from src/:
#   XMAP_DIR=/path/to/xmap OUT_DIR=/path/to/collection/generated \
#     Rscript panel_xmap-classes-simple-links.R

library(grid)

xmap_dir <- Sys.getenv("XMAP_DIR", unset = "~/Dropbox/WORK/PROJECTS/CROSSMAPS/SOFTWARE/xmap")
out_dir <- Sys.getenv("OUT_DIR", unset = file.path(dirname(getwd()), "generated"))
out_name <- "panel-xmap-classes-simple-links"

suppressMessages(devtools::load_all(xmap_dir, quiet = TRUE))
options(width = 60, cli.num_colors = 1)

simple_xmap <- demo$simple_links |>
  as_xmap_tbl(xcode, alphacode, weight)
simple_stats <- demo$simple_stats

call_code <- c(
  "apply_xmap(.data = simple_stats,",
  "           .xmap = simple_xmap,",
  "           values_from = count,",
  "           keys_from = xcode)"
)
call_expr <- parse(text = call_code, keep.source = TRUE)
simple_out <- eval(call_expr[[1]])
stopifnot(sum(simple_out$count) == sum(simple_stats$count))

printed <- function(x) capture.output(print(x))
xmap_lines <- printed(simple_xmap)
stats_lines <- printed(simple_stats)
out_lines <- printed(simple_out)

## ---- key tints: source keys blue, target keys green -----------------------
col_src <- "#A9C1DE"
col_tgt <- "#A3BDAE"

# `text` on its first line; as a column, runs from there to the last row
tint <- function(lines, text, fill, column = TRUE, width = nchar(text)) {
  i <- grep(text, lines, fixed = TRUE)[1]
  list(rows = if (column) i:length(lines) else i,
       start = regexpr(text, lines[i], fixed = TRUE), width = width, fill = fill)
}
xmap_tints <- list(
  tint(xmap_lines, "[7] xcode", col_src, column = FALSE),
  tint(xmap_lines, "[6] alphacode", col_tgt, column = FALSE),
  tint(xmap_lines, ".from$xcode", col_src),
  tint(xmap_lines, ".to$alphacode", col_tgt)
)
stats_tints <- list(tint(stats_lines, "xcode count", col_src, width = nchar("xcode")))
out_tints <- list(tint(out_lines, "alphacode count", col_tgt, width = nchar("alphacode")))

## ---- syntax highlighting, from R's own parser -----------------------------
syntax_col <- c(
  SYMBOL_FUNCTION_CALL = "#A56FD6",
  SYMBOL_SUB = "#7E8BD8",
  EQ_SUB = "#4A86C8",
  LEFT_ASSIGN = "#4A86C8",
  PIPE = "#E08A3C",
  STR_CONST = "#8DB33A",
  NUM_CONST = "#D9822B"
)
default_col <- "#1F3A5F"
tokens <- getParseData(call_expr)
tokens <- tokens[tokens$terminal, c("line1", "col1", "text", "token")]
tokens$col <- ifelse(tokens$token %in% names(syntax_col),
                     syntax_col[tokens$token], default_col)

## ---- geometry (inches, laid out top-down) ---------------------------------
mono <- "Menlo"
fs <- 9
# measured on the device that draws the text, not assumed: a nominal 0.6 em
# drifts the tints off the text by the end of a 50-character line
measure_cw <- function() {
  ragg::agg_png(tempfile(fileext = ".png"), width = 1, height = 1, units = "in", res = 400)
  on.exit(dev.off())
  pushViewport(viewport(gp = gpar(fontfamily = mono, fontsize = fs)))
  convertWidth(stringWidth(strrep("0", 100)), "in", valueOnly = TRUE) / 100
}
cw <- measure_cw()
lh <- 1.22 * fs / 72
px <- 0.06                    # text inset in the grey output blocks
py <- 0.05
bpad <- 0.08                  # text inset in the white boxes
label_h <- lh + 2 * 0.05
gap_label <- 0.05             # white box to its grey block
gap_v <- 0.22
gap_h <- 0.4
margin <- 0.12

text_w <- function(lines) max(nchar(lines, type = "width")) * cw
grey_h <- function(lines) length(lines) * lh + 2 * py

left_w <- max(text_w(xmap_lines), text_w(stats_lines)) + 2 * px
stats_label_top <- margin
stats_top <- stats_label_top + label_h + gap_label
xmap_label_top <- stats_top + grey_h(stats_lines) + gap_v
xmap_top <- xmap_label_top + label_h + gap_label
H <- xmap_top + grey_h(xmap_lines) + margin

# arrows leave the array near its header and the crossmap below its middle;
# the call and its result sit centred between them
arrow1_y <- stats_top + py + 1.5 * lh
arrow2_y <- xmap_top + grey_h(xmap_lines) * 0.65
right_x <- margin + left_w + gap_h
call_w <- max(text_w(call_code), text_w(out_lines)) + 2 * bpad
call_h <- length(call_code) * lh + 2 * bpad
right_h <- call_h + gap_label + grey_h(out_lines)
call_top <- (arrow1_y + arrow2_y) / 2 - right_h / 2
out_top <- call_top + call_h + gap_label
bend_x <- right_x + call_w / 3
W <- right_x + call_w + margin

## ---- drawing --------------------------------------------------------------
ny <- function(t) H - t       # native y runs bottom-up

# One glyph per monospace cell, so text lands on the same grid as the tints on
# every device. Drawing whole strings lets each device space them by its own
# metrics, and cairo_pdf reports a narrower advance for Menlo than ragg does.
split_cells <- function(text, cell0, y, col) {
  ch <- strsplit(text, "")
  n <- lengths(ch)
  idx <- rep(seq_along(text), n)
  list(chars = unlist(ch),
       cell = rep_len(cell0, length(text))[idx] + sequence(n) - 1,
       y = rep_len(y, length(text))[idx],
       col = rep_len(col, length(text))[idx])
}
glyphs <- function(g, x0) {
  keep <- g$chars != " "
  grid.text(g$chars[keep], x0 + (g$cell[keep] - 0.5) * cw, g$y[keep],
            just = "centre", default.units = "native",
            gp = gpar(fontfamily = mono, fontsize = fs, col = g$col[keep]))
}

white_box <- function(x, top, w, h) {
  grid.roundrect(x, ny(top), w, h, r = unit(0.03, "in"), just = c("left", "top"),
                 default.units = "native",
                 gp = gpar(fill = "white", col = "#9A9A9A", lwd = 0.9))
}

grey_block <- function(lines, x, top, w, tints = list()) {
  grid.rect(x, ny(top), w, grey_h(lines), just = c("left", "top"),
            default.units = "native", gp = gpar(fill = "#E5E5E5", col = NA))
  # inset top and bottom so tints on adjacent lines stay separate
  inset <- 0.06 * lh
  for (tn in tints) {
    grid.rect(x + px + (tn$start - 1.15) * cw,
              ny(top + py + (min(tn$rows) - 1) * lh + inset),
              (tn$width + 0.3) * cw, length(tn$rows) * lh - 2 * inset,
              just = c("left", "top"), default.units = "native",
              gp = gpar(fill = tn$fill, col = NA))
  }
  glyphs(split_cells(lines, 1, ny(top + py + (seq_along(lines) - 0.5) * lh),
                     "#111111"), x + px)
}

object_block <- function(name, lines, label_top, top, tints) {
  white_box(margin, label_top, left_w, label_h)
  grid.text(name, margin + px, ny(label_top + label_h / 2), just = "left",
            default.units = "native",
            gp = gpar(fontfamily = mono, fontsize = fs, col = default_col))
  grey_block(lines, margin, top, left_w, tints)
}

# thick elbow arrow: across from (x1, y1), round the corner, then up/down to y2
elbow <- function(x1, y1, xb, y2, body = 0.11, r = 0.15,
                  head_w = 0.26, head_l = 0.15,
                  col = syntax_col[["SYMBOL_SUB"]]) {
  dir <- sign(y2 - y1)
  t <- seq(0, pi / 2, length.out = 16)
  xs <- c(x1, xb - r + r * sin(t), xb)
  ys <- c(y1, y1 + dir * r * (1 - cos(t)), y2 - dir * head_l)
  grid.lines(xs, ny(ys), default.units = "native",
             gp = gpar(col = col, lwd = body * 96, lineend = "butt", linejoin = "round"))
  grid.polygon(c(xb - head_w / 2, xb + head_w / 2, xb),
               ny(c(y2 - dir * head_l, y2 - dir * head_l, y2)),
               default.units = "native", gp = gpar(fill = col, col = NA))
}

draw_panel <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))
  elbow(margin + left_w, arrow1_y, bend_x, call_top - 0.03)
  elbow(margin + left_w, arrow2_y, bend_x, out_top + grey_h(out_lines) + 0.03)
  object_block("simple_stats", stats_lines, stats_label_top, stats_top, stats_tints)
  object_block("simple_xmap", xmap_lines, xmap_label_top, xmap_top, xmap_tints)
  white_box(right_x, call_top, call_w, call_h)
  glyphs(split_cells(tokens$text, tokens$col1,
                     ny(call_top + bpad + (tokens$line1 - 0.5) * lh), tokens$col),
         right_x + bpad)
  grey_block(out_lines, right_x, out_top, call_w, out_tints)
  popViewport()
}

ragg::agg_png(file.path(out_dir, paste0(out_name, ".png")),
              width = W, height = H, units = "in", res = 400, background = "white")
draw_panel()
invisible(dev.off())

cairo_pdf(file.path(out_dir, paste0(out_name, ".pdf")), width = W, height = H)
draw_panel()
invisible(dev.off())
