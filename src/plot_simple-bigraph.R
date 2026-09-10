# Regenerates plots/plot-simple-bigraph.png -- the node-link bigraph of
# demo$simple_links used as @fig-simple-bigraph in the JCGS paper.
#
# The plotting helper is not exported by xmap; it lives in an `include: false`
# chunk of vignette("applying-crossmaps") (xmap#51). Rather than duplicate it
# here and let the two drift, this purls the vignette and evaluates it up to
# the bigraph call, so the figure always matches what the vignette renders.
#
# Usage, from a checkout of cynthiahqy/xmap:
#   XMAP_DIR=/path/to/xmap OUT_DIR=/path/to/collection/plots Rscript plot_simple-bigraph.R

xmap_dir <- Sys.getenv("XMAP_DIR", unset = "~/Dropbox/WORK/PROJECTS/CROSSMAPS/SOFTWARE/xmap")
out_dir <- Sys.getenv("OUT_DIR", unset = file.path(dirname(getwd()), "plots"))

withr::with_dir(xmap_dir, {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
  code <- knitr::purl("vignettes/applying-crossmaps.Rmd",
                      output = tempfile(), quiet = TRUE)
  lines <- readLines(code)
  stop_at <- grep("plot_xmap_bigraph(simple_xmap)", lines, fixed = TRUE)[1]
  eval(parse(text = paste(lines[1:stop_at], collapse = "\n")),
       envir = globalenv())
})

ggplot2::ggsave(
  file.path(out_dir, "plot-simple-bigraph.png"),
  plot_xmap_bigraph(simple_xmap),
  width = 5.5, height = 4.5, dpi = 400, bg = "white"
)
