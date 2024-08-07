# Images for Crossmaps Project


## Submoduling Instructions

Run these commands from inside the target repo (i.e. the one you want
these images to be available in)

To add the contents of the latest commit of the default branch of this
repo as the folder `images/`:

``` zsh
git submodule add https://github.com/cynthiahqy/collection_crossmap-images.git images
```

Updating contents to match the latest commit:

``` zsh
git submodule update --remote --merge
```

to match remote:

``` zsh
git submodule update --remote --rebase
```

## Useful ImageMagick Commands

Crop & transparent background:

``` zsh
convert filename.png -trim -transparent white filename.png
```

For details see:
<https://www.cynthiahqy.com/posts/imagemagick-basic-trim/>

## Images

## Illustrations

illustrations/diagram_crossmap-transform-latex.png

![](illustrations/diagram_crossmap-transform-latex.png)

illustrations/diagram_current-prov.png

![](illustrations/diagram_current-prov.png)

illustrations/diagram_current-workflow.png

![](illustrations/diagram_current-workflow.png)

illustrations/diagram_ex-post-process.png

![](illustrations/diagram_ex-post-process.png)

illustrations/diagram_framework-workflow.png

![](illustrations/diagram_framework-workflow.png)

illustrations/diagram_preprocessing-solutions.png

![](illustrations/diagram_preprocessing-solutions.png)

illustrations/icon-database.png

![](illustrations/icon-database.png)

illustrations/icon-IEEE-VIS.png

![](illustrations/icon-IEEE-VIS.png)

illustrations/icon-official-stats.png

![](illustrations/icon-official-stats.png)

illustrations/plot_aus-agg.png

![](illustrations/plot_aus-agg.png)

illustrations/plot_aus-split.png

![](illustrations/plot_aus-split.png)

illustrations/xmap-hex-sticker.png

![](illustrations/xmap-hex-sticker.png)

## Plots

plots/plot-anzsco-isco-bigraph-with-table.png

![](plots/plot-anzsco-isco-bigraph-with-table.png)

plots/plot-isiccomb-split-by-income-groups.png

![](plots/plot-isiccomb-split-by-income-groups.png)

## Screenshots

screenshots/asc-poster-basic-implications.png

![](screenshots/asc-poster-basic-implications.png)

screenshots/asc-poster.png

![](screenshots/asc-poster.png)

screenshots/screenshot_visxprov-ctry-proposed.png

![](screenshots/screenshot_visxprov-ctry-proposed.png)
