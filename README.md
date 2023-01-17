
<!-- README.md is generated from README.Rmd. Please edit that file -->

# atumscot

<!-- badges: start -->
<!-- badges: end -->

To reproduced code in this repository first clone it, for example by
installing the GitHub CLI tools and running:

``` bash
gh repo clone atumscot/atumscot
```

Then run the following command (requires dependencies and data to be
present):

``` r
targets::tar_make()
```

Note: you need to have a CYCLESTREETS API key for this, see here for
details:
<https://rpackage.cyclestreets.net/reference/journey.html#details-1>

This project uses `targets` for data processing pipeline management,
with outputs like this:

![](https://user-images.githubusercontent.com/1825120/205490893-b1627e3a-5102-4dbe-bc70-97e358e75506.png)

Visualise the project as follows:

``` r
# targets::tar_visnetwork(targets_only = TRUE)
```

The zones in the case study region are as follows:

![](README_files/figure-gfm/zones-1.png)<!-- -->

Baseline cycling levels are shown below:

``` r
tm_shape(rnet) +
  tm_lines(lwd = "bicycle", scale = 9)
```

![](README_files/figure-gfm/overline-1.png)<!-- -->
