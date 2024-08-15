# QA on utility trips


``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(targets)
```

We’ll load data with `tar_load()` and check the utility trips.

``` r
if (!file.exists("_targets.R")) {
  old_working_directory = setwd("../..")
} else {
  old_working_directory = getwd()
}
# tar_make()
tar_source()
```

    Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE


    Attaching package: 'tidygraph'

    The following object is masked from 'package:stats':

        filter


    Attaching package: 'igraph'

    The following object is masked from 'package:tidygraph':

        groups

    The following objects are masked from 'package:lubridate':

        %--%, union

    The following objects are masked from 'package:dplyr':

        as_data_frame, groups, union

    The following objects are masked from 'package:purrr':

        compose, simplify

    The following object is masked from 'package:tidyr':

        crossing

    The following object is masked from 'package:tibble':

        as_data_frame

    The following objects are masked from 'package:stats':

        decompose, spectrum

    The following object is masked from 'package:base':

        union

``` r
tar_load(parameters)
tar_load(zones_stats)
tar_load(utility_stats)
tar_load(od_shopping)
tar_load(od_utility_combined)
tar_load(aadt_parameters)
tar_load(zones)
names(zones)
```

    [1] "DataZone"   "TotPop2011" "ResPop2011" "HHCnt2011"  "geometry"  

``` r
# parameters
setwd(old_working_directory)
```

Starting from first principles, there are 265k people in the case study
area.

``` r
sum(zones$TotPop2011)
```

    [1] 265194

``` r
summary(zones$TotPop2011)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      405.0   637.8   774.5   770.9   891.0  1184.0 

``` r
zones |>
   select(TotPop2011) |>
   plot()
```

![](utility-checks_files/figure-commonmark/unnamed-chunk-3-1.png)

People make around 3 trips to the shops per week, meaning 3 / 7 (0.43)
trips per day. We would therefore expect around 265k \* 0.43 = 114k
shopping trips per day.

``` r
265 * 0.43
```

    [1] 113.95

Let’s check the number of shopping trips in the utility stats.

``` r
sum(utility_stats$shopping_orig_all, na.rm = TRUE)
```

    [1] 118299.4

And in the od data:

``` r
sum(od_shopping$all)
```

    [1] 118299.4

The numbers match-up for the baseline input data. The issue is
associated with scenarios other than the baseline, e.g. Go Dutch. Let’s
check the number of shopping trips in the utility stats for the Go Dutch
scenario.

``` r
sum(utility_stats$shopping_orig_all, na.rm = TRUE)
```

    [1] 118299.4

``` r
sum(utility_stats$shopping_dest_car, na.rm = TRUE)
```

    [1] 77272.48

``` r
sum(utility_stats$shopping_orig_car_go_dutch_fastest, na.rm = TRUE)
```

    [1] 9275.791

All the modes should add up to the total, let’s check this.

``` r
# For the baseline scenario:
sum(
  utility_stats$shopping_orig_car,
  utility_stats$shopping_orig_bicycle,
  utility_stats$shopping_orig_foot,
  utility_stats$shopping_orig_public_transport,
  utility_stats$shopping_orig_taxi,
  na.rm = TRUE
)
```

    [1] 118299.4

``` r
sum(
  utility_stats$shopping_orig_car_go_dutch_fastest,
  utility_stats$shopping_orig_bicycle_go_dutch_fastest,
  utility_stats$shopping_orig_foot_go_dutch_fastest,
  utility_stats$shopping_orig_public_transport_go_dutch_fastest,
  utility_stats$shopping_orig_taxi_go_dutch_fastest,
  na.rm = TRUE
)
```

    [1] 17075.12

The results show that there are too few trips under the go_dutch
scenario. Next step: check the code that generates the utility trips
under the scenarios.

The checks below apply to Scotland South.

The target that generates the graphs for each zone is `zones_stats`,
which was defined as follows:

``` r
  tar_target(zones_stats, {
    stats = dplyr::full_join(commute_stats, school_stats_from, by = "DataZone")
    stats = dplyr::full_join(stats, utility_stats, by = "DataZone")
    stats
  }),
```

The data in `zones_stats` is as follows:

``` r
zones_stats
```

    # A tibble: 378 × 301
       DataZone  comm_orig_all comm_orig_bicycle comm_orig_car comm_orig_foot
       <chr>             <dbl>             <dbl>         <dbl>          <dbl>
     1 S01007481        216.               3.89         184.            22.4 
     2 S01007482         99.2              2.92          72.0           18.5 
     3 S01007483        145.               3.89         108.            26.9 
     4 S01007484         74.4              1.46          56.9           14.1 
     5 S01007485         12.6              0             11.7            0   
     6 S01007486          8.75             0              6.81           0   
     7 S01007487         37.9              0.973         27.2            3.89
     8 S01007488         37.0              0             31.1            3.89
     9 S01007489        139.               4.86         116.            12.6 
    10 S01007490         38.9              0             33.1            5.84
    # ℹ 368 more rows
    # ℹ 296 more variables: comm_orig_public_transport <dbl>, comm_orig_taxi <dbl>,
    #   comm_dest_all <dbl>, comm_dest_bicycle <dbl>, comm_dest_car <dbl>,
    #   comm_dest_foot <dbl>, comm_dest_public_transport <dbl>,
    #   comm_dest_taxi <dbl>, comm_orig_bicycle_go_dutch_fastest <dbl>,
    #   comm_orig_car_go_dutch_fastest <dbl>,
    #   comm_orig_public_transport_go_dutch_fastest <dbl>, …

Let’s look at the shopping trips arriving at a specific zone: S01012355.

``` r
# names(zones_stats)
zone_stats_shopping_arriving = zones_stats |>
  dplyr::filter(DataZone == "S01012355") |>
  dplyr::select(matches("shopping_dest"))
# names(zone_stats_shopping_arriving)
zone_stats_example = zone_stats_shopping_arriving |>
  dplyr::select("shopping_dest_all")
zone_stats_example[[1]]
```

    [1] 640.0917

Let’s get the associated data from the website:

``` r
u = "https://nptscot.blob.core.windows.net/json/DataZone_2024-07-02/S01012355.json"
zone_stats_example_azure = jsonlite::fromJSON(u)
# names(zone_stats_example_azure)
zone_stats_example_azure$shopping_dest_all
```

    [1] 640.0917

The equivalent value the `bicycle` column is:

``` r
zone_stats_shopping_arriving$shopping_dest_bicycle
```

    [1] 1.833888

Under the Go Dutch scenario, the value is:

``` r
zone_stats_shopping_arriving$shopping_dest_bicycle_go_dutch_fastest
```

    [1] 3.098478

The equivalent value for the `car` column is:

``` r
zone_stats_shopping_arriving$shopping_dest_car
```

    [1] 422.5624

Under the Go Dutch scenario, the value is:

``` r
zone_stats_shopping_arriving$shopping_dest_car_go_dutch_fastest
```

    [1] 16.33732

The problem is clear from the above: the number of people arriving is
much higher in the baseline scenario than the other scenarios.

Let’s identify the same value in the `utility_stats` data:

``` r
# names(utility_stats)
```

``` r
utility_stats_example = utility_stats |>
  dplyr::filter(DataZone == "S01012355")
utility_stats_example$shopping_dest_all
```

    [1] 640.0917

``` r
utility_stats_example$shopping_dest_car
```

    [1] 422.5624

Let’s go back another step to look at the inputs and code that generated
the `utility_stats` data. The baseline datasets are taken from the
`utility_stats_baseline` target, which was defined as follows (we start
by loading the inputs):

``` r
stats_all_example = stats_all |>
  dplyr::filter(DataZone == "S01012355")
# Pull out the relevant columns
stats_all_example |>
  dplyr::select(matches("shopping_dest"))
```

    Adding missing grouping variables: `DataZone`

    # A tibble: 1 × 7
    # Groups:   DataZone [1]
      DataZone  shopping_dest_all shopping_dest_car shopping_dest_foot
      <chr>                 <dbl>             <dbl>              <dbl>
    1 S01012355              640.              423.               142.
    # ℹ 3 more variables: shopping_dest_bicycle <dbl>,
    #   shopping_dest_public_transport <dbl>, shopping_dest_taxi <dbl>

The input is in the `stats` object which was generated from the
`od_utility_combined` target. Let’s take a look at summary stats before
and after the `aadt_adjust` function:

``` r
stats = sf::st_drop_geometry(od_utility_combined)
stats = stats[, c(
      "startDZ", "endDZ", "purpose", "all", "car",
      "foot", "bicycle", "public_transport", "taxi"
    )]
stats_purpose_shopping = stats[stats$purpose == "shopping", ]
summary(stats_purpose_shopping$all)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     0.1224  7.3796 22.0196 33.7324 59.6656 99.9084 

``` r
stats_shopping = aadt_adjust(stats[stats$purpose == "shopping", ],
      purpose = "shopping",
      aadt_parameters = aadt_parameters
    )
nrow(stats_shopping)
```

    [1] 3507

``` r
nrow(stats)
```

    [1] 9458

``` r
summary(stats_shopping)
```

       startDZ             endDZ             purpose               all         
     Length:3507        Length:3507        Length:3507        Min.   : 0.1224  
     Class :character   Class :character   Class :character   1st Qu.: 7.3796  
     Mode  :character   Mode  :character   Mode  :character   Median :22.0196  
                                                              Mean   :33.7324  
                                                              3rd Qu.:59.6656  
                                                              Max.   :99.9084  
          car                foot             bicycle         public_transport 
     Min.   : 0.08024   Min.   : 0.02724   Min.   :0.000136   Min.   :0.01146  
     1st Qu.: 4.83376   1st Qu.: 1.65060   1st Qu.:0.022118   1st Qu.:0.69460  
     Median :14.41105   Median : 4.92291   Median :0.085266   Median :2.07163  
     Mean   :22.03378   Mean   : 7.51657   Mean   :0.270686   Mean   :3.16308  
     3rd Qu.:38.91559   3rd Qu.:13.24603   3rd Qu.:0.353670   3rd Qu.:5.57412  
     Max.   :66.25214   Max.   :22.47744   Max.   :3.628631   Max.   :9.45883  
          taxi         
     Min.   :0.002711  
     1st Qu.:0.164313  
     Median :0.490063  
     Mean   :0.748256  
     3rd Qu.:1.318609  
     Max.   :2.237573  

``` r
identical(stats_shopping, stats_purpose_shopping)
```

    [1] FALSE

``` r
waldo::compare(summary(stats_shopping$car), summary(stats_purpose_shopping$car))
```

    `old`: " 0.08024" " 4.83376" "14.41105" "22.03378" "38.91559" "66.25214"
    `new`: " 0.07972" " 4.82410" "14.23059" "21.85693" "38.52807" "66.21236"
