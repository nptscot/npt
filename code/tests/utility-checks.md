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
tar_source()
```

    tar_source(): these files do not exist: R

We’ll load data with `tar_load()` and check the utility trips.

``` r
if (!file.exists("_targets.R")) {
  old_working_directory = setwd("../..")
} else {
  old_working_directory = getwd()
}
# tar_make()
tar_load(parameters)
tar_load(zones_stats)
tar_load(utility_stats)
# parameters
setwd(old_working_directory)
```

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
the `utility_stats` data.
