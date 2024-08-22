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
tar_load(r_utility_fastest)
tar_load(uptake_utility_fastest)
names(zones)
```

    [1] "DataZone"   "TotPop2011" "ResPop2011" "HHCnt2011"  "geometry"  

``` r
# parameters
setwd(old_working_directory)
```

Let’s check the level of the od data:

``` r
names(od_shopping)
```

     [1] "geo_code1"                   "geo_code2"                  
     [3] "length_euclidean_unjittered" "origin_trips"               
     [5] "destination_size"            "geometry"                   
     [7] "interaction"                 "output_col"                 
     [9] "proportion"                  "all"                        
    [11] "bicycle"                     "foot"                       
    [13] "car"                         "public_transport"           
    [15] "taxi"                        "length_euclidean_jittered"  
    [17] "purpose"                    

``` r
length(unique(od_shopping$geo_code1))
```

    [1] 344

``` r
nrow(zones)
```

    [1] 344

There is some duplication in the datasets:

``` r
summary(od_utility_combined$startDZ == od_utility_combined$geo_code1)
```

       Mode    TRUE 
    logical    9458 

``` r
summary(od_utility_combined$endDZ == od_utility_combined$geo_code2)
```

       Mode   FALSE    TRUE    NA's 
    logical    6431    2868     159 

This shows that, for calculating the number of people arriving at each
zone, we need a national dataset of zones: zone level statistics should
not be calculated per region, for the number of people arriving at each
zone at least.

In any case, we’ll replace ‘startDZ’ and ‘endDZ’ with ‘geo_code1’ and
‘geo_code2’.

This shows that there are 344 zones in the case study area, and the od
data is at the zone level.

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

![](utility-checks_files/figure-commonmark/unnamed-chunk-5-1.png)

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

The input data is `r_utility_fastest`, let’s first check that it
contains the same number of trips as the `od_utility_combined` data.

``` r
waldo::compare(
  names(r_utility_fastest),
  names(od_utility_combined)
)
```

        old               | new            
    [1] "route_number"    -                
    [2] "distances"       -                
    [3] "quietness"       -                
    [4] "gradient_smooth" -                
    [5] "geo_code1"       | "geo_code1" [1]
    [6] "geo_code2"       | "geo_code2" [2]
    [7] "car"             | "car"       [3]

         old                       | new                                     
    [11] "dist_euclidean"          | "dist_euclidean"          [7]           
    [12] "public_transport"        | "public_transport"        [8]           
    [13] "taxi"                    | "taxi"                    [9]           
                                   - "geometry"                [10]          
    [14] "dist_euclidean_jittered" | "dist_euclidean_jittered" [11]          
    [15] "purpose"                 | "purpose"                 [12]          
    [16] "endDZ"                   | "endDZ"                   [13]          
    [17] "startDZ"                 | "startDZ"                 [14]          
    [18] "geometry"                -                                         
    [19] "route_hilliness"         -                                         
     ... ...                         ...                       and 1 more ...

``` r
r_utility_fastest_df = r_utility_fastest |>
  sf::st_drop_geometry() |>
  group_by(geo_code1, geo_code2) |>
  slice(1)
nrow(r_utility_fastest_df)
```

    [1] 8843

``` r
nrow(od_utility_combined)
```

    [1] 9458

``` r
sum(r_utility_fastest_df$all)
```

    [1] 150039.8

``` r
sum(od_utility_combined$all)
```

    [1] 188548.6

The results show differences between the two datasets. The differences
are not big enough to explain the differences between the baseline and
Go Dutch scenarios.

The number of trips for each scenario is calculated as follows:

``` r
# uptake_utility_fastest
routes = r_utility_fastest |>
  filter(distances < 10000) |>
  get_uptake_scenarios(purpose = "utility")
```

Let’s check the total amount of trips for the Go Dutch scenario again,
for each OD pair:

``` r
routes_df = routes |>
  sf::st_drop_geometry() |>
  group_by(geo_code1, geo_code2) |>
  slice(1)
nrow(r_utility_fastest_df)
```

    [1] 8843

``` r
nrow(routes_df)
```

    [1] 2518

Let’s check that the output is internally consistent, with the total of
all modes equal to the total number of trips, for each scenario.

``` r
sum(routes_df$all)
```

    [1] 35246.14

``` r
sum(
  routes_df$car,
  routes_df$bicycle,
  routes_df$foot,
  routes_df$public_transport,
  routes_df$taxi
)
```

    [1] 35246.14

``` r
# And for the Go Dutch scenario:
sum(
  routes_df$car_go_dutch,
  routes_df$bicycle_go_dutch,
  routes_df$foot_go_dutch,
  routes_df$public_transport_go_dutch,
  routes_df$taxi_go_dutch
)
```

    [1] 35246.14

This shows that we’re losing a load of trips in the
`get_uptake_scenarios` function.

A quick fix is to update the baseline stats object, replacing
`od_utility_combined` with `uptake_utility_fastest`.

``` r
    stats = sf::st_drop_geometry(uptake_utility_fastest)
    summary(duplicated(paste0(stats$geo_code1, stats$geo_code2)))
```

       Mode   FALSE    TRUE 
    logical    2518      86 

``` r
    summary(duplicated(paste0(stats$startDZ, stats$endDZ)))
```

       Mode   FALSE    TRUE 
    logical    2107     497 

``` r
    # Keep only the first instance of each OD pair
    stats = stats |>
      dplyr::group_by(geo_code1, geo_code2) |>
      dplyr::slice(1)

     stats = stats[, c(
      "startDZ", "endDZ", "purpose", "all", "car",
      "foot", "bicycle", "public_transport", "taxi"
    )]

    stats_orig = stats |>
      dplyr::select(!endDZ) |>
      dplyr::group_by(startDZ, purpose) |>
      dplyr::summarise_all(sum, na.rm = TRUE)

    stats_dest = stats |>
      dplyr::select(!startDZ) |>
      dplyr::group_by(endDZ, purpose) |>
      dplyr::summarise_all(sum, na.rm = TRUE)

    stats_orig$purpose = paste0(stats_orig$purpose, "_orig")
    stats_dest$purpose = paste0(stats_dest$purpose, "_dest")

    stats_orig = tidyr::pivot_wider(stats_orig,
      id_cols = startDZ,
      names_from = "purpose",
      values_from = all:taxi,
      names_glue = "{purpose}_{.value}"
    )

    stats_dest = tidyr::pivot_wider(stats_dest,
      id_cols = endDZ,
      names_from = "purpose",
      values_from = all:taxi,
      names_glue = "{purpose}_{.value}"
    )
    names(stats_orig)[1] = "DataZone"
    names(stats_dest)[1] = "DataZone"

    stats_all = dplyr::full_join(stats_orig, stats_dest, by = "DataZone")
    stats_all
```

    # A tibble: 345 × 37
    # Groups:   DataZone [345]
       DataZone  leisure_orig_all shopping_orig_all visiting_orig_all
       <chr>                <dbl>             <dbl>             <dbl>
     1 S01007481            78.2               39.2             116. 
     2 S01007482            28.3               87.6              67.1
     3 S01007483            40.7               78.1             139. 
     4 S01007484             9.65              60.9              79.6
     5 S01007485             1.18              NA                41.4
     6 S01007486            45.4              127.               39.9
     7 S01007487             7.87              49.5             107. 
     8 S01007488            80.5               42.5             103. 
     9 S01007489            81.6               35.7              56.4
    10 S01007490            65.4               19.8              73.2
    # ℹ 335 more rows
    # ℹ 33 more variables: leisure_orig_car <dbl>, shopping_orig_car <dbl>,
    #   visiting_orig_car <dbl>, leisure_orig_foot <dbl>, shopping_orig_foot <dbl>,
    #   visiting_orig_foot <dbl>, leisure_orig_bicycle <dbl>,
    #   shopping_orig_bicycle <dbl>, visiting_orig_bicycle <dbl>,
    #   leisure_orig_public_transport <dbl>, shopping_orig_public_transport <dbl>,
    #   visiting_orig_public_transport <dbl>, leisure_orig_taxi <dbl>, …

Let’s re-run the `utility_stats` target and check the number of shopping
trips in the Go Dutch scenario.

``` r
utility_stats_new = make_utility_stats(uptake_utility_fastest, "fastest", zones)
```

    `summarise()` has grouped output by 'startDZ'. You can override using the
    `.groups` argument.
    `summarise()` has grouped output by 'endDZ'. You can override using the
    `.groups` argument.

``` r
mean(utility_stats$shopping_orig_car_go_dutch_fastest, na.rm = TRUE)
```

    [1] 37.25217

``` r
mean(utility_stats_new$shopping_orig_car_go_dutch_fastest, na.rm = TRUE)
```

    [1] 37.25217

`stats_all` should now match the number of trips in other scenarios.
Let’s check:

``` r
sum(stats_all$shopping_orig_all, na.rm = TRUE)
```

    [1] 12277.47

``` r
sum(stats_all$shopping_dest_car, na.rm = TRUE)
```

    [1] 8015.269

``` r
# Equivalent from Go Dutch scenario:
sum(utility_stats$shopping_orig_all, na.rm = TRUE)
```

    [1] 118299.4

``` r
sum(utility_stats$shopping_dest_car, na.rm = TRUE)
```

    [1] 77272.48

``` r
totals_1 = c(
  sum(stats_all$shopping_orig_car, na.rm = TRUE),
  sum(stats_all$shopping_orig_bicycle, na.rm = TRUE),
  sum(stats_all$shopping_orig_foot, na.rm = TRUE),
  sum(stats_all$shopping_orig_public_transport, na.rm = TRUE),
  sum(stats_all$shopping_orig_taxi, na.rm = TRUE)
)
# And the stats_all equivalent:
sum(
  stats_all$shopping_orig_car,
  stats_all$shopping_orig_bicycle,
  stats_all$shopping_orig_foot,
  stats_all$shopping_orig_public_transport,
  stats_all$shopping_orig_taxi,
  na.rm = TRUE
)
```

    [1] 12277.47

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
