# AADT multipliers

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

The numbers calulated based on analysis of the raw household travel
survey were as follows:

``` r
aadt_original = read_csv("../../data-raw/AADT_factors.csv")
```

    Rows: 7 Columns: 5
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (1): NPT_purpose
    dbl (4): AADT_all, AADT_bike, Weekly_all_total, Weekly_bike_total

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
knitr::kable(aadt_original)
```

| NPT_purpose          | AADT_all | AADT_bike | Weekly_all_total | Weekly_bike_total |
|:---------------------|---------:|----------:|-----------------:|------------------:|
| Commute              |   0.9726 |    0.7349 |           6.8080 |            5.1446 |
| Leisure              |   0.2093 |    0.1063 |           1.4648 |            0.7444 |
| Other                |   0.8954 |    0.5660 |           6.2680 |            3.9619 |
| Recreational Cycling |   0.0064 |    0.3182 |           0.0450 |            2.2274 |
| School               |   0.1465 |    0.2662 |           1.0256 |            1.8635 |
| Shopping             |   0.6012 |    0.2229 |           4.2085 |            1.5601 |
| Visiting             |   0.2737 |    0.1986 |           1.9162 |            1.3899 |

The updated approach is to use a new variable, AADT_multiplier, that
only effects the following:

- Commute: translating n. commuters to n. trips per day
- School: translating n. students to n. trips per day
- Shopping: accounting for the fact that shopping is less likely to be
  done by bike than other trip purposes

## Commute

We will use the same number as in `AADT_all` for commute, based on the
analysis of the household travel survey. See
https://github.com/nptscot/TT\_-Scottish_Household_Survey#commute for
details.

``` r
aadt_multiplier_commute = aadt_original %>%
  filter(NPT_purpose == "Commute") %>%
  pull(AADT_all)
```

``` r
aadt_multiplier_commute
```

    [1] 0.9726

## School

To calculate the multiplier between n. students and average n. trips per
day, we took the number of school days per year in Scotland and
multiplied by average attendance rate and multiplied by 2 to account for
the fact that school trips go there and back:

``` r
school_days_per_year = 190
school_attendance_rate = 0.902
aadt_multiplier_school = school_days_per_year /
  365 * 
  school_attendance_rate *
  2
```

## Shopping

Based on the analysis of the household travel survey, shopping trips are
disproportionately less likely to be done by bike than other trip
purposes by a factor of 2-3. Given that cycling for shopping should
become easier in a future with more cycling infrastructure, we will use
a factor of 2 (with a multiplier of 0.5) for now. We can update this
number in the future if we have better data.

``` r
aadt_multiplier_shopping = 0.5
```

## Other trip purposes

The basis of the trip numbers per zone were taken from the Scottish
Household Survey and are already in units of AADT, so we will keep them
at 1.

Updating the table presented above, we have:

``` r
aadt_updated = aadt_original %>%
  mutate(AADT_multiplier = case_when(
    NPT_purpose == "Commute" ~ aadt_multiplier_commute,
    NPT_purpose == "School" ~ aadt_multiplier_school,
    NPT_purpose == "Shopping" ~ aadt_multiplier_shopping,
    TRUE ~ 1
  )) |>
  mutate(AADT_multiplier = round(AADT_multiplier, 4)) |>
  # Remove Other
  filter(NPT_purpose != "Other")
```

The multipliers are as follows:

``` r
aadt_updated |>
  select(NPT_purpose, AADT_multiplier) |>
  knitr::kable()
```

| NPT_purpose          | AADT_multiplier |
|:---------------------|----------------:|
| Commute              |          0.9726 |
| Leisure              |          1.0000 |
| Recreational Cycling |          1.0000 |
| School               |          0.9391 |
| Shopping             |          0.5000 |
| Visiting             |          1.0000 |

``` r
write_csv(aadt_updated, "../../data-raw/AADT_factors.csv")
```
