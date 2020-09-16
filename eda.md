Beacon EDA
================
Saurabh Khanna
2020-09-16

  - [EDA](#eda)

``` r
# Libraries
pacman::p_load(tidyverse)

# Data
df <- read_csv(here::here("data", "beacon.csv"))
```

## EDA

``` r
df
```

    ## # A tibble: 15,224 x 16
    ##       id application version beacon_id oai_url stats_id first_beacon       
    ##    <dbl> <chr>       <chr>   <chr>     <chr>   <chr>    <dttm>             
    ##  1     1 ojs         ojs2/2~ ojs-5442~ https:~ 5442f32~ 2020-05-31 00:00:00
    ##  2     2 ojs         ojs2/3~ ojs-5a3d~ http:/~ 5a3dc63~ 2020-05-31 00:00:00
    ##  3     3 ojs         ojs2/2~ ojs-52ca~ http:/~ 52cac44~ 2020-05-31 00:00:00
    ##  4     4 ojs         ojs2/2~ ojs-5d1b~ https:~ 5d1b446~ 2020-05-31 00:00:00
    ##  5     5 ojs         ojs2/2~ ojs-569e~ http:/~ 569e27f~ 2020-06-01 00:00:00
    ##  6     6 ojs         ojs2/2~ ojs-5996~ http:/~ 5996d8b~ 2020-06-01 00:00:00
    ##  7     7 ojs         ojs2/2~ ojs-5d4f~ https:~ 5d4f5ad~ 2020-06-01 00:00:00
    ##  8     8 ojs         ojs2/2~ ojs-596d~ http:/~ 596da86~ 2020-06-01 00:00:00
    ##  9     9 ojs         ojs2/2~ ojs-5a8e~ https:~ 5a8e3cb~ 2020-06-01 00:00:00
    ## 10    10 ojs         ojs2/2~ ojs-5769~ https:~ 5769349~ 2020-06-01 00:00:00
    ## # ... with 15,214 more rows, and 9 more variables: last_beacon <dttm>,
    ## #   last_oai_response <dttm>, admin_email <chr>, issn <chr>, country <chr>,
    ## #   total_record_count <dbl>, last_completed_update <dttm>, errors <dbl>,
    ## #   last_error <chr>
