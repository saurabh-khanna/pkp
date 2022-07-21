Assessing OJS overlaps with scientometric databases
================
Updated: July 20, 2022

-   <a href="#cleaning-raw-beacon-data"
    id="toc-cleaning-raw-beacon-data">Cleaning raw beacon data</a>
-   <a href="#assessing-overlaps" id="toc-assessing-overlaps">Assessing
    overlaps</a>
    -   <a href="#web-of-science" id="toc-web-of-science">Web of Science</a>
    -   <a href="#scopus" id="toc-scopus">Scopus</a>
    -   <a href="#dimensions" id="toc-dimensions">Dimensions</a>
    -   <a href="#ebsco-host" id="toc-ebsco-host">EBSCO Host</a>
    -   <a href="#google-scholar" id="toc-google-scholar">Google Scholar</a>
    -   <a href="#latindex" id="toc-latindex">Latindex</a>

------------------------------------------------------------------------

``` r
# Libraries
pacman::p_load(tidyverse, countrycode, sf, state, htmltools, htmlwidgets, urltools, janitor, DT, RColorBrewer, plotly, readxl)

# raw beacon data
df <- read_csv(here::here("data/beacon-public.csv")) 

# ancillary data and vectors for cleaning
iso_to_name <- read_csv(here::here("data/country-iso3166-to-name.csv"))

canada.states <- 
  c(
    "Alberta", "British Columbia", "Labrador", "Manitoba", "New Brunswick", "Newfoundland", "Nova Scotia", "Nunavut", "North West Terr.", "Ontario", "Prince Edward Is.", "Québec (Province)", "Saskatchewan", "Yukon"
  )
```

## Cleaning raw beacon data

``` r
# Cleaning oai urls
df <- 
  df %>% 
  mutate(oai_url = if_else(str_detect(oai_url, "^//"), str_c("http:", oai_url), oai_url))


# Getting domain, tld, and merging in country names
df <-  
  bind_cols(
    df,
    df %>% pull(oai_url) %>% domain() %>% tld_extract()
  ) %>% 
  mutate(country_consolidated = str_to_lower(country_consolidated) %>% str_trim()) %>% 
  left_join(iso_to_name, by = c("country_consolidated" = "tld"))


# Cleaning country names
df <-
  df %>% 
  mutate(
    country = country_clean,
    country = if_else(country == "Washington (State)", "United States", country),
    country = if_else(str_detect(country, "United States|New York|District of Columbia"), "United States", country),
    country = if_else(country %in% state.name, "United States", country),
    country = if_else(country %in% canada.states, "Canada", country),
    country = if_else(str_detect(country, "China"), "China", country),
    country = if_else(str_detect(country, "Armenia"), "Armenia", country),
    country = if_else(str_detect(country, "Georgia"), "Georgia", country),
    country = if_else(str_detect(country, "Australia|New South Wales|Queensland|Victoria"), "Australia", country),
    country = if_else(str_detect(country, "England|British"), "United Kingdom", country),
    country = if_else(str_detect(country, "Russia|Soviet Union"), "Russia", country),
    country = if_else(str_detect(country, "Palestine"), "Palestine", country),
    country = if_else(country == "Korea (South)", "South Korea", country)
  ) %>% 
  select(-country_clean, -country_marc, -country_issn, -country_tld, -country_ip)


# Mapping countries to continents
continents <-
  df %>%
  pull(country) %>% 
  countrycode(origin = "country.name", destination = "continent")

df <- df %>% bind_cols(continents)

# Rename last column to `continent`
names(df)[length(names(df))] <- "continent"
```

``` r
# Filter to active OJS journals
df <-
  df %>% 
  filter(application == "ojs", record_count_2020 >= 5) %>% 
  distinct(oai_url, repository_name, set_spec, .keep_all = T)
```

Total active journals using OJS (JUOJS):

``` r
df %>% count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1 25671

Total articles published in active JUOJS:

``` r
df %>% summarise(total = sum(total_record_count, na.rm = T))
```

    ## # A tibble: 1 × 1
    ##     total
    ##     <dbl>
    ## 1 5383793

Total distinct ISSNs:

``` r
df %>% 
  mutate(
    issn = str_extract(issn, "[^\n]+") # correcting for multiple reported issns
  ) %>% 
  distinct(issn) %>% 
  drop_na(issn) %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1 22809

------------------------------------------------------------------------

<br/><br/>

## Assessing overlaps

### Web of Science

Total overlap:

``` r
df_wos <-
  read_table(here::here("data/overlaps/wos_data_simon.txt"), na = c("NULL", "NA", "")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  filter(str_detect(issn, "^[0-9]{4}-[0-9]{3}[0-9xX]$")) %>% 
  distinct(issn) %>% 
  drop_na(issn)

# total overlap
df %>% 
  mutate(
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  inner_join(df_wos, by = "issn") %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   279

Top 10 countries in overlap:

``` r
# top 10 countries in overlap
df %>% 
  mutate(
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  inner_join(df_wos, by = "issn") %>% 
  count(country) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_blank()
  ) +
  coord_flip() +
  labs(
    x = "Country", y = "Number of journals"
  )
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Scopus

Total overlap:

``` r
# Scopus data
df_scopus <-
  read_excel(here::here("data/overlaps/scopus_nov2021.xlsx")) %>%
  clean_names() %>% 
  remove_empty() %>% 
  transmute(
    issn = str_replace(print_issn, "-", ""),
    e_issn = str_replace(e_issn, "-", "")
  ) %>%
  distinct() %>% 
  remove_empty()

# join A (issn to issn)
df_join_a <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+"),
    issn = str_replace(issn, "-", "")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_scopus %>% select(-e_issn), by = "issn")

# join B (issn to e-issn)
df_join_b <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+"),
    issn = str_replace(issn, "-", "")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_scopus %>% select(-issn), by = c("issn" = "e_issn"))

bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1  1646

``` r
# 1646/41957
# 1646/22809
```

Top 10 countries in overlap:

``` r
bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>%
  drop_na(country) %>% 
  count(country) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_blank()
  ) +
  coord_flip() +
  labs(
    x = "Country", y = "Number of journals"
  )
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Dimensions

Total overlap:

``` r
# Dimensions data
df_dimensions <-
  read_excel(here::here("data/overlaps/journals_dimensions.xlsx")) %>%
  clean_names() %>% 
  remove_empty() %>% 
  transmute(
    issn = str_replace(issn_print, "-", "") %>% na_if("NULL"),
    e_issn = str_replace(issn_e, "-", "") %>% na_if("NULL")
  ) %>%
  distinct() %>% 
  remove_empty()


# join A (issn to issn)
df_join_a <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+"),
    issn = str_replace(issn, "-", "")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_dimensions %>% select(-e_issn), by = "issn")

# join B (issn to e-issn)
df_join_b <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+"),
    issn = str_replace(issn, "-", "")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_dimensions %>% select(-issn), by = c("issn" = "e_issn"))

bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1 12435

``` r
#12435/22809
#12435/72990
```

Top 10 countries in overlap:

``` r
bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>%
  drop_na(country) %>% 
  count(country) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_blank()
  ) +
  coord_flip() +
  labs(
    x = "Country", y = "Number of journals"
  )
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### EBSCO Host

Total Overlap:

``` r
# EBSCO Host
df_ebsco <-
  read_excel(here::here("data/overlaps/ebscohost.xls")) %>%
  clean_names() %>% 
  remove_empty() %>% 
  distinct(issn) %>% 
  drop_na(issn)

df %>% 
  drop_na(issn) %>%
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  inner_join(df_ebsco, by = "issn") %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   771

Top 10 countries in overlap:

``` r
df %>% 
  drop_na(issn) %>%
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  inner_join(df_ebsco, by = "issn") %>%
  drop_na(country) %>% 
  count(country) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    axis.ticks = element_blank()
  ) +
  labs(
    x = "Country", y = "Number of journals"
  )
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Google Scholar

``` r
# Read in python processed google scholar URLs
## first iteration using journal URLs ##

df_gscholar <- 
  read_csv(here::here("scripts/gscholar_urls_mapped.csv")) %>% 
  select(-result_json) %>% 
  mutate(n_results = parse_integer(n_results))

df_gscholar <-
  df %>%
  separate(oai_url, c("url1", "url2"), "\n") %>% 
  remove_empty() %>% 
  rename(oai_url = url1) %>% 
  filter(!str_detect(oai_url, "\\?page=")) %>% 
  transmute(
    country, total_record_count, context_name,
    url = str_replace(oai_url, "index/oai", set_spec),
    url = str_replace(url, "^https://", ""),
    url = str_replace(url, "^http://", ""),
    url = str_replace(url, "^www108.", ""),
    url = str_replace(url, "^www5.", ""),
    url = str_replace(url, "^www3.", ""),
    url = str_replace(url, "^www2.", ""),
    url = str_replace(url, "^www.", ""),
  ) %>%
  bind_rows(
    df %>%
      separate(oai_url, c("url1", "url2"), "\n") %>% 
      remove_empty() %>% 
      rename(oai_url = url1) %>% 
      filter(str_detect(oai_url, "\\?page=")) %>% 
      transmute(
        country, total_record_count, context_name,
        url = str_replace(oai_url, "oai$", set_spec),
        url = str_replace(url, "^https://", ""),
        url = str_replace(url, "^http://", ""),
        url = str_replace(url, "^www108.", ""),
        url = str_replace(url, "^www5.", ""),
        url = str_replace(url, "^www3.", ""),
        url = str_replace(url, "^www2.", ""),
        url = str_replace(url, "^www.", ""),
      ), .
  ) %>% 
  distinct() %>% 
  inner_join(df_gscholar, by = "url")


# bind_cols(
#     df_gscholar,
#     df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
#   ) %>%
#   left_join(read_csv(here::here("scripts/gscholar_citations.csv")), by = "url") %>% 
#   group_by(domain) %>% 
#   summarise(
#     n_results = sum(n_results, na.rm = T),
#     n_citations = sum(n_citations, na.rm = T)
#   ) %>% 
#   filter(n_results > 0) %>% 
#   select(-n_results) %>% 
#   write_csv(here::here("data/scholar_present_domains_set1.csv"))
```

Total overlap:

``` r
# loading all domains
domains_in_scholar <-
  bind_rows(
    read_csv(here::here("data/scholar_present_domains_set1.csv")),
    read_csv(here::here("data/scholar_present_domains_set2.csv")),
  ) %>% 
  distinct()

# total overlap
bind_cols(
    df_gscholar,
    df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
  ) %>% 
  inner_join(domains_in_scholar, by = "domain") %>%
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1 22679

``` r
#22679/25671

#domains_in_scholar %>% summary()
#domains_in_scholar %>% filter(n_citations < 1)
#552/8548
```

Top 10 countries in overlap:

``` r
# top 10 countries in overlap
bind_cols(
    df_gscholar,
    df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
  ) %>% 
  inner_join(domains_in_scholar, by = "domain") %>% 
  count(country) %>% 
  arrange(-n) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  head(10) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  scale_y_continuous(breaks = scales::breaks_width(1000)) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    axis.ticks = element_blank()
  ) +
  coord_flip() +
  labs(x = "Country", y = "Total journals")
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Number of citations on first Scholar page:

``` r
# number of citations on first gscholar page
bind_cols(
    df_gscholar,
    df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
  ) %>% 
  inner_join(domains_in_scholar, by = "domain") %>% 
  select(domain, url, n_citations) %>% 
  group_by(domain) %>% 
  mutate(journals_within_this_domain = n()) %>% 
  ungroup() %>% 
  mutate(
    n_citations_by_journal = round(n_citations/journals_within_this_domain),
    n_citations_by_journal = pmin(n_citations_by_journal, 500)
  ) %>%
  select(url, n_citations_by_journal) %>%
  ggplot(aes(n_citations_by_journal)) +
  geom_histogram(binwidth = 20, fill = "#0072B2") +
  scale_x_continuous(labels = c("0", "100", "200", "300", "400", "500+")) +
  labs(
    x = "Total citations on first Google Scholar page",
    y = "Number of journals"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.ticks = element_blank()
  )
```

![](ojs_global_paper_files/figure-gfm/scholar-citations-1.png)<!-- -->

Journals with no citations on first page:

``` r
# alter threshold to see proportions as needed
threshold <- 0

bind_cols(
    df_gscholar,
    df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
  ) %>% 
  inner_join(domains_in_scholar, by = "domain") %>% 
  select(domain, url, n_citations) %>% 
  group_by(domain) %>% 
  mutate(journals_within_this_domain = n()) %>% 
  ungroup() %>% 
  mutate(
    n_citations_by_journal = round(n_citations/journals_within_this_domain),
  ) %>%
  count(n_citations_by_journal > threshold)
```

    ## # A tibble: 2 × 2
    ##   `n_citations_by_journal > threshold`     n
    ##   <lgl>                                <int>
    ## 1 FALSE                                  835
    ## 2 TRUE                                 21844

Summary of citations on first page:

``` r
bind_cols(
    df_gscholar,
    df_gscholar %>% pull(url) %>% domain() %>% tld_extract()
  ) %>% 
  inner_join(domains_in_scholar, by = "domain") %>% 
  select(domain, url, n_citations) %>% 
  group_by(domain) %>% 
  mutate(journals_within_this_domain = n()) %>% 
  ungroup() %>% 
  transmute(
    n_citations_by_journal = round(n_citations/journals_within_this_domain),
  ) %>%
  summary()
```

    ##  n_citations_by_journal
    ##  Min.   :    0.0       
    ##  1st Qu.:   18.0       
    ##  Median :   59.0       
    ##  Mean   :  135.2       
    ##  3rd Qu.:  139.0       
    ##  Max.   :59728.0

### Latindex

Total Overlap:

``` r
# Latindex
df_latindex <-
  read_excel(here::here("data/overlaps/latindex.xlsx")) %>%
  clean_names() %>% 
  remove_empty() %>% 
  select(issn, e_issn, online = en_linea) %>% 
  distinct()
  

# join A (issn to issn)
df_join_a <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_latindex %>% select(-e_issn), by = "issn")

# join B (issn to e-issn)
df_join_b <-
  df %>%
  drop_na(issn) %>% 
  transmute(
    country,
    issn = str_extract(issn, "[^\n]+")
  ) %>%
  distinct(issn, .keep_all = T) %>% 
  inner_join(df_latindex %>% select(-issn), by = c("issn" = "e_issn"))

bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>%
  arrange(country, issn, -online) %>% 
  distinct(country, issn) %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1  4208

``` r
# 4208/24486
# 4208/6319
```

Top 10 countries in overlap:

``` r
bind_rows(df_join_a, df_join_b) %>% 
  distinct() %>%
  arrange(country, issn, -online) %>% 
  distinct(country, issn) %>%
  drop_na(country) %>% 
  count(country) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  mutate(country = fct_inorder(country) %>% fct_rev()) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    x = "Country", y = "Number of journals"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_blank()
  )
```

![](ojs_global_paper_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Latin American countries for JUOJS:

``` r
# Latin American countries for JUOJS
df_latam <- read_csv(here::here("data/latindex_countries.csv")) 

df %>% 
  inner_join(df_latam, by = "country") %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1  6319
