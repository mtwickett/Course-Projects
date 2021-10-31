Covid-19 Virus Trends Guided Project
================
Martin Wickett
24/10/2021

## Project Outline

The goal of this project is to answer the following question in relation
to Covid-19:

Which countries have the highest number of positive cases against the
number of tests?

## The Data

-   The dataset can be found
    [here](https://www.kaggle.com/lin0li/covid19testing) on Kaggle
-   The data was collected between 20th January and 1st June 2020
-   It contains over 100 countries, daily & cumulative number of tests
    conducted, number of positive tests per day, number of hospitalized
    per day, number recovered per day, and death cases reported per day

## Prepare the Data

``` r
covid_19 = read.csv("C:/Users/Martin/Desktop/covid19.csv")
```

``` r
dim(covid_19)
```

    ## [1] 10903    14

``` r
colnames(covid_19)
```

    ##  [1] "Date"                    "Continent_Name"         
    ##  [3] "Two_Letter_Country_Code" "Country_Region"         
    ##  [5] "Province_State"          "positive"               
    ##  [7] "hospitalized"            "recovered"              
    ##  [9] "death"                   "total_tested"           
    ## [11] "active"                  "hospitalizedCurr"       
    ## [13] "daily_tested"            "daily_positive"

``` r
head(covid_19)
```

    ##         Date Continent_Name Two_Letter_Country_Code Country_Region
    ## 1 2020-01-20           Asia                      KR    South Korea
    ## 2 2020-01-22  North America                      US  United States
    ## 3 2020-01-22  North America                      US  United States
    ## 4 2020-01-23  North America                      US  United States
    ## 5 2020-01-23  North America                      US  United States
    ## 6 2020-01-24           Asia                      KR    South Korea
    ##   Province_State positive hospitalized recovered death total_tested active
    ## 1     All States        1            0         0     0            4      0
    ## 2     All States        1            0         0     0            1      0
    ## 3     Washington        1            0         0     0            1      0
    ## 4     All States        1            0         0     0            1      0
    ## 5     Washington        1            0         0     0            1      0
    ## 6     All States        2            0         0     0           27      0
    ##   hospitalizedCurr daily_tested daily_positive
    ## 1                0            0              0
    ## 2                0            0              0
    ## 3                0            0              0
    ## 4                0            0              0
    ## 5                0            0              0
    ## 6                0            5              0

``` r
glimpse(covid_19)
```

    ## Rows: 10,903
    ## Columns: 14
    ## $ Date                    <chr> "2020-01-20", "2020-01-22", "2020-01-22", "202~
    ## $ Continent_Name          <chr> "Asia", "North America", "North America", "Nor~
    ## $ Two_Letter_Country_Code <chr> "KR", "US", "US", "US", "US", "KR", "US", "US"~
    ## $ Country_Region          <chr> "South Korea", "United States", "United States~
    ## $ Province_State          <chr> "All States", "All States", "Washington", "All~
    ## $ positive                <int> 1, 1, 1, 1, 1, 2, 1, 1, 4, 0, 3, 0, 0, 0, 0, 1~
    ## $ hospitalized            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ recovered               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ death                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ total_tested            <int> 4, 1, 1, 1, 1, 27, 1, 1, 0, 0, 0, 0, 0, 0, 0, ~
    ## $ active                  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ hospitalizedCurr        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ daily_tested            <int> 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ daily_positive          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~

Filter the dataframe by **`All States`** in the **`Province_State`**
column, then create a new dateframe that excludes the
**`Province_State`** column. This is so only data at the country level
is used.

``` r
covid_19_all_states <- covid_19 %>%
  filter(Province_State == 'All States') %>%
  select(-Province_State)
head(covid_19_all_states)
```

    ##         Date Continent_Name Two_Letter_Country_Code Country_Region positive
    ## 1 2020-01-20           Asia                      KR    South Korea        1
    ## 2 2020-01-22  North America                      US  United States        1
    ## 3 2020-01-23  North America                      US  United States        1
    ## 4 2020-01-24           Asia                      KR    South Korea        2
    ## 5 2020-01-24  North America                      US  United States        1
    ## 6 2020-01-25        Oceania                      AU      Australia        4
    ##   hospitalized recovered death total_tested active hospitalizedCurr
    ## 1            0         0     0            4      0                0
    ## 2            0         0     0            1      0                0
    ## 3            0         0     0            1      0                0
    ## 4            0         0     0           27      0                0
    ## 5            0         0     0            1      0                0
    ## 6            0         0     0            0      0                0
    ##   daily_tested daily_positive
    ## 1            0              0
    ## 2            0              0
    ## 3            0              0
    ## 4            5              0
    ## 5            0              0
    ## 6            0              0

Select columns with only daily data.

``` r
covid_19_all_states_daily <- covid_19_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
head(covid_19_all_states_daily)
```

    ##         Date Country_Region active hospitalizedCurr daily_tested daily_positive
    ## 1 2020-01-20    South Korea      0                0            0              0
    ## 2 2020-01-22  United States      0                0            0              0
    ## 3 2020-01-23  United States      0                0            0              0
    ## 4 2020-01-24    South Korea      0                0            5              0
    ## 5 2020-01-24  United States      0                0            0              0
    ## 6 2020-01-25      Australia      0                0            0              0

Rename the Columns for consistency to snakecase.

``` r
covid_19_all_states_daily <- covid_19_all_states_daily %>%
  rename(
    date = Date,
    country_region = Country_Region,
    hospitalised_curr = hospitalizedCurr
  )
head(covid_19_all_states_daily)
```

    ##         date country_region active hospitalised_curr daily_tested
    ## 1 2020-01-20    South Korea      0                 0            0
    ## 2 2020-01-22  United States      0                 0            0
    ## 3 2020-01-23  United States      0                 0            0
    ## 4 2020-01-24    South Korea      0                 0            5
    ## 5 2020-01-24  United States      0                 0            0
    ## 6 2020-01-25      Australia      0                 0            0
    ##   daily_positive
    ## 1              0
    ## 2              0
    ## 3              0
    ## 4              0
    ## 5              0
    ## 6              0

## Find The Top 10 Covid Cases By Country

``` r
covid_df_all_states_daily_sum <- covid_19_all_states_daily %>%
  group_by(country_region) %>%
  summarise(tested = sum(daily_tested),
  positive = sum(daily_positive),
  active = sum(active),
  hospitalised = sum(hospitalised_curr)) %>%
  arrange(-tested)
covid_df_all_states_daily_sum
```

    ## # A tibble: 108 x 5
    ##    country_region   tested positive  active hospitalised
    ##    <chr>             <int>    <int>   <int>        <int>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0
    ## # ... with 98 more rows

``` r
covid_19_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_19_top_10
```

    ## # A tibble: 10 x 5
    ##    country_region   tested positive  active hospitalised
    ##    <chr>             <int>    <int>   <int>        <int>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0

## Find Which Countries Have The Highest Number Of Positive Cases Against The Number Of Tests

``` r
countries <- covid_19_top_10$country_region
tested_cases <- covid_19_top_10$tested
positive_cases <- covid_19_top_10$positive
active_cases <- covid_19_top_10$active
hospitalised_cases <- covid_19_top_10$hospitalised
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalised_cases) <- countries
positive_cases / tested_cases
```

    ##  United States         Russia          Italy          India         Turkey 
    ##    0.108618191    0.038546552    0.061523368    0.016507300    0.080711720 
    ##         Canada United Kingdom      Australia           Peru         Poland 
    ##    0.054915490    0.113260617    0.005746668    0.060910738    0.025840932

``` r
positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.1, "Turkey" = 0.08)
positive_tested_top_3
```

    ## United Kingdom  United States         Turkey 
    ##           0.11           0.10           0.08

``` r
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.1, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("ratio", "tested", "positive", "active", "hospitalised")
covid_mat
```

    ##                ratio   tested positive  active hospitalised
    ## united_kingdom  0.11  1473672   166909       0            0
    ## united_states   0.10 17282363  1877179       0            0
    ## turkey          0.08  2031192   163941 2980960            0

## Store The Data

``` r
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c(positive_tested_top_3)

datasets <- list(
  original = covid_19,
  all_states = covid_19_all_states,
  daily = covid_19_all_states_daily,
  top_10 = covid_19_top_10
)

matrices <- list(covid_mat)

vectors <- list(
  tested_cases, 
  positive_cases, 
  active_cases, 
  hospitalised_cases, 
  countries
)

data_structure_list <- list(
  "dataframe" = datasets, 
  "matrix" = matrices, 
  "vector" = vectors
)

covid_analysis_list <- list(
  question,
  answer,
  data_structure_list
)

covid_analysis_list[[2]]
```

    ## United Kingdom  United States         Turkey 
    ##           0.11           0.10           0.08
