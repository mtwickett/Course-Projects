Creating An Effective Data Analysis Workflow Guided Project Part 1 and 2
================

### Part 1 Project Outline

In this project we have been asked to find the most profitable R
programming books sold by a fictional company.

### The Data

The data set can be found
[here](https://data.world/dataquest/book-reviews). It includes simulated
programming textbook sales and reviews.

### Download And Clean The Data

``` r
df <- read.csv("C:/Users/Martin/Desktop/Datasets/dataquest-book-reviews/book_reviews.csv")
head(df)
```

    ##                                 book    review      state price
    ## 1                        R Made Easy Excellent         TX 19.99
    ## 2                      R For Dummies      Fair         NY 15.99
    ## 3                        R Made Easy Excellent         NY 19.99
    ## 4                        R Made Easy      Poor         FL 19.99
    ## 5 Secrets Of R For Advanced Students     Great      Texas 50.00
    ## 6                        R Made Easy      <NA> California 19.99

``` r
glimpse(df)
```

    ## Rows: 2,000
    ## Columns: 4
    ## $ book   <chr> "R Made Easy", "R For Dummies", "R Made Easy", "R Made Easy", "~
    ## $ review <chr> "Excellent", "Fair", "Excellent", "Poor", "Great", NA, "Great",~
    ## $ state  <chr> "TX", "NY", "NY", "FL", "Texas", "California", "Florida", "CA",~
    ## $ price  <dbl> 19.99, 15.99, 19.99, 19.99, 50.00, 19.99, 19.99, 19.99, 29.99, ~

The data set has **`4 columns`** and **`2000 rows`**. The column types
are **`character`** except the **`price`** column which is type
**`double`**.

``` r
unique(df[c('review')])
```

    ##       review
    ## 1  Excellent
    ## 2       Fair
    ## 4       Poor
    ## 5      Great
    ## 6       <NA>
    ## 13      Good

``` r
unique(df[c('book')])
```

    ##                                  book
    ## 1                         R Made Easy
    ## 2                       R For Dummies
    ## 5  Secrets Of R For Advanced Students
    ## 9    Top 10 Mistakes R Beginners Make
    ## 32    Fundamentals of R For Beginners

``` r
max(df$price)
```

    ## [1] 50

``` r
min(df$price)
```

    ## [1] 15.99

The review column has NA values. Prices ranges between $15.99 and $50

### Remove Rows With NA

In this project the NA values will be removed rather than using
imputation.

``` r
cbind(
  lapply(
    lapply(
      df, is.na), sum
  )
)
```

    ##        [,1]
    ## book   0   
    ## review 206 
    ## state  0   
    ## price  0

``` r
df <- df %>% filter(
  !(is.na(df$review))
)
print(sum(is.na(df$review)))
```

    ## [1] 0

``` r
nrow(df)
```

    ## [1] 1794

206 rows with NA in the **`review`** column have been removed from the
data set leaving 1794 rows.

``` r
unique(df[c('state')])
```

    ##         state
    ## 1          TX
    ## 2          NY
    ## 4          FL
    ## 5       Texas
    ## 6     Florida
    ## 7          CA
    ## 13 California
    ## 23   New York

### Make State Column Values Consistent

``` r
df <- df %>% mutate(
  state = case_when(
    state == 'Texas' ~ 'TX',
    state == 'Florida' ~ 'FL',
    state == 'California' ~ 'CA',
    state == 'New York' ~ 'NY',
    TRUE ~ state
  )
)
unique(df[c('state')])
```

    ##   state
    ## 1    TX
    ## 2    NY
    ## 4    FL
    ## 7    CA

### Convert Review Values to Numeric Form

``` r
unique(df[c('review')])
```

    ##       review
    ## 1  Excellent
    ## 2       Fair
    ## 4       Poor
    ## 5      Great
    ## 12      Good

``` r
df <- df %>% mutate(
  review_num = case_when(
    review == 'Excellent' ~ 5,
    review == 'Great' ~ 4,
    review == 'Good' ~ 3,
    review == 'Fair' ~ 2,
    review == 'Poor' ~ 1
  ),
  is_high_review = review_num >= 4
)
head(df)
```

    ##                                 book    review state price review_num
    ## 1                        R Made Easy Excellent    TX 19.99          5
    ## 2                      R For Dummies      Fair    NY 15.99          2
    ## 3                        R Made Easy Excellent    NY 19.99          5
    ## 4                        R Made Easy      Poor    FL 19.99          1
    ## 5 Secrets Of R For Advanced Students     Great    TX 50.00          4
    ## 6                        R Made Easy     Great    FL 19.99          4
    ##   is_high_review
    ## 1           TRUE
    ## 2          FALSE
    ## 3           TRUE
    ## 4          FALSE
    ## 5           TRUE
    ## 6           TRUE

### Find What Books Are Most Profitable

``` r
price_df <- df[c('book', 'price')] %>% 
  group_by(book) %>%
  summarise(mean(price))
price_df
```

    ## # A tibble: 5 x 2
    ##   book                               `mean(price)`
    ##   <chr>                                      <dbl>
    ## 1 Fundamentals of R For Beginners             40.0
    ## 2 R For Dummies                               16.0
    ## 3 R Made Easy                                 20.0
    ## 4 Secrets Of R For Advanced Students          50  
    ## 5 Top 10 Mistakes R Beginners Make            30.0

``` r
most_profitable <- df %>% group_by(book) %>%
  summarise(total_profit = sum(price),
            total_sales = length(book),
            percent_reviews_above_good = round(length(is_high_review[is_high_review == TRUE]) / total_sales * 100, 1)) %>% 
  arrange(-total_profit)
most_profitable
```

    ## # A tibble: 5 x 4
    ##   book                               total_profit total_sales percent_reviews_a~
    ##   <chr>                                     <dbl>       <int>              <dbl>
    ## 1 Secrets Of R For Advanced Students       18000          360               37.5
    ## 2 Fundamentals of R For Beginners          14636.         366               41.3
    ## 3 Top 10 Mistakes R Beginners Make         10646.         355               39.7
    ## 4 R Made Easy                               7036.         352               39.5
    ## 5 R For Dummies                             5772.         361               35.5

The most profitable book is **`Secrets Of R For Advanced Students`**.
This is also the most expensive book at $50.

### Part 2 Project Outline

The company has updated data based on a new marketing program. They want
to know if this new program has improved sales.

``` r
df2 <- read_csv('C:/Users/Martin/Desktop/Datasets/dataquest-book-sales-data/sales2019.csv')
```

    ## Rows: 5000 Columns: 5

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (3): user_submitted_review, title, customer_type
    ## dbl  (1): total_purchased
    ## date (1): date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(df2)
```

    ## # A tibble: 6 x 5
    ##   date       user_submitted_review    title        total_purchased customer_type
    ##   <date>     <chr>                    <chr>                  <dbl> <chr>        
    ## 1 2019-05-22 it was okay              Secrets Of ~               7 Business     
    ## 2 2019-11-16 Awesome!                 R For Dummi~               3 Business     
    ## 3 2019-06-27 Awesome!                 R For Dummi~               1 Individual   
    ## 4 2019-11-06 Awesome!                 Fundamental~               3 Individual   
    ## 5 2019-07-18 Hated it                 Fundamental~              NA Business     
    ## 6 2019-01-28 Never read a better book Secrets Of ~               1 Business

``` r
glimpse(df2)
```

    ## Rows: 5,000
    ## Columns: 5
    ## $ date                  <date> 2019-05-22, 2019-11-16, 2019-06-27, 2019-11-06,~
    ## $ user_submitted_review <chr> "it was okay", "Awesome!", "Awesome!", "Awesome!~
    ## $ title                 <chr> "Secrets Of R For Advanced Students", "R For Dum~
    ## $ total_purchased       <dbl> 7, 3, 1, 3, NA, 1, 5, NA, 7, 1, 7, NA, 3, 2, 0, ~
    ## $ customer_type         <chr> "Business", "Business", "Individual", "Individua~

``` r
cbind(
  lapply(
    lapply(df2, is.na), sum)
  )
```

    ##                       [,1]
    ## date                  0   
    ## user_submitted_review 885 
    ## title                 0   
    ## total_purchased       718 
    ## customer_type         0

``` r
min(df2$date)
```

    ## [1] "2019-01-01"

``` r
max(df2$date)
```

    ## [1] "2019-12-31"

``` r
length(unique(df2$date))
```

    ## [1] 365

``` r
unique(df2$user_submitted_review)
```

    ##  [1] "it was okay"                         
    ##  [2] "Awesome!"                            
    ##  [3] "Hated it"                            
    ##  [4] "Never read a better book"            
    ##  [5] "OK"                                  
    ##  [6] "The author's other books were better"
    ##  [7] "A lot of material was not needed"    
    ##  [8] NA                                    
    ##  [9] "Would not recommend"                 
    ## [10] "I learned a lot"

``` r
unique(df2$title)
```

    ## [1] "Secrets Of R For Advanced Students" "R For Dummies"                     
    ## [3] "Fundamentals of R For Beginners"    "R vs Python: An Essay"             
    ## [5] "Top 10 Mistakes R Beginners Make"   "R Made Easy"

``` r
unique(df2$total_purchased)
```

    ##  [1]  7  3  1 NA  5  2  0  6  4  8  9 10 11 12

``` r
unique(df2$customer_type)
```

    ## [1] "Business"   "Individual"

### The Data Summary

The data has **`5 columns`** and **`5000 rows`**. The
**`user_submitted_review`** and **`total_purchased`** columns have
missing values. The data was recorded each day in 2019.

### Missing data

Remove rows where the **`user_submitted_review`** column has NA values.

``` r
df2 <- df2 %>% filter(
  !(is.na(df2$user_submitted_review))
)
nrow(df2)
```

    ## [1] 4115

Replace NA values in the **`total_purchased`** column with the mean
total\_purchased.

``` r
total_purchased_mean <- df2 %>% filter(
  !(is.na(df2$total_purchased))
)
mean <- mean(total_purchased_mean$total_purchased)
mean
```

    ## [1] 3.985561

``` r
df2 <- df2 %>% mutate(
  total_purchased = if_else(is.na(total_purchased),
                            ceiling(mean), total_purchased)
)
unique(df2$total_purchased)
```

    ##  [1]  7  3  1  4  5  2  6  8  9 10 11  0 12

``` r
mean(df2$total_purchased)
```

    ## [1] 3.987606

### Create A Column To Determine If The Review Is Positive

``` r
unique(df2$user_submitted_review)
```

    ## [1] "it was okay"                         
    ## [2] "Awesome!"                            
    ## [3] "Hated it"                            
    ## [4] "Never read a better book"            
    ## [5] "OK"                                  
    ## [6] "The author's other books were better"
    ## [7] "A lot of material was not needed"    
    ## [8] "Would not recommend"                 
    ## [9] "I learned a lot"

``` r
pos_review <- function(string) {
  positive <- case_when(
    string == 'Awesome!' ~ TRUE,
    string == 'Never read a better book' ~ TRUE,
    string == 'I learned a lot' ~ TRUE,
    TRUE ~ FALSE
  )
}

df2 <- df2 %>% mutate(
  positive_review = unlist(map(user_submitted_review, pos_review))
)
head(df2)
```

    ## # A tibble: 6 x 6
    ##   date       user_submitted_review    title        total_purchased customer_type
    ##   <date>     <chr>                    <chr>                  <dbl> <chr>        
    ## 1 2019-05-22 it was okay              Secrets Of ~               7 Business     
    ## 2 2019-11-16 Awesome!                 R For Dummi~               3 Business     
    ## 3 2019-06-27 Awesome!                 R For Dummi~               1 Individual   
    ## 4 2019-11-06 Awesome!                 Fundamental~               3 Individual   
    ## 5 2019-07-18 Hated it                 Fundamental~               4 Business     
    ## 6 2019-01-28 Never read a better book Secrets Of ~               1 Business     
    ## # ... with 1 more variable: positive_review <lgl>

### Split The Data By Date Before And Date After The Marketing Program

``` r
head(df2$date)
```

    ## [1] "2019-05-22" "2019-11-16" "2019-06-27" "2019-11-06" "2019-07-18"
    ## [6] "2019-01-28"

``` r
df2 <- df2 %>% mutate(
  date = ymd(date),
  marketing_program = if_else(date < ymd('2019/07/01'), 'Before', 'After')
)
head(df2)
```

    ## # A tibble: 6 x 7
    ##   date       user_submitted_review    title        total_purchased customer_type
    ##   <date>     <chr>                    <chr>                  <dbl> <chr>        
    ## 1 2019-05-22 it was okay              Secrets Of ~               7 Business     
    ## 2 2019-11-16 Awesome!                 R For Dummi~               3 Business     
    ## 3 2019-06-27 Awesome!                 R For Dummi~               1 Individual   
    ## 4 2019-11-06 Awesome!                 Fundamental~               3 Individual   
    ## 5 2019-07-18 Hated it                 Fundamental~               4 Business     
    ## 6 2019-01-28 Never read a better book Secrets Of ~               1 Business     
    ## # ... with 2 more variables: positive_review <lgl>, marketing_program <chr>

Total sales before and after the marketing program.

``` r
df2 %>% group_by(marketing_program) %>%
  summarise(total_sales = sum(total_purchased))
```

    ## # A tibble: 2 x 2
    ##   marketing_program total_sales
    ##   <chr>                   <dbl>
    ## 1 After                    8194
    ## 2 Before                   8215

This doesn’t show that the program has increased sales.

Below shows the total sales before and after the marketing program for
business and individual customers.

``` r
df2 %>% group_by(marketing_program, customer_type) %>%
  summarise(total_sales = sum(total_purchased))
```

    ## `summarise()` has grouped output by 'marketing_program'. You can override using the `.groups` argument.

    ## # A tibble: 4 x 3
    ## # Groups:   marketing_program [2]
    ##   marketing_program customer_type total_sales
    ##   <chr>             <chr>               <dbl>
    ## 1 After             Business             5745
    ## 2 After             Individual           2449
    ## 3 Before            Business             5615
    ## 4 Before            Individual           2600

The program may work better for business customers but at this level no
improvement is shown for individual customers.

Below shows the total sales before and after the marketing program for
positive and average or lower reviews.

``` r
df2 %>% group_by(marketing_program, positive_review) %>%
  summarise(total_sales = sum(total_purchased))
```

    ## `summarise()` has grouped output by 'marketing_program'. You can override using the `.groups` argument.

    ## # A tibble: 4 x 3
    ## # Groups:   marketing_program [2]
    ##   marketing_program positive_review total_sales
    ##   <chr>             <lgl>                 <dbl>
    ## 1 After             FALSE                  5478
    ## 2 After             TRUE                   2716
    ## 3 Before            FALSE                  5610
    ## 4 Before            TRUE                   2605

This shows an improvement in reviews since the program began.
