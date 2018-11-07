hw5
================
Brennan Baker
November 6, 2018

-   [Problem 1](#problem-1)
    -   [Load the data](#load-the-data)
    -   [Create final df from loaded data](#create-final-df-from-loaded-data)
    -   [Spaghetti plot](#spaghetti-plot)
-   [Problem 2](#problem-2)
    -   [Load the data](#load-the-data-1)
    -   [Description of data](#description-of-data)
    -   [Homicides by city](#homicides-by-city)
    -   [Prop. test](#prop.-test)
    -   [Prop. test plot](#prop.-test-plot)

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Problem 1
=========

### Load the data

### Create final df from loaded data

``` r
# Initialize output of the for loop, final_df, with one participants data
final_df = loaded_data[[1]] 

# Add data from other participants by iterating through loaded data (produced in the map function above) and binding rows to the final_df.
for (i in 2:length(loaded_data)) {
  final_df = bind_rows(final_df,loaded_data[[i]])
}

# Tidy by gathering week, then remove redundant "week" in each value (turn "week_1" into "1")
final_df = final_df %>% 
  gather(key = week, value = observation, week_1:week_8) %>% 
  separate(week, into = c("remove", "week"), sep = "_") %>%
  select(-remove) %>% 
  mutate(arm = replace(arm, arm == "con", "control"),
         arm = replace(arm, arm == "exp", "experimental"))
```

### Spaghetti plot

``` r
final_df %>% 
  mutate(week = as.numeric(week)) %>% 
  ggplot(aes(x = week, y = observation, color = subject_id)) +
  geom_line() +
  facet_grid(~arm) +
  labs(
    title = "Observations over time in each arm",
    x = "Week",
    y = "Observation"
  ) + 
  theme_bw()
```

![](hw5_files/figure-markdown_github/spaghetti%20plot-1.png)

In the experimental arm, the value of the observations increased over time, while in the control arm, there was no change in observations over time.

Problem 2
=========

### Load the data

``` r
homicides_df = read_csv(file = "./data-homicides-master/homicide-data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

### Description of data

The raw data contains a row for each victim of a homicide. The information on each observation includes demographic information of the victim, the location of the killing (city, state, longitude, and latitude), the date of the homicide, and whether an arrest was made (disposition). Each row / victim also has a unique id.

### Homicides by city

After crerating a new city\_state variable, the code below uses summarize to make a df with the number of homicides in each city\_state by disposition. The second summarize function sums the total homicides and the number of unsolved homicides in each city.

``` r
homicides_df = homicides_df %>% 
  mutate(city_state = str_c(city, ",\ ", state)) %>% 
  group_by(city_state, disposition) %>% 
  summarize(number_homicides = n()) %>%
  summarize(total_homicides = sum(number_homicides),
            unsolved_homicides = sum(number_homicides[disposition != "Closed by arrest"]))
homicides_df
```

    ## # A tibble: 51 x 3
    ##    city_state      total_homicides unsolved_homicides
    ##    <chr>                     <int>              <int>
    ##  1 Albuquerque, NM             378                146
    ##  2 Atlanta, GA                 973                373
    ##  3 Baltimore, MD              2827               1825
    ##  4 Baton Rouge, LA             424                196
    ##  5 Birmingham, AL              800                347
    ##  6 Boston, MA                  614                310
    ##  7 Buffalo, NY                 521                319
    ##  8 Charlotte, NC               687                206
    ##  9 Chicago, IL                5535               4073
    ## 10 Cincinnati, OH              694                309
    ## # ... with 41 more rows

For example, we can see from above that there were 2827 total homicides in Baltimore, MD, of which 1825 were unsolved.

### Prop. test

As an argument, prop.test takes a two-dimensional table (or matrix) with 2 columns, giving the counts of successes and failures, respectively. Alternatively, you can set x = numerator, n = denominator for the proportion.

Prop.test just for balitmore. In the tibble below we can see the estimate and the CIs from the test. As a confirmation for the results, 1825/2827 does = 0.646 (the estimate).

``` r
baltimore_proptest = prop.test(x = 1825, n = 2827)
broom::tidy(baltimore_proptest) %>% 
  select(estimate, conf.low, conf.high)
```

    ## # A tibble: 1 x 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1    0.646    0.628     0.663

Prop. tests for every city

``` r
#Below is a function that does a prop. test for a given row of the homicides_df.
prop_test_function = function(x) {
  
  test_result =
        broom::tidy(
          prop.test(x = homicides_df[["unsolved_homicides"]][[x]], homicides_df[["total_homicides"]][[x]])) %>% 
            select(estimate, conf.low, conf.high)
  test_result
}

#Below is a map function that runs the prop. test on each row and saves the results to a list of lists (output)
output = map(1:nrow(homicides_df), prop_test_function)
```

    ## Warning in prop.test(x = homicides_df[["unsolved_homicides"]][[x]],
    ## homicides_df[["total_homicides"]][[x]]): Chi-squared approximation may be
    ## incorrect

``` r
#Below I create a df with the new prop. tests saved as a list column called "prop_test." Then I unnest the list column for graphing below.
homicides_df_prop_tests = homicides_df %>%
  mutate(prop_test = output) %>% 
  unnest() %>% 
  janitor::clean_names()
```

### Prop. test plot

``` r
homicides_df_prop_tests %>% 
  ggplot(aes(x = reorder(city_state, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high)) +
  labs(
    title = "Unsolved cases by city",
    x = "City",
    y = "Proportion of cases unsolved"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw5_files/figure-markdown_github/prop%20test%20plot-1.png)

We can see from the plot that Tulsa, AL has an interesting looking confidence interval. From the data set I see that in Tulsa, AL, there was only 1 homicide (which was unsolved). Thus, the estimate should be 0 as it is in the graph, and the low sample size of homicides explains the wide confidence interval.

It is likely that the data for Tulsa AL is incorrect. Tulsa AL does not exist and that one homicide probably belongs as a data point for Tulsa OK. Below I fix this problem

``` r
# Add 1 to the total homicides in Tulsa, OK
homicides_df_prop_tests[["total_homicides"]][[50]] = 584

# Remove the row for Tulsa, AL
homicides_df_prop_tests = homicides_df_prop_tests %>%
  filter(city_state != "Tulsa,\ AL")
# Re plot
homicides_df_prop_tests %>% 
  ggplot(aes(x = reorder(city_state, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high)) +
  labs(
    title = "Unsolved cases by city",
    x = "City",
    y = "Proportion of cases unsolved"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw5_files/figure-markdown_github/remove%20Tulsa%20AL-1.png)
