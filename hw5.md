hw5
================
Brennan Baker
November 6, 2018

-   [Problem 1](#problem-1)
    -   [Load the data](#load-the-data)
    -   [Create final df from loaded data](#create-final-df-from-loaded-data)
    -   [Make spaghetti plot](#make-spaghetti-plot)
-   [Problem 2](#problem-2)
    -   [Load the data](#load-the-data-1)
    -   [Description of data](#description-of-data)
    -   [Homicides by city](#homicides-by-city)
    -   [prop.test](#prop.test)
    -   [Plots](#plots)

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

``` r
file_names = list.files(path = "./data") #make list of file names

load_data = function(x) {
  
  df = read_csv(file = str_c("./data/", x)) %>% 
    mutate(file = x) %>% #adds col with file name
    separate(file, into = c("file", "remove"), sep = "\\.") %>% #removes .csv from file col
    select(-remove) %>% 
    separate(file, into = c("arm", "subject_id"), sep = "_") #separate file col into arm and id
  df
  
}

loaded_data = map(file_names, load_data)
```

    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_integer(),
    ##   week_8 = col_double()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   week_1 = col_double(),
    ##   week_2 = col_double(),
    ##   week_3 = col_double(),
    ##   week_4 = col_double(),
    ##   week_5 = col_double(),
    ##   week_6 = col_double(),
    ##   week_7 = col_double(),
    ##   week_8 = col_double()
    ## )

### Create final df from loaded data

``` r
#initialize the final_df with one participants data
final_df = loaded_data[[1]] 

#add data from other participants by iterating through loaded data and binding rows
for (i in 2:length(loaded_data)) {
  final_df = bind_rows(final_df,loaded_data[[i]])
}

#tidy by gathering week, then remove redundant "week" in each value (turn "week_1" into "1")
#change con and exp for nicer plot labels
final_df = final_df %>% 
  gather(key = week, value = observation, week_1:week_8) %>% 
  separate(week, into = c("remove", "week"), sep = "_") %>%
  select(-remove) %>% 
  mutate(arm = replace(arm, arm == "con", "control"),
         arm = replace(arm, arm == "exp", "experimental"))
```

### Make spaghetti plot

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

The raw data contains a row for each victim of a homicide. The information on each observation includes demographic information of the victim, the location of the killing (city, state, longitude, and latitude), the date of the homicide, and whether an arrest was made (disposition). Each row also has a unique id.

### Homicides by city

This code first uses summarize to make a df with the number of homicides by disposition. The second summarize function sums the total homicides and the number of unsolved homicides in each city.

``` r
homicides_df = homicides_df %>% 
  mutate(city_state = str_c(city, ",\ ", state)) %>% 
  group_by(city_state, disposition) %>% 
  summarize(number_homicides = n()) %>%
  summarize(total_homicides = sum(number_homicides),
            unsolved_homicides = sum(number_homicides[disposition != "Closed by arrest"]))
```

There were 2827 total homicides in Baltimore, MD, of which 1825 were unsolved.

### prop.test

As an argument, prop.test takes a two-dimensional table (or matrix) with 2 columns, giving the counts of successes and failures, respectively. Alternatively, you can set x = numerator, n = denominator for the proportion.

Prop.test just for balitmore. In the tibble below we can see the estimate and the CIs from the test.

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
#Below is a function that does a prop. test for a given row of the homicides_df
prop_test_function = function(x) {
  
  test_result =
        broom::tidy(
          prop.test(x = homicides_df[["unsolved_homicides"]][[x]], homicides_df[["total_homicides"]][[x]])) %>% 
            select(estimate, conf.low, conf.high)
  test_result
}

#Below is a map function that runs the prop. test on each row and saves the results to a list of lists. Then I unnest for later graphing
output = map(1:nrow(homicides_df), prop_test_function)
```

    ## Warning in prop.test(x = homicides_df[["unsolved_homicides"]][[x]],
    ## homicides_df[["total_homicides"]][[x]]): Chi-squared approximation may be
    ## incorrect

``` r
#Below I create a df with the new prop. tests saved as a list column
homicides_df_prop_tests = homicides_df %>%
  mutate(prop_test = output) %>% 
  unnest() %>% 
  janitor::clean_names()
```

### Plots

``` r
homicides_df_prop_tests %>% 
  ggplot(aes(x = city_state, y = estimate)) +
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

![](hw5_files/figure-markdown_github/unnamed-chunk-2-1.png)

Now run prop.test for each of the cities in your dataset, and extract both the proportion of unsolved homicides and the confidence interval for each. Do this within a “tidy” pipeline, making use of purrr::map, purrr::map2, list columns and unnest as necessary to create a tidy dataframe with estimated proportions and CIs for each city.

Create a plot that shows the estimates and CIs for each city – check out geom\_errorbar for a way to add error bars based on the upper and lower limits. Organize cities according to the proportion of unsolved homicides.

Survey Please complete this survey regarding extra topics to cover at the end of the semester.
