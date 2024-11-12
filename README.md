------------------------------------------------------------------------

## output: github_document

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(readr)
library(stringr)

# Transforming Death columns into long format
deaths <- av %>%
  pivot_longer(cols = starts_with("Death"), names_to = "Time", values_to = "Death") %>%
  mutate(Time = parse_number(Time), Death = replace_na(Death, ""))
head(deaths)
```

    ## # A tibble: 6 × 18
    ##   URL                 Name.Alias Appearances Current. Gender Probationary.Introl
    ##   <chr>               <chr>            <int> <chr>    <chr>  <chr>              
    ## 1 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 2 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 3 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 4 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 5 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 6 http://marvel.wiki… "Janet va…        1165 YES      FEMALE ""                 
    ## # ℹ 12 more variables: Full.Reserve.Avengers.Intro <chr>, Year <int>,
    ## #   Years.since.joining <int>, Honorary <chr>, Return1 <chr>, Return2 <chr>,
    ## #   Return3 <chr>, Return4 <chr>, Return5 <chr>, Notes <chr>, Time <dbl>,
    ## #   Death <chr>

Similarly, deal with the returns of characters.

``` r
# Transforming Return columns into long format
returns <- av %>%
  pivot_longer(cols = starts_with("Return"), names_to = "Time", values_to = "Return") %>%
  mutate(Time = parse_number(Time), Return = replace_na(Return, ""))
head(returns)
```

    ## # A tibble: 6 × 18
    ##   URL                 Name.Alias Appearances Current. Gender Probationary.Introl
    ##   <chr>               <chr>            <int> <chr>    <chr>  <chr>              
    ## 1 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 2 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 3 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 4 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 5 http://marvel.wiki… "Henry Jo…        1269 YES      MALE   ""                 
    ## 6 http://marvel.wiki… "Janet va…        1165 YES      FEMALE ""                 
    ## # ℹ 12 more variables: Full.Reserve.Avengers.Intro <chr>, Year <int>,
    ## #   Years.since.joining <int>, Honorary <chr>, Death1 <chr>, Death2 <chr>,
    ## #   Death3 <chr>, Death4 <chr>, Death5 <chr>, Notes <chr>, Time <dbl>,
    ## #   Return <chr>

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
average_deaths <- deaths %>%
  filter(Death == "YES") %>%
  group_by(`Name.Alias`) %>%
  summarize(num_deaths = n()) %>%
  summarize(avg_deaths = mean(num_deaths))
average_deaths
```

    ## # A tibble: 1 × 1
    ##   avg_deaths
    ##        <dbl>
    ## 1       1.39

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### Keenan Jacobs:

### FiveThirtyEight Statement

> “I counted 89 total deaths — some unlucky Avengers are basically Meat
> Loaf with an E-ZPass — and on 57 occasions the individual made a
> comeback.”

### Code to Fact-Check

``` r
# Counting total deaths
total_deaths <- deaths %>%
  filter(Death == "YES") %>%
  summarize(total_deaths = n())

# Counting total returns
total_returns <- returns %>%
  filter(Return == "YES") %>%
  summarize(total_returns = n())

# Display both totals
total_deaths
```

    ## # A tibble: 1 × 1
    ##   total_deaths
    ##          <int>
    ## 1           89

``` r
total_returns
```

    ## # A tibble: 1 × 1
    ##   total_returns
    ##           <int>
    ## 1            57

### My Answer

The data confirms the statement from FiveThirtyEight regarding the
Avengers’ mortality and comebacks. We found exactly 89 deaths and 57
returns among the characters, aligning with the claim that “89 total
deaths” and “57 occasions the individual made a comeback.”

------------------------------------------------------------------------

### Muhammad Raham Saleem:

### FiveThirtyEight Statement

> But you can only tempt death so many times. There’s a 2-in-3 chance
> that a member of the Avengers returned from their first stint in the
> afterlife, but only a 50 percent chance they recovered from a second
> or third death.

### Code

``` r
# Ensure consistent lowercase values in Death and Return columns
deaths <- deaths %>%
  mutate(Death = tolower(Death))

returns <- returns %>%
  mutate(Return = tolower(Return))

# Merge the 'deaths' and 'returns' datasets based on Name.Alias and Time
death_return <- left_join(deaths, returns, by = c("Name.Alias", "Time"), relationship = "many-to-many")

# Calculate the return probability for the first three death instances only
return_probabilities <- death_return %>%
  filter(Death == "yes", Time <= 3) %>%  # Only consider rows where Death is "yes" and Time is 1, 2, or 3
  group_by(Time) %>%
  summarize(
    total_deaths = n(),                    # Count of total deaths at each time (e.g., first death, second death)
    total_returns = sum(Return == "yes"),   # Count of returns after each death
    return_probability = total_returns / total_deaths # Probability of return
  )

# Display the calculated return probabilities for the first three deaths
print("Return probabilities for the first three death instances (Time):")
```

    ## [1] "Return probabilities for the first three death instances (Time):"

``` r
return_probabilities
```

    ## # A tibble: 3 × 4
    ##    Time total_deaths total_returns return_probability
    ##   <dbl>        <int>         <int>              <dbl>
    ## 1     1          123            56              0.455
    ## 2     2           25             8              0.32 
    ## 3     3            2             1              0.5

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer

First Death:

The return probability after the first death is approximately 45.5%
(0.455), which is lower than the expected 2-in-3 chance (66.7%). This
means that, contrary to the claim, Avengers have less than a 50% chance
of returning after their first death based on this dataset. Second
Death:

The return probability after the second death is 32.0% (0.32), which is
below the expected 50%. This suggests that fewer Avengers return after
their second death than claimed. Third Death:

The return probability after the third death is 50% (0.5), which aligns
with the claim of a 50% chance of returning after a third death. Fourth
and Fifth Deaths:

After the first death, the return probability is 45.5%, not the claimed
66.7%. After the second death, the return probability is 32%, lower than
the expected 50%. After the third death, the return probability does
align with 50%, which is consistent with the claim. These findings
suggest that the likelihood of returning from death decreases over time
and is generally lower than the quoted statistics in the claim.

\###Bradyn Weaver

### FiveThirtyEight Statement

> Given the Avengers’ 53 years in operation and overall mortality rate,
> fans of the comics can expect one current or former member to die
> every seven months or so, with a permanent death occurring once every
> 20 months.

``` r
#For calculating the average months per death:

total_deaths <- sum(av$Death1 == "YES", na.rm = TRUE)
permanent_deaths <- sum(av$Return1 == "NO", na.rm = TRUE)

# Assuming the dataset covers 53 years of Avengers history:
years_in_operation <- 53

# Calculate the average time between any death and permanent deaths
average_months_per_death <- (years_in_operation * 12) / total_deaths
average_months_per_permanent_death <- (years_in_operation * 12) / permanent_deaths

# Print results
cat("Average time between any death:", average_months_per_death, "months\n")
```

    ## Average time between any death: 9.217391 months

``` r
cat("Average time between permanent deaths:", average_months_per_permanent_death, "months\n")
```

    ## Average time between permanent deaths: 27.65217 months

### Include your answer

Based on the dataset analysis above, it appears that FiveThirtyEight may
have been off by 2 months for their average time between any death
estimate. It also appears they were off by 7 months for their permanent
death rate estimate.

### Meghasyam Peddireddy

### FiveThirtyEight Statement

> “If a character is killed but then secretly hidden away in a stasis
> tube, they died.”

### Code to Fact-Check

``` r
# Ensuring consistent values in Death and Return columns
deaths <- deaths %>%
  mutate(Death = tolower(Death))

returns <- returns %>%
  mutate(Return = tolower(Return))

# Merging deaths and returns datasets to check cases where a character 'died' but later 'returned'
death_stasis_check <- deaths %>%
  inner_join(returns, by = c("Name.Alias", "Time")) %>%
  filter(Death == "yes" & Return == "yes")
```

    ## Warning in inner_join(., returns, by = c("Name.Alias", "Time")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 141 of `x` matches multiple rows in `y`.
    ## ℹ Row 281 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Counting the number of such "stasis tube" instances
stasis_count <- nrow(death_stasis_check)

# Displaying the result
death_stasis_check
```

    ## # A tibble: 67 × 34
    ##    URL.x      Name.Alias Appearances.x Current..x Gender.x Probationary.Introl.x
    ##    <chr>      <chr>              <int> <chr>      <chr>    <chr>                
    ##  1 http://ma… "Janet va…          1165 YES        FEMALE   ""                   
    ##  2 http://ma… "Anthony …          3068 YES        MALE     ""                   
    ##  3 http://ma… "Robert B…          2089 YES        MALE     ""                   
    ##  4 http://ma… "Thor Odi…          2402 YES        MALE     ""                   
    ##  5 http://ma… "Steven R…          3458 YES        MALE     ""                   
    ##  6 http://ma… "Clinton …          1456 YES        MALE     ""                   
    ##  7 http://ma… "Clinton …          1456 YES        MALE     ""                   
    ##  8 http://ma… "Pietro M…           769 YES        MALE     ""                   
    ##  9 http://ma… "Wanda Ma…          1214 YES        FEMALE   ""                   
    ## 10 http://ma… "Jacques …           115 NO         MALE     ""                   
    ## # ℹ 57 more rows
    ## # ℹ 28 more variables: Full.Reserve.Avengers.Intro.x <chr>, Year.x <int>,
    ## #   Years.since.joining.x <int>, Honorary.x <chr>, Return1 <chr>,
    ## #   Return2 <chr>, Return3 <chr>, Return4 <chr>, Return5 <chr>, Notes.x <chr>,
    ## #   Time <dbl>, Death <chr>, URL.y <chr>, Appearances.y <int>,
    ## #   Current..y <chr>, Gender.y <chr>, Probationary.Introl.y <chr>,
    ## #   Full.Reserve.Avengers.Intro.y <chr>, Year.y <int>, …

``` r
stasis_count
```

    ## [1] 67

### My Answer

Based on the result from the `death_stasis_check` table, we identified
**67** instances where characters were marked as “dead” but later
“returned.” This supports the FiveThirtyEight statement that Avengers
who are believed to be dead may not always remain so, with some
effectively being “hidden away in a stasis tube” or similar scenarios
that enable their return. This finding aligns with the article’s claim,
illustrating that in the Marvel universe, death is often temporary, and
characters frequently find ways to reappear after seeming to perish.
