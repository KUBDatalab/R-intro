---
title: "Data Wrangling with dplyr and tidyr"

teaching: 20
exercises: 10

---

:::: questions

- How can I select specific rows and/or columns from a dataframe?
- How can I combine multiple commands into a single command?
- How can I create new columns or remove existing columns from a dataframe?
- How can I reformat a dataframe to meet my needs?

::::


:::: objectives

- Describe the purpose of an R package and the **`dplyr`** and **`tidyr`** packages.
- Select certain columns in a dataframe with the **`dplyr`** function `select`.
- Select certain rows in a dataframe according to filtering conditions with the **`dplyr`**
  function `filter`.
- Link the output of one **`dplyr`** function to the input of another function with
  the 'pipe' operator `%>%`.
- Add new columns to a dataframe that are functions of existing columns with `mutate`.
- Use the split-apply-combine concept for data analysis.
- Use `summarize`, `group_by`, and `count` to split a dataframe into groups of observations,
  apply a summary statistics for each group, and then combine the results.
- Describe the concept of a wide and a long table format and for which purpose those
  formats are useful.
- Describe the roles of variable names and their associated values when a table is
  reshaped.
- Reshape a dataframe from long to wide format and back with the `pivot_wider` and
  `pivot_longer` commands from the **`tidyr`** package.
- Export a dataframe to a csv file.

::::



**`dplyr`** is a package for making tabular data wrangling easier by using a
limited set of functions that can be combined to extract and summarize insights
from your data. It pairs nicely with **`tidyr`** which enables you to swiftly
convert between different data formats (long vs. wide) for plotting and analysis.

Similarly to **`readr`**, **`dplyr`** and **`tidyr`** are also part of the
tidyverse. These packages were loaded in R's memory when we called
`library(tidyverse)` earlier.

:::: callout

## Note

 The packages in the tidyverse, namely **`dplyr`**, **`tidyr`** and **`ggplot2`**
 accept both the British (e.g. *summarise*) and American (e.g. *summarize*) spelling
 variants of different function and option names. For this lesson, we utilize
 the American spellings of different functions; however, feel free to use
 the regional variant for where you are teaching.

::::::

## What is an R package?

The package **`dplyr`** provides easy tools for the most common data
wrangling tasks. It is built to work directly with dataframes, with many
common tasks optimized by being written in a compiled language (C++) (not all R
packages are written in R!).

The package **`tidyr`** addresses the common problem of wanting to reshape your
data for plotting and use by different R functions. Sometimes we want data sets
where we have one row per measurement. Sometimes we want a dataframe where each
measurement type has its own column, and rows are instead more aggregated
groups. Moving back and forth between these formats is nontrivial, and
**`tidyr`** gives you tools for this and more sophisticated data wrangling.

But there are also packages available for a wide range of tasks including
building plots (**`ggplot2`**, which we'll see later), downloading data from the
NCBI database, or performing statistical analysis on your data set. Many
packages such as these are housed on, and downloadable from, the
**C**omprehensive **R** **A**rchive **N**etwork (CRAN) using `install.packages`.
This function makes the package accessible by your R installation with the
command `library()`, as you did with `tidyverse` earlier.

To easily access the documentation for a package within R or RStudio, use
`help(package = "package_name")`.

To learn more about **`dplyr`** and **`tidyr`** after the workshop, you may want
to check out this [handy data transformation with **`dplyr`** cheatsheet](https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf)
and this [one about **`tidyr`**](https://github.com/rstudio/cheatsheets/raw/master/data-import.pdf).

## Learning **`dplyr`** and **`tidyr`**

We are working with the same dataset as earlier. Refer to the 
previous lesson to download the data, if you do not have it loaded.

We're going to learn some of the most common **`dplyr`** functions:

- `select()`: subset columns
- `filter()`: subset rows on conditions
- `mutate()`: create new columns by using information from other columns
- `group_by()` and `summarize()`: create summary statistics on grouped data
- `arrange()`: sort results
- `count()`: count discrete values

## Selecting columns and filtering rows

To select columns of a dataframe, use `select()`. The first argument to this
function is the dataframe (`movieSerie`), and the subsequent arguments are the
columns to keep, separated by commas. Alternatively, if you are selecting
columns adjacent to each other, you can use a `:` to select a range of columns,
read as "select columns from ___ to ___."


```r
# to select columns throughout the dataframe
select(movieSerie, title, description)
```

```{.output}
# A tibble: 5,850 × 2
   title                               description                              
   <chr>                               <chr>                                    
 1 Five Came Back: The Reference Films "This collection includes 12 World War I…
 2 Taxi Driver                         "A mentally unstable Vietnam War veteran…
 3 Deliverance                         "Intent on seeing the Cahulawassee River…
 4 Monty Python and the Holy Grail     "King Arthur, accompanied by his squire,…
 5 The Dirty Dozen                     "12 American military prisoners in World…
 6 Monty Python's Flying Circus        "A British sketch comedy series with the…
 7 Life of Brian                       "Brian Cohen is an average young Jewish …
 8 Dirty Harry                         "When a madman dubbed 'Scorpio' terroriz…
 9 Bonnie and Clyde                    "In the 1930s, bored waitress Bonnie Par…
10 The Blue Lagoon                     "Two small children and a ship's cook su…
# ℹ 5,840 more rows
```

```r
# to select a series of connected columns
select(movieSerie, title:description)
```

```{.output}
# A tibble: 5,850 × 4
   title                               type  genre         description          
   <chr>                               <chr> <chr>         <chr>                
 1 Five Came Back: The Reference Films SHOW  documentation "This collection inc…
 2 Taxi Driver                         MOVIE drama         "A mentally unstable…
 3 Deliverance                         MOVIE drama         "Intent on seeing th…
 4 Monty Python and the Holy Grail     MOVIE fantasy       "King Arthur, accomp…
 5 The Dirty Dozen                     MOVIE war           "12 American militar…
 6 Monty Python's Flying Circus        SHOW  comedy        "A British sketch co…
 7 Life of Brian                       MOVIE comedy        "Brian Cohen is an a…
 8 Dirty Harry                         MOVIE thriller      "When a madman dubbe…
 9 Bonnie and Clyde                    MOVIE crime         "In the 1930s, bored…
10 The Blue Lagoon                     MOVIE romance       "Two small children …
# ℹ 5,840 more rows
```

To choose rows based on specific criteria, we can use the `filter()` function.
The argument after the dataframe is the condition we want our final
dataframe to adhere to (e.g. age_certification is PG-13): 


```r
# filters observations where age_certification name is "PG-13" 
filter(movieSerie, age_certification == "PG-13")
```

```{.output}
# A tibble: 451 × 14
   id       title type  genre description release_year age_certification runtime
   <chr>    <chr> <chr> <chr> <chr>              <dbl> <chr>               <dbl>
 1 tm67378  The … MOVIE west… "An arroga…         1966 PG-13                 117
 2 tm145608 Awak… MOVIE drama "Dr. Malco…         1990 PG-13                 120
 3 tm147710 Nati… MOVIE come… "It's Chri…         1989 PG-13                  97
 4 tm142895 Lean… MOVIE drama "When prin…         1989 PG-13                 105
 5 tm107744 Miss… MOVIE thri… "When Etha…         1996 PG-13                 110
 6 tm122434 Forr… MOVIE drama "A man wit…         1994 PG-13                 142
 7 tm191110 Tita… MOVIE drama "101-year-…         1997 PG-13                 194
 8 tm27395  Miss… MOVIE thri… "With comp…         2000 PG-13                 123
 9 tm113513 Dumb… MOVIE come… "Lloyd and…         1994 PG-13                 107
10 tm192405 Gatt… MOVIE scifi "In a futu…         1997 PG-13                 106
# ℹ 441 more rows
# ℹ 6 more variables: seasons <dbl>, imdb_id <chr>, imdb_score <dbl>,
#   imdb_votes <dbl>, tmdb_popularity <dbl>, tmdb_score <dbl>
```

We can also specify multiple conditions within the `filter()` function. We can
combine conditions using either "and" or "or" statements. In an "and" 
statement, an observation (row) must meet **every** criteria to be included
in the resulting dataframe. To form "and" statements within dplyr, we can  pass
our desired conditions as arguments in the `filter()` function, separated by
commas:


```r
# filters observations with "and" operator (comma)
# output dataframe satisfies ALL specified conditions
filter(movieSerie, age_certification == "PG-13",
                   runtime > 100,
                   imdb_score < 6.0)
```

```{.output}
# A tibble: 60 × 14
   id       title type  genre description release_year age_certification runtime
   <chr>    <chr> <chr> <chr> <chr>              <dbl> <chr>               <dbl>
 1 tm12499  The … MOVIE thri… "Angela Be…         1995 PG-13                 114
 2 tm93055  Grow… MOVIE come… "After the…         2010 PG-13                 102
 3 tm159901 Miss… MOVIE acti… "After her…         2005 PG-13                 115
 4 tm88045  How … MOVIE come… "After bei…         2010 PG-13                 121
 5 tm700    10,0… MOVIE acti… "A prehist…         2008 PG-13                 109
 6 tm39487  Catc… MOVIE drama "Gray Whee…         2006 PG-13                 111
 7 tm130574 Did … MOVIE come… "In New Yo…         2009 PG-13                 103
 8 tm148041 What… MOVIE come… "What's Yo…         2009 PG-13                 192
 9 tm237621 The … MOVIE horr… "Two buddi…         2009 PG-13                 122
10 tm59289  Rowd… MOVIE acti… "A small-t…         2012 PG-13                 143
# ℹ 50 more rows
# ℹ 6 more variables: seasons <dbl>, imdb_id <chr>, imdb_score <dbl>,
#   imdb_votes <dbl>, tmdb_popularity <dbl>, tmdb_score <dbl>
```

We can also form "and" statements with the `&` operator instead of commas:


```r
# filters observations with "&" logical operator
# output dataframe satisfies ALL specified conditions
filter(movieSerie, age_certification == "PG-13" & 
                   runtime > 100 & 
                   imdb_score < 6.0)
```

```{.output}
# A tibble: 60 × 14
   id       title type  genre description release_year age_certification runtime
   <chr>    <chr> <chr> <chr> <chr>              <dbl> <chr>               <dbl>
 1 tm12499  The … MOVIE thri… "Angela Be…         1995 PG-13                 114
 2 tm93055  Grow… MOVIE come… "After the…         2010 PG-13                 102
 3 tm159901 Miss… MOVIE acti… "After her…         2005 PG-13                 115
 4 tm88045  How … MOVIE come… "After bei…         2010 PG-13                 121
 5 tm700    10,0… MOVIE acti… "A prehist…         2008 PG-13                 109
 6 tm39487  Catc… MOVIE drama "Gray Whee…         2006 PG-13                 111
 7 tm130574 Did … MOVIE come… "In New Yo…         2009 PG-13                 103
 8 tm148041 What… MOVIE come… "What's Yo…         2009 PG-13                 192
 9 tm237621 The … MOVIE horr… "Two buddi…         2009 PG-13                 122
10 tm59289  Rowd… MOVIE acti… "A small-t…         2012 PG-13                 143
# ℹ 50 more rows
# ℹ 6 more variables: seasons <dbl>, imdb_id <chr>, imdb_score <dbl>,
#   imdb_votes <dbl>, tmdb_popularity <dbl>, tmdb_score <dbl>
```

In an "or" statement, observations must meet *at least one* of the specified conditions. 
To form "or" statements we use the logical operator for "or," which is the vertical bar (|): 


```r
# filters observations with "|" logical operator
# output dataframe satisfies AT LEAST ONE of the specified conditions
filter(movieSerie, age_certification == "PG-13" | 
                   runtime > 100 | 
                   imdb_score < 6.0)
```

```{.output}
# A tibble: 2,852 × 14
   id       title type  genre description release_year age_certification runtime
   <chr>    <chr> <chr> <chr> <chr>              <dbl> <chr>               <dbl>
 1 tm84618  Taxi… MOVIE drama A mentally…         1976 "R"                   114
 2 tm154986 Deli… MOVIE drama Intent on …         1972 "R"                   109
 3 tm120801 The … MOVIE war   12 America…         1967 ""                    150
 4 tm14873  Dirt… MOVIE thri… When a mad…         1971 "R"                   102
 5 tm119281 Bonn… MOVIE crime In the 193…         1967 "R"                   110
 6 tm98978  The … MOVIE roma… Two small …         1980 "R"                   104
 7 tm44204  The … MOVIE acti… A team of …         1961 ""                    158
 8 tm67378  The … MOVIE west… An arrogan…         1966 "PG-13"               117
 9 tm16479  Whit… MOVIE roma… Two talent…         1954 ""                    115
10 tm89386  Hitl… MOVIE hist… A keen chr…         1977 "PG"                  150
# ℹ 2,842 more rows
# ℹ 6 more variables: seasons <dbl>, imdb_id <chr>, imdb_score <dbl>,
#   imdb_votes <dbl>, tmdb_popularity <dbl>, tmdb_score <dbl>
```


## Pipes

What if you want to select and filter at the same time? There are three
ways to do this: use intermediate steps, nested functions, or pipes.

With intermediate steps, you create a temporary dataframe and use
that as input to the next function, like this:


```r
movieSerie2 <- filter(movieSerie, age_certification == "PG-13")
movieSerie_ch <- select(movieSerie2, title:description)
```

This is readable, but can clutter up your workspace with lots of objects that
you have to name individually. With multiple steps, that can be hard to keep
track of.

You can also nest functions (i.e. one function inside of another), like this:


```r
movieSerie_ch <- select(filter(movieSerie, age_certification == "PG-13"),
                         title:description)
```

This is handy, as R evaluates the expression from the inside out (in this case, 
filtering, then selecting), but it can be difficult to read if too many 
functions are nested, 

The last option, *pipes*, are a recent addition to R. Pipes let you take the
output of one function and send it directly to the next, which is useful when
you need to do many things to the same dataset. Pipes in R look like `%>%` and
are made available via the **`magrittr`** package, installed automatically with
**`dplyr`**. If you use RStudio, you can type the pipe with:  
- <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>M</kbd> if you have a PC or <kbd>Cmd</kbd> +
<kbd>Shift</kbd> + <kbd>M</kbd> if you have a Mac.


```r
movieSerie %>%
    filter(age_certification == "PG-13") %>%
    select(title,description)
```

```{.output}
# A tibble: 451 × 2
   title                                 description                            
   <chr>                                 <chr>                                  
 1 The Professionals                     "An arrogant Texas millionaire hires f…
 2 Awakenings                            "Dr. Malcolm Sayer, a shy research phy…
 3 National Lampoon's Christmas Vacation "It's Christmastime, and the Griswolds…
 4 Lean On Me                            "When principal Joe Clark takes over d…
 5 Mission: Impossible                   "When Ethan Hunt, the leader of a crac…
 6 Forrest Gump                          "A man with a low IQ has accomplished …
 7 Titanic                               "101-year-old Rose DeWitt Bukater tell…
 8 Mission: Impossible II                "With computer genius Luther Stickell …
 9 Dumb and Dumber                       "Lloyd and Harry are two men whose stu…
10 Gattaca                               "In a future society in the era of ind…
# ℹ 441 more rows
```

In the above code, we use the pipe to send the `movieSerie` dataset first
through `filter()` to keep rows where `age_certification` is "PG-13", then through
`select()` to keep only the `title` and `description` columns. Since `%>%`
takes the object on its left and passes it as the first argument to the function
on its right, we don't need to explicitly include the dataframe as an argument
to the `filter()` and `select()` functions any more.

Some may find it helpful to read the pipe like the word "then". For instance,
in the above example, we take the dataframe `movieSerie`, *then* we `filter`
for rows with `age_certification == "PG-13"`, *then* we `select` columns `title` and
`description`. The **`dplyr`** functions by themselves are somewhat simple,
but by combining them into linear workflows with the pipe, we can accomplish
more complex data wrangling operations.

If we want to create a new object with this smaller version of the data, we
can assign it a new name:


```r
movieSerie_ch <- movieSerie %>%
    filter(age_certification == "PG-13") %>%
    select(title,description)

movieSerie_ch
```

```{.output}
# A tibble: 451 × 2
   title                                 description                            
   <chr>                                 <chr>                                  
 1 The Professionals                     "An arrogant Texas millionaire hires f…
 2 Awakenings                            "Dr. Malcolm Sayer, a shy research phy…
 3 National Lampoon's Christmas Vacation "It's Christmastime, and the Griswolds…
 4 Lean On Me                            "When principal Joe Clark takes over d…
 5 Mission: Impossible                   "When Ethan Hunt, the leader of a crac…
 6 Forrest Gump                          "A man with a low IQ has accomplished …
 7 Titanic                               "101-year-old Rose DeWitt Bukater tell…
 8 Mission: Impossible II                "With computer genius Luther Stickell …
 9 Dumb and Dumber                       "Lloyd and Harry are two men whose stu…
10 Gattaca                               "In a future society in the era of ind…
# ℹ 441 more rows
```

Note that the final dataframe (`movieSerie_ch`) is the leftmost part of this
expression.


:::: challenge


## Exercise

Using pipes, subset the `movieSerie` data set to include movieSerie
have a `release_year` greater than 1980 and retain only the columns `title`,
 `runtime`, and `age_certification`.
 
::::

:::: solution

## Solution


```r
movieSerie %>%
     filter(release_year > 1980) %>%
     select(title, runtime, age_certification)
```

```{.output}
# A tibble: 5,815 × 3
   title                       runtime age_certification
   <chr>                         <dbl> <chr>            
 1 Seinfeld                         24 TV-PG            
 2 GoodFellas                      145 R                
 3 Full Metal Jacket               117 R                
 4 Once Upon a Time in America     139 R                
 5 When Harry Met Sally...          96 R                
 6 A Nightmare on Elm Street        91 R                
 7 Steel Magnolias                 119 PG               
 8 Police Academy                   97 R                
 9 Christine                       110 R                
10 Knight Rider                     51 TV-PG            
# ℹ 5,805 more rows
```

::::


## Mutate

Frequently you'll want to create new columns based on the values in existing
columns, for example to do unit conversions, or to find the ratio of values in
two columns. For this we'll use `mutate()`.

We might be interested in knowing the differences in scores on imdb vs tmdb:


```r
movieSerie %>%
  mutate(score_difference = imdb_score - tmdb_score) %>% 
  select(imdb_score, tmdb_score, score_difference)
```

```{.output}
# A tibble: 5,850 × 3
   imdb_score tmdb_score score_difference
        <dbl>      <dbl>            <dbl>
 1       NA        NA             NA     
 2        8.2       8.18           0.0210
 3        7.7       7.3            0.400 
 4        8.2       7.81           0.389 
 5        7.7       7.6            0.100 
 6        8.8       8.31           0.494 
 7        8         7.8            0.200 
 8        7.7       7.5            0.200 
 9        7.7       7.5            0.200 
10        5.8       6.16          -0.356 
# ℹ 5,840 more rows
```


:::: challenge 

## Exercise

Create a new dataframe from the `movieSerie` data set that meets the following
criteria: contains only the `title` column and a new column called
`total_score` containing a value that is equal to the total number of scores on 
both imdb and tmdb (`imdb_score` plus `tmdb_score`).
Only the rows where `total_score` is greater than 15 should be shown in the
final dataframe.

**Hint**: think about how the commands should be ordered to produce the data
frame!

:::: solution

## Solution


```r
movieSerie_total_score <- movieSerie %>%
  mutate(total_score = imdb_score + tmdb_score) %>%
  filter(total_score > 15) %>%
  select(title, total_score)
```

::::::::
::::

## Split-apply-combine data analysis and the summarize() function

Many data analysis tasks can be approached using the *split-apply-combine*
paradigm: split the data into groups, apply some analysis to each group, and
then combine the results. **`dplyr`** makes this very easy through the use of
the `group_by()` function.


#### The `summarize()` function

`group_by()` is often used together with `summarize()`, which collapses each
group into a single-row summary of that group. `group_by()` takes as arguments
the column names that contain the **categorical** variables for which you want
to calculate the summary statistics. So to compute the average imdb_score by
genre:


```r
movieSerie %>%
    group_by(genre) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE))
```

```{.output}
# A tibble: 19 × 2
   genre         mean_imdb_score
   <chr>                   <dbl>
 1 action                   6.28
 2 animation                6.55
 3 comedy                   6.33
 4 crime                    6.73
 5 documentation            7.07
 6 drama                    6.73
 7 family                   6.21
 8 fantasy                  6.23
 9 history                  6.69
10 horror                   5.45
11 music                    6.62
12 reality                  6.32
13 romance                  6.07
14 scifi                    6.69
15 sport                    6.68
16 thriller                 6.12
17 war                      6.98
18 western                  6.23
19 <NA>                     7.25
```

You may also have noticed that the output from these calls doesn't run off the
screen anymore. It's one of the advantages of `tbl_df` over dataframe.

You can also group by multiple columns:


```r
movieSerie %>%
    group_by(genre, type) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE))
```

```{.output}
`summarise()` has grouped output by 'genre'. You can override using the
`.groups` argument.
```

```{.output}
# A tibble: 38 × 3
# Groups:   genre [19]
   genre         type  mean_imdb_score
   <chr>         <chr>           <dbl>
 1 action        MOVIE            5.94
 2 action        SHOW             6.87
 3 animation     MOVIE            6.32
 4 animation     SHOW             6.68
 5 comedy        MOVIE            6.09
 6 comedy        SHOW             6.98
 7 crime         MOVIE            6.39
 8 crime         SHOW             7.09
 9 documentation MOVIE            6.99
10 documentation SHOW             7.18
# ℹ 28 more rows
```

Note that the output is a grouped tibble. To obtain an ungrouped tibble, use the
`ungroup` function:


```r
movieSerie %>%
    group_by(genre, type) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
    ungroup()
```

```{.output}
`summarise()` has grouped output by 'genre'. You can override using the
`.groups` argument.
```

```{.output}
# A tibble: 38 × 3
   genre         type  mean_imdb_score
   <chr>         <chr>           <dbl>
 1 action        MOVIE            5.94
 2 action        SHOW             6.87
 3 animation     MOVIE            6.32
 4 animation     SHOW             6.68
 5 comedy        MOVIE            6.09
 6 comedy        SHOW             6.98
 7 crime         MOVIE            6.39
 8 crime         SHOW             7.09
 9 documentation MOVIE            6.99
10 documentation SHOW             7.18
# ℹ 28 more rows
```



Once the data are grouped, you can also summarize multiple variables at the same
time (and not necessarily on the same variable). For instance, we could add a
column indicating the maximum imdb_score given to a movie or serie:


```r
movieSerie %>%
    group_by(genre, type) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
              max_imdb_score = max(imdb_score, na.rm = TRUE))
```

```{.output}
`summarise()` has grouped output by 'genre'. You can override using the
`.groups` argument.
```

```{.output}
# A tibble: 38 × 4
# Groups:   genre [19]
   genre         type  mean_imdb_score max_imdb_score
   <chr>         <chr>           <dbl>          <dbl>
 1 action        MOVIE            5.94            9.1
 2 action        SHOW             6.87            9  
 3 animation     MOVIE            6.32            9.1
 4 animation     SHOW             6.68            9  
 5 comedy        MOVIE            6.09            8.7
 6 comedy        SHOW             6.98            9.2
 7 crime         MOVIE            6.39            8.6
 8 crime         SHOW             7.09            8.8
 9 documentation MOVIE            6.99            8.9
10 documentation SHOW             7.18            9.3
# ℹ 28 more rows
```

It is sometimes useful to rearrange the result of a query to inspect the values.
For instance, we can sort on `min_imdb_score` to put the group with the smallest
imdb_score first:



```r
movieSerie %>%
    group_by(genre, type) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
              max_imdb_score = max(imdb_score, na.rm = TRUE)) %>%
    arrange(max_imdb_score)
```

```{.output}
`summarise()` has grouped output by 'genre'. You can override using the
`.groups` argument.
```

```{.output}
# A tibble: 38 × 4
# Groups:   genre [19]
   genre   type  mean_imdb_score max_imdb_score
   <chr>   <chr>           <dbl>          <dbl>
 1 music   SHOW             6.3             6.9
 2 horror  SHOW             6.19            7  
 3 sport   MOVIE            6.27            7.4
 4 history MOVIE            6.43            7.5
 5 reality MOVIE            7.15            7.5
 6 western SHOW             7.45            7.6
 7 family  MOVIE            5.81            7.7
 8 fantasy SHOW             6.87            7.7
 9 <NA>    MOVIE            7.1             7.7
10 sport   SHOW             7.9             7.9
# ℹ 28 more rows
```

To sort in descending order, we need to add the `desc()` function. If we want to
sort the results by decreasing order of minimum imdb_score:


```r
movieSerie %>%
    group_by(genre, type) %>%
    summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
              max_imdb_score = max(imdb_score, na.rm = TRUE)) %>%
    arrange(desc(max_imdb_score))
```

```{.output}
`summarise()` has grouped output by 'genre'. You can override using the
`.groups` argument.
```

```{.output}
# A tibble: 38 × 4
# Groups:   genre [19]
   genre         type  mean_imdb_score max_imdb_score
   <chr>         <chr>           <dbl>          <dbl>
 1 <NA>          SHOW             7.32            9.6
 2 drama         SHOW             7.19            9.5
 3 reality       SHOW             6.31            9.5
 4 documentation SHOW             7.18            9.3
 5 scifi         SHOW             7.08            9.3
 6 comedy        SHOW             6.98            9.2
 7 action        MOVIE            5.94            9.1
 8 animation     MOVIE            6.32            9.1
 9 action        SHOW             6.87            9  
10 animation     SHOW             6.68            9  
# ℹ 28 more rows
```

#### Counting

When working with data, we often want to know the number of observations found
for each factor or combination of factors. For this task, **`dplyr`** provides
`count()`. For example, if we wanted to count the number of rows of data for
each village, we would do:


```r
movieSerie %>%
    count(release_year)
```

```{.output}
# A tibble: 63 × 2
   release_year     n
          <dbl> <int>
 1         1945     1
 2         1954     2
 3         1956     1
 4         1958     1
 5         1959     1
 6         1960     1
 7         1961     1
 8         1963     1
 9         1966     1
10         1967     2
# ℹ 53 more rows
```

For convenience, `count()` provides the `sort` argument to get results in
decreasing order:


```r
movieSerie %>%
    count(release_year, sort = TRUE)
```

```{.output}
# A tibble: 63 × 2
   release_year     n
          <dbl> <int>
 1         2019   836
 2         2020   814
 3         2021   787
 4         2018   773
 5         2017   563
 6         2022   371
 7         2016   362
 8         2015   223
 9         2014   153
10         2013   135
# ℹ 53 more rows
```


::::::::::::::::::::::::::::::::::::: challenge

## Exercise

1. How many moview and series are there for each age_certification?

2. Use `group_by()` and `summarize()` to find the mean, min, and max for 
tmdb_score. Also add the number of observations (hint: see `?n`).

:::::::::::::::::::::::: solution

## Solution 1


```r
 movieSerie %>%
  count(age_certification)
```

```{.output}
# A tibble: 12 × 2
   age_certification     n
   <chr>             <int>
 1 ""                 2619
 2 "G"                 124
 3 "NC-17"              16
 4 "PG"                233
 5 "PG-13"             451
 6 "R"                 556
 7 "TV-14"             474
 8 "TV-G"               79
 9 "TV-MA"             883
10 "TV-PG"             188
11 "TV-Y"              107
12 "TV-Y7"             120
```

:::::::::::::::::::::::::::::::::

:::::::::::::::::::::::: solution

## Solution 2


```r
movieSerie %>%
  group_by(genre) %>%
   summarize(
       mean_tmdb_score = mean(tmdb_score, na.rm = TRUE),
       min_tmdb_score = min(tmdb_score, na.rm = TRUE),
       max_tmdb_score = max(tmdb_score, na.rm = TRUE),
       n = n()
   )
```

```{.output}
# A tibble: 19 × 5
   genre         mean_tmdb_score min_tmdb_score max_tmdb_score     n
   <chr>                   <dbl>          <dbl>          <dbl> <int>
 1 action                   6.75            2            10      365
 2 animation                7.31            2            10      317
 3 comedy                   6.59            1            10     1305
 4 crime                    6.92            3.8          10      238
 5 documentation            7.11            1            10      665
 6 drama                    6.96            1            10     1421
 7 family                   7.14            1            10      113
 8 fantasy                  6.77            2            10       88
 9 history                  6.94            5.8          10       21
10 horror                   5.83            2.8           8.5    114
11 music                    7.09            5.3           8.8     59
12 reality                  7.19            1            10      171
13 romance                  6.42            2             9.3    232
14 scifi                    7.17            0.5           9.5    239
15 sport                    6.8             5             8.3      4
16 thriller                 6.39            3.7          10      377
17 war                      6.93            4.8           8.8     46
18 western                  6.30            3.6           8.15    16
19 <NA>                     7.06            1            10       59
```

:::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::

## Exporting data

Now that you have learned how to use **`dplyr`** to extract information from
or summarize your raw data, you may want to export these new data sets to share
them with your collaborators or for archival.

Similar to the `read_csv()` function used for reading CSV files into R, there is
a `write_csv()` function that generates CSV files from dataframes.

Before using `write_csv()`, we are going to create a new folder, `data_output`,
in our working directory that will store this generated dataset. We don't want
to write generated datasets in the same directory as our raw data. It's good
practice to keep them separate. The `data` folder should only contain the raw,
unaltered data, and should be left alone to make sure we don't delete or modify
it. In contrast, our script will generate the contents of the `data_output`
directory, so even if the files it contains are deleted, we can always
re-generate them.

Now we can save this dataframe to our `data_output` directory.


```r
write_csv(movieSerie_ch, file = "data_output/movieSerie_changed.csv")
```



:::: keypoints

- Use the `dplyr` package to manipulate dataframes.
- Use `select()` to choose variables from a dataframe.
- Use `filter()` to choose data based on values.
- Use `group_by()` and `summarize()` to work with subsets of data.
- Use `mutate()` to create new variables.
- Use the `tidyr` package to change the layout of dataframes.
- Use `pivot_wider()` to go from long to wide format.
- Use `pivot_longer()` to go from wide to long format.

::::
