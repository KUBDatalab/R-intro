---
title: "Introduction to R"
teaching: 30
exercises: 10
---

:::: questions

- What data types are available in R?
- What is an object?
- How can values be initially assigned to variables of different data types?
- What arithmetic and logical operators can be used?
- How can subsets be extracted from vectors?
- How does R treat missing values?
- How can we deal with missing values in R?

::::::

::::::::::::::::::::::::::::::::::::: objectives

- Define the following terms as they relate to R: object, assign, call, function, arguments, options.
- Assign values to objects in R.
- Learn how to name objects.
- Use comments to inform script.
- Solve simple arithmetic operations in R.
- Call functions and use arguments to change their default options.
- Inspect the content of vectors and manipulate their content.
- Subset and extract values from vectors.
- Analyze vectors with missing data.

::::::::::::::::::::::::::::::::::::::::::::::::

## Creating objects in R

You can get output from R simply by typing math in the console:


```r
3 + 5
```

```{.output}
[1] 8
```

```r
12 / 7
```

```{.output}
[1] 1.714286
```

However, to do useful and interesting things, we need to assign _values_ to
_objects_. To create an object, we need to give it a name followed by the
assignment operator `<-`, and the value we want to give it:


```r
x <- 3
```

`<-` is the assignment operator. It assigns values on the right to objects on
the left. So, after executing `x <- 3`, the value of `x` is `3`. The arrow can
be read as 3 **goes into** `x`.

In RStudio, typing <kbd>Alt</kbd> + <kbd>-</kbd> (push <kbd>Alt</kbd> at the
same time as the <kbd>-</kbd> key) will write `<- ` in a single keystroke in a
PC, while typing <kbd>Option</kbd> + <kbd>-</kbd> (push <kbd>Option</kbd> at the
same time as the <kbd>-</kbd> key) does the same in a Mac.

:::: callout

## Historical

For historical reasons, you can also use `=`
for assignments, but not in every context. Because of the
[slight](https://web.archive.org/web/20210121015950/https://blog.revolutionanalytics.com/2008/12/use-equals-or-arrow-for-assignment.html)
[differences](https://web.archive.org/web/20210810114254/https://stackoverflow.com/questions/1741820/what-are-the-differences-between-and-assignment-operators-in-r)
in syntax, it is good practice to always use `<-` for assignments. More 
generally we prefer the `<-` syntax over `=` because it makes it clear what
direction the assignment is operating (left assignment), and it increases the
read-ability of the code.

::::::

### Naming objects

Objects can be given any name such as `x`, `current_temperature`, or
`subject_id`. You want your object names to be explicit and not too long. 

They cannot start with a number (`2x` is not valid, but `x2` is), and R is case 
sensitive (e.g., `age` is different from `Age`). There are some names that
cannot be used because they are the names of fundamental objects in R (e.g.,
`if`, `else`, `for`, see
[here for a complete list](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html)). 
In general, even if it's allowed, it's best to not use 
them (e.g., `c`, `T`, `mean`, `data`, `df`, `weights`). If in doubt, check 
the help to see if the name is already in use.

It is best to
avoid dots (`.`) within an object name as in `my.dataset`. There are many
objects in R with dots in their names for historical reasons, but because dots
have a special meaning in R (for methods) and other programming languages, it's
best to avoid them.

It is also recommended to use nouns for object names, and
verbs for function names. It's important to be consistent in the styling of your
code (where you put spaces, how you name objects, etc.). Using a consistent
coding style makes your code clearer to read for your future self and your
collaborators.

In R, three popular style guides are
[Google's](https://google.github.io/styleguide/Rguide.xml), [Jean
Fan's](https://jef.works/R-style-guide/) and the
[tidyverse's](https://style.tidyverse.org/). The tidyverse's is very
comprehensive and may seem overwhelming at first. You can install the
[**`lintr`**](https://github.com/jimhester/lintr) package to automatically check
for issues in the styling of your code.

:::: callout

## Objects vs. variables

What are known as `objects` in `R` are known as `variables` in many other
programming languages. Depending on the context, `object` and `variable` can
have drastically different meanings. However, in this lesson, the two words
are used synonymously. For more information see:
[https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Objects](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Objects)

::::::

### Printing objects

When assigning a value to an object, R does not print anything. You
can force R to print the value by using parentheses or by typing
the object name:


```r
area_hectares <- 1.0    # doesn't print anything
area_hectares         # but typing the name of the object print
```

```{.output}
[1] 1
```

```r
(area_hectares <- 1.0)  # putting parenthesis around the call also print
```

```{.output}
[1] 1
```

### Doing math on objects

Now that R has `area_hectares` in memory, we can do arithmetic with it. For instance, we may want to convert this area into acres (area in acres is 2.47 times the area in hectares): 


```r
2.47 * area_hectares
```

```{.output}
[1] 2.47
```

We can also change an object’s value by assigning it a new one:


```r
area_hectares <- 2.5
2.47 * area_hectares
```

```{.output}
[1] 6.175
```

Assigning a value to one object does not automatically change the values of other objects. For example, let’s store the plot’s area in acres in a new object, `area_acres`:


```r
area_acres <- 2.47 * area_hectares
```

and then change `area_hectares` to 50.


```r
area_hectares <- 50
```

::::::::::::::::::::::::::::::::::::: challenge
## Exercise
What do you think is the current content of the object `area_acres`? 123.5 or 6.175?

:::::::::::::::::::::::: solution

## Solution

The value of `area_acres` is still 6.175 because you have not
re-run the line `area_acres <- 2.47 * area_hectares` since
changing the value of `area_hectares`.

:::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: challenge

## Exercise

Create two variables `r_length` and `r_width` and assign them values. It should be noted that, 
because `length` is a built-in R function, R Studio might add "()" after you type `length` and 
if you leave the parentheses you will get unexpected results. 
This is why you might see other programmers abbreviate common words.
Create a third variable `r_area` and give it a value based on the current values of `r_length` 
and `r_width`.
Show that changing the values of either `r_length` and `r_width` does not affect the value of 
`r_area`.

:::::::::::::::::::::::: solution

## Solution


```r
r_length <- 2.5
r_width <- 3.2
r_area <- r_length * r_width
r_area
```

```{.output}
[1] 8
```

```r
# change the values of r_length and r_width
r_length <- 7.0
r_width <- 6.5
# the value of r_area isn't changed
r_area
```

```{.output}
[1] 8
```

:::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::

## Comments

All programming languages allow the programmer to include comments in their code. To do this in R we use the `#` character.
Anything to the right of the `#` sign and up to the end of the line is treated as a comment and is ignored by R. You can start lines with comments
or include them after any code on the line.


```r
# land area in hectares
area_hectares <- 1.0			
area_acres <- area_hectares * 2.47	# convert to acres
area_acres				# print land area in acres.
```

```{.output}
[1] 2.47
```

RStudio makes it easy to comment or uncomment a paragraph: after selecting the
lines you  want to comment, press at the same time on your keyboard
<kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>C</kbd>. If you only want to comment
out one line, you can put the cursor at any location of that line (i.e. no need
to select the whole line), then press <kbd>Ctrl</kbd> + <kbd>Shift</kbd> +
<kbd>C</kbd>.


## Functions and their arguments

Functions are "canned scripts" that automate more complicated sets of commands
including operations assignments, etc. Many functions are predefined, or can be
made available by importing R *packages* (more on that later). A function
usually gets one or more inputs called *arguments*. Functions often (but not
always) return a *value*. A typical example would be the function `sqrt()`. The
input (the argument) must be a number, and the return value (in fact, the
output) is the square root of that number. Executing a function ('running it')
is called *calling* the function. An example of a function call is:


```r
b <- sqrt(area_acres)
```
Here, the value of `a` is given to the `sqrt()` function, the `sqrt()` function
calculates the square root, and returns the value which is then assigned to
the object `b`. This function is very simple, because it takes just one argument.

The return 'value' of a function need not be numerical (like that of `sqrt()`),
and it also does not need to be a single item: it can be a set of things, or
even a dataset. We'll see that when we read data files into R.

Arguments can be anything, not only numbers or filenames, but also other
objects. Exactly what each argument means differs per function, and must be
looked up in the documentation (see below). Some functions take arguments which
may either be specified by the user, or, if left out, take on a *default* value:
these are called *options*. Options are typically used to alter the way the
function operates, such as whether it ignores 'bad values', or what symbol to
use in a plot.  However, if you want something specific, you can specify a value
of your choice which will be used instead of the default.

Let's try a function that can take multiple arguments: `round()`.


```r
round(3.14159)
```

```{.output}
[1] 3
```


Here, we've called `round()` with just one argument, `3.14159`, and it has
returned the value `3`.  That's because the default is to round to the nearest
whole number. If we want more digits we can see how to do that by getting
information about the `round` function.  We can use `args(round)` or look at the
help for this function using `?round`.


```r
args(round)
```

```{.output}
function (x, digits = 0) 
NULL
```


```r
?round
```

We see that if we want a different number of digits, we can
type `digits=2` or however many we want.


```r
round(3.14159, digits = 2)
```

```{.output}
[1] 3.14
```

If you provide the arguments in the exact same order as they are defined you
don't have to name them:


```r
round(3.14159, 2)
```

```{.output}
[1] 3.14
```

And if you do name the arguments, you can switch their order:


```r
round(digits = 2, x = 3.14159)
```

```{.output}
[1] 3.14
```

It's good practice to put the non-optional arguments (like the number you're
rounding) first in your function call, and to specify the names of all optional
arguments.  If you don't, someone reading your code might have to look up the
definition of a function with unfamiliar arguments to understand what you're
doing.

::::::::::::::::::::::::::::::::::::: challenge

## Exercise

Type in `?round` at the console and then look at the output in the Help pane.
What other functions exist that are similar to `round`?
How do you use the `digits` parameter in the round function?

::::::::::::::::::::::::::::::::::::::::::::::::

## Vectors and data types

A vector is the most common and basic data type in R, and is pretty much
the workhorse of R. A vector is composed by a series of values. We can assign a 
series of values to a vector using the `c()` function. For example we can 
create a vector containing the score of movies on imdb and assign it to an 
object `imdb_score`:


```r
imdb_score <- c(62, 21, 77, 80)
imdb_score
```

```{.output}
[1] 62 21 77 80
```

The vector `imdb_score` contains numbers, but a vector can also contain characters. For example, we can have
a vector of movie titles corresponding to the scores (`title`):


```r
title <- c("FTA", "Dostana", "Deliverance", "Life of Brian")
title
```

```{.output}
[1] "FTA"           "Dostana"       "Deliverance"   "Life of Brian"
```

The quotes around "FTA", etc. are essential. Without the quotes R
will assume there are objects called `FTA` etc. As these objects
don't exist in R's memory, there will be an error message.

An important feature of a vector, is that all of the elements are the same type of data.

:::: callout

### Vectors and datatypes
An **atomic vector** is the simplest R **data type** and is a linear vector of 
a single type. Above, we saw 2 of the 6 main **atomic vector** types  that R
uses: `"character"` and `"numeric"` (or `"double"`). These are the basic 
building blocks that all R objects are built from. The other 4 **atomic vector** 
types are:

* `"logical"` for `TRUE` and `FALSE` (the boolean data type)
* `"integer"` for integer numbers (e.g., `2L`, the `L` indicates to R that it's an integer)
* `"complex"` to represent complex numbers with real and imaginary parts (e.g.,
`1 + 4i`) and that's all we're going to say about them
* `"raw"` for bitstreams that we won't discuss further

Vectors are one of the many **data structures** that R uses. Other important
ones are lists (`list`), matrices (`matrix`), data frames (`data.frame`),
factors (`factor`) and arrays (`array`).

::::::

The function `class()` indicates the class (the type of element) of an object:


```r
class(imdb_score)
```

```{.output}
[1] "numeric"
```

```r
class(title)
```

```{.output}
[1] "character"
```

The function `str()` provides an overview of the structure of an object and its
elements. It is a useful function when working with large and complex
objects:


```r
str(imdb_score)
```

```{.output}
 num [1:4] 62 21 77 80
```

```r
str(title)
```

```{.output}
 chr [1:4] "FTA" "Dostana" "Deliverance" "Life of Brian"
```

As you can see there are many functions that allow you to inspect the content 
of a vector. Another example is `length()` that tells you how many elements are 
in a particular vector:


```r
length(imdb_score)
```

```{.output}
[1] 4
```

You can use the `c()` function to add other elements to your vector:

```r
production_country <- c("IN", "US")
production_country
```

```{.output}
[1] "IN" "US"
```

```r
production_country <- c(production_country, "GB") # add to the end of the vector
production_country <- c("US", production_country) # add to the beginning of the vector
production_country
```

```{.output}
[1] "US" "IN" "US" "GB"
```

First the vector `production_country` is created with two values. Then the value
"GB" is added to the end of the vector, and the result is saved back into 
`production_country`. After that the value "US" is added to the front of the 
vector, and again saved back into `production_country`

We can do this over and over again to grow a vector, or assemble a dataset.
As we program, this may be useful to add results that we are collecting or
calculating.



::::::::::::::::::::::::::::::::::::: challenge

## Exercise

We’ve seen that atomic vectors can be of type character, numeric (or double),
integer, and logical. But what happens if we try to mix these types in a
single vector?

:::::::::::::::::::::::: solution 

## Solution

R implicitly converts them to all be the same type.

:::::::::::::::::::::::::::::::::


What will happen in each of these examples? (hint: use `class()`
to check the data type of your objects):

:::::::::::::::::::::::: solution


```r
num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")
```

:::::::::::::::::::::::::::::::::

Why do you think it happens?

:::::::::::::::::::::::: solution

## Solution

Vectors can be of only one data type. R tries to
convert (coerce) the content of this vector to find a "common
denominator" that doesn't lose any information.

:::::::::::::::::::::::::::::::::

How many values in `combined_logical` are `"TRUE"` (as a character) in the
following example:



```r
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
combined_logical <- c(num_logical, char_logical)
```

:::::::::::::::::::::::: solution

## Solution

Only one. There is no memory of past data types, and the coercion
happens the
first time the vector is evaluated. Therefore, the `TRUE` in
`num_logical`
gets converted into a `1` before it gets converted into `"1"` in
`combined_logical`.

:::::::::::::::::::::::::::::::::


You've probably noticed that objects of different types get
converted into a single, shared type within a vector. In R, we
call converting objects from one class into another class
_coercion_. These conversions happen according to a hierarchy,
whereby some types get preferentially coerced into other
types. Can you draw a diagram that represents the hierarchy of how
these data types are coerced?

::::::::::::::::::::::::::::::::::::::::::::::::


## Subsetting vectors

If we want to extract one or several values from a vector, we must provide one
or several indices in square brackets. For instance:


```r
title[2]
```

```{.output}
[1] "Dostana"
```

```r
title[c(3, 2)]
```

```{.output}
[1] "Deliverance" "Dostana"    
```

First R return the second element from the title vector, and after that R 
returns the third and second element from the title vector.

R indices start at 1. Programming languages like Fortran, MATLAB, Julia, and R
start counting at 1, because that's what human beings typically do. Languages in
the C family (including C++, Java, Perl, and Python) count from 0 because that's
simpler for computers to do.


We can also repeat the indices to create an object with more elements than the
original one:


```r
more_title <- title[c(1, 2, 3, 2, 1, 3)]
more_title
```

```{.output}
[1] "FTA"         "Dostana"     "Deliverance" "Dostana"     "FTA"        
[6] "Deliverance"
```

Here we create an new vector `more_title` containing the elements from title in 
the order given in the `c` function

### Conditional subsetting

Sometimes we only want a subset of a vector, but we do not know the exact index 
number. A common way of getting a subset without knowing the index numbers, 
is by using a logical vector. `TRUE` will select the element with the same 
index, while `FALSE` will not:


```r
imdb_score[c(TRUE, FALSE, TRUE, TRUE)]
```

```{.output}
[1] 62 77 80
```

Typically, these logical vectors are not typed by hand, but are the output of
other functions or logical tests. For instance, if you wanted to select only the
imdb_scores above 70:


```r
imdb_score > 70    # will return logicals with TRUE for the indices that meet the condition
```

```{.output}
[1] FALSE FALSE  TRUE  TRUE
```

```r
## so we can use this to select only the values above 5
imdb_score[imdb_score > 70]
```

```{.output}
[1] 77 80
```

You can combine multiple tests using `&` (both conditions are true (AND)) or `|`
(at least one of the conditions is true, (OR)):


```r
imdb_score[imdb_score < 62 | imdb_score > 77]
```

```{.output}
[1] 21 80
```

```r
imdb_score[imdb_score >= 62 & imdb_score <= 77]
```

```{.output}
[1] 62 77
```


:::::callout

Here, `<` stands for "less than", `>` for "greater than", `>=` for "greater than
or equal to", and `==` for "equal to". The double equal sign `==` is a test for
numerical equality between the left and right hand sides, and should not be
confused with the single `=` sign, which performs variable assignment (similar
to `<-`).

:::::



A common task is to search for certain strings in a vector.  One could use the
"or" operator `|` to test for equality to multiple values, but this can quickly
become tedious. 


```r
title[title == "FTA" | title == "Dostana"] # returns both "FTA and Dostana"
```

```{.output}
[1] "FTA"     "Dostana"
```

The function `%in%` allows you to test if any of the elements of a search vector
(on the right hand side) are found in the target vector (on the left hand side):


```r
title %in% c("FTA", "Dostana")
```

```{.output}
[1]  TRUE  TRUE FALSE FALSE
```

Note that the output is the same length as the search vector on the left hand
side, because `%in%` checks whether each element of the search vector is found
somewhere in the target vector. Thus, you can use `%in%` to select the elements
in the search vector that appear in your target vector:


```r
title[title %in% c("FTA", "Dostana")]
```

```{.output}
[1] "FTA"     "Dostana"
```

You can also use a vector with `TRUE` and `FALSE` based on one vector to find 
informations in another vectore. For instance if we want the imdb scores for the 
movies "FTA" and "Dostana" we can do it like this.


```r
imdb_score[title %in% c("FTA", "Dostana")]
```

```{.output}
[1] 62 21
```


## Missing data

As R was designed to analyze datasets, it includes the concept of missing data
(which is uncommon in other programming languages). Missing data are represented
in vectors as `NA`.

When doing operations on numbers, most functions will return `NA` if the data
you are working with include missing values. This feature
makes it harder to overlook the cases where you are dealing with missing data.
You can add the argument `na.rm=TRUE` to calculate the result while ignoring
the missing values.


```r
#Makes an new vector with NA values
imdb_score_na <- c(imdb_score, NA, 54, NA)
#tries to calculate mean - without ignoring NA values
mean(imdb_score_na)
```

```{.output}
[1] NA
```

```r
#tries to find the maximum value - without ignoring NA values
max(imdb_score_na)
```

```{.output}
[1] NA
```

```r
#tries to calculate mean - telling R to ignore NA values
mean(imdb_score_na, na.rm = TRUE)
```

```{.output}
[1] 58.8
```

```r
#tries to find the maximum value - telling R to ignore NA values
max(imdb_score_na, na.rm = TRUE)
```

```{.output}
[1] 80
```

If your data include missing values, you may want to become familiar with the
functions `is.na()`, `na.omit()`, and `complete.cases()`. See below for
examples.



```r
## Extract those elements which are not missing values.
imdb_score_na[!is.na(imdb_score_na)]
```

```{.output}
[1] 62 21 77 80 54
```

```r
## Count the number of missing values.
sum(is.na(imdb_score_na))
```

```{.output}
[1] 2
```

```r
## Returns the object with incomplete cases removed. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
na.omit(imdb_score_na)
```

```{.output}
[1] 62 21 77 80 54
attr(,"na.action")
[1] 5 7
attr(,"class")
[1] "omit"
```

```r
## Extract those elements which are complete cases. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
imdb_score_na[complete.cases(imdb_score_na)]
```

```{.output}
[1] 62 21 77 80 54
```
Recall that you can use the `typeof()` function to find the type of your atomic vector.

::::::::::::::::::::::::::::::::::::: challenge 

## Exercise

1. Using this vector of rooms, create a new vector with the NAs removed.
```r
rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
```

2. Use the function `median()` to calculate the median of the `rooms` vector.

3. Use R to figure out how many households in the set use more than 2 rooms for sleeping.

:::::::::::::::::::::::: solution

## Solution

```r
rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
rooms_no_na <- rooms[!is.na(rooms)]
# or
rooms_no_na <- na.omit(rooms)
# 2.
median(rooms, na.rm = TRUE)
```

```{.output}
[1] 1
```

```r
# 3.
rooms_above_2 <- rooms_no_na[rooms_no_na > 2]
length(rooms_above_2)
```

```{.output}
[1] 4
```

:::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


## Data frames
Normaly we do not work with data in seperate vectors. Instead it would be in 
a spreadsheet like format. In R this is called data frame

A dataframe is made up by columns of vectors, so we can combine the three vectors 
to a dataframe.



```r
df <- data.frame(title, imdb_score, production_country)
df
```

```{.output}
          title imdb_score production_country
1           FTA         62                 US
2       Dostana         21                 IN
3   Deliverance         77                 US
4 Life of Brian         80                 GB
```

So it is possible to make an data frame directly in R, but most of the time 
data are collected outside R, and afterwards imported to R.

Now that we have learned about  writing code, and the basics of R's data
structures, we are ready to start working a real dataset, and learn more about 
data frames.






::::::::::::::::::::::::::::::::::::: keypoints

- Access individual values by location using `[]`.
- Access arbitrary sets of data using `[c(...)]`.
- Use logical operations and logical vectors to access subsets of data.

::::::::::::::::::::::::::::::::::::::::::::::::
