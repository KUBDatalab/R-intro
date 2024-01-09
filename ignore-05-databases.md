---
title: "Using a relational database with R"
teaching: 0
exercises: 0
questions:
- "How can I import data held in an SQLite database into an R data frame?"
- "How can I write data from a data frame to an SQLite table?"
- "How can I create an SQLite database from csv files"

objectives:
- "Install RSQLite package"
- "Create a connection to an SQLite database"
- "Query the database"
- "Create a new databaseand populate it"
- "Use dplyr functions to access and query an SQLite database"

keypoints:
- "First key point."
---





## Introduction

A common problem with R in that all operations are conducted in-memory. Here, "memory" means a computer 
drive called Random Access Memory (RAM for short) which is phsically installed on your computer. RAM 
allows for data to be read in nearly the same amount of time regardless of its physical location
inside the memory. This is in stark contrast to the speed data could be read from CDs, DVDs, or other
storage media, where the speed of transfer depended on how quickly the drive could rotate or the 
arm could move. Unfortunately, your computer has a limited amount of RAM, so the amount of data you
can work with is limited by the available memory. So far, we have used small datasets that can
easily fit into your computer's memory. But what about datasets that are too large for your
computer to handle as a whole?

In this case, it is helpful to organze the data into a database stored outside of R before creating
a connection to the database itself. This connection will essentially remove the limitation of memory
because SQL queries can be sent directly from R to the database and return to R only the results that you
have identified as being neccessary for your analysis.

Once we have made the connection to the database, much of what we do will look familiar because the code we will be using is very similar to what we saw in the SQL lesson and earlier episodes of this R lesson.

In this lesson, we will be connecting to an SQLite database, which allows us to send strings containing SQL statements directly from R to the database and recieve the results. In addition, we will be connecting to the database in such a way that we can use 'dplyr' functions to operate directly on the database tables.


## Prelminaries

First, install and load the neccessary packages. You can install the `RSQLite` package with



```r
install.packages("RSQLite")
```

Load the packages with


```r
library(RSQLite)
library(dplyr)
```

```{.output}

Attaching package: 'dplyr'
```

```{.output}
The following objects are masked from 'package:stats':

    filter, lag
```

```{.output}
The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
```

Next, create a variable that contains the location of the SQLite database we are going to use. Here, we are assuming that it is in the current working directory.


```r
dbfile <- "data/SN7577.sqlite"
```

## Connecting to an SQLite Database

Connect to the SQLite database specified by `dbfile`, above, using the `dbConnect` function.


```r
mydb <- dbConnect(dbDriver("SQLite"), dbfile)
```

Here, `mydb` represents the connection to the database. It will be specified every time we need to access the database.

Now that we have a connection, we can get a list of the tables in the database.


```r
dbListTables(mydb)
```

```{.output}
character(0)
```

Our objective here is to bring data from the database into R by sending a query to the database and then asking for the results of that query.


```r
# Assign the results of a SQL query to an SQLiteResult object
results <- dbSendQuery(mydb, "SELECT * FROM Question1")
```

```{.error}
Error: no such table: Question1
```

```r
# Return results from a custom object to a dataframe
data <- fetch(results)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'res' in selecting a method for function 'fetch': object 'results' not found
```

`data` is a standard R dataframe that can be explored and manipulated.


```r
# Return column names
names(data)
```

```{.output}
NULL
```

```r
# Return description of dataframe structure
str(data)
```

```{.output}
function (..., list = character(), package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    envir = .GlobalEnv, overwrite = TRUE)  
```

```r
# Return the second column
data[,2]
```

```{.error}
Error in data[, 2]: object of type 'closure' is not subsettable
```

```r
# Return the value of the second column, fourth row
data[4,2]
```

```{.error}
Error in data[4, 2]: object of type 'closure' is not subsettable
```

```r
# Return the second column where the value of the column 'key' is greater than 7
data[data$key > 7,2]
```

```{.error}
Error in data$key: object of type 'closure' is not subsettable
```

Once you have retrieved the data you should close the connection.


```r
dbClearResult(results)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'res' in selecting a method for function 'dbClearResult': object 'results' not found
```

In addition to sending simple queries we can send complex one like a join.
You may want to set this up in a concateneted string first for readability.


```r
SQL_query <- paste("SELECT q.value,",
                   "count(*) as how_many",
                   "FROM SN7577 s",
                   "JOIN Question1  q",
                   "ON q.key = s.Q1",
                   "GROUP BY  s.Q1")

results <- dbSendQuery(mydb, SQL_query)
```

```{.error}
Error: no such table: SN7577
```

```r
data <- fetch(results)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'res' in selecting a method for function 'fetch': object 'results' not found
```

```r
data
```

```{.output}
function (..., list = character(), package = NULL, lib.loc = NULL, 
    verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
{
    fileExt <- function(x) {
        db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\.", "", x)
        ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
            x[db])
        ans
    }
    my_read_table <- function(...) {
        lcc <- Sys.getlocale("LC_COLLATE")
        on.exit(Sys.setlocale("LC_COLLATE", lcc))
        Sys.setlocale("LC_COLLATE", "C")
        read.table(...)
    }
    stopifnot(is.character(list))
    names <- c(as.character(substitute(list(...))[-1L]), list)
    if (!is.null(package)) {
        if (!is.character(package)) 
            stop("'package' must be a character vector or NULL")
    }
    paths <- find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc)) 
        paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
            paths)
    paths <- unique(normalizePath(paths[file.exists(paths)]))
    paths <- paths[dir.exists(file.path(paths, "data"))]
    dataExts <- tools:::.make_file_exts("data")
    if (length(names) == 0L) {
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            packageName <- if (file_test("-f", file.path(path, 
                "DESCRIPTION"))) 
                basename(path)
            else "."
            if (file_test("-f", INDEX <- file.path(path, "Meta", 
                "data.rds"))) {
                entries <- readRDS(INDEX)
            }
            else {
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, 
                  "data")
                if (length(entries)) {
                  entries <- unique(tools::file_path_sans_ext(basename(entries)))
                  entries <- cbind(entries, "")
                }
            }
            if (NROW(entries)) {
                if (is.matrix(entries) && ncol(entries) == 2L) 
                  db <- rbind(db, cbind(packageName, dirname(path), 
                    entries))
                else warning(gettextf("data index for package %s is invalid and will be ignored", 
                  sQuote(packageName)), domain = NA, call. = FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- if (missing(package)) 
            paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
                "\n", "to list the data sets in all *available* packages.")
        else NULL
        y <- list(title = "Data sets", header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
    paths <- file.path(paths, "data")
    for (name in names) {
        found <- FALSE
        for (p in paths) {
            tmp_env <- if (overwrite) 
                envir
            else new.env()
            if (file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                  found <- TRUE
                  if (verbose) 
                    message(sprintf("name=%s:\t found in Rdata.rds", 
                      name), domain = NA)
                  thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                  thispkg <- sub("_.*$", "", thispkg)
                  thispkg <- paste0("package:", thispkg)
                  objs <- rds[[name]]
                  lazyLoad(file.path(p, "Rdata"), envir = tmp_env, 
                    filter = function(x) x %in% objs)
                  break
                }
                else if (verbose) 
                  message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
                    name, paste(names(rds), collapse = ",")), 
                    domain = NA)
            }
            if (file_test("-f", file.path(p, "Rdata.zip"))) {
                warning("zipped data found for package ", sQuote(basename(dirname(p))), 
                  ".\nThat is defunct, so please re-install the package.", 
                  domain = NA)
                if (file_test("-f", fp <- file.path(p, "filelist"))) 
                  files <- file.path(p, scan(fp, what = "", quiet = TRUE))
                else {
                  warning(gettextf("file 'filelist' is missing for directory %s", 
                    sQuote(p)), domain = NA)
                  next
                }
            }
            else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1L) {
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
                paths0 <- factor(paths0, levels = unique(paths0))
                files <- files[order(paths0, o)]
            }
            if (length(files)) {
                for (file in files) {
                  if (verbose) 
                    message("name=", name, ":\t file= ...", .Platform$file.sep, 
                      basename(file), "::\t", appendLF = FALSE, 
                      domain = NA)
                  ext <- fileExt(file)
                  if (basename(file) != paste0(name, ".", ext)) 
                    found <- FALSE
                  else {
                    found <- TRUE
                    zfile <- file
                    zipname <- file.path(dirname(file), "Rdata.zip")
                    if (file.exists(zipname)) {
                      Rdatadir <- tempfile("Rdata")
                      dir.create(Rdatadir, showWarnings = FALSE)
                      topic <- basename(file)
                      rc <- .External(C_unzip, zipname, topic, 
                        Rdatadir, FALSE, TRUE, FALSE, FALSE)
                      if (rc == 0L) 
                        zfile <- file.path(Rdatadir, topic)
                    }
                    if (zfile != file) 
                      on.exit(unlink(zfile))
                    switch(ext, R = , r = {
                      library("utils")
                      sys.source(zfile, chdir = TRUE, envir = tmp_env)
                    }, RData = , rdata = , rda = load(zfile, 
                      envir = tmp_env), TXT = , txt = , tab = , 
                      tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
                      txt.bz2 = , txt.xz = assign(name, my_read_table(zfile, 
                        header = TRUE, as.is = FALSE), envir = tmp_env), 
                      CSV = , csv = , csv.gz = , csv.bz2 = , 
                      csv.xz = assign(name, my_read_table(zfile, 
                        header = TRUE, sep = ";", as.is = FALSE), 
                        envir = tmp_env), found <- FALSE)
                  }
                  if (found) 
                    break
                }
                if (verbose) 
                  message(if (!found) 
                    "*NOT* ", "found", domain = NA)
            }
            if (found) 
                break
        }
        if (!found) {
            warning(gettextf("data set %s not found", sQuote(name)), 
                domain = NA)
        }
        else if (!overwrite) {
            for (o in ls(envir = tmp_env, all.names = TRUE)) {
                if (exists(o, envir = envir, inherits = FALSE)) 
                  warning(gettextf("an object named %s already exists and will not be overwritten", 
                    sQuote(o)))
                else assign(o, get(o, envir = tmp_env, inherits = FALSE), 
                  envir = envir)
            }
            rm(tmp_env)
        }
    }
    invisible(names)
}
<bytecode: 0x555eac4d9bd8>
<environment: namespace:utils>
```

```r
dbClearResult(results)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'res' in selecting a method for function 'dbClearResult': object 'results' not found
```

> ## Exercise
>
> What happens if you send invalid SQL syntax?
>
> > ## Solution
> >
> > An error message is returned from SQLite.
> > Notice that R is just the conduit; it cannot check the SQL syntax.
> >
> >
> {: .solution}
{: .challenge}

We can also create a new database and add tables to it. Let's base this new dataframe on the Question1 table that can be found in our existing database.


```r
# First, use a SQL query to extract the Question1 table from the existing database
results = dbSendQuery(mydb, "SELECT * from Question1")
```

```{.error}
Error: no such table: Question1
```

```r
# Then, store it as a dataframe
Q1 <- fetch(results)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'res' in selecting a method for function 'fetch': object 'results' not found
```

Now, we can create the new database and add data to it, either from an external file or a local dataframe.


```r
dbfile_new = "data/a_newdb.sqlite"
mydb_new = dbConnect(dbDriver("SQLite"), dbfile_new)

dbWriteTable(conn = mydb_new , name = "SN7577", value = "data/SN7577.csv",
             row.names = FALSE, header = TRUE)
```

```{.warning}
Warning in file(file, "rt"): cannot open file 'data/SN7577.csv': No such file
or directory
```

```{.error}
Error in file(file, "rt"): cannot open the connection
```

```r
dbWriteTable(conn = mydb_new , name = "Q1", value = Q1,
             row.names = FALSE)
```

```{.error}
Error in h(simpleError(msg, call)): error in evaluating the argument 'value' in selecting a method for function 'dbWriteTable': object 'Q1' not found
```

```r
dbListTables(mydb_new)
```

```{.output}
character(0)
```

## Connecting to a Database for `dplyr` Use

When we want to use `dplyr` functions to operate directly on the database tables,
a different connection method is used.


```r
mydb_dplyr <- src_sqlite(path="data/SN7577.sqlite")
```

```{.warning}
Warning: `src_sqlite()` was deprecated in dplyr 1.0.0.
ℹ Please use `tbl()` directly with a database connection
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
generated.
```

as is the method for running queries. However using the 'tbl' function we still need to provide avalid SQL string. (?)


```r
tbl(mydb_dplyr, sql("SELECT count(*) from SN7577"))
```

```{.error}
Error in `db_query_fields.DBIConnection()`:
! Can't query fields.
Caused by error:
! no such table: SN7577
```

The real advantage of using `dplyr` is that once we have stored the table as an object
(here, `SN7577_d`), we can use `dplyr` functions instead of SQL statements.


```r
# Store the table as an object
SN7577_d <- tbl(mydb_dplyr, sql("SELECT * FROM SN7577"))
```

```{.error}
Error in `db_query_fields.DBIConnection()`:
! Can't query fields.
Caused by error:
! no such table: SN7577
```

```r
# Explore the object
head(SN7577_d, n = 10)
```

```{.error}
Error in eval(expr, envir, enclos): object 'SN7577_d' not found
```

```r
nrow(SN7577_d)
```

```{.error}
Error in eval(expr, envir, enclos): object 'SN7577_d' not found
```

```r
# Apply dplyr functions to the object
SN7577_d %>%
  filter(numage > 60) %>%
  select(sex, age, numage) %>%
  group_by(sex, age) %>%
  summarize(avg_age = mean(numage))
```

```{.error}
Error in eval(expr, envir, enclos): object 'SN7577_d' not found
```

Notice that on the `nrow` command we get NA rather than a count of rows. Thisis because `dplyr` doesn't hold the full table even after the 'Select * ...'

If you need the row count you can use


```r
SN7577_d %>%
  tally()
```

```{.error}
Error in eval(expr, envir, enclos): object 'SN7577_d' not found
```

> ## Exercise
>
> Store the SN7577 table as an object for `dplyr` use.
>
> Write a query using `dplyr` functions that will return the average age (`numage`) by sex for all records where
> the response for Q2 is missing (missing values are indicated by a value of -1).
>
> > ## Solution
> >
> > 
> > ```r
> > SN7577_d <- tbl(mydb_dplyr, sql("SELECT * FROM SN7577"))
> > ```
> > 
> > ```{.error}
> > Error in `db_query_fields.DBIConnection()`:
> > ! Can't query fields.
> > Caused by error:
> > ! no such table: SN7577
> > ```
> > 
> > ```r
> > SN7577_d %>%
> >   filter(Q2 == -1)   %>%
> >   group_by(sex)   %>%
> >   summarize(avg_age = mean(numage))
> > ```
> > 
> > ```{.error}
> > Error in eval(expr, envir, enclos): object 'SN7577_d' not found
> > ```
> >
> {: .solution}
{: .challenge}

{% include links.md %}
