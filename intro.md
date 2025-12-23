---
title: "Before we Start"
teaching: 10
exercises: 5
---

:::: questions

- How to find your way around Positron?
- How to interact with R?
- How to manage your environment?
- How to install packages?

::::::

::::::::::::::::::::::::::::::::::::: objectives

- Install latest version of R
- Install latest version of Positron
- Navigate the Positron GUI
- Install additional packages using the packages tab
- Install additional packages using R code

::::::::::::::::::::::::::::::::::::::::::::::::


## What is R? What is Positron?

The term "`R`" is used to refer to both the programming language and the
software that interprets the scripts written in R.

[Positron](https://positron.posit.co/) is currently a very popular way to not only write
your R scripts but also to interact with the R software. Positron is the most
popular IDE (Integrated Development Environmemt) for R. An IDE is a piece of
software that provides tools to make programming easier.

To make it easier to interact with R, we will use Positron. To function correctly,
Positron needs R and therefore both need to be installed on your computer.


## Why learn R?

There are numerous reasons to learn and use R. In the following we will point 
out a few of them.

### R does not involve lots of pointing and clicking

The learning curve might be steeper than with other software, but with R, the
results of your analysis do not rely on remembering a succession of pointing
and clicking, but instead on a series of written commands, and that's a good
thing! So, if you want to redo your analysis because you collected more data,
you don't have to remember which button you clicked in which order to obtain
your results; you just have to run your script again.

Working with scripts makes the steps you used in your analysis clear, and the
code you write can be inspected by someone else who can give you feedback and
spot mistakes.

Working with scripts forces you to have a deeper understanding of what you are
doing, and facilitates your learning and comprehension of the methods you use.

### R code is great for reproducibility

Reproducibility is when someone else (including your future self) can obtain the
same results from the same dataset when using the same analysis.

R integrates with other tools to generate manuscripts from your code. If you
collect more data, or fix a mistake in your dataset, the figures and the
statistical tests in your manuscript are updated automatically.

An increasing number of journals and funding agencies expect analyses to be
reproducible, so knowing R will give you an edge with these requirements.

### R is interdisciplinary and extensible

With 18,000+ packages that can be installed to extend its capabilities, R
provides a framework that allows you to combine statistical approaches from many
scientific disciplines to best suit the analytical framework you need to analyze
your data. For instance, R has packages for image analysis, GIS, time series,
population genetics, and a lot more.

### R works on data of all shapes and sizes

The skills you learn with R scale easily with the size of your dataset. Whether
your dataset has hundreds or millions of lines, it won't make much difference to
you.

R is designed for data analysis. It comes with special data structures and data
types that make handling of missing data and statistical factors convenient.

R can connect to spreadsheets, databases, and many other data formats, on your
computer or on the web.

### R produces high-quality graphics

The plotting functionalities in R are endless, and allow you to adjust any
aspect of your graph to convey most effectively the message from your data.

### R has a large and welcoming community

Thousands of people use R daily. Many of them are willing to help you through
mailing lists and websites such as [Stack Overflow](https://stackoverflow.com/),
or on the [Positron Community](https://forum.posit.co/). Questions which
are backed up with [short, reproducible code
snippets](https://www.tidyverse.org/help/) are more likely to attract
knowledgeable responses.

### Not only is R free, but it is also open-source and cross-platform

Anyone can inspect the source code to see how R works. Because of this
transparency, there is less chance for mistakes, and if you (or someone else)
find some, you can report and fix bugs.

Because R is open source and is supported by a large community of developers and
users, there is a very large selection of third-party add-on packages which are
freely available to extend R's native capabilities.



<img src="./fig/r-manual.jpeg" alt="Positron extends what R can do, and makes it easier to write R code and interact with R." width="100%" style="display: block; margin: auto;" />


<img src="./fig/r-automatic.jpeg" alt="automatic car gear shift representing the ease of Positron" width="100%" style="display: block; margin: auto;" />

Positron extends what R can do, and makes it easier to write R code and interact
with R. <a href="https://unsplash.com/photos/D19rXKDUPYM">Left photo credit</a>; <a href="https://unsplash.com/photos/Wec3M4dY_LE">Right photo credit</a>.




## A tour of Positron

Let's start by learning about [Positron](https://positron.posit.co/), which is an
Integrated Development Environment (IDE) for working with R.

The Positron IDE open-source product is free under the
[Elastic License 2.0](https://github.com/posit-dev/positron?tab=License-1-ov-file#readme).
The Positron IDE is also available with a commercial license and priority email
support from Posit Software, Inc.

We will use the Positron IDE to write code, navigate the files on our computer,
inspect the variables we create, and visualize the plots we generate. Positron
can also be used for other things (e.g., version control, developing packages,
writing Shiny apps) that we will not cover during the workshop.

One of the advantages of using Positron is that all the information
you need to write code is available in a single window. Additionally, Positron
provides many shortcuts, autocompletion, and highlighting for the major file
types you use while developing in R. Positron makes typing easier and less
error-prone.

### The Positron Interface

Let's take a quick tour of Positron.

![Positron interface](fig/R_00_Positron_01.png){alt='A picture of Positron interface'}

Positron is divided into four "panes". The placement of these
panes and their content can be customized. Click "Costumize layout" in the upper right corner.

The Default Layout is:
- Top Center - **Code editor**: your scripts and documents
- Bottom Center - **Console**: what R would look and be like without Positron
- Top Right - **Variables**: your objects and data frames
- Bottom Right - **Plots**: see your visualisations

### Getting set up

It is good practice to keep a set of related data, analyses, and text
self-contained in a single folder called the **working directory**. All of the
scripts within this folder can then use *relative paths* to files. Relative
paths indicate where inside the project a file is located (as opposed to
absolute paths, which point to where a file is on a specific computer). Working
this way makes it a lot easier to move your project around on your computer and
share it with others without having to directly modify file paths in the
individual scripts.

Positron provides a helpful set of tools to do this through its "Project Folder"
interface, which not only creates a working directory for you but also remembers
its location (allowing you to quickly navigate to it). The interface also
(optionally) preserves custom settings and open files to make it easier to
resume work after a break.

An easy way to work like this is to create project folders in Positron.


### Create a new project folder and a new script file

* Under the `File` menu, click on `New Folder from Template`, choose `R Project`, then click
  `Next`
* Enter a name for this new folder and choose a convenient
  location for it. This will be your **working directory** for working on this
  project (e.g., `~/r_intro`)
* Click on `Next`
* Choose `Project Configurations` by selecting the version of R you wish to work with.
* Click `Create`
* A new unsaved script is automatically created. Click <kbd>ctrl</kbd>/cmd-s to save
* Create a new file where you will type our scripts. Go to File > New File > R
  script. Click the save icon on your toolbar and save your script as
  "`script.R`".

The simplest way to open an existing Positron project folder is to open Positron and select the project folder from the drop-down menu in the upper-right corner.

By doing it this way, you have easy acces to the data, plots and scripts belonging to your project folder.

### Organizing your working directory

Using a consistent folder structure across your projects will help keep things
organized and make it easy to find/file things in the future. This
can be especially helpful when you have multiple projects. In general, you might
create directories (folders) for **scripts**, **data**, and **documents**. Here
are some examples of suggested directories:

 - **`data/`** Use this folder to store your raw data and intermediate datasets.
   For the sake of transparency and 
   [provenance](https://en.wikipedia.org/wiki/Provenance), you
   should *always* keep a copy of your raw data accessible and do as much of
   your data cleanup and preprocessing programmatically (i.e., with scripts,
   rather than manually) as possible.
 - **`data_output/`** When you need to modify your raw data,
   it might be useful to store the modified versions of the datasets in a
   different folder.
 - **`documents/`** Used for outlines, drafts, and other
   text.
 - **`fig_output/`** This folder can store the graphics that are generated
   by your scripts.
 - **`scripts/`** A place to keep your R scripts for
   different analyses or plotting.

You may want additional directories or subdirectories depending on your project
needs, but these should form the backbone of your working directory.

![Example of a working directory structure](fig/rstudio_project_files.jpeg){alt='A picture showing the layout of a good working directory as described above'}

### The working directory

The working directory is an important concept to understand. It is the place
where R will look for and save files. When you write code for your project, your
scripts should refer to files in relation to the root of your working directory
and only to files within this structure.

Using the Positron project folder structure makes this easy and ensures that your working directory
is set up properly. If you need to check it, you can use `getwd()`. If for some
reason your working directory is not the same as the location of your Positron 
project folder, it is likely that you opened an R script or RMarkdown file. 


### Downloading the data and getting set up

For this lesson we will use the following folders in our working directory:
**`data/`**, **`data_output/`** and **`fig_output/`**. Let's write them all in
lowercase to be consistent. We can create them using the Positron interface by
clicking on the "New Folder" button in the file pane (bottom right), or directly
from R by typing at console:


``` r
dir.create("data")
dir.create("data_output")
dir.create("fig_output")
```

Begin by downloading the dataset called `movie_series.csv` and place it in the `data`-folder you just created. You can do this directly 
from Positron by copying this R-code and pasting it in your terminal:


``` r
download.file("https://raw.githubusercontent.com/KUBDatalab/R-intro/main/episodes/data/movie_series.csv", "data/movie_series.csv", mode = "wb")
```


## Interacting with R in Positron

The basis of programming is that we write down instructions for the computer to
follow, and then we tell the computer to follow those instructions. We write, or
*code*, instructions in R because it is a common language that both the computer
and we can understand. We call the instructions *commands* and we tell the
computer to follow the instructions by *executing* (also called *running*) those
commands.

There are two main ways of interacting with R in Positron: by using the console or by using
script files (plain text files that contain your code). The console pane (in
Positron, the lower center panel) is the place where commands written in the R
language can be typed and executed immediately by the computer. This is also where
the results will be shown for commands that have been executed. You can type
commands directly into the console and press <kbd>Enter</kbd> to execute those
commands. However, they will be forgotten when you close the session.

Because we want our code and workflow to be reproducible, it is better to type
the commands we want in the script editor and save the script. This way, there
is a complete record of what we did, and anyone (including our future selves)
can easily replicate the results on their computer.

Positron allows you to execute commands directly from the script editor by using
the <kbd>Ctrl</kbd> + <kbd>Enter</kbd> shortcut (on Mac, <kbd>Cmd</kbd> +
<kbd>Return</kbd> will work). The command on the current line in the
script (indicated by the cursor) or all of the commands in
selected text will be sent to the console and executed when you press
<kbd>Ctrl</kbd> + <kbd>Enter</kbd>. If there is information in the console
you do not need anymore, you can clear it with <kbd>Ctrl</kbd> + <kbd>L</kbd>.
<!-- You can find other keyboard shortcuts in this -->
<!-- [Positron cheatsheet about the Positron IDE](https://github.com/rstudio/cheatsheets/raw/master/rstudio-ide.pdf). -->

At some point in your analysis, you may want to check the content of a variable
or the structure of an object without necessarily keeping a record of it in
your script. You can type these commands and execute them directly in the
console.

If R is ready to accept commands, the R console shows a `>` prompt. If R
receives a command (by typing, copy-pasting, or sent from the script editor using
<kbd>Ctrl</kbd> + <kbd>Enter</kbd>), R will try to execute it and, when
ready, will show the results and come back with a new `>` prompt to wait for new
commands.

If this does not happen it means that your code is incomplete. In other words R is waiting for more information in order to execute the code. To correct this, delete the latest line in the console, add the missing information in your script and run the code in question again.

Incomplete lines of code are often caused by an uneven number of parantheses or qoutation markes. It could also be caused by spelling mistakes.

## Packages

In addition to the core R installation, there are in excess of
18,000 additional packages which can be used to extend the
functionality of R. Many of these have been written by R users and
have been made available in central repositories, like the one
hosted at CRAN, for anyone to download and install into their own R
environment. 


### Installing additional packages using Positron

You can see if you have a package installed by typing the command
`installed.packages()` into the console and examine the output.

If you wish to install a package, write `install.packages()` with the name of the package in quotation marks between the two parantheses.

Because installing packages requires access to the CRAN repository, an internet connection is necessary.

It is also possible to install packages from other repositories as
well as Github or the local file system, but we won’t be looking at these options in this lesson.


### Installing `tidyverse`

For this course we need the package `tidyverse`, so in order to install this packages you can write the following in the console


``` r
install.packages("tidyverse")
```

The `tidyverse` package is really a package consisting of several packages. Among these are `ggplot2` and `dplyr`, both of which require other packages to run correctly.
All of these packages will be installed automatically, when you install `tidyverse`. The installation time may vary, depending on what has already been installed. As the install proceeds, messages relating to its progress will be written to the console.

::::::::::::::::::::::::::::::::::::: challenge 
Use the console to confirm you have the `tidyverse` installed.

:::::::::::::::::::::::: solution


Type `tidy` in the console. Based on the four letters, Positron will auto-suggest `tidyverse`, if the package has already been installed. Depending on the network connection, you might have to allow a few seconds the the auto-suggest to work.


:::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: keypoints

- Use Positron to write and execute R scripts
- Use `install.packages()` to install packages (libraries)

::::::::::::::::::::::::::::::::::::::::::::::::
