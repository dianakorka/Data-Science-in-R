## Introduction

Here we will study the open data about politicians in Zürich and see if we can find interesting patterns. 
The data comes to you in a relational database.

![](https://d7whxh71cqykp.cloudfront.net/uploads/image/data/2221/04-project-zh.png)

In this project, we will increase the complexity a bit: 

- First the data is on four different tables, so there will be a lot more joins. 
- Second we are not giving you `.csv` files but a functional `SQLite` database. 

Although we have seen how you can use your normal `{dplyr}` knowledge directly with databases, you will still have to write the code to connect to it and `collect()` the data you need at the right time.

## The data

We placed in your repository a `.db` file:

* `zh_politicians.db`

This file is a `SQLite` database and contains data about elected politicians in Zürich over the years. 
Some columns like `activity` are in German but this won't affect your analysis. 
The different tables have information about:

- The elected persons (name, gender, year of birth, year of death...)
- The mandates people have been elected for
- The addresses of the elected persons
- The affiliations to political organisation

This is a relational database, so you will probably need to do a lot of `JOINS` between tables. 
Take the time to study the database structure, either directly with `R` using `{DBI}` and `{dplyr}` or first with the software we used in the course "DB Browser for SQLite".

## Warning

This dataset is **quite messy**: consider this as a real-life example! Missing dates, misspelled names…

Your goal is to do your best with what you were given and explain your thought process along the way. 
For example, if a politician seems to be 3 years old or 200 years old, filter him/her out before calculating the average age of politicians...

## Before you start

Early on, you will realise that you need to create some "missing" data. 
You will need to use advanced concepts like nested cells.
Although we strongly advise that you review the relevant units in the course, we also put a little example below.

> What if I create a column with `mutate()` that put not one value in each cells, but a collection of values in each cell (`vector` or `list`)?

There is a dataset, called `starwars` that comes pre-installed with `{dplyr}` that will show you that kind of table. 
Try the code below:

```r
library(dplyr)
starwars %>%
  select(name, species, films) # Showing only a few columns for clarity
```

```
# A tibble: 87 x 3
   name               species films    
   <chr>              <chr>   <list>   
 1 Luke Skywalker     Human   <chr [5]>
 2 C-3PO              Droid   <chr [6]>
```

As you can see the cells in the column `films` do not contain "single value". 
Instead, they have `vector` of `character`:

- Row 1 has `5` items in the `films` column (probably meaning that "Luke Skywalker" was in 5 films)
- Row 2 has `6` items in the `films` column (probably meaning that "C-3PO" was in 6 films)...

You can get a better look at the content of the film column if you use `View()`:

```r
library(dplyr)
starwars %>%
  select(name, species, films) %>%
  View()
```

With `View()`, you will see at least the first items. 
For example the first row show something like this:

```
c("Revenge of the Sith", "Return of the Jedi", 
  "The Empire Strikes Back", "A New Hope", 
  "The Force Awakens")
```

We can see that the cell contains a `vector` indeed. 
Another way to see the content could be to extract the column out of the `tibble` with our trusted `pull()` function.

```r
library(dplyr)
starwars %>%
  pull(films) %>%
  head(3) # showing just the first result...
```

```
[[1]]
[1] "Revenge of the Sith"     "Return of the Jedi"      "The Empire Strikes Back"
[4] "A New Hope"              "The Force Awakens"      

[[2]]
[1] "Attack of the Clones"    "The Phantom Menace"      "Revenge of the Sith"    
[4] "Return of the Jedi"      "The Empire Strikes Back" "A New Hope"             

[[3]]
[1] "Attack of the Clones"    "The Phantom Menace"      "Revenge of the Sith"    
[4] "Return of the Jedi"      "The Empire Strikes Back" "A New Hope"             
[7] "The Force Awakens"
```

Here again, looking at these double brackets (`[[`) we see that we are dealing with a `list` with each item being a `vector` of films.

The `list-columns`, as they are called, are one of the most powerful features of the `{tidyverse}`: they kind of let you create "multi-dimensional" `tibble`. 
But this only become interesting if they let you "come back" to a normal "flat" `tibble`...which is what the `unnest()` function from `{tidyr}` is used for!

Let's say that I want to calculate the number of characters that have been in each movie. 
For that I need to be able to use `group_by()` on a *per-film* basis. 
How can I get a column that will contains only one `film`? `unnest()` to the rescue!

```
library(dplyr)
library(tidyr)
starwars %>%
  select(name, species, films) %>%
  unnest(films)
```

```
# A tibble: 173 x 3
   name           species films                  
   <chr>          <chr>   <chr>                  
 1 Luke Skywalker Human   Revenge of the Sith    
 2 Luke Skywalker Human   Return of the Jedi     
 3 Luke Skywalker Human   The Empire Strikes Back
 4 Luke Skywalker Human   A New Hope             
 5 Luke Skywalker Human   The Force Awakens      
 6 C-3PO          Droid   Attack of the Clones   
 7 C-3PO          Droid   The Phantom Menace     
 8 C-3PO          Droid   Revenge of the Sith    
 9 C-3PO          Droid   Return of the Jedi     
10 C-3PO          Droid   The Empire Strikes Back
# ... with 163 more rows
```

Just like that we now have one film per row (with all the other values being repeated)! 
That is why our `tibble` went from `87` to `173` rows. 
And this `tibble` can be used to do calculation per film!

In this project, you will need this concept to work your way easily from rows that only have "start year" and "end year" to something usable for analysis.

`list-columns` is a topic that we can only brush on in this course. But if you want to learn more about them now, we cannot recommend enough these two videos (in this order):

- [Garrett Grolemund - How to work with list columns](https://resources.rstudio.com/webinars/how-to-work-with-list-columns-garrett-grolemund)
- [Jenny Bryan - Thinking inside the box](https://www.rstudio.com/resources/webinars/thinking-inside-the-box-you-can-do-that-inside-a-data-frame/)

## And one more hint

Do you remember `map()`, the function from the `{purrr}` package that we used to apply functions to each elements of a collection (i.e. `list` or `vector`)?
Let's do a quick recap.

Most functions in R can accept collections and happily do their job on each item. 
If I use the `ymd()` function from `{lubridate}`, we can give it one date or two thousands dates and it will work.

However, some functions are not designed to work with collections and this can be annoying. 
For example, if I have a list of websites that I want to download, I cannot give a `vector` with lots of urls to the `GET()` function of the `{httr}` package. 
It will crash and say:

> Hey, my `url` argument (which is `GET()` first argument) must be of length `1`!

...which basically its way to say that it can only accept one url at a time.

`map()` is a great workaround in these situation. 
`map()` takes a collection as its first argument and a function as its second argument. 
Then it applies the function to each element in the collection and wrap all the results in a new `list` that we can use for analysis.

So if we have three urls, we could do:

```r
library(httr)
library(purrr)

urls <- c("www.google.com", "www.wikipedia.com", "www.extensionschool.ch")

map(urls, GET)
```

Note that we did not do:

```r
map(GET(urls))
```

This is something that **always confuse people** when they discover `map()`. They will say:

> But I put it in `map()`! It should work now!

However if you do it this way, R will first execute `GET(urls)` and crash even before it ever get the chance to run `map()`. 
That is why the collection and the function are **given separately**.

What does this mean if you want to use a function on two collections? 
For example, you want to use the function `seq()` which takes a start value (the argument is called `from=`) and an end value (the argument is called `to=`) and create a `vector` with all the value in between.

```r
seq(1, 10)
```

```
[1]  1  2  3  4  5  6  7  8  9 10
```

We did not spend time on `seq()` in the course, but we saw its cousin, the `:` operator (e.g. `1:10`).

`seq()`, like `GET()`, doesn't work on collections. 
If I give a `vector` of three values for `from=` and a `vector` of three values for `to=`, I will **not** get three `vector`s with all the values in between.

```r
seq(c(1,10,100), c(3, 13, 103))
```

```r
Error in seq.default(c(1, 10, 100), c(3, 13, 103)) :
  'from' must be of length 1
```

Can we use something like `seq()` with `{purrr}`? 
We sure can, with just one twist: `map()` only takes one collection. If you do:

```r
map(c(1,10,100), c(3, 13, 103), seq)
```

`map()` will think that the second argument (i.e. `c(3, 13, 103)`) is the function you want to apply to the first argument... 
Needless to say that it will be terribly confused! 
But `map2()` is designed to take two arguments before getting a function, so this will work:

```r
map2(c(1,10,100), c(3, 13, 103), seq)
```

```
[[1]]
[1] 1 2 3

[[2]]
[1] 10 11 12 13

[[3]]
[1] 100 101 102 103
```

So **powerful**!
Now I see you coming with:

> What if I have 3, 6 or 28 collections?

Realistically, it nearly never happen. 
Even `map2()` is rare. 
But if you want to venture into more collections, you can explore the `pmap()` function, which we won't cover here.

Why is `map2()` important if it is so niche? 
First we think understanding `map2()` helps you understanding `map()`. 
Second, you might want to use `mutate` and `seq()` with two columns from your `tibble` and get a result for each row in this project. 
Who knows, `map2()` could be handy if you ever found yourself in this situation...

---

## Exercises
### Part 1

The `MANDATES` table has information about when people were elected, when their mandate ended and on which assembly they sat.

Using a line plot, show how the number of people with an active mandate changed over the years. 
You will have one line per assembly.

![](https://d7whxh71cqykp.cloudfront.net/uploads/image/data/4199/04-project-politics-sketch.png)

You can see above what it will roughly look like. 
Don't bother working on exact dates: you can base your "active" years just on years, even if this creates some "dents" in the line (i.e. when looking only at active years, not precise dates, some mandates are double counted on election years since the old mandates end and the new mandates start).

### Part 2

Expand on the plot you just produced. 
This time you want to show a facet charts with one chart per assembly. 
In each chart, have one line for men and one line for women.

### Part 3

Create a new plot showing the proportion of elected politicians from each party in year `2000`. 
You want to show this by assembly, so use one facet with one pie chart per assembly. 
Also show your result in a table.

If you don't know how to do a pie chart, try to search on the internet how to do pie charts with `{ggplot2}`. 
We will happily give you some links if you cannot find any, but one of the goal of this course is that you feel confident enough to find your own resources on the internet.

There are several ways to create nice-looking tables in R notebook. 
Have a look at the "Table Suggestions" part in the [cheatsheet](https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf) and choose your favorite.

### Part 4

Have another look at the composition of the assemblies: this time use a line chart to show how it changed over the years.

### Part 5

For the politicians that have a `YEAR_OF_DEATH` in the data, find the average life span. 
Does it change if you go by a `TITLE`?
Use first a plot, and **then a statistical test** to assess if the average life span is different for politicians having a `TITLE` compared to those without any `TITLE`.

We only ask you to compare `TITLE` vs. no `TITLE`, there is no need to analyse every possible title.

### Part 6

Create a new variable which splits the politicians into 2 subgroups: one subgroup of politicians who were `BORN_BEFORE_1918` (i.e. for which `YEAR_OF_BIRTH < 1918`), and another subgroup of politicians who were `BORN_AFTER_1918` (i.e. for which `YEAR_OF_BIRTH >= 1918`). 
Reply to the questions asked in the previous part (i.e. graphical comparison **and** statistical test) for each of the 2 subgroups.

### Part 7

Which politicians have had the most mandates? 
Create a top 10 horizontal bar chart.

### Part 8

Do some politicians have multiple mandates at the same time? 
You will need to find a way to check if some mandates started before the previous one of the same politician ended.

### Part 9

Have some politicians been affiliated to different parties over the years?

### Part 10

Take a sample of 20 politicians with a listed address and plot them on a `{leaflet}` map. 
You will need to use an API that converts the addresses to geocoordinates.
