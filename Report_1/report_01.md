## Introduction

This project is challenging on the data extraction side. 
You are going to get your dataset from the website of a fake rental agency. 
The goal is to practice your scraping skills with the `{rvest}` package and end up with a clean `tibble` that you can use for analysis.

![](https://d7whxh71cqykp.cloudfront.net/uploads/image/data/2228/04-project-immoscrape.png)

## The data

The data doesn't come to you as a dataset, it comes to you as a website.

- [https://epfl-exts.github.io/rental-scrape/](https://epfl-exts.github.io/rental-scrape/)

Your first challenge is to write the code needed to scrape the data contained in it. 
To help you see what the end goal is, we placed a sample extract of the final dataset in your repository. 
The file has only `5` rows and is called `sample_scraped_rental.csv`.

## Exercises

### Part 1

Get the full dataset out of the site. 
Your code should end up with a `tibble` of a bit more than `600` rows and `9` columns. 
Make sure you take the time to convert each column to the right type and not all `character`.

### Part 2

Create a scatterplot showing how price evolve with living space of the flat.

### Part 3

Create a bar plot showing the number of properties by postcode. Is this agency more "active" in certain areas?

### Part 4

Create a more complex scatterplot, showing how price evolve with living space of the flat by postcode and by floor. 
You can use colors and/or facets to make the categories visible.

![](https://d7whxh71cqykp.cloudfront.net/uploads/image/data/4207/04-project-4-sketch.png)

Can you conclude anything from it? 
Put your thoughts below your plot. 
Don't overthink this: this is not so much about the quality of your analysis than checking that you can put a report together. 
For example, integrate the most expansive and least expansive mean postcode/floor combo in your text with [inline code](https://rmarkdown.rstudio.com/lesson-4.html).

### Part 5

Can you see any trends for listings with addresses only available on demand?
Are they more expensive or less? 
Bigger living space? 
Higher floor? 

Boxplot or violin plots work really well to compare distributions of values between groups. 
So use one for each of the three questions above.

### Part 6

In this question, use the same groups as above: flats which addresses are only available on demand versus other flats.
Make a table summarising group size, median, average, standard-deviation, minimum and maximum of variable *price per square-meter* (expressed in CHF/$m^2$). 
Then use a t-test to compare the average price per square-meter for these 2 groups of flats.

### Part 7

Same questions as above (i.e. summary table and t-test) regarding variable *price* (expressed in CHF). 
What is your conclusion?

### Part 8

Convert a subset of 30 addresses to latitude and longitude using an API and plot them on a map using `{ggmap}` (or `{leaflet}` if you cannot get `{ggmap}` to work).
