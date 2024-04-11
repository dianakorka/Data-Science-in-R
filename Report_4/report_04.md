## Introduction

Our last project takes us back to the good old spreadsheet. 
You will need to extract the data from it before starting your analysis.

![](https://d7whxh71cqykp.cloudfront.net/uploads/image/data/2277/04-project-olympics.png)

We will look at a dataset that contains data about athletes at the Olympic Games.

## The data

The data is split in four sheets.

- **games**: basic data about the games (season, city...)
- **athletes**: contains basic data about the athletes (name, height, weight, gender...) that are constant for all games. Note that we only have one measure of weight, height and gender so you will have to assume that this did not change even if the athlete participated to the Games more than once
- **country**: contains basic data about the athletes that sometimes changed over time (country, age)
- **medals**: a list of results (type of medals for the winners, `NA` for the others)

Before you start doing lots of `joins`, check what happened at the 1956 games, it might be interesting...

## Exercises

### Part 1

Have some athletes competed for different countries over time?

### Part 2

Who are the ten athletes that took part in most games?

### Part 3

What athlete(s) kept a Gold medal for the longest time?

### Part 4

What country(ies) kept a Gold medal for the longest time?

### Part 5

Who are the ten athletes that competed in the most events (some athletes take part in more than one event during games) ?

### Part 6

Create a new table showing the number of medals per country (rows) and per year (column). 
Keep only the 15 countries with the most medals overall.

### Part 7

*Is there are relationship between country and the probability of winning a medal?*

We know that some countries win more medals than others. 
In this part we want to work out whether some countries win more medals simply because they have more athletes competing for more events, or whether their athletes are in fact performing better in general.

For this part, look only at the top 15 countries that you established in part 6.

- First, create a horizontal barchart listing for each of the fifteen countries the percentage of medals won out of all medals competed for by that country.
- Run a Chi Square test to test the hypothesis: "There is no relationship between country and whether they win a medal or not".
- Include in your report a description of the interpretation of the results.

### Part 8
Create a scatterplot showing the average height and weight of competitors per sport (one dot per sport). 
Add labels with the sport names for:

- the largest average height
- the largest average weight
- the smallest average height
- the smallest average weight
- the largest average BMI
- the smallest average BMI

It might be that the same dot qualify for multiple labels. 
The formula to calculate Body Mass Index (BMI) is:

```
weight in kg / (height in meters)^2
```

### Part 9

Create a line plot showing the number of medals given by year (one line for Gold, one line for Silver and one line for Bronze). 
Does it change over time? 
Use facet to separate the medals at Summer games and Winter games.
