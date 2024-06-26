FDS Final Project: Report \#3
================

## Part 1

In this project, I use an API to get your data from the “The Movie DB”,
<https://www.themoviedb.org/>.

``` r
library(httr)
library(purrr)
library(magrittr)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(infer)
```

Try to send a GET request to some of the example queries and inspect the
result.

Below is the request for the movie with id 550.

``` r
mov_550 <- GET("https://api.themoviedb.org/3/movie/550?api_key=32dcb877caa66aa7c0cf44a3fbf215ce")

content(mov_550) %>% 
pluck("original_title")
```

    ## [1] "Fight Club"

Inspired just by these examples, how would create new requests: What are
the highest grossing dramas from 2010? See below

``` r
mov_gross_2010<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&certification_country=US&certification=R&sort_by=revenue.desc&primary_release_year=2010&with_genres=18")

content(mov_gross_2010, as="parsed") %>% 
  pluck("results") %>% 
  map(pluck, "original_title")
```

    ## [[1]]
    ## [1] "The King's Speech"
    ## 
    ## [[2]]
    ## [1] "Black Swan"
    ## 
    ## [[3]]
    ## [1] "Shutter Island"
    ## 
    ## [[4]]
    ## [1] "Sex and the City 2"
    ## 
    ## [[5]]
    ## [1] "Due Date"
    ## 
    ## [[6]]
    ## [1] "The Town"
    ## 
    ## [[7]]
    ## [1] "Love & Other Drugs"
    ## 
    ## [[8]]
    ## [1] "The Other Woman"
    ## 
    ## [[9]]
    ## [1] "Green Zone"
    ## 
    ## [[10]]
    ## [1] "The Fighter"
    ## 
    ## [[11]]
    ## [1] "Edge of Darkness"
    ## 
    ## [[12]]
    ## [1] "The American"
    ## 
    ## [[13]]
    ## [1] "Going the Distance"
    ## 
    ## [[14]]
    ## [1] "For Colored Girls"
    ## 
    ## [[15]]
    ## [1] "<U+8449><U+554F>2"
    ## 
    ## [[16]]
    ## [1] "127 Hours"
    ## 
    ## [[17]]
    ## [1] "The Kids Are All Right"
    ## 
    ## [[18]]
    ## [1] "Biutiful"
    ## 
    ## [[19]]
    ## [1] "Let Me In"
    ## 
    ## [[20]]
    ## [1] "Faster"

Have Will Ferrell and Liam Neeson even been in a movie together? The
answer is yes, in 4 movies.

``` r
mov_2ppl<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_people=23659,3896")

content(mov_2ppl, as="parsed") %>% 
  pluck("results") %>% 
  map(pluck, "original_title")
```

    ## [[1]]
    ## [1] "The Lego Movie"
    ## 
    ## [[2]]
    ## [1] "Daddy's Home 2"
    ## 
    ## [[3]]
    ## [1] "Anchorman 2: The Legend Continues"
    ## 
    ## [[4]]
    ## [1] "And the Oscar Goes To..."

Can you find kids movies with Tom Cruise in them? Yes

``` r
mov_kidTC<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&certification_country=US&certification.lte=G&with_cast=500&sort_by=popularity.desc")

content(mov_kidTC, as="parsed") %>% 
  pluck("results") %>% 
  map(pluck, "original_title")
```

    ## [[1]]
    ## [1] "Mission: Impossible - Fallout"
    ## 
    ## [[2]]
    ## [1] "Mission: Impossible - Rogue Nation"
    ## 
    ## [[3]]
    ## [1] "Edge of Tomorrow"
    ## 
    ## [[4]]
    ## [1] "Oblivion"
    ## 
    ## [[5]]
    ## [1] "Mission: Impossible - Ghost Protocol"
    ## 
    ## [[6]]
    ## [1] "Mission: Impossible"
    ## 
    ## [[7]]
    ## [1] "American Made"
    ## 
    ## [[8]]
    ## [1] "Eyes Wide Shut"
    ## 
    ## [[9]]
    ## [1] "Minority Report"
    ## 
    ## [[10]]
    ## [1] "Collateral"
    ## 
    ## [[11]]
    ## [1] "Mission: Impossible III"
    ## 
    ## [[12]]
    ## [1] "War of the Worlds"
    ## 
    ## [[13]]
    ## [1] "Mission: Impossible II"
    ## 
    ## [[14]]
    ## [1] "Valkyrie"
    ## 
    ## [[15]]
    ## [1] "Top Gun"
    ## 
    ## [[16]]
    ## [1] "Jack Reacher: Never Go Back"
    ## 
    ## [[17]]
    ## [1] "Interview with the Vampire"
    ## 
    ## [[18]]
    ## [1] "The Last Samurai"
    ## 
    ## [[19]]
    ## [1] "A Few Good Men"
    ## 
    ## [[20]]
    ## [1] "Austin Powers in Goldmember"

## Part 2

From RStudio, what query would you make to find the id of the animation
movies company “Pixar”? Show both the query and how you extract the id
from the result in your report.

I used the get/search/company endpoint. There are two id’s stored for
Pixar: 3 and 125928. As the company stored under 3 has full details
(such as origin country)I think this must be the correct id for Pixar.

``` r
mov_pix <- GET("https://api.themoviedb.org/3/search/company?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&query=Pixar&page=1")

content(mov_pix, as="parsed") %>% 
  pluck("results") %>% 
  map(pluck, "id")
```

    ## [[1]]
    ## [1] 3
    ## 
    ## [[2]]
    ## [1] 125928

## Part 3

Now that we have the id of Pixar, we should be able to find all the
movies that they have worked on. Write a query that will give you all
the Pixar movies and sort them by descending revenue. The result will be
given to you as a JSON (parsed to a list by {httr}).

Convert this list to a tibble so you have one row per film and one
column per interesting piece of information. If you want to get the
other pages, you need to play with the page parameter in your url.

I used a GET/discover/movie/with\_companies request and I saw that there
are six pages of results. Below I create a request for each of the 6
pages.

``` r
mov_pix1<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=1")

mov_pix2<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=2")

mov_pix3<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=3")

mov_pix4<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=4")

mov_pix5<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=5")

mov_pix6<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=3&sort_by=revenue.desc&page=6")
```

Below I convert the extracted lists into tibbles, then I used
bind\_rows() to join them up into a single tibble with 108 rows.

``` r
pix1<- content(mov_pix1, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date")) %>% 
  map(flatten_df) %>% 
  bind_rows()

pix2 <- content(mov_pix2, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date")) %>% 
  map(flatten_df) %>% 
  bind_rows()

pix3 <- content(mov_pix3, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date")) %>% 
  map(flatten_df) %>% 
  bind_rows()

pix4 <- content(mov_pix4, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date")) %>% 
  map(flatten_df) %>% 
  bind_rows()

pix5 <- content(mov_pix5, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date"))%>%
  map(flatten_df) %>% 
  bind_rows()

pix6 <- content(mov_pix6, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average", "release_date")) %>% 
  map(flatten_df) %>% 
  bind_rows()

pix_all <- bind_rows(pix1, pix2, pix3, pix4, pix5, pix6) %>% 
  arrange(desc(vote_count)) %>% 
  mutate(company="Pixar")

glimpse(pix_all)
```

    ## Observations: 108
    ## Variables: 7
    ## $ id             <int> 150540, 14160, 12, 10681, 585, 9806, 862, 35491...
    ## $ original_title <chr> "Inside Out", "Up", "Finding Nemo", "WALL·E", "...
    ## $ popularity     <dbl> 32.518, 18.046, 24.666, 21.498, 20.515, 20.154,...
    ## $ vote_count     <int> 14003, 13497, 12805, 11922, 11890, 11686, 11575...
    ## $ vote_average   <dbl> 7.9, 7.9, 7.8, 8.0, 7.8, 7.7, 7.9, 8.2, 7.7, 7....
    ## $ release_date   <chr> "2015-06-09", "2009-05-28", "2003-05-30", "2008...
    ## $ company        <chr> "Pixar", "Pixar", "Pixar", "Pixar", "Pixar", "P...

## Part 4.

You may know that Pixar was acquired by Disney in 2006, after they had
already been collaborating on films for more than a decade. For the last
part of the report, we are going to look into whether this was a smart
strategic decision by Disney, by comparing the popularity of both Disney
and Pixar films that came out from 2006 onwards.

    *First, acquire the "id" for Disney using the search endpoint. Note that if you try to find the company ID for Disney, there will be more than one result (Disney has many subsidiaries with the name "Disney" in it). For this exercise, specifically look for "Walt Disney Pictures" in the USA.

``` r
mov_disney <- GET("https://api.themoviedb.org/3/search/company?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&query=Disney&page=1")

content(mov_disney, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "name", "origin_country")) %>% 
  map(flatten_df) %>%
  bind_rows() %>% 
  filter(name=="Walt Disney Pictures")
```

    ## # A tibble: 1 x 3
    ##      id name                 origin_country
    ##   <int> <chr>                <chr>         
    ## 1     2 Walt Disney Pictures US

    I find that the id for the Walt Disney Pictures is 2.
    
    
    *Second, get the vote averages and vote counts for films from Walt Disney Productions and from Pixar using the discover/movies endpoint. Use the API documentation to find out how to get films from 2006 onwards.
    
    There are 34 pages of films from Disney, so I need to either automate the data collection from pages, or limit the search to films released after 2006. I do this by using primary_release_date.gte and .lte. Now down to 8 pages. That's better :-) 

``` r
mov_dis1<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=1")

mov_dis2<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=2")

mov_dis3<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=3")

mov_dis4<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=4")

mov_dis5<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=5")

mov_dis6<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=6")

mov_dis7<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=7")

mov_dis8<- GET("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page=8")
```

Now collecting the data and compiling into tibble for Disney, like
before for Pixar.

``` r
dis1<- content(mov_dis1, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis2 <- content(mov_dis2, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis3 <- content(mov_dis3, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis4 <- content(mov_dis4, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis5 <- content(mov_dis5, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis6 <- content(mov_dis6, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis7 <- content(mov_dis7, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis8 <- content(mov_dis8, as="parsed") %>% 
  pluck("results") %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows()

dis_all <- bind_rows(dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis8) %>%  
  arrange(desc(vote_count)) %>% 
  mutate(company="Disney")

glimpse(dis_all)
```

    ## Observations: 149
    ## Variables: 6
    ## $ id             <int> 284054, 150540, 284053, 10681, 321612, 76338, 1...
    ## $ original_title <chr> "Black Panther", "Inside Out", "Thor: Ragnarok"...
    ## $ popularity     <dbl> 28.991, 32.518, 35.014, 21.498, 27.003, 26.955,...
    ## $ vote_count     <int> 14131, 14003, 12946, 11922, 11837, 11223, 11059...
    ## $ vote_average   <dbl> 7.4, 7.9, 7.5, 8.0, 6.9, 6.6, 7.8, 7.3, 7.7, 7....
    ## $ company        <chr> "Disney", "Disney", "Disney", "Disney", "Disney...

Now combining post 2006 Pixar and Disney data together and limiting to
vote\_count of at least 50.

A suggestion here would be to filter the data by including only films
with a vote\_count of at least 50. Consider that if only a few people
voted on the film, the vote average will not be as representative as
when lots of people have voted on the film.

``` r
pix_dis<- pix_all %>% 
  filter(release_date>"2005.12.31") %>% 
  select(-release_date) %>% 
  bind_rows(dis_all) %>% 
  filter(vote_count>=50)

glimpse(pix_dis)
```

    ## Observations: 150
    ## Variables: 6
    ## $ id             <int> 150540, 14160, 10681, 354912, 2062, 10193, 6217...
    ## $ original_title <chr> "Inside Out", "Up", "WALL·E", "Coco", "Ratatoui...
    ## $ popularity     <dbl> 32.518, 18.046, 21.498, 24.122, 19.816, 26.248,...
    ## $ vote_count     <int> 14003, 13497, 11922, 10174, 10113, 9543, 8955, ...
    ## $ vote_average   <dbl> 7.9, 7.9, 8.0, 8.2, 7.7, 7.8, 6.9, 6.7, 7.0, 7....
    ## $ company        <chr> "Pixar", "Pixar", "Pixar", "Pixar", "Pixar", "P...

\*Now, compare the vote averages using boxplots and a t-test, with the
aim of answering the question Are the films from Pixar on average more
popular than the films from Walt Disney Pictures?

First the boxplot

``` r
pix_dis %>% 
  ggplot(mapping = aes(x=company, y=vote_average))+
  geom_boxplot()+
  labs(title = "Distribution of average votes by company",
       subtitle= "films released in 2006 or earlier, which received at least 50 votes",
       x="company",
       y="average number of votes")
```

![](Final-project-report-3_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The boxplot suggests that indeed there is a significant difference
between the two companies by average number of votes. The median from
Pixar falls outside the inter-quartile range for Disney.

Now doing the t-test.

``` r
pix_dis %>% 
  mutate(company=factor(company, levels=c("Disney", "Pixar"))) %>% 
  t_test(vote_average ~ company, order = c("Disney", "Pixar"), var.equal=FALSE)
```

    ## # A tibble: 1 x 6
    ##   statistic  t_df    p_value alternative lower_ci upper_ci
    ##       <dbl> <dbl>      <dbl> <chr>          <dbl>    <dbl>
    ## 1     -4.72  81.6 0.00000983 two.sided     -0.885   -0.360

The t-test confirms that Pixar movies are on average more popular than
Disney films.

Below I automate the collection of data for all the 8 pages at the same
time for Disney movies. This can be used instead of the long way in
parts 3 and 4 above but I prefer to keep both versions of the code. All
of the others steps are exactly the same.

``` r
vec8 <- seq(from=1, to=8)

mov_dis<- str_glue("https://api.themoviedb.org/3/discover/movie?api_key=32dcb877caa66aa7c0cf44a3fbf215ce&with_companies=2&primary_release_date.gte=2006-01-01&primary_release_date.lte=2020-01-22&page={vec8}") %>% 
  map(httr::GET) %>% 
  map(httr::content, as="parsed") %>% 
  map(purrr::pluck, "results")

disney_tb<- mov_dis %>% 
  flatten() %>% 
  map(magrittr::extract, c("id", "original_title" ,"popularity", "vote_count", "vote_average")) %>% 
  map(flatten_df) %>% 
  bind_rows() %>% 
  mutate(company="Disney")

glimpse(disney_tb)
```

    ## Observations: 149
    ## Variables: 6
    ## $ id             <int> 181812, 420809, 330457, 920, 109445, 102651, 42...
    ## $ original_title <chr> "Star Wars: The Rise of Skywalker", "Maleficent...
    ## $ popularity     <dbl> 82.933, 74.733, 73.723, 65.046, 34.781, 29.621,...
    ## $ vote_count     <int> 2756, 1660, 2046, 8634, 11026, 9232, 4732, 1105...
    ## $ vote_average   <dbl> 6.5, 7.2, 7.0, 6.7, 7.3, 7.1, 7.0, 7.8, 6.7, 7....
    ## $ company        <chr> "Disney", "Disney", "Disney", "Disney", "Disney...
