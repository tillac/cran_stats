# Analysis of CRAN packages

This repo aims to analyse some caracteristics of CRAN package.

---> only scrap views pages
---> cut cranlogs calls

## Data collecting

The data collecting part was inspired by this question on [SO](https://stackoverflow.com/questions/11560865/list-and-description-of-all-packages-in-cran-from-within-r).

We collect the datas :

+ through `tools::CRAN_package_db()` to get the descriptions ;
+ through scraping from CRAN, with `polite` and `rvest` for the views;
+ through `cranlogs` to get the download stats.

Some inspiration for the cleaning come from the `grapher` package (and this [function](https://github.com/JohnCoene/grapher/blob/master/R/generate.R)). 

## Some possible subjects

+ by how many people and by who are packages built and maintained ?
+ same for organizations ?
+ analysis of collaboration on packages through author list ;
+ identify "core" packages (the one that people download themselves) and underlaying packages (the one downloaded by packages) ;
+ GIF of the growing of CRAN ;
+ do packages have nationality ?
+ predict the topic of packages with depends and description and compare it to in views subject .
