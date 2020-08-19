# Analysis of CRAN packages

This repo aims to analyse some caracteristics of CRAN package.

## Data collecting

We collect the datas :

+ through scraping from CRAN, with `polite` and `rvest` ;
+ through `cranlogs` to get the download stats.


## Some possible subjects

+ by how many people and by who are packages built and maintained ?
+ same for organizations ?
+ analysis of collaboration on packages through author list ;
+ identify "core" packages (the one that people download themselves) and underlaying packages (the one downloaded by packages) ;
+ GIF of the growing of CRAN ;
+ do packages have nationality ?
+ predict the topic of packages with depends and description and compare it to in views subject .
