# for wikipedia pages for all US counties,
#  find the urls that these pages point to.
#  create the adjacency matrix.
#  saved in file data/wikiCountyGraph.RData

install.packages("rvest")
library(rvest)


url <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
cnty <- url %>%  # help from https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%  # in chrome, inspect elements, highlight table, copy xpath
  html_table()

cnty= cnty[[1]]
str(cnty)


library(stringr)

fun = function(x){  
  # given a line of cnty, it extracts the url for a wiki page of a county
  # stolen from http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r
  url = paste("https://en.wikipedia.org/wiki/", 
              sub(" ", "_", x[2]),",_", sub(" ", "_", x[3]), sep="")  # the page url's are nicely structured!
  lines = try(readLines(url), silent = T)
  if(length(lines) ==0) return(c())
  html <- paste(lines, collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  return(links)
}

fun(cnty[1,])  # test case

tmp = cnty[sample(nrow(cnty), 30),]
a = apply(tmp[1:4,], 1, fun)
# a = apply(cnty, 1, fun)

j =(unlist(a))
fj = factor(j)
p = c(0,cumsum(unlist(lapply(a, length))))

A = sparseMatrix(j = as.numeric(fj), p = p)
dim(A)
colnames(A) = levels(fj)
rownames(A) = cnty[,1]

save(A, file = "data/wikiCountyGraph.RData")


