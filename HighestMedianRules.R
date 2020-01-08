# package <- function(p) { 
#   if (!is.element(p, installed.packages()[,1])) {
#   install.packages(p); 
#   }
#   library(p, character.only = TRUE)
# } # loads packages with automatical install if needed
# 
# package("devtools")
# # devtools::install_github("klutometis/roxygen")
# # library(roxygen2)
# package("roxygen2")
# 
# create("HighestMedianRules")
# setwd('./HighestMedianRules')

# rm(list=ls())

##### Rules #####

#' Gauge
#' 
#' This function returns a custom description of the grades of a candidate in terms of the shares of proponents (p), opponents (q), median grades (g), and the gauge (i.e. the median grade followed by + or -, a + corresponding to a larger share of proponents than opponents).
#' @param grades A numeric vector containing the shares of each grades of a candidate, from the lowest grade to the highest.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param return A string containing the information to return. Default to 'qp' (shares of opponents and proponents). Possible values: 'g', 'p', 'q', 'pq', 'qp', 'qpg', 'pqg', 'gpq', 'gqp', 'text' (i.e. the gauge), 'all'. If the string does not match one of the above, 'all' is returned.
#' @export
#' @examples gauge(elec2012['Hollande',], return = 'gqp')
gauge <- function(grades, k = 0.5, scale = c(), return = 'qp') {
  if (length(scale) != length(grades)) scale <- c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2)))
  majo <- 0
  i <- 0
  while (majo < k) { 
    i <- i + 1
    majo <- majo + grades[i]  }
  p <- 1 - majo
  q <- majo - grades[i]
  g <- i+min(scale)-1
  if (g > min(scale) & g < max(scale)) {
    if (p*k > q*(1-k)) text = paste(g, "+ ", sep="") 
    else  text = paste(g, "- ", sep="")  }
  else text = paste(g," ")
  if (return=='g') return(g) 
  else if (return=='p') return(p)
  else if (return=='q') return(q)
  else if (return=='pq') return(c(p=p, q=q))
  else if (return=='qp') return(c(q=q, p=p))
  else if (return=='gpq') return(c(g=g, p=p, q=q))
  else if (return=='gqp') return(c(g=g, q=q, p=p))
  else if (return=='pgq') return(c(p=p, g=g, q=q))
  else if (return=='qgp') return(c(q=q, g=g, p=p))
  else if (return=='text') return(text=text)
  else if (return=='all') return(c(g=g, p=p, q=q, text=text))
  else return(c(g, p, q, text))
}

#' Gauges
#' 
#' Aggregator of 'gauge': returns the gauge of each row (i.e. candidate) from a matrix (i.e. the voting profile). See function 'gauge'.
#' @examples gauges(grades = elec2012, return = 'gqp)
gauges <- function(grades, k = 0.5, scale = c(), return = 'qp') return(apply(grades, 1, function(x) return(gauge(x, k, scale, return))))

#' Score
#' 
#' This function returns the score of a candidate, using a custom voting rule.
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param grades A numeric vector containing the shares of each grades of a candidate, from the lowest grade to the highest.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param name Text to be printed along the gauge in case print = TRUE and return_text = FALSE. Defaults to "".
#' @param print Prints the gauge and the argument 'name'. Default to TRUE.
#' @param return_text Prints the gauge. Defaults to FALSE.
#' @export
#' @examples score(rule='d', elec2012['Hollande',], scale=-2:4, name="Hollande")
score <- function(rule="mj", grades=elec2012['Hollande',], k = 0.5, scale=c(), name="", print = T, return_text = FALSE) {
  grades <- grades / sum(grades)
  if (length(scale) != length(grades)) scale <- c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2)))
  g <- gauge(grades, k, scale, 'g')
  p <- gauge(grades, k, scale, 'p')
  q <- gauge(grades, k, scale, 'q')
  text <- gauge(grades, k, scale, 'text')
  if (print & !return_text) print(paste(text, name, sep=""))
  if (return_text) return(text)
  else 
    if (rule=="MJ" | rule=="mj" | rule=="$mj$") return(as.numeric(g + (p>q)*p - (p<=q)*q))
    else if (rule=='s' | rule=='S' | rule=='sigma' | rule=="$\\sigma$" | rule=="$s$") return(as.numeric(g + 0.5*((p-q)/(p+q+3*10^(-8))) + 0.5*((g==max(scale))*(0.5-q) + (g==min(scale))*(p-0.5)) )) # the last two terms are for the ultimate tie-breaking rule
    else if (rule=="D" | rule=="d" | rule=="delta" | rule=="Delta" | rule=="$\\Delta$" | rule=="$d$") return(as.numeric(g + p - q))
    else if (rule=="N" | rule=="n" | rule=="nu" | rule=="$\\nu$" | rule=="$n$") {
      return(g + (p-q)/(2*(1-p-q))) }
    else if (rule=="mean" | rule=="average") return(as.numeric(grades%*%scale))
    else print("rule must be: mj, d, s, n or mean.")
}

#' Aggregate scores
#' 
#' Aggregator of scores: returns a vector with the score of each row (i.e. candidate) in a matrix (i.e. the voting profile). See function 'score'.
#' @param grades A voting profile, i.e. a matrix with the shares of grades of each candidate on each row, from the lowest grade to the highest.
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param names String vector, each string to be printed along the gauges in case print = TRUE and return_text = FALSE. Defaults to "".
#' @param print Prints the gauges and the argument 'names'. Default to TRUE.
#' @param return_text Prints the gauges. Defaults to FALSE.
#' @param rounds Number of rounding digits. Default to 3.
#' @export
#' @examples aggregate_scores(elec2012, rule='d', scale=-2:4, names=candidats_2012)

aggregate_scores <- function(grades, rule='mj', k = 0.5, scale=c(), names = c(), print = FALSE, return_text = FALSE, rounds=3) {
  # res <- matrix(ncol = length(names), nrow = 4)
  if (!is.matrix(grades)) grades <- matrix(grades, nrow = 1)
  if (any(grades < 0)) print('WARNING: some shares of grades are negative!')
  if (nrow(grades)!=length(names)) names <- c(1:length(nrow(grades)))
  if (ncol(grades)!=length(scale)) scale <- c((floor(-ncol(grades)/2)+1):(ncol(grades)+floor(-ncol(grades)/2)))
  res <- c()
  # for (i in 1:ncol(grades)) res[,i] <- gauge(grades[,i], k, names[i])
  for (i in 1:nrow(grades)) {
    if (rule=="MJ" | rule=="mj" | rule=="$mj$") res <- c(res, score(rule='mj', grades[i,], k, names[i], print, return_text, scale))
    else if (rule=='s' | rule=='S' | rule=='sigma' | rule=="$\\sigma$" | rule=="$s$") res <- c(res, score(rule='s', grades[i,], k, names[i], print, return_text, scale))
    else if (rule=="D" | rule=="d" | rule=="delta" | rule=="Delta" | rule=="$\\Delta$" | rule=="$d$") res <- c(res, score(rule='d', grades[i,], k, names[i], print, return_text, scale))
    else if (rule=='mean' | rule=='average') res <- c(res, score(rule='mean', grades[i,], k, names[i], print, return_text, scale))
    else if (rule=="N" | rule=="n" | rule=="nu" | rule=="$\\nu$" | rule=="$n$") res <- c(res, score(rule='n', grades[i,], k, names[i], print, return_text, scale))
  }
  if (return_text) return(res)
  else if (rounds) return(round(as.numeric(res), rounds))
  else return(as.numeric(res))
}

#' Ranking
#' 
#' Returns a matrix with the scores of candidates sorted in decreasing order, for a given voting rule.
#' @param grades A voting profile, i.e. a matrix with the shares of grades of each candidate on each row, from the lowest grade to the highest.
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, 2020, Social Choice & Welfare.
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param names String vector, each string to be printed in case print = TRUE. Defaults to c().
#' @param print Prints the argument 'names'. Default to FALSE
#' @export
#' @examples ranking(elec2012, rule='d', scale=-2:4, names=candidats_2012)

ranking <- function(grades, rule='mj', k = 0.5, scale=c(), names = c(), print = FALSE) {
  res <- matrix(nrow = nrow(grades), ncol = 3)
  scores <- aggregate_scores(rule=rule, grades, k, names, print, scale=scale)
  scores_text <- aggregate_scores(rule=rule, grades, k, names, print, return_text = TRUE, scale=scale)
  res[,1] <- names[order(scores, decreasing = TRUE)]
  res[,2] <- scores_text[order(scores, decreasing = TRUE)]
  res[,3] <- sort(scores, decreasing = TRUE)
  return(res)
}

#' Rankings
#' 
#' Returns a matrix with the scores of candidates sorted in decreasing order of score mj, for the five following voting rules (with k=0.5): mj, d, s, n, mean. See function 'score' for more details.
#' @param grades A voting profile, i.e. a matrix with the shares of grades of each candidate on each row, from the lowest grade to the highest.
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param names String vector, each string to be printed in case print = TRUE. Defaults to c().
#' @param return_distances If TRUE, returns the Kendall distance between the rules (using AllKendall) instead of the matrix of scores. Default to FALSE.
#' @param rounds If TRUE, rounds the scores (to 3 digits for highest median rules and 2 digits for range voting). Default to TRUE.
#' @export
#' @examples ranking(elec2012, rule='d', scale=-2:4, names=candidats_2012)

rankings <- function(grades, scale=c(), names = c(), return_distance=FALSE, rounds=T) {
  scores <- aggregate_scores(rule='mj', grades=grades, names=names, scale=scale, rounds = 3*rounds)
  scores_text <- aggregate_scores(rule='mj', grades=grades, names=names, return_text = TRUE, scale=scale, rounds = 3*rounds)
  res <- data.frame(matrix(nrow = nrow(grades), ncol = 7))
  res[,1] <- names[order(scores, decreasing = TRUE)]
  res[,2] <- aggregate_scores(rule='mean', grades=grades, names=names, scale=scale, rounds = 2*rounds)[order(scores, decreasing = TRUE)]
  res[,3] <- scores_text[order(scores, decreasing = TRUE)]
  res[,4] <- sort(scores, decreasing = TRUE)
  res[,5] <- aggregate_scores(rule='d', grades=grades, names=names, scale=scale, rounds = 3*rounds)[order(scores, decreasing = TRUE)]
  res[,6] <- aggregate_scores(rule='s', grades=grades, names=names, scale=scale, rounds = 3*rounds)[order(scores, decreasing = TRUE)]
  res[,7] <- aggregate_scores(rule='n', grades=grades, names=names, scale=scale, rounds = 3*rounds)[order(scores, decreasing = TRUE)]
  colnames(res) <- c('choices', 'mean', 'MG', '$mj$', '$d$', '$s$', '$n$') # c('choices', 'mean', 'MG', '$mj$', '$\\Delta$', '$\\sigma$', '$\\nu$') c('choices', 'mean', 'MG', 'MJ', 's', 'D', 'n')
  all_orders <- do.call("rbind", lapply(list(aggregate_scores(rule='mean', grades=grades, names=names, scale=scale, rounds=F), aggregate_scores(grades=grades, names=names, scale=scale, rounds=F), 
          aggregate_scores(rule='d', grades=grades, names=names, scale=scale, rounds=F), aggregate_scores(rule='s', grades=grades, names=names, scale=scale, rounds=F), 
          aggregate_scores(rule='n', grades=grades, names=names, scale=scale, rounds=F)), rank))
  distances <- AllKendall(all_orders, all_orders)
  colnames(distances) <- rownames(distances) <- c('mean', '$mj$', '$d$', '$s$', '$n$') # c('mean', '$mj$', '$\\Delta$', '$\\sigma$', '$\\nu$') c('mj', 's', 'd', 'mean', 'n') 
  if (return_distance) return(distances)
  else return(res)  
}

# document()
# setwd('..')
# install("HighestMedianRules")
