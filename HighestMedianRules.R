##### Rules #####

#' Gauge
#'
#' This function returns a custom description of the grades of a candidate in terms of the shares of proponents (p), opponents (q), median grades (g), and the gauge (i.e. the median grade followed by + or -, a + corresponding to a larger share of proponents than opponents).
#' @param grades A numeric vector containing the shares of each grades of a candidate, from the lowest grade to the highest.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
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
  p <- as.numeric(1 - majo)
  q <- as.numeric(majo - grades[i])
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
#' @param grades A numeric vector containing the shares of each grades of a candidate, from the lowest grade to the highest.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param return A string containing the information to return. Default to 'qp' (shares of opponents and proponents). Possible values: 'g', 'p', 'q', 'pq', 'qp', 'qpg', 'pqg', 'gpq', 'gqp', 'text' (i.e. the gauge), 'all'. If the string does not match one of the above, 'all' is returned.
#' @export
#' @examples gauges(grades = elec2012, return = 'gqp')
gauges <- function(grades, k = 0.5, scale = c(), return = 'qp') return(apply(grades, 1, function(x) return(gauge(x, k, scale, return))))

#' Score
#'
#' This function returns the score of a candidate, using a custom voting rule.
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare  (forthcoming).
#' @param grades A numeric vector containing the shares of each grades of a candidate, from the lowest grade to the highest.
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param name Text to be printed along the gauge in case print = TRUE and return_text = FALSE. Defaults to "".
#' @param print Prints the gauge and the argument 'name'. Default to TRUE.
#' @param return_text Prints the gauge. Defaults to FALSE.
#' @export
#' @examples score(rule='d', elec2012['Hollande',], scale=-2:4, name="Hollande")
score <- function(rule="mj", grades=elec2012['Hollande',], k = 0.5, scale=c(), name="", print = T, return_text = FALSE) {
  if (any(grades < 0)) print('WARNING: some shares of grades are negative!')
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
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
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
    if (rule=="MJ" | rule=="mj" | rule=="$mj$") res <- c(res, score(rule='mj', grades[i,], k, scale, names[i], print, return_text))
    else if (rule=='s' | rule=='S' | rule=='sigma' | rule=="$\\sigma$" | rule=="$s$") res <- c(res, score(rule='s', grades[i,], k, scale, names[i], print, return_text))
    else if (rule=="D" | rule=="d" | rule=="delta" | rule=="Delta" | rule=="$\\Delta$" | rule=="$d$") res <- c(res, score(rule='d', grades[i,], k, scale, names[i], print, return_text))
    else if (rule=='mean' | rule=='average') res <- c(res, score(rule='mean', grades[i,], k, scale, names[i], print, return_text))
    else if (rule=="N" | rule=="n" | rule=="nu" | rule=="$\\nu$" | rule=="$n$") res <- c(res, score(rule='n', grades[i,], k, scale, names[i], print, return_text))
  }
  if (return_text) return(res)
  else if (rounds) return(round(as.numeric(res), rounds))
  else return(as.numeric(res))
}

#' Ranking
#'
#' Returns a matrix with the scores of candidates sorted in decreasing order, for a given voting rule.
#' @param grades A voting profile, i.e. a matrix with the shares of grades of each candidate on each row, from the lowest grade to the highest.
#' @param rule The voting rule to be used. Default to 'mj'. Possible values: 'mj' (majority judgment), 'd' (difference), 's' (relative share), 'n' (normalized difference), 'mean' (range voting). For more details, see "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
#' @param k The quantile used to compute the gauge. Default to 0.5 (the median). For more details, see paragraph Extensions in 3.2.1 of "Tie-Breaking the Highest Median", Fabre, Social Choice & Welfare (forthcoming).
#' @param scale A numeric vector containing the values of the scale of grades. Default to c((floor(-length(grades)/2)+1):(length(grades)+floor(-length(grades)/2))).
#' @param names String vector, each string to be printed in case print = TRUE. Defaults to c().
#' @param print Prints the argument 'names'. Default to FALSE
#' @export
#' @examples ranking(elec2012, rule='d', scale=-2:4, names=candidats_2012)

ranking <- function(grades, rule='mj', k = 0.5, scale=c(), names = c(), print = FALSE) {
  res <- matrix(nrow = nrow(grades), ncol = 3)
  scores <- aggregate_scores(rule=rule, grades, k, scale=scale, names, print)
  scores_text <- aggregate_scores(rule=rule, grades, k, scale=scale, names, print, return_text = TRUE)
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
#' @param return_distance If TRUE, returns the Kendall distance between the rules (using AllKendall) instead of the matrix of scores. Default to FALSE.
#' @param rounds If TRUE, rounds the scores (to 3 digits for highest median rules and 2 digits for range voting). Default to TRUE.
#' @importFrom RMallow AllKendall
#' @export
#' @examples rankings(elec2012, scale=-2:4, names=candidats_2012)

rankings <- function(grades, scale=c(), names = c(), return_distance=FALSE, rounds=T) {
  scores <- aggregate_scores(rule='mj', grades=grades, scale=scale, names=names, rounds = 3*rounds)
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
  distances <- RMallow::AllKendall(all_orders, all_orders)
  colnames(distances) <- rownames(distances) <- c('mean', '$mj$', '$d$', '$s$', '$n$') # c('mean', '$mj$', '$\\Delta$', '$\\sigma$', '$\\nu$') c('mj', 's', 'd', 'mean', 'n')
  if (return_distance) return(distances)
  else return(res)
}


##### Data #####
#' Example voting profile: matrix containing the shares of grades (in -2:4) of candidates on each row.
elec2012 <- matrix(0,nrow=10, ncol=7) # Balinski & Laraki (2016) p. 14, 737 voters
#' Names of candidates of the example voting profile 'elec2012'
candidats_2012 <- c("Hollande", "Bayrou", "Sarkozy", "Melenchon", "Dupont-Aignan", "Joly", "Poutou", "Le Pen", "Arthaud", "Cheminade")
row.names(elec2012) <- candidats_2012
colnames(elec2012) <- c("To reject", "Poor", "Fair", "Good", "Very Good", "Excellent", "Outstanding")
elec2012['Hollande',] <- c(0.1424, 0.1425, 0.1479, 0.1167, 0.1642, 0.1615, 0.1248)
elec2012['Bayrou',] <- c(0.0869, 0.1194, 0.2008, 0.2524, 0.2171, 0.0977, 0.0258)
elec2012['Sarkozy',] <- c(0.3175, 0.0787,0.1113, 0.1099, 0.1628, 0.1235, 0.0963)
elec2012['Melenchon',] <- c(0.2537, 0.1506, 0.171, 0.1465, 0.129, 0.095, 0.0543)
elec2012['Dupont-Aignan',] <- c(0.3392, 0.2551, 0.2022, 0.1126, 0.0597, 0.0258, 0.0054)
elec2012['Joly',] <- c(0.3853, 0.2469, 0.1465, 0.118, 0.0651, 0.0299, 0.0081)
elec2012['Poutou',] <- c(0.4573, 0.2809, 0.1248, 0.0773, 0.0448, 0.0136, 0.0014)
elec2012['Le Pen',] <- c(0.4763, 0.0624, 0.1398, 0.0936, 0.095, 0.0733, 0.0597)
elec2012['Arthaud',] <- c(0.4993, 0.2524, 0.1316, 0.0651, 0.038, 0.0136, 0.0)
elec2012['Cheminade',] <- c(0.5197, 0.2687, 0.1167, 0.0583, 0.0244, 0.0081, 0.0041)
rowSums(elec2012)

