## Replication code for "Tie-breaking the Highest Median: Alternatives to the Majority Judgment"
#     by Adrien Fabre (2019), under licence CC-BY
# All files can be found on github: https://github.com/bixiou/highest_median TODO: put on github
#     in particular, file to load the packages, functions and data (packages_functions_data.R),
#     R environment with all the data prepared (.RData) and python notebook for complementary computations

source("packages_functions_data.R")
# One should also load .RData to avoid lenghty computations and retrieve the exact same results in the random draws.

##### 1. Introduction #####
# Figure 1: Graphical example. This plot is reworked on images/new_tie_breaking_inv.pptx using also images/tie-breaking.svg
barres(file="new_tie_breaking_RdYlGn", color = color(7, theme='RdYlGn', rev_color = T)[c(2,4,7)], thin=T, data=new_example_data, nsp=F, sort=FALSE, legend=c(-1,0,1),labels=c("MJ winner",  "nu winner", "Delta winner", "s winner"))
# Table 1 is computed mentally


##### 3. Different tie-breaking rules ######
# Figure 2 is plotted using Python:, cf. 'ternary plot.ipynb'


##### 5.2 Sensitivity to small fluctuations #####
# Figure 3: score of each rule
share_1 <- .2
share2 <- .1
max_p <- 1-share_1-share2
steps <- 1000
xs <- seq(1/steps, max_p, 1/steps)
old_mar <- par()$mar
par(mar=c(3.1, 3.1, 0.1, 0.1))
example_grades <- cbind(share_1, max_p-xs, xs, share2) 
plot(xs, aggregate_scores('n', example_grades), type='l', col='green', lwd=2, lty=1, xlim=c(0,max_p), ylim=c(-0.3,1), ylab='', xlab='') + grid() # , xlab=expression(italic(p)), ylab='Score', 
lines(xs[1:steps*(.5-share2)], aggregate_scores('s', example_grades[1:steps*(.5-share2),]), type='l', col='blue', lwd=2, lty=3)
lines(xs[1:steps*(.5-share2)], aggregate_scores('d', example_grades[1:steps*(.5-share2),]), type='l', col='red', lwd=2, lty=2)
lines(xs[(steps*(share_1-share2)+1):(steps*max_p)], aggregate_scores('mj', example_grades[(steps*(share_1-share2)+1):(steps*max_p),]), type='l', col='black', lwd=2, lty=1)
lines(xs[1:(steps*(share_1-share2))], aggregate_scores('mj', example_grades[1:(steps*(share_1-share2)),]), type='l', col='black', lwd=2, lty=1)
lines(xs[(steps*(.5-share2)+1):(steps*max_p)], aggregate_scores('d', example_grades[(steps*(.5-share2)+1):(steps*max_p),]), type='l', col='red', lwd=2, lty=2, ylim=c(-0.5,max_p), ylab='', xlab='') + grid() # , xlab=expression(italic(p)), ylab='Score', 
lines(xs[(steps*(.5-share2)+1):(steps*max_p)], aggregate_scores('s', example_grades[(steps*(.5-share2)+1):(steps*max_p),]), type='l', col='blue', lwd=2, lty=3)
legend('bottomright', col=c('red', 'blue', 'green', 'black'), lwd=2, lty=c(2,3,1,1), legend=c(expression(italic(d)), expression(italic(s)), expression(italic(n)), expression(italic(mj))), title='Score')
mtext(text = expression(paste(italic(x),  ': share of grades +1')), side = 1, line = 2.2) + mtext(text = 'Score', side = 2, line = 2.2)
par(mar=old_mar) #  (given 10% of -1, 10% of +2, x of +1 and the rest of 0)

# # old figure: more readable but less easy to explain
# max_p <- 0.7
# ps <- seq(0, max_p, 0.01)
# # qs <- rep(0.1, max_p*100+1)
# old_mar <- par()$mar
# par(mar=c(3.1, 3.1, 0.1, 0.1))
# example_grades <- cbind(0.1, 1-ps-0.1, pmax(ps-0.1, 0), pmin(0.1, ps)) # all equivalent: cbind(qs, 1-ps-qs, ps - 0.1*(ps>=0.1), 0.1*(ps>=0.1)) old: cbind(qs, 1-ps-qs, ps-0.1, 0.1) # /!\ check also avant-dernière line before legend
# plot(ps[1:51], aggregate_scores('d', example_grades[1:51,]), type='l', col='red', lwd=2, lty=2, xlim=c(0,max_p), ylim=c(-0.5,0.8), ylab='', xlab='') + grid() # , xlab=expression(italic(p)), ylab='Score', 
# lines(ps[1:51], aggregate_scores('s', example_grades[1:51,]), type='l', col='blue', lwd=2, lty=3)
# lines(ps, aggregate_scores('n', example_grades), type='l', col='green', lwd=2, lty=1)
# lines(c(0.101, ps[12:(max_p*100+1)]), aggregate_scores('mj', rbind(c(0.1, 0.799, 0.001, 0.1), example_grades[12:(max_p*100+1),])), type='l', col='black', lwd=2, lty=1)
# lines(ps[1:11], aggregate_scores('mj', example_grades[1:11,]), type='l', col='black', lwd=2, lty=1)
# lines(ps[51:(max_p*100+1)]+0.001, aggregate_scores('d', cbind(.1, 1-ps-0.001-.1, pmax(ps+0.001-0.1, 0), pmin(0.1, ps))[51:(max_p*100+1),]), type='l', col='red', lwd=2, lty=2, ylim=c(-0.5,max_p), ylab='', xlab='') + grid() # , xlab=expression(italic(p)), ylab='Score', 
# lines(c(0.501, ps[52:(max_p*100+1)]), aggregate_scores('s', rbind(c(0.1, 0.399, 0.401, 0.1), example_grades[52:(max_p*100+1),])), type='l', col='blue', lwd=2, lty=3)
# legend('bottomright', col=c('red', 'blue', 'green', 'black'), lwd=2, lty=c(2,3,1,1), legend=c(expression(italic(d)), expression(italic(s)), expression(italic(n)), expression(italic(mj))), title='Score')
# mtext(text = expression(paste(italic(x),  ': share of grades > 0')), side = 1, line = 2.2) + mtext(text = 'Score', side = 2, line = 2.2)
# par(mar=old_mar) # 


# Quantile shifts
start <- Sys.time() # 2h
n <- 100000
min_group <- 0 
max_group <- 0.5
p1s <- runif(n, min_group, max_group)
q1s <- runif(n, min_group, max_group)
p2s <- runif(n, min_group, max_group)
q2s <- runif(n, min_group, max_group)

# distribution of scores for each rule. Then average percentile change in distribution triggered by random reallocation of 2% of votes.
scores <- list()
for (rule in rules) scores[[rule]] <- aggregate_scores(rule = rule, grades = data.frame(list(q=q1s, r=1-p1s-q1s, p=p1s)), rounds=FALSE)
# plot_rules(scores, "cdf", xlab="Score (x)", ylab="Share with score < x")

# Random reallocation of 2% of grades: dp+dq=reallocation, sgn(dp) and sgn(dq) are independent and equiprobable -/+.
reallocation <- 0.02
dps <- runif(n, 0, reallocation)
dqs <- reallocation - dps
P1s <- p1s + sign(round(1e4 * dps) %% 2 - 0.5) * dps
Q1s <- q1s + sign(round(1e6 * dqs) %% 2 - 0.5) * dqs 
distribution_scores <- list()
for (rule in rules) distribution_scores[[rule]] <- ecdf(scores[[rule]])
# /!\ Run the above only once (do not re-run for the robustness check, do not re-run if you want to obtain exact same results as in paper)

new_scores <- list()
for (rule in rules) new_scores[[rule]] <- aggregate_scores(rule = rule, grades = data.frame(list(q=Q1s, r=1-P1s-Q1s, p=P1s)), rounds=FALSE)
rank_shift <- list()
for (rule in rules) rank_shift[[rule]] <- abs(distribution_scores[[rule]](new_scores[[rule]]) - distribution_scores[[rule]](scores[[rule]]))
Sys.time() - start

# Figure 4: CDF of quantile shifts following a random reallocation of 2% of grades
plot_rules(rank_shift, "cdf", xlab="Quantile shift (r)", ylab="Share with quantile shift < r", ylim=c(0.96,1), xlim=c(0.1, 1))

# Table 4: probabilities of large and very large shifts
length(which(rank_shift$D > 0.2))/n # 0.00796
length(which(rank_shift$s > 0.2))/n # 0.01638
length(which(rank_shift$n > 0.2))/n # 0.00392
length(which(rank_shift$mj > 0.2))/n # 0.01638

length(which(rank_shift$D > 0.5))/n # 0.00023
length(which(rank_shift$s > 0.5))/n # 0.00089
length(which(rank_shift$n > 0.5))/n # 0.0005
length(which(rank_shift$mj > 0.5))/n # 0.00898


## Footnote 14, Robustness: take dq independent from dp
start <- Sys.time() # 1h
reallocation <- 0.02
dps2 <- runif(n, 0, reallocation)
dqs2 <- runif(n, 0, reallocation)
P1s2 <- p1s + sign(round(1e4 * dps2) %% 2 - 0.5) * dps2
Q1s2 <- q1s + sign(round(1e6 * dqs2) %% 2 - 0.5) * dqs2 

new_scores2 <- list()
for (rule in rules) new_scores2[[rule]] <- aggregate_scores(rule = rule, grades = data.frame(list(q=Q1s2, r=1-P1s2-Q1s2, p=P1s2)), rounds=FALSE)
rank_shift2 <- list()
for (rule in rules) rank_shift2[[rule]] <- abs(distribution_scores[[rule]](new_scores2[[rule]]) - distribution_scores[[rule]](scores[[rule]]))
Sys.time() - start

# Figure 4 bis: CDF of quantile shifts following a random reallocation of 2% of grades
plot_rules(rank_shift2, "cdf", xlab="Quantile shift (r)", ylab="Share with quantile shift < r", ylim=c(0.96,1), xlim=c(0.1, 1))

# Table 4 bis: probabilities of large and very large shifts
length(which(rank_shift2$D > 0.2))/n # 0.0076
length(which(rank_shift2$s > 0.2))/n # 0.01586
length(which(rank_shift2$n > 0.2))/n # 0.00413
length(which(rank_shift2$mj > 0.2))/n # 0.01513

length(which(rank_shift2$D > 0.5))/n # 0.00026
length(which(rank_shift2$s > 0.5))/n # 0.00085
length(which(rank_shift2$n > 0.5))/n # 0.00056
length(which(rank_shift2$mj > 0.5))/n # 0.00785


##### 6. The different rules in practice #####
nb_pairs <- (6*7+9*10+10*11+11*12)/2 # 187
kendall_distances <- rankings(return_distance=T)+rankings(grades=elec2017, names=candidats_2017, scale=-2:3, return_distance=T)+rankings(grades=distributions, names=distributions_names, scale=-2:2, return_distance=T)+rankings(grades=laprimaire, names=rownames(laprimaire), scale=-2:2, return_distance=T)
round(kendall_distances/nb_pairs, 3)
# Table 5: Kendall distances between the different rules
write_clip(gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(100*kendall_distances/nb_pairs, digits=1), comment=F, sanitize.text.function = identity), fixed=T), fixed=T), fixed=T))

# Kendall distances on all datasets combined
scores_elec2012_noround <- rankings(rounds=F)
scores_elec2017_noround <- rankings(grades=elec2017, names=candidats_2017, scale=-2:3, rounds=F)
scores_distrib_noround <- rankings(grades=distributions, names=distributions_names, scale=-2:2, rounds=F)
scores_laprimaire_noround <- rankings(grades=laprimaire, names=rownames(laprimaire), scale=-2:2, rounds=F) # -1:3
all_scores <- apply(rbind(scores_elec2012_noround, scores_elec2017_noround, scores_laprimaire_noround, scores_distrib_noround)[,c(2,4:7)], 2, as.numeric)
rownames(all_scores) <- rbind(scores_elec2012_noround, scores_elec2017_noround, scores_laprimaire_noround, scores_distrib_noround)[,1]
all_orders <- do.call('rbind', lapply(split(all_scores, col(all_scores, as.factor = T)), rank))
kendall_distances_merged <- AllKendall(all_orders, all_orders)
colnames(kendall_distances_merged) <- rownames(kendall_distances_merged) <- c('mean', '$mj$', '$\\Delta$', '$\\sigma$', '$\\nu$')
round(kendall_distances_merged/(20*39), 3)
# write_clip(gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(100*kendall_distances_merged/(20*39), digits=1), comment=F, sanitize.text.function = identity), fixed=T), fixed=T), fixed=T))


##### Appendix A. Applying the tie-breaking rules on real examples #####
scores_elec2012 <- rankings()
scores_elec2012[nrow(scores_elec2012)+1,] <- c(list('Hollande changed', 0, '1-'), rep(list(0), ncol(scores_elec2012)-3))
for (rule in colnames(scores_elec2012)[c(2,4:7)]) scores_elec2012[nrow(scores_elec2012),rule] <- round(score(rule, c(0.1424, 0.1425, 0.1679, 0.0967, 0.1642, 0.1615, 0.1248), print = F), 3) # c(0, 0.4528, 1-0.4528-0.4505, 0.4505)
scores_elec2012[11,c(2,4:7)] <- as.numeric(scores_elec2012[11,c(2,4:7)])
scores_elec2017 <- rankings(grades=elec2017, names=candidats_2017, scale=-2:3)
scores_distrib <- rankings(grades=distributions, names=distributions_names, scale=-2:2)
scores_laprimaire <- rankings(grades=laprimaire, names=rownames(laprimaire), scale=-2:2) # -1:3
scores_elec2012 
scores_elec2017 
scores_distrib 
scores_laprimaire 
# Tables 6-9
write_clip(gsub('Hollande changed', '\\hline Hollande changed', gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(scores_elec2012, display=c('s', 's', 'f', 's', rep('f', 4)), digits=c(NA, NA, 2, rep(3, 5))), comment=F, include.rownames=FALSE, sanitize.colnames.function = identity), fixed=T), fixed=T), fixed=T), fixed=T))
write_clip(gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(scores_elec2017, display=c('s', 's', 'f', 's', rep('f', 4)), digits=c(NA, NA, 2, rep(3, 5))), comment=F, include.rownames=FALSE, sanitize.colnames.function = identity), fixed=T), fixed=T), fixed=T))
write_clip(gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(scores_laprimaire, display=c('s', 's', 'f', 's', rep('f', 4)), digits=c(NA, NA, 2, rep(3, 5))), comment=F, include.rownames=FALSE, sanitize.colnames.function = identity), fixed=T), fixed=T), fixed=T))
write_clip(gsub('\\end{table}', '}', gsub('\\begin{table}[ht]', '', gsub('\\begin{tabu', '\\makebox[\\columnwidth][c]{ \\begin{tabu', print.xtable(xtable(scores_distrib, display=c('s', 's', 'f', 's', rep('f', 4)), digits=c(NA, NA, 2, rep(3, 5))), comment=F, include.rownames=FALSE, sanitize.colnames.function = identity), fixed=T), fixed=T), fixed=T))
# Figures 5-8
barres(file="elec2012_RdYlGn", color = color(7, theme='RdYlGn', rev_color = T), data=t(elec2012[10:1,]), nsp=F, thin=T, sort=FALSE, legend = colnames(elec2012), labels=candidats_2012[10:1]) # title="<b>Evaluations of candidates at the 2017 French presidential election</b>", 
barres(file="elec2017_RdYlGn", color = color(6, theme='RdYlGn', rev_color = T), data=t(elec2017[11:1,]), nsp=F, thin=T, sort=FALSE, legend = colnames(elec2017), labels=candidats_2017[11:1]) # title="<b>Evaluations of candidates at the 2017 French presidential election</b>", 
barres(file="laprimaire_RdYlGn", color = color(5, theme='RdYlGn', rev_color = T), data=t(laprimaire[12:1,]), nsp=F, thin=T, sort=FALSE, legend = c("Poor", "Fair", "Quite Good", "Good", "Very Good"), labels=rownames(laprimaire)[12:1]) # title="<b>Evaluations of candidates at the 2017 French presidential election</b>", 
barres(file="distributions_RdYlGn", color = color(5, theme='RdYlGn', rev_color = T), data=t(distributions), nsp=F, thin=T, sort=F, legend=-2:2, labels=row.names(distributions))
