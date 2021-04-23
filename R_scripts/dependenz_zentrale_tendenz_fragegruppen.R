setwd("/home/cygnid/OneDrive/PHLU/BS/MA/V2/stat/")
source("data_manip.R")

#----------------------------------------------------------- DATENZUSAMMENFASSUNG ------------------------
pupData_items_long_rc %>%
         group_by(item) %>%
         get_summary_stats(value, type = "median_iqr")

teaData_items_long_rc %>%
         group_by(item) %>%
         get_summary_stats(value, type = "median_iqr")

#----------------------------------------------------------- FRIEDMANNTEST  ------------------------------
testFried <- function (fData, fGroup) {
  res.fried <- filter(fData, group == fGroup) %>% friedman_test(value ~ item | rater)
  print(res.fried)
  
  cat("# chi-squared quantile \n")
  print(qchisq(0.05,
         df = res.fried$df,
         lower.tail=FALSE))
  
  cat("# chi-squared probability \n")
  print(pchisq(res.fried$statistic,
             df = res.fried$df,lower.tail = FALSE))
  
  filter(fData, group == fGroup) %>% friedman_effsize(value ~ item | rater) %>% print(n = Inf)
  return(TRUE)
}

#------------------------------------------------------------ MULTIPLER PAARWEISER VERGLEICH  ------------
testBonf <- function (fData, fGroup) {
  pwc <- filter(fData, group == fGroup) %>% wilcox_test(value ~ item, paired = TRUE, p.adjust.method = "bonferroni", alternative = )
  pwc %>% print(n = Inf)
  print(pwc)
  
  fSize <- filter(fData, group == fGroup) %>% wilcox_effsize(value ~ item)
  fSize %>% print(n = Inf)
  print(fSize)
  return(TRUE)
}

#------------------------------------------------------------ TESTSTATISTIK LERNENDE ---------------------
testFried(pupData_items_long_rc, "VOR") #FRIEDMANNTEST OG LERNENDE
testBonf(pupData_items_long_rc, "VOR") #MULTIPLER PAARWEISER VERGLEICH OG LERNENDE

testFried(pupData_items_long_rc, "VOG") #FRIEDMANNTEST OR LERNENDE
testBonf(pupData_items_long_rc, "VOG") #MULTIPLER PAARWEISER VERGLEICH OR LERNENDE

testFried(pupData_items_long_rc, "VAK") #FRIEDMANNTEST AK LERNENDE
testBonf(pupData_items_long_rc, "VAK") #MULTIPLER PAARWEISER VERGLEICH AK LERNENDE

#------------------------------------------------------------- TESTSTATISTIK LEHRENDE --------------------
testFried(teaData_items_long_rc, "VLOR") #FRIEDMANNTEST LOR LEHRENDE
testBonf(teaData_items_long_rc, "VLOR") #MULTIPLER PAARWEISER VERGLEICH LOR LEHRENDE

testFried(teaData_items_long_rc, "VLOG") #FRIEDMANNTEST LOG LEHRENDE
testBonf(teaData_items_long_rc, "VLOG") #MULTIPLER PAARWEISER VERGLEICH LOG LEHRENDE

testFried(teaData_items_long_rc, "VLAK") #FRIEDMANNTEST LAK LEHRENDE
testBonf(teaData_items_long_rc, "VLAK") #MULTIPLER PAARWEISER VERGLEICH LAK LEHRENDE
