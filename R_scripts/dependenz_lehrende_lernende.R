#setwd
source("data_manip.R")

#------------------------------------------------------------------------------------------------- VERBINDEN ------------------------
joinedData_items_long <- full_join(pupData_items_long_rc %>% mutate(typ = "pup"), teaData_items_long_rc %>% mutate(typ = "tea"))

joinedData_items_long <- joinedData_items_long %>% mutate(group = dplyr::recode(group,
                                                                         "VLAK" = "VAK",
                                                                         "VLOG" = "VOG",
                                                                         "VLOR" = "VOR"))

joinedData_items_long <- joinedData_items_long %>% mutate(typ = factor(typ))

joinedData_items_long <- joinedData_items_long %>% mutate(item = factor(item))

#------------------------------------------------------------------------------------------------- DATENZUSAMMENFASSUNG ------------------------
joinedData_items_long %>%
  group_by(typ, item) %>%
  get_summary_stats(value, type = "median_iqr") %>%
  print(n = Inf)

#------------------------------------------------------------------------------------------------- MANN-WHITNEY-U-TEST ------------------------
testMann <- function (fData, fItem1, fItem2){
  stat.test <- filter(fData, item == fItem1 | item ==fItem2) %>%
    wilcox_test(value ~ typ) %>%
    add_significance()
  print(stat.test)
}

testMann(joinedData_items_long, "VOR03", "VLOR03")
testMann(joinedData_items_long, "VOR05", "VLOR05")
testMann(joinedData_items_long, "VOG03", "VLOG03")
