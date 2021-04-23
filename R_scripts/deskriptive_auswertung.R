setwd("/home/cygnid/OneDrive/PHLU/BS/MA/V2/stat/")
source("data_manip.R")

#-------------------------- GESTAPELTES BALKENDIAGRAMM -------------------------------------------------------
plotStackedBarChart <- function (fData, fGroup, fName) {
  pSBC <- plot(
    likert(
      as.data.frame(
        fData %>% select(starts_with(fGroup)))),
    ordered = TRUE,
    include.histogram = FALSE,
    col = mycols[1-4]) +
    labs(y = "Prozent") +
    theme(legend.title = element_blank())
  ggsave(fName, width = 200, height = 220, units = "mm", plot = pSBC)
  return(pSBC)
}

#-------------------------- HAEUFIGKEIT BARPLOT -------------------------------------------------------------
freqBarPlot <- function (fData, fGroup, fName) {
  fBP <- ggplot(fData %>% filter(group == fGroup), aes(x = value, group = item)) +
    geom_bar(aes(y = ..prop.., fill = "A"), stat = "count") +
    facet_wrap(~ item) +
    theme(legend.position="none", axis.text.x = element_text(angle=60, hjust=1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) +
    ylab("relative Häufigkeit") +
    xlab("") +
    scale_fill_manual(values=c("A"=as.character(mycols[5]))) +
    geom_text(aes(label=scales::percent(..prop.., accuracy=0.1), y=..prop..), stat="count", vjust=-.5, size=3) + 
    scale_x_discrete(limits = c("nicht wichtig", "eher nicht wichtig", NA, "eher wichtig", "wichtig"))
  ggsave(fName, width = 200, height = 200, units = "mm", plot = fBP)
  return(fBP)
}

#-------------------------- BALKENDIAGRAMME MANIFEST LERNENDE ------------------------------------------------
gPlot <- ggplot(pupData, aes(x = VSS01)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_roe.png", width = 100, height = 100, units = "mm", plot = gPlot)

gPlot <- ggplot(pupData, aes(x = VSS02)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_ges.png", width = 100, height = 100, units = "mm", plot = gPlot)

gPlot <- ggplot(pupData, aes(x = VSS03)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_stu.png", width = 100, height = 100, units = "mm", plot = gPlot)

gPlot <- ggplot(pupData, aes(x = VSS04)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_niv.png", width = 100, height = 100, units = "mm", plot = gPlot)

gPlot <- ggplot(pupData, aes(x = VSS05)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_erf.png", width = 100, height = 100, units = "mm", plot = gPlot)

gPlot <- ggplot(pupData, aes(x = VSS06)) +
  geom_bar(stat = "count", fill = mycols[5]) +
  ylab("Anzahl") +
  xlab("") +
  ylim(0,175) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme (axis.text.x = element_text(angle=45, hjust=1))
ggsave("maniVar_akt.png", width = 100, height = 100, units = "mm", plot = gPlot)

#-------------------------- BALKENDIAGRAMME MANIFEST LEHRENDE ------------------------------------------------
manLP <-teaData %>% select(starts_with("VLP"))
manLP  <- manLP  %>% rename("VLP01 - Oberstufe Cham" = "VLP01",
                        "VLP02.G1 - Mädchen" = "VLP02.G1",
                        "VLP02.G2 - Knaben" = "VLP02.G2",
                        "VLP02.G3 - Koedukativ" = "VLP02.G3",
                        "VLP03.G1 - 1. Oberstufe" = "VLP03.G1",
                        "VLP03.G2 - 2. Oberstufe" = "VLP03.G2",
                        "VLP03.G3 - 3. Oberstufe" = "VLP03.G3",
                        "VLP03.G4 - andere Stufe" = "VLP03.G4",
                        "VLP04.G1 - Realklasse" = "VLP04.G1",
                        "VLP04.G2 - Sekundarklasse" = "VLP04.G2",
                        "VLP04.G3 - Kunst- und Sportklasse" = "VLP04.G3",
                        "VLP04.G4 - Integrierte Klasse" = "VLP04.G4",
                        "VLP04.G5 - andere Sonderklasse" = "VLP04.G5",
                        "VLP05 - Fernunterrichtserfahrung" = "VLP05")
manLP_long <- manLP  %>%
  pivot_longer(c("VLP01 - Oberstufe Cham",
                 "VLP02.G1 - Mädchen",
                 "VLP02.G2 - Knaben",
                 "VLP02.G3 - Koedukativ",
                 "VLP03.G1 - 1. Oberstufe",
                 "VLP03.G2 - 2. Oberstufe",
                 "VLP03.G3 - 3. Oberstufe",
                 "VLP03.G4 - andere Stufe",
                 "VLP04.G1 - Realklasse",
                 "VLP04.G2 - Sekundarklasse",
                 "VLP04.G3 - Kunst- und Sportklasse",
                 "VLP04.G4 - Integrierte Klasse",
                 "VLP04.G5 - andere Sonderklasse",
                 "VLP05 - Fernunterrichtserfahrung"),
               names_to="item", values_to="value")

lpPlot <- ggplot(manLP_long, aes(x = value, group = item)) +
            geom_bar(stat = "count", fill = mycols[5]) +
            facet_wrap( ~ item) +
            ylab("Anzahl") +
            ylim(c(0,30)) +
            xlab("") +
            geom_text(stat='count', aes(label=..count..), vjust=-1)

ggsave("test.png", width = 300, height = 300, units = "mm", plot = lpPlot)
#-------------------------- DESKRIPTIVE STATISTIK LERNENDE ---------------------------------------------------
summary(pupData %>% select(starts_with("VSS")))

summary(pupData %>% select(starts_with("VOR")))
summary(pupData %>% select(starts_with("VOG")))
summary(pupData %>% select(starts_with("VAK")))

plotStackedBarChart(pupData_named, "VOG", "iPlot_VOG.png")
plotStackedBarChart(pupData_named, "VOR", "iPlot_VOR.png")
plotStackedBarChart(pupData_named, "VAK", "iPlot_VAK.png")

freqBarPlot(pupData_items_long, "VOR", "fPlot_VOR.png")
freqBarPlot(pupData_items_long, "VOG", "fPlot_VOG.png")
freqBarPlot(pupData_items_long, "VAK", "fPlot_VAK.png")

#-------------------------- DESKRIPTIVE STATISTIK LEHRENDE --------------------------------------------------
summary(teaData %>% select(starts_with("VLP")))

summary(teaData %>% select(starts_with("VLOR")))
summary(teaData %>% select(starts_with("VLOG")))
summary(teaData %>% select(starts_with("VLAK")))

plotStackedBarChart(teaData_named, "VLOR", "iPlot_VLOR.png")
plotStackedBarChart(teaData_named, "VLOG", "iPlot_VLOG.png")
plotStackedBarChart(teaData_named, "VLAK", "iPlot_VLAK.png")

freqBarPlot(teaData_items_long, "VLOR", "fPlot_VLOR.png")
freqBarPlot(teaData_items_long, "VLOG", "fPlot_VLOG.png")
freqBarPlot(teaData_items_long, "VLAK", "fPlot_VLAK.png")

