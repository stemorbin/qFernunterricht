rm(list = ls())
#setwd

#-------------------------- LIBRARIES LADEN -------------------------------------------------------------
library(tidyverse)
library(likert)
library(ggplot2)
library(rstatix)

#-------------------------- FARBEN DEFINIEREN -----------------------------------------------------------
mycols <- c(
  shade0 = "#fdebee",
  shade1 = "#fad7dc",
  shade2 = "#f39ca8",
  shade3 = "#eb6178",
  shade4 = "#e83857"
)
mycols_html <- str_remove(mycols,"#")

#-------------------------- FUNKTION MODUS --------------------------------------------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#-------------------------- DATEN LESEN ----------------------------------------------------------------
lvlStrings = c("nicht wichtig", "eher nicht wichtig", "eher wichtig", "wichtig")

pupData <- read_csv("/home/cygnid/OneDrive/PHLU/BS/MA/V2/stat/komplette_resultate_lernende.csv",
              na = c("", " "),
              col_types = cols(
                id = col_integer(),
                submitdate = col_datetime(format = ""),
                interviewtime = col_double(),
                EF01 = col_factor(levels = c("Lernender","Sportlehrperson"), ordered = FALSE),
                VSS01 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Roehrliberg
                VSS02 = col_factor(levels = c("Mädchen",
                                              "Knaben",
                                              "koedukativ"), ordered = FALSE), #Unterricht
                VSS03 = col_factor(levels = c("1. Oberstufe",
                                              "2. Oberstufe",
                                              "3. Oberstufe",
                                              "andere Stufe"), ordered = FALSE), #Stufe,
                VSS04 = col_factor(levels = c("Realklasse",
                                              "Sekundarklasse",
                                              "Kunst- und Sportklasse",
                                              "Integrierte Klasse",
                                              "andere Sonderklasse"), ordered = FALSE), #Niveau,
                VSS05 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Fernunterrichterfahren
                VSS06 = col_factor(levels = c("keine",
                                              "ca. 1 Stunde/Woche",
                                              "ca. 2-4 Stunden/Woche",
                                              "mehr als 4 Stunden/Woche"), ordered = TRUE), #Aktivität
                VOR01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOR07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOG07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VAK07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VOF01 = col_character(),
                VOF02 = col_character()
              ))

teaData <- read_csv("/home/cygnid/OneDrive/PHLU/BS/MA/V2/stat/komplette_resultate_lehrende.csv",
              na = c("", " "),
              col_types = cols(
                id = col_integer(),
                submitdate = col_datetime(format = ""),
                interviewtime = col_double(),
                EF01 = col_factor(levels = c("Lernender","Sportlehrperson"), ordered = FALSE),
                VLP01 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Roehrliberg
                VLP02.G1 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Mädchen
                VLP02.G2 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Knaben
                VLP02.G3 = col_factor(levels = c("ja","nein"), ordered = FALSE), #koedukativ
                VLP03.G1 = col_factor(levels = c("ja","nein"), ordered = FALSE), #1. Oberstufe
                VLP03.G2 = col_factor(levels = c("ja","nein"), ordered = FALSE), #2. Oberstufe
                VLP03.G3 = col_factor(levels = c("ja","nein"), ordered = FALSE), #3. Oberstufe
                VLP03.G4 = col_factor(levels = c("ja","nein"), ordered = FALSE), #andere Stufen
                VLP04.G1 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Realklasse
                VLP04.G2 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Sekundarklasse
                VLP04.G3 = col_factor(levels = c("ja","nein"), ordered = FALSE), #KSK
                VLP04.G4 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Integrierte Klasse
                VLP04.G5 = col_factor(levels = c("ja","nein"), ordered = FALSE), #andere Sonderklasse
                VLP05 = col_factor(levels = c("ja","nein"), ordered = FALSE), #Fernunterrichterfahren
                VLOR01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOR07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOG07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK01 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK02 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK03 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK04 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK05 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK06 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLAK07 = col_factor(levels = lvlStrings, ordered = TRUE),
                VLOF01 = col_character(),
                VLOF02 = col_character()
              ))

#-------------------------- DATENMANIPULATION LERNENDE -------------------------------------------------
pupData <- rename(pupData, "rater" = "id")

pupData_items_long <- pupData %>%
  select(rater |starts_with("VAK") | starts_with("VOR") | starts_with("VOG")) %>%
  pivot_longer(c("VAK01","VAK02","VAK03","VAK04","VAK05","VAK06","VAK07",
                 "VOR01","VOR02","VOR03","VOR04","VOR05","VOR06","VOR07",
                 "VOG01","VOG02","VOG03","VOG04","VOG05","VOG06","VOG07"),
               names_to = "item",
               values_to = "value")

pupData_items_long <- pupData_items_long %>% mutate(rater = factor(rater))

pupData_items_long <- pupData_items_long %>% mutate(group = str_extract(pupData_items_long$item, "^..."))

pupData_items_long <- pupData_items_long %>% mutate(group = factor(group))

pupData_items_long_rc <- pupData_items_long %>% mutate(value = dplyr::recode(value,
                                                                             "nicht wichtig" = -2,
                                                                             "eher nicht wichtig" = -1,
                                                                             "eher wichtig" = 1,
                                                                             "wichtig" = 2))

pupData_items_long_rc <- pupData_items_long_rc %>% mutate(value = replace_na(value, 0))

head(pupData_items_long)

#-------------------------- DATENMANIPULATION LEHRENDE -------------------------------------------------
teaData <- rename(teaData, "rater" = "id")

teaData_items_long <- teaData %>%
  select(rater |starts_with("VLAK") | starts_with("VLOR") | starts_with("VLOG")) %>%
  pivot_longer(c("VLAK01","VLAK02","VLAK03","VLAK04","VLAK05","VLAK06","VLAK07",
                 "VLOR01","VLOR02","VLOR03","VLOR04","VLOR05","VLOR06","VLOR07",
                 "VLOG01","VLOG02","VLOG03","VLOG04","VLOG05","VLOG06","VLOG07"),
               names_to = "item",
               values_to = "value")

teaData_items_long <- teaData_items_long %>% mutate(rater = factor(rater))

teaData_items_long <- teaData_items_long %>% mutate(group = str_extract(teaData_items_long$item, "^...."))

teaData_items_long <- teaData_items_long %>% mutate(group = factor(group))

teaData_items_long_rc <- teaData_items_long %>% mutate(value = dplyr::recode(value,
                                                                             "nicht wichtig" = -2,
                                                                             "eher nicht wichtig" = -1,
                                                                             "eher wichtig" = 1,
                                                                             "wichtig" = 2))

teaData_items_long_rc <- teaData_items_long_rc %>% mutate(value = replace_na(value, 0))

head(teaData_items_long)

#-------------------------- BENENNUNG ITEMS LERNENDE  --------------------------------------------------
pupData_named <- pupData %>% rename("VAK1 - Ich mussinnerhalb von 24 Stunden / einem Tag bei Fragen zu Aufgaben im Sportfernunterricht Hilfe von meiner Sportlehrperson erhalten." = "VAK01",
                                    "VAK2 - Ich muss durch die Aufgaben, mit denen ich mich im Sportfernunterricht beschäftige, richtig heraus gefordert werden." = "VAK02",
                                    "VAK3 - Ich muss bei den Aufgaben im Sportfernunterricht aus unterschiedlichen Schwierigkeitsstufen auswählen können." = "VAK03",
                                    "VAK4 - Wenn ich eine Aufgabe im Sportfernunterricht erledigt habe, muss mir klar sein, was ich dabei gelernt/geleistet habe." = "VAK04",
                                    "VAK5 - Wenn ich eine Aufgabe im Sportfernunterricht bearbeite, will ich selbst bestimmen, auf welche Weise ich die Aufgabe bearbeite." = "VAK05",
                                    "VAK6 - Wenn ich eine Aufgabe im Sportfernunterricht bearbeite, will ich selbst bestimmen, zu welchem Zeitpunkt ich die Aufgabe bearbeite." = "VAK06",
                                    "VAK7 - Wenn ich eine Aufgabe im Sportfernunterricht bearbeite, will ich selbst bestimmen, in welchem Tempo ich die Aufgabe bearbeite." = "VAK07",
                                    "VOR1 - Wie wichtig ist dir, dass die Bearbeitung der Aufgaben im Sportfernunterricht nicht länger als zwei Stunden pro Woche dauert?" = "VOR01",
                                    "VOR2 - Ich muss meine Aufgaben im Sportfernunterricht ohne zusätzliches Material durchführen können." = "VOR02",
                                    "VOR3 - Meine Aufgaben im Sportfernunterricht müssen verständlich erklärt sein. Ich muss wissen, was ich tun muss." = "VOR03",
                                    "VOR4 - Meine Sportlehrperson soll meine Aufgaben, die ich im Sportfernunterricht erledige, genau kontrollieren." = "VOR04",
                                    "VOR5 - Ich muss genau wissen, an welchem Ort ich meine Aufgaben für den Sportfernunterricht abholen kann und wo ich die erledigten Aufgaben abgeben kann." = "VOR05",
                                    "VOR6 - Wenn Aufgaben im Sportfernunterricht nicht erledigt werden, muss das Konsequenzen haben." = "VOR06",
                                    "VOR7 - Meine Resultate aus den Aufgaben im Sportfernunterricht muss ich mit meinen Mitschülerinnen und Mitschüler vergleichen können." = "VOR07",
                                    "VOG1 - Wie wichtig ist im Fernunterricht der wöchentliche Kontakt (Text-, Sprach- oder Videochat) mit meiner Sportlehrperson." = "VOG01",
                                    "VOG2 - Alle meine Aufgaben, die ich im Sportfernunterricht abgegeben habe, sollen mir von meiner Sportlehrperson mit einer Rückmeldung wieder zurückgegeben werden." = "VOG02",
                                    "VOG3 - Ich möchte beim Bearbeiten meiner Aufgaben im Sportfernunterricht Spass haben." = "VOG03",
                                    "VOG4 - Die Aufgaben, die ich im Sportfernunterricht bearbeite, müssen etwas mit meinen eigenen Erfahrungen im Alltag zu tun haben." = "VOG04",
                                    "VOG5 - Ich möchte jede Woche im Sportfernunterricht aus verschiedenen Aufgaben eine Aufgabe auswählen können und diese anschliessend bearbeiten." = "VOG05",
                                    "VOG6 - Wenn wir uns an die Vorgaben von Behörden und Schule halten, möchte ich Aufgaben im Sportfernunterricht lösen, die ich auch zusammen mit meinen Mitschülerinnen und Mitschüler (Freundinnen/Freunde) bearbeiten kann." = "VOG06",
                                    "VOG7 - Wenn ich eine Aufgabe nicht oder nur teilweise lösen kann, will ich dabei auch trotzdem etwas Neues dazu lernen/können." = "VOG07")

#-------------------------- BENENNUNG ITEMS LEHRENDE  ---------------------------------------------------
teaData_named <- teaData %>% rename("VLAK1 - Dass meine SuS innerhalb von 24 Stunden / einem Tag bei Fragen zu Aufgaben im Sportfernunterricht Hilfe von mir erhalten, ist mir ..." = "VLAK01",
                                    "VLAK2 - SuS müssen durch die Aufgaben, mit denen sie sich im Sportfernunterricht beschäftigen, richtig herausgefordert werden." = "VLAK02",
                                    "VLAK3 - SuS müssen bei den Aufgaben im Sportfernunterricht aus unterschiedlichen Schwierigkeitsstufen auswählen können." = "VLAK03",
                                    "VLAK4 - Wenn SuS eine Aufgabe im Sportfernunterricht erledigt haben, muss ihnen klar sein, was sie dabei gelernt/geleistet haben." = "VLAK04",
                                    "VLAK5 - Wenn SuS eine Aufgabe im Sportfernunterricht bearbeiten, müssen sie selbst bestimmen können, auf welche Weise sie die Aufgabe bearbeiten." = "VLAK05",
                                    "VLAK6 - Wenn SuS eine Aufgabe im Sportfernunterricht bearbeiten, müssen sie selbst bestimmen können, zu welchem Zeitpunkt sie die Aufgabe bearbeiten." = "VLAK06",
                                    "VLAK7 - Wenn SuS eine Aufgabe im Sportfernunterricht bearbeiten, müssen sie selbst bestimmen können, in welchem Tempo sie die Aufgabe bearbeiten." = "VLAK07",
                                    "VLOR1 - Wie wichtig ist dir, dass die Bearbeitung der Aufgaben im Sportfernunterricht nicht länger als zwei Stunden pro Woche dauert?" = "VLOR01",
                                    "VLOR2 - Sie SuS müssen meine Aufgaben im Sportfernunterricht ohne zusätzliches Material durchführen können." = "VLOR02",
                                    "VLOR3 - Meine Aufgaben im Sportfernunterricht müssen verständlich erklärt sein. Die SuS müssen wissen, was zu tun ist." = "VLOR03",
                                    "VLOR4 - Die Aufgaben, die die SuS im Sportfernunterricht erledigen, müssen ausführlich kontrolliert werden." = "VLOR04",
                                    "VLOR5 - Die SuS müssen wissen, an welchem Ort sie ihre Aufgaben für den Sportfernunterricht abholen und wo sie die erledigten Aufgaben abgeben können." = "VLOR05",
                                    "VLOR6 - Wenn Aufgaben im Sportfernunterricht nicht erledigt werden (z. B. zu spät oder nicht abgegeben / ungenügend bearbeitet), muss das Konsequenzen haben." = "VLOR06",
                                    "VLOR7 - Die Resultate aus den Aufgaben im Sportfernunterricht müssen die SuS unter einander vergleichen können." = "VLOR07",
                                    "VLOG1 - Auch im Fernunterricht ist mir der wöchentliche Kontakt (Text-, Sprach- oder Videochat) mit meinen SuS ..." = "VLOG01",
                                    "VLOG2 - Alle Aufgaben, die durch die SuS bearbeitet wurden, gebe ich mit einer Rückmeldung wieder zurück." = "VLOG02",
                                    "VLOG3 - Die SuS sollen beim Bearbeiten meiner Aufgaben im Sportfernunterricht Spass haben." = "VLOG03",
                                    "VLOG4 - Die Aufgaben, die ich im Sportfernunterricht abgebe, müssen etwas mit den Erfahrungen im Alltag der SuS zu tun haben." = "VLOG04",
                                    "VLOG5 - Die SuS müssen jede Woche im Sportfernunterricht aus verschiedenen Aufgaben eine Aufgabe auswählen können und diese anschliessend bearbeiten." = "VLOG05",
                                    "VLOG6 - Wenn wir uns an die Vorgaben von Behörden und Schule halten, möchte ich, dass die SuS auch in Gruppen/Kleingruppen arbeiten können." = "VLOG06",
                                    "VLOG7 - Wenn SuS eine Aufgabe nicht oder nur teilweise lösen können, müssen sie trotzdem etwas Neues dazu lernen/können." = "VLOG07")
