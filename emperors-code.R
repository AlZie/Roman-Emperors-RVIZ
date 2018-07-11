library(scales)
library(tidyverse)

#Import file from GIT
RAW <- read_csv("https://raw.githubusercontent.com/zonination/emperors/master/emperors.csv")

#modern city and province by hand check. NOT USED LATER YET!
modern_cities <- tibble(birth.cty = c("Rome","Alba","Narni", "Shahba", "Lugdunum","Antitum","Terracina","Terentinum","Falacrine","Italica","Lanuvium","Milan","Leptis Magna","Iol Caesarea","Emesa","Arca Caesarea","Budalia","Sirmium","Interamna Nahars","Narbo","Salona","Dardania","Romuliana","Naissus", "Felix Romuliana","Arelate","Constantinople","Singidunum", "Cibalae","Cauca"),
                        birth.cty.mod = c("Rome","Alba","Narni", "Shahba", "Lyon", "Antitum", "Terracina", "Terentinum", "Rieti", "Seville","Lanuvio","Milano","Al Khums","Cherchell","Homs","Arqa", "Martinci","Sremska Mitrovica","Terni", "Narbonne", "Solin", "?anakkale", "Zaje?ar", "Ni?", "Zaje?ar", "Arles", "Istanbul", "Belgrade", "Vinkovci","Segovia")) # Collected by hand
modern_provinces <- tibble(birth.prv = c( "Italia", "Gallia Lugdunensis", "Hispania Baetica", "Libya", "Mauretania", "Syria", "Thrace", "Phrygia", "Unknown", "Pannonia Inferior", "Pannonia", "Africa", "Gallia Narbonensis", "Dalmatian", "Moesia", "Moesia Superior", "Dacia Aureliana", "Hispania"),
                           birth.prv.mod = c("Italy", "France", "Spain", "Libya", "Algeria", "Syria", "Bulgaria", "Turkey", "Unknown", "Serbia", "Serbia", "Africa", "France", "Croatia", "Turkey", "Serbia", "Bulgaria", "Spain"))
EMP <- RAW %>% left_join(modern_cities) %>% left_join(modern_provinces)

#birth and reign.start date with minus sign
EMP$birth[[1]] <- seq(seq(seq(as.Date("0/1/1"),length = 2,by = "-62 years")[[2]],length = 2, by = "8 months")[[2]],length = 2, by = "22 days")[[2]]
EMP$reign.start[[1]] <- seq(seq(seq(as.Date("0/1/1"),length = 2,by = "-26 years")[[2]],length = 2, by = "0 months")[[2]],length = 2, by = "15 days")[[2]]
EMP$birth[[2]] <- seq(seq(seq(as.Date("0/1/1"),length = 2,by = "-41 years")[[2]],length = 2, by = "10 months")[[2]],length = 2, by = "15 days")[[2]]
EMP$birth[[4]] <- seq(seq(seq(as.Date("0/1/1"),length = 2,by = "-9 years")[[2]],length = 2, by = "7 months")[[2]],length = 2, by = "0 days")[[2]]
EMP$birth[[6]] <- seq(seq(seq(as.Date("0/1/1"),length = 2,by = "-2 years")[[2]],length = 2, by = "11 months")[[2]],length = 2, by = "23 days")[[2]]

for (i in seq(1:68)) {
  if (is.na(EMP$birth[[i]]) == 1) {
    EMP$birth[[i]] <- EMP$reign.start[[i]]
  }
}

#setting colour, shape and hline positions
colours <- c("#e6194bff", "#3cb44bff",
            "#911eb4ff", "#cc6d00ff",
            "#0082c8ff", "#00d1d1ff", 
            "#787800ff", "#800000ff")
shapes <- c(0, 1, 2, 5, 6, 7, 9, NA)
dyn_loc <- 68 - c(5, 11, 18, 26, 48, 63, 67, 68)
dynasty <- unique(EMP$dynasty)
# Emperors life lines plot (the sure one)
EMP %>% 
  transform(rev(reign.start)) %>%
  ggplot(aes(y = reorder(name, desc(reign.start)),
             col = rise)) +
  geom_segment(aes(x = reign.start, xend = reign.end, yend = name),
               size = 1.75) +
  geom_segment(aes(x = birth, xend = death, yend = name)) +
  geom_point(aes(x = death, shape = cause),
             col = "#000000FF", size = 1.5, position = position_nudge(x = 2000)) +
  geom_vline(xintercept = -719528, col = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = dyn_loc + 0.5, linetype = "dashed") +
  geom_text(aes(x = death, label = name),
            size = 2.5, hjust = "left" ,nudge_x = 4000) +
  geom_text(aes(x = as.Date(-719528, origin = "1970/1/1"), y = name[[65]]),
            label = "BCE", nudge_x = -5000, col = "black", size = 3) +
  geom_text(aes(x = as.Date(-719528, origin = "1970/1/1"), y = name[[65]]),
            label = "CE", nudge_x = 5000, col = "black", size = 3) +
  geom_label(aes(x = as.Date(-554299, origin = "1970/1/1"), y = name[[5]]),
            label = "The lives, reigns, and deaths \n of 68 Roman emperors \n from 26 BC to 395", hjust = "right", col = "black", size = 5) +
  annotate(geom = "text", x = as.Date(-748748, origin = "1970/1/1"), y = dyn_loc + 1.15,
           label = dynasty, size = 3, hjust = "left") +
  theme_light() +
  expand_limits(x = as.Date(c(-728659, -554299),origin = "1970/1/1")) +
  scale_color_manual(values = colours, name = "Rise to power") +
  scale_shape_manual(values = shapes, name = "Cause of death") +
  scale_x_date(breaks = seq.Date(from = as.Date(-756052, origin = "1970/1/1"),
                                 to = as.Date(-536906, origin = "1970/1/1"), 
                                 by = "50 years"),
               minor_breaks = seq.Date(from = as.Date(-756052, origin = "1970/1/1"),
                                      to = as.Date(-536906, origin = "1970/1/1"), 
                                      by = "10 years"),
               labels = date_format(format = "%Y")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 0)) +
  xlab("Year")
