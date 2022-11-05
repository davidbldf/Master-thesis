#basics

library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
setwd("~/Uni/Sustainable Development/SS21/Master Thesis_neu/R/final")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #adds colourblind palette for graphs

#import data
Antworten <- read_delim("results.csv", delim = ";", escape_double = FALSE, 
                        na = "empty", trim_ws = TRUE)

#Antworten_normal <- read_delim("antworten_normal.csv", 
#                               delim = ";", escape_double = FALSE, na = "empty", 
#                               trim_ws = TRUE)

#Antworten_sus <- read_delim("antworten_sus.csv", 
#                            delim = ";", escape_double = FALSE, na = "empty", 
 #                           trim_ws = TRUE)

#Frage 1
#Frage 2
#Frage 3
#q03adjusted_values creates dataframe for the distribution of percentages for each mode of transport according to the
#trips per employee, since the real numbers are skewed due to the differences in employees and trips among survey participants

#Values for 2019
Q03_adjusted_values <- data.frame("Bahn" = Antworten$reisen_pro_ma * Antworten$Q3.SQ004_SQ001./100,
                                    "Auto" = Antworten$reisen_pro_ma * Antworten$Q3.SQ004_SQ002./100,
                                    "Flugzeug" = Antworten$reisen_pro_ma * Antworten$Q3.SQ004_SQ003./100)
Q03_adjusted_values[is.na(Q03_adjusted_values)] <- 0  #converts NA to 0


#convert NA in Antworten.csv to 0 for further calculations
Antworten$Q3.SQ004_SQ001.[is.na(Antworten$Q3.SQ004_SQ001.)] <- 0  
Antworten$Q3.SQ004_SQ002.[is.na(Antworten$Q3.SQ004_SQ002.)] <- 0 
Antworten$Q3.SQ004_SQ003.[is.na(Antworten$Q3.SQ004_SQ003.)] <- 0 

Q03_dataframe <- data.frame("mode of transport" = c("Train / Public transport",
                                                  "Car",
                                                  "Plane"),
                           "avg" = c(sum(Antworten$Q3.SQ004_SQ001.)/27, #hier mit 27 ersetzen
                                     sum(Antworten$Q3.SQ004_SQ002.)/27, #weil 1 person frage nicht beantwortete
                                     sum(Antworten$Q3.SQ004_SQ003.)/27),
                           "adjusted" = c(sum(Q03_adjusted_values$Bahn),
                                          sum(Q03_adjusted_values$Auto),
                                          sum(Q03_adjusted_values$Flugzeug)))

Q03_dataframe <- Q03_dataframe %>%                                  
  mutate(adjusted_pct = c(sum(Q03_adjusted_values$Bahn / sum(Q03_dataframe$adjusted)*100),
                          sum(Q03_adjusted_values$Auto / sum(Q03_dataframe$adjusted)*100),
                          sum(Q03_adjusted_values$Flugzeug / sum(Q03_dataframe$adjusted)*100))) #add percentage column                           
Q03_dataframe$avg = round(Q03_dataframe$avg, digits = 2)
Q03_dataframe$adjusted_pct = round(Q03_dataframe$adjusted_pct, digits = 2)

Q03_dataframe$mode.of.transport <- factor(Q03_dataframe$mode.of.transport,
                                  levels = c("Train / Public transport",
                                             "Car",
                                             "Plane"))

Q03_graph <- ggplot(data = Q03_dataframe) + 
  geom_bar(aes(x= mode.of.transport, y = avg, fill = mode.of.transport),colour = "black", stat = 'identity') + 
  ggtitle("Q03") +
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=mode.of.transport, y = avg, label = paste0(avg,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)
ggsave("Q3_graph_2019.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


Q03_graph_adjusted_2019 <- ggplot(data = Q03_dataframe) + 
  geom_bar(aes(x= mode.of.transport, y = adjusted_pct, fill = mode.of.transport),colour = "black", stat = 'identity') + 
  ggtitle("Q03: Mode of transport in 2019") +
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=mode.of.transport, y = adjusted_pct, label = paste0(adjusted_pct,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)
ggsave("Q3_graph_adjusted_2019.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



##Values for 2021##
Q03_adjusted_values_2021 <- data.frame("Bahn" = Antworten$reisen_pro_ma * Antworten$Q3.SQ005_SQ001./100,
                                  "Auto" = Antworten$reisen_pro_ma * Antworten$Q3.SQ005_SQ002./100,
                                  "Flugzeug" = Antworten$reisen_pro_ma * Antworten$Q3.SQ005_SQ003./100)
Q03_adjusted_values_2021[is.na(Q03_adjusted_values_2021)] <- 0  #converts NA to 0


#convert NA in Antworten.csv to 0 for further calculations
Antworten$Q3.SQ005_SQ001.[is.na(Antworten$Q3.SQ005_SQ001.)] <- 0  
Antworten$Q3.SQ005_SQ002.[is.na(Antworten$Q3.SQ005_SQ002.)] <- 0 
Antworten$Q3.SQ005_SQ003.[is.na(Antworten$Q3.SQ005_SQ003.)] <- 0 

Q03_dataframe_2021 <- data.frame("mode of transport" = c("Train / Public transport",
                                                    "Car",
                                                    "Plane"),
                            "avg" = c(sum(Antworten$Q3.SQ005_SQ001.)/27, #hier mit 27 ersetzen
                                      sum(Antworten$Q3.SQ005_SQ002.)/27, #weil 1 person frage nicht beantwortete
                                      sum(Antworten$Q3.SQ005_SQ003.)/27),
                            "adjusted" = c(sum(Q03_adjusted_values_2021$Bahn),
                                           sum(Q03_adjusted_values_2021$Auto),
                                           sum(Q03_adjusted_values_2021$Flugzeug)))

Q03_dataframe_2021 <- Q03_dataframe_2021 %>%                                  
  mutate(adjusted_pct = c(sum(Q03_adjusted_values_2021$Bahn / sum(Q03_dataframe_2021$adjusted)*100),
                          sum(Q03_adjusted_values_2021$Auto / sum(Q03_dataframe_2021$adjusted)*100),
                          sum(Q03_adjusted_values_2021$Flugzeug / sum(Q03_dataframe_2021$adjusted)*100))) #add percentage column                           
Q03_dataframe_2021$avg = round(Q03_dataframe_2021$avg, digits = 2)
Q03_dataframe_2021$adjusted_pct = round(Q03_dataframe_2021$adjusted_pct, digits = 2)

Q03_dataframe_2021$mode.of.transport <- factor(Q03_dataframe_2021$mode.of.transport,
                                          levels = c("Train / Public transport",
                                                     "Car",
                                                     "Plane"))

Q03_graph_2021 <- ggplot(data = Q03_dataframe_2021) + 
  geom_bar(aes(x= mode.of.transport, y = avg, fill = mode.of.transport),colour = "black", stat = 'identity') + 
  ggtitle("Q03") +
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=mode.of.transport, y = avg, label = paste0(avg,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)
ggsave("Q3_graph_2021.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


Q03_graph_adjusted_2021 <- ggplot(data = Q03_dataframe_2021) + 
  geom_bar(aes(x= mode.of.transport, y = adjusted_pct, fill = mode.of.transport),colour = "black", stat = 'identity') + 
  ggtitle("Q03: Mode of transport in 2021") +
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=mode.of.transport, y = adjusted_pct, label = paste0(adjusted_pct,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)
ggsave("Q3_graph_adjusted_2021.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 4
Q07_dataframe<- data.frame("Bereich" = c("0 - 9%",
                               "10 - 19%",
                               "20 - 29%",
                               "30 - 39%",
                               "40 - 49%",
                               "50 - 59%",
                               "60 - 69%",
                               "70 - 79%",
                               "80 - 89%",
                               "90 - 100%"),
  "Anzahl" = c(sum(Antworten$Q7 >= 0 & Antworten$Q7 <10),
                 sum(Antworten$Q7 >= 10 & Antworten$Q7 <20),
                 sum(Antworten$Q7 >= 20 & Antworten$Q7 <30),
                 sum(Antworten$Q7 >= 30 & Antworten$Q7 <40),
                 sum(Antworten$Q7 >= 40 & Antworten$Q7 <50),
                 sum(Antworten$Q7 >= 50 & Antworten$Q7 <60),
                 sum(Antworten$Q7 >= 60 & Antworten$Q7 <70),
                 sum(Antworten$Q7 >= 70 & Antworten$Q7 <80),
                 sum(Antworten$Q7 >= 80 & Antworten$Q7 <90),
                 sum(Antworten$Q7 >= 90 & Antworten$Q7 <=100)))
                 
Q07_dataframe <- Q07_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q07_dataframe$perc = round(Q07_dataframe$perc, digits = 2)

Q07_n_total = sum(Q07_dataframe$Anzahl)       

#Create Graph
Q07_graph <- ggplot(data = Q07_dataframe) + 
  geom_bar(aes(x=Bereich, y = perc, fill = Bereich),colour = "black", stat = 'identity') + 
  ggtitle("Q07: How many business trips in your company were replaced by virtual meetings in 2021?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Bereich, y = perc, label = paste0(perc,"%"), vjust = 1.2), size = 3.5)

ggsave("Q7_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 5

Q08_liste_answers_sums <- list(sum(Antworten$Q8.SQ001. == "Y"), 
                              sum(Antworten$Q8.SQ002. == "Y"), 
                              sum(Antworten$Q8.SQ003. == "Y"), 
                              sum(Antworten$Q8.SQ004. == "Y"), 
                              sum(Antworten$Q8.SQ005. == "Y"), 
                              sum(Antworten$Q8.SQ006. == "Y"), 
                              sum(Antworten$Q8.other. != ""))


Q08_dataframe <- data.frame( "Antworten" = c("Attending an event/conference or further training",
                                            "Project business or regular meeting e.g. consultation", 
                                            "Acquisition, contract initiation in a face-to-face meeting",
                                            "Internal meeting or internal project", 
                                            "Workshop with clients",
                                            "No meetings have been replaced", 
                                            "Other"),
                           "Anzahl" = unlist(Q08_liste_answers_sums))
Q08_dataframe$Antworten <- factor(Q08_dataframe$Antworten,
                                 levels = c("Attending an event/conference or further training",
                                            "Project business or regular meeting e.g. consultation", 
                                            "Acquisition, contract initiation in a face-to-face meeting",
                                            "Internal meeting or internal project", 
                                            "Workshop with clients",
                                            "No meetings have been replaced", 
                                            "Other"))

Q08_dataframe[is.na(Q08_dataframe)] <- 0  #converts NA to 0

Q08_dataframe <- Q08_dataframe %>%                                  
   mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q08_dataframe$perc = round(Q08_dataframe$perc, digits = 2)

Q08_n_total = sum(Q08_dataframe$Anzahl)

#Create Graph
Q08_graph <- ggplot(data = Q08_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q08: Which meetings have been replaced by virtual meetings? ") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(1,1,1.5,1.5,"cm")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2), size = 3.5) +
  scale_fill_manual(values=cbPalette) 
ggsave("Q08_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


##Q08_alternative - shows percentage not dependent on total answers, but on total participants
Q08_dataframe_alt <- data.frame( "Antworten" = c("Attending an event/conference or further training",
                                             "Project business or regular meeting e.g. consultation", 
                                             "Acquisition, contract initiation in a face-to-face meeting",
                                             "Internal meeting or internal project", 
                                             "Workshop with clients",
                                             "No meetings have been replaced", 
                                             "Other"),
                             "Anzahl" = unlist(Q08_liste_answers_sums))
Q08_dataframe_alt$Antworten <- factor(Q08_dataframe_alt$Antworten,
                                  levels = c("Attending an event/conference or further training",
                                             "Project business or regular meeting e.g. consultation", 
                                             "Acquisition, contract initiation in a face-to-face meeting",
                                             "Internal meeting or internal project", 
                                             "Workshop with clients",
                                             "No meetings have been replaced", 
                                             "Other"))

Q08_dataframe_alt[is.na(Q08_dataframe_alt)] <- 0  #converts NA to 0

Q08_dataframe_alt <- Q08_dataframe_alt %>%                                  
  mutate(perc = (Anzahl / 28)*100) #add percentage column

Q08_dataframe_alt$perc = round(Q08_dataframe_alt$perc, digits = 2)

Q08_graph_alt <- ggplot(data = Q08_dataframe_alt) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q08: Which meetings have been replaced by virtual meetings? ") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(1,1,1.5,1.5,"cm")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2), size = 3.5) +
  scale_fill_manual(values=cbPalette) 
ggsave("Q08_graph_alt.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")

#Frage 6
#->siehe Hypothesis 3

#Frage 7
Q10_answers_summen <- list(sum(Antworten$Q10.SQ001.== ("1")),
                           sum(Antworten$Q10.SQ001.== ("2")),
                           sum(Antworten$Q10.SQ001.== ("3")),
                           sum(Antworten$Q10.SQ001.== ("4")),
                           sum(Antworten$Q10.SQ001.== ("5")),
                           sum(Antworten$Q10.SQ001.== ("99")))


Q10_dataframe <- data.frame("Antworten" = c("Much less",
                                            "Less",
                                            "Just as much",
                                            "More",
                                            "Much more",
                                            "I do not know / No answer"),
                            "Anzahl" = unlist(Q10_answers_summen))
Q10_dataframe$Antworten <- factor(Q10_dataframe$Antworten,
                                  levels = c("Much less",
                                             "Less",
                                             "Just as much",
                                             "More",
                                             "Much more",
                                             "I do not know / No answer"))

Q10_dataframe <- Q10_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q10_dataframe$perc = round(Q10_dataframe$perc, digits = 2)

Q10_n_total = sum(Q10_dataframe$Anzahl)

Q10_graph <- ggplot(data = Q10_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten), colour = "black", stat = 'identity') + 
  ggtitle("Q10: Based on your experience with virtual meetings during the pandemic, \nhow do you see the number of business travel activities in your company developing \nover the next two to three years compared to the travel activity in 2019? ") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank())+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q10_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Errorbar plot
# nicht möglich da verschiedene gruppen nicht gebildet werden können
#Q10_errorplot_total <- as.numeric(Antworten$Q10.SQ001.)
#Q10_errorplot_normal <- as.numeric(Antworten_normal$Q10.SQ001.)
#Q10_errorplot_sustainable <- as.numeric(Antworten_sus$Q10.SQ001.)##

#Q10_errorplot_total[Q10_errorplot_total == "99"] <- NA
#Q10_errorplot_normal[Q10_errorplot_normal == "99"] <- NA
#Q10_errorplot_sustainable[Q10_errorplot_sustainable == "99"] <- NA

#Q10_errorplot_results= data.frame("Group" = c("Total",
#                                          "Normal",
#                                          "Sustainable"),
#                        "mean" = c(mean(Q10_errorplot_total, na.rm = TRUE),
#                                   mean(Q10_errorplot_normal, na.rm = TRUE),
#                                   mean(Q10_errorplot_sustainable, na.rm = TRUE)),
#                        "sd" = c(sd(Q10_errorplot_total, na.rm = TRUE),
#                                 sd(Q10_errorplot_normal, na.rm = TRUE),
#                                 sd(Q10_errorplot_sustainable, na.rm = TRUE)))
#
#Q10_errorplot_results$Group <- factor(Q10_errorplot_results$Group,
#                            levels = c("Sustainable",
#                                       "Normal",
#                                       "Total"))
#
#Q10_errorplot_graph <- ggplot(Q10_errorplot_results, aes(x=Group, y=mean)) + 
#  labs(x = "Group", y = "Mean")+
#  geom_point()+
#  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                position=position_dodge(.9)) +
#  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6))+
#  coord_flip()#

#ggsave("Q10_errorplot_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#Frage 8
Q11_answers_summen <- list(sum(Antworten$Q11 == ("A1")), 
                           sum(Antworten$Q11 == ("A2")), 
                           sum(Antworten$Q11 == ("A3")),
                           sum(Antworten$Q11 == ("-oth-")))


Q11_dataframe <- data.frame("Antworten" = c("Digitisation ", 
                                            "Sustainability aspects", 
                                            "Cost saving",
                                            "Other"),
                            "Anzahl" = unlist(Q11_answers_summen))

Q11_dataframe$Antworten <- factor(Q11_dataframe$Antworten,
                                  levels = c("Digitisation ", 
                                             "Sustainability aspects", 
                                             "Cost saving",
                                             "Other"))
Q11_dataframe <- Q11_dataframe %>%                                  
   mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q11_dataframe$perc = round(Q11_dataframe$perc, digits = 2)

Q11_graph <- ggplot(data = Q11_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q11: Main reason for a decrease in travel activities") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q11_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 9
Q12_answers_summen <- list(sum(Antworten$Q12 == ("A1")), 
                           sum(Antworten$Q12 == ("A2")), 
                           sum(Antworten$Q12 == ("A3")), 
                           sum(Antworten$Q12 == ("-oth-")))


Q12_dataframe <- data.frame("Antworten" = c("General growth in revenue", 
                                            "Increasing importance of personal contact", 
                                            "Building professional networks",
                                            "Other"),
                            "Anzahl" = unlist(Q12_answers_summen))

Q12_dataframe$Antworten <- factor(Q12_dataframe$Antworten,
                                  levels = c("General growth in revenue", 
                                             "Increasing importance of personal contact", 
                                             "Building professional networks",
                                             "Other"))

Q12_dataframe <- Q12_dataframe %>%                                  
   mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q12_dataframe$perc = round(Q12_dataframe$perc, digits = 2)

Q12_n_total = sum(Q12_dataframe$Anzahl)

Q12_graph <- ggplot(data = Q12_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q12: Main reason for an increase in travel activities") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q12_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 10
Q13_answers_summen <- list(sum(Antworten$Q13 == ("A1")), 
                           sum(Antworten$Q13 == ("A2")))

Q13_dataframe <- data.frame("Antworten" = c("Yes", "No"),
                            "Anzahl" = unlist(Q13_answers_summen))

Q13_dataframe$Antworten <- factor(Q13_dataframe$Antworten,
                                  levels = c("Yes", "No"))
Q13_n_total <- sum(Q13_dataframe$Anzahl)

Q13_dataframe <- Q13_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q13_dataframe$perc = round(Q13_dataframe$perc, digits = 2)

Q13_graph <- ggplot(data = Q13_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q13:Have there been any benefits due to the cancellation of business trips \ndue to the pandemic and the associated increased use of video conferences?") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(hjust = .5, colour = "black"), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank()) +
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) + 
  scale_fill_manual(values=cbPalette)

ggsave("Q13_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 11
Q14_answers_summen <- list(sum(Antworten$Q14.SQ001.== ("Y")),
                           sum(Antworten$Q14.SQ002. == ("Y")),
                           sum(Antworten$Q14.SQ003. == ("Y")), 
                           sum(Antworten$Q14.SQ004. == ("Y")),
                           sum(Antworten$Q14.other. != ""))


Q14_dataframe <- data.frame("Antworten" = c("Lower costs", 
                                            "Environmental benefits", 
                                            "Greater flexibility in the organisation of work", 
                                            "Greater acceptance of virtual communication", 
                                            "Other"),
                            "Anzahl" = unlist(Q14_answers_summen))
Q14_dataframe$Antworten <- factor(Q14_dataframe$Antworten,
                                  levels = c("Lower costs", 
                                             "Environmental benefits", 
                                             "Greater flexibility in the organisation of work", 
                                             "Greater acceptance of virtual communication", 
                                             "Other"))

Q14_dataframe <- Q14_dataframe %>%                                  
  mutate(perc = (Anzahl / 26)*100) #add percentage column
                                    #26 weil 26 leute mit "ja" antworteten

Q14_dataframe$perc = round(Q14_dataframe$perc, digits = 2)

Q14_n_total = sum(Q14_dataframe$Anzahl)

Q14_graph <- ggplot(data = Q14_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten), colour = "black", stat = 'identity') + 
  ggtitle("Q14: What where the benefits?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q14_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 12
Q15_answers_summen <- list(sum(Antworten$Q15 == ("A1")), 
                           sum(Antworten$Q15 == ("A2")))

Q15_dataframe <- data.frame("Antworten" = c("Yes", "No"),
                            "Anzahl" = unlist(Q15_answers_summen))
Q15_dataframe$Antworten <- factor(Q15_dataframe$Antworten,
                                  levels = c("Yes", "No"))

Q15_dataframe <- Q15_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q15_dataframe$perc = round(Q15_dataframe$perc, digits = 2)

Q15_n_total = sum(Q15_dataframe$Anzahl)

Q15_graph <- ggplot(data = Q15_dataframe) + 
  geom_bar(aes(x= Antworten, y = perc, fill = Antworten), stat = 'identity', colour = "black") + 
  ggtitle("Q15: Have there been any disadvantages due to the cancellation of business trips \ndue to the pandemic and the associated increased use of video conferences?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(hjust = .5), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank() ,
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q15_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 13
Q16_answers_summen <- list(sum(Antworten$Q16.SQ001.== ("Y")),
                           sum(Antworten$Q16.SQ002. == ("Y")),
                           sum(Antworten$Q16.SQ003. == ("Y")), 
                           sum(Antworten$Q16.SQ004. == ("Y")),
                           sum(Antworten$Q16.SQ005. != ("")), 
                           sum(Antworten$Q16.other. != ("")))



Q16_dataframe <- data.frame("Antworten" = c("Security concerns (IT-related)", 
                                           "Internal communication problems", 
                                           "Communication problems with clients", 
                                           "Loss of personal contact", 
                                           "Loss in corporate culture", 
                                           "Other"),
                            "Anzahl" = unlist(Q16_answers_summen))

Q16_dataframe$Antworten <- factor(Q16_dataframe$Antworten,
                                  levels = c("Security concerns (IT-related)", 
                                             "Internal communication problems", 
                                             "Communication problems with clients", 
                                             "Loss of personal contact", 
                                             "Loss in corporate culture", 
                                             "Other"))

Q16_dataframe <- Q16_dataframe %>%                                  
  mutate(perc = (Anzahl / 12)*100) #add percentage column

Q16_dataframe$perc = round(Q16_dataframe$perc, digits = 2)

Q16_n_total = sum(Q16_dataframe$Anzahl)

Q16_graph <- ggplot(data = Q16_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q16: What were the disadvantages?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  theme(axis.text.x = element_text(angle = 45, colour ="black", hjust = 1), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank() ,
        plot.margin =margin(1,1,1,1.05,"cm"),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q16_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")




#Frage 14
Q04_answers_summen <- list(sum(Antworten$Q4 == ("A1")), 
                           sum(Antworten$Q4 == ("A2")),
                           sum(Antworten$Q4 == ("A3")))

Q04_dataframe <- data.frame("Antworten" = c("Yes",
                                            "No",
                                            "I do not know / No answer"),
                            "Anzahl" = unlist(Q04_answers_summen))

Q04_dataframe$Antworten <- factor(Q04_dataframe$Antworten,
                                  levels = c("Yes",
                                             "No",
                                             "I do not know / No answer"))

Q04_dataframe <- Q04_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q04_dataframe$perc = round(Q04_dataframe$perc, digits = 2)

Q04_n_total = sum(Q04_dataframe$Anzahl)

Q04_graph <- ggplot(data = Q04_dataframe) + 
  geom_bar(aes(x= Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q04: Does your company have a business travel policy? ") +
  theme(axis.text.x = element_text(hjust = .5, colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)


ggsave("Q04_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 15
Q05_answers_summen <- list(sum(Antworten$Q5.SQ001.== ("Y")), 
                           sum(Antworten$Q5.SQ002. == ("Y")),
                           sum(Antworten$Q5.SQ003. == ("Y")),
                           sum(Antworten$Q5.SQ004. == ("Y")), 
                           sum(Antworten$Q5.SQ005. == ("Y")), 
                           sum(Antworten$Q5.SQ006. == ("Y")), 
                           sum(Antworten$Q5.SQ007. == ("Y")), 
                           sum(Antworten$Q5.SQ008. == ("Y")))


Q05_dataframe <- data.frame("Antworten" = c("Before booking a trip, \nthe possibility of a virtual alternative is checked.",
                                            "There is a domestic flight ban and/or flights \nare only permitted from a certain \ndistance/flight time.", 
                                            "When choosing the means of transport, \npriority is given to public transport and rail.",
                                            "When using cars, the smallest possible \nrental car class or vehicles with hybrid or other \nalternative drive systems shall be used.", 
                                            "The carbon footprint of business travel is determined.", 
                                            "For unavoidable travel, we make compensation payments.", 
                                            "None of the above.", 
                                            "I do not know / Not specified."),
                            "Anzahl" = unlist(Q05_answers_summen))

Q05_dataframe[is.na(Q05_dataframe)] <- 0  #converts NA to 0

Q05_dataframe$Antworten <- factor(Q05_dataframe$Antworten,
                                 levels = c("Before booking a trip, \nthe possibility of a virtual alternative is checked.",
                                            "There is a domestic flight ban and/or flights \nare only permitted from a certain \ndistance/flight time.", 
                                            "When choosing the means of transport, \npriority is given to public transport and rail.",
                                            "When using cars, the smallest possible \nrental car class or vehicles with hybrid or other \nalternative drive systems shall be used.", 
                                            "The carbon footprint of business travel is determined.", 
                                            "For unavoidable travel, we make compensation payments.", 
                                            "None of the above.", 
                                            "I do not know / Not specified."))     #factor transformation to set order in x axis

Q05_dataframe <- Q05_dataframe %>%                                  
  mutate(perc = (Anzahl / 28)*100) #28, weil wir hier nicht die totale summe brauchen wegen multiple choice
#before:   mutate(perc = (Anzahl / sum(Anzahl))*100) #
Q05_dataframe$perc = round(Q05_dataframe$perc, digits = 2)

Q05_n_total = sum(Q05_dataframe$Anzahl)

Q05_graph <- ggplot(data = Q05_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q05: Which of the listed statements regarding business trips apply to your company? ") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), axis.title.x=element_blank(),
        axis.title.y=element_blank() ,legend.title = element_blank(), legend.position ="bottom") +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  guides(fill=guide_legend(nrow=4, byrow= TRUE)) # byrow orders legend by rows instead of by columns.

ggsave("Q05_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#Q05 gesamtzahl an leuten die mehrere statements hatten
Q05_totals <- list(sum(Antworten$Q5.total.score== ("0")), 
                           sum(Antworten$Q5.total.score == ("1")),
                           sum(Antworten$Q5.total.score == ("2")),
                           sum(Antworten$Q5.total.score == ("3")), 
                           sum(Antworten$Q5.total.score == ("4")), 
                           sum(Antworten$Q5.total.score == ("5")), 
                           sum(Antworten$Q5.total.score == ("6")), 
                           sum(Antworten$Q5.total.score == ("7")))


Q05_df_totals <- data.frame("Score" = c("0",
                                            "1",
                                            "2",
                                            "3",
                                            "4",
                                            "5",
                                            "6",
                                            "7"),
                            "Participants" = unlist(Q05_totals))

Q05_totals_graph <- ggplot(data = Q05_df_totals) + 
  geom_bar(aes(x=Score, y = Participants, fill = Score),colour = "black", stat = 'identity') + 
  ggtitle("Total score of survey participants for Q05") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +       #kürzt text auf 25 Zeichen
  scale_y_continuous(breaks=seq(0,12,by=2))+
  theme(axis.text.x = element_text(hjust = .5, colour = "black"), 
        legend.position = "none" )+
  labs(x="Total score", y="Survey participants")+
  scale_fill_manual(values=cbPalette)
ggsave("Q05_totals_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#Frage 16
Q051_anzahl <- list(sum(Antworten$Q51.SQ001. == ("A1")),
                    sum(Antworten$Q51.SQ002. == ("A1")),
                    sum(Antworten$Q51.SQ003. == ("A1")),
                    sum(Antworten$Q51.SQ004. == ("A1")),
                    sum(Antworten$Q51.SQ001. == ("A2")),
                    sum(Antworten$Q51.SQ002. == ("A2")),
                    sum(Antworten$Q51.SQ003. == ("A2")),
                    sum(Antworten$Q51.SQ004. == ("A2")),
                    sum(Antworten$Q51.SQ001. == ("A3")),
                    sum(Antworten$Q51.SQ002. == ("A3")),
                    sum(Antworten$Q51.SQ003. == ("A3")),
                    sum(Antworten$Q51.SQ004. == ("A3")))

Q051_dataframe = data.frame("Antworten" = c("Before booking a trip, the possibility \nof a virtual alternative is checked",
                                            "There is a domestic flight ban and/or flights \nare only permitted from a certain \ndistance/flight time.", 
                                            "When choosing the means of transport, \npriority is given to public transport and rail.",
                                            "When using cars, the smallest possible \nrental car class or vehicles with hybrid or other \nalternative drive systems shall be used."),
                            "Degree of bindingness" = c("Mandatory", 
                                                        "Mandatory",
                                                        "Mandatory",
                                                        "Mandatory",
                                                        "Recommendation", 
                                                        "Recommendation", 
                                                        "Recommendation", 
                                                        "Recommendation",
                                                        "I do not know / No answer",
                                                        "I do not know / No answer", 
                                                        "I do not know / No answer", 
                                                        "I do not know / No answer"),
                            "Anzahl" = unlist(Q051_anzahl))

Q051_dataframe$Degree.of.bindingness <- factor(Q051_dataframe$Degree.of.bindingness,
                                  levels = c("Mandatory", 
                                             "Recommendation",
                                             "I do not know / No answer"))


Q051_graph <- ggplot(data = Q051_dataframe, aes(Antworten, x= Antworten, y = Anzahl, fill = "Degree of bindingness")) + 
  geom_bar(aes(x=Antworten, y = Anzahl, fill = Degree.of.bindingness), colour = "black", stat = 'identity') + 
  labs(x="Antworten", y="Anzahl", title="Q051: To which degree are the aspects you have specified so far binding in your company? ", fill="Degree of bindingness") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        plot.margin = margin(1,1,1.5,2,"cm"),
  axis.text.x = element_text(hjust = 1,size=8, angle=30, colour = "black"))+
  scale_fill_manual(values=cbPalette)       



ggsave("Q051_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")
  


#Frage 17
Q06_answers_summen <- list(sum(Antworten$Q6 == ("A1")),
                           sum(Antworten$Q6 == ("A2")),
                           sum(Antworten$Q6 == ("A3")),
                           sum(Antworten$Q6 == ("A4")),
                           sum(Antworten$Q6 == ("-oth-")))

Q06_dataframe <- data.frame("Antworten" = c("Senior Management",
                                            "Head of department",
                                            "No approval necessary",
                                            "I do not know / No answer",
                                            "Other"),
                            "Anzahl" = unlist(Q06_answers_summen))

Q06_dataframe$Antworten <- factor(Q06_dataframe$Antworten,
                                  levels = c("Senior Management",
                                             "Head of department",
                                             "No approval necessary",
                                             "Other",
                                             "I do not know / No answer"))

Q06_dataframe <- Q06_dataframe %>%                                  
  mutate(perc = (Anzahl / sum(Anzahl))*100) #add percentage column

Q06_dataframe$perc = round(Q06_dataframe$perc, digits = 2)

Q06_n_total = sum(Q06_dataframe$Anzahl)
  
Q06_graph <- ggplot(data = Q06_dataframe) + 
  geom_bar(aes(x= Antworten, y = perc, fill = Antworten),colour = "black", stat = 'identity') + 
  ggtitle("Q06: Who in the company decides whether a business trip takes place?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 20),) +      
  theme(axis.text.x = element_text(hjust = 1,colour = "black", angle =30),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)

ggsave("Q06_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 18
Q17_answers_summen <- list(sum(Antworten$Q17.SQ001. == ("Y")),
                           sum(Antworten$Q17.SQ002. == ("Y")),
                           sum(Antworten$Q17.SQ003. == ("Y")), 
                           sum(Antworten$Q17.SQ004. == ("Y")), 
                           sum(Antworten$Q17.SQ005. == ("Y")), 
                           sum(Antworten$Q17.SQ006. == ("Y")),
                           sum(Antworten$Q17.other. != ("")))


Q17_dataframe <- data.frame("Antworten" = c("Prohibition of all business trips",
                                             "Prohibition of certain business trips",
                                             "Prohibition of business trips to risk countries",
                                             "Replacement by digital formats",
                                             "No, there are no guidelines on this",
                                             "I do not know /No answer",
                                             "Other"),
                             "Anzahl" = unlist (Q17_answers_summen))
                             

Q17_dataframe[is.na(Q17_dataframe)] <- 0  #converts NA to 0

Q17_dataframe$Antworten <- factor(Q17_dataframe$Antworten,
                                  levels = c("Prohibition of all business trips",
                                             "Prohibition of certain business trips",
                                             "Prohibition of business trips to risk countries",
                                             "Replacement by digital formats",
                                             "No, there are no guidelines on this",
                                             "Other",
                                             "I do not know /No answer"))

Q17_dataframe <- Q17_dataframe %>%                                  
  mutate(perc = (Anzahl / 28)*100) #add percentage column
# original:   mutate(perc = (Anzahl / sum(Anzahl))*100) 
Q17_dataframe$perc = round(Q17_dataframe$perc, digits = 2)

Q17_n_total = sum(Q17_dataframe$Anzahl)

Q17_graph <- ggplot(data = Q17_dataframe) + 
  geom_bar(aes(x=Antworten, y = perc, fill = Antworten), colour = "black", stat = 'identity') + 
  ggtitle("Q17: Should the contact restrictions be tightened again in the future due to increasing numbers
          \nof infections, do you have guidelines in your company on how to deal with business travel activities?") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 20)) + 
  theme(axis.text.x = element_text(colour = "black", angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank() ,
        legend.title = element_blank(), 
        legend.position ="bottom",
        plot.margin = margin(1,1,1.5,1, "cm"),
        plot.title = element_text(lineheight = 0.5)) +
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Antworten, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
    guides(fill=guide_legend(nrow=3, byrow= TRUE)) # byrow orders legend by rows instead of by columns.

ggsave("Q17_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Frage 19
Q18_dataframe <- data.frame("Sustainable aspects in the travel policy" = Antworten$Q18.SQ001.,
                        "Cost control" = Antworten$Q18.SQ002.,
                        "Digital work and communication" = Antworten$Q18.SQ003.,
                        "Employee safety" = Antworten$Q18.SQ004.,
                        "Employee health" = Antworten$Q18.SQ006.,
                        "Cybersecurity" = Antworten$Q18.SQ005.)

Q18_dataframe[Q18_dataframe == "99"] <- NA
Q18_dataframe[Q18_dataframe == ""] <- NA

#convert to numeric (if necessary)
#Q18_dataframe$Sustainable.aspects.in.the.travel.policy <- as.numeric(Q18_dataframe$Sustainable.aspects.in.the.travel.policy)
#Q18_dataframe$Cost.control <- as.numeric(Q18_dataframe$Cost.control)
#Q18_dataframe$Digital.work.and.communication <- as.numeric(Q18_dataframe$Digital.work.and.communication)
#Q18_dataframe$Employee.safety <- as.numeric(Q18_dataframe$Employee.safety)
#Q18_dataframe$Employee.health <- as.numeric(Q18_dataframe$Employee.health)
#Q18_dataframe$Cybersecurity <- as.numeric(Q18_dataframe$Cybersecurity)


Q18_results= data.frame("issue" = c("Sustainable aspects in the travel policy",
                                    "Cost control",
                                    "Digital work and communication",
                                    "Employee safety",
                                    "Employee health",
                                    "Cybersecurity"),
                        "mean" = c(mean(Q18_dataframe$Sustainable.aspects.in.the.travel.policy, na.rm = TRUE),
                                   mean(Q18_dataframe$Cost.control, na.rm = TRUE),
                                   mean(Q18_dataframe$Digital.work.and.communication, na.rm = TRUE),
                                   mean(Q18_dataframe$Employee.safety, na.rm = TRUE),
                                   mean(Q18_dataframe$Employee.health, na.rm = TRUE),
                                   mean(Q18_dataframe$Cybersecurity, na.rm = TRUE)),
                        "sd" = c(sd(Q18_dataframe$Sustainable.aspects.in.the.travel.policy, na.rm = TRUE),
                                 sd(Q18_dataframe$Cost.control, na.rm = TRUE),
                                 sd(Q18_dataframe$Digital.work.and.communication, na.rm = TRUE),
                                 sd(Q18_dataframe$Employee.safety, na.rm = TRUE),
                                 sd(Q18_dataframe$Employee.health, na.rm = TRUE),
                                 sd(Q18_dataframe$Cybersecurity, na.rm = TRUE)))

Q18_results$issue <- factor(Q18_results$issue,
                            levels = c("Cybersecurity",
                                       "Employee health",
                                       "Employee safety",
                                       "Digital work and communication",
                                       "Cost control",
                                       "Sustainable aspects in the travel policy"))

Q18_graph <- ggplot(Q18_results, aes(x=issue, y=mean)) + 
  ggtitle("Q18: How relevant will the following issues be for your company \nin relation to business travel in the next 2 to 3 years?")+
  theme(plot.title = element_text(hjust =.5))+
  labs(x = "Issue", y = "Relevance")+
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6))+
  theme( plot.margin = margin(1, 1, 1, 1, "cm"),)+
  coord_flip()
ggsave("Q18_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#Frage 20
Q19_dataframe = data.frame("Size" = c("Micro (< 10)",
                                      "Small (10 - 49)",
                                      "Medium (50 - 249)",
                                      "Large (> 250)"),
                           "Count" = c(sum(Antworten$Q19 < 10, na.rm=TRUE),
                                       sum(Antworten$Q19 >= 10 & Antworten$Q19 < 50, na.rm=TRUE),
                                       sum(Antworten$Q19 >= 50 & Antworten$Q19 < 250, na.rm=TRUE),
                                       sum(Antworten$Q19 >= 250)))

Q19_dataframe <- Q19_dataframe %>%                                  
  mutate(perc = (Count / sum(Count))*100) #add percentage column
Q19_dataframe$perc = round(Q19_dataframe$perc, digits = 2)                           

Q19_dataframe$Size <- factor(Q19_dataframe$Size,
                            levels = c("Micro (< 10)",
                                        "Small (10 - 49)",
                                        "Medium (50 - 249)",
                                        "Large (> 250)"))

Q19_graph <-  ggplot(data = Q19_dataframe) + 
  geom_bar(aes(x= Size, y = perc, fill = Size),colour = "black", stat = 'identity') + 
  ggtitle("Q19: Company size") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 20),) +      
  theme(axis.text.x = element_text(hjust = 1,colour = "black", angle =30),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  geom_text(aes(x=Size, y = perc, label = paste0(perc,"%"), vjust = 1.2)) +
  scale_fill_manual(values=cbPalette)
ggsave("Q19_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



 #Hypothesis 1
H01_qqplot <- ggplot(Antworten, aes(sample = Antworten$reisen_pro_ma)) + #erstellt QQplot
  stat_qq() + stat_qq_line()+
  labs(title = "QQ plot of trips per employee")
ggsave("H01_qqplot.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")

H01_histo <- ggplot(Antworten, aes(x=Antworten$reisen_pro_ma)) +
  geom_histogram(bins = 30, color = "black", fill = "grey")+
  labs(
    title = "Histogram of trips per employee",
    x = "Trips per employee",
    y = "Count")
ggsave("H01_histo.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")

shapiro.test(Antworten$reisen_pro_ma)
#Explanation
#H0 : the sample is normally distributed
#H1 : the sample is not normally distributed
#If p > a then accept H0 | a = significance level (normally 0.05)
#If p </= a then reject H0 in favor of H1.


#bei NV (Pearson)
cor.test(Antworten$reisen_pro_ma, Antworten$Q5.total.score, alternative = "less")
#Explanation
#Null hypothesis – There is no significant correlation between x and y 
#The alternative hypothesis – There is a significant correlation between x and y
#if p < 0.05 --> reject Null Hypothesis

H01_scatterplot <- ggplot(Antworten, aes(Antworten$reisen_pro_ma, Antworten$Q5.total.score))+
  geom_point() + 
  labs(x = "Annual trips per employee", y = "Sustainability score") +
  geom_smooth( method="lm", colour = "yellow")+
  scale_y_continuous(breaks=seq(0,6,by=1)) #changes y axis marks
ggsave("H01_scatterplot.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#bei keiner NV (Spearman)
cor.test(Antworten$reisen_pro_ma, Antworten$Q5.total.score, method = "spearman", alternative = "less")
#Explanation
#Null hypothesis – There is no significant correlation between x and y 
#The alternative hypothesis – There is a significant correlation between x and y
#if p < 0.05 --> reject Null Hypothesis



#Hypothesis 2

#erstellen von scatterplot mit yaxis(trips per ma) und xaxis(anteil von bahn)
H02_scatterplot_publictransport <- ggplot(Antworten, aes(Antworten$reisen_pro_ma, Antworten$Q3.SQ004_SQ001.))+
  geom_point() + 
  labs(x = "Annual trips per employee", y = "Percentage of trips by public transport / trains") +
  geom_smooth( method="lm", colour = "yellow") 
ggsave("H02_scatterplot_PT.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")

H02_scatterplot_car <- ggplot(Antworten, aes(Antworten$reisen_pro_ma, Antworten$Q5.total.score))+
  geom_point() + 
  labs(x = "Percentage of trips made with public transportation or trains in 2019", y = "Sustainability score") +
  geom_smooth( method="lm", colour = "yellow") 
ggsave("H02_scatterplot_PT.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")


#Chi squared test
#Explanation 
#H0: The variables are not associated i.e., are independent. (NULL Hypothesis)
#H1: The variables are associated, i.e., are dependent. (Alternative Hypothesis)
#If p < 0.05 --> reject Null Hypothesis


h02_df_chisq_normal <- data.frame("Bahn" = Antworten_normal$reisen.pro.ma * Antworten_normal$Q3.SQ004_SQ001./100,
                           "Auto" = Antworten_normal$reisen.pro.ma * Antworten_normal$Q3.SQ004_SQ002./100,
                           "Flugzeug"= Antworten_normal$reisen.pro.ma * Antworten_normal$Q3.SQ004_SQ003./100)


h02_df_chisq_sus <- data.frame("Bahn" = Antworten_sus$reisen.pro.ma * Antworten_sus$Q3.SQ004_SQ001./100,
                               "Auto" = Antworten_sus$reisen.pro.ma * Antworten_sus$Q3.SQ004_SQ002./100,
                               "Flugzeug"= Antworten_sus$reisen.pro.ma * Antworten_sus$Q3.SQ004_SQ003./100)

h02_df_chisq_combined <- data.frame("normie" = c(sum(h02_df_chisq_normal$Bahn),
                                   sum(h02_df_chisq_normal$Auto),
                                   sum(h02_df_chisq_normal$Flugzeug)),
                     "sus" = c(sum(h02_df_chisq_sus$Bahn),
                               sum(h02_df_chisq_sus$Auto),
                               sum(h02_df_chisq_sus$Flugzeug)))

h02_chisq_result <- chisq.test(h02_df_chisq_combined)



#Hypothesis 3
h03_df_q9 <- data.frame("visit" = Antworten$Q9.SQ001.,
                        "project" = Antworten$Q9.SQ002.,
                        "acquisition" = Antworten$Q9.SQ003.,
                        "internal" = Antworten$Q9.SQ004.,
                        "workshop" = Antworten$Q9.SQ005.)

h03_df_q9[h03_df_q9 == "99"] <- NA
h03_df_q9[h03_df_q9 == ""] <- NA
h03_df_q9[h03_df_q9 == "A4"] <- NA #das noch rausnehmen

h03_results= data.frame("event" = c("Attending an event/conference or further training",
                                    "Project business or regular meeting e.g. consultation", 
                                    "Acquisition, contract initiation in a face-to-face meeting",
                                    "Internal meeting or internal project", 
                                    "Workshop with clients"),
                        "mean" = c(mean(h03_df_q9$visit, na.rm = TRUE),
                                   mean(h03_df_q9$project, na.rm = TRUE),
                                   mean(h03_df_q9$acquisition, na.rm = TRUE),
                                   mean(h03_df_q9$internal, na.rm = TRUE),
                                   mean(h03_df_q9$workshop, na.rm = TRUE)),
                        "sd" = c(sd(h03_df_q9$visit, na.rm = TRUE),
                                 sd(h03_df_q9$project, na.rm = TRUE),
                                 sd(h03_df_q9$acquisition, na.rm = TRUE),
                                 sd(h03_df_q9$internal, na.rm = TRUE),
                                 sd(h03_df_q9$workshop, na.rm = TRUE)))

h03_results$event <- factor(h03_results$event,
                                  levels = c("Workshop with clients",
                                             "Internal meeting or internal project", 
                                             "Acquisition, contract initiation in a face-to-face meeting", 
                                             "Project business or regular meeting e.g. consultation", 
                                             "Attending an event/conference or further training"))
                                             
h03_graph <- ggplot(h03_results, aes(x=event, y=mean)) + 
  labs(x = "Events", y = "Satisfaction")+
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6))+
  theme( plot.margin = margin(1, 1, 5, 1, "cm"),)+
  coord_flip()
ggsave("H03_graph.png", path = "C:/Users/David/Documents/Uni/Sustainable Development/SS21/Master Thesis_neu/R/Graphs")



#Hypothesis 4
# --> siehe Q11