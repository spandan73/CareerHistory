library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)

gannt = readxl::read_xls("career_outlook.xlsx")

acts <- unique(gannt$Task)
elms <- unique(gannt$Field)


g.gantt <- gather(gannt, "state","date", 3:4)%>%
  mutate(date = as.Date(date), Task = factor(Task, acts[length(acts):1]), Field = factor(Field,elms))

actcols <- c("red","#2e75B6","#bf9000","#7030A0","#cd6600","#2e7000","black")

ganntt_gg <- ggplot(g.gantt, aes(date,Task, colour = Field))+
  geom_line(linewidth = 10)+
  scale_color_manual(values = actcols,name = "Scientific Area")+
  labs(x = "Service year", y = NULL)+
  theme_pubclean()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,face = "bold", colour = "#660033", size = 12), 
        axis.title.x = element_text(size = 16,face = "bold", colour = "#660033"),
        axis.text.y = element_text(size = 12, face = "bold", colour = "#660033"),
        legend.text = element_text(size = 15,face = "bold", colour = "#660033"),
        legend.title =element_text(size = 16,face = "bold", colour = "#660033"))

ganntt_gg

ggsave(ganntt_gg, filename = "Career_timeline.png", dpi = 600, height = 6, width = 15, units = "in")
