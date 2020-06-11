## first tidy tuesday!!!!
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(RColorBrewer)

data <- tt_load("2020-06-09")

science <- data$science
firsts <- data$firsts

breaks <- seq(1730, 2010, by = 10)

firsts <- firsts %>%
  mutate(year_bin =  breaks[findInterval(year, breaks)])

## favourites:
sub <- firsts[c(1,3,4,8,10,14,15,18,20,25,27,44,50,72,78,91,100,113,126,140,171,252,309,345,364,428,457),]

clean_person <- str_split_fixed(sub$person, pattern = "\\(", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = ", ", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = "\\[", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = "; ", n=2)[,1] 

clean_person[6] <- "John Gloucester, a former slave"
clean_person[11] <- "William Wells"

sub$person <- clean_person

sub$noteworthy <- paste(sub$accomplishment, sub$person, sep = " - ")
sub$accomplishment[13] <- "First African-American to vote in an election under the 15th Amendment"
sub$accomplishment[23] <- "First African-American women named Times Person of the Year"

beautiful_plot <- ggplot(firsts, aes(x = year_bin, fill = category)) +
  geom_bar(width = 9.5, position = "stack") +
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0.02), breaks = append(breaks-5, 2015), labels = append(as.character(breaks), "2020")) +
  scale_y_continuous(expand = c(0.02,0), limits = c(0, 150)) +
  labs(title = "Breaking the Colour Barrier:", subtitle = "A timeline of African American first achievements", fill = "A first accomplishment in:", x = "Year", caption = "Data from https://en.wikipedia.org/wiki/List_of_African-American_firsts") +
  theme(plot.title=element_text(size = 30, 
                                margin = margin(t = 30, r = 30, b = 7), 
                                hjust = 0.05, family = "Times"),
        plot.subtitle = element_text(size = 20, 
                                   margin = margin(t = 7, l = 40), 
                                   family = "Times", hjust = 0.05),
        axis.title.x = element_text(size = 13, family = "Times", margin = margin(t = 10, b = 20)),
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line("black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Times", size = 11),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = .5),
        axis.ticks.length.x.bottom = unit(.4, "cm"),
        axis.ticks.length.x.top = unit(.4, "cm"),
        legend.position = "bottom",
        legend.justification = "right",
        legend.title = element_text(size = 12, family = "Times"),
        legend.key.height = unit(.25, "cm"),
        legend.key.width = unit(.8, "cm"),
        legend.text = element_text(family = "Times", size = 12)) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_brewer(palette = 'RdBu') +
  annotate("text", x = 1730, y = 2, label = paste(sub$accomplishment[1],
                                                   sub$person[1], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1760, y = 3, label = paste(sub$accomplishment[2],
                                                sub$person[2], sep = "\n"), 
         size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1770, y = 5, label = paste(sub$accomplishment[3],
                                                  sub$person[3], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1780, y = 3, label = paste(sub$accomplishment[4],
                                                  sub$person[4], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1790, y = 4, label = paste(sub$accomplishment[5],
                                                  sub$person[5], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1800, y = 3, label = paste(sub$accomplishment[6],
                                                  sub$person[6], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1810, y = 2, label = paste(sub$accomplishment[7],
                                                  sub$person[7], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1820, y = 5, label = paste(sub$accomplishment[8],
                                                  sub$person[8], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1830, y = 3, label = paste(sub$accomplishment[9],
                                                  sub$person[9], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1840, y = 5, label = paste(sub$accomplishment[10],
                                                  sub$person[10], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1850, y = 7, label = paste(sub$accomplishment[11],
                                                  sub$person[11], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1860, y = 19, label = paste(sub$accomplishment[12],
                                                  sub$person[12], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1870, y = 16, label = paste(sub$accomplishment[13],
                                                  sub$person[13], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1880, y = 11, label = paste(sub$accomplishment[14],
                                                  sub$person[14], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1890, y = 11, label = paste(sub$accomplishment[15],
                                                  sub$person[15], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1900, y = 13, label = paste(sub$accomplishment[16],
                                                   sub$person[16], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1910, y = 13, label = paste(sub$accomplishment[17],
                                                   sub$person[17], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1920, y = 13, label = paste(sub$accomplishment[18],
                                                   sub$person[18], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1930, y = 13, label = paste(sub$accomplishment[19],
                                                   sub$person[19], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1940, y = 37, label = paste(sub$accomplishment[20],
                                                   sub$person[20], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1950, y = 40, label = paste(sub$accomplishment[21],
                                                   sub$person[21], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1960, y = 65, label = paste(sub$accomplishment[22],
                                                   sub$person[22], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1970, y = 55, label = paste(sub$accomplishment[23],
                                                   sub$person[23], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1980, y = 30, label = paste(sub$accomplishment[24],
                                                   sub$person[24], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 1990, y = 43, label = paste(sub$accomplishment[25],
                                                   sub$person[25], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 2000, y = 50, label = paste(sub$accomplishment[26],
                                                   sub$person[26], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) +
  annotate("text", x = 2010, y = 35, label = paste(sub$accomplishment[27],
                                                   sub$person[27], sep = "\n"), 
           size = 3, angle = 90, family = "Times", hjust = 0) 
  
  
ggsave(beautiful_plot, path = "./finished products/", filename = "01_2020-06-09_African-American-Firsts.png", device = "png", width = 11.97, height = 8.36, dpi = 500)
