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

## choose favourite accomplishments manually:
sub <- firsts[c(1,3,4,8,10,14,15,18,20,25,27,44,50,72,78,91,100,113,126,140,171,252,309,345,364,428,457),]

## clean up the names:
clean_person <- str_split_fixed(sub$person, pattern = "\\(", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = ", ", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = "\\[", n=2)[,1] 
clean_person <- str_split_fixed(clean_person, pattern = "; ", n=2)[,1] 
clean_person[6] <- "John Gloucester, a former slave"
clean_person[11] <- "William Wells"
sub$person <- clean_person

## clean up the accomplishments to shorten a few:
sub$accomplishment[13] <- "First African-American to vote in an election under the 15th Amendment"
sub$accomplishment[23] <- "First African-American women named Times Person of the Year"


beautiful_plot <- ggplot(firsts, aes(x = year_bin, fill = category)) +
  geom_bar(width = 9.5, position = "stack") +
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0.02), breaks = append(breaks-5, 2015), labels = append(as.character(breaks), "2020")) +
  scale_y_continuous(expand = c(0.02,0), limits = c(0, 170)) +
  labs(title = "Breaking the Colour Barrier:", subtitle = "A timeline of African American first achievements", fill = "A first achievement in:", x = "Year", caption = "Data from https://en.wikipedia.org/wiki/List_of_African-American_firsts") +
  theme(plot.title=element_text(size = 30, 
                                margin = margin(t = 30, r = 30, b = 7), 
                                hjust = 0.05, family = "Times"),
        plot.subtitle = element_text(size = 20, 
                                   margin = margin(t = 7, l = 40), 
                                   family = "Times", hjust = 0.05),
        axis.title.x = element_text(size = 13, family = "Times", margin = margin(t = 10, b = 15)),
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
  scale_fill_brewer(palette = 'RdBu') 
  
  
## get counts in each year bin
table <- as.data.frame(table(firsts$year_bin)) 
colnames(table) <- c("year_bin", "freq")
table$year_bin <- as.numeric(as.character(table$year_bin))
sub <- left_join(sub, table)

## create labels for each year_bin:
z = 1730
row <- 1
while (z < 2020) {
  if (z != 1740 & z != 1750) {
    beautiful_plot <-  beautiful_plot + annotate("text", x = sub$year_bin[row], y = sub$freq[row]+1, 
                                                 label = paste(sub$accomplishment[row],
                                                               sub$person[row], sep = "\n"), 
                                                 size = 3, angle = 90, family = "Times", hjust = 0)
    row = row + 1
  }
  z = z + 10
  
}

ggsave(beautiful_plot, path = "./finished products/", filename = "01_2020-06-09_African-American-Firsts.png", device = "png", width = 12.97, height = 8.36, dpi = 500)
