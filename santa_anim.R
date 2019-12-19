library(tidyverse)
library(viridis)
library(RColorBrewer)
library(gganimate)
library(png)

santa_img <- png::readPNG("santa.png") 

balance1 <- data.frame(
  amount = c(0)
)

balance2 <- data.frame(
  amount = round(c(128274, 2335, 2140, 6207, 1875, 2945, 819, 608,
             5063, 3586, 815, 157, 221, 168, 0.19), 2)
)

balance2 <- balance2 %>% 
  dplyr::arrange(-amount) %>% 
  dplyr::mutate(desc = c(1:nrow(balance2)))

balance <- dplyr::bind_rows(balance1, balance2)

balance$desc <- factor(balance$desc, levels = balance$desc)
balance$id <- seq_along(balance$amount)


balance$end <- round(cumsum(balance$amount), 2)
balance$start <- round(c(0, head(balance$end, -1)), 2)

cols <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122")

narr <- data.frame(
  state = c(1:16),
  text1 = c(
    "",
    "Running the workshop",
    "Negotiating with Elves",
    "Looking after reindeer",
    "Plowing snow",
    "Meeting children in Malls",
    "Professional Shopping",
    "Gift wrapping",
    "Letter reading",
    "Behavioral Investigating",
    "Sleigh Piloting",
    "List checking (twice)",
    "Cookie and Milk Tasting",
    "Gift placing (under tree)",
    "Chimney climbing",
    "Wishing a Merry Christmas"
  ),
  text2 = c("",
            "2912 hours of an Industrial Engineer",
            "182.5 hours of a Lobor Negotiator",
            "365 hours of a Farmworker",
            "180 Hours of a Highway Maintenance Worker",
            "168 Hours of a Customer Service Representative",
            "120 Hours of a Sales Related Worker",
            "168 Hours of a Packer /Packager",
            "100 Hours of a Correspondence Clerk",
            "30 Hours of a Private Detective",
            "10 Hours of an Airline Pilot",
            "30 Hours of a Bookkeeper",
            "10 Hours of an Agricultural Inspector",
            "10 Hours of a Shipper and Receiver",
            "10 Hours of a Chimney Sweeper",
            "0.01 Hours of a Public Announcer"
            ),
  text3 = c("", 
            "Hourly rate:  $44.05",
            "Hourly rate:  $34.01",
            "Hourly rate: $13.87",
            "Hourly rate: $19.92",
            "Hourly rate: $17.53",
            "Hourly rate: $19.46",
            "Hourly rate: $12.74",
            "Hourly rate: $18.75",
            "Hourly rate: $27.31",
            "Hourly rate: $81.52",
            "Hourly rate: $20.25",
            "Hourly rate: $22.10",
            "Hourly rate: $16.82",
            "Hourly rate: $15.73",
            "Hourly rate: $18.77"
            )
)


data1 <- balance[1, ]
data1$state <- 1
data1$color <- cols[1]

for (i in 2:nrow(balance)) {

  data_new <- balance[c(1:i), ]
  data_new$state <- i
  data_new$max <- max(data_new$end)
  data_new$color <- cols[1:i]
  
  data1 <- bind_rows(data1, data_new)

}

data <- dplyr::inner_join(data1, narr)


santa_plot <- ggplot(data) + 
  geom_rect(aes(xmin = 0, 
                xmax = 0.5, ymin = end, ymax = start, fill = color)
            ) +
  scale_fill_discrete() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  xlim(c(0, 1.5)) +
  ggtitle("How much should Santa Claus earn in today's labor market?") +
  theme(plot.title = element_text(hjust = 0.5, family = "Roboto Mono Bold", size = 24),
        plot.caption = element_text(size = 14, family = "Roboto Mono Medium")) +
  ggplot2::geom_text(x = 0.25, aes(y = max + 5000, label = paste("$", prettyNum(max, big.mark = ",")), fill = NULL),
                     data = data, size = 8, family = 'Roboto Mono Bold') +
  ggplot2::geom_text(x = 1, aes(y = 75000, label = text1, fill = NULL),
                     data = data, size = 8, family = 'Roboto Mono Bold') +
  ggplot2::geom_text(x = 1, aes(y = 65000, label = text2, fill = NULL),
                     data = data, size = 6, family = 'Roboto Mono') +
  ggplot2::geom_text(x = 1, aes(y = 55000, label = text3, fill = NULL),
                     data = data, size = 6, family = 'Roboto Mono') +
  annotation_raster(santa_img, xmin = 0.7, xmax = 1.3, ymin = 80000, ymax = 130000) +
  labs(caption = 'Data source: insure.com | Plot by @dr_keithmcnulty') +
  transition_states(state, state_length = 10) +
  enter_fade() + ease_aes('sine-in-out')


santa_anim <- gganimate::animate(santa_plot, 200, fps = 5, duration = 100, width = 1000, height = 800, renderer = ffmpeg_renderer())

save_animation(santa_anim, "santa_anim.mp4")

