library(ggplot2)
library(dplyr)
library(readODS)
library(reshape2)
library(wesanderson)

data <- as.data.frame(read_ods('./VITL_SHARE_NDEPS_20240908.ods'))
data$`Normalized Diluted EPS` <- as.numeric(data$`Normalized Diluted EPS`)

str(data)

title = "Vital Farms의 희석EPS와 희석유통주식수 추이"
subtitle = "EPS는 증가하고 있으며 동시에 희석유통주식수도 느리지만 꾸준히 증가하고 있다."
theme_set(theme_bw())
data.melted <- melt(data, id.vars = 'Period')
ggplot(data=data, aes(x = Period, y='Normalized Diluted EPS', group = 1)) +
  geom_line() + scale_y_continuous(breaks = seq(0, 1, 0.1))

data$'Normalized Diluted EPS'
data$Period

  guides(fill=guide_legend(title="")) +
  theme_light() +
  theme(plot.subtitle=element_text(size=14, margin= margin(5, 5, 5, 5)),
        plot.title = element_text(size=20, margin = margin(10, 10, 10, 10)),
        axis.text = element_text(size=12),
        legend.position = 'top',
        legend.justification = 'left',
        legend.text = element_text(size = 16)) +
  labs(title=title, subtitle = subtitle) + ylab('단위(백만달러)') + xlab('기간') +
  scale_fill_manual("legend", values=c("Revenue"=wes_palette("Darjeeling2", 1), "Operating Income"=wes_palette("Zissou1", 1, type = "continuous")))

