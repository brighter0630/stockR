library(ggplot2)
library(dplyr)
library(readODS)
library(reshape2)
library(wesanderson)

data <- as.data.frame(read_ods('./VITL_REVENUE_OI_20240908.ods'))

title = "Vital Farms의 매출과 영업이익"
subtitle = "매출과 영업이익은 비교적 꾸준하게 증가하는 모습이다."

data.melted <- melt(data)
ggplot(data=data.melted, aes(x=Period, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
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

