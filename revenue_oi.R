library(ggplot2)
library(dplyr)
library(readODS)
library(reshape2)
library(wesanderson)

data <- as.data.frame(read_ods('./ONON_REVENUE_EBITDA_20240909.ods'))
data <- as.data.frame(read_ods('./ONON_Q_REVENUE_EBITDA_20240909.ods'))

title = "ONON의 연간 매출과 EBITDA"
subtitle = "매출과 EBITDA는 20% 이상의 속도로 증가하는 모습이다."

data.melted <- melt(data)
head(data.melted)
ggplot(data=data.melted, aes(x=factor(Period, levels = unique(Period)), y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
  guides(fill=guide_legend(title="")) +
  theme_light() +
  theme(plot.subtitle=element_text(size=14, margin= margin(5, 5, 5, 5)),
        plot.title = element_text(size=20, margin = margin(10, 10, 10, 10)),
        axis.text = element_text(size=12),
        legend.position = 'top',
        legend.justification = 'left',
        legend.text = element_text(size = 16)) +
  labs(title=title, subtitle = subtitle) + ylab('단위(백만달러)') + xlab('기간')

