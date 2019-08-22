library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(fields)

# Load & transform data from stocks, [Country, Amount]
stocks <- read.csv("stocks.csv")

# Make required variables
stocks <- stocks %>%
  arrange(Amount) %>%
  mutate(Country = factor(Country, levels = Country)) %>%
  mutate(ETF = "ETF") %>%
  mutate(Amount = Amount / sum(Amount)) %>%
  mutate(Percent = percent(Amount))

# Function for defining colors used in the graph
heatColors <- function(n, alpha = 1) {
  rev(designer.colors(n = n, col = brewer.pal(9, "Spectral")))
}

# Plot a stacked bar chart
ggplot(stocks, aes(x = ETF,
                   y = Amount,
                   fill = Country,
                   label = paste(Country, Percent))) +
  geom_col() +
  xlab("") +
  ylab("") +
  ggtitle("") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = heatColors(nrow(stocks)), name = "Country or sector") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE)
