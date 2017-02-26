library(ggplot2)

payments <- read.csv("payments.csv", stringsAsFactors = FALSE)

#transform currency columns to numeric (remove $  and ,)
payments$Average.Covered.Charges <- as.numeric(gsub("[$,]", "", payments$Average.Covered.Charges))
payments$Average.Total.Payments <- as.numeric(gsub("[$,]", "", payments$Average.Total.Payments))

#get data for NY city
ny <- subset(payments, Provider.City == "NEW YORK")

g <- ggplot(ny, aes(Average.Covered.Charges, Average.Total.Payments)) +
    xlim(0, 150000) + ylim(0, 35000) +
    geom_point(color = "steelblue", size = 3, alpha = 1/2) +
    geom_smooth(method = "lm") +
    xlab("Mean Covered Charges ($'s)") +
    ylab("Mean Total Payments ($'s)") +
    ggtitle("Mean Covered Charges vs. Mean Total Payments In New York")

ggsave("plot1.pdf", plot = g)
