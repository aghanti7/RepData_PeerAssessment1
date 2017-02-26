library(ggplot2)
library(dplyr)

payments <- read.csv("payments.csv", stringsAsFactors = FALSE)

#transform currency columns to numeric (remove $  and ,)
payments$Average.Covered.Charges <- as.numeric(gsub("[$,]", "", payments$Average.Covered.Charges))
payments$Average.Total.Payments <- as.numeric(gsub("[$,]", "", payments$Average.Total.Payments))
#make state a factor; add column DRG as number extracted from DRG.Definition
payments <- transform(payments,
                      Provider.State = factor(Provider.State),
                      DRG = sub("(^[0-9]+) - .*", "\\1", DRG.Definition))

#group by DRG and then state, and summarize the totals
sub <- payments %>%
    group_by(DRG, Provider.State) %>%
    summarize(totCov = sum(Average.Covered.Charges), totPay = sum(Average.Total.Payments))

#get the top state in avg total payments for each DRG; sort desc by avg total payments
sub1 <- sub %>% group_by(DRG) %>% arrange(desc(totPay)) %>% slice(1) %>% arrange(desc(totPay))
#get top 5 DRGs
topDRGs <- head(sub1$DRG, 5)
#subset top DRGs
sub2 <- subset(sub, DRG %in% topDRGs)
#pick top 5 states in each DRG
topDrgStates <- sub2 %>% arrange(desc(totPay)) %>% slice(1:5)

#now that we have top 5 DRGs and top 5 states within them
topPayments <- subset(payments,
                      DRG %in% topDrgStates$DRG & Provider.State %in% topDrgStates$Provider.State)

g <- ggplot(topPayments, aes(Average.Covered.Charges/1000, Average.Total.Payments/1000, color = Provider.State)) +
    facet_wrap(DRG ~ Provider.State, scales = "free") +
    geom_point(alpha = 1/2) +
    geom_smooth(method = "lm") +
    xlab("Mean Covered Charges (in 1000's of $'s)") +
    ylab("Mean Total Payments (in 1000's of $'s)") +
    labs(title = "Mean Covered Charges vs. Mean Total Payments\nby Medical Condition & State\n(for top 5 conditions and top 5 states)")

ggsave("plot2.pdf", plot = g)
