library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(lsr)
library(xlsx)

mensIBUTable <- read.csv("~/biathlon/mensIBUTable.csv", stringsAsFactors=FALSE)
mensJuniorTable <- read.csv("~/biathlon/mensJuniorTable.csv", stringsAsFactors=FALSE)
mensWorldTable <- read.csv("~/biathlon/mensWorldTable.csv", stringsAsFactors=FALSE)
womensIBUTable <- read.csv("~/biathlon/womensIBUTable.csv", stringsAsFactors=FALSE)
womensJuniorTable <- read.csv("~/biathlon/womensJuniorTable.csv", stringsAsFactors=FALSE)
womensWorldTable <- read.csv("~/biathlon/womensWorldTable.csv", stringsAsFactors=FALSE)

names(mensWorldTable)[4] <- "url"

mensWorldTable$group <- "mensWC"
womensWorldTable$group <- "womensWC"
mensIBUTable$group <- "mensIBU"
womensIBUTable$group <- "womensIBU"
mensJuniorTable$group <- "mensJunior"
womensJuniorTable$group <- "womensJunior"

biathlon <- rbind(mensWorldTable, womensWorldTable, mensIBUTable, womensIBUTable, mensJuniorTable, womensJuniorTable)

biathlon$birthMonth <- month(biathlon$dob)
biathlon$birthQuarter <- 0

for (i in 1:length(biathlon$birthMonth)) {
  if (biathlon$birthMonth[i] >= 1 & biathlon$birthMonth[i] <= 3) {biathlon$birthQuarter[i] <- 1}
  if (biathlon$birthMonth[i] >= 4 & biathlon$birthMonth[i] <= 6) {biathlon$birthQuarter[i] <- 2}
  if (biathlon$birthMonth[i] >= 7 & biathlon$birthMonth[i] <= 9) {biathlon$birthQuarter[i] <- 3}
  if (biathlon$birthMonth[i] >= 10 & biathlon$birthMonth[i] <= 12) {biathlon$birthQuarter[i] <- 4}
}

biathlon$birthQuarterFactor <- as.factor(biathlon$birthQuarter)

biathlonSumQuarters <- biathlon %>%
  group_by(group) %>%
  summarise(Quarter1 = sum(birthQuarter == 1),
            Quarter2 = sum(birthQuarter == 2),
            Quarter3 = sum(birthQuarter == 3),
            Quarter4 = sum(birthQuarter == 4))

WCtotal <- data.frame(group = "WCtotal", biathlonSumQuarters[biathlonSumQuarters$group == "mensWC", 2:5] + biathlonSumQuarters[biathlonSumQuarters$group == "womensWC", 2:5], stringsAsFactors = FALSE)
IBUtotal <- data.frame(group = "IBUtotal", biathlonSumQuarters[biathlonSumQuarters$group == "mensIBU", 2:5] + biathlonSumQuarters[biathlonSumQuarters$group == "womensIBU", 2:5], stringsAsFactors = FALSE)
juniorTotal <- data.frame(group = "juniorTotal", biathlonSumQuarters[biathlonSumQuarters$group == "mensJunior", 2:5] + biathlonSumQuarters[biathlonSumQuarters$group == "womensIBU", 2:5], stringsAsFactors = FALSE)

biathlonSumQuarters <- rbind(biathlonSumQuarters, WCtotal, IBUtotal, juniorTotal)

View(biathlonSumQuarters)

biathlonChi <- biathlonSumQuarters %>%
  group_by(group) %>%
  mutate(Total = sum(Quarter1, Quarter2, Quarter3, Quarter4),
         NotInQuarter1 = Total - Quarter1,
         OddsIfQuarter1 = Quarter1/NotInQuarter1,
         NotInQuarter2 = Total - Quarter2,
         OddsIfQuarter2 = Quarter2/NotInQuarter2,
         NotInQuarter3 = Total - Quarter3,
         OddsIfQuarter3 = Quarter3/NotInQuarter3,
         NotInQuarter4 = Total - Quarter4,
         OddsIfQuarter4 = Quarter4/NotInQuarter4,
         ChiSquared = round(chisq.test(x = c(Quarter1, Quarter2, Quarter3, Quarter4), p = c(1/4, 1/4, 1/4, 1/4))[[1]], digits = 2),
         Df = chisq.test(x = c(Quarter1, Quarter2, Quarter3, Quarter4), p = c(1/4, 1/4, 1/4, 1/4))[[2]],
         P.value = round(chisq.test(x = c(Quarter1, Quarter2, Quarter3, Quarter4), p = c(1/4, 1/4, 1/4, 1/4))[[3]], digits = 2),
         CramersV = cramersV(x = c(Quarter1, Quarter2, Quarter3, Quarter4), p = c(1/4, 1/4, 1/4, 1/4)),
         Q1vsQ4 = OddsIfQuarter1/OddsIfQuarter4,
         Q1vsQ4lowerCI = exp(log(Q1vsQ4) - 1.96 * sqrt((1/Quarter1) + (1/Quarter4) + (1/NotInQuarter1) + (1/NotInQuarter4))),
         Q1vsQ4upperCI = exp(log(Q1vsQ4) + 1.96 * sqrt((1/Quarter1) + (1/Quarter4) + (1/NotInQuarter1) + (1/NotInQuarter4))),
         Q1vsQ3 = OddsIfQuarter1/OddsIfQuarter3,
         Q1vsQ3lowerCI = exp(log(Q1vsQ3) - 1.96 * sqrt((1/Quarter1) + (1/Quarter3) + (1/NotInQuarter1) + (1/NotInQuarter3))),
         Q1vsQ3upperCI = exp(log(Q1vsQ3) + 1.96 * sqrt((1/Quarter1) + (1/Quarter3) + (1/NotInQuarter1) + (1/NotInQuarter3))),
         Q1vsQ2 = OddsIfQuarter1/OddsIfQuarter2,
         Q1vsQ2lowerCI = exp(log(Q1vsQ2) - 1.96 * sqrt((1/Quarter1) + (1/Quarter2) + (1/NotInQuarter1) + (1/NotInQuarter2))),
         Q1vsQ2upperCI = exp(log(Q1vsQ2) + 1.96 * sqrt((1/Quarter1) + (1/Quarter2) + (1/NotInQuarter1) + (1/NotInQuarter2))),
         Quarter1Percentage = round((Quarter1/Total)*100, digits = 1),
         Quarter2Percentage = round((Quarter2/Total)*100, digits = 1),
         Quarter3Percentage = round((Quarter3/Total)*100, digits = 1),
         Quarter4Percentage = round((Quarter4/Total)*100, digits = 1))

View(oddsTable)

statsTable <- biathlonChi %>%
  select(group, ChiSquared, Df, P.value, CramersV, Q1vsQ4, Q1vsQ4lowerCI, Q1vsQ4upperCI, Q1vsQ3, Q1vsQ3lowerCI, Q1vsQ3upperCI, Q1vsQ2, Q1vsQ2lowerCI, Q1vsQ2upperCI)

write.csv(statsTable, "raeStats.csv")



biathlonChi_long <- gather(biathlonChi, quarter, percentage, Quarter1Percentage:Quarter4Percentage)
biathlonChi_long$group <- factor(biathlonChi_long$group, levels = c("womensJunior", "womensIBU", "womensWC", "mensJunior", "mensIBU", "mensWC", "juniorTotal", "IBUtotal", "WCtotal"))

biathlonViz <- biathlonChi_long %>%
  arrange(group) %>%
  select(group, quarter, percentage)

View(biathlonViz)

# Create womens graph

womensViz <- biathlonViz %>%
  filter(group %in% c("womensJunior", "womensIBU", "womensWC"))

levels(womensViz$group)[levels(womensViz$group) == "womensJunior"] <- "Junior"
levels(womensViz$group)[levels(womensViz$group) == "womensIBU"] <- "IBU"
levels(womensViz$group)[levels(womensViz$group) == "womensWC"] <- "World Cup"

annoWomen <- data.frame(quarter = "Quarter2Percentage", percentage = 27, 
                        lab = c("*"),
                        group = factor("Junior", levels = c("Junior", "IBU", "World Cup")))

annoWomen

womensRAE <- ggplot(data=womensViz, aes(x = quarter, y = percentage, fill = quarter)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "PuRd", direction = -1, name = "Quarter", labels = c("January-March", "April-June", "July-September", "October-December")) +
  facet_grid(. ~ group) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  xlab("Quarter") +
  ylab("Relative distribution (%)") +
  theme(legend.position="top") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  geom_text(data = annoWomen, label = "*", nudge_x = 0.5, size = 12) +
  ggtitle("Relativ age distribution across three competitive levels in women's biathlon") +
  theme(plot.title = element_text(face = "bold", size = 14))
  
womensRAE

# Create mens graph
  
mensViz <- biathlonViz %>%
  filter(group %in% c("mensJunior", "mensIBU", "mensWC"))

levels(mensViz$group)[levels(mensViz$group) == "mensJunior"] <- "Junior"
levels(mensViz$group)[levels(mensViz$group) == "mensIBU"] <- "IBU"
levels(mensViz$group)[levels(mensViz$group) == "mensWC"] <- "World Cup"  

mensRAE <- ggplot(data=mensViz, aes(x = quarter, y = percentage, fill = quarter)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Quarter", labels = c("January-March", "April-June", "July-September", "October-December")) +
  facet_grid(. ~ group) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  xlab("Quarter") +
  ylab("Relative distribution (%)") +
  theme(legend.position="top") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  ggtitle("Relativ age distribution across three competitive levels in men's biathlon") +
  theme(plot.title = element_text(face = "bold", size = 14))

mensRAE

# Create total graph

totalViz <- biathlonViz %>%
  filter(group %in% c("juniorTotal", "IBUtotal", "WCtotal"))

levels(totalViz$group)[levels(totalViz$group) == "juniorTotal"] <- "Junior"
levels(totalViz$group)[levels(totalViz$group) == "IBUtotal"] <- "IBU"
levels(totalViz$group)[levels(totalViz$group) == "WCtotal"] <- "World Cup"

annoTotal <- data.frame(quarter = "Quarter2Percentage", percentage = 27, 
                        lab = c("*"),
                        group = factor("Junior", levels = c("Junior", "IBU", "World Cup")))

totalRAE <- ggplot(data=totalViz, aes(x = quarter, y = percentage, fill = quarter)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Greens", direction = -1, name = "Quarter", labels = c("January-March", "April-June", "July-September", "October-December")) +
  facet_grid(. ~ group) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  xlab("Quarter") +
  ylab("Relative distribution (%)") +
  theme(legend.position="top") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  geom_text(data = annoTotal, label = "*", nudge_x = 0.5, size = 12) +
  ggtitle("Relativ age distribution across three competitive levels in biathlon") +
  theme(plot.title = element_text(face = "bold", size = 14))

totalRAE

# Combine plots
RAEplot <- grid.arrange(womensRAE, mensRAE, totalRAE, nrow = 3)

RAEplot
