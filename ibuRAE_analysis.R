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


