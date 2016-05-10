library(ggplot2)

BankType <- c("Commercial", "Public", "Cooperative")
TotalAssets <- c(3233, 2205, 1083)
PercAssets <- c("50%", "34%", "16%")
TotalLending <- c(1139, 1426, 645)
PercLending <- c("36%", "44%", "20%")
BankSector <- data.frame(BankType, TotalAssets, PercAssets, TotalLending, PercLending)
BankSector$BankType <- as.factor(BankSector$BankType)


p5 <- ggplot(BankSector, aes(BankType, TotalAssets)) +
  geom_bar(stat = "identity", width=0.4) +
  geom_text(aes(label=PercAssets), vjust=2, colour="white") +
  theme_bw() +
  xlab("") +
  ylab("Total Assets (in '000 Euro)")

p6 <- ggplot(BankSector, aes(BankType, TotalLending)) +
  geom_bar(stat = "identity", width=0.4) +
  geom_text(aes(label=PercLending), vjust=2, colour="white") +
  theme_bw() +
  xlab("") +
  ylab("Total Lending (in '000 Euro)")