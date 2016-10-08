tab <- table(x, useNA = "ifany")

tab <- data.frame(Name = getNameForOffenseCode(names(tab)), Code = names(tab), Freq = as.numeric(tab))
tab <- tab[order(tab$Freq, decreasing = TRUE),]

variableSummary <- rmdFormat(tab)
