nums <- sapply(train, is.numeric)
sort(abs(cor(train[ , nums])[,"Happy"]), decreasing = TRUE)
