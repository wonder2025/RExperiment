array<-array(rnorm(1000,0,1) )
hist(array, col="grey", prob=TRUE)
lines(density(array), col="green",lwd="3")

states <- as.data.frame(state.x77[, c("Murder", "Population", 
                                      "Illiteracy", "Income", "Frost")])
