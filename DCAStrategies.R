# paths, functions & packages
path <- "C:/Users/shane/OneDrive/Documents/Projects/DCAStrategies"
setwd(path)
summary.statistics <- function(x) {
	result <- c(mean(x), sd(x), quantile(x, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)))
	return(result)
}

# Data
MSCIReturns <- read.delim(file = "MSCI Returns.txt", sep = "\t")

# Parameters
N_Paths <- 250														# Number of repetitions in Monte-Carlo simulation
HorizonSnapshots <- c(1, 2, 5, 10, 20, 25)							# Snapshots at which 
MaxMonths <- 12														# Time to invest the initial amount
DCA <- 1															# Regular Amount per month until the end
InitialAmount <- 100												# Initial Amount to invest

# Variables
set.seed(1)															# Seed for reproductibility
StartLowerBound <- N_Obs - 12 * max(HorizonSnapshots)				# The earliest starting point in the simulation
StartingPoints <- sample(StartLowerBound, N_Paths, replace = TRUE)	# Random starting points
N_Obs <- nrow(MSCIReturns)											# Number of observations in the data
InvestAmounts1 <- rep(1, 12 * max(HorizonSnapshots))

# Simulation
if(exists("ScenariosResults")) {
	rm(ScenariosResults)
}
for (k in 1:N_Paths) {
	StartingPoint <- StartingPoints[k]
	PortfolioValues <- MSCIReturns[StartingPoint:(StartingPoint - 1 + 12 * max(HorizonSnapshots)), ]
	PortfolioValues[, "MSCI_WORLD"] <- PortfolioValues[, "MSCI_WORLD"] / PortfolioValues[1, "MSCI_WORLD"]
	PortfolioValues[, "NUMBER_OF_SHARES"] <- NA
	for (i in 1:MaxMonths) {
		InvestAmounts2 <- c(rep(InitialAmount / i, i), rep(0, 12 * max(HorizonSnapshots) - 1))
		InvestAmounts <- InvestAmounts1 + InvestAmounts2
		PortfolioValues[1, "NUMBER_OF_SHARES"] <- InvestAmounts[1] / PortfolioValues[1, "MSCI_WORLD"]
		for (j in 2:nrow(PortfolioValues)) {
			PortfolioValues[j, "NUMBER_OF_SHARES"] <- PortfolioValues[j - 1, "NUMBER_OF_SHARES"] + InvestAmounts[j] / PortfolioValues[j, "MSCI_WORLD"]
		}
		PortfolioValues[, "TOTAL"] <- PortfolioValues[, "MSCI_WORLD"] * PortfolioValues[, "NUMBER_OF_SHARES"]
		ScenariosResultsTemp <- cbind(rep(i, length(HorizonSnapshots)), rep(k, length(HorizonSnapshots)), HorizonSnapshots, PortfolioValues[12 * HorizonSnapshots, "TOTAL"])
		colnames(ScenariosResultsTemp) <- c("CASE", "PATH", "HORIZON", "TOTAL")
		if (exists("ScenariosResults")) {
			ScenariosResults <- rbind(ScenariosResults, ScenariosResultsTemp)
		}
		if(!exists("ScenariosResults")) {
			ScenariosResults <- ScenariosResultsTemp
		}
	}
}

ScenariosResults <- as.data.frame(ScenariosResults)
Statistics <- with(ScenariosResults, aggregate(TOTAL, by = list(CASE, HORIZON), summary.statistics))
Statistics <- cbind(Statistics[, 1:2], Statistics[, 3])
colnames(Statistics) <- c("Case", "Horizon", "Mean", "StDev", paste("Q_", 100 * c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), "%", sep = ""))

write.table(file = "Scenario Results.txt", ScenariosResults, sep = "\t", row.names = FALSE)
write.table(file = "Statistics.txt", Statistics, sep = "\t", row.names = FALSE)