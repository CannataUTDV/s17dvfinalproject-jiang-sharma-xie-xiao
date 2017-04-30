require(readr)
require(plyr)

file_path = "../01 Data/clean.csv"
df <- read.csv("https://query.data.world/s/9d1cp2e3zczmcu4w3gk96mn81",header=T);
names(df)

str(df) 

dimensions <- c("state_name", "state", "year")
measures <- c("Population", "Employment", "Unemployment", "Unemployment.rate", "Marginally.Food.Insecure", "Food.Insecure", "Very.Low.Food.Secure", "Gross.State.Product", "Number.of.low.income.uninsured.children", "Percent.Low.Income.Unisured.Children", "Personal.income", "Workers..compensation")
df <- subset(df, select = append(dimensions, measures))



# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}
if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
    # Change ? to empty string in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="?",replacement= ""))
  }
}

na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
  for(m in measures) {
    print(m)
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], na2zero))
    df[m] <- data.frame(lapply(df[m], as.numeric)) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}

write.csv(df, file_path, row.names=FALSE, na = "")