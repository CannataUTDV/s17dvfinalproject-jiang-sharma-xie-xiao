require(readr)
require(plyr)

file_path = "../01 Data/clean_employment.csv"
df <- read.csv("https://query.data.world/s/cizs1r6ue2rpotke5qbfjqj1n",header=T);

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(df)) {
    df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- c("state_name", "state", "year")
measures <- setdiff(names(df), dimensions)

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
        df[m] <- data.frame(lapply(df[m], function(x) as.numeric(as.character(x)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
    }
}

write.csv(df, file_path, row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 0 || ! is.na(dimensions)) {
    for(d in dimensions) {
        sql <- paste(sql, paste(d, "varchar2(4000),\n"))
    }
}
if( length(measures) > 0 || ! is.na(measures)) {
    for(m in measures) {
        if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
        else sql <- paste(sql, paste(m, "number(38,4)\n"))
    }
}
sql <- paste(sql, ");")
#cat(sql)