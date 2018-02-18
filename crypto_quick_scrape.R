############################################################
###           Crypto Quick Scrape, by Matt Lunkes        ###
############################################################


#### Overview: ######################################## ####

# Set-up  

# Step 1: Read in the "View All" table
# Step 2: Clean up the name column
# Step 3: Clean out special characters
# Step 4: Create analysis columns
# Step 5: Export to a csv file

# Set-up  ############################################# ####
############################################################

# chooseCRANmirror(0)  #<-- when prompted, type in the number of your preferred mirror
# install.packages("rvest")
library("rvest")


# Step 1: Read in the "View All" table  ############### ####
############################################################

# Prep the url, read-IN the url, grab the table (via the xpath node), & convert to a data.frame
cmc_url <- "https://coinmarketcap.com/all/views/all/"
cmc <- cmc_url %>% read_html()
cmc <- cmc %>% html_nodes(xpath = '//*[@id="currencies-all"]')
cmc <- cmc %>% html_table() %>% data.frame()
cmc <- cmc[,-1] #<-- Strip out the index column

# Check out your new data.frame!
str(cmc)
head(cmc[,c(1,5)]) #<- in particular, look at Name & Supply


# Step 2: Clean up the name column  ################### ####
############################################################

# Clean up the name column
name <- strsplit(cmc$Name,"\n")
name <- sapply(name,`[`,2)
name <- gsub(" ","-",name)
cmc$Name <- name

head(cmc[,c(1,5)]) #<- Recheck!


# Step 3: Clean out special characters  ############### ####
############################################################

# Create separate columns to mark columns previously denoted w/ special characters ("*" and "?")
cmc$Not_Mineable <- grepl("\\*",cmc$Circulating.Supply)
cmc$Unk_Mkt_Cap <- grepl("\\?",cmc$Market.Cap)
cmc$Unk_Price <- grepl("\\?",cmc$Price)
cmc$Unk_Supply <- grepl("\\?",cmc$Circulating.Supply)

# Clean up "* Not Mineable" data out of Supply column & "?" out of MktCap & Price
cmc$Market.Cap <- gsub("\\?","",cmc$Market.Cap)
cmc$Price <- gsub("\\?","",cmc$Price)
cmc$Circulating.Supply <- gsub("\\?|\n|\\*","",cmc$Circulating.Supply)

# Create numeric columns without the "$" and "," characters
cmc$Mkt_Cap_sort <- (as.numeric(gsub("\\$|,","",cmc$Market.Cap)))
cmc$Price_sort <- as.numeric(gsub("\\$|,","",cmc$Price))
cmc$Circ_Supply_sort <- as.numeric(gsub(",","",cmc$Circulating.Supply))


# Step 4: Create analysis columns  ############# ####
############################################################

# Make a single category filter column for sub-cent coins
cmc$Price_below_a_cent <- cmc$Price_sort < 0.01
summary(cmc$Price_below_a_cent)

# Create a normalized price column, to see what each coin's price would be if it had Bitcoin's circ
cmc$Price_norm_by_Circ <- cmc$Mkt_Cap_sort / cmc$Circ_Supply_sort[1] 
# Similarly, see what each coin's price would be if it had Bitcoin's MktCap
cmc$Price_norm_by_Mkt_Cap <- cmc$Mkt_Cap_sort[1] / cmc$Circ_Supply_sort

# Check out the math across the current top 5:
head(cbind(cmc$Name,cmc$Price, cmc$Circulating.Supply, cmc$Market.Cap, cmc$Price_norm_by_Circ, cmc$Price_norm_by_Mkt_Cap),5)

# Loop through and categorize Circulating Supplies!
for(i in 1:length(cmc$Circ_Supply_sort))  {

    if ( !is.na(cmc$Circ_Supply_sort[i]) & cmc$Circ_Supply_sort[i] <= 1e7 ) {
    cmc$Circ_Category[i] <- "1: Below 10M"
    }

    else if ( !is.na(cmc$Circ_Supply_sort[i]) & cmc$Circ_Supply_sort[i] > 1e7 & cmc$Circ_Supply_sort[i] <= 1e8 ) {
    cmc$Circ_Category[i] <- "2: 10M - 100M"
    }

    else if ( !is.na(cmc$Circ_Supply_sort[i]) & cmc$Circ_Supply_sort[i] > 1e8 & cmc$Circ_Supply_sort[i] <= 1e9 ) {
    cmc$Circ_Category[i] <- "3: 100M - 1B"
    }

    else if ( !is.na(cmc$Circ_Supply_sort[i]) & cmc$Circ_Supply_sort[i] > 1e9 & cmc$Circ_Supply_sort[i] <= 1e12 ) {
    cmc$Circ_Category[i] <- "4: 1B - 1T"
    }

    else if ( !is.na(cmc$Circ_Supply_sort[i]) & cmc$Circ_Supply_sort[i] > 1e12 ) {
    cmc$Circ_Category[i] <- "5: Above 1T"
    }

    else {
    cmc$Circ_Category[i] <- "Unknown"
    }
}

# Table and plot out the results!
as.data.frame(table(cmc$Circ_Category))
bp <- barplot(table(cmc$Circ_Category), col = rgb(0,119/255,181/255), ylim = c(0,500), main = "Cryptos by Circulating Supply", xlab = "Category", ylab = "Count")
text(x = bp, y = table(cmc$Circ_Category), label = table(cmc$Circ_Category), pos = 3, cex = 0.8, col = "red")


# Step 5: Export to a csv file  ####################### ####
############################################################

# Review the structure of your data one last time
str(cmc)

# Set WD to Downloads
setwd("~/Downloads")
# Write it to a csv
write.csv(cmc, file = paste("cmc_scrape_",Sys.Date(),".csv", sep=""))


############################################################
###                          End                         ###
############################################################