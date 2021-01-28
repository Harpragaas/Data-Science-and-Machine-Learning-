# PUERTO RICO HURRICANE MORTALITY

# author - Harpragaas Singh (Sept 2020)

library(tidyverse)
library(pdftools)
options(digits = 3) 

#extracting the pdf file 
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

# The file contains a report combining graph and table

#1. Creating a tidy dataset with each row representing one observation. The variables in this dataset will be year, month, day and deaths.
txt <- pdf_text(fn)

# txt is a character string of length 12. Each entry represents the text in each page. The mortality data is in there somewhere.


#2. Extracting the 9th page containg the details for the month of September:
x <- str_split(txt[9], "\n") #here x is a list with 1 entry
s <- x[[1]]
class(s) 
length(s) #here s is a character vector with 40 entries


# When inspecting the string we obtained above, we see a common problem: white space before and after the other characters. 

#3. Trimming the string to remove spaces at start or end of the strings
s <- str_trim(s)
s[1]    # print string, visually inspect last character

# We want to extract the numbers from the strings stored in s. However, there are a lot of non-numeric characters 
# that will get in the way. We can remove these, but before doing this we want to preserve the string with the column header, which includes the month abbreviation.

# 4. Preserving the string with the column header, which includes the month abbreviation.
header_index <- str_which(s, "2015")[1]
header_index

#5. Extracting two objects from header row : 1."month" : store the month
#                                            2."header": store the column names
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month # SEP
header[3] #2017

# Towards the end of the page defined by s you see a "Total" row followed by rows with other summary statistics.

# 6. create tail_index with the index of the "Total" entry
tail_index  <- str_which(s, "Total")
tail_index # value is 35

# 7. Because our PDF page includes graphs with numbers, some of our rows have just one number (from the y-axis of the plot).
# Creating an object n with the count of numbers in each row.
n <- str_count(s, "\\d+")
sum(n == 1) # 2 rows have single number in them

# 8. We are now ready to remove entries from rows that we know we don't need.
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s) # Only 30 entries remain 

# 9. Now we are ready to remove all text that is not a digit or space.
s <- str_remove_all(s, "[^\\d\\s]")

# 10. Convert s into a data matrix with just the day and death count data
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

# 11. Mean no. of deaths in September
tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)

mean(tab$"2015") # Mean np.of deaths per day in September'15 = 75
mean(tab$"2016") # Mean no.of deaths per day in September'16 = 79

#Hurricane Maria hit Puerto Rico on September 20,2017

mean(tab$"2017"[1:19]) 
# Mean no.of deaths per day in September'17 from 1st till 19th = 84
mean(tab$"2017"[20:30]) 
# Mean no.of deaths per day in September'17 from 20th till 30th = 122

# 12. Changing tab to tidy format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

# 13. Plotting the deaths vs day with color denoting year.
# We exclude 2018 since we have no data. We add a vertical line for 20th September,the day Hurricane Maria hit in 2017
tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()


# The graph shows:
#1. September 2015 and 2016 deaths by day are roughly equal to each other.
#2. After the hurricane in September 2017, there were over 100 deaths per day every day for the rest of the month.
#3. No days before September 20, 2017 have over 100 deaths per day.
