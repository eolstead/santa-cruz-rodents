# Reading in Santa Cruz Rodent Data from Google Sheets
# Erin Olstead and Ellen Bledsoe
# January 2023

# PACKAGES ####

install.packages("googlesheets4") # run this line of code only once
library(googlesheets4) # need to run this line every time you run this script

# DATA ####

microsite <- read_sheet("https://docs.google.com/spreadsheets/d/11OT2-G6UCpaIVKsEvykI1HtpqDd9xdFLZJoLI3cf86g/edit#gid=0")
capture <- read_sheet("https://docs.google.com/spreadsheets/d/11OT2-G6UCpaIVKsEvykI1HtpqDd9xdFLZJoLI3cf86g/edit#gid=0", sheet = 2)


