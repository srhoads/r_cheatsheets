library(tidyverse)
df <- culties <- cul <- read.csv('https://raw.githubusercontent.com/srhoads/thesis/culties/.gitignore/cul/culties.csv', na.strings = c("", NA)) # no need to change path; should run as is
# Using CulturalTies data
###### GGPLOT ####################################################################################################
library(ggplot2)

####### SCATTERPLOT #######
# mapping can be x = , y = , color = , shape = , size, alpha = *transparency of points*, stroke = *pts shrink as y increases*,
# ggplot(data = <DATA>) + <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>)) # scatterplot template

mpg # dataset; scatterplot examples
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) # geom_point
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = class, size = hwy)) 
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = class, size = hwy, stroke = cyl), color = "blue") # manually putting color outside aes() overrides color inside aes()

# facets display plots of subsets of data
# facet_wrap uses ONE variable to subset upon
# facet_grid uses TWO or ONE variable to facet upon
ggplot(mpg) + 
  geom_point(aes(displ, hwy)) + 
  facet_wrap(~ class, nrow = 2) # faceting plots by class of car
ggplot(mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class)  # facet_grid basically doing same as above      

# geom_smooth vs geom_point: thick line vs indiv pts
# linetype = *variable for which each level has a line; ie could have a line for each race*
ggplot(mpg) + 
  geom_smooth(aes(x = displ, y = hwy)) # geom_smooth
ggplot(mpg) + 
  geom_smooth(aes(x = displ, y = hwy, linetype = drv))

# multiple geoms at once
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
# can rewrite above by putting x & y inside ggplot() instead of in geoms
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

####### BAR GRAPH aka HISTOGRAM #######
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut )) # can't have a y coord bc just finding count/frequency

as_tibble(df)
enframe(df$Name1st)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
### GGPLOT RELEVANT FXNs! #######
### coord_cartesian() lets you zoom in and out if you have outliers especially
ggplot(df, aes(x = MCT1, y = diversity.Fr.T1)) + 
         geom_point(aes(size = diversity.St.T1, alpha = diversity.Sup.T1)) # without coord_cartesian
ggplot(df, aes(x = MCT1, y = diversity.Fr.T1)) + 
  geom_point(aes(size = diversity.St.T1, alpha = diversity.Sup.T1)) + 
  coord_cartesian(ylim = c(0,10)) # with coord_cartesian: zoomed out
###### DPLYR #########################################################################################
library(dplyr)
df <- culties <- cul <- read.csv('https://raw.githubusercontent.com/srhoads/thesis/culties/.gitignore/cul/culties.csv', na.strings = c("", NA)) # no need to change path; should run as is
flights <- nycflights13::flights
# Pick observations by their values ( filter() ).
   filter(df, Race != 7)
   filter(df, Race == 1 | 2 | 3 | 4 | 5 | 6, Gender != 2 | NA) 
   filter(df, HSStateRegion == "NorthEast", HSStateRegion != "South")
   filter(flights, month == 1, day == 1)
# Reorder the rows ( arrange() ).
    arrange(df, desc(CBT1), MCT1, desc(CBT2)) %>%
      select(Name1st, CBT1, MCT1, CBT2, MCT2)
# Rename variables with ( rename () )
    rename(df, HSName = HSName.1) # change HSName.1 to HSName
# Pick variables by their names ( select() ).
    select(df, 1:10) # selects columns 1:10 # select(df, 13) # selects column 13
    select(df, School:Race, HSState) # selects columns between school & race, and then HSState
    select(df, -(School:indegSupportT2), -MCT1) # selects columns that not NOT school through indeg and not mct1
     # Helper functions like...
      select(df, starts_with("MC")) # starts_with( , ignore.case = TRUE)
      select(df, ends_with("T2")) # ends_with( , ignore.case = TRUE)
      select(df, contains("divers", ignore.case = F)) # contains( , ignore.case = TRUE)
      select(df, num_range("MCT", 1:2, width = 1)) # num_range( , width = NULL) # with means they're x places away from one another
    MCT2 <- c("MCT1", "CBT2") # make vector w/ its name as a variable from df
      select(df, one_of(MCT2)) # gives you whatever MCT2 is stored as
          select(df, MCT2) # gives you just the one col value of MCT2
      select(df, everything()) # everything()
          select(df, MCT1, MCT2, HSStateRegion, everything()) # lets u put variables u want at beginning in different order/new order/reordering
# Create new variables with functions of existing variables ( mutate() ). 
      mutate(df,
             Age_corrected = Age + 18, # New column for age
             RaceWhite = ifelse(Race == 7, 1, 0), # New column for binary race
             AgeAbove22 = Age_corrected - 22)
# Only keep NEW variables ( transmute() )
      transmute(df,
                TotalCBMCT1 = CBT1 + MCT1,
                TotalCBT2MCT2 = CBT2 + MCT2)
      transmute(df,
                InOutDegRatioStT2 = indegstudyT2 %/% outdegstudyT2,# %/% is integer dividing sign/integer division sign
                RemainderInOutDegRatioStT2 = indegstudyT2 %% outdegstudyT2)
# Collapse many values down to a single summary ( summarise() ). or ( summarize() )
  summarise(df, aveMCT1 = mean(MCT1, na.rm = TRUE)) # gets u column average/colmean. na.rm inside math fxn
# Group observations by parameters when using summarise()/summarize() by using ( group_by() )
dvy <-  df %>%       # data &
        group_by(HSStateRegion, Gender) %>%  # group the data into rows based on columns region & gender
        summarise(Diversity_Ideology = mean(MCT1-CBT1, na.rm = TRUE),  # make a column for each group's ave mct1-cbt1
        n = n(),   # column for the count/total number of observations of each group
        prop = n()/nrow(df)) %>% # column for the proportion of people in group to total # of ppl
print()
# GGPLOT the table made above!
ggplot(dvy, aes(x = n, y = Diversity_Ideology)) + 
  geom_point(aes(shape = as.factor(Gender), color = HSStateRegion)) + 
  geom_abline() +
  geom_smooth()
# another group_by & ggplotting sample
    batting <- as_tibble(Lahman::Batting)
    batters <- batting %>% group_by(playerID) %>% summarise(
      ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE) )
    batters %>% 
      filter(ab > 100) %>% 
      ggplot(mapping = aes(x = ab, y = ba)) + 
        geom_point() + 
        geom_smooth(se = FALSE) 
### Summaries
    df %>%
      group_by(Gender, RaceName, HSType) %>%
      summarise(
        aveMCT1 = mean(MCT1),
        aveCBT1 = mean(CBT1),
        aveDIVfrT1 = mean(diversity.Fr.T1),
        NonWhiteLow = mean(PercentNonWhite[PercentNonWhite < .25], na.rm = TRUE),
        n = n(),
        prop = n()/nrow(df) 
      )
### Using count in dplyr
    df %>%
      count(RaceName, wt = indegstudyT1) # here you're counting the total cummulative indegree for each race of student
    df %>%
      count(RaceName)
    ### RcppRoll #### Rolling sums, rolling means, rolling products, rolling mins, maximums ##
  cumsum(df$ID)
  cummean()
  cummin()
  cummax()
### Ranking & rows functions
  y <- c(-2, 1, 1, 4, NA, 10, 7, 1)
  min_rank(y) # ranks the values # min_rank(desc(y)) # ranks the values descending
  row_number(y) # numbers the rows 1 to nrow
  dense_rank(y) # ranks by how many of each value
  percent_rank(y) # each value's percent of total
  cume_dist(y)
### gather() same observations in a column
table4a
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "totals") # gather all the indiv date data into unique dates & add up values
table4b
(tidy4b <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "totals"))
left_join(tidy4a, tidy4b, by = c("country", "year"))
df$County_BachelorOrHigher
(dfcountyeduc_gthr <- df %>%
  select(1:3, closeSupportT2:County_BachelorOrHigher) %>%
  gather(County_LessThanHSDiploma, County_HSDiplomaOnly, County_SomeCollegeOr2yrDeg, County_BachelorOrHigher, key = "Education", value = EducTotals))
### spread() is the opposite of gathering!
table2
table2 %>%
  spread(key = "type", value = "count")
table2 %>%
  spread(key = "year", value = "count")
### separate() splits variables that are stuck in one column: ie if you wanted a column for month and one for year from column 09-2017
table3   
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/") # separating rate into 2 columns: cases & population # splits wherever there isn't an alphanumeric character
(tidytable3 <- table3 %>% # lol
  separate(country, into = c("begin", "end"), sep = "i")) # separating rate into 2 columns: cases & population # splits wherever there isn't an alphanumeric character
### unite() is the opposite of separate
tidytable3 %>%

### FACTORS ##############################################################################################################################################################################

df %>%
  factor(HSLocale, levels = c("Ruralfringe", "Fringerural", "Fringetown", "Remotetown", "Smallcity", "Largesuburb", "Midsizesuburb", "Midsizecity", "Largecity"))
