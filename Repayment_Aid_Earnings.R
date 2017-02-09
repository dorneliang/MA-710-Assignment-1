# We will use the `dplyr` library for 
# the `select`, `mutate`, `group_by` and `summarize` functions
library(dplyr)

# We use the `readr` library for the `read_csv` function
library(readr)
csb.df = read_csv(paste0('C:/Users/Chengdong Liang/Desktop/MA 710/Assignment 1/CollegeScorecard_Raw_Data/', 'MERGED2014_15_PP.csv'))


# Read the following documentation to find variables of interest:
# https://collegescorecard.ed.gov/assets/FullDataDocumentation.pdf

# List the variable names of the dataset
names(csb.df)

#Aid variables at first
csb.df %>% # Do _not_ select `UNITID`
  select(PCTPELL, PCTFLOAN, DEBT_MDN:CUML_DEBT_P10
  ) %>% 
  {.} -> csb.aid.df
str(csb.aid.df)

csb.aid.df %>% 
  mutate_at(vars(), funs(as.numeric)
  ) %>%
  {.} -> csb.aid2.df

csb.aid2.df %>% 
  summarise_at(.cols = vars(),.funs = function(x) sum(is.na(x))
  )%>%
  {.} -> csb.aid3.df


class(csb.aid3.df)

csb.aid4.df <- subset(csb.aid3.df[1,] < 1000)
csb.aid4.df

# PCTPELL, PCTFLOAN, CUML_DEBT_N and DEBT_N
csb.df %>% # Do _not_ select `UNITID`
  select(PCTPELL, PCTFLOAN, DEBT_N, CUML_DEBT_N
  ) %>% 
  {.} -> csb.aid.variable.df
csb.aid.variable.df


#Earning variables at second
csb.df %>% # Do _not_ select `UNITID`
  select(COUNT_NWNE_P10:GT_25K_P9
  ) %>% 
  {.} -> csb.earning.df
str(csb.earning.df)

csb.earning.df %>% 
  mutate_at(vars(), funs(as.numeric)
  ) %>%
  {.} -> csb.earning2.df

csb.earning2.df %>% 
  summarise_at(.cols = vars(),.funs = function(x) sum(is.na(x))
  )%>%
  {.} -> csb.earning3.df


class(csb.earning3.df)

csb.earning4.df <- subset(csb.earning3.df[1,] < 1000)
csb.earning4.df

#All earning variables are NULL 100%


#Repayment variables at last
csb.df %>% # Do _not_ select `UNITID`
  select(CDR2, CDR3, RPY_1YR_RT:NOTFIRSTGEN_RPY_7YR_RT, REPAY_DT_MDN, REPAY_DT_N, RPY_1YR_N:NOTFIRSTGEN_RPY_7YR_N, RPY_3YR_RT_SUPP:NOTFIRSTGEN_RPY_3YR_RT_SUPP,CDR2_DENOM:CDR3_DENOM
  ) %>% 
  {.} -> csb.repayment.df
str(csb.repayment.df)

csb.repayment.df %>% 
  mutate_at(vars(), funs(as.numeric)
  ) %>%
  {.} -> csb.repayment2.df

csb.repayment2.df %>% 
  summarise_at(.cols = vars(),.funs = function(x) sum(is.na(x))
  )%>%
  {.} -> csb.repayment3.df


class(csb.repayment3.df)

csb.repayment4.df <- subset(csb.repayment3.df[1,] < 1100)
csb.repayment4.df

# CDR3, CDR3_DENOM
csb.df %>% # Do _not_ select `UNITID`
  select(CDR3, CDR3_DENOM
  ) %>% 
  {.} -> csb.repayment.variable.df
csb.repayment.variable.df


# Select all 6 variables
csb.df %>% # Do _not_ select `UNITID`
  select(CDR3, CDR3_DENOM, PCTPELL, PCTFLOAN, DEBT_N, CUML_DEBT_N
  ) %>%
  mutate_all(funs(as.numeric)
  ) %>%
  {.} -> csb.final.variable.df

csb.final.variable.df %>%
  summarise_all(.funs = function(x) mean(x, na.rm=TRUE)
  ) %>%
  {.} -> variable.mean
csb.final.variable.df %>%
  summarise_all(.funs = function(x) median(x, na.rm=TRUE)
  ) %>%
  {.} -> variable.median
csb.final.variable.df %>%
  summarise_all(.funs = function(x) var(x, na.rm=TRUE)
  ) %>%
  {.} -> variable.var
csb.final.variable.df %>%
  summarise_all(.funs = function(x) max(x, na.rm=TRUE)
  ) %>%
  {.} -> variable.max
csb.final.variable.df %>%
  summarise_all(.funs = function(x) min(x, na.rm=TRUE)
  ) %>%
  {.} -> variable.min

merge(variable.mean,variable.median)
variable.min
variable.max
temp1 <- merge(variable.mean, variable.median, all=TRUE)
temp2 <- merge(temp1,variable.var, all=TRUE)
temp3 <- merge(temp2, variable.max, all=TRUE)
temp4 <- merge(temp3, variable.min, all=TRUE)
temp4
