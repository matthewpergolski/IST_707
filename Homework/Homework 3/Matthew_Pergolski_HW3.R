# Homework 3 | IST 707
# Matthew L. Pergolski

###########################################################################




###########################################################################

# Directions
    
    # HW3 Association Rules
    # 
    # For this homework, we are going to explore the bank data, available on the LMS, and an
    # accompanying description of the attributes and their values. The dataset contains
    # attributes on each person's demographics and banking information in order to determine
    # they will want to obtain the new PEP (Personal Equity Plan).
    # 
    # Your goal is to perform Association Rule discovery on the dataset using Weka.
    # 
    # First perform the necessary preprocessing steps required for association rule mining,
    # specifically the id field needs to be removed and a number of numeric fields need
    # discretization or otherwise converted to nominal.
    # 
    # Next perform association rule discovery on the preprocessed data. Experiment with
    # different parameters and preprocessing so that you get on the order of 20-30 strong rules,
    # e.g. rules with high lift and confidence which at the same time have relatively good
    # support. Don't forget to report in details what you have tried.
    #    
    # Finally, set PEP as the right hand side of the rules, and see what rules are generated.
    # 
    # Select the top 5 most "interesting" rules and for each specify the following:
    #   Support, Confidence and Lift values
    #   An explanation of the pattern and why you believe it is interesting based on the
    #     business objectives of the company.
    #   Any recommendations based on the discovered rule that might help the company
    #     to better understand behavior of its customers or to develop a business
    #     opportunity.
    # 
    # Note that the top 5 most interesting rules are most likely not the top 5 in the strong rules.
    # They are rules, that in addition to having high lift and confidence, also provide some non-
    #   trivial, actionable knowledge based on underlying business objectives.
    # 
    # To complete this assignment, write a short report describing your association rule mining
    # process and the resulting 5 interesting rules, each with their three items of explanation
    # and recommendations. For at least one of the rules, discuss the support, confidence and
    # lift values and how they are interpreted in this data set.
    # 
    # You should write the report as if you are working for a client who knows little about data
    # mining. Your report should give your client some insightful and reliable suggestions on
    # what kinds of potential buyers your client should contact, and convince your client that
    # your suggestions are reliable based on the evidence gathered from your experiment
    # results.
    # 
    # In more detail, your report should include:
    #   
    #   Description of preprocessing steps
    #   Description of parameters and experiments in order to obtain strong rules
    #   Give the top 5 most interesting rules and the 3 items listed above for each rule
    #   For one rule, discuss the support, confidence and lift numbers and how they were
    #     computed from the data
    # 

###########################################################################
 
# DATA EXPLORATION / PRE-PROCESSING
    
    # load and review data
    bank.df <- read.csv('/Users/pergolicious/Downloads/bankdata_csv_all.csv')
    str(bank.df)
    is.na(bank.df)
    
    # remove ID column
    bank.df <- bank.df[,-1]
    head(bank.df)
    
    # discretize age
    bank.df$age <- cut(bank.df$age, 
                       breaks = c(0,10,20,30,40,50,60,Inf), 
                       labels = c('kid', 'adolescent', '20s', '30s', '40s', '50s', 'Gettin` Up There'))
     
    str(bank.df)  
    head(bank.df)
    
    # discretize income
    
    min.income <- min(bank.df$income)
    min.income
    
    max.income <- max(bank.df$income)
    max.income
    
    bins <- 3
    
    mid <- (max.income - min.income) / bins
    mid
    
    mid * 3
    
    bank.df$income <- cut(bank.df$income,
                          breaks = c(min.income, mid, max.income, Inf),
                          labels = c('min', 'mid', 'max'))
        
    str(bank.df)   
    head(bank.df)            
    
    # Convert 'Children' to nominal / FACTOR
    typeof(bank.df$children)
    bank.df$children <- factor(bank.df$children)
    typeof(bank.df$children)
  
    str(bank.df)
            
      
    # Convert other variables to factor
    bank.df[,c(2:3,5:ncol(bank.df))] <- lapply(bank.df[,c(2:3,5:ncol(bank.df))], factor)
    str(bank.df)
            
###########################################################################
    
# MODELING / ANALYSIS
    
    # rule set 1
    bank.rules.1 <- apriori(bank.df, 
                          parameter = list(supp = 0.1, 
                                           conf = 0.75, 
                                           maxlen = 4), 
                          appearance = list(rhs = c('pep=YES','pep=NO')))
    head(inspect(bank.rules.1))
    
    options(digits = 2)
    
    bank.rules.1.sort <- sort(bank.rules.1, by = 'lift', decreasing = TRUE)
    View(inspect(bank.rules.1.sort))
    
    bank.rules.1.vis <- plot(bank.rules.1.sort[1:20], method = 'graph')
    bank.rules.1.vis
          
      
    # rule set 2
    bank.rules.2 <- apriori(bank.df, 
                            parameter = list(supp = 0.05, 
                                             conf = 0.86, 
                                             maxlen = 4), 
                            appearance = list(rhs = c('pep=YES','pep=NO')))
    head(inspect(bank.rules.2))
    
    options(digits = 2)
    
    bank.rules.2.sort <- sort(bank.rules.2, by = 'lift', decreasing = TRUE)
    View(inspect(bank.rules.2.sort))
    
    bank.rules.2.vis <- plot(bank.rules.2.sort[1:20], method = 'graph')
    bank.rules.2.vis
    
    
    # rule set 3
    
    bank.rules.3 <- apriori(bank.df, 
                            parameter = list(supp = 0.01, 
                                             conf = 1, 
                                             maxlen = 4), 
                            appearance = list(rhs = c('pep=YES','pep=NO')))
    head(inspect(bank.rules.3))
    
    options(digits = 2)
    
    bank.rules.3.sort <- sort(bank.rules.3, by = 'lift', decreasing = TRUE)
    View(inspect(bank.rules.3.sort))
    
    bank.rules.3.vis <- plot(bank.rules.3.sort[1:20], method = 'graph')
    bank.rules.3.vis
    
    
    # rule set 4
    bank.rules.4 <- apriori(bank.df, 
                            parameter = list(supp = 0.1, 
                                             conf = 0.7, 
                                             maxlen = 4), 
                            appearance = list(rhs = c('pep=YES','pep=NO')))
    head(inspect(bank.rules.4))
    
    options(digits = 2)
    
    bank.rules.4.sort <- sort(bank.rules.4, by = 'lift', decreasing = TRUE)
    View(inspect(bank.rules.4.sort))
    
    bank.rules.4.vis <- plot(bank.rules.4.sort[1:20], method = 'graph')
    bank.rules.4.vis
    
    
    
    # rule set 5
    
    bank.rules.5 <- apriori(bank.df, 
                            parameter = list(supp = 0.2, 
                                             conf = .55, 
                                             maxlen = 4), 
                            appearance = list(rhs = c('pep=YES','pep=NO')))
    head(inspect(bank.rules.5))
    
    options(digits = 2)
    
    bank.rules.5.sort <- sort(bank.rules.5, by = 'lift', decreasing = TRUE)
    View(inspect(bank.rules.5.sort))
    
    bank.rules.5.vis <- plot(bank.rules.5.sort[1:20], method = 'graph')
    bank.rules.5.vis
    
    # Five Interesting rules selection
    
    # Rule selection 1
    inspect(bank.rules.4.sort[12])
    plot(bank.rules.4.sort[12], method = 'graph')
    
    # Rule selection 2
    inspect(bank.rules.4.sort[11])
    plot(bank.rules.4.sort[11], method = 'graph')
    
    # Rule selection 3
    inspect(bank.rules.4.sort[10])
    plot(bank.rules.4.sort[10], method = 'graph')
    
    # Rule selection 4
    inspect(bank.rules.4.sort[1])
    plot(bank.rules.4.sort[1], method = 'graph')
    
    # Rule selection 5
    inspect(bank.rules.4.sort[33])
    plot(bank.rules.4.sort[33], method = 'graph')
    
    
    
    
    
    
    
    
    
    
    
        
        
