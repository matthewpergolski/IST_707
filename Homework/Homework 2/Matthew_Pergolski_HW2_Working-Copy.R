# HW2 Instruction
# 
# This is a real job interview question from a data analysis company, and I doubt there is a
# standard answer to this question. So feel free to explore your story by using the data exploration
# and transformation techniques appropriately.
# 
# ----------instruction quote begins-------------
#   
#   Here is a small dataset for you to work with.
# 
# Each of 5 schools (A, B, C, D and E) is implementing the same math course this semester, with
# 35 lessons. There are 30 sections total. The semester is about 3/4 of the way through.
# 
# For each section, we record the number of students who are:
#   
#   very ahead (more than 5 lessons ahead)
# middling (5 lessons ahead to 0 lessons ahead)
# behind (1 to 5 lessons behind)
# more behind (6 to 10 lessons behind)
# very behind (more than 10 lessons behind)
# completed (finished with the course)
# 
# What's the story (or stories) in this data? Find it, and tell it visually and, above all, truthfully.
# 
# -----------instruction quote ends-

###################################################################



###################################################################

# INTRODUCTION

    # Read homework 2 file into RStudio and review data set
    
    hw.2.pathname <- '/Users/pergolicious/OneDrive - Syracuse University/Syracuse University/Courses/IST 707/Homework/Homework 2/Data Files/data-storyteller.csv'
    print(hw.2.pathname)
    hw.2.data.set <- read.csv(hw.2.pathname)
    head(hw.2.data.set)
    str(hw.2.data.set)
    summary(hw.2.data.set)
    
 # ABOUT THE DATA   
    
    # Pre-processing
    
    # 'Clean' Attribute Headers
    colnames(hw.2.data.set)
    colnames(hw.2.data.set) <- gsub("\\.", "", colnames(hw.2.data.set))
    colnames(hw.2.data.set) <- gsub('[0-9]+', '', colnames(hw.2.data.set))
    colnames(hw.2.data.set) <- gsub("([[:upper:]])", "\\2 \\1" , colnames(hw.2.data.set))
    colnames(hw.2.data.set) <- str_trim(colnames(hw.2.data.set))
    colnames(hw.2.data.set)
    
    str(hw.2.data.set)
    summary(hw.2.data.set)
    head(hw.2.data.set)
    
    hist(hw.2.data.set$`Very Ahead`)
    hist(hw.2.data.set$Middling)
    median(hw.2.data.set$Middling)
    
    hist(hw.2.data.set$Behind)
    median(hw.2.data.set$Behind)
    
    hist(hw.2.data.set$`More Behind`)
    median(hw.2.data.set$`More Behind`)
    
    hist(hw.2.data.set$`Very Behind`)
    median(hw.2.data.set$`Very Behind`)
    
    hist(hw.2.data.set$Completed)
    mean(hw.2.data.set$Completed)
    
    # Change 'School' and 'Section' attributes to factors instead of 'char' and 'num'
    hw.2.data.set$School <- sapply(hw.2.data.set$School, factor)
    hw.2.data.set$Section <- sapply(hw.2.data.set$Section, factor)
    str(hw.2.data.set)
    View(hw.2.data.set)

# DATA EXPLORATION
    
    head(hw.2.data.set)
    str(hw.2.data.set)
    summary(hw.2.data.set$School)
    summary(hw.2.data.set)
    boxplot(hw.2.data.set[,3:ncol(hw.2.data.set)])
    head(hw.2.data.set)
    str(hw.2.data.set[,c(1,(3:ncol(hw.2.data.set)))])
    
    # Group data by class
    
    agg.hw.2.data.set <- aggregate(hw.2.data.set[,3:ncol(hw.2.data.set)], 
                                   by = list(hw.2.data.set$School), 
                                   FUN = sum, 
                                   simplify = TRUE)       
    head(agg.hw.2.data.set)
    agg.hw.2.data.set$Total <- rowSums(agg.hw.2.data.set[,-1])
    agg.hw.2.data.set
    
    
    ggplot(hw.2.data.set, aes(x = `Very Behind`, group = School, fill = School)) +
      geom_histogram(position = "dodge") + theme_bw()
    
    # boxplot(agg.hw.2.data.set$Completed ~ agg.hw.2.data.set$Group.1, data = agg.hw.2.data.set)
    # plot(agg.hw.2.data.set$Total ~ agg.hw.2.data.set$Group.1, data = agg.hw.2.data.set)
    # plot(agg.hw.2.data.set$Total ~ agg.hw.2.data.set$Group.1, data = agg.hw.2.data.set)
    # line(agg.hw.2.data.set$Total, agg.hw.2.data.set$Group.1)
    
    
    # as.data.frame(t(agg.hw.2.data.set))
    # 
    # t.agg.hw.2.data.set <- as.data.frame(t(agg.hw.2.data.set))
    # t.agg.hw.2.data.set
    # 
    # names(t.agg.hw.2.data.set) <- t.agg.hw.2.data.set[1,]
    # t.agg.hw.2.data.set <- t.agg.hw.2.data.set[-1,]
    # t.agg.hw.2.data.set
    # View(t.agg.hw.2.data.set)
    # 
    # str(t.agg.hw.2.data.set)
    # t.agg.hw.2.data.set$A <- as.numeric(t.agg.hw.2.data.set$A)
    # t.agg.hw.2.data.set$B <- as.numeric(t.agg.hw.2.data.set$B)
    # t.agg.hw.2.data.set$C <- as.numeric(t.agg.hw.2.data.set$C)
    # t.agg.hw.2.data.set$D <- as.numeric(t.agg.hw.2.data.set$D)
    # t.agg.hw.2.data.set$E <- as.numeric(t.agg.hw.2.data.set$E)
    # str(t.agg.hw.2.data.set)
    # 
    # barplot(t.agg.hw.2.data.set)
    
    head(agg.hw.2.data.set)
    
    
    boxplot(agg.hw.2.data.set[,2:7])
    boxplot(hw.2.data.set[,3:ncol(hw.2.data.set)])
    
    
    summary(agg.hw.2.data.set)
    summary(agg.hw.2.data.set)
    str(agg.hw.2.data.set)
    View(agg.hw.2.data.set)

    describe(agg.hw.2.data.set)
    
    plot(x = agg.hw.2.data.set$Group.1, agg.hw.2.data.set$Total)

    ggplot(agg.hw.2.data.set) + 
      geom_line(group = 1, aes(x = Group.1, y = `Behind`)) +
      geom_point(group = 1, aes(x = Group.1, y = `Behind`)) +
      xlab("School") + ylab("`Behind` Students")
        
    # scale / normalize variables
    
    scale.agg.hw.2.data.set <- as.data.frame(scale(agg.hw.2.data.set[,3:ncol(agg.hw.2.data.set)]))
    scale.agg.hw.2.data.set$School <- agg.hw.2.data.set$Group.1
    scale.agg.hw.2.data.set$`Very Ahead` <- agg.hw.2.data.set$`Very Ahead`
    scale.agg.hw.2.data.set 
    summary(scale.agg.hw.2.data.set)

    ggplot(scale.agg.hw.2.data.set) + 
      geom_line(group = 1, aes(x = School, y = `Behind`)) +
      geom_point(group = 1, aes(x = School, y = `Behind`)) +
      xlab("School") + ylab("`Behind` Students")
    
    head(scale.agg.hw.2.data.set)
    boxplot(scale.agg.hw.2.data.set[,1:5])
    head(agg.hw.2.data.set)
    boxplot(agg.hw.2.data.set[,2:7])
    
        