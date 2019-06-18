# Library's
library(stringr)
library(stopwords)
library(english)
library(gsubfn)

# token : The query string is split into collection of words based on space as delimiter and each word is treated as token
# token can be of two types : 1) Keyword token : this is used to set the flags , 2) Non-keyword token : we add this token to the group based on flags set.
# keywords : These are used to group tokens on matching. 
# When token is equal to keyword then subsequent tokens are grouped under the matched keyword group.
# group : This is the collection of tokens which falls under matched keyword.
# There are 3 groups : selectionTokensGroup,tableTokensGroup(fromTokenGroup),conditionTokensGroup(whereTokenGroup) 
# Flags : Before adding each non keyword token to any group we check the flags and decide where it belongs to. 
# stopwords : we sanitize these words from the given input string
# Aim : Inspect each token and find out which group does it belongs to and add it to the respective group later create sql query using the group's  


# This are Keywords assigned for SELECT tokens
select_keyword<-c("select","choose","get","create","*","fetch","show")

# This are Keywords assigned for FROM tokens
from_keyword<-c("from")

# This are Keywords assigned for WHERE tokens
where_keyword<-c("where","for","taken")

# list of keywords which will be eliminated if it matches the input tokens  
stopwords = c("the","is","value","deviation")

# Input query statement
# input<-readline(prompt = "Enter th query : ")
# cat("Enter the Query : ")
# input<-readline("stdin",n=1)

 # input<-"alerts from where twenty two days"
# l1 <- setNames(as.list(1:100), as.english(c(1:100)))
# input<-gsubfn("\\w+", l1, gsubfn("\\w+ \\w+", l1, input))
input<-"select alerts for last twenty days"
input<-tolower(input) #Convert uppercsae to lowercase
# input<-gsubfn("\\w+", setNames(as.list(1:100), as.english(1:100)), input)
l1 <- setNames(as.list(1:100), as.english(c(1:100)))
input<-gsubfn("\\w+", l1, gsubfn("\\w+ \\w+", l1, input))

input

input<-tolower(input) #Convert uppercsae to lowercase
input<-tolower(input) #Convert uppercsae to lowercase
input<-gsub("get all","\\*",input) # Replace get all by *
input<-gsub("yesterday","last day",input) # Replace yesterday by last day

input<-unlist(strsplit(input," ")) # Tokenize the sentence

input<-replace(input, grepl("^alert", input), "alerts") # Replace alerts,alert by alerts  

input<- input[!input %in% stopwords] # Command to remove stopwords from the input 

# Command to replace synonyms of mean,median,maximum,minimum and stdev by standard sql query word

 meanWords<-c("average","mean","avg") # synonyms of mean
 minWords<-c("minimum", "min","least") # synonyms of minimum
 maxWords<-c("max","maximum","highest") # synonyms of maximum
 median<-c("median") # synonyms of median
 stdv<-c("standard","stdev") # synonyms of standard devation

 dictionary<-list(meanWords, minWords, maxWords,median,stdv) # listing the above synonyms
 replaceWord<-c("avg(val)", "min(val)", "max(val)","median(val)","stdev(val)") # standard sql query replace word for synonyms

 for (index in 1:length(dictionary)) {
   input<-replace(input, grepl(paste(dictionary[[index]],collapse="|"),input), replaceWord[index]) 
   # command to find and replace dictionary word by replaceWord
 }                            #"average|mean|avg"
 input
 copyInp<-c()
 for(i in 1:(length(input))){
     copyInp<-c(copyInp,input[i])
     if (input[i] %in% replaceWord & input[i+1] %in% replaceWord){
     copyInp<-c(copyInp,",")
   }
 }
input<- copyInp

# Necessary flags set used for code
isSelectionToken<- FALSE # flag set for select keyword 
isTableToken<-FALSE # flag set for from keyword
isCondtionToken<-FALSE # flag set for where keyword
isAll <- FALSE # Flag to find all word
isLast <-FALSE # Flag to find last word in where condition
isLastWithInterval <- FALSE # Flag set to obtain last word 

# Empty array set to save obtained output
selectionTokensGroup <-c()
tableTokensGroup <-c()
conditionTokensGroup <-c()

selectionsentence <- ""
tablesentence <- ""
conditionsentence <-""
Query<-""

for(token in input) {
  token<-trimws(token)
  if (token %in% select_keyword) {  # check whether token is select keyword
    isSelectionToken<-TRUE # if token is selection token then selection flag is TRUE
    isTableToken<-FALSE  # if selecion flag is TRUE then from flag is FALSE
    isCondtionToken<-FALSE  # if selecion flag is TRUE then Where flag is FALSE
    print(isSelectionToken) # print the selection token word found
    next
  }
  if (token %in% from_keyword) { # check whether token is from keyword 
    isTableToken<-TRUE # if token is from token then from flag is TRUE
    isSelectionToken<-FALSE # if from flag is TRUE then selection flag is FALSE
    isCondtionToken<-FALSE # if from flag is TRUE then where flag is FALSE
    print(isTableToken)  # print the from token word found
    next
  }
  if (token %in% where_keyword) { # check whether token is where keyword
    isCondtionToken<-TRUE # if token is Where token then Where flag is TRUE
    isSelectionToken<-FALSE # if where flag is TRUE then selection flag is FALSE
    isTableToken<-FALSE # if where flag is TRUE then from flag is FALSE
    print(isCondtionToken) # print the where token word found
    next
  }
  if(isSelectionToken) { # if token is selection keyword
    if(token != "all") {
      selectionTokensGroup <-c(selectionTokensGroup,token) # then put the selection keywords into selectionTokensGroup
      next
    }
    
  }
  if(isTableToken) { # if token is from keyword
    if(token != "all") {
    tableTokensGroup <-c(tableTokensGroup,token) # then put the from keywords into tableTokensGroup
    next  
    }
  }
  if(isCondtionToken) { # if token is where keyword
    if(token == "last") { # look for last keyword in where tokens and match 
      isLast = TRUE # if last is found then flag isLAst as TRUE
      next
    }
    if(isLast) # if there is last word
    {
      if(isLastWithInterval) # if last words with next words
      {
        token <- paste(token,"'",sep="") # paste tokens and with cotes
        isLast <- FALSE # Turn islsat flag into False
      }
      else {
        if(!grepl("\\d",token)){ # if the token does not contain number 
          token<-paste("current_date() - interval '1",token)
          token<-paste(token,"'",sep="")# paste number before the token
          isLast = FALSE # and turn is last Flag into False
        }
        else {
          token <- paste("current_date() - interval '",token, sep="") # otherwise paste the obtained number in token and paste in the format
          isLastWithInterval = TRUE # Turn isLastWithInterval as TRUE
        }
      }
    }
    print(token) #command to print obtained tokens
    conditionTokensGroup <-c(conditionTokensGroup,token) # Save the obtained result in conditionTokensGroup 
    next
  }
}

 print(selectionTokensGroup) # Print selectionTokensGroup
 print(tableTokensGroup) # print from tokens
 print(conditionTokensGroup) # print where tokens

if(length(selectionTokensGroup)) { # obtain length of selectionTokensGroup 
  selectionsentence<-str_c(selectionTokensGroup,collapse=" ")
  
  Query<-paste("select",selectionsentence) # print select keyword followed by selectionTokensGroup  
} else {
  Query<-paste(Query,"select")
}
if(length(tableTokensGroup)){ # obtain length of fromTokens(tableTokensGroup)
  tablesentence<-str_c(tableTokensGroup,collapse=" ")
  Query<-paste(Query,"from",tablesentence) # print and save previous query along with tableTokensGroup with from keyword before from keywords 
} else {
  tablesentence<-paste("sensor_data")
  Query<-paste(Query,"from sensor_data") # otherwise print word "from sensor_data"
}
if(length(conditionTokensGroup)){ # obtain length of whereTokens(conditionTokensGroup)
  Query<-paste(Query,"where") # prints and save previous output followed by where word
  
  for ( token in conditionTokensGroup ) # look at tokens in conditionTokensGroup
  {
    
    if(token == "all") # if the Tokens is equal to all
    {
      isAll <- TRUE  # then turn isAll flag as TRUE
      next
    }
    if(isAll) 
    {
      isAll <- FALSE
      
      Query<-paste(Query," %",token,"%",sep="") # Save Query by inserting % symbols on either side of tokens along with previous obtained query
      conditionsentence <- paste(conditionsentence," %",token,"%",sep="")
    }
    else {
      Query<-paste(Query,token,sep=" ") # otherwise if there is no all paste normally along with previous obtained query
      conditionsentence <-  paste(conditionsentence,token,sep=" ")
      }
  }

}

Query

selectionsentence
tablesentence
conditionsentence

# r output to json
library(rjson)
report_suites <- list(selection=c(selectionsentence),table=c(tablesentence),condition=c(conditionsentence))
request.body <- toJSON(report_suites)


