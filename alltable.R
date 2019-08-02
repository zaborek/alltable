# @ Nick Zaborek
# data created: 7-26-2019
#
# purpose: to create a pretty descriptive table easily
#
# args: df, group, grouplabels, specs
#
# output: 
#
# depends on: meanr(), prettyp(), genargs()



# generate debug data
set.seed(123)
myDat <- mtcars %>%
  mutate(Transmission = ifelse(am, "Manual", "Automatic"),
         Engine = ifelse(vs, "Straight", "V-Shaped"),
         Yamagata = rbinom(nrow(.), size = 1, prob = runif(1)),
         Zika = rbinom(nrow(.), size = 1, prob = runif(1)),
         Hanta = rbinom(nrow(.), size = 1, prob = runif(1)),
         Cough = rbinom(nrow(.), size = 1, prob = runif(1)),
         Malaise = rbinom(nrow(.), size = 1, prob = runif(1)),
         `Sore throat` = rbinom(nrow(.), size = 1, prob = runif(1))) %>%
  mutate_at(vars(Yamagata, Zika, Hanta, Cough, Malaise, `Sore throat`), list(~as.logical(.)))

data = myDat
varsCat = c("Engine", "cyl", "gear", "PCR results", "Symptoms")
varsNumerical = c("qsec", "mpg", "disp", "hp", "drat", "wt")
preserveOrder = c("cyl", "gear")
checkboxes = list("PCR results" = c("Yamagata", "Zika", "Hanta"),
                  "Symptoms" = c("Cough", "Malaise", "Sore throat"))
format="%.2f"



######################## helper functions ######################################

# returns a pretty p-value
prettyPval <- function (x) { ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))}

# returns a formatted, na.rm=T mean of a vector
meanf <- function (x, format="%.1f") {
  rv = mean(x, na.rm=T)
  rv = sprintf(format, rv)
  rv
}

# returns a formatted, na.rm=T sd of a vector
sdf <- function(x, format="%.1f"){
  rv = sd(x, na.rm=T)
  rv = sprintf(format,rv)
  rv
}

################################################################################


# main handling function that takes in all args and outputs a two column char matrix
# with the 1st col = Variable, listing the unique variable (for linking columns)
# and the 2nd col = Value, the value to be displayed in the table
calcColumn <- function(checkboxes = list(NULL),
                       varsCat,
                       varsNumerical = NULL,
                       preserveOrder,
                       data,
                       strata = NULL,
                       format = "%.1f") {
  
  # distinguish between general categorical vars and checkbox categorical vars
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    varsCatSelect <- varsCat[-which(varsCat %in% names(checkboxes))]
  } else {
    varsCatSelect <- varsCat
  }
  
  # row1 of returned data frame with columns: 
  row1 <- data.frame(Variable = "Total",
                     Subcategory = NA,
                     Value = comma(nrow(data)),
                     type = "n",
                     stringsAsFactors = FALSE)
  
  # handle numerical variables: create a row for each numerical variable with
  # variable = variable name
  # Subcategory = NA
  # Value = character expression of mean/median/(sd)/etc "5 (2.3)"
  # type = character value of type: "mean (sd)"
  if (length(varsNumerical) >= 1) {
    row2 = row1[F,]
    for (numvar in sort(varsNumerical)){
      Variable = numvar
      Subcategory = NA
      Value = paste0(meanf(data[[numvar]]), " (", sdf(data[[numvar]]), ")")
      type = "mean (sd)"
      rv = data.frame(Variable, Subcategory, Value, type, stringsAsFactors = F)
      row2 = rbind(row2, rv, stringsAsFactors=F)
    }
    ###
  } else { 
    row2 <- data.frame()
  }
  
  
  
  
  # handle categorical variables: create a row for each subcategory of a categorical variable with
  # variable = variable name (eg, color)
  # Subcategory = subcategory of variable (eg, blue)
  # n = number of occurances of that subcatgory as numeric
  # order2 = NA if cat var does is no specified in preserveOrder
  #          otherwise gets matching index of levels(data[[catvar]])
  #         as numeric
  
  row3 = data.frame(Variable=character(),
                    Subcategory=character(),
                    n=integer(),
                    order2=double(),
                    Value=character())
  
  for (catvar in sort(varsCatSelect)){
    for(lvl in sort(unique(data[[catvar]]))){
      Variable = catvar
      Subcategory = lvl
      n = length(which(data[[catvar]]==lvl))
      Value = paste0(n, " (", sprintf(format, 100*n/length(data[[catvar]])), ")")
      
      order2 = ifelse (Variable %in% preserveOrder,
                       which(Subcategory == levels(data[[catvar]])),
                       NA)
      
      rv = data.frame(Variable,
                      Subcategory,
                      n=paste(n),
                      order2,
                      Value, stringsAsFactors = F)
      row3 = rbind(row3, rv, stringsAsFactors=F)
    }
  }
  
  
  
  
  # Handle Checkbox variables
  # Variable = checkbox variable name
  # Subcategory = subcategory of categorical variable
  # n = count of subcategory (int)
  # order2 = ??
  # Value = n (%) (chr)
  
  row4 <- row3[F,]
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    
    for (chkvar in sort(names(checkboxes))){
      for (lvl in sort(checkboxes[[chkvar]])){
        Variable = chkvar
        Subcategory = lvl
        n = sum(data[[lvl]])
        order2 = 1
        Value = paste0(n, " (", sprintf(format, 100*n/length(data[[lvl]])), ")")
        
        rv = data.frame(Variable,
                        Subcategory,
                        n=paste(n),
                        order2,
                        Value, stringsAsFactors = F)
        row4 = rbind(row4, rv, stringsAsFactors=F)
      }
    }
  }
  
  
  # return as pretty char datafame
  # with columns: name, value
  
  # handle numvars
  charmat = cbind(row2$Variable, row2$Value)
  charmat = rbind(c(row1[,c(1,3)]), charmat)
  
  # handle catvars
  for (catvar in unique(row3$Variable)){
    charmat = rbind(charmat, c(catvar, ""))
    idx_ = which(row3$Variable==catvar)
    for(idx in idx_){
      charmat = rbind(charmat,
                      c(paste0("   ",row3$Variable[idx],":", row3$Subcategory[idx]), row3$Value[idx])
      )
    }
  }
  
  
  # handle checkboxes
  
  for (chkvar in unique(row4$Variable)){
    charmat = rbind(charmat, c(chkvar, ""))
    idx_ = which(row4$Variable==chkvar)
    for(idx in idx_){
      charmat = rbind(charmat,
                      c(paste0("   ",row4$Variable[idx],":", row4$Subcategory[idx]), row4$Value[idx])
      )
    }
  }
  
  # return charmat
  charmat
}

# overarching function to call calcColumn, combine results, and calc p-values
allTable <- function(checkboxes = list(NULL),
                     varsCat,
                     varsNumerical = NULL,
                     preserveOrder,
                     data,
                     strata = NULL,
                     format = "%.1f"){
  
  # calculate overall table
  outmat = calcColumn(checkboxes,
                      varsCat,
                      varsNumerical,
                      preserveOrder,
                      data, format=format)
  colnames(outmat)[2] = "Overall"
  
  
  # return if no strata -- easy case
  if (is.null(strata)){
    return(outmat)
  }
  
  # else handle stratified case
  
  # call calcColumn for each strata subset of data
  for( strat in sort(data[[strata]]) ){
    idx.strat = which(data[[strata]] == strat)
    data.strat = data[idx.strat,]
    
    outmat = calcColumn(checkboxes,
                        varsCat,
                        varsNumerical,
                        preserveOrder,
                        data.strat, format=format)
    colnames(outmat)[2] = strat
    
    # merge into existing
  }
  
  
}


# try to call


# easy case, works for no strata
allTable(data = data,
         varsCat = c("Engine", "cyl", "gear", "PCR results", "Symptoms"),
         varsNumerical = c("qsec", "mpg", "disp", "hp", "drat", "wt"),
         preserveOrder = c("cyl", "gear"),
         checkboxes = list("PCR results" = c("Yamagata", "Zika", "Hanta"),
                           "Symptoms" = c("Cough", "Malaise", "Sore throat")))












