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

rangef <- function(x, format="%.1f"){
  min_ = sprintf(format, min(x, na.rm=T))
  max_ = sprintf(format, max(x, na.rm=T))
  rv = paste0("[",min_, "-",max_,"]")
  rv
}

################################################################################


# main handling function that takes in all args and outputs a two column char matrix
# with the 1st col = Variable, listing the unique variable (for linking columns)
# and the 2nd col = Value, the value to be displayed in the table
calcColumn <- function(checkboxes = list(NULL),
                       varsCat,
                       varsNumerical = NULL,
                       preserveOrder = list(NULL),
                       data,
                       strata = NULL,
                       format = "%.1f",
                       inc.range = T,
                       inc.sd = T) {
  
  # distinguish between general categorical vars and checkbox categorical vars
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    varsCatSelect <- varsCat[-which(varsCat %in% names(checkboxes))]
  } else {
    varsCatSelect <- varsCat
  }
  
  # row1 of returned data frame with columns: 
  row1 <- data.frame(Variable = "Total",
                     Subcategory = NA,
                     Value = format(nrow(data),big.mark=",",scientific=FALSE),
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
      
      suffix = ""
      if(inc.range==T){
        suffix = paste0(" ", rangef(data[[numvar]]))
      }
      if(inc.sd==T){
        suffix = paste0(suffix, " (", sdf(data[[numvar]]), ")")
      }
      Value = paste0(meanf(data[[numvar]]), suffix)
      
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
  if (length(names(checkboxes)) > 0) {
    
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
  out.varname = c(row1$Variable, row2$Variable)
  
  # handle numvars
  charmat = cbind(row2$Variable, row2$Value)
  charmat = rbind(c(row1[,c(1,3)]), charmat)
  
  # handle catvars
  for (catvar in unique(row3$Variable)){
    charmat = rbind(charmat, c(catvar, ""))
    out.varname = c(out.varname, catvar)
    idx_ = which(row3$Variable==catvar)
    for(idx in idx_){
      out.varname = c(out.varname, paste0("    ", row3$Subcategory[idx]))
      charmat = rbind(charmat,
                      c(paste0("   ",row3$Variable[idx],":", row3$Subcategory[idx]), row3$Value[idx])
      )
    }
  }
  
  
  # handle checkboxes
  
  for (chkvar in unique(row4$Variable)){
    charmat = rbind(charmat, c(chkvar, ""))
    out.varname = c(out.varname, chkvar)
    idx_ = which(row4$Variable==chkvar)
    for(idx in idx_){
      out.varname = c(out.varname, paste0("   ",row4$Subcategory[idx]))
      charmat = rbind(charmat,
                      c(paste0("   ",row4$Variable[idx],":", row4$Subcategory[idx]), row4$Value[idx])
      )
    }
  }
  
  # return charmat
  cbind(out.varname, charmat)
}

# overarching function to call calcColumn, combine results, and calc p-values
allTable <- function(checkboxes = list(NULL),
                     varsCat = NULL,
                     varsNumerical = NULL,
                     preserveOrder=list(NULL),
                     data,
                     strata = NULL,
                     format = "%.1f",
                     inc.range=T,
                     inc.sd=F){
  
  # handle vanilla case when no vars are specified
  if(is.null(varsNumerical) & is.null(varsCat) & is.null(names(checkboxes))){
    checkboxes=list()
    varsCat = c()
    varsNumerical= c()
    for (varname in colnames(data)){
      if(!is.null(strata)){if(varname == strata){next}}
      var.class = class(data[[varname]])
      if(var.class %in% c("factor", "character")){
        varsCat = c(varsCat, varname)
      } # end categorical
      else if (var.class %in% c("integer", "numeric")){
        # handle numerical case with only 0/1 as entries
        if (sort(unique(data[[varname]])) == c(0,1) ){
          checkboxes[["Binary variables"]] = c(checkboxes[["Binary variables"]], varname)
        }else{
          varsNumerical = c(varsNumerical, varname)
        }
      } # end numeric
      else if (var.class == "logical"){
        checkboxes[["Binary variables"]] = c(checkboxes[["Binary variables"]], varname)
      }# end logical
    } # end for loop
  }# end vanilla case
  
  
  # calculate overall table
  outmat = calcColumn(checkboxes,
                      varsCat,
                      varsNumerical,
                      preserveOrder,
                      data, format=format, inc.range=inc.range, inc.sd=inc.sd)
  colnames(outmat)[3] = "Overall"
  
  
  # return if no strata -- easy case
  if (is.null(strata)){
    outmat[,2] = outmat[,1]
    outmat = outmat[,-1]
    return(outmat)
  }
  
  # else handle stratified case
  
  # call calcColumn for each strata subset of data
  for(strat in sort(unique(data[[strata]])) ){
    idx.strat = which(data[[strata]] == strat)
    data.strat = data[idx.strat,]
    
    outmatc = calcColumn(checkboxes,
                         varsCat,
                         varsNumerical,
                         preserveOrder,
                         data.strat, format=format, inc.range=inc.range, inc.sd=inc.sd)
    colnames(outmatc)[3] = strat
    
    # merge into existing outmat
    idx = outmat[,2] %in% outmatc[,2]
    strat.col = rep("", nrow(outmat))
    strat.col[idx] = outmatc[,3]
    outmat = cbind(outmat,strat.col)
    colnames(outmat)[ncol(outmat)] = strat
  }
  
  outmat[,2] = outmat[,1]
  outmat = outmat[,-1]
  
  
  
  
  # generate p-values. ##################################
  tmp = c()
  
  # numerical variables
  for (numvar in varsNumerical){
    Variable = numvar
    p.value.unf = kruskal.test(data[[numvar]] ~ as.factor(data[[strata]]))$p.value
    p.value = prettyPval(p.value.unf)
    
    tmp = rbind(tmp, c(Variable, p.value, p.value.unf))
  }
  
  # categorical variables
  # distinguish between general categorical vars and checkbox categorical vars
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    varsCatSelect <- varsCat[-which(varsCat %in% names(checkboxes))]
  } else {
    varsCatSelect <- varsCat
  }
  
  # suppress chisq warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  for (catvar in varsCatSelect){
    Variable = catvar
    p.value.unf = chisq.test(table(data[[catvar]], data[[strata]]), correct=F)$p.value
    p.value = prettyPval(p.value.unf)
    
    tmp = rbind(tmp, c(Variable, p.value, p.value.unf))
  }
  
  
  # checkbox variables
  for (chkvar in names(checkboxes)){
    for (lvl in sort(checkboxes[[chkvar]])){
      Variable = lvl
      p.value.unf = chisq.test(table(data[[lvl]], data[[strata]]), correct=F)$p.value
      p.value = prettyPval(p.value.unf)
      
      tmp = rbind(tmp, c(Variable, p.value, p.value.unf))
    }
  }
  
  options(warn = oldw)
  
  idx.p = match(trimws(tmp[,1]), trimws(outmat[,1]))
  out.p = rep("", nrow(outmat))
  out.p[idx.p] = tmp[,2]
  
  outmat = cbind(outmat, out.p)
  colnames(outmat)[ncol(outmat)] = "p-value"
  
  
  p.values = data.frame(variable=tmp[,1], stringsAsFactors = F)
  p.values$p.value = as.numeric(tmp[,3])
  
  rv = list(outmat, p.values)
  
  return(rv)
  
}



