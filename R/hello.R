#' first row to names
#'
#' @param firstrow first row to names
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- firstrow(x)

firstrow <- function(x) {
  x <- x %>% row_to_names(row_number=1)
  x
}


#' drop first column
#'
#' @param dropcol drop first column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- dropcol(x)

dropcol <- function(x) {
  x <- x[, -1]
  x
}


#' rename gender options
#'
#' @param renamegender rename gender options
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamegender(x)

renamegender <- function(x) {
  x[x=="* Another option" | x=="*Another option"] <- "Other"
  x
}


#' replace value "<5"
#'
#' @param replacefive replace value "<5"
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- replacefive(x)

replacefive <- function(x) {
  x[x=="< 5"] <- "5"
  x
}


#' change column names
#'
#' @param changecolnames change column names for staff cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- changecolnames(x)

changecolnames <- function(x) {
  x$Cohort <- x$'Staff Cohort'
  x
}


#' apply as.numeric to numeric values
#'
#' @param asnumeric apply as.numeric to numeric values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- asnumeric(x)

asnumeric <- function(x) {
  x$Headcount <- as.numeric(x$Headcount)
  x$WTE <- as.numeric(x$WTE)
  x
}


#' sum total of wte
#'
#' @param wtetotal sum total of wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- wtetotal(x)

wtetotal <- function(x) {
  x$all <- x %>%
    group_by(Institution) %>%
    mutate(all=sum(WTE))
  x
}


#' rename wte total column
#'
#' @param renamewtetotal rename wte total column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamewtetotal(x)

renamewtetotal <- function(x) {
    x$all <- x$all$all
    x
}


#' create new column with funding total for each institution
#'
#' @param wtefunding create new column with total funding
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- wtefunding(x)

wtefunding <- function(x) {
  x$WTE_funding <- x %>%
    group_by(Institution, Funding) %>%
    mutate(WTE_funding = sum(WTE))
  x
}


#' rename funding total column
#'
#' @param renamefundingtotal rename funding total column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamefundingtotal(x)

renamefundingtotal <- function(x) {
  x$WTE_funding <- x$WTE_funding$WTE_funding
  x
}


#' drop "Other" rows
#'
#' @param dropother drop "Other" rows
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- dropother(x)

dropother <- function(x) {
  x <- x[which(!x$Gender == "Other"),]
  x
}


#' pivot wider gender
#'
#' @param pivotwidergenderfunding pivot wider gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotwidergenderfunding(x)

pivotwidergenderfunding <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_wider(id_cols=c(Institution, Funding, WTE_funding, all), names_from=Gender, values_from=WTE, values_fn=list) %>%
    unnest(everything())
  x
}


#' create column for "Other" funding
#'
#' @param createotherfunding create column for "Other" funding
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- createotherfunding(x)

createotherfunding <- function(x) {
  x$Other <- (x$WTE_funding-(x$Female+x$Male))
  x
}


#' pivot longer gender
#'
#' @param pivotlongergender pivot longer gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotlongergender(x)

pivotlongergender <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_longer(cols=c("Female", "Male", "Other"))
  x
}


#' rename gender column
#'
#' @param renamegendercol rename gender column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamegendercol(x)

renamegendercol <- function(x) {
  x$gender <- x$name
  x$name <- NULL
  x
}


#' calculate percent of total
#'
#' @param percent calculate percent of total
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percent(x)

percent <- function(x) {
  x$all_pc <- (x$pc_all*100)
  x
}


#' round percent values
#'
#' @param roundvalues round percent values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundvalues(x)

roundvalues <- function(x) {
  x$valueround <- base::round(x$pc_all*100, digits=0)
  x
}


#' create percentage label
#'
#' @param percentlabel create percentage label
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percentlabel(x)

percentlabel <- function(x) {
  x$pclabel <- gsub(" ", "", paste((x$valueround), "%"))
  x
}


#' remove values for labels <5%
#'
#' @param pclabel remove values for labels <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pclabel(x)

pclabel <- function(x) {
  x$pclabel[x$pc_all<0.05]=NA
  x
}


#' collapse cohort
#'
#' @param collapsecohort collapse cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapsecohort(x)

collapsecohort <- function(x) {
  x <- x %>%
    group_by(Institution, Funding, Gender) %>%
    summarise(across(c(Headcount, WTE), sum))
  x
}


#' rename funding values
#'
#' @param renamefunding rename funding values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamefunding(x)

renamefunding <- function(x) {
  x[x=="Core-funded"] <- "CF"
  x[x=="Non-Core funded"] <- "NCF"
  x
}


#' pivot wider funding
#'
#' @param pivotwiderfunding pivot wider funding
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotwiderfunding(x)

pivotwiderfunding <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_wider(id_cols=c(Institution, WTE_funding, gender, all), names_from=Funding, values_from=value, values_fn=list) %>%
    unnest(everything())
  x
}


#' create gender percentage columns
#'
#' @param percentfunding create gender percentage columns
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percentfunding(x)

percentfunding <- function(x) {
  x$pc_CF <- (x$CF/x$WTE_funding)
  x$pc_NCF <- (x$NCF/x$WTE_funding)
  x
}


#' drop wte funding column
#'
#' @param dropcol2 drop wte funding column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- dropcol2(x)

dropcol2 <- function(x) {
  x <- x[, -2]
  x
}


#' merge rows
#'
#' @param mergerows merge rows
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- mergerows(x)

mergerows <- function(x) {
  x <- x %>%
    group_by(Institution, gender) %>%
    fill(everything(), .direction = "down") %>%
    fill(everything(), .direction = "up") %>%
    slice(1)
  x
}


#' create column for funding wte total percent
#'
#' @param fundingtotalpc create column for funding wte total percent
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- fundingtotalpc(x)

fundingtotalpc <- function(x) {
  x$all_pc <- (1-(x$pc_CF+x$pc_NCF))
  x
}


#' round percent values
#'
#' @param roundfundingvalues round percent values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundfundingvalues(x)

roundfundingvalues <- function(x) {
  x$CFvalueround <- base::round(x$pc_CF*100, digits=0)
  x$NCFvalueround <- base::round(x$pc_NCF*100, digits=0)
  x
}


#' create CF percentage labels
#'
#' @param CFpercentlabel create CF percentage labels
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- CFpercentlabel(x)

CFpercentlabel <- function(x) {
  x$CFpclabel <- gsub(" ", "", paste((x$CFvalueround), "%"))
  x
}


#' create NCF percentage labels
#'
#' @param NCFpercentlabel create NCF percentage labels
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- NCFpercentlabel(x)

NCFpercentlabel <- function(x) {
  x$NCFpclabel <- gsub(" ", "", paste((x$NCFvalueround), "%"))
  x
}


#' remove CF labels for values <5%
#'
#' @param CFpclabel remove CF labels for values <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- CFpclabel(x)

CFpclabel <- function(x) {
  x$CFpclabel[x$pc_CF<0.05]=NA
  x
}


#' remove NCF labels for values <5%
#'
#' @param NCFpclabel remove NCF labels for values <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- NCFpclabel(x)

NCFpclabel <- function(x) {
  x$NCFpclabel[x$pc_NCF<0.05]=NA
  x
}


#' create column with total percent of wte per group (gender)
#'
#' @param allpercentfunding create column with total percent of wte per group (gender)
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- allpercentfunding(x)

allpercentfunding <- function(x) {
  x$all_pc <- ((x$CF+x$NCF)/x$all)
  x
}


#' round all values
#'
#' @param roundallvalues round all values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundallvalues(x)

roundallvalues <- function(x) {
  x$all_pcround <- base::round(x$all_pc*100, digits=0)
  x
}


#' create all percent label
#'
#' @param allpercentlabel create all percent label
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- allpercentlabel(x)

allpercentlabel <- function(x) {
  x$allpclabel <- gsub(" ", "", paste((x$all_pcround), "%"))
  x
}


#' collapse cohort funding
#'
#' @param collapsefunding collapse cohort funding
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapsefunding(x)

collapsefunding <- function(x) {
  x <- x %>%
    group_by(Institution, Cohort, Gender) %>%
    summarise(across(c(Headcount, WTE), sum))
  x
}


#' create new column with cohort funding total for each institution
#'
#' @param wtecohort create new column with cohort funding total for each institution
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- wtecohort(x)

wtecohort <- function(x) {
  x$WTE_cohort <- x %>%
    group_by(Institution, Cohort) %>%
    mutate(WTE_cohort = sum(WTE))
  x
}


#' rename cohort funding total column
#'
#' @param renamecohorttotal rename cohort funding total column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamecohorttotal(x)

renamecohorttotal <- function(x) {
  x$WTE_cohort <- x$WTE_cohort$WTE_cohort
  x
}


#' pivot wider gender cohort
#'
#' @param pivotwidergendercohort pivot wider gender cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotwidergendercohort(x)

pivotwidergendercohort <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_wider(id_cols=c(Institution, Cohort, WTE_cohort, all), names_from=Gender, values_from=WTE, values_fn=list) %>%
    unnest(everything())
  x
}


#' create column for "Other" cohort
#'
#' @param createothercohort create column for "Other" cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- createothercohort(x)

createothercohort <- function(x) {
  x$Other <- (x$WTE_cohort-(x$Female+x$Male))
  x
}


#' pivot wider cohort
#'
#' @param pivotwidercohort pivot wider cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotwidercohort(x)

pivotwidercohort <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_wider(id_cols=c(Institution, WTE_cohort, gender, all), names_from=Cohort, values_from=value, values_fn=list) %>%
    unnest(everything())
  x
}


#' create cohort gender percentage columns
#'
#' @param percentcohort create cohort gender percentage columns
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percentcohort(x)

percentcohort <- function(x) {
  x$pc_academic <- (x$Academic/x$WTE_cohort)
  x$pc_pmss <- (x$PMSS/x$WTE_cohort)
  x
}


#' round cohort percentage values
#'
#' @param roundcohortvalues round cohort percentage values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundcohortvalues(x)

roundcohortvalues <- function(x) {
  x$academicvalueround <- base::round(x$pc_academic*100, digits=0)
  x$pmssvalueround <- base::round(x$pc_pmss*100, digits=0)
  x
}


#' create percentage labels for academic cohort
#'
#' @param academicpercentlabel create percentage labels for academic cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- academicpercentlabel(x)

academicpercentlabel <- function(x) {
  x$academicpclabel <- gsub(" ", "", paste((x$academicvalueround), "%"))
  x
}


#' create percentage labels for academic cohort
#'
#' @param pmsspercentlabel create percentage labels for pmss cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pmsspercentlabel(x)

pmsspercentlabel <- function(x) {
  x$pmsspclabel <- gsub(" ", "", paste((x$pmssvalueround), "%"))
  x
}


#' remove percentage labels for academic cohort <5%
#'
#' @param academicpclabel remove percentage labels for academic cohort <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- academicpclabel(x)

academicpclabel <- function(x) {
  x$academicpclabel[x$pc_academic<0.05]=NA
  x
}


#' remove percentage labels for pmss cohort <5%
#'
#' @param pmsspclabel remove percentage labels for pmss cohort <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pmsspclabel(x)

pmsspclabel <- function(x) {
  x$pmsspclabel[x$pc_pmss<0.05]=NA
  x
}


#' create column with cohort total percent of wte per group (gender)
#'
#' @param allpercentcohort create column with cohort total percent of wte per group (gender)
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- allpercentcohort(x)

allpercentcohort <- function(x) {
  x$all_pc <- ((x$Academic+x$PMSS)/x$all)
  x
}


#' collapse all
#'
#' @param collapseall collapse all
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapseall(x)

collapseall <- function(x) {
  x <- x %>%
    group_by(Institution, Gender) %>%
    summarise(across(c(Headcount, WTE), sum))
  x
}


#' all percent total
#'
#' @param allpercenttotal all percent total
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- allpercenttotal(x)

allpercenttotal <- function(x) {
  x$all_pc <- (x$WTE/x$all)
  x
}


#' rename institution type
#'
#' @param renametype rename institution type
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renametype(x)

renametype <- function(x) {
  x[x=="Institution type"] <- "Type"
  x
}


#' collapse by institution type
#'
#' @param collapse3 collapse by institution type
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapse3(x)

collapse3 <- function(x) {
  x <- x %>%
    group_by(Type, Institution, Grade, Gender) %>%
    summarise(across(c(Headcount, WTE), sum))
}


#' create column with grade total for each institution
#'
#' @param wtegrade create column with grade total for each institution
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- wtegrade(x)

wtegrade <- function(x) {
  x$WTE_grade <- x %>%
    group_by(Institution, Grade) %>%
    mutate(WTE_grade = sum(WTE))
  x
}


#' rename grade funding total
#'
#' @param renamegradetotal rename grade funding total
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamegradetotal(x)

renamegradetotal <- function(x) {
  x$WTE_grade <- x$WTE_grade$WTE_grade
  x
}


#' pivot wider gender grade
#'
#' @param pivotwidergendergrade pivot wider gender grade
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pivotwidergendergrade(x)

pivotwidergendergrade <- function(x) {
  x <- as.data.frame(x)
  x <- x %>% pivot_wider(id_cols=c(Institution, Grade, WTE_grade, all), names_from=Gender, values_from=WTE, values_fn=list) %>%
    unnest(everything())
  x
}


#' create grade column for "Other" cohort
#'
#' @param createothergrade create grade column for "Other" cohort
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- createothergrade(x)

createothergrade <- function(x) {
  x$Other <- (x$WTE_grade-(x$Female+x$Male))
  x
}


#' count percent of total institution wte
#'
#' @param pc_total count percent of total institution wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pc_total(x)

pc_total <- function(x) {
  x$pctotal <- (x$value/x$all)
  x
}


#' calculate percent of grade total wte
#'
#' @param pc_grade calculate percent of grade total wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pc_grade(x)

pc_grade <- function(x) {
  x$pcgrade <- (x$value/x$WTE_grade)
  x
}


#' round grade percent values
#'
#' @param roundgradevalues round grade percent values
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundgradevalues(x)

roundgradevalues <- function(x) {
  x$gradevalueround <- base::round(x$pctotal*100, digits=0)
  x
}


#' create grade percentage labels
#'
#' @param gradepercentlabel create grade percentage labels
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- gradepercentlabel(x)

gradepercentlabel <- function(x) {
  x$gradepclabel <- gsub(" ", "", paste((x$gradevalueround), "%"))
  x
}


#' remove grade percent labels for values <5%
#'
#' @param gradepclabel remove grade percent labels for values <5%
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- gradepclabel(x)

gradepclabel <- function(x) {
  x$gradepclabel[x$pctotal<0.05]=NA
  x
}


#' replace value "* Another option"
#'
#' @param replaceanother replace value "* Another option"
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- replaceanother(x)

replaceanother <- function(x) {
  x[x=="* Another option" | x=="*Another option"] <- "Other"
  x
}


#' subset academic staff with TUs only
#'
#' @param tusub subset academic staff with TUs only
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- tusub(x)

tusub <- function(x) {
  x <- subset(x, x[[1]] %in% c("Atlantic Technological University", "Munster Technological University", "South East Technological University", "Technological University Dublin", "Technological University of the Shannon"))
  x
}


#' collapse academic staff by grade and gender
#'
#' @param collapseacgrade collapse academic staff by grade and gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapseacgrade(x)

collapseacgrade <- function(x) {
  x <- x %>%
    group_by(Grade,gender) %>%
    summarise(across(c(value), sum))
  x
}



#' calculate percent of grade total wte for academic staff
#'
#' @param acpcgrade calculate percent of grade total wte for academic staff
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- acpcgrade(x)

acpcgrade <- function(x) {
    x <- group_by(x, Grade) %>% mutate(pctotal = value/sum(value))
    x
}


#' add column for "All TUs"
#'
#' @param alltus add column for "All TUs"
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- alltus(x)

alltus <- function(x) {
  x$type <- "All TUs"
  x
}


#' add column for "All HEIs"
#'
#' @param allhei add column for "All HEIs"
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- allhei(x)

allhei <- function(x) {
  x$type <- "All HEIs"
  x
}


#' add column for "TUS"
#'
#' @param tusonly add column for "TUS"
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- tusonly(x)

tusonly <- function(x) {
  x$type <- "TUS"
  x
}



#' subset data for TUS only
#'
#' @param tussub subset data for TUS only
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- tussub(x)

tussub <- function(x) {
  x <- subset(x, x[[1]] %in% ("Technological University of the Shannon"))
  x
}


#' subset hei data for gender
#'
#' @param heigendersub subset hei data for gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- heigendersub(x)

heigendersub <- function(x) {
  x <- subset(x[,c(1,4:6)])
  x
}


#' collapse hei by gender
#'
#' @param collapsehei collapse hei by gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapsehei(x)

collapsehei <- function(x) {
  x <- x %>%
    group_by(Gender) %>%
    summarise(across(c(Headcount, WTE), sum))
  x
}


#' calculate percent of total for hei headcount
#'
#' @param percenthchei calculate percent of total for hei headcount
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percenthchei(x)

percenthchei <- function(x) {
    x$pcHC <- (x$Headcount/sum(x$Headcount))
    x
  }


#' round percent values for headcount
#'
#' @param roundhcvalues round percent values for headcount
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundhcvalues(x)

roundhcvalues <- function(x) {
  x$hcround <- base::round(x$pcHC*100, digits=0)
  x
}


#' create percentage label for headcount
#'
#' @param percenthclabel create percentage label for headcount
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percenthclabel(x)

percenthclabel <- function(x) {
  x$pchclabel <- gsub(" ", "", paste((x$hcround), "%"))
  x
}


#' remove values for labels <5% for headcount
#'
#' @param pchclabel remove values for labels <5% for headcount
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pchclabel(x)

pchclabel <- function(x) {
  x$pchclabel[x$pcHC<0.05]=NA
  x
}


#' calculate percent of total for hei wte
#'
#' @param percentwtehei calculate percent of total for hei wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percentwtehei(x)

percentwtehei <- function(x) {
  x$pcWTE <- (x$WTE/sum(x$WTE))
  x
}


#' round percent values for wte
#'
#' @param roundwtevalues round percent values for wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- roundwtevalues(x)

roundwtevalues <- function(x) {
  x$wteround <- base::round(x$pcWTE*100, digits=0)
  x
}


#' create percentage label for wte
#'
#' @param percentwtelabel create percentage label for wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- percentwtelabel(x)

percentwtelabel <- function(x) {
  x$pcwtelabel <- gsub(" ", "", paste((x$wteround), "%"))
  x
}


#' remove values for labels <5% for wte
#'
#' @param pcwtelabel remove values for labels <5% for wte
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pcwtelabel(x)

pcwtelabel <- function(x) {
  x$pcwtelabel[x$pcWTE<0.05]=NA
  x
}


#' collapse all tus data for pmss pay grade
#'
#' @param collapsepmsspay collapse all tus data for pmss pay grade
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapsepmsspay(x)

collapsepmsspay <- function(x) {
  x <- x %>%
    group_by(`Pay grade`, Gender) %>%
    summarise(across(c(WTE), sum))
  x
}


#' calculate percent of grade total wte for pmss staff
#'
#' @param pmsspcgrade calculate percent of grade total wte for pmss staff
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pmsspcgrade(x)

pmsspcgrade <- function(x) {
  x <- group_by(x, `Pay grade`) %>% mutate(pctotal = WTE/sum(WTE))
  x
}


#' rename "Contract duration" column
#'
#' @param renameconcol rename "Contract duration" column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renameconcol(x)

renameconcol <- function(x) {
  x$contract <- x$`Contract duration`
  x$`Contract duration` <- NULL
  x
}


#' rename "Contract-type" column
#'
#' @param renamestatuscol rename "Contract-type" column
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- renamestatuscol(x)

renamestatuscol <- function(x) {
  x$status <- x$`Contract-type`
  x$`Contract-type` <- NULL
  x
}


#' subset hei data for contract
#'
#' @param consub subset hei data for contract
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- consub(x)

consub <- function(x) {
  x <- subset(x[,c(1,4,6:9)])
  x
}



#' subset hei data for academic contract only
#'
#' @param acconsub subset hei data for academic contract only
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- acconsub(x)

acconsub <- function(x) {
  x <- subset(x, x[[4]] %in% ("Academic"))
  x
}


#' subset hei data for pmss contract only
#'
#' @param pmssconsub subset hei data for pmss contract only
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- pmssconsub(x)

pmssconsub <- function(x) {
  x <- subset(x, x[[4]] %in% ("PMSS"))
  x
}


#' collapse tus only contract data by gender
#'
#' @param collapsetuscon collapse tus only contract data by gender
#' @param x dataframe to which the function is applied
#'
#' @returns changes are saved to the dataframe
#' @export
#'
#' @examples x <- collapsetuscon(x)

collapsetuscon <- function(x) {
  x <- x %>%
    group_by(contract, status) %>%
    summarise(across(c(WTE), sum))
  x
}
