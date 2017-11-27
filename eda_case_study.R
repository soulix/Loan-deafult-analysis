rm(list=ls())
install.packages("corrplot")
library(lubridate)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gsubfn)
library(magrittr)
library(gdata)


#---- Importing Loan Dataset -----

loan_df<- read.csv("loan.csv", stringsAsFactors = FALSE,na.strings = c(""))




#---- data Cleaning -----

# Removing the columns thats hass only NA valus.And those have only "0 & NA" or only single value like "INDIVIDUAL".
#removing 'pymnt_plan' and 'initial_list_status' because it has only a single value. also URL and Description.

loan<- subset(loan_df, select = -c( annual_inc_joint : bc_util , mo_sin_old_il_acct : percent_bc_gt_75 , tot_hi_cred_lim : total_il_high_credit_limit ))

loan<- subset(loan,select= -c(collections_12_mths_ex_med:delinq_amnt,tax_liens ))

loan<- subset(loan,select = -c(pymnt_plan:desc,initial_list_status))

              
# Cheking for duplicate rows

length(unique(loan$id))== nrow(loan) #if true that is no duplicate valus.primary key is 'id'(loan id)

#renaming Coulmn name "term" with "term_month".replacing mothns by " ".

names(loan)[names(loan) == 'term'] <- 'term_month'

loan$term_month<- gsub("months", "", loan$term_month)

# claening employee length

names(loan)[names(loan)== 'emp_length']<- 'emp_length_years'

loan$emp_length_years<- gsub("years","",loan$emp_length_years)
loan$emp_length_years<- gsub("year","",loan$emp_length_years)
loan$emp_length_years<- gsub("n/a","NA",loan$emp_length_years)




#Replacing % with blank in interest rate(int_rate) column

loan$int_rate<- gsub("%", " ", loan$int_rate)

#making all the values of the emp_title coulmn in lower case

loan$emp_title<- tolower(loan$emp_title)
loan$title<- tolower(loan$title)

#replacing 'xx' with blanks in zip code

loan$zip_code<- gsub("xx","",loan$zip_code)

#renaming 'revol_util' to 'revol_util_percent' and then replacing "%" with " ".

names(loan)[names(loan)== 'revol_util']<- 'revol_util_percent'

loan$revol_util_percent<- gsub("%","",loan$revol_util_percent)


#converting all the colums containing dates in standard R format i.e. yyyy-mm-dd .
#Default format: "%Y-%m-%d"


loan$issue_d<-paste("01-", loan$issue_d,sep = "")
loan$issue_d<-as.Date(loan$issue_d, format = "%d-%b-%y")
typeof(loan$issue_d)

loan$earliest_cr_line<-paste("01", loan$earliest_cr_line,sep = "-")
loan$earliest_cr_line<-as.Date(loan$earliest_cr_line, format = "%d-%b-%y")

loan$last_pymnt_d<-paste("01", loan$last_pymnt_d,sep = "-")
loan$last_pymnt_d<-as.Date(loan$last_pymnt_d, format = "%d-%b-%y")

loan$last_credit_pull_d<-paste("01-", loan$last_credit_pull_d,sep = "")
loan$last_credit_pull_d<-as.Date(loan$last_credit_pull_d, format = "%d-%b-%y")

#lets convert some variables of loan dataset as numeric.because it will be used later for analysis.

loan$int_rate<- as.numeric(loan$int_rate)
loan$term_month<-as.numeric(loan$term_month)
loan$revol_util_percent<-as.numeric(loan$revol_util_percent)

typeof(loan_default$int_rate)






#---- Data Analysis-----

#Taking charged off loans in a separate data set

loan_default<- loan %>% filter(loan$loan_status== "Charged Off") 


#frequency distribution of loan status.

loan_stat_count<- loan %>% group_by(loan_status) %>% summarise(count= n())

ggplot(loan,aes(x=loan$loan_status)) + geom_bar()

#analysis on interest rate



int_rate_count<- loan_default %>% group_by(int_rate) %>% summarise(count= n()) %>% arrange(desc(count))

ggplot(int_rate_count,aes(x=int_rate, y= count)) + geom_col()

b <- c(-Inf,5,8,12,16,20,25, Inf)
names <- c( "0 to 5", "5 to 8", "8 to 12", "12 to 16","16 to 20","20 to 25","25 to 30")

int_rate_count<-int_rate_count%>% mutate(int_rate_bin_charged_off= cut(int_rate, breaks = b, labels = names))

ggplot(int_rate_count,aes(int_rate_bin_charged_off)) + geom_bar()

int_rate_bin_percent<- int_rate_count %>% group_by(int_rate_bin_charged_off) %>% summarise(bin_count=n()) %>% mutate(charged_off_int_rate_bin_percent= 100*(bin_count/sum(bin_count)))

bin_b <- c(-Inf,10,20,30,Inf)
bin_names <- c( "low", "moderate", "high", "very high")

int_rate_bin_percent<- int_rate_bin_percent %>% mutate(risk= cut(charged_off_int_rate_bin_percent, breaks = bin_b, labels = bin_names))

ggplot(int_rate_bin_percent,aes(x=int_rate_bin_charged_off ,y= charged_off_int_rate_bin_percent,fill= risk)) +geom_col()+labs(x="risk profile on the basis of interest rate")




#analysis on delinq_2yrs.

delinq_2yrs<- loan %>% select(delinq_2yrs)  %>% group_by(delinq_2yrs) %>% summarise(count=n())%>% mutate(percent=100*(count/sum(count)))
delinq_2yrs <- within(delinq_2yrs, cum_sum <- cumsum(count))
delinq_2yrs<- within(delinq_2yrs,cum_perc<-cumsum(percent))

typeof(loan$delinq_2yrs)

delinq_2yrs<- delinq_2yrs %>% mutate(Bad_Flag_Tagging = ifelse(delinq_2yrs %in% 0, "good",
                                                         ifelse(delinq_2yrs %in% 1, "indent","bad")))
                                                                
ggplot(delinq_2yrs,aes(x=delinq_2yrs ,y= percent,fill= Bad_Flag_Tagging)) +geom_col()+labs(x="Bad Flag Tagging", y="cummulative percentage of two year delinq")


#corelation matrix and plotting to get an idea about the association/dependencies of varibles between each of them.


cor_mat_loan<- loan %>% select(loan_amnt,funded_amnt,funded_amnt_inv,term_month,int_rate,installment,annual_inc,dti,delinq_2yrs,inq_last_6mths,open_acc,revol_bal,revol_util_percent,out_prncp,out_prncp_inv,total_acc,total_rec_prncp,total_rec_int,total_pymnt,total_pymnt_inv,last_pymnt_amnt)
cor_mat<- cor_mat_loan %>% select(loan_amnt,funded_amnt,funded_amnt_inv,term_month,dti,delinq_2yrs,open_acc,revol_bal,revol_util_percent,total_acc)

cor_mat_chage_off<-loan_default%>%select(loan_amnt,funded_amnt,funded_amnt_inv,term_month,dti,delinq_2yrs,open_acc,revol_bal,revol_util_percent,total_acc)

#---- rquery.cormat() function----

#+++++++++++++++++++++++++
# Computing of correlation matrix
#+++++++++++++++++++++++++
# Required package : corrplot
# x : matrix
# type: possible values are "lower" (default), "upper", "full" or "flatten";
#display lower or upper triangular of the matrix, full  or flatten matrix.
# graph : if TRUE, a correlogram or heatmap is plotted
# graphType : possible values are "correlogram" or "heatmap"
# col: colors to use for the correlogram
# ... : Further arguments to be passed to cor or cor.test function
# Result is a list including the following components :
# r : correlation matrix, p :  p-values
# sym : Symbolic number coding of the correlation matrix
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}



#----

#entire
cor_mat<- as.matrix(cor_mat)
rquery.cormat(cor_mat)
rquery.cormat(cor_mat, type="flatten", graph=FALSE)

round(rquery.cormat(cor_mat_chage_off),2)
rquery.cormat(cor_mat_chage_off, type="flatten", graph=FALSE)


