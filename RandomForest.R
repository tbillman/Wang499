#Wang MAT 499 Project
#Random Forest Regression
#############Package Installs#############
install.packages("randomForest")
install.packages("MASS")
install.packages("parallel")
library("randomForest")
library("MASS")
library("parallel")
###############Data Entry#################
#reading in Freddie Mac Origination Data
fred.o <-read.table(file ="C:/Users/Thomas/Desktop/Data/FreddieQ12012/FredOrig2012Q1.txt",header = FALSE, sep = "|")
head(fred.o)
names = c("CreditScore",
          "FirstPmt",
          "FirstTimeHomebuyer",
          "Maturity Date",
          "MSA Code",
          "MI Percentage",
          "Number of Units",
          "Occupancy Status",
          "CLTV",
          "DTI",
          "UPB",
          "LTV",
          "Interest Rate",
          "Channel",
          "PPM",
          "Product",
          "State",
          "Property Type",
          "Postal Code",
          "Loan Sequence Number",
          "Loan Purpose",
          "Original Term",
          "Borrower Num",
          "Seller Name",
          "Servicer Name",
          "Super Conforming")
colnames(fred.o) <- names
head(fred.o)

fred.p <- read.table(file ="C:/Users/Thomas/Desktop/Data/FreddieQ12012/Perf.txt",header = FALSE, sep = "|")
names = c("Sequence Number",
          "Period",
          "Current UPB",
          "Delinquincy Status",
          "Loan Age",
          "Months to Maturity",
          "Repurchased",
          "Modification",
          "Zero Balance",
          "Zero Balance Date",
          "Current Interest Rate",
          "Current Deferred UPB",
          "Last Paid Installment",
          "MI Recoveries",
          "Net Sales Proceeds",
          "Non MI Recoveries",
          "Expenses",
          "Legal Costs",
          "Maintainence and Preservation Costs",
          "Tax and Insurance",
          "Misc",
          "Actual Loss",
          "Modification Cost")
colnames(fred.p) = names
#####Linking Performance and Origination of one loan#####

#use the set.grab(index)function

#testing if the mortage is current
set[dim(set)[1],4] == 0

i = fred.o$`Interest Rate`[1]
n = fred.o$`Original Term`[1]

#####Actual Loss First Look#####
fred.p[ which (fred.p$`Actual Loss` != "NA"),]
length(which(fred.p$`Actual Loss` != "NA"))

#look at the first one
dat  = fred.p[ which (fred.p$`Actual Loss` != "NA"),][1,]
num = dat[1]
OUPB = fred.o[which(as.character(fred.o$`Loan Sequence Number`) == 
                      levels(fred.p$`Sequence Number`)[as.numeric(num)]),][11]
#all of them?
dat  = fred.p[ which (fred.p$`Actual Loss` != "NA"),]
num = dat[,1]
loss.OUPB = NULL
for (x in 1:length(num)){
OUPB = fred.o[which(as.character(fred.o$`Loan Sequence Number`) == 
                      levels(fred.p$`Sequence Number`)[as.numeric(num[x])]),][11]
AL = dat[x,22]
vect = c(OUPB,AL)
loss.OUPB = rbind(loss.OUPB, vect)
}
colnames(loss.OUPB) = C("Original Loan Amount",
                        "Actual Loss from Forclosure")

#####Calculating payments#####
# We need the following:
# n = number of periods
# L = UPB
# r = monthly interest rate

#start with the first one

###calculating number of periods (n)

end = date.read(fred.o[1,4])
beg = date.read(fred.o[1,2])
n = nmonths(end,beg)

### L = UPB
L = fred.o[1,11]

### r = monthly interest rate (we assume reported in APR)
r = fred.o[1,13]/1200 
vrn = (1+r)^(-n)

### calculating the payment
pmt = (r)*(L)/(1-vrn)
pmt = pmt.calc(L = L, r = r,n = n)
#I get the same thing with my financial calculator (nice)


#####NPV compared to bond (Current Case)#####
#using 30 year treasury rate (assume effective) as risk-free rate proxy
beg
i = (1.0293)^(1/12) - 1
vin = (1 + i)^(-n)
npv = pmt * (1-vin) /  (i) - L
npv

#this represents the amount the bank is charging to hold the risk of default



#####Looking at Defaulted Loans######

#getting a few samples w/ ZBC of 03 or 09 (column)
#list of loans that were foreclosed on with or without loss
bad = which(as.character(fred.p$`Zero Balance`) == as.character(03) | 
        as.character(fred.p$`Zero Balance`) == as.character(09))
#look at the first one
loan.def = fred.p[bad[1],]
#find UPB in the previous entry
nplan = loan.def$`Loan Age` + loan.def$`Months to Maturity`
nreal = as.numeric(loan.def$`Loan Age`)
t = nreal + nmonths(end = date.read(loan.def$`Zero Balance Date`), start = date.read(loan.def$`Last Paid Installment`)) - 1
i = (1.0293)^(1/12) - 1
r =loan.def$`Current Interest Rate` / 1200
vinreal = (1 + i)^(-nreal)
vit = (1 + i)^(-t)
OUPB = fred.o[which(as.character(fred.o$`Loan Sequence Number`) == 
                   levels(fred.p$`Sequence Number`)[as.numeric(loan.def$`Sequence Number`)]),][11]
CUPB = fred.p[bad[1]-1,3]
pmt = pmt.calc(L = OUPB, r = r, n = nplan)
AL = loan.def$`Actual Loss`
NPV = pmt * (1-vinreal) / i - OUPB + vit * (CUPB + AL)
NPV
#####Looking at Prepaid Loans#####
#first we need a base set of all payments for the loan
set1 = set.grab(1)
NPV1 = prepaid.npv(set = set1, i = i)

set2 = set.grab(2)
NPV2 = prepaid.npv(set= set2, i = i)

#####Loan Classification#####
set1 = set.grab(1)
set1.type = classify(set1)
set1.type

set2 = set.grab(2)
set2.type = classify(set2)
set2.type

set143 = set.grab(143)
set143.type = classify(set143)
set143.type

set80592 = set.grab(80592)
set80592.type = classify(set80592)
set80592.type

#nice, it works
#it's pretty slow, but I don't know how to speed it up

#####Lists####
#small scale lapply
org<- fred.o[1:100,]

#look at how many performance rows we need
set100<-set.grab(100)
set100[dim(set100)[1],]
#this number is 4763
per = fred.p[1:4763,]

org.list = split(org, seq(nrow(org)))
org = as.list(org.list)
per.list = split(per, seq(nrow(per)))
per = as.list(per.list)
