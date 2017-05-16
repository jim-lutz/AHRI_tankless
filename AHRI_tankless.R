fn_script = "AHRI_tankless.R"
# explore RE vs new tankless algorithms 
# solve for input during recovery effiency test (Pgas.RE) and cyclic losses (Lcyc)
# "Tue May 16 14:31:04 2017"  refactor 


# clean up leftovers before starting
# clear all the objects except fn_script
l_obj=ls(all=TRUE)
l_obj = c(l_obj, "l_obj") # be sure to include l_obj
rm(list = l_obj[l_obj != "fn_script"])
# clear the plots
if(!is.null(dev.list())){
  dev.off(dev.list()["RStudioGD"])
}
# clear history
cat("", file = ".nohistory")
loadhistory(".nohistory")
# clear the console
cat("\014")


# install and load data.table
if(!require(data.table)){install.packages("data.table")}
library(data.table)

# install and load ggplot
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# read data from AHRI temporary online directory
fn_tankless_data = "AHRI_directory.csv"
DT_whs <- data.table(read.csv(file=fn_tankless_data,  comment.char = "#"))

# see what we've got
nrow(DT_whs)
  # 275
names(DT_whs)
    # [1] "AHRI.Reference.Number"   "Model.Status"            "Trade.Brand.Name"        "Manufacturer"           
    # [5] "Model.Number"            "Energy.Source"           "Heater.Type"             "Usage.Bin"              
    # [9] "First.Hour.Rating"       "MaxGPM"                  "Uniform.Energy..Factor"  "Nominal..Capacity..gal."
    # [13] "Rated.Storage.Volume"    "Input..MBtuh.kW."        "Recovery.Efficiency...." "Sold.In"                

# clean up some column names
setnames(DT_whs, old = c("Uniform.Energy..Factor","Input..MBtuh.kW.","Recovery.Efficiency...."), 
                 new = c("UEF","Rated.Input","RE"))

# convert Recovery.Efficiency to a fraction
DT_whs[,RE:=RE/100]

# convert Rated.Input to BTUH
DT_whs[,Rated.Input:=Rated.Input*1000]

unique(DT_whs[,Heater.Type])
# [1] Instantaneous Storage      

unique(DT_whs[,Energy.Source])
# [1] Both Natural Gas and Propane Gas   Natural Gas                        Propane Gas                       
# [4] Electric Resistance                Oil                                Electric Resistance - Grid-enabled
# [7] Heat Pump with Tank               

# keep only gas-fired tankless 
DT_tankless <- DT_whs[grep("Gas",Energy.Source),][Heater.Type=="Instantaneous",]

nrow(DT_tankless)
  # 97
names(DT_tankless)

# see what we've got
qplot(data=DT_tankless, x=Rated.Input)
qplot(data=DT_tankless, x=Rated.Input, y=MaxGPM)

summary(DT_tankless[,list(Usage.Bin,MaxGPM,UEF,Rated.Input,RE)])
    # Usage.Bin                  MaxGPM           UEF          Rated.Input           RE        
    # High Usage      :73   Min.   :2.500   Min.   :0.8000   Min.   :103200   Min.   :0.8200  
    # Low Usage       : 4   1st Qu.:4.300   1st Qu.:0.8100   1st Qu.:150000   1st Qu.:0.8400  
    # Medium Usage    :20   Median :4.800   Median :0.8200   Median :190000   Median :0.8500  
    # Very Small Usage: 0   Mean   :4.634   Mean   :0.8543   Mean   :177826   Mean   :0.8831  
    #                       3rd Qu.:5.100   3rd Qu.:0.9300   3rd Qu.:199000   3rd Qu.:0.9500  
    #                       Max.   :5.900   Max.   :0.9700   Max.   :199900   Max.   :1.0000  
# Looks OK. 

# read UEF test procedure draw patterns
fn_UEF_draw_pattern = "UEF draw patterns.csv"
DT_UEF_DP <- data.table(read.csv(file=fn_UEF_draw_pattern,  comment.char = "#"))

DT_UEF_DP # seems OK

# define specific heat of water at test procedure conditions
Cp = 8.1374703	# BTU/gallon °F

# define Tdelta at test procedure conditions
Tdelta =	125 - 58	# °F

# add duration and Qout to the UEF draw patterns data.table
DT_UEF_DP[,Duration:=Volume/Flow]  # in minutes
DT_UEF_DP[,Qout:=Volume*Tdelta*Cp] # BTU

# make a data.table that's just the first draw data from UEF draw pattern for the RE calcs
DT_RE <- DT_UEF_DP[Draw==1,list(Usage.Bin, 
                                Flow.RE     = Flow, 
                                Qout.RE     = Qout, 
                                Duration.RE = Duration,
                                N.RE        =1)
                   ]
setkey(DT_RE,Usage.Bin)

# make a data.table thats draw data for the entire UEF test procedure for UEF calcs
DT_UEF <- DT_UEF_DP[,list(Duration.UEF  = sum(Duration),
                          Volume.UEF    = sum(Volume),
                          Qout.UEF      = sum(Qout),
                          N.UEF         = length(Qout)), 
                    by = Usage.Bin]
DT_UEF[,Flow.UEF:=Volume.UEF / Duration.UEF]
setkey(DT_UEF,Usage.Bin)

# add RE and UEF data to the tankless WH data.table
setkey(DT_tankless,Usage.Bin)
DT_tankless <- merge(DT_tankless, DT_RE)
DT_tankless <- merge(DT_tankless, DT_UEF)

# add Qin
DT_tankless[,Qin.RE:=Qout.RE/RE]
DT_tankless[,Qin.UEF:=Qout.UEF/UEF]

#check that it worked
summary(DT_tankless)
# OK

str(DT_tankless)

# try matrix solution for
# Qin.RE = Pgas.RE * Duration.RE + N.RE * Lcyc
# Qin.UEF = Pgas.RE * (Flow.UEF/Flow.RE) * Duration.UEF + N.UEF * Lcyc

# make simple variables first for the nth WH
n=1
DT_tankless[n,]

Duration.RE.n    = DT_tankless[n,Duration.RE]  
Flow.RE.n        = DT_tankless[n,Flow.RE]
N.RE.n           = DT_tankless[n,N.RE]  
Qin.RE.n         = DT_tankless[n,Qin.RE]

Flow.UEF.n       = DT_tankless[n,Flow.UEF]
Duration.UEF.n   = DT_tankless[n,Duration.UEF]
N.UEF.n          = DT_tankless[n,N.UEF]
Qin.UEF.n        = DT_tankless[n,Qin.UEF]

# find solutions to:
# A[1,1] * Pgas.RE + A[1,2] * Lcyc_gas = Qin.RE
# A[2,1] * Pgas.RE + A[2,2] * Lcyc_gas = Qin.UEF

# Matrix A, the coeficients of Fadj & Lcyc_gas
A <- matrix(nrow=2,ncol=2)
A[1,1] <- Duration.RE.n
A[1,2] <- N.RE.n
A[2,1] <- (Flow.UEF.n/Flow.RE.n)* Duration.UEF.n
A[2,2] <- N.UEF.n
A

# Matrix B, the constants
b <- matrix(nrow=2)
b[1] <- Qin.RE.n
b[2] <- Qin.UEF.n
b

X <- solve(A,b)
X
      #            [,1]
      # [1,] 1693.42550
      # [2,]   93.21608

Pgas.RE.n  <- X[1,]  # BTU/min
Lcyc_gas.n <- X[2,]  # BTU/cycle

# check answer
A[1,1] * Pgas.RE.n + A[1,2] * Lcyc_gas.n
Qin.RE.n
identical(A[1,1] * Pgas.RE.n + A[1,2] * Lcyc_gas.n, Qin.RE.n)
#  FALSE
A[1,1] * Pgas.RE.n + A[1,2] * Lcyc_gas.n - Qin.RE.n
#   1.818989e-12

A[2,1] * Pgas.RE.n + A[2,2] * Lcyc_gas.n
Qin.UEF.n
identical(A[2,1] * Pgas.RE.n + A[2,2] * Lcyc_gas.n, Qin.UEF.n)
#  TRUE





eqn_label <- sprintf("EF = RE * %5.4f + %6.5f, r2=%5.4f n=%d",
                            mod$coefficients[[2]],
                            mod$coefficients[[1]],
                            summary(mod)$adj.r.squared,
                            nrow(DT_tankless))
# plot model on chart
ggplot(data=DT_tankless,  aes(Recovery.Efficiency,  Energy.Factor)) +
  geom_point() +
  geom_smooth(method ="lm") + geom_text(x=.90, y=.80,label=eqn_label)

ggsave("tanklessEF.png")

