fn_script = "AHRI_tankless.R"
# explore RE vs new tankless algorithms 
# "Mon May 15 11:52:26 2017"
# "Mon May 15 16:40:25 2017"  fix RE.Max
# "Mon May 15 18:54:52 2017"  matrix solution to Fadj & Lcyc_gas
# "Tue May 16 06:05:15 2017"  1st pass at matrix solution


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
fn_tankless_data = "/home/jiml/HotWaterResearch/projects/CECHWT24/2016 CBECC UEF/AHRI Directory/AHRI_directory.csv"
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
fn_UEF_draw_pattern = "/home/jiml/HotWaterResearch/projects/CECHWT24/2016 CBECC UEF/UEF draw patterns.csv"
DT_UEF_DP <- data.table(read.csv(file=fn_UEF_draw_pattern,  comment.char = "#"))

DT_UEF_DP # seems OK

# define specific heat of water at test procedure conditions
Cp = 8.1374703	# BTU/gallon °F

# define Tdelta at test procedure conditions
Tdelta =	125 - 58	# °F

# add duration and Qout to the UEF draw patterns data.table
DT_UEF_DP[,Duration:=Volume/Flow]  # in minutes
DT_UEF_DP[,Qout:=Volume*Tdelta*Cp] # BTU

# calculate RE.Max at MaxGPM for all the water heaters as Qout / Qin
DT_tankless[,RE.Max := (MaxGPM * 60 * Cp * Tdelta)/Rated.Input ]

# compare RE.Max to RE
qplot(data=DT_tankless, x=RE, y=RE.Max)

# look at ranges of RE.Max
summary(DT_tankless[,list(RE,RE.Max)])
    #        RE             RE.Max      
    # Min.   :0.8200   Min.   :0.7037  
    # 1st Qu.:0.8400   1st Qu.:0.8092  
    # Median :0.8500   Median :0.8264  
    # Mean   :0.8831   Mean   :0.8514  
    # 3rd Qu.:0.9500   3rd Qu.:0.8877  
    # Max.   :1.0000   Max.   :0.9814  
# Seems OK

# make a data.table that's just the first draw data from UEF draw pattern for the RE calcs
DT_RE <- DT_UEF_DP[Draw==1,list(Usage.Bin, 
                                Flow.RE=Flow, 
                                Volume.RE=Volume,
                                Qout.RE=Qout, 
                                Duration.RE=Duration,
                                N.RE=1)
                   ]
setkey(DT_RE,Usage.Bin)

# make a data.table thats draw data for the entire UEF test procedure for UEF calcs
DT_UEF <- DT_UEF_DP[,list(Volume.UEF    = sum(Volume),
                          Duration.UEF  = sum(Duration),
                          Qout.UEF      = sum(Qout),
                          N.UEF = length(Qout)
                          ), by = Usage.Bin]
DT_UEF[,Flow.UEF:=Volume.UEF / Duration.UEF]
setkey(DT_UEF,Usage.Bin)

# add RE and UEF data to the tankless WH data.table
setkey(DT_tankless,Usage.Bin)
DT_tankless <- merge(DT_tankless, DT_RE)
DT_tankless <- merge(DT_tankless, DT_UEF)

#check that it worked
summary(DT_tankless)
# OK

# calculate the turndown ratio at RE test procedure flowrates
DT_tankless[,TD.RE:=Flow.RE/MaxGPM]
# and average turndown for UEF test procedure
DT_tankless[,TD.UEF:=Flow.UEF/MaxGPM]

# try matrix solution 
# make simple variables first for the nth WH
n=1
DT_tankless[n,]
Rated.Input.n    = DT_tankless[n,Rated.Input] 
TD.UEF.n         = DT_tankless[n,TD.UEF]
Duration.UEF.n   = DT_tankless[n,Duration.UEF]
N.UEF.n          = DT_tankless[n,N.UEF]
Qout.UEF.n       = DT_tankless[n,Qout.UEF]

TD.RE.n          = DT_tankless[n,TD.RE]  
Duration.RE.n    = DT_tankless[n,Duration.RE]  
N.RE.n           = DT_tankless[n,N.RE]  
Qout.RE.n        = DT_tankless[n,Qout.RE]

# find solutions to:
# A[1,1] * Fadj + A[1,2] * Lcyc_gas = Qout.UEF
# A[2,1] * Fadj + A[2,2] * Lcyc_gas = Qout.RE

# Matrix A, the coeficients of Fadj & Lcyc_gas
A <- matrix(nrow=2,ncol=2)
A[1,1] <- Rated.Input.n * TD.UEF.n * Duration.UEF.n
A[1,2] <- N.UEF.n
A[2,1] <- Rated.Input.n * TD.RE.n * Duration.RE.n
A[2,2] <- N.RE.n
A

# Matrix B, the constants
b <- matrix(nrow=2)
b[1] <- Qout.UEF.n
b[2] <- Qout.RE.n
b

X <- solve(A,b)
X
    #              [,1]
    # [1,] 1.616453e-02
    # [2,] 9.635467e-13
# X[2,] too small 

Fadj.n <- X[1,]
Lcyc_gas.n <- X[2,]

# check answer
A[1,1] * Fadj.n + A[1,2] * Lcyc_gas.n
Qout.UEF.n
identical(A[1,1] * Fadj.n + A[1,2] * Lcyc_gas.n, Qout.UEF.n)
#  TRUE

A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n
Qout.RE.n
identical(A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n, Qout.RE.n)
# FALSE
(A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n) - Qout.RE.n
# -1.818989e-12, 
# not a stable solution? error is larger than Fadj?

##

# Try again with a more standard WH?
names(DT_tankless)
DT_tankless[Model.Number=="ATI-110U 200",]

Rated.Input.n    = DT_tankless[Model.Number=="ATI-110U 200",Rated.Input] 
TD.UEF.n         = DT_tankless[Model.Number=="ATI-110U 200",TD.UEF]
Duration.UEF.n   = DT_tankless[Model.Number=="ATI-110U 200",Duration.UEF]
N.UEF.n          = DT_tankless[Model.Number=="ATI-110U 200",N.UEF]
Qout.UEF.n       = DT_tankless[Model.Number=="ATI-110U 200",Qout.UEF]

TD.RE.n          = DT_tankless[Model.Number=="ATI-110U 200",TD.RE]  
Duration.RE.n    = DT_tankless[Model.Number=="ATI-110U 200",Duration.RE]  
N.RE.n           = DT_tankless[Model.Number=="ATI-110U 200",N.RE]  
Qout.RE.n        = DT_tankless[Model.Number=="ATI-110U 200",Qout.RE]

# find solutions to:
# A[1,1] * Fadj + A[1,2] * Lcyc_gas = Qout.UEF
# A[2,1] * Fadj + A[2,2] * Lcyc_gas = Qout.RE

# Matrix A, the coeficients of Fadj & Lcyc_gas
A <- matrix(nrow=2,ncol=2)
A[1,1] <- Rated.Input.n * TD.UEF.n * Duration.UEF.n
A[1,2] <- N.UEF.n
A[2,1] <- Rated.Input.n * TD.RE.n * Duration.RE.n
A[2,2] <- N.RE.n
A

# Matrix B, the constants
b <- matrix(nrow=2)
b[1] <- Qout.UEF.n
b[2] <- Qout.RE.n
b

X <- solve(A,b)
X
#              [,1]
# [1,] 1.616453e-02
# [2,] 9.635467e-13
# X[2,] too small 

Fadj.n <- X[1,]
Lcyc_gas.n <- X[2,]

# check answer
A[1,1] * Fadj.n + A[1,2] * Lcyc_gas.n
Qout.UEF.n
identical(A[1,1] * Fadj.n + A[1,2] * Lcyc_gas.n, Qout.UEF.n)
#  TRUE

A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n
Qout.RE.n
identical(A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n, Qout.RE.n)
# FALSE
(A[2,1] * Fadj.n + A[2,2] * Lcyc_gas.n) - Qout.RE.n
# -1.818989e-12, 
# not a stable solution? error is larger than Fadj?



####







# fit a model
mod <-lm( Energy.Factor ~ Recovery.Efficiency ,data=DT_tankless)
summary(mod)$adj.r.squared
mod$coefficients
mod$coefficients[[1]] # Intercept
mod$coefficients[[2]] # RE coefficient
mod$adj.r.squared
nrow(DT_tankless)

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

