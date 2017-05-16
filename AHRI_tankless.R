fn_script = "AHRI_tankless.R"
# explore RE vs new tankless algorithms 
# "Mon May 15 11:52:26 2017"
# "Mon May 15 16:40:25 2017"  fix RE.Max
# "Mon May 15 18:54:52 2017"  matrix solution to Fadj & Lcyc_gas
# "Tue May 16 06:05:15 2017"  1st pass at matrix solution, failed
# "Tue May 16 11:25:51 2017"  go back to for Lcyc.RE and Lcyc.UEF
#                             Negative RE cyclic losses and Lcyc.UEF < Lcyc.RE?
#                             see if pieces of Lcyc calcs make sense


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
# Seems to work. Although RE.Max is less than RE. So efficiency is not constant with flow rate

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

str(DT_tankless)

# calculate the cyclic losses from the RE test procedure 
DT_tankless[,Lcyc.RE:=(Volume.RE * Tdelta * Cp)/RE - (Rated.Input/60)*Volume.RE/MaxGPM]
summary(DT_tankless[,Lcyc.RE]) # Negative RE cyclic losses??!!

# calculate the cyclic losses from the UEF test procedure 
DT_tankless[,Lcyc.UEF:=(Volume.UEF * Tdelta * Cp)/UEF - (Rated.Input/60)*Volume.UEF/MaxGPM]
summary(DT_tankless[,Lcyc.UEF]) # Negative RE cyclic losses??!!

qplot(data = DT_tankless, x=Lcyc.RE, y=Lcyc.UEF)
# Sign wrong somewhere? Lcyc.UEF < Lcyc.RE?

# calculate Qin
DT_tankless[,Qin.RE:=Qout.RE/RE]
DT_tankless[,Qin.UEF:=Qout.UEF/UEF]
summary(DT_tankless[,list(Qin.RE,Qout.RE,Qin.UEF,Qout.UEF)])
# Qin > Qout, OK

# calculate Lcyc using both RE & UEF
DT_tankless[,Lcyc:=(Qout.RE/RE + Qout.UEF/UEF) - (Rated.Input/(60*MaxGPM))*(Volume.RE+Volume.UEF)]
summary(DT_tankless[,Lcyc])

ggsave("tanklessEF.png")


