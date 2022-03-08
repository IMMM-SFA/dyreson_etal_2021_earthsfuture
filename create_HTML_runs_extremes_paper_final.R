#This code prepares the data further for production of results in HTML
# Load required R libraries 
library(rmarkdown)
library(data.table)
library(RColorBrewer)
library(plyr)
library(stringr)
library(lubridate)

#### Functions #################################################################
makecats=function(item){
  #Add categorical labels for type of stressor or water condition
  item[,Label:=as.character(item$Scenario)]
  item[str_detect(item$Label,"Combo")==TRUE, Label := "Combination"] 
  item[str_detect(item$Label,"Derate")==TRUE, Label := "Derating"] 
  item[str_detect(item$Label,"Load")==TRUE, Label := "Load"] 
  item[str_detect(item$Label,"Water")==TRUE, Label := "Water"] 
  item[str_detect(item$Label,"none")==TRUE, Label := "Base"] 
  return(item)
}

makewaters=function(item){
  #Add categorical labels for type of stressor or water condition
  item[Label=="Combination"|Label=="Water",Water:=as.character(item[Label=="Combination"|Label=="Water"]$Scenario)]
  item[str_detect(item$Water,"B")==FALSE, Water := "1977 Drought"] 
  item[str_detect(item$Water,"B")==TRUE, Water := "2001 Drought"] 
  item[is.na(item$Water)==TRUE,Water:="Non-drought"]
  
  return(item)
}

##### FUNCTION TO RBIND FROM ENVIRONMENTS ############
rbindruns = function(item,env_list) {
  
  modellist<-list(data.table(0))
  for (i in 1:length(env_list)) {
    modellist[[i]]=get(item,envir=env_list[[i]])                    #Get the specified item from each i environment
  }
  setattr(modellist,'names',list_of_names)
  bound=rbindlist(modellist,use.names=TRUE,idcol="Infrastructure")            #Combine
  return(bound)
  
}

###### FUNCTION TO MELT HYDRO GENERATION ITEMS #####################################################
meltitem = function(item,env_list,new_item,id_list){
  for (i in 1:length(env_list)) {
    temp_item=get(item,envir=env_list[[i]])    
    temp_item<-melt(temp_item, id.vars=id_list, variable.name='Scenario')
    assign(new_item,temp_item,envir=env_list[[i]])
    rm(temp_item)
  }
}

 ################################################################################
#Load data
`Low VRE`=new.env()     #Set one environment variable for each model type
`Mod. VRE`=new.env()
list_of_names=c("Low VRE","Mod. VRE")
env_list=list(`Low VRE`,`Mod. VRE`)
#setwd('')               #Optional set working directory
load('./3mo_060920/IM3_ext_3mo_060920_M678_lowVG_final.RData' ,`Low VRE`)
load('./3mo_060920/IM3_ext_3mo_060920_M678_highVG_final.RData',`Mod. VRE`)

annual.gen.stats<-rbindruns('annual.gen.stats',env_list)
violations<-rbindruns('violations',env_list)
net.gen.RSG<-rbindruns('net.gen.RSG',env_list)
meltitem('gen.type.region',env_list,'gen.type.region',c("Type","TEPPC.Region"))
gen.type.region<-rbindruns('gen.type.region',env_list)
meltitem('load.region',env_list,'load.region.interval',c("time","TEPPC.Region"))
load.region.interval<-rbindruns('load.region.interval',env_list)
setnames(load.region.interval,"value","MW")
meltitem('gen.type.region.time',env_list,'gen.type.region.interval',c("time","Type","TEPPC.Region"))
gen.type.interval<-rbindruns('gen.type.region.interval',env_list)         
setnames(gen.type.interval,"value","MW")
meltitem('thermal.hour.cap',env_list,'thermal.hour.cap',c("time"))
thermal.hour.cap<-rbindruns('thermal.hour.cap',env_list)
setnames(thermal.hour.cap,"value","MW")
reserve.type<-rbindruns('reserve.type',env_list)
meltitem('cap.type.region',env_list,'cap.type.region',c("Type","TEPPC.Region"))
cap.type.region<-rbindruns('cap.type.region',env_list)


# Filename to save .HTML file.
output.filename = 'IM3_ext_3mo_060920_1drought_heat4_M678_test.html'
scenario.name='scenarios'                     #prefix for plots

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Order to display results when creating plots (these will be across the x-axis)

# CUSTOM
# plotting.order=annual.gen.stats$Scenario
plotting.order = list(
                      "DA_IM3_2010_none",
                      "DA_IM3_2010_Derate4",
                      "DA_IM3_2010_Load4",
                      "DA_IM3_2010_WaterA", 
                      "DA_IM3_2010_Combo4A",
                     "DA_IM3_2024_none",
                     "DA_IM3_2024_Derate4",
                     "DA_IM3_2024_Load4" ,
                     "DA_IM3_2024_WaterA",  
                     "DA_IM3_2024_Combo4A") 
             
scenario.labels = gsub("DA_IM3_",'',plotting.order)
scenario.labels = gsub("_3mo",'',scenario.labels)

scenario.labels.detail=c(
  "none",
  "Derate 4",
  "Load 4",
  "Water 1977", 
  "Heat 4 Water 1977",
  "none",
  "Derate 4",
  "Load 4" ,
  "Water 1977", 
  "Heat 4  Water 1977")
scenario.labels.detail=c("none","Derating","Load","Drought","Combination","none","Derating","Load","Drought","Combination")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Making labels
makecats(annual.gen.stats)
makewaters(annual.gen.stats)
annual.gen.stats$Label = factor(annual.gen.stats$Label, levels = c("Base","Derating","Load","Water","Combination"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set order for generation types to appear in plots, and the color each generaiton type should be. 
gen.order = c('Nuclear', 'Coal', 'Hydro', 'Gas CC', 'Gas CT', 'Steam', 'CHP-QF', 'ICE Gas', 
              'Biomass', 'Geothermal', 'Other', 
              'Storage', 'CSP','CSP-TES', 'PV', 'RPV','Wind','DR')

gen.color = c('firebrick','gray80',  'lightblue', 'darkolivegreen4','lightpink',  'purple', 'gray20', 'purple',
              'mediumpurple2', 'khaki1', 'mediumpurple3', 
              'gray40', 'goldenrod3', 'goldenrod', 'darkorange','darkorange2', 'steelblue3','black')

gen.color = setNames( gen.color, gen.order )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set colors to plot all the scenarios as
scenarios = plotting.order
scenario.color=setNames(heat.colors(length(scenarios),alpha=1),scenarios)

#Set colors for scenario groups
labels.color=c('gray40','darkolivegreen4','darkorange','steelblue3','firebrick')
labels.color=setNames(labels.color,c('Base','Derating','Load','Water','Combination'))

water.color=c('gray40','blue','steelblue3')
water.color=setNames(water.color,c('Non-drought','1977 Drought','2001 Drought'))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette=setNames(cbPalette,c('Northern California','Rockies','Canada','Basin','Southwest','Northwest','Mexico','Southern California'))

namer=data.table(Scenario=as.character(scenarios),label_detail=scenario.labels.detail)
namer$label_detail=factor(namer$label_detail,levels=c("none","Derating","Load","Drought","Combination"))

# ##########################################################################
# Below is calculations and data foramtting to get the data in the correct format for plotting in the next file
# ##########################################################################

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate total cost including violations and rank scenarios according to total cost
violations=violations[Scenario %in% plotting.order]
annual.gen.stats=annual.gen.stats[Scenario %in% plotting.order]
annual.gen.stats[, `Total Cost (USD)` := (`Cost ($)` + violations[,`Unserved Energy Cost ($000)`]*1000 + violations[,`Reserve Shortage Cost ($000)`]*1000 + 
                                          violations[,`Line Flow Violation Cost ($000)`]*1000 + violations[, `Hydro violation cost ($000)`]*1000) ]
annual.gen.stats[, Rank := rank(annual.gen.stats$`Total Cost ($)`)]
annual.gen.stats$Scenario = factor(annual.gen.stats$Scenario, levels = plotting.order)
annual.gen.stats[,`Unit Cost (USD per MWh)`:= `Cost ($)`/`Generation (GWh)`/1000]
annual.gen.stats$Infrastructure = factor(annual.gen.stats$Infrastructure,levels=c("Low VRE","Mod. VRE"))
annual.gen.stats[,`Congested Lines`:=`# Congested Lines`]
annual.gen.stats[,`# Congested Lines`:=NULL]
setnames(annual.gen.stats,c("Cost ($)"),c("Cost (USD)"))
setnames(annual.gen.stats,c("Curtailment %"),c("Curtailment Percent"))
setnames(annual.gen.stats,c("Curtailment % of VG"),c("Curtailment Percent of VG"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generation and Load by Region GWh - Replaces imports/exports

net.gen.RSG=net.gen.RSG[Scenario %in% plotting.order]
makecats(net.gen.RSG)
makewaters(net.gen.RSG)
net.gen.RSG$Scenario=factor(net.gen.RSG$Scenario, levels=plotting.order)
net.gen.RSG$Infrastructure = factor(net.gen.RSG$Infrastructure,levels=c("Low VRE","Mod. VRE"))
net.gen.RSG$Label = factor(net.gen.RSG$Label, levels = c("Base","Derating","Load","Water","Combination"))
net.gen.RSG$TEPPC.Region=factor(net.gen.RSG$TEPPC.Region,
                                levels=c("Northern California","Rockies","Canada","Basin","Southwest","Northwest","Mexico","Southern California"))

net.gen.RSG=merge(net.gen.RSG,net.gen.RSG[Label=="Base",.(TEPPC.Region,Infrastructure,V1Generation,V1Load,netgen)],all.x="true",
                  by=c("TEPPC.Region","Infrastructure"),
                  suffixes=c("",".none"))

net.gen.RSG[,netgen.diff:=netgen-netgen.none]
net.gen.RSG[,Generation.diff:=V1Generation-V1Generation.none]
net.gen.RSG[,Load.diff:=V1Load-V1Load.none]
net.gen.RSG[,Load.perdiff:=Load.diff/V1Load.none]

net.gen.RSG=merge(net.gen.RSG,net.gen.RSG[Label=="Water",.(TEPPC.Region,Infrastructure,Water,V1Generation,V1Load,netgen)],all.x="true",
                  by=c("TEPPC.Region","Infrastructure","Water"),
                  suffixes=c("",".drought"))
net.gen.RSG[,netgen.drdiff:=netgen-netgen.drought]
net.gen.RSG[,Generation.drdiff:=V1Generation-V1Generation.drought]
net.gen.RSG[,Load.drdiff:=V1Load-V1Load.drought]

net.gen.RSG.change=net.gen.RSG[,.(netgen.abs.sum=sum(abs(netgen)),netgen.abs.none.sum=sum(abs(netgen.none))),by=c("Scenario","Water","Label","Infrastructure")]
net.gen.RSG.change[,percent.change:=(netgen.abs.sum-netgen.abs.none.sum)/netgen.abs.none.sum*100,by=c("Scenario","Water","Label","Infrastructure")]
net.gen.RSG.change=merge(net.gen.RSG.change,annual.gen.stats[,.(Infrastructure,Water,Scenario,`Cost (USD)`,`Unit Cost (USD per MWh)`,Label)],
                                                             by=c("Infrastructure","Water","Scenario","Label"),all.x=TRUE)
net.gen.RSG.change.SCA=net.gen.RSG[TEPPC.Region=="Southern California",.(netgen.abs.sum=sum(abs(netgen)),netgen.abs.none.sum=sum(abs(netgen.none))),by=c("Scenario","Water","Label","Infrastructure")]
net.gen.RSG.change.SCA[,percent.change:=(netgen.abs.sum-netgen.abs.none.sum)/netgen.abs.none.sum*100,by=c("Scenario","Water","Label","Infrastructure")]


net.gen.RSG[netgen<=0,netgen.imports:=-netgen] #Look only at imports
net.gen.RSG[netgen>0,netgen.imports:=0]
net.gen.RSG[netgen.none<=0,netgen.imports.none:=-netgen.none]
net.gen.RSG[netgen.none>0,netgen.imports.none:=0]
net.gen.RSG[,netgen.imports.diff:=netgen.imports-netgen.imports.none]

net.gen.RSG[netgen<=0,netgen.exports:=0] #Look only at exports
net.gen.RSG[netgen>0,netgen.exports:=netgen]
net.gen.RSG[netgen.none<=0,netgen.exports.none:=0]
net.gen.RSG[netgen.none>0,netgen.exports.none:=netgen.none]
net.gen.RSG[,netgen.exports.diff:=netgen.exports-netgen.exports.none]
net.gen.RSG=merge(net.gen.RSG,namer,by=c("Scenario"))
# ##########################################################################
# Plots with all generation types
# Cap by type and region
cap.type.region$Type = factor(cap.type.region$Type, levels = rev(gen.order))
cap.type.region = cap.type.region[Scenario %in% plotting.order, ]
cap.type.region$Scenario = factor(cap.type.region$Scenario, levels = plotting.order)

makecats(cap.type.region)
makewaters(cap.type.region)
cap.type.region$Label = factor(cap.type.region$Label, levels = c("Base","Derating","Load","Water","Combination"))
cap.type.region$TEPPC.Region=factor(cap.type.region$TEPPC.Region,
                                    levels=c("Northern California","Rockies","Canada","Basin","Southwest","Northwest","Mexico","Southern California"))


cap.type.region[Type=="PV"&TEPPC.Region=="Basin",value:=(value-30792)]
# 30.79226 GW of PV capacity is removed from Basin region, this is unused capacity that does not match intended High VRE infrastructure. 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Gen by type and region
gen.type.region = gen.type.region[Scenario %in% plotting.order, ]

makecats(gen.type.region)
makewaters(gen.type.region)

gen.type.region.tmp=gen.type.region[Label=="Base",.(Type, TEPPC.Region,Infrastructure,value)] #Get baseline
gen.type.region=merge(gen.type.region,gen.type.region.tmp,by=c("Type","TEPPC.Region","Infrastructure"))
rm(gen.type.region.tmp)
setnames(gen.type.region,c("value.x","value.y"),c("value","none"))
gen.type.region[,diff:=value-none]

gen.type.region.tmp=gen.type.region[Label=="Water",.(Type, TEPPC.Region,value,Water,Infrastructure)] #Get baseline
gen.type.region=merge(gen.type.region,gen.type.region.tmp,by=c("Type","TEPPC.Region","Water","Infrastructure"),
                      suffixes=c("",".drought"),all.x=TRUE)
rm(gen.type.region.tmp)
gen.type.region[,drdiff:=value-value.drought]
gen.type.region<-merge(gen.type.region,namer,by=c("Scenario"))
gen.type.region$Type = factor(gen.type.region$Type, levels = rev(gen.order))
gen.type.region$Scenario = factor(gen.type.region$Scenario, levels = plotting.order)
gen.type.region$Label = factor(gen.type.region$Label, levels = c("Base","Derating","Load","Water","Combination"))
gen.type.region$TEPPC.Region=factor(gen.type.region$TEPPC.Region,
                                levels=c("Northern California","Rockies","Canada","Basin","Southwest","Northwest","Mexico","Southern California"))


load.region.interval=load.region.interval[Scenario %in% plotting.order]
load.region.interval$Scenario = factor(load.region.interval$Scenario, levels = plotting.order)

makecats(load.region.interval)
makewaters(load.region.interval)
load.region.interval$Label = factor(load.region.interval$Label, levels = c("Base","Derating","Load","Water","Combination"))
load.region.interval[,hour:=hour(time)]
load.region.interval[,day:=day(time)]
load.region.interval[,Month:=month(time)]


gen.type.interval$Type = factor(gen.type.interval$Type, levels = rev(gen.order))
gen.type.interval <- gen.type.interval[Scenario %in% plotting.order]
gen.type.interval$Scenario <- factor(gen.type.interval$Scenario,level=plotting.order)
gen.type.interval[,hour:=hour(time)]
gen.type.interval[,day:=day(time)]
gen.type.interval[,Month:=month(time)]
makecats(gen.type.interval)
makewaters(gen.type.interval)
gen.type.interval$Label = factor(gen.type.interval$Label, levels = c("Base","Derating","Load","Water","Combination"))
gen.typeonly.interval = gen.type.interval[,sum(MW,na.rm=TRUE),by=c("time","hour","day","Month","Scenario","Label","Water","Infrastructure","Type")]

setnames(gen.typeonly.interval,"V1","MW")

## Capacity ##
thermal.hour.cap=thermal.hour.cap[Scenario %in% plotting.order]
makecats(thermal.hour.cap)
makewaters(thermal.hour.cap)
thermal.hour.cap$Label = factor(thermal.hour.cap$Label, levels = c("Base","Derating","Load","Water","Combination"))


## Quantify hourly thermal availability (or fleet CF) ##########
thermal.hour.fleetcf=gen.type.interval[Type=='Coal'|Type=='Gas CC'|Type=='Gas CT',
                                       sum(MW),by=c("time","Scenario","Infrastructure")]
setnames(thermal.hour.fleetcf,"V1","Gen")
thermal.hour.fleetcf=merge(thermal.hour.cap,thermal.hour.fleetcf,
                           by=c("time","Scenario","Infrastructure"),all=TRUE)
thermal.hour.fleetcf[,FleetCF:=Gen/MW]
thermal.hour.fleetcf[,interval := rank(-FleetCF,ties.method="random"), by=Scenario]
thermal.hour.fleetcf[,LeftoverMW:=MW-Gen]
thermal.hour.fleetcf[,leftoverinterval := rank(-LeftoverMW,ties.method="random"), by=Scenario]
thermal.hour.fleetcf[,hour:=hour(time)]
thermal.hour.fleetcf[,day:=day(time)]
thermal.hour.fleetcf[,Month:=month(time)]

#### Hourly net generation ####
net.gen.RSG.interval=merge(load.region.interval,gen.type.interval[,sum(MW),by=c("time","TEPPC.Region","Scenario","Infrastructure"),],
                           by=c("time","TEPPC.Region","Scenario","Infrastructure"),all=TRUE)
setnames(net.gen.RSG.interval,c("V1"),c("MW.gen"))
net.gen.RSG.interval[,netgen:=MW.gen-MW]
net.gen.RSG.interval[,interval.scen := rank(-netgen,ties.method="random"), by=c("Scenario","TEPPC.Region","Infrastructure")]

#Group importers & exporters (according to net)
net.gen.interval=net.gen.RSG.interval[netgen<0,sum(netgen),by=c("time","Scenario","Label","Water","Infrastructure")]
setnames(net.gen.interval,"V1","negative.net.gen")
net.gen.interval=merge(net.gen.interval,
                       net.gen.RSG.interval[netgen>0,sum(netgen),by=c("time","Scenario","Label","Water","Infrastructure")]
                       ,all=TRUE)
setnames(net.gen.interval,"V1","positive.net.gen")
net.gen.interval[,hour:=hour(time)]
net.gen.interval[,day:=day(time)]
net.gen.interval[,Month:=month(time)]
net.gen.interval[,interval.scen:=rank(positive.net.gen,ties.method="random"),by=c("Scenario")]
net.gen.interval[,positive.net.gen.base:=positive.net.gen[Label=="Base"],by=c("Infrastructure","time","Month","day","hour")]
net.gen.interval[,net.gen.base.diff:=positive.net.gen-positive.net.gen.base]
net.gen.interval$Label = factor(net.gen.interval$Label, levels = c("Combination","Water","Load","Derating","Base")) #Reverse levels for plot

net.gen.day=net.gen.interval[,.(tot.net.gen=sum(positive.net.gen),max.net.gen=max(positive.net.gen),
                                time=min(time),mean.net.gen=mean(positive.net.gen)),
                             by=c("day","Month","Scenario","Label","Water","Infrastructure")]

#manual selection # Peak load times in base year are July 15 and Aug 25
start.2010=as.POSIXct("2010-8-7",tz="UTC")
end.2010=as.POSIXct("2010-8-14",tz="UTC")
start.2024=as.POSIXct("2024-8-7",tz="UTC")
end.2024=as.POSIXct("2024-8-14",tz="UTC")

# Labeling helpers
reg_names<-c(`Base`="-",`Derating`="Derating",
             `Load`="Load",`Water`="Water",`Combination`="Combination",
             `Basin`="Basin",`Canada`="Canada",`Northern California`="N. Cal.",
             `Northwest`="Northwest",`Rockies`="Roc.",`Southern California`="S. Cal.",
             `Mexico`="Mexico",`Southwest`="Southwest",
             `Low VRE`="Low VRE",`Mod. VRE`="Mod. VRE" ,
             `Coal`="Coal",`Gas CC`="Gas CC",`Hydro`="Hydro")
month_names<-c(`1`="Jan",`2`="Feb",`3`="Mar",`4`="Apr",
               `5`="May",`6`="June",`7`="July",`8`="Aug",
               `9`="Sep",`10`="Oct",`11`="Nov",`12`="Dec")

# ##########################################################################s
# ##########################################################################
# Call the .Rmd file which creates the resulting HTML report file. 
render(input='./rplexos_queries/plot_creator_extremes_paper_final.Rmd', c("html_document"), 
       output_file=output.filename)
