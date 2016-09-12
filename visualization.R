#TODO:
#Add titles and axis labels to the plots
#Some variables should be printed as tables rather than as plots, e.g. for what's installed. Otherwise it's useless.
#Add if-else statements in here as necessary to accommodate different forms of inputs
#Modify this so it generates a single report summarizing the results of a model run in a single file, with both table and image outputs,
# maybe including a page for the installed technologies and a page for the time varying data, and eventually a page with GIS outputs 

library(xlsx)
library(reshape)
library(ggplot2)

P = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Input_energy")
P_melted <- melt(P, id=c("NA."))
plot1 <- ggplot(P_melted,aes(x=NA.,y=value,color=variable)) + geom_line() +
  labs(title="Energy supplied per technology",x="Hour", y="Energy supplied (kWh)")
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\P.png",plot=plot1,width=20,height=5)
rm(P)
rm(P_melted)

P_export = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Exported_energy")
PE_melted <- melt(P_export, id=c("NA."))
plot2 <- ggplot(PE_melted,aes(x=NA.,y=value,color=variable)) + geom_line()
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\P_export.pdf",plot=plot2,width=10,height=5)
rm(P_export)
rm(PE_melted)

y_on = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Operation")
yon_melted <- melt(y_on, id=c("NA."))
plot3 <- ggplot(yon_melted,aes(x=NA.,y=value,color=variable)) + geom_line()
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\y_on.pdf",plot=plot3,width=10,height=5)
rm(y_on)
rm(yon_melted)

Qin = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Storage_input_energy")
Qout = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Storage_output_energy")
Qout$Elec = -Qout$Elec
Qout$Heat = -Qout$Heat
Q_combined = cbind(Qin,Qout)
Q_melted = melt(Q_combined, id=c("NA."))
plot4 <- ggplot(Q_melted,aes(x=NA.,y=value,color=variable)) + geom_line()
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\Q.pdf",plot=plot4,width=10,height=5)
rm(Qin)
rm(Qout)
rm(Q_combined)
rm(Q_melted)

E = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Storage_SOC")
E_melted <- melt(E, id=c("NA."))
plot5 <- ggplot(E_melted,aes(x=NA.,y=value,color=variable)) + geom_line()
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\E.pdf",plot=plot5,width=10,height=5)
rm(E)
rm(E_melted)

y = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Installation")
y_melted <- melt(y, id=c("NA."))
plot6 <- ggplot(y_melted,aes(x=variable,y=value,fill=NA.)) + geom_bar(stat="identity", position=position_dodge())
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\Y.pdf",plot=plot6,width=10,height=5)
rm(y)
rm(y_melted)

Capacity = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\energy_hub_results.xlsx","Capacity")
Capacity_melted <- melt(Capacity, id=c("NA."))
plot7 <- ggplot(Capacity_melted,aes(x=variable,y=value,fill=NA.)) + geom_bar(stat="identity", position=position_dodge())
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\Capacity.pdf",plot=plot7,width=10,height=5)
rm(Capacity)
rm(Capacity_melted)

Heat = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\results_BSabstract\\Energy_outputs_dynamic_pricing.xlsx","Heat2")
Elec = read.xlsx("H:\\projects\\ModularEnergyHub\\aimms_model\\energy_hub\\results_BSabstract\\Energy_outputs_dynamic_pricing.xlsx","Elec3")
Elec <- Elec[1:168,]
#Elec$PV[is.na(Elec$PV)] <- 0
#Elec$Grid[is.na(Elec$Grid)] <- 0
#Elec$Battery.output[is.na(Elec$Battery.input)] <- 0
Elec$Battery.input <- NULL
#Elec$Demand <- NULL
Elec_melted <- melt(Elec, id=c("Hour"))
plot1 <- ggplot(Elec_melted,aes(x=Hour,y=value,color=variable)) + geom_line() +
  labs(title="Electricity supplied per technology (dynamic pricing)",x="Hour", y="Energy supplied (kWh)",color="Energy source")
ggsave(filename="H:\\projects\\ModularEnergyHub\\results\\Output.jpg",plot=plot1,width=15,height=3)

