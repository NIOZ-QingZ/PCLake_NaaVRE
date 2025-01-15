# ---------------------------------------------------
# Initialisation
# ---------------------------------------------------

#set java memory settings prior to loading java enabled packaged (e.g. xlsx)
options( java.parameters = "-Xmx4g")

# load needed libraries
LoadPackage("ggplot2")
LoadPackage("deSolve")
LoadPackage("reshape2") 
#LoadPackage("xlsx") 
LoadPackage("XLConnect") 

# define path of working directory
dir_SCEN        <- paste(dir_SCHIL,work_case,"/",sep="")
#path_DATM		=	paste(dir_DATM,file_DATM, sep='')
path_DATM		=	file_DATM
cat(path_DATM, '\n')

#check if we specifically indicated that we are doing a run to generate initrep, 
#	if not set the variable to false for standard runs
if(exists("tGENERATE_INIT")==FALSE){
	tGENERATE_INIT	=	FALSE
}

#check if we specifically indicated that we are doing a bifurcation run , 
#	if not set the variable to false for standard runs
if(exists("tRUN_BIFURC")==FALSE){
	tRUN_BIFURC	=	FALSE
}

# create logfile
start_time      <- Sys.time()
LogFile         <- paste(dir_SCEN,"logfile.txt",sep="")
OpenLogFil(LogFile)
WriteLogFile(LogFile,ln="")
WriteLogFile(LogFile,ln="initializing......")

# create main directory for model output
dir.create(dir_SCEN,showWarnings=FALSE)

# create directory for model results (tables)
dir.create(paste(dir_SCEN,"results/",sep=""),showWarnings=FALSE)

# create directory for model output (which content is cleared, if any)
Dir_output      <- paste(dir_SCEN,"output/",sep="")
dir.create(Dir_output,showWarnings=FALSE)
ClrDir(Dir_output)

# create directory for model code to be compiled 
Dir_source_adjusted <- paste(dir_SCHIL,"scripts/source_cpp_adjusted/",sep="")
dir.create(Dir_source_adjusted,showWarnings=FALSE)
ClrDir(Dir_source_adjusted)


# ---------------------------------------------------
# 	Read input from DATM implementation in excel directly
# ---------------------------------------------------
WriteLogFile(LogFile,ln=paste("Reading DATM excel input from file ",file_DATM,sep=''))

#read in the workbook
wbDATM=loadWorkbook(file=path_DATM)

#---Read control values for the model
dfRUNSETTINGS_PRICE_RAW	=	readWorksheet(wbDATM, sheet="Control", startRow=1, endRow=90 ,startCol=1,endCol=5)
#read.xlsx2(file=path_DATM, sheetName="Control", startRow=1, endRow=90 ,colIndex=1:5, as.data.frame=TRUE, header=TRUE)
dfRUNSETTINGS1		=	dfRUNSETTINGS_PRICE_RAW[which(dfRUNSETTINGS_PRICE_RAW[,2]!=""),]
dfRUNSETTINGS		=	dfRUNSETTINGS1[-1,-1]
rownames(dfRUNSETTINGS)	=	as.character(unlist(dfRUNSETTINGS1[-1,1]))
colnames(dfRUNSETTINGS)	=	as.character(unlist(dfRUNSETTINGS1[1,-1]))
for(nCOL in 1:ncol(dfRUNSETTINGS)){ dfRUNSETTINGS[,nCOL]	=	as.numeric(as.character(unlist(dfRUNSETTINGS[,nCOL]))) }

#---Read state values for the model
dfSTATES_PRICE_RAW		=	readWorksheet(wbDATM, sheet="states", startRow=1, endRow=200 ,startCol=1,endCol=90)
#read.xlsx2(file=path_DATM, sheetName="states", startRow=1, endRow=200, colIndex=1:90, as.data.frame=TRUE, header=TRUE)
dfSTATES			=	dfSTATES_PRICE_RAW[which(dfSTATES_PRICE_RAW[,2]!=""),which(colnames(dfSTATES_PRICE_RAW) %in% 
										c("sStateName","sInitialStateName","iReportState","sDefaultSetTurbid0","sDefaultSetClear1","sAltenativeSet2","sAlternativeSet3"))]
rownames(dfSTATES)	=	as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sStateName")]))
dfSTATES			=	dfSTATES[,-which(colnames(dfSTATES)=="sStateName")]
rownames(dfSTATES)	=	gsub("_","",rownames(dfSTATES))
dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")]	=	gsub("_","",as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")])))
for(nCOL in which(colnames(dfSTATES)%in%c("iReportState","sDefaultSetTurbid0","sDefaultSetClear1","sAltenativeSet2","sAlternativeSet3"))){ 
	dfSTATES[,nCOL]	=	as.numeric(as.character(unlist(dfSTATES[,nCOL]))) }
	

#---Read parameter values for the model
dfPARAMS_PRICE_RAW		=	readWorksheet(wbDATM, sheet="parameters", startRow=1, endRow=1200 ,startCol=1,endCol=90)
#read.xlsx2(file=path_DATM, sheetName="parameters", startRow=1, endRow=1200, colIndex=1:90, as.data.frame=TRUE, header=TRUE)
dfPARAMS			=	dfPARAMS_PRICE_RAW[which(dfPARAMS_PRICE_RAW[,2]!=""),which(colnames(dfPARAMS_PRICE_RAW) %in% 
										c("sName","iReport","sMinValue","sMaxValue","sDefault0","sSet1","sSet2","sSet3"))]
rownames(dfPARAMS)	=	as.character(unlist(dfPARAMS[which(colnames(dfPARAMS)=="sName")]))
dfPARAMS			=	dfPARAMS[,-which(colnames(dfPARAMS)=="sName")]
rownames(dfPARAMS)	=	gsub("_","",rownames(dfPARAMS))
for(nCOL in 1:ncol(dfPARAMS)){ dfPARAMS[,nCOL]	=	as.numeric(as.character(unlist(dfPARAMS[,nCOL]))) }


#---Read auxilliaries to report from the DATM file 
dfAUXIL_PRICE_RAW		=	readWorksheet(wbDATM, sheet="derivatives", startRow=1, endRow=2800 ,startCol=1,endCol=15)
#read.xlsx2(file=path_DATM, sheetName="derivatives", startRow=1, endRow=2000, colIndex=c(1,5), as.data.frame=TRUE, header=TRUE)
dfAUXIL			=	dfAUXIL_PRICE_RAW[which(dfAUXIL_PRICE_RAW[,2]!=""),which(colnames(dfAUXIL_PRICE_RAW) %in% 
										c("sName","iReport"))]
rownames(dfAUXIL)	=	as.character(unlist(dfAUXIL[which(colnames(dfAUXIL)=="sName")]))
dfAUXIL			=	dfAUXIL[,-which(colnames(dfAUXIL)=="sName"),drop=FALSE]
rownames(dfAUXIL)	=	gsub("_","",rownames(dfAUXIL))
for(nCOL in 1:ncol(dfAUXIL)){ dfAUXIL[,nCOL]	=	as.numeric(as.character(unlist(dfAUXIL[,nCOL]))) }

#define variables to report the output from
lVARS_REPORT	=	c(rownames(dfSTATES[which(dfSTATES$iReportState==1),,drop=F]),rownames(dfAUXIL[which(dfAUXIL$iReport==1),,drop=F]), rownames(dfPARAMS[which(dfPARAMS$iReport==1),,drop=F]))


# ---------------------------------------------------

# ********************************************************
# --------------------------------------------------------
# Define run settings or read run setting from interface:
# 1. run time, integrator, output time step 
# 2. initial state to be changed
# 3. parameters to be changed
# 4. forcing functions to be imposed on the model 
# 5. define output variables (states and auxilaries) 
# --------------------------------------------------------
# ********************************************************
WriteLogFile(LogFile,ln="- reading run settings")
# -----------------------------------------------------------
# 1. run time, integrator, output time step 
# -----------------------------------------------------------
#Get from RunSettings
#	Note that we use the settings of the first run set only

# model run time (in years)
runtime_years      <- dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dReady"),1]    
# time step at which output is generated (in days)
output_time_step   <- dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dRepStep"),1]      
integrator         <- 18     # integrator to solve the model equations (ordinary first order differential equations)
							 # 		Note, we do not take the integrator from the run settings as R has different integers which can be superior
	# available intergrators:
    # 1 = Euler (fixed time step)
    # 2 = Runge Kutta 2nd order (Heun) (fixed time step)
    # 3 = Runge Kutta 4th order (fixed time step)
    # 4 = Runge Kutta pair of order 3(2) (variable time step)
    # 5 = Runge Kutta pair of order 3(2), according to Bogacki & Shampine (1989) (variable time step)
    # 6 = Runge Kutta pair of order 4(3), according to Fehlberg (1967) (variable time step)
    # 7 = Runge Kutta pair of order 5(4), according to Fehlberg (1967) (variable time step)
    # 8 = Runge Kutta pair of order 5(4), according to Cash & Karp (1990) (variable time step)
    # 9 = Runge Kutta pair of order 5(4), according to ..... (variable time step)
    # 10= Runge Kutta pair of order 6(5(4)), according to Dormand & Prince (1980) (variable time step)
    # 11= Runge Kutta pair of order 7(5(4)), according to Dormand & Prince (1980) (variable time step)
    # 12= Runge Kutta pair of order 8(7), according to Dormand & Prince (1980) (variable time step)
    # 13= Runge Kutta pair of order 8(7), according to Fehlberg (1967) (variable time step)
    # 14= "lsoda"
    # 15= "lsode"
    # 16= "lsodes"
    # 17= "lsodar"
    # 18= "vode" (RECOMMENDED!!!)
    # 19= "daspk"
    # 20= "ode23"
    # 21= "ode45"
    # 22= "radau"
    # 23= "bdf"
    # 24= "bdf_d"
    # 25= "adams"
    # 26= "impAdams"
    # 27= "impAdams_d"
    # 28= "iteration"

# define time over which results will be reported
fREP_START_YEAR		=	dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dRepStart"),1] 
	
# define time over which results will be averaged for e.g. bifurcation analysis 
#	Generally refers to a summer growing season period of e.g. day 150-210 (standard setting PCLake) or day 91-259 (the summer half of the year, 1 April to 30 Sept)
fAVG_START_YEAR		=	dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dAvgStart"),1] 
fAVG_START_DAY		=	dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dAvgStartWithinYear"),1]  
fAVG_END_DAY		=	dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dAvgEndWithinYear"),1] 

# timestep at which derivatives are calculated (if integrator uses fixed time step)
internal_time_step <- dfRUNSETTINGS[which(rownames(dfRUNSETTINGS)=="dIntStep"),1]    

# -----------------------------------------------------------
# 2. initial state to be changed
# -----------------------------------------------------------

inits_to_change <- c(
#  sDepthW = 2.0,
#  sPO4W = 0.00001
)
  
# -----------------------------------------------------------
# 3. parameters to be changed
# -----------------------------------------------------------

# define the sediment type
sediment_type  <- 0                              # available sediment types: 0=default settings (/source_cpp/*sp.cpp), 1=clay, 2=peat, 3=sand

# get parameter values that define the sediment_type
soil_param     <- SetSedimentType(sediment_type) 

pars_to_change <- c(
 	#InitCalc = 1 #strongly recommended if initial state is changed
)


