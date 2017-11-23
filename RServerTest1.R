list.files(rxGetOption("sampleDataDir"))
inDataFile <- file.path(rxGetOption("sampleDataDir"), "mortDefaultSmall2000.csv")
mortData <- rxImport(inData = inDataFile)
class(mortData)
#mortDataXDF<-file.path(rxGetOption("sampleDataDir"),"mortDataXDF.xdf")
mortDataXDF<-file.path("d:","mortDataXDF.xdf")
mortData <- rxImport(inData = inDataFile,outFile = mortDataXDF)
rxGetInfo(mortDataXDF)

rxImport(inData = inDataFile,outFile = mortDataXDF,rowsPerRead = 200,overwrite = TRUE)

rxGetInfo(mortDataXDF,getVarInfo = TRUE,getBlockSizes = TRUE)

library(parallel)
#核数
detectCores()

ls("package:datasets")

nrow(mortData)
ncol(mortData)
names(mortData)
head(mortData, n = 3)
rxGetInfo(mortData, getVarInfo = TRUE, numRows=3)

mortDataNew <- rxDataStep(
  # Specify the input data set
  inData = mortData,
  # Put in a placeholder for an output file
  outFile = outFile2,
  # Specify any variables to keep or drop
  varsToDrop = c("year"),
  # Specify rows to select
  rowSelection = creditScore < 850,
  # Specify a list of new variables to create
  transforms = list(
    catDebt = cut(ccDebt, breaks = c(0, 6500, 13000),
                  labels = c("Low Debt", "High Debt")),
    lowScore = creditScore < 625))
