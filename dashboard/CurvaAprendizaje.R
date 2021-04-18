library(drc)
library(devtools)
library(cpp11)
library(aomisc)

generarAjusteDatos <- function(operariosData){
  operarios = unique(operariosData[c("Operario")])[[1]]
  resultados = list()
  for(i in 1:length(operarios)){
    actualData <- subset(operariosData,operariosData$Operario==operarios[[i]])
    model <- drm(Tiempo ~ Unidad, fct = DRC.powerCurve(),
                 data = actualData)
    resultados[[operarios[[i]]]]= model
  }
  return (resultados)
  #summary(model)
  #plot(model, log="", main = "Power curve (b = 0.33)")
}
potencial = function(a,b,x){a*(x^b)}

generarDatos = function(a,b){
  return (potencial(a,b,1:100))
}

mejorOperario = function(operarios){
  nombres=names(operarios)
  mejorOp=""
  val=101
  for(i in 1:length(operarios)){
    actVal=2^(operarios[[i]]["parmMat"][[1]][[2]])*100
    if(actVal<val){
      mejorOp=nombres[[i]]
      val=actVal
    }
  }
  return(paste0(mejorOp," con una tasa de aprendizaje de: ",round(val,2),"%"))
}

infoOperarios = function(operarios){
  a_vector <- vector(length = length(operarios))
  b_vector <- vector(length = length(operarios))
  ta_vector <- vector(length = length(operarios))
  me_vector <- vector(length = length(operarios))
  for(i in 1:length(operarios)){
    actVal=2^(operarios[[i]]["parmMat"][[1]][[2]])*100
    a_vector[[i]] = round(operarios[[i]]["parmMat"][[1]][[1]],3)
    b_vector[[i]] = round(operarios[[i]]["parmMat"][[1]][[2]],3)
    ta_vector[[i]] = paste0(round(actVal,2),"%")
    me_vector[[i]] = paste0(round(100-actVal,2),"%")
  }
  return(data.frame(
    Nombre = names(operarios), 
    "T.A" = ta_vector,
    "T.M" = me_vector, 
    k = a_vector,
    n = b_vector
  ))
}
