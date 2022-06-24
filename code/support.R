carga <- function(){
  myDF <<- readxl::read_xlsx("data/blupDoencas_2022-06-24.xlsx",sheet = "Sheet1")
  myCount <<- readxl::read_xlsx("data/TPPs_count.xlsx",sheet = "Sheet1")
  
  myGenLoc <<- readxl::read_xlsx("data/TPPs_genoLoc-2022-06-23.xlsx",sheet = "Sheet1")
  myChecks0 <- readxl::read_xlsx("data/renataMilho21062022.xlsx",sheet = "Checks")
  myChecks <- tidyr::gather(myChecks0, TPP, valor,TPP09:TPP12)
  myChecks$valor <- as.numeric(myChecks$valor)
  myChecks <<- myChecks
  myJoint <<- readxl::read_xlsx("data/conjuntaGenotipos.xlsx",sheet = "Sheet1")
  myDD <<- readxl::read_xlsx("data/blupDoencas_2022-06-24.xlsx", sheet = "Sheet1")
}

colorFun <- function(reactColor){
  testeFun <- is.na(reactColor)
  
  if (testeFun == T){
    resultColor <- "teal"
    return(resultColor)
    break
  }

  if (reactColor < 3){
    resultColor <- "green"
  }else if(reactColor >=3 && reactColor <5){
    resultColor <- 'blue'
  }else if(reactColor>=5 && reactColor < 7){
    resultColor <- 'orange'
  }else if(reactColor>= 7){
    resultColor <- "red"
  }
  
  return(resultColor)
}
