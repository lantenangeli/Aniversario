#' @export
aniversário <- function(x,y,z){
  
  
  x <- readline("Dia do seu aniversário?(DD)    :")
  
  if(is.na(as.numeric(x))){
    stop("O valor fornecido não é válido. Por favor digite um número.")
    return(x)
  }
  
  
  
  y <- readline("Mês do seu aniversário?(MM)    :")
  
  if(is.na(as.numeric(y))){
    stop("O valor fornecido não é válido. Por favor digite um número.")
    return(y)
  }
  
  
  z <- readline("Ano do seu aniversário?(YYYY)    :")
  
  if(is.na(as.numeric(z))){
    stop("O valor fornecido não é válido. Por favor digite um número.")
    return(z)
  }
  
  
  
  x <- as.numeric(x); y <- as.numeric(y); z <- as.numeric(z)
  ani <- as.character(paste0(x,"/",y,"/",z))
  ani2 <- strptime(ani, format = "%d/%m/%Y")
  
  
  hoje <- format(Sys.Date(), "%d-%m")
  ani3 <- format(ani2, "%d-%m")
  idade <- as.numeric(format(Sys.Date(), "%Y")) - z
  fa <- paste0("Feliz aniversário! ", idade, " anos... está ficando velho, hein?")
  
  ani_agora <- strptime(ani3, format = "%d-%m")
  hoje2 <- strptime(hoje, format = "%d-%m")
  a <- ifelse(hoje2 < ani_agora, paste0(round(abs(hoje2 - ani_agora), 1), " dia(s) até seu próximo aniversário!"), 
              paste0(365 - (round(abs(hoje2 - ani_agora), 1)), " dia(s) até seu próximo aniversário!"))
  
  
  
  try(
  if(hoje == ani3){
    stop(print(fa))
    return(a)},
    silent = T)
  

}

