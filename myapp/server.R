library(shiny)
library(birk)
library(stringr)

convert <- function(input = '100.1001 x 100 x 122 cm') {
  #Converts ','  to '.'
  input <- gsub(",", ".", input)
  
  #Identify Numbers
  numbers <- na.omit(as.numeric(unlist(strsplit(unlist(input), "[^0-9.^0-9]+"))))
  
  #Identify Unit to convert from
  input_vector <- unlist(str_split(input, " x "))
  unit.from <- unlist(strsplit(input_vector[length(input_vector)], "[[:digit:]]+"))
  unit.from <- str_trim(unit.from[length(unit.from)])
  
  #Identify Unit to convert to
  units <- data.frame(from = c('cm', 'mm', 'm', 'ml', 'in', 'in', 'ft', 'floz'), 
                      to = c('inch', 'inch', 'ft', 'imp_oz','cm', 'mm', 'm', 'ml'), 
                      to2 = c('in', 'in', 'ft', 'floz','cm', 'mm', 'm', 'ml'), stringsAsFactors = F)
  unit.to <- as.character(units[which(units[,1] == unit.from),2])
  
  #Do the conversion
  numbers_conv <- unlist(lapply (numbers, function (x) { round(conv_unit(x, unit.from, unit.to), 1)}))
  
  return(paste(paste(numbers_conv, sep = '', collapse=" x "), as.character(units[which(units[,1] == unit.from),3])))
}



shinyServer(function(input, output, session) {
  
  # You can access the value of the widget with input$text, e.g.
  output$meh <- renderText(noquote(convert({input$myTextInput})), quoted = FALSE)
  
})
