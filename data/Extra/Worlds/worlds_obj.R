# 
# repeat {
#   input <- readline(prompt = "Do you want data for Individual or Squad? (ind/squad): ")
#   input <- tolower(input)  # Convert input to lowercase
#   
#   if (input %in% c("ind", "squad")) {
#     type = input
#     break  
#   } else {
#     cat("Invalid input. Please enter 'ind' or 'squad'.\n")
#   }
# }


WorldsOut <- worlds_obj(rsBerlin24, t = t)
dataWI1  <- WorldsOut$data[[1]]
dataWI2  <- WorldsOut$data[[2]]
worlds_rat <- WorldsOut$rat