# set parameters based on click


function(input, output, session) {
  
  source("server_reactive_inputs.R", local=TRUE) 
  
  source("server_map_makers.R", local=TRUE) 
  
  source("server_outputs.R", local=TRUE) 
}
