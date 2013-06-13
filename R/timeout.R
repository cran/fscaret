timeout <- function(expr, seconds, my.pid){
	# Function written after nabble user        
        
        killer.pid <- system(intern = TRUE, paste(" (sleep", seconds,
                " ; kill -9", my.pid,
                ")>/dev/null&\n echo $!", sep=" "))
               
        on.exit(system(paste("kill", killer.pid, "> /dev/null 2>&1", sep=" ")))
        withCallingHandlers(expr, interrupt=function(...){
        stop("Timedout", call.=FALSE)
	
        })
}