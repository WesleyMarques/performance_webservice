library("ggplot2")

getLogClient = read.table("./data/100reqs/GET_log_client.log");
names(getLogClient) <- c("idLog", "req", "time");
getLogClient$time <- as.numeric(gsub(",", ".", paste(getLogClient$time)))
getLogClient$time <- getLogClient$time*1000; 
getLogCpu = read.table("./data/100reqs/GET_mylogfile.cpu.log");
names(getLogCpu) <- c("Timestamp","log", "idleTime", "usedTime");
getLogCpu$log <- NULL
getLogCpu["idleTime_normal"] <- 0;
getLogCpu["usedTime_normal"] <- 0;
getLogMemory = read.table("./data/100reqs/GET_mylogfile.memory.log");
names(getLogMemory) <- c("Timestamp","log", "usedMem", "percentUsed");
getLogMemory$log <- NULL
getLogService = read.table("./data/100reqs/GET_mylogfile.service.log");
names(getLogService) <- c("Timestamp","log","idLog", "req", "method", "time", "idLog2");
getLogService$idLog <- NULL;
getLogService$log <- NULL;
getLogService$time <- as.integer(gsub("ms", "", paste(getLogService$time)))
plot(getLogService$time, type="l", col="red", ylim =c(0,400))
par(new=TRUE)
plot(getLogClient$time, type="l", col="blue", ylim =c(0,400))

summary(getLogClient$time)

cpu_real <- function(){
  
  for (i in 2:nrow(getLogCpu)) {
    getLogCpu$idleTime_normal[i] <- getLogCpu$idleTime[i]-getLogCpu$idleTime[1];
    getLogCpu$usedTime_normal[i] <- getLogCpu$usedTime[i]-getLogCpu$usedTime[1]; 
  }
  return(getLogCpu)
}
getLogCpu <- cpu_real()

plot(getLogCpu$idleTime_normal, type="l", col="red", ylim = c(0, 500400), ylab = "Soma acumulada do Uso/Ociosidade(ms)")
par(new=TRUE)
plot(getLogCpu$usedTime_normal, type="l", col="blue", ylim = c(0, 500400), ylab = "")

plot(getLogMemory$percentUsed, type="l", col="blue")
