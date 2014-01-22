##Runs Frequencies on a set of variables
frequencies<-function(data,r.digits=1){
	frequencies.var<-function(variable){
		freq<-table(variable)
		freq<-as.data.frame(freq)
		freq$Percentage<-round(freq$Freq/sum(freq$Freq)*100,r.digits)
		freq$Cum.Percentage<-round(cumsum(freq$Freq)/sum(freq$Freq)*100,r.digits)
		
		num.miss<-matrix(0,nrow=1,ncol=3)
		colnames(num.miss)<-c("Valid","Missing","Total")
		row.names(num.miss)<-"# of cases"
		num.miss[1,]<-c(sum(freq$Freq),sum(is.na(variable)),length(variable))
		
		colnames(freq)<-c("Value","# of Cases","      %","Cumulative %")
		result<-list(Frequencies=freq,case.summary=num.miss)
		class(result)<-"freq.table"
		return(result)
	}
	if(!is.data.frame(data)){
		data<-as.data.frame(data)
	}
	
	results<-list()
	
	for(index in 1:dim(data)[2]){
		results[[names(data)[index]]]<-frequencies.var(data[,index])        
	}
	return(results)
}

##Prints frequency tables
print.freq.table<-function(x,...){
	cat("------------------------------------------------------------\n")
	cat("--                        Frequencies                     --\n")
	cat("--                                                        --\n")
	
	print(x[[1]])
	cat("--                                                        --\n")
	cat("--                        Case Summary                    --\n")
	cat("--                                                        --\n")
	print(x[[2]])
	cat("--                                                        --\n")
	cat("--                                                        --\n")
	cat("------------------------------------------------------------\n\n\n\n\n\n")
} 