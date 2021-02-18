####################################################################################################
# This is script is for sensitivity analysis using inputs and output    			####
# activity levels obtained from cell collective 						####
# Author: Bhanwar Puniya, Ph.D. (bpuniya2@unl.edu)						####

#################################################################################################### 					
# download data of dynamic analysis performed in cell collective and place in a directory 	####
# extract all the data 										####
# Run this script in the directory where cell collcetive data is placed 			####
####################################################################################################

# install package 
install.packages (sensitivity)

#load package
library(sensitivity)


for (i in 1:length(dir())) 
{

	file_name = paste(dir()[i], ".txt", sep="")
	outfilepath = paste(getwd(),"/",dir()[i],"/",file_name, sep="")
	filepath = paste(getwd(),"/",dir()[i], sep="")

	inputfilepath = paste(filepath,"/","(2700-3000)inputs.csv", sep="") 
	# change the file names accordingly. 
	outputfilepath = paste(filepath,"/","(2700-3000)outputs.csv", sep="") 
	# change the file names accordingly. 

	input= read.csv(inputfilepath,header=T)
	output= read.csv(outputfilepath,header=T)

	# remove the components which has zero values in all simulations in input and output files
	Avg_col= sapply(input, mean)
	x1=which(Avg_col ==0)
	new_input=input[-x1]

		if(ncol(new_input)==0){
			new_input = input
		} else 
		{new_input = new_input}

	Avg_col2= sapply(output, mean)
	x2=which(Avg_col2 ==0)
	new_output=output[-x2]
	#run sensitiity analysis
	x_comb=NULL;
		for (j in 1:ncol(new_output))

			{
			xpcc=pcc(X = new_input, y = new_output[,j],nboot=100)
			xpcc = xpcc$PCC$original
			x_comb = cbind(x_comb,xpcc)
			###uncomment below if you want to use SRC method ###
			#xsrc = src(X = new_input, y = new_output[,j],nboot=100) 
			#xsrc = xsrc$SRC$original
			#x_comb = cbind(x_comb,xsrc)
			}


	rownames(x_comb) = names(new_input)
	colnames(x_comb) = names(new_output)
	# write outputs in table 
	write.table(x_comb,outfilepath, sep ="\t")
}





