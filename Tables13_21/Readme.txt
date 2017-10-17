The files included in this folder are used to generate Tables 13-21. 
The makefile is designed to help in the execution process of these files. The file takes 2 hours to run with 24 cores. You can use an instance in Amazon Web Services. I also include instructions necessary to run the code in such a server. The following modifications are necessary in the makefile in order to run the code properly:

1. Amazon server name. e.g. ec2-54-210-213-35. Stored in environmental variable
SERV

2. Local directory where file is stored. Environmental variable DIR

3. Key(.pem) to access the server stored in environmental variable KEYDIR

4. Name of the key(.pem) used to access the server in environmental variable KEYNAME

5. Directory where the boost library is stored. Environmental variable INCDIR1. 

6. Directory where the nlopt library is stored. Environmental variable INCDIR2. 

7. Number of cores to be used in the parallelization. Environmental variable OMP_NUM_THREADS. 

After these modifications, the makefile is ready to run in order to get the corresponding values for the table. 

In order to replicate the point estimates of tables 25-36, it is necessary to run the following line in the command line prompt: 

``make parallelrun” 

The output of the file is called “PAR32.csv”. It contains the point estimates of the minimization of the -log likelihood function. The file BEHAVIORAL.csv is the dataset in cvs format. 

This is enough to replicate the point estimates obtained in the tables. 

In order to run the computation of the standard errors, it is necessary to run the R-code "BEHAVIORALSTANDARDERRORS.r"


Comments: razuero@iadb.org
