#File designed to send files to amazon server
#Define environmental variables



#1. Amazon server name. e.g. ec2-54-210-213-35
SERV=[INSERT/SERVERS/NAME/HERE]


#3. Local directory where file is stored
DIR=[INSERT/LOCAL/DIRECTORY/HERE]

#4. Key stored in:
KEYDIR=[INSERT/DIRECTORY/TO/KEY/HERE]

#5. Key name
KEYNAME=[INSERT/KEY/NAME.PEM/HERE]


#scp -i /Users/rodrigoazuero/Documents/Parallelcomputing/amazoncomputing/rodazuero-key-pair-nvirginia.pem /Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL65.zip ec2-user@ec2-52-87-167-229.compute-1.amazonaws.com:~
toserver:
	scp -i $(KEYDIR)$(KEYNAME) $(DIR).zip ec2-user@$(SERV).compute-1.amazonaws.com:~
	

accessserver:
	ssh -i $(KEYDIR)$(KEYNAME) ec2-user@$(SERV).compute-1.amazonaws.com

#---------------------
#Housekeeping        #
#---------------------

#1. C++ compiler
CC=g++

#2. Flags
CFLAGS=-fopenmp -Wall  

#3. Name of output
OUTPUT=main
OUTPUTNLOPT=mainnlopt
OUTPUTSMOOTH=smooth
OUTPUTPARALLEL=paralleloutput
#4. Files to run in main:
SOURCE=main.cpp
SOURCEHOPSPACK=main_hopspack.cpp
SOURCENLOPT=main_nlopt.cpp
SOURCESMOOTH=mainSmoothingDistribution.cpp
SOURCEPARALLEL=MainParallel.cpp

#5. Home directory. in TESLA is /home/razu/
HOME=/home/razu/

#6. Dynamic libraries 
DLIB=-lgsl -lgslcblas

#7. Set number of threads	
export OMP_NUM_THREADS=32

#-------------------------------------
#3. Include directories:
INCDIR1=/home/razu/boost_1_61_0/
INCDIR2=$(HOME)install/include/

#The previous version of include was with /usr/include/c++/3.4.6/backward/ 
#and additionally with /usr/lib64/R/library/BH/include/

#Generating the INC for all the paths
INC=$(INCDIR1) $(INCDIR2) $(INCDIR3) 
#-> This is for tesla
#INC=$(INCDIR3)
INC_PARAMS=$(foreach d, $(INC), -I$d)
#-------------------------------------

#3.1 Include directories in my pc
INCDIRMYPC1=PATH=/Users/rodrigoazuero/local/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2/include
INCMYPC=$(INCDIRMYPC1) 
INC_PARAMS_MYPC=$(foreach d, $(INCMYPC), -I$d)
#-------------------------------------

#4. Lnlopt library 
LNLOPT=-L$(HOME)install/lib/ -lnlopt




configureserver:
	sudo yum install htop
	sudo yum install boost
	sudo yum install boost-devel
	sudo yum install gsl 
	sudo yum install gsl-devel
	sudo yum install gcc-c++

myprog:
	$(CC) $(SOURCE) $(INC_PARAMS) $(LNLOPT)   $(CFLAGS) $(DLIB)  -o $(OUTPUT)

nlopt:
	$(CC) $(SOURCENLOPT) $(INC_PARAMS) $(LNLOPT)   $(CFLAGS) $(DLIB)  -o $(OUTPUTNLOPT) 

runnlopt: nlopt
	nohup ./$(OUTPUTNLOPT) > nohupNLOPT.out&

myprogmypc:
	$(CC) $(SOURCE) $(INC_PARAMS_MYPC) -lnlopt   $(CFLAGS) $(DLIB)  -o $(OUTPUT)

myproghopspack:
	$(CC) $(SOURCEHOPSPACK) $(INC_PARAMS) $(LNLOPT)   $(CFLAGS) $(DLIB)  -o $(OUTPUT)

nloptinst:
	wget http://ab-initio.mit.edu/nlopt/nlopt-2.4.2.tar.gz -P $(HOME)
	tar  -zxvf $(HOME)nlopt-2.4.2.tar.gz -C$(HOME)
	mkdir $(HOME)/install
	cd $(HOME)nlopt-2.4.2 && ./configure --prefix=$(HOME)install && make && make install

parallel: 
	$(CC) $(SOURCEPARALLEL) $(INC_PARAMS) $(LNLOPT)   $(CFLAGS) $(DLIB)  -o $(OUTPUTPARALLEL)

parallelrun: parallel
	nohup ./$(OUTPUTPARALLEL)  > nohupPARALLR.out&

smoothdist: 
	$(CC) $(SOURCESMOOTH) $(INC_PARAMS) $(LNLOPT)   $(CFLAGS) $(DLIB)  -o $(OUTPUTSMOOTH)

smoothdistrun: smoothdist
	nohup ./$(OUTPUTSMOOTH) > nohupSMOOTHDIST.out&

nloptuninst:
	rm $(HOME)nlopt-2.4.2.tar.gz
	rm -r $(HOME)nlopt-2.4.2





hopspackinstall:
	wget http://www.sandia.gov/hopspack/code_drops/hopspack-2.0.2-src.tar.gz
	tar -zxvf hopspack-2.0.2-src.tar.gz
	cd hopspack-2.0.2-src && mkdir build_mt
	cd hopspack-2.0.2-src && cmake ../hopspack-2.0.2-src -Dmt=yes
	cd hopspack-2.0.2-src && make

hopspackrun: myproghopspack
	nohup ../hopspack-2.0.2-src/HOPSPACK_main_threaded hopspackCHILE.txt> nohuphopspack.out&

clean: 
	rm $(OUTPUT)


#In my pc: omp.h is in /Users/rodrigoazuero/local/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2/include
#Include it to path PATH=$PATH:/Users/rodrigoazuero/local/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2/include
#Also include the executable so that hopspack finds it. PATH=$PATH:/home/razu/BEHAVIORAL55
#Myproghopspack: g++ main.cpp  -I/usr/include/c++/3.4.6/backward/  -I/usr/lib64/R/library/BH/include/  -I/home/razu/install/include/ -L/home/razu/install/lib/ -lnlopt   -fopenmp -Wall   -lgsl -lgslcblas  -o main
#g++ main_hopspack.cpp  -I/usr/include/c++/3.4.6/backward/  -I/usr/lib64/R/library/BH/include/  -I/home/razu/install/include/ -L/home/razu/install/lib/ -lnlopt   -fopenmp -Wall   -lgsl -lgslcblas  -o main

#Locating C++  libraries in TESLA

#locate iostream.h->usr/include/c++/3.4.6/backward
#locate locate discrete_distribution.hpp->/usr/lib64/R/library/BH/include/
# //This is where i find the iostream.h

#After running nloptinst the nlopt library will be installed in /home/razu/install/lib

#In order to run it in the background: execute nohup ./main.out & 

# g++  main.cpp  -I/home/razu/install/include/ -L/home/razu/install/lib/ -lnlopt -Wall -lgsl -lgslcblas   -o main
# g++  main.cpp  -I/home/razu/install/include/ -I/usr/include/c++/3.4.6/backward/  -I/usr/lib64/R/library/BH/include/ -L/home/razu/install/lib/ -lnlopt -Wall -lgsl -lgslcblas   -o main
#g++   -I/usr/include/c++/3.4.6/backward/  -I/usr/lib64/R/library/BH/include/  -I/home/razu/install/include/ -L/home/razu/install/lib/ -lnlopt -Wall -lgsl -lgslcblas  main.cpp -omain


