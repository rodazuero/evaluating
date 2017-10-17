
#=========================#
#Table of sample selection#
#=========================#
#For this I use the information from the do-file:
#/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/FiguresandTables/SAMPLESELECTION/SampleSelection.do
Filter=rbind("Initial sample",
             "Household not surveyed in 2012",
             "Household not surveyed in 2010",
             "Parent not living in household",
             "Siblings within five years of age",
             "Children's incomplete questionnaire",
             "Family's incomplete questionnaire")
Numbers=rbind("18,310","16,033","12,898","7,855","4,125","2,247","950")
TABLEL<-cbind(Filter,Numbers)
row.names(TABLEL)<-NULL
colnames(TABLEL)<-c("Filter","Number of households")



DT<-2

#Version with title
tabout<-xtable(TABLEL,caption="Description of sample used in the analysis",digits=DT,label="tab:TableFilters",table.placement="H")
align(tabout)<-"llc"
write(print(tabout,include.rownames=(FALSE),   table.placement="H",sanitize.text.function=function(x){x},size="\\footnotesize"),file="TableFilters.tex")

#Version without title
tabout<-xtable(TABLEL,digits=DT,label="tab:TableFiltersNOTITLE",table.placement="H")
align(tabout)<-"llc"
write(print(tabout,include.rownames=(FALSE),   table.placement="H",sanitize.text.function=function(x){x}),file="TableFiltersNOTITLE.tex")

