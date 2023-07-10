
#This Function takes a List of Issue Combinations we want to find jacards distance between 
# and a data frame of the population and returns a data frame with 5 columns 
#Flag, Flag, Count, Issue, Issue. . 

Get_Counts<-function(issues_Internal, Population){
  N_COUNT<-data.frame()
  D_between<-data.frame()
  j=1
  l=1
  k=1
  for (i in 1:ncol(issues_Internal))
  {
      # Takes the names of the two issues we want to compare from our list 
      # of combinations and puts them in input names 
      input_names<-issues_Internal[1:2,i] 
      
      # count_ counts the number of occurrences of each combination of flags for those
      # issues and puts them in n 
      n<- Population %>% count_(input_names, .drop = FALSE)
      n<-data.frame(lapply(n, as.character), stringsAsFactors=FALSE)
      # Extracts the Names of the two issues without the MI_ in front of them and puts
      # puts them in TST which is a 1x3 data frame with two NAs in front of the names 
      Combo_Names<-CNAMES(input_names)
      print(Combo_Names)
      TST<-data.frame(col1 = NA, col2=NA, col3=Combo_Names[3])
      # Puts the 1X3 data frame of NAs and Name below the combination count of N 
      n[nrow(n)+1,1:3]<-TST
      
      
      # j is the row where we want to start placing the new data we just calculated
      # l is the row where we want to stop placing the data we just calculated
      j<-nrow(N_COUNT)+1
      k<-nrow(n)
      l=j+k-1
      
      ## Saves the counts as not characters 
      n3<-n[,3]
      
      
      # Turns n From a list of counted factors into a list of characters which is
      # needed to be able to combine the data this way 
      n<-data.frame(lapply(n, as.character), stringsAsFactors=FALSE)
      
      
      # re-inserts the counts after the as.character 
      n[,3]<-n3
      
      # takes N and puts it at the bottom of ANDS 
      N_COUNT[j:l,1:3]<-n
      D_between[i,1]<-j
      D_between[i,2]<-l-1
  }
  #print(typeof(D_between))
  #print(typeof(input_names))
  
  
  assign("input_names_GLO",input_names, envir = .GlobalEnv)
  
  
  N_COUNT<-N_COUNT %>% rename("Total"="n")
  
  Forjac_internal<-list(D_between, N_COUNT)
  
  
  
  return(Forjac_internal)
}



# This Function takes the names of the issues from the l2 data 
# which are stored as "MI_ISSUE" and returns a vector in the 
# format ("Issue", Issue", "Issue-Issue" )

CNAMES<-function(input_names) {
  # Extract the first value
  value1 <- substring(input_names[1], first = 4)
  
  # Extract the second value
  value2 <- substring(input_names[2], first = 4)
  
  # Combine the values with a dash
  combined_string <- paste(value1, value2, sep = "-")
  
  
  NAMESANDCOMBOS<-cbind(value1,value2,combined_string)
  # Return the combined string
  return(NAMESANDCOMBOS)
}


# This Function Calculates the Jaccards distance between 
# issues/Flags. IT returns Jaccards distance along with 
# the Raw counts for each flag combination. It calls How_Partisan
# which retruns the counts broken down by independent, republican, 
# and democrat. 

D_Jac<-function(D_between, N_COUNT, input_names2, Population){
  
# Gets the Length of N_count, Which stores the counts 
# for each combination. 
jacdistance<-vector(length = nrow(N_COUNT))
#print(nrow(N_COUNT))
M=1
for(i in 1:nrow(D_between))
{
  
  k<-D_between[i,1]
  q<-D_between[i,2]

  
  CHURCHMINI<-N_COUNT[k:q,1]
  RELIGIOUSMINI<-N_COUNT[k:q,2]
  N31<-as.integer(N_COUNT[k:q,3])
  for(j in 1:(q-k+1))
  {
    MATCH1<-CHURCHMINI==CHURCHMINI[j]
    MATCH2<-RELIGIOUSMINI==RELIGIOUSMINI[j]
    top<-N31[j]
    bottom<-sum(N31[MATCH1])+sum(N31[MATCH2])-top
    jacdistance[M]<-top/bottom
    M=M+1
  }
  jacdistance[M]=NA
  M=M+1
  
}

Part_DF<-How_Partisan(input_names2, Population)
Mer_1<-merge(N_COUNT,Part_DF, by=1:2, all=T)
Mer_2<-cbind(Mer_1,jacdistance)

return(Mer_2)

}

How_Partisan<-function(input_names3, Population){
  
  N_Ind<-Population %>% filter(MI_POLITICAL_PARTY=="Independent") %>% count_(input_names3) %>% rename("Independent"="n" )
  N_Rep<-Population %>% filter(MI_POLITICAL_PARTY=="Republican") %>% count_(input_names3) %>% rename("Republican" = "n")
  N_Dem<- Population %>% filter(MI_POLITICAL_PARTY=="Democratic") %>% count_(input_names3) %>% rename("Democratic" = "n")
  

  TST5<-merge(N_Rep,N_Dem, by=1:2, all=T) 
  TST6<-merge(TST5,N_Ind, by=1:2, all=T)
  return(TST6)
}



Pop_Cut<-function(Race, Age){
  
 if(Race != "NONE" && Age != "NONE")
 {
   population_internal <- Michigan %>% filter(MI_RACE=={{Race}} & MI_AGE_RANGE=={{Age}})
   
   population_internal<-sample_n(population_internal, nrow(population_internal))
 }
  else if (Race != "NONE")
  {
    population_internal <- Michigan %>% filter(MI_RACE=={{Race}})
    population_internal<-sample_n(population_internal, nrow(population_internal))
    
  }
  else 
  {
    population_internal <- Michigan %>% filter(MI_AGE_RANGE=={{Age}})
    population_internal<-sample_n(population_internal, nrow(population_internal))
    
  }
return(population_internal)
}


Con_Probs<-function(N_COUNT, population_2 ,issues_internal){
  #print(issues_internal[2,1])
  Prob_AB<-data.frame()
  Prob_BA<-data.frame()
   
   
  Prob_AB_Internal<-data.frame()
  A <- population_2 %>% count(.data[[(issues_internal[1,1])]])
  B <- population_2 %>% count(.data[[(issues_internal[2,1])]])
      #table
      #print(A)
      #print(B)
      #print(N_COUNT)
      
      Prob_AB<-population_2 %>% group_by(.data[[(issues_internal[1,1])]],.data[[(issues_internal[2,1])]]) %>%
        tally() %>% spread(.data[[(issues_internal[1,1])]], n) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% mutate_if(is.numeric, ~(./sum(.)))
      
      Prob_BA<-population_2 %>% group_by(.data[[(issues_internal[2,1])]],.data[[(issues_internal[1,1])]]) %>%
        tally() %>% spread(.data[[(issues_internal[2,1])]], n) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% mutate_if(is.numeric, ~(./sum(.)))
      
      
      
      
    #for (i in 1:nrow(A)){     
     #Prob_AB_Internal<- (N_COUNT %>% filter(.data[[(issues_internal[1,1])]]==A[i,1]) %>% select(Total))
     #Prob_AB_Internal<-as.numeric(Prob_AB_Internal[,1])/A[i,2]
      
     #Prob_AB_Internal[,i]/sum(Prob_AB_Internal[,i])
     #Prob_AB<-rbind(Prob_AB_Internal,Prob_AB)
    #}
    
     #print(Prob_BA)
     #print(Prob_AB)
     #colnames(Prob_AB)<-B[,1]
     #Prob_AB<-cbind(A[,1], Prob_AB)
     #Prob_AB %>% rename("FLAGS"="A[,1]") 
     
     
    return(Prob_BA)
          
}

display_venn<-function(issueAV, issueBV, Lev1, Lev2, population_internal){

  # Gets the Counts for each flag and puts them into tst11 and then gets the counts 
  # for people that have both flags. 
  tst11 <- population_internal %>% filter(.data[[issueAV]]=={{Lev1}}) %>% select(CCID)
  tst12 <- population_internal %>% filter(.data[[issueBV]]=={{Lev2}}) %>% select(CCID)
  tst13 <-  population_internal %>% filter(.data[[issueAV]]=={{Lev1}} & .data[[issueBV]]=={{Lev2}}) %>% select(CCID)
  
  # Generates a new page to put the venn object in. 
  grid.newpage()
  
  # generates the Venn diagram grid object. 
  draw.pairwise.venn(area1 = nrow(tst11), area2 = nrow(tst12), cross.area = nrow(tst13), category =(c(Lev1, Lev2)), fill = c("blue", "red"), alpha = .5)
  
}


# This function returns the flag that has the most voters in common with the 
# flag selected in the first slot. 

Most_Similar_Flag<-function(JAC_WITH_TAGS, issues_internal, Lev1)
{
 # Places the Names in the same format as those in the Jac_With_tags data frame  
  Jac_Match<-CNAMES(issues_internal)
  print(Jac_Match[1])
  TM<-Jac_Match[1]
  
  
  MATCHED <- filter(JAC_WITH_TAGS, V4 == {{TM}} | V5 == {{TM}}) %>% 
  filter(MI_CHURCH=={{Lev1}} | MI_RELIGIOUS=={{Lev1}}) %>% arrange(desc(as.numeric(n))) %>% 
  rename(FLAG_1=MI_CHURCH, FLAG_2=MI_RELIGIOUS, ISSUE_1=V4, ISSUE_2=V5)
  
  #print(head(MATCHED))
  
  return(head(MATCHED))
  
}



# Returns the Zip codes with the most number of votres with these flags and issues. 
#

Zip_Codes<-function(population_internal, issue1, issue2, Lev1, Lev2, P)
{
  
  if(issue1==issue2)
  {
    return(0)
  }
  print(issue1)
  print(issue2)
  print(Lev1)
  print(Lev1)
  
  Zip_Count<-population_internal %>%  filter(.data[[issue1]]=={{Lev1}} & .data[[issue2]]=={{Lev2}}) %>% count(ZIP) %>% rename( Selected_Voters=n)
    
  Zip_Total<- population_internal %>% count(ZIP) %>% rename(Total_Voters=n)
  print(Zip_Total)
  print(Zip_Count)
  
  
  Zip_End<-merge(Zip_Total, Zip_Count, by=1, all=T) %>% mutate(Percent=Selected_Voters/Total_Voters) #%>% select(Zip,Total,Percent)
  
  #print(Zip_Count)
  #count({{issue1}},{{issue2}})
  if(P==1)
  {
    return(Zip_Scatter(Zip_End))

  }
  
  return(Zip_End) 
  
}

Zip_Scatter<-function(ZIP_IN)
{
  yo<-ggplot(ZIP_IN)+
    geom_point(aes(x=Total_Voters, y=Percent))+
    theme_classic()
    
  return(yo)
}





shinyApp(ui, server)









