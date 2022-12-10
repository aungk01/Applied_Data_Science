library("dplyr")  
library(corrplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)

df <- read.csv('dementia_dataset.csv')
model <- readRDS("glm_1.rda") 

plot_graph <- function(df , M.F = input$select_column,Group = input$select_column){
    df <- subset(df, Group %in% c("Demented", "Nondemented"))
    
    
    
    options(scipen = 100000)
    
  if(!("All" %in% M.F)){
    if("M" %in% M.F){
      df <- df %>% filter( M.F == "M")
    }else{df<- df %>%filter(M.F == "F")
      }
   }
  if(!("All" %in% Group)){
      if("Demented" %in% Group){
        df <- df %>% filter( Group == "Demented")
      }else{df<- df %>%filter(Group == "Nondemented")
      }
    }
    
  ggplot(data = df , aes(x = M.F, y = Age, fill = Group))+ 
    geom_violin(trim= FALSE, adjust = 1 )+
    labs(title="Dementia vs Gender",x="Gender", y = "Age") + theme_minimal()
  
 
  
  
  }
  
  
  plot_cor <- function(df, co = input$Cor){
    colnames(df)[1] <- "n"
    
    df_1 = subset(df, select = -c(n, MRI.ID, Hand, M.F,Group))
    
    corrplot(cor(df_1), method ="pie") 
    
    
  }
  plot_variate <- function(df, co = input$sympton, Group = input$Group_1 ){
    df <- subset(df, Group %in% c("Demented", "Nondemented"))
    options(scipen = 100000)
    data_summary <- function(x) {
      mu <- mean(x)
      sigma1 <- mu-sd(x)
      sigma2 <- mu+sd(x)
      return(c(y=mu,ymin=sigma1,ymax=sigma2))
    }
    
    
    if( co %in% df ){
      if(co == df$CDF){
      df <- df %>% filter(CDF)
      
      }
      
      else if( co %in% df && co == df$ASF){
      df <- subset(df, select(ASF))
      }
      else if(co %in% df && co == df$EDUC){
      df <- subset(df,select(EDUC))
      }else{ return()}
  
    }
   if(!("All" %in% Group )){
     if("Demented" %in% Group){
       df <- df %>% filter( Group == "Demented")
     }else{df<- df %>%filter(Group == "Nondemented")
     }
   }
    
if( co == 'CDR'){ 
  
  ggplot(data = df , aes(x = CDR , y = Age, fill = Group))+ 
    geom_dotplot(binaxis='y', stackdir='center',
                 stackratio=0.90, dotsize= 0.60) + 
    labs(title=" Demented and Nondemented population Between CDR vs Age ",x="CDR", y = "Age")+
    scale_fill_brewer(palette="Dark2") + theme_minimal()
                }
     
else if ( co == 'ASF'){
  ggplot(data = df , aes(x = ASF , y = Age, fill = Group))+ 
    geom_dotplot(binaxis='y', stackdir='center',
                 stackratio=0.90, dotsize= 0.60) + 
    labs(title=" Demented and Nondemented population Between ASF vs Age ",x="ASF", y = "Age")+
    scale_fill_brewer(palette="Dark2") + theme_minimal()
  }

else{
  
    ggplot(data = df , aes(x = EDUC , y = Age, fill = Group))+ 
    geom_dotplot(binaxis='y', stackdir='center',
                 stackratio=0.90, dotsize= 0.60) + 
    labs(title=" Demented and Nondemented population in Education vs Age ",x="Education", y = "Age")+
    scale_fill_brewer(palette="Dark2") + theme_minimal()
  
  
     
}
    
    
  
    
  }
  
 
   plot_table <-function( Age = input$integer,EDUC = input$edu,CDR = input$cdr, ASF= input$asf){
     #source('each_call.R', local = FALSE)
     
     min_max_norm <- function(x) {
       (x - min(x)) / (max(x) - min(x))
     }
     
     foos<- data.frame(matrix(ncol=5, nrow=1))
     colnames(foos)<-c('Age','EDUC','CDR','ASF')
     if( !is.na(Age) && !is.na(EDUC) && !is.na(CDR) && !is.na(ASF)){
       foos$Age<-Age
       foos$EDUC<-EDUC
       foos$CDR<-CDR
       foos$ASF<-ASF
      }
     else{return()}
     #nor<- data.frame(matrix(ncol=5, nrow=1))
     #$colnames(nor)<-c('Age','EDUC','CDR','ASF')
     #foo$Gender<-Gender
     #rn(foo)
     #(foo$Age* 0.9650) +(foo$EDUC*0.9205)+((foo$CDR/100)*-17.3772 )+ 8.2301
     #foo$Gender<- factor(foo$Gender)
     #my_predict <- reactiveVal('')
     #foo<- data.frame(matrix(ncol=5, nrow=1))
     #colnames(foo)<-c('AGE','EDUC','CDR','ASF')
     
     result <- {
                    
                    
                     pr<-predict(model, foos, type="response")
                     #pt <- predict(model, foo)
                     #p <- 100-pr
                     #p_t <- format(round(pt, 2), nsmall = 2)
                     pp <- format(round(pr, 2), nsmall = 2)
     
                     
                    }
     result_1<- {
                   pt <- predict(model, foos)
                   p_1 <- format(round(pt, 2), nsmall = 2)
                } 
     
     
     str1 <- paste("Predicted value: ",result)
     HTML(paste(str1,sep = '<br/>'))
     str2 <- paste("Effect of independent variable: ",result_1)
     HTML(paste(str1,str2,sep = '<br/>'))
     
    
    
  }
  