library(vroom)
library(data.table)
library(ggplot2)
library(lubridate)
library(lme4)
library(data.table)
library(lubridate)
library(mclust)
library(scales)
library(scales)
library(stringr)
library(gt)
library(rprojroot)

# data 
# https://data.cityofnewyork.us/City-Government/Citywide-Payroll-Data-Fiscal-Year-/k397-673e/about_data
# civil service titls 
# jobs api 

deps <- c("johanna castro-campusan", "rachel cordero",         
          "wesley jones",           
          "alaa moussawi",          
          "bradley reid",
          "smita deshmukh",
          "jeffrey baker", 
          "andrea vazquez"
)

ass_deps <- c(
  "mandy yu", 
  "jaunita john"          
  ,"rose martinez"        
  ,"johnathan chei"      
  ,"audrey son"          
  ,"malcom butehorn"      
  ,"christopher murray"  
  ,"nell beekman"             
  ,"david seitzer"       
  ,"robert calandra"          
  ,"sara liss"            
  ,"elliott lynn"        
  
)

data_staff <- c("BROOKE FRYE",
                "UZAIR QADIR", 
                "BENJAMIN WITTE", 
                "RACHAEL ALEXANDROFF",
                "NICHOLAS SOLOMON", 
                "CHRISTOPHER ZAWORA", 
                "ERIC KOEPCKE", 
                "NICHOLAS MONTALBANO", 
                "MELISSA NUNEZ", 
                "JULIA FREDENBURG",
                "RACHEL AVRAM", 
                "REESE HIROTA", 
                "ANNE DRISCOLL",
                "JAMES WU", 
                "ERIK BROWN", 
                "DANYLO ORLOV", 
                "TAYLOR FRANCISCO")
web_staff <- c("ENAN RAHMAN", 
               "JOYCE LI", 
               "YULIN SHEN", 
               "JOHNATHAN CHEI", 
               "NICOLAS TRIANTAPHILIDES",
               "AHMAD ZAKI", 
               "PAVEL MACHUCA ZAVARZI", 
               "MANDY YU", 
               "CHRISTOPHER STEPHENSON", 
               "SHERIFF AHMED", 
               "RYAN KERINS",
               "OMAR KHALIL", 
               "ANTHONY TAYLOR")
policy <- c("ADMINISTRATIVE STAFF ANALYST",
            "ASSOCIATE STAFF ANALYST", 
            "LEGISLATIVE POLICY ANALYST",
            "SENIOR LEGISLATIVE POLICY ANALYST",
            "STAFF ANALYST")
counsel <- c(
  "*AGENCY ATTORNEY",              
  "AGENCY ATTORNEY",                
  "AGENCY ATTORNEY DC37",   
  "LEGISLATIVE COUNSEL",
  "*ATTORNEY AT LAW", 
  "ATTORNEY AT LAW")      
