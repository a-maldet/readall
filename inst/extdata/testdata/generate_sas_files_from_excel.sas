libname mylib "C:\Users\u0383597\Desktop\local_repo\r_pakete\tlsread\inst\extdata"; 
run;

PROC IMPORT OUT=mylib.sas1 DATAFILE= "C:/Users/u0383597/Desktop/local_repo/r_pakete/tlsread/inst/extdata/testdata/excel1.xlsx" 
            DBMS=xlsx REPLACE;
     GETNAMES=YES;
RUN;
proc print data=mylib.sas1;   
run; 

proc contents data=mylib.sas1;   
run; 



PROC IMPORT OUT=mylib.sas2 DATAFILE= "C:/Users/u0383597/Desktop/local_repo/r_pakete/tlsread/inst/extdata/testdata/excel2.xlsx" 
            DBMS=xlsx REPLACE;
     GETNAMES=YES;
RUN;
proc print data=mylib.sas2;   
run; 

proc contents data = mylib.sas2;
run;

data mylib.sas2;
  set test;
run;

data _null_;
   set test;
   file "C:/Users/u0383597/Desktop/local_repo/r_pakete/tlsread/inst/extdata/sas2.sas7bdat" encoding="utf-8";
   put a1 a2 a3 a4 a5;
run;



 proc print data = test.sas2; 
 RUN; 

DATA test2; 
  set 'C:\Users\u0383597\Desktop\local_repo\r_pakete\tlsread\inst\extdata\sas2.sas7bdat'; 
RUN; 
 proc print data = test2; 
 RUN; 
