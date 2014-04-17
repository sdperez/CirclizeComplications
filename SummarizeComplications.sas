libname r 'C:\Users\sdperez.EMORYUNIVAD\Desktop\My Documents\NSQIP_PUF';
*Import 2012 Puf file;
proc contents data=r.acs_nsqip_puf12 varnum;
run;
proc freq data=r.acs_nsqip_puf12;
table SURGSPEC;
run;
Data GenSurg; set r.acs_nsqip_puf12;
if SURGSPEC='General Surgery';
run;

*Create a frequency table of complications;
proc freq data=GenSurg;
table SUPINFEC*WNDINFD*ORGSPCSSI*DEHIS*OUPNEUMO*REINTUB*PULEMBOL*FAILWEAN*RENAINSF*OPRENAFL*URNINFEC*
CNSCVA*CNSCOMA*NEURODEF*CDARREST*CDMI*OTHBLEED*OTHGRAFL*OTHDVT*OTHSYSEP*OTHSESHOCK/list out=complications;
run;
/*
proc freq data=GenSurg;
table SUPINFEC*WNDINFD*ORGSPCSSI*DEHIS*OUPNEUMO*REINTUB*PULEMBOL*FAILWEAN*RENAINSF*OPRENAFL*URNINFEC*
CNSCVA*CNSCOMA*NEURODEF*CDARREST*CDMI*OTHBLEED*OTHGRAFL*OTHDVT*OTHSYSEP*OTHSESHOCK*READMISSION/list out=complications;
run;*/

*Export Data;
PROC EXPORT DATA= WORK.Complications 
            OUTFILE= "C:\Users\sdperez.EMORYUNIVAD\Documents\GitHub\CirclizeComplications\Complications.xls" 
            DBMS=EXCEL REPLACE;
RUN;
