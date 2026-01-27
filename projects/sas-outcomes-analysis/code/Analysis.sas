/* Library Set up */

libname project "/home/u64135464/PROJECT";

PROC DATASETS LIBRARY=project DETAILS ;
  CONTENTS data=_ALL_;
RUN;


DATA project.info;
set project.surgeries;
format event_date mmddyy10.;
run;

proc sort data=project.info;
by event_date;
run;

PROC MEANS DATA=project.info N NMISS;
VAR AGE;
Run;

/* FORMATS */
PROC FORMAT;
    VALUE racef
        1, 2 = "Native American"
        3     = "African American non-Hispanic"
        4     = "Hispanic"
        8     = "Asian"
        9     = "White non-Hispanic";
RUN;


/* MISSING AGE VALUES */
PROC SORT DATA=project.surgeries;
  BY PT_ID;
RUN;

DATA project.filled (RENAME=(NEWAGE=AGE));
  SET  project.surgeries;
  BY PT_ID;
  RETAIN INI_DATE INI_AGE;
IF FIRST.PT_ID=1 THEN INI_DATE=EVENT_DATE;
IF FIRST.PT_ID=1 THEN INI_AGE=AGE;
DIFF=ROUND((EVENT_DATE-INI_DATE)/365.25, 0.01);
NEWAGE=INI_AGE+DIFF;
DROP AGE DIFF INI_AGE INI_DATE;
RUN;

PROC MEANS DATA=project.filled N NMISS;
VAR AGE;
Run;

/* Merging data set */
PROC SORT DATA=project.filled;
  BY PT_ID;
RUN;

PROC SORT DATA=project.smoking;
  BY PT_ID;
RUN;

DATA project.complete;
MERGE project.filled project.smoking;
BY PT_ID;
RUN;

/* Patient Demographics */

DATA first_only;
    SET project.complete;
    BY PT_ID;
    IF first.PT_ID;
    Format RACE racef.;
RUN;

PROC PRINT DATA=first_only;
RUN;

PROC FREQ DATA=first_only;
    TABLES RACE * (SEX HOSMKG);
RUN;

/* Age Catagories */


PROC FORMAT;
	VALUE agecatf
	1 = "Under 1"
	2 = "1-Under5"
	3 = "5-Under18"
	4 = "18-Under30"
	5 = "30-Under50"
	6 = "50-Under65"
	7 = "65-Under75"
	8 = "75 and older";
RUN;
		
DATA first_only;
SET first_only;
IF AGE <1 THEN AGECAT = 1;
IF 1 <= AGE <5 THEN AGECAT = 2;
IF 5 <= AGE <18 THEN AGECAT = 3;
IF 18 <= AGE <30 THEN AGECAT = 4;
IF 30 <= AGE <50 THEN AGECAT = 5;
IF 50 <= AGE <65 THEN AGECAT = 6;
IF 65 <= AGE <75 THEN AGECAT = 7;
IF AGE => 75 THEN AGECAT = 8;
format AGECAT agecatf.;
RUN;

ODS RTF FILE="~/age.rtf";
PROC TABULATE DATA=first_only;
CLASS RACE AGECAT;
    TABLE RACE ALL, AGECAT*N / misstext="0" ;
RUN;
ODS RTF CLOSE ;

/* DEMOGRAPHICS (AGE and SMOKING STATUS) */

PROC TABULATE DATA=first_only;
CLASS HOSMKG SEX AGECAT;
    TABLE AGECAT, ALL*(PCTN) SEX*(ROWPCTN) HOSMKG*(ROWPCTN)  / misstext="0";
RUN;

/* DETERMINING NUMBER OF SURGERIES PER DAY */

PROC SORT DATA=project.complete;
BY PT_ID EVENT_DATE;
RUN;

DATA project.count;
SET project.complete;
BY PT_ID EVENT_DATE;
IF First.EVENT_DATE THEN COUNT=0;
COUNT + 1;
RUN;

ODS RTF FILE="~/count.rtf";
PROC FREQ DATA=project.count;
TABLES COUNT / NOPERCENT NOCUM;
RUN;
ODS RTF CLOSE ;

/* TABULATING COMBINATIONS OF SURGERIES */

PROC SORT DATA=project.count;
BY PT_ID EVENT_DATE COUNT;
RUN;

DATA project.doubles;
SET project.count;
BY PT_ID EVENT_DATE COUNT;
LENGTH CONDX2 $10;
LENGTH FDX $10;
RETAIN FDX;
IF first.EVENT_DATE then FDX = "";
IF COUNT = 1 THEN FDX=CONDX;
IF COUNT = 2 THEN CONDX2 = FDX;
DROP FDX;
RUN;

PROC PRINT DATA=project.doubles (obs=100);
RUN;

PROC FREQ DATA=project.doubles;
TABLES CONDX2*CONDX / ALL;
RUN;

/* MEAN AGE AT DEATH */

DATA project.deaths;
    SET project.complete;
    BY PT_ID;
    IF OUTCOME = 1;
    IF first.PT_ID;
    Format RACE racef.;
RUN;

ODS RTF FILE="~/deaths.rtf";
PROC MEANS MEAN MEDIAN MIN MAX DATA=project.deaths;
CLASS RACE HOSMKG;
VAR AGE;
RUN;
ODS RTF CLOSE ;

/*BOXPLOT COMPARING SMOKING AND NON SMOKING */
TITLE "Box plot for the groups of Non-smoking (0) and Smoking (1)";
PROC SGPLOT DATA=project.deaths;
VBOX AGE / CATEGORY=HOSMKG;
RUN;

/*T-TESTS */
TITLE "Two sample t-test";
PROC TTEST DATA=project.deaths;
CLASS HOSMKG;
VAR AGE;
RUN;