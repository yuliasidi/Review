/* Based on article #62*/
data YesNo;
input trt $ NumYes Total;
Response="Yes"; Count=NumYes;       output;
Response="No "; Count=Total-NumYes; output;
datalines;
trt1    74 396
trt2 	62 392
;
run;

proc freq order=data;
weight Count;
table trt * Response / riskdiff(cl=exact);
exact riskdiff;
run;
/* Exact 95% CI : -0.0248-0.0821*/

/* Wald/FM/NEWCOMBE */
proc freq order=data;
weight Count;
table trt * Response /  riskdiff(method=WALD noninf margin=0.066) alpha=0.025;
table trt * Response /  riskdiff(method=FM noninf margin=0.066) alpha=0.025;
table trt * Response /  riskdiff(method=NEWCOMBE noninf margin=0.066) alpha=0.025;
run;


data YesNo2;
input trt $ NumYes Total;
Response="Yes"; Count=NumYes;       output;
Response="No "; Count=Total-NumYes; output;
datalines;
trt2    208 267
trt1 	188 267
;
run;

proc freq data = YesNo2 order=data;
weight Count;
*table trt * Response /  riskdiff(method=WALD noninf margin=0.1) alpha=0.025;
table trt * Response /  riskdiff(method=FM noninf margin=0.1 norisks) alpha=0.025;
*table trt * Response /  riskdiff(method=NEWCOMBE noninf margin=0.1) alpha=0.025;
run;

proc power;
twosamplefreq test=fm
groupproportions   = (0.9 0.9)
nullproportiondiff = -0.05
alpha              = 0.025
sides              = U
power              = 0.9
ntotal             = .;
run;
