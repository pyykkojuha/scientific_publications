* Association between prenatal provision of LNS and cesarean delivery
* Analysis code
* Software: Stata/SE 15.1

******************* LOAD DATA **************************************************

clear
set more off
version 15.1

import delimited "Association_between_prenatal_provision_of_LNS_and_cesarean_delivery.csv", delimiter(comma) varnames(1) 

******** LABELS ****************************************************************

* group:
* 1 = IFA
* 2 = MMN
* 3 = LNS

* sex:
* 0 = girl
* 1 = boy

* binary coding:
* 0 = no
* 1 = yes

*
**
***
********************************************************************************
*** Supplemental table 1 - Comparison of dropout + groups by included **********
********************************************************************************

* By lost to follow-up (included vs excluded) * * *

tab lost_to_followup 

tabstat moth_age moth_weight moth_height moth_bmi enr_ga moth_edu, by(lost_to_followup) stat(mean sd)

anova moth_age    lost_to_followup 
anova moth_weight lost_to_followup 
anova moth_height lost_to_followup 
anova moth_bmi    lost_to_followup 
anova enr_ga      lost_to_followup 
anova moth_edu    lost_to_followup 

tab primiparous   lost_to_followup , exact col
tab moth_aenemia  lost_to_followup , exact col
tab moth_hiv      lost_to_followup , exact col
tab moth_malaria  lost_to_followup , exact col

* By group (included) * * *

tab group if lost_to_followup==0

tabstat moth_age moth_weight moth_height moth_bmi enr_ga moth_edu if lost_to_followup==0, by(group) stat(mean sd)

anova moth_age    group if lost_to_followup==0
anova moth_weight group if lost_to_followup==0
anova moth_height group if lost_to_followup==0
anova moth_bmi    group if lost_to_followup==0
anova enr_ga      group if lost_to_followup==0
anova moth_edu    group if lost_to_followup==0

tab primiparous   group if lost_to_followup==0, exact col
tab moth_aenemia  group if lost_to_followup==0, exact col
tab moth_hiv      group if lost_to_followup==0, exact col
tab moth_malaria  group if lost_to_followup==0, exact col

* By group (excluded) * * *

tab group if lost_to_followup==1

tabstat moth_age moth_weight moth_height moth_bmi enr_ga moth_edu if lost_to_followup==1, by(group) stat(mean sd)

anova moth_age    group if lost_to_followup==1
anova moth_weight group if lost_to_followup==1
anova moth_height group if lost_to_followup==1
anova moth_bmi    group if lost_to_followup==1
anova enr_ga      group if lost_to_followup==1
anova moth_edu    group if lost_to_followup==1

tab primiparous   group if lost_to_followup==1, exact col
tab moth_aenemia  group if lost_to_followup==1, exact col
tab moth_hiv      group if lost_to_followup==1, exact col
tab moth_malaria  group if lost_to_followup==1, exact col

*
**
***
********************************************************************************
* FIGURE 1. FLOW CHARTS ********************************************************
********************************************************************************

tab abortion         group
tab twin             group
tab c_section        group if lost_to_followup == 0      /* delivery information */
tab measurement_info group if lost_to_followup == 0      /* measured at one month visit */

*
**
***
********************************************************************************
* TEXT: lost to follow-up ******************************************************
********************************************************************************

tab measurement_info group, exact col

*
**
***
********************************************************************************
*** Table 1. Baseline characteristics of the participating women at enrolment, by study group
********************************************************************************

tab group
tabstat moth_age moth_weight moth_height moth_bmi enr_ga moth_edu, by(group) stat(mean sd)

anova moth_age    group 
anova moth_weight group 
anova moth_height group 
anova moth_bmi    group 
anova enr_ga      group 
anova moth_edu    group 

tab primiparous   group , exact col
tab moth_aenemia  group , exact col
tab moth_hiv      group , exact col
tab moth_malaria  group , exact col

*
**
***
****
*****
******
*******
********************************************************************************
*** ¡DROP DROPOUTS FROM ANALYSIS!                        ***********************
*** dropout = no delivery information / twins / abortion ***********************
********************************************************************************

drop if lost_to_followup == 1

********************************************************************************
********************************************************************************
********************************************************************************
*******
******
*****
****
***
**
*

*
**
***
********************************************************************************
* TEXT: Results ****************************************************************
********************************************************************************

tab c_section              
tab c_planned           
tab c_emer                  
tab delivery_complication 

tab delivery_place

* Sick visits ************************************************************
tab nsv     group , col exact 
glm nsv   i.group , fam(bin) link(log) nolog eform                    // IFA-MMN & IFA-LNS
glm nsv ib2.group , fam(bin) link(log) nolog eform                    // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

* Hospital delivery ************************************************************
tab delivery_hospital     group , col exact
glm delivery_hospital   i.group , fam(bin) link(log) nolog eform      // IFA-MMN & IFA-LNS
glm delivery_hospital ib2.group , fam(bin) link(log) nolog eform      // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

* HC delivery ************************************************************
tab delivery_hc     group , col exact
glm delivery_hc   i.group , fam(bin) link(log) nolog eform            // IFA-MMN & IFA-LNS
glm delivery_hc ib2.group , fam(bin) link(log) nolog eform            // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

*
**
***
********************************************************************************
*** Table 2. Comparison of delivery complications by study group ***************
********************************************************************************
   
tab c_section               group, col exact
tab c_emer                  group, col exact
tab c_planned               group, col exact
tab delivery_complication   group, col exact

* Caesarean section ************************************************************
tab c_section     group , col exact chi2
glm c_section   i.group , fam(bin) link(log) nolog eform              // IFA-MMN & IFA-LNS
glm c_section ib2.group , fam(bin) link(log) nolog eform              // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

* Emercency caesarean section **************************************************
tab c_emer     group , col exact chi2
glm c_emer   i.group , fam(bin) link(log) nolog eform                 // IFA-MMN & IFA-LNS
glm c_emer ib2.group , fam(bin) link(log) nolog eform                 // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

* Planned caesarean section ****************************************************
tab c_planned     group , col exact chi2
glm c_planned   i.group , fam(bin) link(log) nolog eform              // IFA-MMN & IFA-LNS
glm c_planned ib2.group , fam(bin) link(log) nolog eform              // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

* Delivery complication *********************************************************
tab delivery_complication     group , col exact chi2
glm delivery_complication   i.group , fam(bin) link(log) nolog eform  // IFA-MMN & IFA-LNS
glm delivery_complication ib2.group , fam(bin) link(log) nolog eform  // MMN-LNS 
pwcompare group, effects
test i2.group = i3.group = 0

*
**
***
********************************************************************************
*** Table 3. Risk ratio to caesarean section ***********************************
********************************************************************************
	
* Model 1
glm c_section i.group               , fam(bin) link(log) nolog eform 
glm c_section sex                   , fam(bin) link(log) nolog eform 
glm c_section primiparous           , fam(bin) link(log) nolog eform  
glm c_section del_ga                , fam(bin) link(log) nolog eform 
 
glm c_section laz                   , fam(bin) link(log) nolog eform  
glm c_section waz                   , fam(bin) link(log) nolog eform  
glm c_section wlz                   , fam(bin) link(log) nolog eform  
glm c_section hcz                   , fam(bin) link(log) nolog eform  

glm c_section moth_height           , fam(bin) link(log) nolog eform  
glm c_section moth_bmi              , fam(bin) link(log) nolog eform  
glm c_section moth_age              , fam(bin) link(log) nolog eform  
glm c_section weekly_weight_gain    , fam(bin) link(log) nolog eform
 
glm c_section moth_hiv              , fam(bin) link(log) nolog eform  
glm c_section moth_malaria          , fam(bin) link(log) nolog eform  
glm c_section moth_agp_above1       , fam(bin) link(log) nolog eform  
glm c_section moth_crp_above5       , fam(bin) link(log) nolog eform  

glm c_section moth_edu              , fam(bin) link(log) nolog eform  
glm c_section hh_asset_below_median , fam(bin) link(log) nolog eform  

* Model 2
glm c_section i.group sex primiparous del_ga  /// 
               moth_height  moth_crp_above5 moth_edu  ///
			   , fam(bin) link(log) nolog eform 

* Model 3
glm c_section i.group sex primiparous del_ga  /// 
			   laz wlz hcz ///
               moth_height weekly_weight_gain moth_crp_above5 moth_edu   ///
			   , fam(bin) link(log) nolog eform 
			   		   
*
**
***
********************************************************************************
*** Table 4. Stratified analysis of caesarean section between groups  **********
********************************************************************************

*interaction tests

xi: glm c_section i.group primiparous moth_malaria moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super1
xi: glm c_section i.group*primiparous moth_malaria moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super2
lrtest super1 super2

xi: glm c_section i.group moth_malaria primiparous moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super3
xi: glm c_section i.group*moth_malaria primiparous moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super4
lrtest super3 super4

xi: glm c_section i.group moth_edu_below_median primiparous moth_malaria moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super5
xi: glm c_section i.group*moth_edu_below_median primiparous moth_malaria moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super6
lrtest super5 super6

xi: glm c_section i.group moth_bmi_below_median primiparous moth_malaria moth_edu_below_median i.site, fam(bin) link(log) nolog eform
estimates store super7
xi: glm c_section i.group*moth_bmi_below_median primiparous moth_malaria moth_edu_below_median i.site, fam(bin) link(log) nolog eform
estimates store super8
lrtest super7 super8

* ratios
tab c_section group if primiparous==1, col exact
tab c_section group if primiparous==0, col exact

tab c_section group if moth_malaria==0, col exact
tab c_section group if moth_malaria==1, col exact

tab c_section group if moth_edu_below_median==1, col exact
tab c_section group if moth_edu_below_median==0, col exact

tab c_section group if moth_bmi_below_median==1, col exact
tab c_section group if moth_bmi_below_median==0, col exact

* RR
glm c_section   i.group moth_malaria moth_edu_below_median moth_bmi_below_median i.site if primiparous == 1 , fam(bin) link(log) nolog eform
glm c_section   i.group moth_malaria moth_edu_below_median moth_bmi_below_median i.site if primiparous == 0 , fam(bin) link(log) nolog eform

glm c_section   i.group primiparous moth_edu_below_median moth_bmi_below_median i.site if moth_malaria == 0 , fam(bin) link(log) nolog eform
glm c_section   i.group primiparous moth_edu_below_median moth_bmi_below_median i.site if moth_malaria == 1 , fam(bin) link(log) nolog eform

glm c_section   i.group moth_malaria primiparous moth_bmi_below_median i.site if moth_edu_below_median == 1 , fam(bin) link(log) nolog eform
glm c_section   i.group moth_malaria primiparous moth_bmi_below_median i.site if moth_edu_below_median == 0 , fam(bin) link(log) nolog eform

glm c_section   i.group moth_malaria moth_edu_below_median primiparous i.site if moth_bmi_below_median == 1 , fam(bin) link(log) nolog eform
glm c_section   i.group moth_malaria moth_edu_below_median primiparous i.site if moth_bmi_below_median == 0 , fam(bin) link(log) nolog eform

*
**
***
********************************************************************************
*** Supplemental table 2. Risk ratio to emercenry caesarean section ************
********************************************************************************
	
* Model 1
glm c_emer i.group               , fam(bin) link(log) nolog eform 
glm c_emer sex                   , fam(bin) link(log) nolog eform 
glm c_emer primiparous           , fam(bin) link(log) nolog eform  
glm c_emer del_ga                , fam(bin) link(log) nolog eform 
 
glm c_emer laz                   , fam(bin) link(log) nolog eform 
glm c_emer waz                   , fam(bin) link(log) nolog eform  
glm c_emer wlz                   , fam(bin) link(log) nolog eform  
glm c_emer hcz                   , fam(bin) link(log) nolog eform 
 
glm c_emer moth_height           , fam(bin) link(log) nolog eform  
glm c_emer moth_bmi              , fam(bin) link(log) nolog eform  
glm c_emer moth_age              , fam(bin) link(log) nolog eform 
glm c_emer weekly_weight_gain    , fam(bin) link(log) nolog eform
 
glm c_emer moth_hiv              , fam(bin) link(log) nolog eform  
glm c_emer moth_malaria          , fam(bin) link(log) nolog eform  
glm c_emer moth_agp_above1       , fam(bin) link(log) nolog eform  
glm c_emer moth_crp_above5       , fam(bin) link(log) nolog eform 
 
glm c_emer moth_edu              , fam(bin) link(log) nolog eform  
glm c_emer hh_asset_below_median , fam(bin) link(log) nolog eform  
 
* Model 2
glm c_emer i.group sex primiparous del_ga  /// 
               moth_height  moth_crp_above5 moth_edu  ///
			   , fam(bin) link(log) nolog eform 

* Model 3
glm c_emer i.group sex primiparous del_ga  /// 
			   laz wlz hcz ///
               moth_height weekly_weight_gain moth_crp_above5 moth_edu  ///
			   , fam(bin) link(log) nolog eform 
		   
*
**
***
*******************************************************************************************************
*** Supplemental table 3. Stratified analysis of emergency caesarean section between groups  **********
*******************************************************************************************************

*interaction tests

xi: glm c_emer i.group primiparous moth_malaria moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super1
xi: glm c_emer i.group*primiparous moth_malaria moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super2
lrtest super1 super2

xi: glm c_emer i.group moth_malaria primiparous moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super3
xi: glm c_emer i.group*moth_malaria primiparous moth_edu_below_median moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super4
lrtest super3 super4

xi: glm c_emer i.group moth_edu_below_median primiparous moth_malaria moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super5
xi: glm c_emer i.group*moth_edu_below_median primiparous moth_malaria moth_bmi_below_median i.site, fam(bin) link(log) nolog eform
estimates store super6
lrtest super5 super6

xi: glm c_emer i.group moth_bmi_below_median primiparous moth_malaria moth_edu_below_median i.site, fam(bin) link(log) nolog eform
estimates store super7
xi: glm c_emer i.group*moth_bmi_below_median primiparous moth_malaria moth_edu_below_median i.site, fam(bin) link(log) nolog eform
estimates store super8
lrtest super7 super8

* ratios
tab c_emer group if primiparous==1, col exact
tab c_emer group if primiparous==0, col exact

tab c_emer group if moth_malaria==0, col exact
tab c_emer group if moth_malaria==1, col exact

tab c_emer group if moth_edu_below_median==1, col exact
tab c_emer group if moth_edu_below_median==0, col exact

tab c_emer group if moth_bmi_below_median==1, col exact
tab c_emer group if moth_bmi_below_median==0, col exact

* RR
glm c_emer   i.group moth_malaria moth_edu_below_median moth_bmi_below_median i.site if primiparous == 1 , fam(bin) link(log) nolog eform
glm c_emer   i.group moth_malaria moth_edu_below_median moth_bmi_below_median i.site if primiparous == 0 , fam(bin) link(log) nolog eform

glm c_emer   i.group primiparous moth_edu_below_median moth_bmi_below_median i.site if moth_malaria == 0 , fam(bin) link(log) nolog eform
glm c_emer   i.group primiparous moth_edu_below_median moth_bmi_below_median i.site if moth_malaria == 1 , fam(bin) link(log) nolog eform

glm c_emer   i.group moth_malaria primiparous moth_bmi_below_median i.site if moth_edu_below_median == 1 , fam(bin) link(log) nolog eform
glm c_emer   i.group moth_malaria primiparous moth_bmi_below_median i.site if moth_edu_below_median == 0 , fam(bin) link(log) nolog eform

glm c_emer   i.group moth_malaria moth_edu_below_median primiparous i.site if moth_bmi_below_median == 1 , fam(bin) link(log) nolog eform
glm c_emer   i.group moth_malaria moth_edu_below_median primiparous i.site if moth_bmi_below_median == 0 , fam(bin) link(log) nolog eform

*end
