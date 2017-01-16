# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Merge Violations
  # Merges assessments and inspection information onto violations, and then merges on cfr key

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "/Users/Sarah/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # violations data
violations.in.file.name = paste0(clean.path, "/clean_violations.rds", collapse = NULL)
  # inspections data
inspections.in.file.name = paste0(clean.path, "/clean_inspections.rds", collapse = NULL)
  # assessments data
assessments.in.file.name = paste0(clean.path, "/clean_assessments.rds", collapse = NULL)
  # cfr key
cfr.key.in.file.name = paste0(clean.path, "/clean_cfr_key.rds", collapse = NULL)

# outputs
  # merged violations
violations.out.file.name = paste0(merged.path, "/merged_violations.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(merged.path, recursive = TRUE)

################################################################################

# load violations
  # 868722 rows; 14 columns; unique on violationno
violations = readRDS(violations.in.file.name)

# load inspections
  # 192652 rows; 10 columns; unique on eventno
inspections = readRDS(inspections.in.file.name)

# load assessments
  # 843818 rows; 13 columns; unique on violationno
assessments = readRDS(assessments.in.file.name)

# load cfr key
  # 2026 rows; 11 columns; unique on subsection_code
cfr.key = readRDS(cfr.key.in.file.name)

# bye
rm(root, clean.path, merged.path, violations.in.file.name,
   inspections.in.file.name, assessments.in.file.name, cfr.key.in.file.name)

################################################################################

# MERGE DATASETS TOGETHER

violations$violationno = factor(violations$violationno)
assessments$violationno = factor(assessments$violationno)

# merge assessments data with violations data
  # 868722 rows; 14 columns; unique on violationno-mineid
violations1 = merge(violations, assessments, by = c("mineid","violationno"), all = FALSE)
violations2 = merge(violations, assessments, by = c("violationno"), all = FALSE)

# drop duplicate variables from merge 
common_varstbs = sub(".x", "", names(violations)[grep(".x", names(violations), fixed = T)], fixed = T)
violations = violations[, -grep(".y", names(violations), fixed = T)]
names(violations)[grep(".x", names(violations), fixed = T)] = common_varstbs

# clean occurrencedate field
assessments_violations$occurrencedate = as.character(assessments_violations$occurrencedate)
assessments_violations$violation_occur_dt = as.character(assessments_violations$violation_occur_dt)
sum((assessments_violations$occurrencedate != assessments_violations$violation_occur_dt), na.rm = TRUE) 

# remove these cases - wind up with 843,672 obs
assessments_violations = assessments_violations[((assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | 
                                                   is.na(assessments_violations$occurrencedate) | 
                                                   is.na(assessments_violations$violation_occur_dt)), ]

# if missing occurrencedate, fill in violation_occur_dt, then drop so we have just one var
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$occurrencedate = 
  assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$violation_occur_dt
assessments_violations$violation_occur_dt = NULL

######################################################################################################

# MERGE VIOLATIONS WITH INSPECTIONS

# merge inspections data and merged assessments and violations data
inspections$inspecid = paste("L", inspections$eventno, sep = "")
violations$inspecid = paste("L", violations$eventno, sep = "")
violations = merge(violations, inspections, by = c("mineid", "eventno")) 

# remove duplicate variables 
common_varstbs = sub(".x", "", names(violations)[grep(".x", names(violations), fixed = TRUE)], fixed = TRUE)
for (i in 1:length(common_varstbs)) {
  violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(violations[, "inspecmerge"] == 2, violations[, paste(common_varstbs[i], ".y", sep = "")], violations[, paste(common_varstbs[i], ".x", sep = "")])
}
violations = violations[, -grep(".y", names(violations), fixed = T)]
names(violations)[grep(".x", names(violations), fixed = T)] = common_varstbs

# format date variables
datevars = names(violations)[grep("date", names(violations))]
for (i in 1:length(datevars)) {
  violations[, datevars[i]] = as.Date(as.character(violations[, datevars[i]]), "%m/%d/%Y")
}
violations$quarter = as.yearqtr(violations$dateissued)
violations$year = as.yearqtr(violations$dateissued)
violations$year = as.numeric(format(violations$quarter, "%Y"))

# drop data before study period
violations = violations[(violations$quarter > "1999 Q4"), ]

# format contractorid
violations$contractorid = sub("^$", NA, violations$contractorid)
violations$contractorid = ifelse((violations$violatortypecode == "Contractor" & 
                                    is.na(violations$contractorid)), violations$violator_id, violations$contractorid)

######################################################################################################

# MERGE CFR DATA ONTO VIOLATIONS

# format cfr code in violations
names(violations)[names(violations) == "cfrstandardcode"] = "subsection_code"
violations$subsection_code = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", violations$subsection_code)
violations$subsection_code = gsub("(-([a-z]+)\\))+(\\([0-9])*", "",  violations$subsection_code)

merged_violations$subsection_code_marker = paste("S", merged_violations$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")

# where subsection is missing (<10 cases), part_section can be subbed in 
violations$part_section2 = violations$part_section
violations$part_section2 = gsub("\\([a-z]+\\)", "", violations$part_section2)
violations$part_section2 = gsub("\\([0-9]+\\)", "", violations$part_section2)
violations$subsection_code = ifelse((is.na(violations$subsection_code) & !is.na(violations$part_section2)), 
                                    violations$part_section2, violations$subsection_code)
violations = violations[,-match("part_section2", names(violations))]

# This statement below yields 0 - so there are no improperly missing subsections. In every violation without a subsection_code, 
# there is a non-missing section_of_act. As detailed on page 7 of the Citation and Order Writing  Handbook for Coal Mines and 
# Metal and Non-metal Mines (MSHA Handbook Series, December 2013, Chapter 3: Violations Description & Issuing Form), which can be 
# retrieved here: http://arlweb.msha.gov/READROOM/HANDBOOK/PH13-I-1.pdf - a violation can be issued EITHER for a violation of a 
# specific regulation (a part/subsection of Title 30 - Chapter 1 - Suchapter P - Part 100 of the Code of Federal Regulations(CFR)) 
# or for a violation of the Mine Act. For our purposes, NIOSH is only interested in which sections (parts) and subsections of the 
# CFR code are predictive of injuries. Therefore, we preserve violations that are missing "subsection_code" but not missing "section_of_act"
# (these are violations of the Mine Act and not the CFR), and we make sure they are reflected in our final "totalviolations"
# (per mine quarter) variable. We will also create a "total_mine_act_violations" variable, down the line. However, we will not 
# prove the predictive power of these types of violations. - Sarah L. 8/19/2016 @ 1:02 PM. 
#sum(is.na(violations$section_of_act) & is.na(violations$subsection_code))

# merge cfr key data and violations data 
violations = merge(violations, cfr.key, by = "subsection_code", all = TRUE)

# Some observations do not have relevant/maybe relevant designations mapped onto them (are missing for these vars).
# This happens in two cases: (1) if there is a subsection that we have NOT marked in terms of relevance in the cfr
# key. This only happens for subsections codes lower than 40. In our meeting with NIOSH we strictly considered parts
# 40 and above, and (2) if subsection_code is code, which happens in cases of violations of the Mine Act. These
# observations are never missing mineactsectioncode. Here, we make all relevance vars equal to zero if missing.

varlist = names(violations[, grep("relevant", names(violations))])
for (j in 1:length(varlist)) {
  violations[, varlist[j]] = ifelse(is.na(violations[, varlist[j]]), 0, violations[, varlist[j]])
}

# remove duplicate variables
common_varstbs = sub(".x", "", names(violations)[grep(".x", names(violations), fixed = TRUE)], fixed = TRUE)
for (i in 1:length(common_varstbs)) {
  violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(violations[, "merge"] == 2, 
                                                                  violations[, paste(common_varstbs[i], ".y", sep = "")], 
                                                                  violations[, paste(common_varstbs[i], ".x", sep = "")])
}
violations = violations[, -grep(".y", names(violations), fixed = TRUE)]
names(violations)[grep(".x", names(violations), fixed = TRUE)] = common_varstbs

# drop data that didn't merge onto violations
violations = violations[complete.cases(violations$violationno), ]

# condition the per-day vars on positive denominator
# there are 256 cases of zero inspection days and positive violation counts 6/6/16
violations$contractor_violation_cnt = ifelse(violations$violatortypecode == "Contractor", violations$violator_violation_cnt, NA)
violations$operator_violation_pInspDay = ifelse((violations$violatortypecode == "Operator" & 
                                                   violations$violator_inspection_day_cnt > 0), 
                                                violations$violator_violation_cnt/violations$violator_inspection_day_cnt, NA)
violations$contractor_repeated_viol_cnt = ifelse(violations$violatortypecode == "Contractor", violations$violator_repeated_viol_cnt, NA)
violations$operator_repeated_viol_pInspDay = ifelse((violations$violatortypecode == "Operator" & 
                                                       violations$violator_inspection_day_cnt > 0), 
                                                    violations$violator_repeated_viol_cnt/violations$violator_inspection_day_cnt, NA)

################################################################################

# output violations-level data
saveRDS(violations, file = violations.out.file.name)

################################################################################

rm(list = ls())

################################################################################
