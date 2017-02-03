# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Merge Violations
  # Merges assessments and cfr information onto violations

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # violations data
violations.in.file.name = paste0(clean.path, "/clean_violations.rds", collapse = NULL)
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

# load assessments
  # 843818 rows; 11 columns; unique on violationno
assessments = readRDS(assessments.in.file.name)

# load cfr key
  # 2026 rows; 11 columns; unique on subsection_code
cfr.key = readRDS(cfr.key.in.file.name)

# bye
rm(root, clean.path, merged.path, violations.in.file.name,
   assessments.in.file.name, cfr.key.in.file.name)

################################################################################

# MERGE ASSESSMENTS ON VIOLATIONS

# format violationnos
violations$violationno = factor(violations$violationno)
assessments$violationno = factor(assessments$violationno)
violations$violationno = as.character(violations$violationno)
assessments$violationno = as.character(assessments$violationno)
violations$violationno = str_pad(violations$violationno, 7, pad = "0")
assessments$violationno = str_pad(assessments$violationno, 7, pad = "0")

# merge assessments data with violations data
  # 843787 rows; 23 columns; unique on violationno-mineid (a few violationno's are not unique)
violations = merge(violations, assessments, by = c("mineid","violationno"), all = FALSE)

################################################################################

# CLEAN UP VIOLATION INFORMATION

# format subsectioncode from assessments data
violations$subsectioncode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", violations$subsectioncode)
violations$subsectioncode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "",  violations$subsectioncode)
violations$subsectioncodemarker = paste("S", violations$subsectioncode, sep = "")

# in 2 cases where subsection is missing, partsection can be subbed in 
violations$partsection2 = violations$partsection
violations$partsection2 = gsub("\\([a-z]+\\)", "", violations$partsection2)
violations$partsection2 = gsub("\\([0-9]+\\)", "", violations$partsection2)
violations$subsectioncode = ifelse((is.na(violations$subsectioncode) & !is.na(violations$partsection2)), 
                                   violations$partsection2, violations$subsectioncode)
violations$partsection2 = NULL

################################################################################

# MERGE CFR DATA ONTO VIOLATIONS

# The statement below yields 0 - so there are no improperly missing subsections. In every violation without a subsectioncode, 
# there is a non-missing sectionofact. As detailed on page 7 of the Citation and Order Writing Handbook for Coal Mines and 
# Metal and Non-metal Mines (MSHA Handbook Series, December 2013, Chapter 3: Violations Description & Issuing Form), which can be 
# retrieved here: http://arlweb.msha.gov/READROOM/HANDBOOK/PH13-I-1.pdf - a violation can be issued EITHER for a violation of a 
# specific regulation (a part/subsection of Title 30 - Chapter 1 - Suchapter P - Part 100 of the Code of Federal Regulations(CFR)) 
# or for a violation of the Mine Act. For our purposes, NIOSH is only interested in which sections (parts) and subsections of the 
# CFR code are predictive of injuries. Therefore, we preserve violations that are missing "subsectioncode" but not missing "sectionofact"
# (these are violations of the Mine Act and not the CFR), and we make sure they are reflected in our final "totalviolations"
# (per mine quarter) variable. - Sarah L. 8/19/2016 @ 1:02 PM. 
# sum(is.na(violations$sectionofact) & is.na(violations$subsectioncode))

# merge cfr key data and violations data 
  # 845021 rows; 34 columns; unique on violationno-mineid-eventno
violations = merge(violations, cfr.key, by = "subsectioncode", all = TRUE)

# drop data that didn't merge onto violations (has a subsection code but no violations data)
  # 843787 rows; 34 columns; unique on violationno-mineid-eventno
violations = violations[which(!is.na(violations$violationno)),]

# Some observations do not have relevant/maybe relevant designations mapped onto them (are missing for these vars).
# This happens in two cases: (1) if there is a subsection that we have NOT marked in terms of relevance in the cfr
# key. This only happens for subsections codes lower than 40. In our meeting with NIOSH we strictly considered parts
# 40 and above, and (2) if subsectioncode is code, which happens in cases of violations of the Mine Act. These
# observations are never missing mineactsectioncode. Here, we make all relevance vars equal to zero if missing.
varlist = names(violations[, grep("relevant", names(violations))])
for (j in 1:length(varlist)) {
  violations[, varlist[j]] = ifelse(is.na(violations[, varlist[j]]), 0, violations[, varlist[j]])
}

################################################################################

# format date variables
datevars = names(violations)[grep("date", names(violations))]
for (i in 1:length(datevars)) {
  violations[, datevars[i]] = as.Date(as.character(violations[, datevars[i]]), "%m/%d/%Y")
}
violations$quarter = as.yearqtr(violations$dateissued)
violations$year = as.numeric(format(violations$quarter, "%Y"))

# drop data before study period
  # 843760 rows; 34 columns; unique on violationno-mineid
violations = violations[(violations$year > "1999"), ]

################################################################################

# DROP VARIABLES WE DON'T NEED
# drop = c(
# [1] "assesscaseno"            "cfr_part_code"           "cfr_part_desc"           "cfr_section_code_desc"   "cfr_subpart_code"       
# [6] "cfr_subpart_desc"        "cfr_subpart_desc_2"      "currentassessmentamount" "dateissued"              "dateterminated"         
# [11] "datevacated"             "eventno"                 "issuedate"               "mineactsectioncode"      "mineid"                 
# [16] "MRmayberelevant"         "MRrelevant"              "partsection"             "penaltypoints"           "proposedpenaltyamount"  
# [21] "PSmayberelevant"         "PSrelevant"              "quarter"                 "sectionofact"            "sigandsubdesignation"   
# [26] "sigandsubindicator"      "subsectioncode"          "subsectioncodemarker"    "typeaction1"             "typeaction2"            
# [31] "violationno"             "violationtypecode"       "violatortypecode"        "year"       
# )

################################################################################

# output violations-level data
  # 843760 rows; 34 columns; unique on violationno-mineid
saveRDS(violations, file = violations.out.file.name)

################################################################################

rm(list = ls())

################################################################################
