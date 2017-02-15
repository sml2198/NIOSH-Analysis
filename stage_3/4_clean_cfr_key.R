# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Clean CFR Key
  # Marks all CFR subsection codes as either relevent or maybe relevant to MR or PS
    # based on a meeting between Alison Morantz, Nikhil Saifullah, and Sarah Levine 
    # at NIOSH in Pittsburgh in 2/12/2016 
  # Outputs a CFR key to be merged onto violations data
  # Also outputs MR and PS keys to make appendx tables

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/8/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL) 
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # CFR key 
cfr.key.in.file.name = paste0(originals.path, "/cfr_key/cfr_key.csv", collapse = NULL)

# outputs
  # cleaned CFR key with all relevant subparts marked
cfr.key.out.file.name = paste0(cleaned.path, "/clean_cfr_key.rds", collapse = NULL)
  # cleaned CFR key with MR relevant subparts marked (used to make appendix tables)
MR.cfr.key.out.file.name = paste0(cleaned.path, "/MR_clean_cfr_key.csv", collapse = NULL)
  # cleaned CFR key with PS relevant subparts marked (used to make appendix tables)
PS.cfr.key.out.file.name = paste0(cleaned.path, "/PS_clean_cfr_key.csv", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read CFR key
cfr.key = read.csv(cfr.key.in.file.name)

# bye
rm(cfr.key.in.file.name)

################################################################################

# EDIT DATA

# format cfr subsection code
cfr.key$cfr_section_code_desc = as.character(cfr.key$cfr_section_code_desc)

# generate subsection codes variables
subsectioncodelength = attr(regexpr("[0-9]+\\.[0-9]+(-)*[0-9]*", cfr.key$cfr_section_code_desc), "match.length")
cfr.key$subsectioncode = substr(cfr.key$cfr_section_code_desc, 2, 1 + subsectioncodelength)

# bye
rm(subsectioncodelength)

################################################################################

# MARK SUBSECTIONS RELEVANT TO MAINTENANCE & REPAIR

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "48" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart A", "Subpart B")) & 
                              !(cfr.key$subsectioncode %in% c("48.10", "48.3", "48.9", 
                                                              "48.23", "48.29", "48.30", 
                                                              "48.31", "48.32")), 1, 0)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "71" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart H")) & 
                              (cfr.key$subsectioncode %in% c("71.701")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "72" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart E")) & 
                              (cfr.key$subsectioncode %in% c("72.610", "72.620", "72.630")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart B")) & 
                              (cfr.key$subsectioncode %in% c("75.100", "75.150", "75.151", 
                                                             "75.153", "75.155", "75.156")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart C")) & 
                              (cfr.key$subsectioncode %in% c("75.202", "75.208", "75.211", 
                                                             "75.212")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart D")) & 
                              (cfr.key$subsectioncode %in% c("75.312", "75.320", "75.324", 
                                                             "75.340", "75.341", "75.342", 
                                                             "75.344", "75.352", "75.382")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.503", "75.504", "75.505",
                                                             "75.506", "75.506-1", "75.507")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.510-1", "75.511", "75.511-1", 
                                                             "75.512", "75.512-1", "75.512-2", 
                                                             "75.513", "75.513-1", "75.514")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.515", "75.516", "75.516-1",
                                                             "75.516-2", "75.517", "75.517-1", 
                                                             "75.518", "75.518-1")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.519", "75.519-1", "75.520", 
                                                             "75.523", "75.523-1", "75.523-2")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart G")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart H")) & 
                              (cfr.key$subsectioncode %in% c("75.700", "75.700-1", "75.701", 
                                                             "75.701-1", "75.701-2", "75.701-3", 
                                                             "75.701-4", "75.701-5")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart H")) & 
                              (cfr.key$subsectioncode %in% c("75.702", "75.702-1", "75.703", 
                                                             "75.703-1", "75.703-2", "75.703-3", 
                                                             "75.703-4", "75.704", "75.704-1")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart H")) & 
                              (cfr.key$subsectioncode %in% c("75.705", "75.705-1", "75.705-2", 
                                                             "75.705-3", "75.705-6", "75.705-8", 
                                                             "75.705-9", "75.706")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.800", "75.800-2", "75.800-3", 
                                                             "75.800-4", "75.801", "75.802", 
                                                             "75.803", "75.803-2", "75.804")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.805", "75.806", "75.807", 
                                                             "75.808", "75.809", "75.810", 
                                                             "75.811", "75.812")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.814", "75.815", "75.816", 
                                                             "75.818", "75.819", "75.820", 
                                                             "75.821", "75.825", "75.827")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.831", "75.832", "75.834", 
                                                             "75.828", "75.829")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart J")) & 
                              (cfr.key$subsectioncode %in% c("75.900", "75.900-2", "75.900-3", 
                                                             "75.900-4", "75.901", "75.902", 
                                                             "75.902-1", "75.902-2", "75.902-4")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart J")) & 
                              (cfr.key$subsectioncode %in% c("75.903", "75.904", "75.905", 
                                                             "75.907")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart K")) & 
                              (cfr.key$subsectioncode %in% c("75.1001", "75.1001-1", "75.1003-1")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart L")) & 
                              (cfr.key$subsectioncode %in% c("75.1100-2", "75.1103-4", "75.1104", 
                                                             "75.1106", "75.1106-2", "75.1106-3",
                                                             "75.1106-4", "75.1106-5", "75.1106-6")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart L")) & 
                              (cfr.key$subsectioncode %in% c("75.1107-14")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart O")) & 
                              (cfr.key$subsectioncode %in% c("75.1400", "75.1400-1", "75.1400-2", 
                                                             "75.1400-3", "75.1400-4", "75.1401",
                                                             "75.1401-1", "75.1402-2", "75.1403-10",
                                                             "75.1403-11", "75.1403-2", "75.1403-3", 
                                                             "75.1403-4", "75.1403-5", "75.1403-6", 
                                                             "75.1403-7", "75.1403-8", "75.1403-9",
                                                             "75.1404", "75.1404-1", "75.1405", 
                                                             "75.1405-1", "75.1431", "75.1432", 
                                                             "75.1433", "75.1434", "75.1435",
                                                             "75.1436", "75.1437", "75.1438")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart Q")) & 
                              (cfr.key$subsectioncode %in% c("75.1600-2")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart R")) & 
                              (cfr.key$subsectioncode %in% c("75.1712-10", "75.1712-6", "75.1720", 
                                                             "75.1721", "75.1725", "75.1726", 
                                                             "75.1727", "75.1728", "75.1729",
                                                             "75.1730", "75.1731")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart T")) & 
                              (cfr.key$subsectioncode %in% c("75.1903", "75.1909", "75.1910", 
                                                             "75.1913", "75.1914", "75.1915")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart B")) & 
                              (cfr.key$subsectioncode %in% c("77.103", "77.104")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart C")) & 
                              (cfr.key$subsectioncode %in% c("77.200", "77.202", "77.203", 
                                                             "77.204", "77.205", "77.206", 
                                                             "77.207", "77.208", "77.210",
                                                             "77.216")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart D")) & 
                              (cfr.key$subsectioncode %in% c( "77.305", "77.309", "77.311",
                                                              "77.314", "77.315")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart E", "Subpart F", "Subpart G", 
                                                               "Subpart H", "Subpart I", "Subpart J")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart L")) & 
                              (cfr.key$subsectioncode %in% c("77.1103", "77.1104", "77.1111", 
                                                             "77.1112")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart O")) & 
                              (cfr.key$subsectioncode %in% c("77.1403", "77.1405", "77.1431", 
                                                             "77.1432", "77.1433", "77.1434", 
                                                             "77.1435", "77.1436", "77.1437",
                                                             "77.1438")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart Q")) & 
                              (cfr.key$subsectioncode %in% c("77.1605", "77.1606")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart R")) & 
                              (cfr.key$subsectioncode %in% c("77.1710")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart S")) & 
                              (cfr.key$subsectioncode %in% c("77.1800", "77.1801", "77.1802")), 1, cfr.key$MRrelevant)

cfr.key$MRrelevant = ifelse(cfr.key$cfr_part_code == "77" &
                              (cfr.key$cfr_subpart_code %in% c("Subpart T")) & 
                              (cfr.key$subsectioncode %in% c("77.1906", "77.1915", "77.1916")), 1, cfr.key$MRrelevant)

################################################################################

# MARK SUBSECTIONS MAYBE RELEVANT TO MAINTENANCE & REPAIR

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "47" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart E")), 1, 0)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "72" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart D")) &
                                   (cfr.key$subsectioncode %in% c("72.503")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart B")) &
                                   (cfr.key$subsectioncode %in% c("75.160")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart C")) &
                                   (cfr.key$subsectioncode %in% c("75.214")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart D")) &
                                   (cfr.key$subsectioncode %in% c("75.337")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart L")) &
                                   (cfr.key$subsectioncode %in% c("75.1101-20", "75.1102")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart T")) &
                                   (cfr.key$subsectioncode %in% c("75.1911", "75.1912")), 1, cfr.key$MRmayberelevant)

cfr.key$MRmayberelevant = ifelse(cfr.key$cfr_part_code == "77" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart L")) &
                                   (cfr.key$subsectioncode %in% c("77.1106")), 1, cfr.key$MRmayberelevant)

################################################################################

# MARK SUBSECTIONS RELEVANT TO PINNING & STRIKING

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "48" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart A", "Subpart B")) & 
                              !(cfr.key$subsectioncode %in% c("48.10", "48.3", "48.9", 
                                                              "48.23", "48.29", "48.30", 
                                                              "48.31", "48.32")), 1, 0)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart B")) & 
                              (cfr.key$subsectioncode %in% c("75.100", "75.153", "75.155", 
                                                             "75.156")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart C")) & 
                              (cfr.key$subsectioncode %in% c("75.203", "75.204", "75.205", 
                                                             "75.207", "75.208", "75.209", 
                                                             "75.212", "75.213", "75.215")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart D")) & 
                               (cfr.key$subsectioncode %in% c("75.332", "75.334", "75.340")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.500", "75.500-1", "75.501", 
                                                             "75.501-2", "75.502", "75.503", 
                                                             "75.505", "75.506-1", "75.507")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart F")) & 
                              (cfr.key$subsectioncode %in% c("75.507-1", "75.508-1", "75.509", 
                                                             "75.510", "75.512-1", "75.523", 
                                                             "75.523-3", "75.524")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart G")) & 
                              (cfr.key$subsectioncode %in% c("75.602", "75.603", "75.604", 
                                                             "75.605", "75.606", "75.607")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart H")) & 
                              (cfr.key$subsectioncode %in% c("75.703-3", "75.703-4")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.807", "75.810", "75.811", 
                                                             "75.812")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart I")) & 
                              (cfr.key$subsectioncode %in% c("75.816", "75.817")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart J")) & 
                              (cfr.key$subsectioncode %in% c("75.906")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart K")) & 
                              (cfr.key$subsectioncode %in% c("75.1002", "75.1003", "75.1003-2")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart N")) & 
                              (cfr.key$subsectioncode %in% c("75.1311")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart O")) & 
                              (cfr.key$subsectioncode %in% c("75.1400", "75.1400-1", "75.1403-10", 
                                                             "75.1403-6", "75.1403-7", "75.1403-8", 
                                                             "75.1404", "75.1404-1", "75.1405")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart O")) & 
                              (cfr.key$subsectioncode %in% c("75.1405-1")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart R")) & 
                              (cfr.key$subsectioncode %in% c("75.1719-2", "75.1719-4", "75.1720", 
                                                             "75.1725")), 1, cfr.key$PSrelevant)

cfr.key$PSrelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                              (cfr.key$cfr_subpart_code %in% c("Subpart T")) & 
                              (cfr.key$subsectioncode %in% c("75.1906", "75.1916")), 1, cfr.key$PSrelevant)

################################################################################

# MARK SUBSECTIONS MAYBE RELEVANT TO PINNING & STRIKING

cfr.key$PSmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart B")) & 
                                   (cfr.key$subsectioncode %in% c("75.160")), 1, 0)

cfr.key$PSmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart D")) & 
                                   (cfr.key$subsectioncode %in% c("75.327", "75.337", "75.343", 
                                                                  "75.373", "75.385", "75.388", 
                                                                  "75.389")), 1, cfr.key$PSmayberelevant)

cfr.key$PSmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart N")) & 
                                   (cfr.key$subsectioncode %in% c("75.1315", "75.1316", "75.1318", 
                                                                  "75.1322")), 1, cfr.key$PSmayberelevant)

cfr.key$PSmayberelevant = ifelse(cfr.key$cfr_part_code == "75" & 
                                   (cfr.key$cfr_subpart_code %in% c("Subpart O")) & 
                                   (cfr.key$subsectioncode %in% c("75.1403-5")), 1, cfr.key$PSmayberelevant)

################################################################################

# EDIT DATA

# generate MR and PS keys
  # 366 rows; 3 columns
MR.cfr.key = cfr.key[which(cfr.key$MRrelevant == 1 | cfr.key$MRmayberelevant == 1), 
                     c("cfr_section_code_desc", "MRrelevant", "subsectioncode")]
  # 97 rows; 3 columns
PS.cfr.key = cfr.key[which(cfr.key$PSrelevant == 1 | cfr.key$PSmayberelevant == 1), 
                     c("cfr_section_code_desc", "PSrelevant", "subsectioncode")]

# order by relevance and subsection code
MR.cfr.key = MR.cfr.key[order(-MR.cfr.key$MRrelevant, MR.cfr.key$subsectioncode),]
PS.cfr.key = PS.cfr.key[order(-PS.cfr.key$PSrelevant, PS.cfr.key$subsectioncode),]

# rename variables
MR.cfr.key$MRrelevant = ifelse(MR.cfr.key$MRrelevant == 0, "Maybe Relevant", "Relevant")
PS.cfr.key$PSrelevant = ifelse(PS.cfr.key$PSrelevant == 0, "Maybe Relevant", "Relevant")

################################################################################

# OUTPUT DATA

# output cleaned CFR key
  # 2026 rows; 11 columns; unique on subsectioncode
saveRDS(cfr.key, file = cfr.key.out.file.name)

# output cleaned MR CFR key
  # 366 rows; 2 columns; unique on subsectioncode
write.csv(MR.cfr.key[, c("cfr_section_code_desc", "MRrelevant")], file = MR.cfr.key.out.file.name, row.names = FALSE, na = "")

# output cleaned PS CFR key
  # 97 rows; 2 columns; unique on subsectioncode
write.csv(PS.cfr.key[, c("cfr_section_code_desc", "PSrelevant")], file = PS.cfr.key.out.file.name, row.names = FALSE, na = "")

# bye
rm(list = ls())

################################################################################
