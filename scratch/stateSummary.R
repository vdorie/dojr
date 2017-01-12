agencies <- read.csv("~/Downloads/openjustice_agencies_comparison.csv", header = FALSE)
colnames(agencies) <-
  c("year",
    "county",
    "jrs_number",
    "jrs_name",
    "arr_ttl",
    "arr_fny",
    "arr_mdm",
    "arr_sts",
    "crm_vlt_ttl",
    "crm_prp_ttl",
    "clr_vlt",
    "clr_prp",
    "dic_ttl",
    "dic_arr",
    "dic_jil",
    "aoo_dth_ttl",
    "aoo_dth_fny",
    "aoo_dth_acc",
    "aoo_ast_ttl",
    "aoo_ast_inj",
    "aoo_ast_noi",
    "srv_cls",
    "unknown",
    "dmo_ttl",
    "dmo_hsp",
    "dmo_wht",
    "dmo_blk",
    "dmo_nat",
    "dmo_asn",
    "dmo_oth",
    "dmo_pct_hsp",
    "dmo_pct_wht",
    "dmo_pct_blk",
    "dmo_pct_nat",
    "dmo_pct_asn",
    "dmo_pct_oth",
    "arr_rte_ttl",
    "arr_rte_fny",
    "arr_rte_mdm",
    "arr_rte_sts",
    "crm_vlt_rte",
    "crm_prp_rte",
    "dic_rte_ttl",
    "dic_rte_arr",
    "dic_rte_jil",
    "aoo_rte_dth_ttl",
    "aoo_rte_dth_fny",
    "aoo_rte_dth_acc",
    "aoo_rte_ast_ttl",
    "aoo_rte_ast_inj",
    "aoo_rte_ast_noi",
    "lbr_pvt",
    "lbr_emp",
    "lbr_une",
    "lbr_med_ern",
    "lbr_med_hhd_inc",
    "edu_lt_hs",
    "edu_hs",
    "edu_bch")

allAgencies <- subset(agencies, jrs_name == "All Combined")

years <- sort(unique(allAgencies$year))

stateTotals <- allAgencies[seq_along(years),]
stateTotals$county <- "Statewide"
stateTotals$year   <- years
row.names(stateTotals) <- NULL

colsToSum <- c("arr_ttl", "arr_fny", "arr_mdm", "arr_sts", "crm_vlt_ttl", "crm_prp_ttl", "dic_ttl", "dic_arr", "dic_jil", "aoo_dth_ttl",
               "aoo_dth_fny", "aoo_dth_acc", "aoo_ast_ttl", "aoo_ast_inj", "aoo_ast_noi", "srv_cls", "dmo_ttl", "dmo_hsp", "dmo_wht",
               "dmo_blk", "dmo_nat", "dmo_asn", "dmo_oth")
for (col in colsToSum) {
  allAgencies[,col] <- suppressWarnings(as.integer(as.character(allAgencies[,col])))
  stateTotals[,col] <- suppressWarnings(as.integer(as.character(stateTotals[,col])))
}

rateCols <- c("clr_vlt", "clr_prp", "dmo_pct_hsp", "dmo_pct_wht", "dmo_pct_blk", "dmo_pct_nat", "dmo_pct_asn", "dmo_pct_oth",
              "arr_rte_ttl", "arr_rte_fny", "arr_rte_mdm", "arr_rte_sts", "crm_vlt_rte", "crm_prp_rte", "dic_rte_ttl", "dic_rte_arr",
              "dic_rte_jil", "aoo_rte_dth_ttl", "aoo_rte_dth_fny", "aoo_rte_dth_acc", "aoo_rte_ast_ttl", "aoo_rte_ast_inj",
              "aoo_rte_ast_noi")
for (col in rateCols) {
  allAgencies[,col] <- suppressWarnings(as.numeric(as.character(allAgencies[,col])))
  stateTotals[,col] <- suppressWarnings(as.numeric(as.character(stateTotals[,col])))
}

arrestCounts <- read.csv("~/Downloads/crimes_data_2006-2015.csv")

for (i in seq_along(years)) {
  allAgencies.i <- subset(allAgencies, year == years[i])
  
  for (col in colsToSum)
    stateTotals[i,col] <- sum(allAgencies.i[,col], na.rm = TRUE)
  
  if (years[i] == 2005) {
    stateTotals[i,"clr_vlt"] <- 100 * sum(allAgencies.i[,"crm_vlt_ttl"] * allAgencies.i[,"clr_vlt"] / 100) / stateTotals[i,"crm_vlt_ttl"]
    stateTotals[i,"clr_prp"] <- 100 * sum(allAgencies.i[,"crm_prp_ttl"] * allAgencies.i[,"clr_prp"] / 100) / stateTotals[i,"crm_prp_ttl"]
  } else {
    violentTotal     <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "Violent_sum")))
    violentClearance <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "ViolentClr_sum")))
    
    if (sum(violentTotal) != stateTotals[i,"crm_vlt_ttl"]) stop("mismatch in violent total for year ", years[i])
    
    stateTotals[i,"clr_vlt"] <- 100 * sum(violentClearance) / sum(violentTotal)
    
    propertyTotal     <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "Property_sum")))
    propertyClearance <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "PropertyClr_sum")))
    
    if (sum(propertyTotal) != stateTotals[i,"crm_prp_ttl"]) stop("mismatch in property total for year ", years[i])
    
    stateTotals[i,"clr_prp"] <- 100 * sum(propertyClearance) / sum(propertyTotal)
  }
  
  for (col in c("dmo_hsp", "dmo_wht", "dmo_blk", "dmo_nat", "dmo_asn")) {
    pctCol <- paste0(strsplit(col, "_")[[1L]], collapse = "_pct_")
    stateTotals[i,pctCol] <- floor(10 * stateTotals[i,col] / stateTotals[i,"dmo_ttl"]) / 10
  }
  
  for (col in c("arr_ttl", "arr_fny", "arr_mdm", "arr_sts", "dic_ttl", "dic_arr", "dic_jil")) {
    rateCol <- paste0(strsplit(col, "_")[[1L]], collapse = "_rte_")
    stateTotals[i,rateCol] <- 100000 * stateTotals[i,col] / stateTotals[i,"dmo_ttl"]
  }
  
  stateTotals[i,"crm_vlt_rte"] <- 100000 * stateTotals[i,"crm_vlt_ttl"] / stateTotals[i,"dmo_ttl"]
  stateTotals[i,"crm_prp_rte"] <- 100000 * stateTotals[i,"crm_prp_ttl"] / stateTotals[i,"dmo_ttl"]
  
  for (col in c("aoo_dth_ttl", "aoo_dth_fny", "aoo_dth_acc", "aoo_ast_ttl", "aoo_ast_inj", "aoo_ast_noi")) {
    splitCol <- strsplit(col, "_")[[1L]]
    rateCol <- paste(splitCol[1L], "rte", splitCol[2L], splitCol[3L], sep = "_")
    stateTotals[i,rateCol] <- 100000 * stateTotals[i,col] / stateTotals[i,"dmo_ttl"]
  }
}

cncRaw <- read.csv("~/Downloads/ca_doj_crimes_clearances_1982-2014_05-20-2016.csv")

propertyClearances <- c(
  "burglary_forcible_entry_total_cleared", "burglary_unlawful_entry_total_cleared", "burglary_attempted_forcible_entry_total_cleared",
  "larceny_theft_total_cleared", "motor_vehicle_theft_auto_total_cleared", "motor_vehicle_theft_trucks_buses_total_cleared",
  "motor_vehicle_theft_other_total_cleared")
violentClearances <- c(
  "murder_nonnegligent_manslaughter_total_cleared", "rape_total_cleared", "attempt_to_rape_total_cleared", "robbery_firearm_total_cleared", "robbery_knife_cutting_instrument_total_cleared", "robbery_other_dangerous_weapon_total_cleared", "robbery_strongarm_total_cleared", "assault_firearm_total_cleared", "assault_knife_cutting_instrument_total_cleared", "assault_other_dangerous_weapon_total_cleared", "assault_hand_fist_feet_total_cleared")

## just run for 2005
# for (i in seq_along(years)) {
for (i in 1L) {
  cncRaw.i <- subset(cncRaw, year == years[i], c(propertyClearances, violentClearances))
  
  stateTotals[i,"clr_vlt"] <- 100 * sum(cncRaw.i[,violentClearances]) / stateTotals[i,"crm_vlt_ttl"]
  stateTotals[i,"clr_prp"] <- 100 * sum(cncRaw.i[,propertyClearances]) / stateTotals[i,"crm_prp_ttl"]
  
  if (years[i] > 2005) {
    violentClearance <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "ViolentClr_sum")))
    
    if (sum(violentClearance) != sum(cncRaw.i[,violentClearances])) stop("mismatch in violent clearances for year ", years[i])
    
    propertyClearance <- sapply(paste(levels(allAgencies$county), "County"), function(county) sum(subset(arrestCounts, Year == years[i] & County == county, "PropertyClr_sum")))
    
    if (sum(propertyClearance) != sum(cncRaw.i[,propertyClearances])) stop("mismatch in property clearances for year ", years[i])
  }
}

stateTotals$srv_cls <- NA
