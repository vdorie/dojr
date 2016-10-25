purchases <- read.csv("~/Downloads/temp/Gun Data Analysis intermediate files and all others/dros_document_dealer_subset_08.12.csv")

## remove bad rows
purchases <- purchases[-c(2289215L, 2289222L, 2289224L, 5887863L),]

factorToInteger <- function(x, dropset) {
  if (!is.factor(x) && is.integer(x)) return(x)
  x[x %in% dropset] <- NA_integer_
  x <- droplevels(x)
  suppressWarnings(levels.i <- as.integer(levels(x)))
  if (any(is.na(levels.i))) stop("could not coerce levels: ", paste0(levels(x)[is.na(levels.i)], collapse = "/"))
  
  levels.i[as.integer(x)]
}
factorToDouble <- function(x, dropset) {
  if (!is.factor(x) && is.double(x)) return(x)
  x[x %in% dropset] <- NA_integer_
  x <- droplevels(x)
  suppressWarnings(levels.d <- as.double(levels(x)))
  if (any(is.na(levels.d))) stop("could not coerce levels: ", paste0(levels(x)[is.na(levels.d)], collapse = "/"))
  
  level.d[as.integer(x)]
}

cleanFactor <- function(x, dropset) {
  x[x %in% dropset] <- NA_integer_
  x <- droplevels(x)
  x
}

integerColumns <-
  c("weapon_key", "seller_person_key", "dealer_id", "sale_transaction_date",
    "multiple_purchase_number")

for (colName in integerColumns) purchases[[colName]] <- factorToInteger(purchases[[colName]], "")

invisible(gc(FALSE))

factorColumns <-
  c("gun_show_flag", "peace_officer_exemption_flag", "bfsc_exemption_code", "dros_gun_type",
    "undetermined_flag", "conviction_status", "voluntary_mental_status", "involuntary_mental_status",
    "restraining_order_status", "thirty_day_exempt_flag", "new_used_flag", "multiple_purchase_exmpt_flag",
    "fsd_compliance", "data_date", "transaction_code", "transaction_type", "bfsc_exemption", "last_queue_status",
    "waiting_period_exemption_type", "dealer_type_description", "dealer_status_description",
    "dealership_dros_submission_type", "dealership_assault_weapon_flag", "dealership_county", "business_name")

for (colName in factorColumns) purchases[[colName]] <- cleanFactor(purchases[[colName]], "")

invisible(gc(FALSE))

# cbind(sapply(purchases, class), t(purchases[1,]))
# sapply(purchases, function(x) if (is.factor(x)) nlevels(x) else NA_integer_)

# write.csv(purchases, "~/Documents/DoJ/guns/purchases.csv", row.numbers = FALSE)

dealership <- read.csv("~/Downloads/temp/Raw Gun Data/dealership/dealership_final.csv")
dealership <- dealership[,-1L]

all(dealerships$business_name[match(purchases$dealer_id, dealerships$dealer_id)] == purchases$business_name, na.rm = TRUE)

person <- read.csv("~/Downloads/temp/Raw Gun Data/person/person_final.csv")

person <- person[-c(1140622, which(person$birth_date == "PAUL")),]

integerColumns <- c("person_key", "birth_date")
factorColumns  <- c("id_type", "id_qualifier", "id_number", "last_name", "sdx_last_name", "first_name",
  "sdx_first_name", "descent_code", "gender", "middle_name", "suffix_name", "place_of_birth_code",
  "occupation_desc", "hair_color_code", "eye_color_code", "bfsc_number", "arn_number",
  "country_of_citizenship_code", "address_type", "street_address", "city", "county",
  "address_type_description")


for (colName in integerColumns) person[[colName]] <- factorToInteger(person[[colName]], "")
person[["height_feet"]] <- factorToInteger(person[["height_feet"]], c("", "@"))
# person[["height_inches"]] <- factorToInteger(person[["height_inches"]], c("", "@@"))
person$height_inches[person$height_inches %in% c("", "@@")] <- NA
person$height_inches <- droplevels(person$height_inches)
person[["weight_pounds"]] <- factorToInteger(person[["weight_pounds"]], c("", "@@@"))

invisible(gc(FALSE))

for (colName in factorColumns) person[[colName]] <- cleanFactor(person[[colName]], "")

invisible(gc(FALSE))


weapon <- read.csv("~/Downloads/temp/Raw Gun Data/weapon/weapon_final.csv")

weapon <- subset(weapon, !is.na(weapon_key))

integerColumns <- c("primary_caliber")
# doubleColumns <- "barrel_length"

factorColumns <- c("gun_make_code", "gun_type", "gun_category_code", "serial_number",
  "model_name", "ori_number", "frame_only_flag", "barrel_length", "barrel_length_units",
  "gun_color_code", "country_of_origin", "other_serial_number", "other_handgun_comment",
  "gun_material", "gun_make_code_description", "gun_type_description",
  "gun_category_code_description", "gun_color_code_description", "country_of_origin_description")

for (colName in integerColumns) weapon[[colName]] <- factorToInteger(weapon[[colName]], "")

for (colName in factorColumns) weapon[[colName]] <- cleanFactor(weapon[[colName]], "")


person.pii <- person
person$street_address <- NULL
person$address_type <- NULL
person$address_type_description <- NULL
person$last_name <- NULL
person$sdx_last_name <- NULL
person$first_name <- NULL
person$sdx_first_name <- NULL
person$middle_name <- NULL
person$suffix_name <- NULL
person$occupation_code <- NULL
person$occupation_desc <- NULL

person$birth_year <- person$birth_date %/% 10000L
person$birth_date <- NULL
person$zip_code <- NULL
person$city <- NULL

## cut down weapon table; unique ID, not serial number
## decrease weapon type info down to broad strokes
## anonymize gun manufacturers
## dealership, no email
## anonymize dealership info, address and name

uniquePersonKeys <- unique(person$person_key)

new_person_key <- sample(length(uniquePersonKeys))

person$new_person_key <- new_person_key[match(person$person_key, uniquePersonKeys)]

new_purchaser_person_key <- new_person_key[match(purchase$purchaser_person_key, uniquePersonKeys)]
new_seller_person_key    <- new_person_key[match(purchase$seller_person_key, uniquePersonKeys)]

purchase$purchaser_person_key <- new_purchaser_person_key
purchase$seller_person_key    <- new_seller_person_key

save(purchase, dealership, person, weapon, file = "~/Documents/DoJ/guns/guns.Rdata")


write.csv(purchase, file = "~/Documents/DoJ/guns/purchase.csv", row.names = FALSE)
write.csv(dealership, file = "~/Documents/DoJ/guns/dealership.csv", row.names = FALSE)
write.csv(person, file = "~/Documents/DoJ/guns/person.csv", row.names = FALSE)
write.csv(weapon, file = "~/Documents/DoJ/guns/weapon.csv", row.names = FALSE)
