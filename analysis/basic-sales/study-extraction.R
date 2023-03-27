rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(squareupr)
library(dplyr)
library(tidyverse)
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# for asserting conditions meet expected patterns.
requireNamespace("scales"   )# formatting

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/basic-sales/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_data_input <- "./data-private/derived/..."
# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# do not commit the line below

# do not commit the line above 

# ---- inspect-data ------------------------------------------------------------
sq_list_locations() %>% as_tibble() %>% t()
our_locations <- sq_list_locations()
lapply(our_locations, glimpse)
our_locations$address
our_locations %>% 
  select(id, name, address, timezone, 
         capabilities, status, created_at) 

# list all transactions 
# by default, if a date is provided with no time, then the time component is set to midnight
ds_tran <- 
  sq_list_transactions(
    location = our_locations$id[1] # we only have 1 location
    # ,begin_time = as.Date('2015-10-17') # account start date
    # ,begin_time = as.Date('2022-03-21') #
    # ,end_time = as.Date('2023-03-21')
    
    ,begin_time = "2022-01-01"
    ,end_time   = "2022-12-31"
  )
ds_tran %>% head()
# because we have a single location, this table may not be as useful
# we will extract sales from the `tenders` comnponent


# ---- tweak-data --------------------------------------------------------------
# how to view the contnets of the `tender` field
ds_tran$tenders[[1]]

d0 <- ds_tran %>% slice(1:2)#
d0 %>% purrr::transpose() %>% View()


# create a function that will extract fields from `tenders`
extract_from_transactions <- function(x){
  if(!is.null(x$tender)){
    tibble(
       transaction_id = sq_null_to_na(x$tender[[1]]$transaction_id)
      ,customer_id = sq_null_to_na(x$tender[[1]]$customer_id)
      ,payment_id = sq_null_to_na(x$tender[[1]]$id)
      ,money_spent = sq_null_to_na(x$tender[[1]]$amount_money$amount)
      ,date_of_sale = sq_null_to_na(x$tender[[1]]$created_at)
      ,card_type = sq_null_to_na(x$tender[[1]]$card_details$card$card_brand)
    )  
  } else {
    tibble(customer_id = NA_character_, 
           money_spent = NA_integer_)
  }
}

# for testing
d0 %>% 
  purrr::transpose() %>%
  # pull out just the information we want from each transaction
  purrr::map_df(extract_from_transactions) 

# to verify what id to use
ds_tran %>% filter(id == "uqOZy9LOAh0tweajE3Nr8r4eV") # transaction_id from `tenders`

# extract information from `tenders` component
ds_tran_cust <- 
  ds_tran %>% 
  purrr::transpose() %>%
  # pull out just the information we want from each transaction
  purrr::map_df(extract_from_transactions) 


# explore other data you can get out
ds_cust <-  sq_list_customers()
ds_cust
ds_cust$segment_ids[[1]]  



# sq_list_categories(location = our_locations$id[1]) # NOT_FOUND
# sq_list_customers()# works
# sq_list_discounts(location = our_locations$id[1])# NOT_FOUND
# sq_list_fees(location = our_locations$id[1])# NOT_FOUND
# sq_list_generic_v1(location = our_locations$id[1],) # not sure
# sq_list_items(location = our_locations$id[1]) # NOT_FOUND
# sq_list_locations() # works
# sq_list_modifiers(location = our_locations$id[1])# NOT_FOUND
# sq_list_payments(location = our_locations$id[1],begin_time = "2022-01-01","2022-12-31") # works
# sq_list_transactions() # works
# sq_list_items(location = our_locations$id[1]) # NOT_FOUND

# ---- table-1 -----------------------------------------------------------------
# looking inside sq_list_payments()
ds_pay <- 
  sq_list_payments(location = our_locations$id[1],begin_time = "2022-01-01","2022-12-31")

ds_pay %>% slice(1) %>% purrr::transpose()
ds_pay$tender[[1]]
ds_pay$itemizations[[1]]

# ---- graph-1 -----------------------------------------------------------------
# extract gross, net, tax from payments
ds_pay <- 
  sq_list_payments(
    location = our_locations$id[1]
    ,begin_time = "2022-01-01"
    ,end_time   = "2022-12-31"
    )
d0 <- ds_pay %>% slice(1:2)
d0 %>% purrr::transpose() %>% View()
  
extract_from_payments <- function(x){
  # if(!is.null(x$tender)){
    tibble(
      payment_id = sq_null_to_na(x$id)
      ,collected_amount = sq_null_to_na(x$total_collected_money$amount)
      ,net_amount = sq_null_to_na(x$net_total_money$amount)
      ,gross_amount = sq_null_to_na(x$gross_sales_money$amount)
      # ,inclusive_tax = sq_null_to_na(x$inclusive_tax_money$amount)
      # ,additive_tax = sq_null_to_na(x$additive_tax_money$amount)
      ,tax_total = sq_null_to_na(x$tax_money$amount)
      ,processing_fee = sq_null_to_na(x$processing_fee_money$amount)
      # ,gross_sales_amount = sq_null_to_na(x$gross_sales_money$amount)

    )  
  # } else {
  #   tibble(payment_id = NA_character_, 
  #          money_spent = NA_integer_)
  # }
}

# extract information from `tenders` component
d2 <- 
  d0 %>% 
  purrr::transpose() %>%
  # pull out just the information we want from each transaction
  purrr::map_df(extract_from_payments) %>% 
  mutate(
    # test = (collected_amount == net_amount + processing_fee)
    # test = (collected_amount == gross_amount + tax_total)
    # test = (collected_amount == gross_amount +  tax_total)
    
  )
d2

ds_payment <- 
  ds_pay %>% 
  purrr::transpose() %>%
  purrr::map_df(extract_from_payments)

ds_sales <- 
  ds_tran_cust %>% 
  full_join(ds_payment, by = "payment_id")


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd" # connect with Rmd for publishing
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
