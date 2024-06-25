#look at transfer_entropy.Rmd for final 

library(RTransferEntropy)
library(future)

cur <- comb_df %>% filter(id == "mi100", period == "Post5")

future::plan(multisession)

te_df <- data_frame()
skipped_te <- data_frame()
# 
for(id_str in ids %>% pull(id)) {
  for(period_str in periods_str){
    #  for(period_str in periods_str){
    # for(id_str in c("mi021", "mi053")) {
    #     for(period_str in c("Post60", "PrepTask", "Post5")){
    
    cur = comb_df %>% filter(id == id_str, period == period_str)
    
    print(id_str)
    print(period_str)
    
    if (dim(cur)[1] == 0) {
      
      skipped_instance <- c(skipped_id = id_str,
                            skipped_period = period_str
                            reason = "null")
      skipped_te <- rbind(skipped_te, skipped_instance)
      
      next
      } # skip if empty
    # if (period_str == "PrepTask") {next} # skip if PrepTask
    # if (length(cur$rate_hr_diff) < 35) {next} # skip if less than 35 intervals
    
    # TE1hb = transfer_entropy(cur$rate_hr_diff, cur$pval_bp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    # TE2hr = transfer_entropy(cur$rate_hr_diff, cur$rate_rsp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    # TE3br = transfer_entropy(cur$pval_bp_diff, cur$rate_rsp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    
    # Calculate each TE and put in an object
    
    TE1hb <- tryCatch({
      transfer_entropy(cur$rate_hr_diff, cur$pval_bp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    }, error = function(e) {
      message(paste("Error TE1 HR to BP in dataset", id_str, period_str, ": skipping this instance"))
      
      skipped_instance <- c(skipped_id = id_str,
                            skipped_period = period_str
                            reason = "function_failure")
      skipped_te <- rbind(skipped_te, skipped_instance)
      
      return(NULL)  # Return NULL or any other placeholder
    })
    
    if (!is.null(TE1hb)) {
      TE1hb <- TE1hb
    }
    
    TE2hr <- tryCatch({
      transfer_entropy(cur$rate_hr_diff, cur$pval_bp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    }, error = function(e) {
      message(paste("Error TE1 HR to RR in dataset", id_str, period_str, ": skipping this instance"))
      
      skipped_instance <- c(skipped_id = id_str,
                            skipped_period = period_str
                            reason = "function_failure")
      skipped_te <- rbind(skipped_te, skipped_instance)
      
      return(NULL)  # Return NULL or any other placeholder
    })
    
    if (!is.null(TE2hr)) {
      TE2hr <- TE2hr
    }
    
    TE3br <- tryCatch({
      transfer_entropy(cur$rate_hr_diff, cur$pval_bp_diff, type = 'quantiles',  lx = 3, ly = 3, burn = 20)
    }, error = function(e) {
      message(paste("Error TE1 BP to RR in dataset", id_str, period_str, ": skipping this instance"))
      
      skipped_instance <- c(skipped_id = id_str,
                            skipped_period = period_str,
                            reason = "function_failure")
      skipped_te <- rbind(skipped_te, skipped_instance)
      
      return(NULL)  # Return NULL or any other placeholder
    })
    
    if (!is.null(TE3br)) {
      TE3br <- TE3br
    }
    
    # add all directions to TE dataframe
    te_row <- c(id = id_str,
                period = period_str,
                condition = condition,
                te_hr_bp = TE1hb$coef[1, 1],
                te_bp_hr = TE1hb$coef[2, 1],
                te_hr_rr = TE2hr$coef[1, 1],
                te_rr_hr = TE2hr$coef[2, 1],
                te_bp_rr = TE3br$coef[1, 1],
                te_rr_bp = TE3br$coef[2, 1],
                te_hr_bp_p = TE1hb$coef[1, 4],
                te_bp_hr_p = TE1hb$coef[2, 4],
                te_hr_rr_p = TE2hr$coef[1, 4],
                te_rr_hr_p = TE2hr$coef[2, 4],
                te_bp_rr_p = TE3br$coef[1, 4],
                te_rr_bp_p = TE3br$coef[2, 4]
    )
    
    te_df <- rbind(te_df, te_row)
    
  }
}

colnames(te_df) <- names(te_row)

plan(sequential)
