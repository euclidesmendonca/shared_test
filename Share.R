# Testing Shareable options
summary(cars)



ancova_scanner <- function(group,n_group, outcomes, covars=NULL,data,print.model=NULL){
    require(dplyr)
    require(lsmeans)
    require('effects')
    require('emmeans')
    require('multcomp')
    
    
    #Results 
    main_results <- matrix(NA,nrow = length(outcomes),ncol =  n_group + length(covars)+5) %>% as.data.frame()
    colnames(main_results) <- c("iID", "Outcome", paste("Group", 1:n_group),"F","p_value", covars, "R2")
    
    #Preparing data
    
    lm.formula <- paste("outcome.y ~ predictor.x + ",
                        paste(covars, collapse = "+"))
    
    iID <- 0
    for(i in outcomes){
        
        
        data$outcome.y <- data[,i] 
        data[,"predictor.x"] <- data[,group]
        
        lm.1 <- lm(formula(lm.formula), data)
        
        
        iID <- iID+1
        main_results$iID     [iID] <- iID
        main_results$Outcome [iID] <- i
        
        main_results$R2 [iID] <- summary(lm.1)$r.squared %>% round(2)
        main_results$F [iID] <- anova(lm.1)$F[1] %>% round(2)
        main_results$p_value [iID] <- anova(lm.1)$Pr[1] %>% round(3)
        main_results[iID,3:(n_group+2)]  <- paste0(as.data.frame(lsmeans(lm.1, "predictor.x"))[,2] %>% round(2),
                                                   " (",as.data.frame(lsmeans(lm.1, "predictor.x"))$SE %>% round(2),")")
        
        main_results[iID,(n_group+5): (n_group+5+(length(covars)-1)) ]  <- paste0( 
            summary(lm.1)$coef[(n_group+1):(n_group+length(covars)),1] %>% round(3),
            " (",
            summary(lm.1)$coef[(n_group+1):(n_group+length(covars)),4] %>% round(3),")")
        
        
        
        print(i); print( summary (glht(lm.1, linfct=mcp(predictor.x='Tukey'))))
    }
    
    main_results <<- main_results
    
}

#test
