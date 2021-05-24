europ_new = 
  europ %>% 
  mutate(wave_adj_num = (as.numeric(wave_adj)-2006)/2)

var_names <- c('HDL_clinic', 'AD', 'sex', 'wave_adj_num', 'med_lipid', 'HTN', 'diabetes', 'stroke', 'smoke', 'drink', 'APOE2010_bin', 
               'age', 'education', 'BMI', 'im_recall', 'dl_recall', 'serial7', 'backwc', 'vocab', 'ms_total', 'cog_total')
explanary_names <- c('HDL clinical level', 'Cognitive status', 'Sex', 'Measurement wave', 'Lipid-lowering medication', 'Hypertension', 'Diabetes', 'Stroke', 'Smoking status', 'Drinking status', 'APOE4 allele carrier', 
                     'Age', 'Years of education', 'BMI', 'Immediate word recall', 'Delayed word recall', 'Serial 7 subtraction', 'Backwards count from 20', 'Vocabulary', 'Mental status', 'Total cognition')

model <- lapply(var_names, function(x) {
  lm(substitute(PGS_HDL ~ i, list(i = as.name(x))), data = europ_new)
})


build_coef_table <- function(var_names, model) {
  
  summary_results <- lapply(model, summary)
  
  # Make to a full table
  coef_table = array(NA, dim = c(length(var_names), 6))
  
  for (i in 1:length(var_names)) {
    
    coeff = summary_results[[i]]$coefficients[2]
    # stderr = summary_results[[i]]$coefficients[4]
    confinterval = confint(model[[i]], level = 0.95)
    lower_CI = confinterval[2]
    upper_CI = confinterval[4]
    CI_95 = paste0(sprintf('%.2f', coeff), ' (', sprintf('%.2f',lower_CI), ', ', sprintf('%.2f',upper_CI), ')')
    
    coef_table[i,1] = var_names[i]
    coef_table[i,2] = length(summary_results[[i]]$residuals)
    coef_table[i,3] = coeff
    coef_table[i,4] = lower_CI
    coef_table[i,5] = upper_CI
    coef_table[i,6] = CI_95
    
    i = i + 1
  }
  
  coef_table_final = as.data.frame(coef_table)
  colnames(coef_table_final) = c('var_name', 'N', 'beta', 'lower_CI', 'upper_CI', 'text')
  
  return(coef_table_final)
}

table1 <- build_coef_table(var_names, model)

forest.data =
  table1 %>% 
  dplyr::select(beta, lower_CI, upper_CI) %>% 
  mutate(sub = c('Categorical', rep(NA, 10), 
                 'Continuous' , rep(NA, 9))) %>% 
  mutate(beta = round(as.numeric(as.character(beta)), 3),
         lower_CI = round(as.numeric(as.character(lower_CI)), 2),
         upper_CI = round(as.numeric(as.character(upper_CI)), 2)) %>% 
  mutate(class = c(rep(2, 11), rep(3, 10)))

library(forestplot)

tabletext <- cbind(
  c("Category", "\n", forest.data$sub),
  c("Exposure", "\n", explanary_names),
  c("Beta coefficient (95% CI)", "\n", as.character(table1$text))
)

pop.cols <- c("black","black","black")


pdf("~/Desktop/HDL_Forest(color)_0708.pdf",width = 10, height = 6)
forestplot(labeltext = tabletext, graph.pos = 1,
           mean = c(NA, NA, forest.data$beta), 
           lower = c(NA, NA, forest.data$lower_CI),
           upper = c(NA, NA, forest.data$upper_CI),
           
           xticks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
           zero = 0,
           
           title = "Figure 3. Associations between covariates and HDL polygenic risk score, Health and Retirement Study, 
           Wave 2006 to 2012, European ancestry sample (n = 8927)",
           xlab = "Effect Size of covariates on HDL polygenic risk score",
           
           txt_gp = fpTxtGp(label = list(gpar(fontface = "bold", cex = 0.8, fontfamily = "serif"),
                                         gpar(cex = 0.8, fontfamily = "serif"),
                                         gpar(cex = 0.8, fontfamily = "serif")),
                            ticks = gpar(cex = 0.6, fontfamily = "serif"),
                            xlab = gpar(cex = 0.8, fontfamily = "serif"),
                            title = gpar(cex = 1, fontfamily = "serif")),
           col = fpColors(text = pop.cols[c(1, 1, forest.data$class)], 
                          box  ="black",
                          lines = "black", 
                          zero ="gray50"),
           
           cex = 0.2, lineheight = "auto", boxsize = 0.25, 
           lwd.ci = 1, ci.vertices = TRUE, ci.vertices.height = 0.15)
dev.off()


#---------------------------------------------- TC -------------------------------------------------------
var_names <- c('TC_clinic_bin', 'AD', 'sex', 'wave_adj_num', 'med_lipid', 'HTN', 'diabetes', 'stroke', 'smoke', 'drink', 'APOE2010_bin', 
               'age', 'education', 'BMI', 'im_recall', 'dl_recall', 'serial7', 'backwc', 'vocab', 'ms_total', 'cog_total')
explanary_names <- c('TC clinical level', 'Cognitive status', 'Sex', 'Measurement wave', 'Lipid-lowering medication', 'Hypertension', 'Diabetes', 'Stroke', 'Smoking status', 'Drinking status', 'APOE4 allele carrier', 
                     'Age', 'Years of education', 'BMI', 'Immediate word recall', 'Delayed word recall', 'Serial 7 subtraction', 'Backwards count from 20', 'Vocabulary', 'Mental status', 'Total cognition')

model <- lapply(var_names, function(x) {
  lm(substitute(PGS_TC ~ i, list(i = as.name(x))), data = europ_new)
})


build_coef_table <- function(var_names, model) {
  
  summary_results <- lapply(model, summary)
  
  # Make to a full table
  coef_table = array(NA, dim = c(length(var_names), 6))
  
  for (i in 1:length(var_names)) {
    
    coeff = summary_results[[i]]$coefficients[2]
    # stderr = summary_results[[i]]$coefficients[4]
    confinterval = confint(model[[i]], level = 0.95)
    lower_CI = confinterval[2]
    upper_CI = confinterval[4]
    CI_95 = paste0(sprintf('%.2f', coeff), ' (', sprintf('%.2f',lower_CI), ', ', sprintf('%.2f',upper_CI), ')')
    
    coef_table[i,1] = var_names[i]
    coef_table[i,2] = length(summary_results[[i]]$residuals)
    coef_table[i,3] = coeff
    coef_table[i,4] = lower_CI
    coef_table[i,5] = upper_CI
    coef_table[i,6] = CI_95
    
    i = i + 1
  }
  
  coef_table_final = as.data.frame(coef_table)
  colnames(coef_table_final) = c('var_name', 'N', 'beta', 'lower_CI', 'upper_CI', 'text')
  
  return(coef_table_final)
}

table1 <- build_coef_table(var_names, model)

forest.data =
  table1 %>% 
  dplyr::select(beta, lower_CI, upper_CI) %>% 
  mutate(sub = c('Categorical', rep(NA, 10), 
                 'Continuous' , rep(NA, 9))) %>% 
  mutate(beta = round(as.numeric(as.character(beta)), 3),
         lower_CI = round(as.numeric(as.character(lower_CI)), 2),
         upper_CI = round(as.numeric(as.character(upper_CI)), 2)) %>% 
  mutate(class = c(rep(2, 11), rep(3, 10)))

library(forestplot)

tabletext <- cbind(
  c("Category", "\n", forest.data$sub),
  c("Exposure", "\n", explanary_names),
  c("Beta coefficient (95% CI)", "\n", as.character(table1$text))
)

pop.cols <- c("black","black","black")


pdf("~/Desktop/TC_Forest(color)_0708.pdf",width = 10, height = 6)
forestplot(labeltext = tabletext, graph.pos = 1,
           mean = c(NA, NA, forest.data$beta), 
           lower = c(NA, NA, forest.data$lower_CI),
           upper = c(NA, NA, forest.data$upper_CI),
           
           xticks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
           zero = 0,
           
           title = "Figure 3. Associations between covariates and TC polygenic risk score, Health and Retirement Study, 
           Wave 2006 to 2012, European ancestry sample (n = 8927)",
           xlab = "Effect Size of covariates on TC polygenic risk score",
           
           txt_gp = fpTxtGp(label = list(gpar(fontface = "bold", cex = 0.8, fontfamily = "serif"),
                                         gpar(cex = 0.8, fontfamily = "serif"),
                                         gpar(cex = 0.8, fontfamily = "serif")),
                            ticks = gpar(cex = 0.6, fontfamily = "serif"),
                            xlab = gpar(cex = 0.8, fontfamily = "serif"),
                            title = gpar(cex = 1, fontfamily = "serif")),
           col = fpColors(text = pop.cols[c(1, 1, forest.data$class)], 
                          box  ="black",
                          lines = "black", 
                          zero ="gray50"),
           
           cex = 0.2, lineheight = "auto", boxsize = 0.25, 
           lwd.ci = 1, ci.vertices = TRUE, ci.vertices.height = 0.15)
dev.off()