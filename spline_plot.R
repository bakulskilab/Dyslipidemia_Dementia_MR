library(voxel)
output_fig_path = '/Users/Mingzhou/Desktop/AD_Grant/Cholesterol/output/prevalent/spline_term/'








cind_normal$sex_factor = as.factor(cind_normal$sex_cat)
spline_adj1 = gam(AD_bin ~ s(HDL) + sex_factor + s(HDL, by = sex_factor) + age + education + living + wave_adj + med_lipid_cat + PC1_5A + PC1_5B + PC1_5C + PC1_5D + PC1_5E, data = cind_normal)
hdl_cind = plotGAM(spline_adj1, smooth.cov = "HDL", groupCovs = "sex_factor") + # plot customization goes here 
            geom_rug(data = cind_normal[cind_normal$sex_cat == "Female", ], aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.5, colour = '#F8766D', sides = 'b') +
            geom_rug(data = cind_normal[cind_normal$sex_cat == "Male", ],  aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.1, colour = '#00BFC4', sides = 't') +
            labs(x = "High Density Lipoprotein Cholesterol concentration (mg/dL)",
                 y = "Predicted cognitive status (CIND = 1 vs. Normal = 0)",
                 title = "Non-linear association between HDL and cognitive status (stratified by sex group)") +
            coord_cartesian(ylim = c(0, 0.3)) +
            scale_x_continuous(breaks = c(20, 40, 50, 60, 80, 100, 120)) + 
            geom_vline(xintercept = 40, colour = '#F8766D', linetype = 2) +
            geom_vline(xintercept = 50, colour = '#00BFC4', linetype = 2) +
            theme_classic(base_family = 'serif',
                          base_size = 12)
ggsave(plot = hdl_cind, path = output_fig_path, filename = "hdl_cind.png", width = 6, height = 4)

dementia_normal$sex_factor = as.factor(dementia_normal$sex_cat)
spline_adj2 = gam(AD_bin ~ s(HDL) + sex_factor + s(HDL, by = sex_factor) + age + education + living + wave_adj + med_lipid_cat + PC1_5A + PC1_5B + PC1_5C + PC1_5D + PC1_5E, data = dementia_normal)
hdl_dem = plotGAM(spline_adj2, smooth.cov = "HDL", groupCovs = "sex_factor") + # plot customization goes here 
            geom_rug(data = dementia_normal[dementia_normal$sex_cat == "Female", ], aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.5, colour = '#F8766D', sides = 'b') +
            geom_rug(data = dementia_normal[dementia_normal$sex_cat == "Male", ],  aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.1, colour = '#00BFC4', sides = 't') +
            labs(x = "High Density Lipoprotein Cholesterol concentration (mg/dL)",
                 y = "Predicted cognitive status (Dementia = 1 vs. Normal = 0)",
                 title = "Non-linear association between HDL and cognitive status (stratified by sex group)") +
            coord_cartesian(ylim = c(0, 0.1)) +
            scale_x_continuous(breaks = c(20, 40, 50, 60, 80, 100, 120)) + 
            geom_vline(xintercept = 40, colour = '#F8766D', linetype = 2) +
            geom_vline(xintercept = 50, colour = '#00BFC4', linetype = 2) +
            theme_classic(base_family = 'serif',
                          base_size = 12)
ggsave(plot = hdl_dem, path = output_fig_path, filename = "hdl_dem.png", width = 6, height = 4)


spline_adj3 = gam(AD_bin ~ s(HDL) + sex_factor + age + education + living + wave_adj + med_lipid_cat + PC1_5A + PC1_5B + PC1_5C + PC1_5D + PC1_5E, data = cind_normal)
hdl_cind = plotGAM(spline_adj3, smooth.cov = "HDL") + # plot customization goes here 
  geom_rug(data = cind_normal[cind_normal$sex_cat == "Female", ], aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.5, colour = '#F8766D', sides = 'b') +
  geom_rug(data = cind_normal[cind_normal$sex_cat == "Male", ],  aes_string(y = "AD_bin", x = "HDL" ), alpha = 0.1, colour = '#00BFC4', sides = 't') +
  labs(x = "High Density Lipoprotein Cholesterol concentration (mg/dL)",
       y = "Predicted cognitive status (Dementia = 1 vs. Normal = 0)",
       title = "Non-linear association between HDL and cognitive status") +
  coord_cartesian(ylim = c(0, 0.3)) +
  scale_x_continuous(breaks = c(20, 40, 50, 60, 80, 100, 120)) + 
  geom_vline(xintercept = 40, colour = '#F8766D', linetype = 2) +
  geom_vline(xintercept = 50, colour = '#00BFC4', linetype = 2) +
  theme_classic(base_family = 'serif',
                base_size = 12)
ggsave(plot = hdl_dem, path = output_fig_path, filename = "hdl_dem.png", width = 6, height = 4)

#=================================

spline_adj3 = gam(AD_bin ~ s(TC) + sex_factor + s(TC, by = sex_factor) + age + education + living + wave_adj + med_lipid_cat + PC1_5A + PC1_5B + PC1_5C + PC1_5D + PC1_5E, data = cind_normal)
plotGAM(spline_adj3, smooth.cov = "TC", groupCovs = "sex_factor") + # plot customization goes here 
  geom_rug(data = cind_normal[cind_normal$sex_cat == "Female", ], aes_string(y = "AD_bin", x = "TC" ), alpha = 0.5, colour = '#F8766D', sides = 'b') +
  geom_rug(data = cind_normal[cind_normal$sex_cat == "Male", ],  aes_string(y = "AD_bin", x = "TC" ), alpha = 0.1, colour = '#00BFC4', sides = 't') +
  labs(x = "Total Cholesterol concentration (mg/dL)",
       y = "Predicted cognitive status (Dementia = 1 vs. Normal = 0)",
       title = "Non-linear association between TC and cognitive status (stratified by sex group)") +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_continuous(breaks = c(80, 120, 160, 200, 240, 280, 320, 360, 400, 440, 480)) + 
  geom_vline(xintercept = 240, colour = '#F8766D', linetype = 2) +
  geom_vline(xintercept = 240, colour = '#00BFC4', linetype = 2) +
  theme_classic(base_family = 'serif',
                base_size = 12)

spline_adj4 = gam(AD_bin ~ s(TC) + sex_factor + s(TC, by = sex_factor) + age + education + living + wave_adj + med_lipid_cat + PC1_5A + PC1_5B + PC1_5C + PC1_5D + PC1_5E, data = dementia_normal)
plotGAM(spline_adj4, smooth.cov = "TC") + # plot customization goes here 
  geom_rug(data = dementia_normal[dementia_normal$sex_cat == "Female", ], aes_string(y = "AD_bin", x = "TC" ), alpha = 0.5, colour = '#F8766D', sides = 'b') +
  geom_rug(data = dementia_normal[dementia_normal$sex_cat == "Male", ],  aes_string(y = "AD_bin", x = "TC" ), alpha = 0.1, colour = '#00BFC4', sides = 't') +
  labs(x = "Total Cholesterol concentration (mg/dL)",
       y = "Predicted cognitive status (Dementia = 1 vs. Normal = 0)",
       title = "Non-linear association between TC and cognitive status (stratified by sex group)") +
  coord_cartesian(ylim = c(0, 0.1)) +
  scale_x_continuous(breaks = c(20, 40, 50, 60, 80, 100, 120)) + 
  geom_vline(xintercept = 40, colour = '#F8766D', linetype = 2) +
  geom_vline(xintercept = 50, colour = '#00BFC4', linetype = 2) +
  theme_classic(base_family = 'serif',
                base_size = 12)

