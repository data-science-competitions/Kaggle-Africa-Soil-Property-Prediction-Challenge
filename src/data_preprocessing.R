################################################################################
#                              Data Preprocessing                              #
################################################################################


###########################################################
# Encoding the data via Wavelet transform (decomposition) #
###########################################################
lvl = 8 # 2^8 = 256 basis function will be returned for each signal
sure_thresholding = FALSE # TRUE/FALSE to use SURE shrinkage on signal
train.infrared.dwt = WaveletTransform(subset(train.infrared, 
                                             select=c(m4547.38:m599.76)), # 1157 obs. of 2048 (2^11) features
                                      lvl, sure_thresholding)
test.infrared.dwt = WaveletTransform(subset(test.infrared, 
                                            select=c(m4547.38:m599.76)), # 727 obs. of 2048 (2^11) features 
                                     lvl, sure_thresholding)
# Set basis names
colnames(train.infrared.dwt) = paste0("dwt_",(1:2**lvl)-1)
colnames(test.infrared.dwt) = paste0("dwt_",(1:2**lvl)-1)