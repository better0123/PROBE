extract_features <- function(voxelCube, feature.family = c("stat", "morph", "glcm", "rlm", "szm" ), n_grey = n_grey){

  if( length(dim(voxelCube)) == 2 ) {
    n.imgObj <- array(0, dim = c(dim(voxelCube),3) )
    n.imgObj[,,2] <- voxelCube
    voxelCube <- n.imgObj
  }
  
  def <- c()
  if("stat" %in% feature.family) {
    cat("computing stat features...\n")
    stat.df <- statisticalFeatures(voxelCube)
    def <- c(def,stat.df)
  }
  
  if("morph" %in% feature.family) {
    cat("computing morph features...\n")
    morph.df <- morphologicalFeatures(voxelCube, px = 1, py = 1, pz = 1)
    def <- c(def,morph.df)
  }
  
  if("glcm" %in% feature.family){
    cat("computing glcm features...\n")
    F_cm <-  glcmTexturalFeatures(voxelCube, n_grey=n_grey)
    F_cm <- do.call(data.frame,lapply(F_cm, function(x) replace(x, is.infinite(x),NA)))
    cm.df <- colMeans(F_cm,na.rm = T)
    def <- c(def,cm.df)
  }

  
  if("rlm" %in% feature.family) {
    cat("computing glrm features...\n")
    F_rlm <-glrlmTexturalFeatures(voxelCube, n_grey=n_grey)
    F_rlm <- do.call(data.frame,lapply(F_rlm, function(x) replace(x, is.infinite(x),NA)))
    rlm.df <- colMeans(F_rlm,na.rm = T)
    def <- c(def,rlm.df)
  }
  
  if("szm" %in% feature.family) {
    cat("computing szm features...\n")
    F_szm <- glszmTexturalFeatures(voxelCube, n_grey=n_grey)
    F_szm <- do.call(data.frame,lapply(F_szm, function(x) replace(x, is.infinite(x),NA)))
    szm.df <- colMeans(F_szm,na.rm = T)
    def <- c(def,szm.df)
  }

  return(def)
}
  