library(jpeg)
img <- readJPEG('C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW4\\tiger.jpeg')
red <- img[,,1]
green <- img[,,2]
blue <- img[,,3]
r_pca <- prcomp(red, center = FALSE)
g_pca <- prcomp(green, center = FALSE)
b_pca <- prcomp(blue, center = FALSE)
rgb.pca <- list(r_pca, g_pca, b_pca)

pcs <- c(3, 5, 10, 25, 50, 100, 150, 200, 250, 300, 350)
all_pcs <- ncol(r_pca$rotation)
pcs <- c(pcs, all_pcs)
for (i in pcs) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW4\\img_compressed_', round(i,0), '_components.jpg', sep = ''))
}

for (i in pcs){
  size <- file.size(paste('C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW4\\img_compressed_', round(i,0), '_components.jpg', sep = ''))
  print(paste('Size for',round(i,0),'components',size/1000,'KBs',sep = ' '))
}  
sprintf('Originial file size is %.2f KBs',file.size('C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW4\\tiger.jpeg')/1000)

variance <- c()
compression_ratio <- c()

for (i in pcs) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  
  # calculate variance captured
  total_pixels <- prod(dim(img)[1:2])
  mse <- mean((img - pca.img)^2)
  variance_captured <- 1 - mse/var(img)^2
  variance <- c(variance, variance_captured)
  
  # calculate compression ratio
  compressed_size <- file.size(paste('C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW4\\img_compressed_', round(i,0), '_components.jpg', sep = ''))
  compression_ratio <- c(compression_ratio, (total_pixels*3)/compressed_size)
}

# plot variance captured
plot(pcs, variance, type='b', xlab='Number of principal components', ylab='Variance captured', main='Variance captured vs number of principal components')

# plot compression ratio
plot(pcs, compression_ratio, type='b', xlab='Number of principal components', ylab='Compression ratio', main='Compression ratio vs number of principal components')
