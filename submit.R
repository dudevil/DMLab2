source(paste0(getwd(), '/models.R'), echo = F)

submission1 <- data.frame(matrix(prediction1, ncol=12))
submission1 <- cbind(1:nrow(submission1), submission1)
names(submission1) <- c('id', sapply(1:12, function(x) paste0('Outcome_M', x)))
write.csv(submission1, paste0(getwd(), '/data/submission1.csv'), row.names=F)
