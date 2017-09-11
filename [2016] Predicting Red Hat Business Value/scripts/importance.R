
#importance <- summary(reg_SGB, order = TRUE, method = relative.influence)


importance <- varImp(model_ONE)

importance <- as.data.frame(importance[1][1])

importance <- importance[order(importance$Overall),,drop = FALSE]
par(mar=c(10,8,4,2))
importance <- barplot(importance[,1], main="Segmentation Criteria Importance", horiz=TRUE, names.arg = row.names(importance),las = 2, col = "deepskyblue", 
                      border = "darkblue", cex.names = 0.7,  xlab = "Relative Importance Score")


importance_REST <- varImp(model_REST)

importance_REST <- as.data.frame(importance[1][1])

importance_REST <- importance[order(importance$Overall),,drop = FALSE]
par(mar=c(10,8,4,2))
importance_REST <- barplot(importance[,1], main="Segmentation Criteria Importance", horiz=TRUE, names.arg = row.names(importance),las = 2, col = "deepskyblue", 
                      border = "darkblue", cex.names = 0.7,  xlab = "Relative Importance Score")
system("say fertig")