
BIOKOT <- aggregate(c(100*infodata["סהכ נפגעים"]/infodata["מס' תושבים"], infodata["אשכול פריפריאלי"], infodata["אשכול חברתי כלכלי"]), by=list(infodata$ישוב), FUN=sum)
#Summing seekers by district.
colnames(BIOKOT) <- c("ישוב", "PIK", "אשכול פריפריאלי", "אשכול חברתי כלכלי")

#Sorting by percentage
BIOKOT <- BIOKOT[order(BIOKOT["PIK"], decreasing = TRUE),]
BIOKOT$ישוב <- factor(BIOKOT$ישוב, levels = BIOKOT$ישוב)

#create a sample group - a subtable with every 5th value
df.new = BIOKOT[seq(1, nrow(BIOKOT), 5), ]

#tlabel=paste0(round(BIOKOT$PIK,3),"%")
ggplot(df.new, aes(y = unlist(df.new$PIK), x = unlist(df.new["ישוב"]))) +
  geom_bar(position = 'dodge', stat="identity", width=0.8, fill="navy") +
  geom_text(aes(label=" "), position=position_dodge(width=0.1), vjust=-0.25) +
  labs(title = " אחוז מוערבים בתאונה לפי ישוב\n  (n=39 מדגם מייצג)" , x = "ישוב" , y = "מעורבים מסך אוכלוסיה %") +
  theme(axis.text.x = element_text(size=9, angle=90, vjust=0) , plot.title = element_text(hjust = 0.5))+
  ylim(0,15)
#Enter Vectors
y<-unlist(100*infodata["סהכ נפגעים"]/infodata["מס' תושבים"])
x0<-rep(1,184)
x1<-unlist(infodata["אשכול פריפריאלי"])
x2<-unlist(infodata["אשכול חברתי כלכלי"])

#Calc Matrices
matrix1<-as.matrix(data.frame(x0,x1,x2))
matrix2<-solve(t(matrix1)%*%matrix1)
matrix2
matrix3<-t(matrix1)%*%as.matrix(y)
Bmatrix<-matrix2%*%matrix3
Bmatrix

#ANOVA
matrix1.1<-as.matrix(data.frame(x1,x2))
anovatable<-aov(y~matrix1.1)
summary.aov(anovatable)
lm1<-lm(y~matrix1.1)
summary(lm1)
  