AccidentsByDistricts <- aggregate(infodata["סהכ תאונות דרכים"], by=list(infodata$מחוז), FUN=sum)
#Summing seekers by district.
colnames(AccidentsByDistricts) <- c("מחוז", "סהכ תאונות דרכים")
#Sorting by percentage
AccidentsByDistricts <- AccidentsByDistricts[order(AccidentsByDistricts["סהכ תאונות דרכים"], decreasing = TRUE),]
AccidentsByDistricts$מחוז <- factor(AccidentsByDistricts$מחוז, levels = AccidentsByDistricts$מחוז)
#Plot
ggplot(AccidentsByDistricts, aes(y = unlist(AccidentsByDistricts["סהכ תאונות דרכים"]), x = unlist(AccidentsByDistricts$מחוז))) +
  geom_bar(position = 'dodge', stat="identity", width=0.8, fill="Red") +
  geom_text(aes(label=paste0(unlist(AccidentsByDistricts["סהכ תאונות דרכים"]))), position=position_dodge(width=0.1), vjust=-0.25) +
  labs(title = "תאונות דרכים לפי מחוז" , x = "" , y = "מספר תאונות הדרכים") +
  theme(axis.text.x = element_text(size=11, angle=0, vjust=1), plot.title = element_text(hjust = 0.5))







NABD <- aggregate(infodata["סהכ תאונות דרכים"]/infodata["מס' תושבים"], by=list(infodata$מחוז), FUN=sum)
#Summing seekers by district.
colnames(NABD) <- c("מחוז", "סך תאונות דרכים מנורמל למס' תושבים")
#Sorting by percentage
NABD <- NABD[order(NABD["סך תאונות דרכים מנורמל למס' תושבים"], decreasing = TRUE),]
NABD$מחוז <- factor(NABD$מחוז, levels = NABD$מחוז)
#Plot
ggplot(NABD, aes(y = unlist(NABD["סך תאונות דרכים מנורמל למס' תושבים"]), x = unlist(NABD$מחוז))) +
  geom_bar(position = 'dodge', stat="identity", width=0.8, fill="301934") +
  geom_text(aes(label=paste0(round(unlist(NABD["סך תאונות דרכים מנורמל למס' תושבים"]),2),"%")), position=position_dodge(width=0.1), vjust=-0.25) +
  labs(title = "שיעור תאונות הדרכים ביחס לאוכלוסיה לפי מחוז" , x = "" , y = "תאונות הדרכים %") +
  theme(axis.text.x = element_text(size=11, angle=0, vjust=1), plot.title = element_text(hjust = 0.5)) +
  ylim(0, 2.5)
