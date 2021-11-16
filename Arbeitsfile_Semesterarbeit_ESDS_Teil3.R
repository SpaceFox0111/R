
#Einlesen Beispieldatensatz hsb2 > umbenennen in teildaten23

require(tidyr)

# Datensatz von [wide to long] umwandeln

teildaten23_long <- reshape(teildaten23, varying = c("read", "write", "math", "science", "socst"), 
                            v.names = "score", 
                            timevar = "subj", 
                            times = c("read", "write", "math", "science", "socst"), 
                            new.row.names = 1:1000, 
                            direction = "long")

#Datensatz von [long to wide] umwandeln

teildaten23_wide <- spread(teildaten23_long, subj, score)




