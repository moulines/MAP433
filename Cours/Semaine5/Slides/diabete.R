pima <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data ", header =F , sep =",")
colnames ( pima ) <- c(" npreg ", " glucose ", "bp", " triceps ", " insulin ", "bmi", "diabetes ", "age", " class ")
