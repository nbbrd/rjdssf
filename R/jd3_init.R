if(!require(rJava)){
  install.packages("rJava")
}
library("rJava")
.jinit()
.jaddClassPath("./Java/demetra-design-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-datatypes-1.0.0-SNAPSHOT.jar")
#.jaddClassPath("./Java/demetra-datatypes-descriptors-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-core-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-modelling-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-modelling-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-ssf-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-sts-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-sts-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-r-1.0.0-SNAPSHOT.jar")

jdr_root_env <- new.env(parent = emptyenv())