#h2o
h2o.init(nthreads=7)
#agg2 <- agg[!is.bad.numeric(PE,PB,CURRENTRATIO,DE,mkcapdec,PEdecile,pbdecile,negflag,PEc,chg_year),.(PE,PB,CURRENTRATIO,DE,mkcapdec,PEdecile,pbdecile,negflag,PEc,chg_year)]
#write.csv(prepped,"test.csv",row.names=FALSE)
#write.csv(agg,"test2.csv",row.names=FALSE)
test.hex <- h2o.uploadFile(path = "test2.csv", destination_frame = "test.hex")
test.gbm <- h2o.gbm(predictors,target,test.hex,ntrees=500,max_depth=5,learn_rate=0.1,nfolds=5)
prepped[,predicted:=as.data.frame(predict(test.gbm,test.hex))[[1]]]
decileize(c("chg_year","predicted"),prepped)

nb <- h2o.naiveBayes(test.hex,x=predictors,y=target) #naive bayes is categorical only
nn <- h2o.deeplearning(x=pred,y="chg_5_year",training_frame=test.hex)
for(i in 1:100) {
  test.glm <- h2o.glm(predictors,target,test.hex,lambda=.001)
}

#plot univariates
h2o.download_mojo(test.gbm, getwd(), FALSE)

#from C:\users\McFortran\Downloads\h2o-3.10.4.5
#java -cp h2o.jar hex.genmodel.tools.PrintMojo --tree 0 -i filename.zip -o model.gv
#"C:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tpng model.gv -o model.png
