Sys.setlocale(locale="US")
createDataSet <- function(){
  dataSet <<- data.frame(c(1,1,1,0,0),c(1,1,0,1,1),c("yes","yes","no","no","no"),stringsAsFactors=FALSE);
  #dataSet <<- list(c(1,1,'yes'),c(1,1,'yes'),c(1,0,'no'),c(0,1,'no'),c(0,1,'no'))
  label <<- c("no surfacting","flippers");
  names(dataSet) <<- c("no surfactin","flippers","fish")
}

calcShannonEnt <- function(dataSet){
  numEntries <- nrow(dataSet)
  labelCounts <- list()
  for(i in 1:numEntries){
    currentLabel <- dataSet[i,ncol(dataSet)]
    if(!(currentLabel %in% names(labelCounts))){
        labelCounts[[currentLabel]] = 0;
    }
    labelCounts[[currentLabel]] = labelCounts[[currentLabel]] + 1;
  }
  shannonEnt = 0
  for(i in 1:length(labelCounts)){
    prob = labelCounts[[i]]/numEntries;
    shannonEnt = shannonEnt - prob*log2(prob)
  }
  return(shannonEnt)
}

splitDataSet <- function(dataSet,axis,value){
  retDataSet = data.frame();
  for(i in 1:nrow(dataSet)){
    if(dataSet[i,axis]==value){
      reducedFeatVec = dataSet[i,c(axis,ncol(dataSet))]
      retDataSet <- rbind(retDataSet,reducedFeatVec)
    }
  }
  return(retDataSet)
}

chooseBestFeatureToSplit <- function(dataSet){
  numFeatures = ncol(dataSet)-1
  baseEntropy = calcShannonEnt(dataSet)
  bestInfoGain = 0
  bestFeature = 1
  
  for(i in 1:numFeatures){
    featList = dataSet[,i];
    uniqueVals = unique(featList)
    newEntropy = 0
    for(j in 1:length(uniqueVals)){
      subDataSet = splitDataSet(dataSet,i,uniqueVals[j])
      prob = nrow(subDataSet) / nrow(dataSet)
      newEntropy = newEntropy + prob*calcShannonEnt(subDataSet)
    }
    infoGain = baseEntropy - newEntropy
    if(infoGain > bestInfoGain){
      bestInfoGain = infoGain
      bestFeature = i
    }
  }
  return(bestFeature)
}

majorityCnt <- function(classList){
  tableClassList = table(classList)
  return(names(subset(tableClassList,tableClassList==max(t),)))
}

createTree <- function(dataSet,label){
  classList <- dataSet[,ncol(dataSet)]
  if(dim(table(classList))==1){
    return(unique(classList))
  }
  if(ncol(dataSet)==1){
    return(majorityCnt(classList))
  }
  bestFeat = chooseBestFeatureToSplit(dataSet)
  bestFeatLabel = label[bestFeat]
  myTree=list(label[bestFeat])
  label = label[-bestFeat]
  featValues = dataSet[,bestFeat]
  uniqueVals = unique(featValues)
  for(i in length(uniqueVals)){
    myTree= append(myTree,createTree(splitDataSet(dataSet,bestFeat,uniqueVals[i]),label))
  }
  return(myTree)
}

#Test Part
createDataSet()
calcShannonEnt(dataSet)
splitDataSet(dataSet,1,1)
chooseBestFeatureToSplit(dataSet)
classList = dataSet[,ncol(dataSet)]
majorityCnt(classList)
myTree = createTree(dataSet,label)

myTree=list(label[1])
myTree = append(myTree,c(0,"no"))
myTree = append(myTree,)
t2=list(list(c(0,"no"),c(1,list("flippers",list(c(0,"no"),c(1,"yes")))))) 
myTree = append(t1,t2)
myTree <- list(label[1],list(c(0,"no"),c(1,list("flippers",list(c(0,"no"),c(1,"yes"))))))

