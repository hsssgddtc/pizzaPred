Sys.setlocale(locale="US")
createDataSet <- function(){
  dataSet <<- data.frame(c(1,1,1,0,0),c(1,1,0,1,1),c("yes","yes","no","no","no"),stringsAsFactors=FALSE);
  #dataSet <<- list(c(1,1,'yes'),c(1,1,'yes'),c(1,0,'no'),c(0,1,'no'),c(0,1,'no'))
  label <<- c("no surfacing","flippers");
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
      reducedFeatVec = dataSet[i,c(-axis)]
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
  return(names(subset(tableClassList,tableClassList==max(tableClassList),)))
}

createTree <- function(dataSet,label){
  #print(dataSet)
  classList <- dataSet[,ncol(dataSet)]
  if(dim(table(classList))==1){
    return(unique(as.vector(classList)))
  }
  if(ncol(dataSet)==1){
    return(majorityCnt(classList))
  }
  bestFeat = chooseBestFeatureToSplit(dataSet)
  bestFeatLabel = label[bestFeat]
  myTree <- list()
  #order = length(myTree)-length(label)+1
  #names(myTree)[order] <<- bestFeatLabel
  featValues = dataSet[,bestFeat]
  uniqueVals = unique(featValues)
  #myTree[[bestFeatLabel]] <<- vector(mode="list",length=length(uniqueVals))
  #names(myTree[[bestFeatLabel]]) <<- uniqueVals
  for(i in 1:length(uniqueVals)){
    #myTree<<- append(myTree,createTree(splitDataSet(dataSet,bestFeat,uniqueVals[i]),label))
    value = as.character(uniqueVals[i])
    myTree[[bestFeatLabel]][[value]] <- createTree(splitDataSet(dataSet,bestFeat,value),label[-bestFeat])
    #while value ==0 , the index will be a problem
  }
  return(myTree)
}

classify <- function(inputTree,featLabels,testVec){
  #print(testVec)
  firstStr = names(inputTree)
  #print(firstStr)
  #print(featLabels)
  featIndex = match(firstStr,featLabels)
  #print(featIndex)
  for(i in 1:length(names(inputTree[[firstStr]]))){
    #print(names(inputTree[[firstStr]])[[i]])
   # print(testVec[featIndex])
    if(names(inputTree[[firstStr]])[[i]]==testVec[featIndex]){
      if(class(inputTree[[firstStr]][[i]])=="list"){
        classLabel=classify(inputTree[[firstStr]][[i]],featLabels,testVec)
      }
      else{
        classLabel=inputTree[[firstStr]][[i]]
      }
    }
  }
  return(classLabel)
}

#Test Part
createDataSet()
calcShannonEnt(dataSet)
splitDataSet(dataSet,1,0)
chooseBestFeatureToSplit(dataSet)
classList = dataSet[,ncol(dataSet)]
majorityCnt(classList)

myTree = createTree(dataSet,label)

classify(myTree,label,c(0,1))
classify(myTree,label,c(1,0))
classify(myTree,label,c(1,1))

