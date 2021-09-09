string=$CODEBUILD_BUILD_ID;
delimiter=":"
declare -a array=($(echo $string | tr "$delimiter" " "))
project=${array[0]}
if [ "$project" == "StagingElm" ]
then
  aws codepipeline start-pipeline-execution --name SociomataStagingStatic
else
  aws codepipeline start-pipeline-execution --name ProdStatic
fi
