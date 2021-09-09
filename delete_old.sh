string=$CODEBUILD_BUILD_ID;
delimiter=":"
declare -a array=($(echo $string | tr "$delimiter" " "))
project=${array[0]}
if [ "$project" == "StagingElm" ]
then
  ELM_KEY=$(aws s3api list-objects-v2  --bucket "sociomata-staging-static" --prefix "js/elm" --query Contents[0].Key)
  aws s3api delete-objects --bucket "sociomata-staging-static" --delete Objects=[{Key=$ELM_KEY}]
else
  ELM_KEY=$(aws s3api list-objects-v2  --bucket "sociomata-staging-static" --prefix "js/elm" --query Contents[0].Key)
  aws s3api delete-objects --bucket "sociomata-staging-static" --delete Objects=[{Key=$ELM_KEY}]
fi
