#
# This command file creates a set of pushdown functions in the myVDB1/myModel1
# 
play ./resources/commandFiles/addModels.cmd
cd myModel1
add-pushdown-function myPushdownFunction1
add-pushdown-function myPushdownFunction2
add-pushdown-function MyPushdownFunction3
