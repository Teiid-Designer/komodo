#
# This command file creates a set of resultset columns in the myVDB1/myModel1/myPushdownFunction1/resultSet
# 
play ./src/test/resources/commandFiles/addPushdownFunctions.cmd
cd myPushdownFunction1
set-result-set TabularResultSet
cd resultSet
add-column myColumn1
add-column myColumn2
add-column MyColumn3

