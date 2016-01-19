#
# This command file creates a set of columns in the myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-column myColumn1
add-column myColumn2
add-column MyColumn3
