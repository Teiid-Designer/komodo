#
# This command file creates a set of indexes patterns in myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-index myIndex1
add-index myIndex2
add-index MyIndex3