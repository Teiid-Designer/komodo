#
# This command file creates a set of access patterns in myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-access-pattern myAccessPattern1
add-access-pattern myAccessPattern2
add-access-pattern MyAccessPattern3