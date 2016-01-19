#
# This command file creates a primary key in myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-primary-key myPrimaryKey