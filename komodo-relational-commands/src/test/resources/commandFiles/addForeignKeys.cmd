#
# This command file creates a set of foreignKeys in the myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-foreign-key myForeignKey1 /workspace/myVDB1/myModel1/myTable2
add-foreign-key myForeignKey2 /workspace/myVDB1/myModel1/myTable2
add-foreign-key MyForeignKey3 /workspace/myVDB1/myModel1/myTable2

