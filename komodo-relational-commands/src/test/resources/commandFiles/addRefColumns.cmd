#
# This command file creates a set of reference columns in myVDB1/myModel1/myTable1/myForeignKey1
# 
play ./src/test/resources/commandFiles/addForeignKeys.cmd
cd ..
cd myTable2
add-column myColumn1
add-column myColumn2
add-column MyColumn3
cd ..
cd myTable1
cd myForeignKey1
add-ref-column  /workspace/myVDB1/myModel1/myTable2/myColumn1
add-ref-column  /workspace/myVDB1/myModel1/myTable2/myColumn2
add-ref-column  /workspace/myVDB1/myModel1/myTable2/MyColumn3