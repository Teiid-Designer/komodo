#
# This command file creates a set of columns in the myVDB1/myModel1/myTable1/myView1
# 
play ./src/test/resources/commandFiles/addViews.cmd
cd myView1
add-column myColumn1
add-column myColumn2
add-column MyColumn3
