#
# This command file creates a set of data roles in myVDB1
# 
play ./src/test/resources/commandFiles/addVDBs.cmd
cd myVDB1
add-data-role myDatarole1
add-data-role myDatarole2
add-data-role MyDatarole3