#
# This command file creates a set of translators in myVDB1
# 
play ./src/test/resources/commandFiles/addVDBs.cmd
cd myVDB1
add-translator myTranslator1 tType
add-translator myTranslator2 tType
add-translator MyTranslator3 tType