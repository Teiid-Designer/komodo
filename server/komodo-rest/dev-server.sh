
MVN_OPTIONS="${MVN_OPTIONS} -P tomcat-test-server"
MVN_OPTIONS="${MVN_OPTIONS} -DskipTests" # Skip tests
MVN_OPTIONS="${MVN_OPTIONS} -Dlog4j.configuration=file:./conf/log4j.properties" # Use logging configuration file
#MVN_OPTIONS="${MVN_OPTIONS} -e -X"

# Run tomcat using the exploded war
mvn clean ${MVN_OPTIONS} tomcat7:run-war
