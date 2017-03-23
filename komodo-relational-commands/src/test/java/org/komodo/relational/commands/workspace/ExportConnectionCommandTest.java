/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational.commands.workspace;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.utils.FileUtils;

/**
 * Test Class to test VDB {@link ExportVdbCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class ExportConnectionCommandTest extends AbstractCommandTest {

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-connection jdbcSource",
                                    "cd jdbcSource",
                                    "set-property jndiName java:/jdbcSource",
                                    "set-property driverName oracle",
                                    "set-custom-property myProp1 prop1Value",
                                    "set-custom-property myProp2 prop2Value",
                                    "workspace",
                                    "create-connection raSource false",
                                    "cd raSource",
                                    "set-property jndiName java:/raSource",
                                    "set-property className org.something.classname",
                                    "set-property driverName salesforce",
                                    "set-custom-property myProp1 prop1Value",
                                    "set-custom-property myProp2 prop2Value",
                                    "workspace"};
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldExportJdbcSourceFromConnectionContext() throws Exception {
        // Create the export destination file, ensuring it does not already exist
        File exportDest = createExportFile();

        // The test commands
        final String[] commands = { "commit",
                                    "cd jdbcSource",
                                    "export-connection " + exportDest.getAbsolutePath() };


        testExportJdbcSource( exportDest, commands );
    }

    @Test
    public void shouldExportJdbcSourceFromWorkspaceContext() throws Exception {
        // Create the export destination file, ensuring it does not already exist
        File exportDest = createExportFile();

        // The test commands
        final String[] commands = { "commit",
                                    "export-connection jdbcSource " + exportDest.getAbsolutePath() };


        testExportJdbcSource( exportDest, commands );
    }

    @Test
    public void shouldExportRASourceFromConnectionContext() throws Exception {
        // Create the export destination file, ensuring it does not already exist
        File exportDest = createExportFile();

        // The test commands
        final String[] commands = { "commit",
                                    "cd raSource",
                                    "export-connection " + exportDest.getAbsolutePath() };


        testExportRASource( exportDest, commands );
    }

    @Test
    public void shouldExportRASourceFromWorkspaceContext() throws Exception {
        // Create the export destination file, ensuring it does not already exist
        File exportDest = createExportFile();

        // The test commands
        final String[] commands = { "commit",
                                    "export-connection raSource " + exportDest.getAbsolutePath() };


        testExportRASource( exportDest, commands );
    }

    private void testExportJdbcSource( final File exportDest, final String[] commands ) throws Exception {
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        assertTrue(exportDest.exists());

        // Verify the exported XML content
        String exportedXml = FileUtils.readSafe(exportDest);

        assertTrue( exportedXml.contains("<jdbc-connection name=\"jdbcSource\">") );
        assertTrue( exportedXml.contains("<jndi-name>java:/jdbcSource</jndi-name>") );
        assertTrue( exportedXml.contains("<driver-name>oracle</driver-name>") );
        assertTrue( exportedXml.contains("<property name=\"myProp1\">prop1Value</property>") );
        assertTrue( exportedXml.contains("<property name=\"myProp2\">prop2Value</property>") );
    }

    private void testExportRASource( final File exportDest, final String[] commands ) throws Exception {
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        assertTrue(exportDest.exists());

        // Verify the exported XML content
        String exportedXml = FileUtils.readSafe(exportDest);

        assertTrue( exportedXml.contains("<resource-connection name=\"raSource\">") );
        assertTrue( exportedXml.contains("<jndi-name>java:/raSource</jndi-name>") );
        assertTrue( exportedXml.contains("<driver-name>salesforce</driver-name>") );
        assertTrue( exportedXml.contains("<property name=\"myProp1\">prop1Value</property>") );
        assertTrue( exportedXml.contains("<property name=\"myProp2\">prop2Value</property>") );
        assertTrue( exportedXml.contains("<driver-class>org.something.classname</driver-class>") );
    }

    private File createExportFile() {
        File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt"); //$NON-NLS-1$  //$NON-NLS-2$
        exportDest.deleteOnExit();
        if (exportDest.exists()) exportDest.delete();
        return exportDest;
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
        candidates.add("jdbcSource");
        candidates.add("raSource");
        candidates.add("mySource1");
        candidates.add("mySource2");
        candidates.add("mySource3");

    	setup("commandFiles","addConnections.cmd");
    	assertTabCompletion("export-connection ", candidates);
    }

}
