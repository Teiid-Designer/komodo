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
package org.komodo.relational.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetCustomPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class SetCustomOptionCommandTest extends CustomOptionCommandTest {

    @Test
    public void testCustomOptionTable() throws Exception {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        setup("commandFiles","addCustomOptions.cmd");

        String[][] expectedOptions={
        {"myCustomOption1", "myValue1"},
        {"myCustomOption2", "myValue2"},
        {"MyCustomOption3", "myValue3"},
        {"my.custom.option", "myValue1"},
        {"My\"Custom\"Option3", "myValue3"}};


        Vdb vdb=getVDB("myVDB1",wkspMgr);
        assertNotNull(vdb);
        Model[] models=vdb.getModels(getTransaction(), "myModel1");
        assertEquals(1, models.length);
        Table[] tables=models[0].getTables(getTransaction(), "myTable1");
        assertEquals(1, tables.length);
        assertCustomOptions(tables[0],expectedOptions);
    }

    @Test
    public void testCustomOptionColumn() throws Exception {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        setup("commandFiles","addColumns.cmd");
        final String[] commands = {
            "cd myColumn1",
            "set-custom-option myCustomOption myValue" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        String[] expectedOption= {"myCustomOption", "myValue"};

        Vdb vdb=getVDB("myVDB1",wkspMgr);
        assertNotNull(vdb);
        Model[] models=vdb.getModels(getTransaction(), "myModel1");
        assertThat(models.length,is(1));
        Table[] tables=models[0].getTables(getTransaction(), "myTable1");
        assertThat( tables.length,is(1));
        Column[] columns=tables[0].getColumns(getTransaction(), "myColumn1");
        assertThat(columns.length,is(1));
        StatementOption[] options = columns[0].getCustomOptions(getTransaction());
        assertThat(options.length,is(1));

        assertThat("Option key does not match",options[0].getName(getTransaction()),is(expectedOption[0]));
        assertThat("Option value does not match",options[0].getStringValue(getTransaction()),is(expectedOption[1]));
    }

    @Test
    public void testCustomOptionProcedure() throws Exception {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        setup("commandFiles","addStoredProcedures.cmd");
        final String[] commands = {
            "cd myStoredProcedure1",
            "set-custom-option myCustomOption myValue" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        String[][] expectedOptions= {{"myCustomOption", "myValue"}};

        Vdb vdb=getVDB("myVDB1",wkspMgr);
        assertNotNull(vdb);
        Model[] models=vdb.getModels(getTransaction(), "myModel1");
        assertThat(models.length,is(1));
        Procedure[] procedures=models[0].getProcedures(getTransaction(), "myStoredProcedure1");
        assertThat( procedures.length,is(1));
        assertCustomOptions(procedures[0],expectedOptions);

    }

    @Test
    public void testCustomOptionResultSet() throws Exception {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        setup("commandFiles","addResSetColumns.cmd");
        final String[] commands = {
            "cd myColumn1",
            "set-custom-option myCustomOption myValue" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        String[][] expectedOptions= {{"myCustomOption", "myValue"}};

        Vdb vdb=getVDB("myVDB1",wkspMgr);
        assertNotNull(vdb);
        Model[] models=vdb.getModels(getTransaction(), "myModel1");
        assertThat(models.length,is(1));
        Function[] procedures=models[0].getFunctions(getTransaction(), "myPushdownFunction1");
        assertThat( procedures.length,is(1));
        PushdownFunction function=wkspMgr.resolve(getTransaction(), procedures[0], PushdownFunction.class);
        assertNotNull(function);
        TabularResultSet resultSet=wkspMgr.resolve(getTransaction(), function.getResultSet(getTransaction()),TabularResultSet.class);
        ResultSetColumn col=null;
        for(ResultSetColumn rsc:resultSet.getColumns(getTransaction())){
            if(rsc.getName(getTransaction()).equals("myColumn1")){
                col=rsc;
            }
        }
        assertNotNull(col);
        assertCustomOptions(col,expectedOptions);
    }

    @Test
    public void testNoOptionContainer() throws Exception {
        setup("commandFiles","addTables.cmd"); //Current context is /workspace/myVDB1/myModel1
        assertCommandsNotAvailable("set-custom-option","unset-custom-option");
    }
}
