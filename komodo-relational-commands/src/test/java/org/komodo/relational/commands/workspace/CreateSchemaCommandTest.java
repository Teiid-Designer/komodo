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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link CreateSchemaCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class CreateSchemaCommandTest extends AbstractCommandTest {

    @Test
    public void testCreateSchema1() throws Exception {
        final String[] commands = { "create-schema testSchema" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

    	WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
    	Schema[] schemas = wkspMgr.findSchemas(getTransaction());

    	assertEquals(1, schemas.length);
    	assertEquals("testSchema", schemas[0].getName(getTransaction()));
    }

    @Test
    public void shouldDisplayHelp( ) throws Exception {
        CreateSchemaCommand command = new CreateSchemaCommand(wsStatus);
        command.setWriter( getOutputWriter() );
        command.printHelp(CompletionConstants.MESSAGE_INDENT);

        String writerOutput = getCommandOutput();
        assertThat( writerOutput, !writerOutput.contains( CreateSchemaCommand.class.getSimpleName() + ".help" ), is( true ) );
        assertThat( writerOutput, writerOutput.contains( CreateSchemaCommand.NAME ), is( true ) );
    }

    @Test
    public void shouldFailTooManyArgs( ) throws Exception {
        CreateSchemaCommand command = new CreateSchemaCommand(wsStatus);
        command.setArguments(new Arguments( "aName anExtraArg" ));  //$NON-NLS-1$
        CommandResult result = command.execute();

        assertThat( result.isOk(), is( false ) );
        assertThat( result.getMessage(), result.getMessage().contains( CreateSchemaCommand.NAME ), is( true ) );
    }

}
