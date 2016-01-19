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
package org.komodo.relational.commands.resultset;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.commands.RenameCommand;

/**
 * Test Class to test {@link ResultSetRenameCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ResultSetRenameCommandTest extends AbstractCommandTest {

    @Test
    public void shouldNotBeAvailableForDataTypeResultSet() throws Exception {
        setup( "commandFiles", "addPushdownFunctions.cmd" );

        final String[] commands = { "cd myPushdownFunction1",
                                    "set-result-set DataTypeResultSet",
                                    "cd resultSet" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( RenameCommand.NAME );
    }

    @Test
    public void shouldNotBeAvailableForTabularResultSet() throws Exception {
        setup( "commandFiles", "addPushdownFunctions.cmd" );

        final String[] commands = { "cd myPushdownFunction1",
                                    "set-result-set TabularResultSet",
                                    "cd resultSet" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( RenameCommand.NAME );
    }

}
