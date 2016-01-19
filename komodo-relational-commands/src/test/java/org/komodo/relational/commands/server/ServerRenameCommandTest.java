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
package org.komodo.relational.commands.server;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test class for {@link ServerRenameCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ServerRenameCommandTest extends AbstractServerCommandTest {

    @Test
    public void shouldRenameServer() throws Exception {
        final String teiidName = "myTeiid";
        final String newName = "blah";
        final String[] commands = { ( "create-teiid " + teiidName ),
                                    "commit",
                                    "set-server " + teiidName,
                                    "cd " + teiidName,
                                    "rename " + newName,
                                    "cd .." };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject server = this.wsStatus.getStateObjects().get( ServerCommandProvider.SERVER_DEFAULT_KEY );
        assertThat( server, is( notNullValue() ) );
        assertThat( server.getName( getTransaction() ), is( newName ) );
    }

}
