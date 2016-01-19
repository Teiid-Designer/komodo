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
package org.komodo.shell.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link SetPrimaryTypeCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class SetPrimaryTypeCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailWhenTooManyArgs() throws Exception {
        final String[] commands = { "workspace",
                                    "add-child myChild",
                                    "cd myChild",
                                    "set-primary-type nt:folder extraArg" };
        execute( commands );
    }

    @Test
    public void shouldNotHaveSetPrimaryTypeAvailableAtRoot() throws Exception {
        assertCommandsNotAvailable( SetPrimaryTypeCommand.NAME );
    }

    @Test
    public void shouldNotHaveSetPrimaryTypeAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( SetPrimaryTypeCommand.NAME );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotSetInvalidType() throws Exception {
        final String[] commands = {
            "workspace",
            "add-child myChild",
            "cd myChild",
            "set-primary-type myType"
        };
        execute( commands );
    }

    @Test
    public void shouldSetPrimaryType() throws Exception {
        final String[] commands = {
            "workspace",
            "add-child myChild",
            "cd myChild",
            "set-primary-type nt:folder"
        };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( "myChild" ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getPrimaryType(getTransaction()).getName(), is( "nt:folder" ) );
    }

}
