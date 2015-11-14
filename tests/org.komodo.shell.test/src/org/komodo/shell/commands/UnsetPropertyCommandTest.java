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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link UnsetPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class UnsetPropertyCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "unset-property aProp extraArg" };
        execute( commands );
    }

    @Test
    public void shouldNotBeAvailableAtLibrary() throws Exception {
        final String[] commands = { "library" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( UnsetPropertyCommand.NAME );
    }

    @Test
    public void shouldNotBeAvailableAtRoot() throws Exception {
        assertCommandsNotAvailable( UnsetPropertyCommand.NAME );
    }

    @Test
    public void shouldNotBeAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( UnsetPropertyCommand.NAME );
    }

    @Test
    public void shouldUnsetProperty() throws Exception {
        final String child = "blah";
        final String prop = "foo";
        final String originalValue = "bar";
        final String[] commands = { "workspace",
                                    "add-child " + child,
                                    "cd " + child,
                                    "set-property " + prop + ' ' + originalValue,
                                    "commit",
                                    "set-property " + prop + " bar",
                                    "unset-property " + prop };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject kobject = _repo.getFromWorkspace( getTransaction(), child );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject.getProperty( getTransaction(), prop ), is( nullValue() ) );
    }

}
