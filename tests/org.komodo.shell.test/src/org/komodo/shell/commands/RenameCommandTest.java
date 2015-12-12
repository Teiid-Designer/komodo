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
import static org.junit.Assert.fail;
import org.junit.Test;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link RenameCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class RenameCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String childName = "blah";
        final String newChildName = "blech";
        final String[] commands = {
            "workspace",
            "add-child " + childName,
            "rename " + childName + " " + newChildName + " extraArg"};
        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotRenameCurrentContextIfDuplicateNameAndTypeSibling() throws Exception {
        final String name1 = "name1";
        final String name2 = "name2";
        final String[] commands = { "workspace",
                                    "add-child " + name1,
                                    "add-child " + name2,
                                    "cd " + name2,
                                    "rename " + name1 };

        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotRenameIfDuplicateNameAndTypeChild() throws Exception {
        final String name1 = "name1";
        final String name2 = "name2";
        final String[] commands = { "workspace",
                                    "add-child " + name1,
                                    "add-child " + name2,
                                    "rename " + name1 + " " + name2 };

        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotRenameIfNameDoesNotChange() throws Exception {
        final String childName = "blah";
        final String[] commands = { "workspace",
                                    "add-child " + childName,
                                    "rename " + childName + " " + childName };

        execute( commands );
    }

    @Test
    public void shouldNotRenameReservedPaths() throws Exception {
        for ( final String reservedPath : ObjectImpl.RESERVED_PATHS ) {
            this.wsStatus.setCurrentContext( new ObjectImpl( _repo, reservedPath, 0 ) );

            try {
                this.wsStatus.getCurrentContext().rename( getTransaction(), "blah" );
                fail("Renaming of path '" + reservedPath + "' should not be allowed");
            } catch ( final Exception e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldRenameChild() throws Exception {
        final String childName = "blah";
        final String newChildName = "blech";
        final String[] commands = {
            "workspace",
            "add-child " + childName,
            "rename " + childName + " " + newChildName};

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( newChildName ) );
    }

    @Test
    public void shouldRenameSelf() throws Exception {
        final String name = "foo";
        final String newName = "bar";
        final String[] commands = { "workspace",
                                    "add-child " + name,
                                    "cd " + name,
                                    "rename " + newName };

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertThat( this.wsStatus.getCurrentContext().getName( getTransaction() ), is( newName ) );
    }

}
