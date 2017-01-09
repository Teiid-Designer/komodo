/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.shell.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test class for the {@link RemoveDescriptorCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class RemoveDescriptorCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailWhenTooManyArgs() throws Exception {
        final String removed = "mix:referenceable";
        final String childName = "blah";
        final String[] commands = { "workspace",
                                    "add-child " + childName,
                                    "cd " + childName,
                                    "add-descriptor " + removed,
                                    "remove-descriptor " + removed + " extraArg" };
        execute( commands );
    }

    @Test
    public void shouldNotHaveRemoveDescriptorAvailableAtLibrary() throws Exception {
        final String[] commands = { "library" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( RemoveDescriptorCommand.NAME );
    }

    @Test
    public void shouldNotHaveRemoveDescriptorAvailableAtRoot() throws Exception {
        assertCommandsNotAvailable( RemoveDescriptorCommand.NAME );
    }

    @Test
    public void shouldNotHaveRemoveDescriptorAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsNotAvailable( RemoveDescriptorCommand.NAME );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotRemoveDescriptorIfNotFound() throws Exception {
        final String childName = "blah";
        final String[] commands = { "workspace",
                                    "add-child " + childName,
                                    "cd " + childName,
                                    "remove-descriptor mix:referenceable" };
        execute( commands );
    }

    @Test
    public void shouldRemoveDescriptor() throws Exception {
        final String removed = "mix:referenceable";
        final String childName = "blah";
        final String[] commands = { "workspace",
                                    "add-child " + childName,
                                    "cd " + childName,
                                    "add-descriptor " + removed,
                                    "remove-descriptor " + removed };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject library = _repo.komodoWorkspace( getTransaction() );
        assertThat( library.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].hasDescriptor( getTransaction(), removed ), is( false ) );
    }

}
