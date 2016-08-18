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
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test class for the {@link AddChildCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class AddChildCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "add-child childName extraArg" };
        execute( commands );
    }

    @Test
    @Ignore("Security now locks this down until such time as the library start to be really used")
    public void shouldAddChildAtLibrary() throws Exception {
        final String childName = "blah";
        final String[] commands = { "library", "add-child " + childName };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject library = _repo.komodoLibrary( getTransaction() );
        assertThat( library.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
    }

    @Test
    public void shouldAddChildAtWorkspace() throws Exception {
        final String childName = "blah";
        final String[] commands = { "workspace", "add-child " + childName };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotBeAllowedToAddEnvironmentAtRoot() throws Exception {
        final String childName = KomodoLexicon.Komodo.ENVIRONMENT;
        final String[] commands = { "add-child " + childName };
        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotBeAllowedToAddLibraryAtRoot() throws Exception {
        final String childName = KomodoLexicon.Komodo.LIBRARY;
        final String[] commands = { "add-child " + childName };
        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotBeAllowedToAddWorkspaceAtRoot() throws Exception {
        final String childName = KomodoLexicon.Komodo.WORKSPACE;
        final String[] commands = { "add-child " + childName };
        execute( commands );
    }

}
