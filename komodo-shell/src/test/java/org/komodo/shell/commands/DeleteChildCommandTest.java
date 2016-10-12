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

import java.util.ArrayList;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test class for the {@link DeleteChildCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class DeleteChildCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldNotDeleteChildAtRoot() throws Exception {
        final String[] commands = { "delete-child blah" }; // delete-child is not available at root
        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String child1Name = "blah1";
        final String[] commands = {
            "library",
            "add-child " + child1Name,
            "delete-child " + child1Name + " optionalType extraArg" };
        execute( commands );
    }

    @Test
    @Ignore("Security now locks this down until such time as the library start to be really used")
    public void shouldDeleteChildAtLibrary() throws Exception {
        final String child1Name = "blah1";
        final String child2Name = "blah2";
        final String[] commands = {
            "library",
            "add-child " + child1Name,
            "add-child " + child2Name,
            "delete-child " + child1Name };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject library = _repo.komodoLibrary( getTransaction() );
        assertThat( library.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( child2Name ) );
    }

    @Test
    public void shouldDeleteChildAtWorkspace() throws Exception {
        final String child1Name = "blah1";
        final String child2Name = "blah2";
        final String[] commands = {
            "workspace",
            "add-child " + child1Name,
            "add-child " + child2Name,
            "delete-child " + child1Name };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( child2Name ) );
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
       	setup("commandFiles","addChildren.cmd");
       	assertTabCompletion("delete-child invalid", candidates);

    	candidates.add("myChild1");
    	assertTabCompletion("delete-child myChild1", candidates);

    	candidates.add("myChild2");
    	assertTabCompletion("delete-child myCh", candidates);

    	candidates.add("MyChild3");
    	assertTabCompletion("delete-child ", candidates);
    }
}
