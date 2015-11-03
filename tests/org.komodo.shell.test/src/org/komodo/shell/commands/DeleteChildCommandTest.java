/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
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
        setup( commands );
        execute();
    }
    
    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String child1Name = "blah1";
        final String[] commands = { 
            "library", 
            "add-child " + child1Name,
            "delete-child " + child1Name + " optionalType extraArg" };
        setup( commands );
        execute();
    }
    
    @Test
    public void shouldDeleteChildAtLibrary() throws Exception {
        final String child1Name = "blah1";
        final String child2Name = "blah2";
        final String[] commands = { 
            "library", 
            "add-child " + child1Name,
            "add-child " + child2Name,
            "delete-child " + child1Name };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

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
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( child2Name ) );
    }

}