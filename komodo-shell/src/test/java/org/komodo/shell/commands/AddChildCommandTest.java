/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
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
    public void shouldAllowAtRoot() throws Exception {
        final String child = "blah";
        final String[] commands = { "add-child " + child };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final KomodoObject root = _repo.komodoWorkspace( getTransaction() ).getParent( getTransaction() );
        assertThat( root.getChild( getTransaction(), child ), is( notNullValue() ) );
    }

    @Test
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
