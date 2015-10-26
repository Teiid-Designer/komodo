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
 * Test class for the {@link AddChildCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class AddChildCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldNotAddChildAtRoot() throws Exception {
        final String[] commands = { "add-child blah" }; // add-child is not available at root
        setup( commands );
        execute();
    }

    @Test
    public void shouldAddChildAtLibrary() throws Exception {
        final String childName = "blah";
        final String[] commands = { "library", "add-child " + childName };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

        final KomodoObject library = _repo.komodoLibrary( getTransaction() );
        assertThat( library.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
    }

    @Test
    public void shouldAddChildAtWorkspace() throws Exception {
        final String childName = "blah";
        final String[] commands = { "workspace", "add-child " + childName };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
    }

}
