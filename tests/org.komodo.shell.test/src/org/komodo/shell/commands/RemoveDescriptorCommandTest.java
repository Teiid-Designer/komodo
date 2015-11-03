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
        setup( commands );
        execute();
    }

    @Test
    public void shouldNotHaveRemoveDescriptorAvailableAtLibrary() throws Exception {
        final String[] commands = { "library" };
        setup( commands );
        final CommandResult result = execute();

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
        setup( commands );
        final CommandResult result = execute();

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
        setup( commands );
        execute();
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
        setup( commands );

        final CommandResult result = execute();
        assertCommandResultOk( result );

        final KomodoObject library = _repo.komodoWorkspace( getTransaction() );
        assertThat( library.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
        assertThat( library.getChildren( getTransaction() )[ 0 ].hasDescriptor( getTransaction(), removed ), is( false ) );
    }

}
