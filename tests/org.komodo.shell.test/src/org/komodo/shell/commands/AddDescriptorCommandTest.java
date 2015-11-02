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
 * Test class for the {@link AddDescriptorCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class AddDescriptorCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = {
            "workspace",
            "add-child blah",
            "cd blah",
            "add-descriptor aDescriptor extraArg"};
        setup( commands );
        execute();
    }

    @Test
    public void shouldNotHaveAddDescriptorAvailableAtLibrary() throws Exception {
        final String[] commands = { "library" };
        setup( commands );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertCommandsNotAvailable( AddDescriptorCommand.NAME );
    }

    @Test
    public void shouldNotHaveAddDescriptorAvailableAtRoot() throws Exception {
        assertCommandsNotAvailable( AddDescriptorCommand.NAME );
    }

    @Test
    public void shouldNotHaveAddDescriptorAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace" };
        setup( commands );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertCommandsNotAvailable( AddDescriptorCommand.NAME );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotAddBadDescriptor() throws Exception {
        final String childName = "blah";
        final String[] commands = {
            "workspace",
            "add-child " + childName,
            "cd " + childName,
            "add-descriptor aDescriptor"};
        setup( commands );
        execute();
    }

    @Test
    public void shouldAddDescriptor() throws Exception {
        final String expected = "mix:referenceable";
        final String childName = "blah";
        final String[] commands = {
            "workspace",
            "add-child " + childName,
            "cd " + childName,
            "add-descriptor " + expected };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getDescriptors(getTransaction()).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].hasDescriptor(getTransaction(), expected ), is( true ) );
    }

}
