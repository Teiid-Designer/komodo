/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.NO_PROPERTIES;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PROPERTIES_HEADER;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PROPERTY_NOT_SET;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all VDB properties.
 */
public final class ShowVdbPropertiesCommand extends VdbShellCommand {

    static final String NAME = "show-vdb-properties"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowVdbPropertiesCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final UnitOfWork uow = getTransaction();
        final Vdb vdb = getVdb();
        final PropertyDescriptor[] descriptors = vdb.getPropertyDescriptors( uow );

        if ( descriptors.length == 0 ) {
            print( MESSAGE_INDENT, getWorkspaceMessage(NO_PROPERTIES) );
        } else {
            // print header
            print( MESSAGE_INDENT, getWorkspaceMessage(PROPERTIES_HEADER, getDisplayType( vdb ), vdb.getName( uow ) ) );

            for ( final PropertyDescriptor descriptor : descriptors ) {
                final String name = descriptor.getName();
                String value = null;

                if ( vdb.hasProperty( uow, name ) ) {
                    value = vdb.getProperty( uow, name ).getStringValue( uow );
                } else {
                    value = getWorkspaceMessage(PROPERTY_NOT_SET);
                }

                print( MESSAGE_INDENT, String.format( "%-25s%-25s", name, value ) ); //$NON-NLS-1$
            }
        }

        return true;
    }

}
