/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.INVALID_MODEL_TYPE_PROPERTY_VALUE;
import java.util.List;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set Model properties
 */
public final class SetModelPropertyCommand extends ModelShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetModelPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final String name = requiredArgument( 0, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );
            final String value = requiredArgument( 1, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );

            final Model model = getModel();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DESCRIPTION:
                    model.setDescription( transaction, value );
                    break;
                case METADATA_TYPE:
                    model.setMetadataType( transaction, value );
                    break;
                case MODEL_TYPE:
                    if ( Model.Type.PHYSICAL.name().equals( value ) ) {
                        model.setModelType( transaction, Model.Type.PHYSICAL );
                    } else if ( Model.Type.VIRTUAL.name().equals( value ) ) {
                        model.setModelType( transaction, Model.Type.VIRTUAL );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_MODEL_TYPE_PROPERTY_VALUE, VISIBLE );
                    }
                    break;
                case VISIBLE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        model.setVisible( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, VISIBLE );
                    }

                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Model.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getMessage( SET_PROPERTY_SUCCESS, name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( SET_PROPERTY_ERROR ), e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        if ( ( args.size() == 1 ) ) {
            String theArg = getArguments().get(0);
            if( VISIBLE.equals(theArg) ) {
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
            } else if( MODEL_TYPE.equals(theArg) ) {
                candidates.add( Model.Type.PHYSICAL.name() );
                candidates.add( Model.Type.VIRTUAL.name() );
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
