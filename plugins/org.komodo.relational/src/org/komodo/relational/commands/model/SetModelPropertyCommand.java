/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.INVALID_MODEL_TYPE_PROPERTY_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Model properties
 */
public final class SetModelPropertyCommand extends ModelShellCommand {

    static final String NAME = "set-model-property"; //$NON-NLS-1$

    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String METADATA_TYPE = "metadataType"; //$NON-NLS-1$
    private static final String MODEL_TYPE = "modelType"; //$NON-NLS-1$
    private static final String VISIBLE = "visible"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, METADATA_TYPE, MODEL_TYPE, VISIBLE } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetModelPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final Model model = getModel();
        
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case DESCRIPTION:
                model.setDescription(transaction, value);
                break;
            case METADATA_TYPE:
                model.setMetadataType(transaction, value);
                break;
            case MODEL_TYPE:
                if ( Model.Type.PHYSICAL.name().equals( value ) ) {
                    model.setModelType( transaction, Model.Type.PHYSICAL );
                } else if ( Model.Type.VIRTUAL.name().equals( value ) ) {
                    model.setModelType( transaction, Model.Type.VIRTUAL );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_MODEL_TYPE_PROPERTY_VALUE, VISIBLE ) );
                    success = false;
                }
                break;
            case VISIBLE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    model.setVisible( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, VISIBLE ) );
                    success = false;
                }

                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

        return success;
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
