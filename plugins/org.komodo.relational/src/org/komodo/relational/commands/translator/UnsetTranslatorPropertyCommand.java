/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.translator;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_MISSING_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_PROPERTY_SUCCESS;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Translator;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set {@link Translator translator} properties.
 */
public final class UnsetTranslatorPropertyCommand extends TranslatorShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetTranslatorPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, getWorkspaceMessage( UNSET_MISSING_PROPERTY_NAME ) );

            final Translator translator = getTranslator();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DESCRIPTION:
                    translator.setDescription( transaction, null );
                    break;
                // TYPE is mandatory and does not have a default so it can't be unset
                // case TYPE:
                //     translator.setType( transaction, null );
                //     break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Translator.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getMessage( UNSET_PROPERTY_SUCCESS, name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
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
        return 1;
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
            // TYPE cannot be unset
            final List< String > props = new ArrayList< >( ALL_PROPS );
            props.remove( TYPE );

            if ( lastArgument == null ) {
                candidates.addAll( props );
            } else {
                for ( final String item : props ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
