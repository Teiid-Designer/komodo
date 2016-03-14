/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} to set the value of a global property
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * set-global &lt;prop-name&gt; &lt;prop-value&gt;
 * </code>
 */
public class SetGlobalPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-global"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetGlobalPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            // property name and value are required
            String propNameArg = requiredArgument(0, I18n.bind(ShellI18n.invalidArgMsgGlobalPropertyName));
            String propValueArg = requiredArgument(1, I18n.bind(ShellI18n.invalidArgMsgPropertyValue ) );

            // validate global property name and value
            WorkspaceStatus wsStatus = getWorkspaceStatus();
            String errorMsg = null;
            if(wsStatus.isGlobalProperty(propNameArg)) {
                errorMsg = getWorkspaceStatus().validateGlobalPropertyValue( propNameArg, propValueArg );
            } else if(wsStatus.isProvidedGlobalProperty(propNameArg)) {
                errorMsg = wsStatus.validateProvidedGlobalPropertyValue(propNameArg, propValueArg);
            } else {
                errorMsg = I18n.bind( ShellI18n.invalidGlobalProperty, propNameArg );
            }

            if ( !StringUtils.isEmpty( errorMsg ) ) {
                return new CommandResultImpl( false, I18n.bind( ShellI18n.invalidGlobalProperty, errorMsg ), null );
            }

            // Set the property
            if( wsStatus.isGlobalProperty(propNameArg) ) {
                wsStatus.setGlobalProperty(propNameArg, propValueArg);
            } else {
                String propType = getWorkspaceStatus().getProvidedGlobalPropertyTypes().get(propNameArg);
                wsStatus.setProvidedGlobalProperty(propNameArg, propValueArg, propType);
            }
            return new CommandResultImpl( I18n.bind( ShellI18n.globalPropertySet, propNameArg ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
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
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setGlobalPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setGlobalPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setGlobalPropertyUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            // Global property completion options
            final Set< String > potentials = new HashSet<String>(WorkspaceStatus.GLOBAL_PROPS.keySet());
            
            // Add provided globals to potentials
            Set< String > providedPropNames = getWorkspaceStatus().getProvidedGlobalProperties().stringPropertyNames();
            potentials.addAll( providedPropNames );

            if ( lastArgument == null ) {
                candidates.addAll( potentials );
            } else {
                for ( final String name : potentials ) {
                    if ( name.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( name );
                    }
                }
            }
        } else if ( getArguments().size() == 1 ) {
            if ( getWorkspaceStatus().isBooleanProperty( getArguments().get( 0 ) ) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
