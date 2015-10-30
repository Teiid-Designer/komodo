/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;

/**
 * Shows the descriptors of a {@link KomodoObject}
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * show-descriptors
 * </code>
 */
public class ShowDescriptorsCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "show-descriptors"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowDescriptorsCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final KomodoObject kobject = getContext();
            Descriptor[] descriptors = kobject.getDescriptors(getTransaction());
            List<String> items = new ArrayList<String>();
            for(Descriptor desc : descriptors) {
                items.add(desc.getName());
            }
            if(!items.isEmpty()) {
                PrintUtils.printList(getWriter(), items, Messages.getString(Messages.ShowDescriptorsCommand.ListHeader));
            } else {
                print( MESSAGE_INDENT, Messages.getString(Messages.ShowDescriptorsCommand.NoDescriptors) );
            }
            return CommandResult.SUCCESS;
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
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( !KomodoObjectUtils.isRoot(getContext()) && !KomodoObjectUtils.isRootChild(getContext()) );
    }

}
