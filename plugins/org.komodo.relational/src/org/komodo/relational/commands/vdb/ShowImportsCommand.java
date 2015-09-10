/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.IMPORT_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowImportsCommand.IMPORTS_HEADER;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowImportsCommand.NO_IMPORTS;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all VDB imports in a VDB.
 */
public final class ShowImportsCommand extends VdbShellCommand {

    static final String NAME = "show-imports"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowImportsCommand( final WorkspaceStatus status ) {
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
        final VdbImport[] imports = vdb.getImports( uow );

        if ( imports.length == 0 ) {
            print( MESSAGE_INDENT, getMessage(NO_IMPORTS, vdb.getName(uow)) );
        } else {
            print( MESSAGE_INDENT, getMessage(IMPORTS_HEADER, vdb.getName(uow)) );
            List<String> names = new ArrayList<String>(imports.length);
            for ( final VdbImport theImport : imports ) {
                names.add(theImport.getName(uow));
            }
            
            PrintUtils.printList(getWorkspaceStatus(), names, getMessage(IMPORT_NAME));
        }
        print();

        return true;
    }

}
