/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.workspace;

import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INPUT_FILE_ERROR;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.MISSING_INPUT_VDB_FILE_PATH;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.MISSING_VDB_NAME;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.VDB_INPUT_FILE_IS_EMPTY;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.OVERWRITE_ARG_INVALID;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.VDB_OVERWRITE_DISABLED;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.UploadVdbCommand.VDB_UPLOADED;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;

/**
 * Loads a {@link Vdb VDB} from a local file.
 */
public final class UploadVdbCommand extends WorkspaceShellCommand {

    static final String NAME = "upload-vdb"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UploadVdbCommand( final WorkspaceStatus status ) {
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
            final String vdbName = requiredArgument( 0, getMessage( MISSING_VDB_NAME ) );
            final String fileName = requiredArgument( 1, getMessage( MISSING_INPUT_VDB_FILE_PATH ) );
            final String overwriteArg = optionalArgument( 2, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, getMessage( OVERWRITE_ARG_INVALID, overwriteArg ), null );
            }

            { // Validates the supplied fileNameArg is a valid, readable file, and has property extension
                final String validationResult = validateReadableFileArg( fileName );

                if ( !CompletionConstants.OK.equals( validationResult ) ) {
                    return new CommandResultImpl( false, getMessage( INPUT_FILE_ERROR, fileName, validationResult ), null );
                }
            }

            // read file
            final String content = new String( Files.readAllBytes( Paths.get( fileName ) ) );

            if ( StringUtils.isEmpty( content ) ) {
                return new CommandResultImpl( false, getMessage( VDB_INPUT_FILE_IS_EMPTY, fileName ), null );
            }

            final Repository.UnitOfWork uow = getTransaction();

            // make sure we can overwrite
            Vdb[] allVdbs = getWorkspaceManager().findVdbs(uow);
            boolean hasVdb = false;
            for(Vdb theVdb : allVdbs) {
                if(vdbName.equals(theVdb.getName(uow))) {
                    hasVdb = true;
                    break;
                }
            }
            if ( hasVdb && !overwrite ) {
                return new CommandResultImpl( false, getMessage( VDB_OVERWRITE_DISABLED, fileName, vdbName ), null );
            }

            // create VDB
            final Vdb vdb = getWorkspaceManager().createVdb( uow, null, vdbName, fileName );
            final KomodoObject fileNode = vdb.addChild( uow, JcrLexicon.CONTENT.getString(), null );
            fileNode.setProperty( uow, JcrLexicon.DATA.getString(), content );

            return new CommandResultImpl( getMessage( VDB_UPLOADED, vdbName ) );
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
        return 3;
    }

}
