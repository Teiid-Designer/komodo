package org.komodo.shell.commands.core;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;

/**
 * renames the referenced node
 * 
 * @author blafond
 *
 */
public class RenameCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * Constructor.
     * @param name the command name
     * @param wsStatus the workspace status
     */
    public RenameCommand(String name, WorkspaceStatus wsStatus) {
        super(name, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objNameArg = requiredArgument(0, Messages.getString("RenameCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
        String newName = requiredArgument(1, Messages.getString("RenameCommand.InvalidArgMsg_NewName")); //$NON-NLS-1$

        try {
            rename(objNameArg, newName);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("RenameCommand.ObjectRenamed", objNameArg, newName)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("RenameCommand.Failure", objNameArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    private void rename(String objName, String newName) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        KomodoObject parent = wsStatus.getCurrentContext().getKomodoObj();
        
        KomodoObject child = parent.getChild(null, objName);
        if( child != null ) {
        	child.rename(null, newName);
        } else {
        	throw new Exception(Messages.getString("RenameCommand.cannotRename_objectDoesNotExist", objName)); //$NON-NLS-1$
        }
    }
}
