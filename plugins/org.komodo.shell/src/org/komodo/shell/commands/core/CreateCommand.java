/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoType;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 *
 */
public class CreateCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * Constructor.
     * @param name the command name
     * @param wsStatus the workspace status
     */
    public CreateCommand(String name, WorkspaceStatus wsStatus) {
        super(name, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#initValidWsContextTypes()
     */
    @Override
    public void initValidWsContextTypes() {
        List<String> validTypes = new ArrayList<String>(1);
        validTypes.add(KomodoLexicon.Komodo.WORKSPACE);
        this.validWsContextTypes = validTypes;
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String subcmdArg = requiredArgument(0, Messages.getString("CreateCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
        String objNameArg = requiredArgument(1, Messages.getString("CreateCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$

        // Check that creating a type is valid at this context
        if (!this.validWsContextTypes.contains(getWorkspaceStatus().getCurrentContext().getName())) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.ErrorInvalidTypeAtContext", subcmdArg.toUpperCase())); //$NON-NLS-1$
            return false;
        }

        try {
            create(subcmdArg, objNameArg);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.ObjectCreated", subcmdArg, objNameArg)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.Failure", subcmdArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    /**
     * Sets a property on the artifact.
     * @param artifact
     * @param propName
     * @param propValue
     */
    private void create(String objType, String objName) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceContext currentContext = wsStatus.getCurrentContext();

        KomodoType kType = KomodoType.getKType(objType);
        if (kType == null)
            throw new Exception(Messages.getString("CreateCommand.notValidType", objType)); //$NON-NLS-1$

        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        KomodoObject parent = currentContext.getKomodoObj();

        switch (kType) {
            case SCHEMA:
                wkspManager.createSchema(null, parent, objName);
                break;
            case VDB_MODEL:
                wkspManager.createModel(null, parent, objName);
                break;
            case DATA_SOURCE:
            case REPOSITORY:
            case TEIID:
                wkspManager.createTeiid(null, parent, objName);
                break;
            case VDB:
            case VDB_ENTRY:
            case VDB_IMPORT:
            case VDB_MODEL_SOURCE:
            case VDB_TRANSLATOR:
            default:
                throw new UnsupportedOperationException(Messages.getString("CreateCommand.unsupported", kType.toString())); //$NON-NLS-1$
        }
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) {

        if (getArguments().isEmpty()) {
            List<String> validTypes = KomodoType.getAllTypeNames();
            for (String type : validTypes) {
                if (lastArgument == null || type.startsWith(lastArgument.toUpperCase())) {
                    candidates.add(type + " "); //$NON-NLS-1$
                }
            }
            return 0;
        }
        return -1;
    }

}
