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
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 *
 */
public class CreateCommand extends BuiltInShellCommand {

//    private RelationalBuilder relationalBuilder = RelationalBuilderImpl.getInstance();

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
        // Cannot do a create at certain Relational contexts
        List<String> validTypes = new ArrayList<String>(1);
        validTypes.add(KomodoLexicon.Komodo.WORKSPACE);
//        validTypes.add(String.TABLE);
//        validTypes.add(String.PROCEDURE);
//        validTypes.add(String.RESULT_SET);
//        validTypes.add(String.SCHEMA);
//        validTypes.add(String.VIEW);
        this.validWsContextTypes = validTypes;
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
//        String subcmdArg = requiredArgument(0, Messages.getString("CreateCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
//        String objNameArg = requiredArgument(1, Messages.getString("CreateCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
//
//        // Check that type is valid at this context
//        List<String> validTypes = getWorkspaceStatus().getCurrentContext().getValidTypesForCreate();
//        if (!validTypes.contains(subcmdArg.toUpperCase())) {
//            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.ErrorInvalidTypeAtContext", subcmdArg.toUpperCase())); //$NON-NLS-1$
//            return false;
//        }
//
//        try {
//            create(subcmdArg, objNameArg);
//            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.ObjectCreated", subcmdArg, objNameArg)); //$NON-NLS-1$
//            if (getWorkspaceStatus().getRecordingStatus())
//                recordCommand(getArguments());
//        } catch (Exception e) {
//            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.Failure", subcmdArg)); //$NON-NLS-1$
//            print(CompletionConstants.MESSAGE_INDENT, "\t" + e.getMessage()); //$NON-NLS-1$
//            return false;
//        }
        return true;
    }

    /**
     * Sets a property on the artifact.
     * @param artifact
     * @param propName
     * @param propValue
     */
    private void create(String objType, String objName) {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceContext currentContext = wsStatus.getCurrentContext();
        
        
        
        
        
//        if (currentContext.isRelational()) {
//            KomodoObject currentObj = currentContext.getKomodoObj();
//            int relType = getRelationalType(objType);
//            KomodoObject newObj = relationalBuilder.create(relType, objName, currentObj);
//            currentContext.addChild(newObj);
//        } else if (currentContext.getType() == String.PROJECT && TYPES_LITERAL.MODEL.equalsIgnoreCase(objType)) {
//            KomodoObject newObj = relationalBuilder.create(TYPES.MODEL, objName);
//            currentContext.addChild(newObj);
//        } else if (String.PROJECT.toString().equalsIgnoreCase(objType)) {
//            WorkspaceContext projContext1 = new WorkspaceContextImpl(wsStatus, currentContext, objName, String.PROJECT);
//            currentContext.addChild(projContext1);
//        }
    }

    /**
     * Maps type name to the relational type
     * @param objType relataonal obj name
     * @return relational type
     */
    private int getRelationalType(String objType) {
        int relType = -1;
//        if (String.MODEL.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.MODEL;
//        } else if (String.SCHEMA.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.SCHEMA;
//        } else if (String.TABLE.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.TABLE;
//        } else if (String.VIEW.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.VIEW;
//        } else if (String.PROCEDURE.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.PROCEDURE;
//        } else if (String.PARAMETER.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.PARAMETER;
//        } else if (String.COLUMN.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.COLUMN;
//        } else if (String.PRIMARY_KEY.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.PK;
//        } else if (String.FOREIGN_KEY.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.FK;
//        } else if (String.UNIQUE_CONSTRAINT.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.UC;
//        } else if (String.ACCESS_PATTERN.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.AP;
//        } else if (String.RESULT_SET.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.RESULT_SET;
//        } else if (String.INDEX.toString().equalsIgnoreCase(objType)) {
//            relType = TYPES.INDEX;
//        }
        return relType;
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) {

//        if (getArguments().isEmpty()) {
//            List<String> validTypes = getWorkspaceStatus().getCurrentContext().getValidTypesForCreate();
//            for (String type : validTypes) {
//                if (lastArgument == null || type.startsWith(lastArgument.toUpperCase())) {
//                    candidates.add(type + " "); //$NON-NLS-1$
//                }
//            }
//            return 0;
//        }
        return -1;
    }

}
