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
import org.komodo.relational.model.legacy.RelationalBuilder;
import org.komodo.relational.model.legacy.RelationalBuilderImpl;
import org.komodo.relational.model.legacy.RelationalObject;
import org.komodo.relational.model.legacy.RelationalConstants.TYPES;
import org.komodo.relational.model.legacy.RelationalConstants.TYPES_LITERAL;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.WorkspaceContextImpl;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 *
 */
public class CreateCommand extends BuiltInShellCommand {

	private RelationalBuilder relationalBuilder = RelationalBuilderImpl.getInstance();
	
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
		List<WorkspaceContext.Type> validTypes = new ArrayList<WorkspaceContext.Type>(1);
		validTypes.add(WorkspaceContext.Type.HOME);
		validTypes.add(WorkspaceContext.Type.PROJECT);
		validTypes.add(WorkspaceContext.Type.MODEL);
		validTypes.add(WorkspaceContext.Type.TABLE);
		validTypes.add(WorkspaceContext.Type.PROCEDURE);
		validTypes.add(WorkspaceContext.Type.RESULT_SET);
		validTypes.add(WorkspaceContext.Type.SCHEMA);
		validTypes.add(WorkspaceContext.Type.VIEW);
		this.validWsContextTypes = validTypes;
	}
	
	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		String subcmdArg = requiredArgument(0, Messages.getString("CreateCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
		String objNameArg = requiredArgument(1, Messages.getString("CreateCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
		
		// Check that type is valid at this context
		List<String> validTypes = getWorkspaceStatus().getCurrentContext().getValidTypesForCreate();
		if(!validTypes.contains(subcmdArg.toUpperCase())) {
			print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CreateCommand.ErrorInvalidTypeAtContext", subcmdArg.toUpperCase())); //$NON-NLS-1$
            return false;
		}

		try {
			create(subcmdArg, objNameArg);
			print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CreateCommand.ObjectCreated", subcmdArg, objNameArg)); //$NON-NLS-1$
			if(getWorkspaceStatus().getRecordingStatus()) recordCommand(getArguments());
		} catch (Exception e) {
			print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CreateCommand.Failure", subcmdArg)); //$NON-NLS-1$
			print(CompletionConstants.MESSAGE_INDENT,"\t" + e.getMessage()); //$NON-NLS-1$
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
	private void create(String objType, String objName) {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		if(currentContext.isRelational()) {
			RelationalObject currentObj = currentContext.getRelationalObj();			
			int relType = getRelationalType(objType);
			RelationalObject newObj = relationalBuilder.create(relType, objName, currentObj);
			currentContext.addChild(newObj);
		} else if(currentContext.getType()==WorkspaceContext.Type.PROJECT && TYPES_LITERAL.MODEL.equalsIgnoreCase(objType)) { 
			RelationalObject newObj = relationalBuilder.create(TYPES.MODEL, objName);
			currentContext.addChild(newObj);
		} else if(WorkspaceContext.Type.PROJECT.toString().equalsIgnoreCase(objType)) {
			WorkspaceContext projContext1 = new WorkspaceContextImpl(wsStatus,currentContext,objName,WorkspaceContext.Type.PROJECT);
			currentContext.addChild(projContext1);
		}
	}

	/**
	 * Maps type name to the relational type
	 * @param objType relataonal obj name
	 * @return relational type
	 */
	private int getRelationalType(String objType) {
		int relType = -1;
		if(WorkspaceContext.Type.MODEL.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.MODEL;
		} else if(WorkspaceContext.Type.SCHEMA.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.SCHEMA;
		} else if(WorkspaceContext.Type.TABLE.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.TABLE;
		} else if(WorkspaceContext.Type.VIEW.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.VIEW;
		} else if(WorkspaceContext.Type.PROCEDURE.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.PROCEDURE;
		} else if(WorkspaceContext.Type.PARAMETER.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.PARAMETER;
		} else if(WorkspaceContext.Type.COLUMN.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.COLUMN;
		} else if(WorkspaceContext.Type.PRIMARY_KEY.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.PK;
		} else if(WorkspaceContext.Type.FOREIGN_KEY.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.FK;
		} else if(WorkspaceContext.Type.UNIQUE_CONSTRAINT.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.UC;
		} else if(WorkspaceContext.Type.ACCESS_PATTERN.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.AP;
		} else if(WorkspaceContext.Type.RESULT_SET.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.RESULT_SET;
		} else if(WorkspaceContext.Type.INDEX.toString().equalsIgnoreCase(objType)) {
			relType = TYPES.INDEX;
		}
		return relType;
	}
	
	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {

		if (getArguments().isEmpty()) {
			List<String> validTypes = getWorkspaceStatus().getCurrentContext().getValidTypesForCreate();
			for(String type : validTypes) {
				if (lastArgument == null || type.startsWith(lastArgument.toUpperCase())) {
					candidates.add(type+" "); //$NON-NLS-1$
				}
			}
			return 0;
		}
		return -1;
	}

}