/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import java.util.List;

import org.komodo.relational.RelationalProperty;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.FindWorkspaceNodeVisitor;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Creates various Vdb and relational objects based on command line inputs.
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
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String subcmdArg = requiredArgument(0, Messages.getString("CreateCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
        String objNameArg = requiredArgument(1, Messages.getString("CreateCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$

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

    private void create(String objType, String objName) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceContext currentContext = wsStatus.getCurrentContext();
        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        KomodoObject parent = currentContext.getKomodoObj();

        KomodoType kType = KomodoType.getKomodoType(objType);
        switch (kType) {
            case STATEMENT_OPTION: {
                String optionValue = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_StatementOptionValue")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(StandardDdlLexicon.VALUE, optionValue));
            } break;
            case VDB_ENTRY: {
                String entryPath = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_EntryPath")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(VdbLexicon.Entry.PATH, entryPath));
        	} break;
            case VDB_TRANSLATOR: {
                String transType = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_TranslatorType")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(VdbLexicon.Translator.TYPE, transType));
            } break;
            case VDB: {
                String filePath = optionalArgument(2, Messages.getString("CreateCommand.DefaultVdb_VdbFilePath")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(VdbLexicon.Vdb.ORIGINAL_FILE, filePath));
            } break;
            case MODEL: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.modelNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(VdbLexicon.Model.MODEL, name));
            } break;
            case TABLE: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.tableNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.TABLE_STATEMENT, name));
            } break;
            case VIEW: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.viewNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.VIEW_STATEMENT, name));
            } break;
            case COLUMN: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.columnNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, name));
            } break;
            case FOREIGN_KEY: {
                String tableRefPath = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_FKTableRefPath")); //$NON-NLS-1$
                
                FindWorkspaceNodeVisitor visitor = new FindWorkspaceNodeVisitor(tableRefPath);
                visitor.visit(wsStatus.getRootContext());
                WorkspaceContext otherTableContext = visitor.getNodeContext();

                if (otherTableContext == null)
                    throw new Exception(Messages.getString("CreateCommand.invalidForeignKeyRefPath", tableRefPath)); //$NON-NLS-1$
            	
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT, otherTableContext.getKomodoObj()));
            } break;
            case PRIMARY_KEY: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.primaryKeyNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.Constraint.TABLE_ELEMENT, name));
            } break;
            case STORED_PROCEDURE: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.procedureNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT, name));
            } break;
            case PARAMETER: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.parameterNameRequired")); //$NON-NLS-1$
                wkspManager.create(null, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateProcedure.PARAMETER, name));
            } break;
            case UNKNOWN:
                throw new Exception(Messages.getString("CreateCommand.notValidType", objType)); //$NON-NLS-1$
            default:
                wkspManager.create(null, parent, objName, kType);
        }
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) {

        if (getArguments().isEmpty()) {
            List<String> validTypes = KomodoType.getTypes();
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
