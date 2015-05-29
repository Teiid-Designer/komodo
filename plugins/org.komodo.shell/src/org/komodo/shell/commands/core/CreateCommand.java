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

import org.komodo.relational.RelationalProperty;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.FindWorkspaceNodeVisitor;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Creates various Vdb and relational objects based on command line inputs.
 */
public class CreateCommand extends BuiltInShellCommand implements StringConstants {

    private static final String CREATE = "create"; //$NON-NLS-1$
    
    /**
     * Constructor.
     * @param wsStatus the workspace status
     */
    public CreateCommand(WorkspaceStatus wsStatus) {
        super(CREATE, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        
        // TODO: DataTypeResultSet - doesnt use the name - autonames 'resultSet'
        
        // --------------------------------------------------
        // Make sure the correct number of args are present
        // --------------------------------------------------
        // Obj type and name are required for all types
        String typeArg = requiredArgument(0, Messages.getString("CreateCommand.InvalidArgMsg_ObjectType")); //$NON-NLS-1$
        String objNameArg = requiredArgument(1, Messages.getString("CreateCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$

        // Context path only required for some types
        String pathArg = null;
        if(requiresContextPath(typeArg)) {
            pathArg = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_ContextPath")); //$NON-NLS-1$
        } else {
            pathArg = optionalArgument(2);
        }
        
        // Some types require a fourth arg
        String fourthArg = null;
        if(requiresFourthArg(typeArg)) {
            KomodoType type = KomodoType.getKomodoType(typeArg);
            if(type == KomodoType.FOREIGN_KEY) {
                fourthArg = requiredArgument(3, Messages.getString("CreateCommand.InvalidArgMsg_FKTableRefPath")); //$NON-NLS-1$
            } else {
                fourthArg = requiredArgument(3, Messages.getString("CreateCommand.InvalidArgMsg_FourthArg")); //$NON-NLS-1$
            }
        }

        // ----------------------------------------------
        // Validate the arg values
        // ----------------------------------------------
        if (!validate(typeArg,objNameArg,pathArg,fourthArg)) {
            return false;
        }
        
        // ---------------------------
        // Get context for the create
        // ---------------------------
        WorkspaceContext context = ContextUtils.getContextForPath(getWorkspaceStatus(), pathArg);

        // ---------------------------
        // Create the object
        // ---------------------------
        try {
        	// Create
            create(typeArg, objNameArg, context, fourthArg);
            // Commit transaction
            getWorkspaceStatus().commit("CreateCommand"); //$NON-NLS-1$
            // Print message
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.ObjectCreated", typeArg, objNameArg)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("CreateCommand.Failure", typeArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    /**
     * Validate the supplied args
     * @param typeArg the object type
     * @param objNameArg the object name
     * @param pathArg the parent path
     * @param fourthArg optional fourth arg
     * @return 'true' if valid, 'false' if not.
     * @throws Exception
     */
    protected boolean validate(String typeArg, String objNameArg, String pathArg, String fourthArg) throws Exception {
        // Validate the path if supplied
        if(!StringUtils.isEmpty(pathArg)) {
            if (!validatePath(pathArg)) {
                return false;
            }
        }
        
        // Get the context for object create.  otherwise use current context
        WorkspaceContext context = getWorkspaceStatus().getCurrentContext();
        if (!StringUtils.isEmpty(pathArg)) {
            context = ContextUtils.getContextForPath(getWorkspaceStatus(), pathArg);
        }
        
        // Validate the type is valid for the context
        if (!validateChildType(typeArg,context)) {
            return false;
        }
        
        // Validate the name
        KomodoType kType = KomodoType.getKomodoType(typeArg);
        if (!validateObjectName(objNameArg,kType)) {
            return false;
        }
        
        // Validate fourth arg if it's required
        if(requiresFourthArg(typeArg)) {
            if (!validateFourthArg(typeArg,context,fourthArg)) {
                return false;
            }
        }
        
        // Check for existing object type with requested name.  dont allow a duplicate
        WorkspaceContext childContext = context.getChild(objNameArg, typeArg);
        if(childContext!=null) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CreateCommand.noDuplicateAllowed", objNameArg, typeArg)); //$NON-NLS-1$
            return false;
        }
        
        return true;
    }
    
    /**
     * Validate the fourth arg.  Not all creates require this arg.  Validiation is specific to the type.
     * @param objType the object type
     * @param context the context where created
     * @param fourthArg the fourth arg
     * @return 'true' if valid, 'false' if not.
     */
    private boolean validateFourthArg(String objType, WorkspaceContext context, String fourthArg) {
        KomodoType kType = KomodoType.getKomodoType(objType);
        
        // For foreign keys, the fourth arg is a table reference
        if(kType == KomodoType.FOREIGN_KEY) {
            String tableRefPath = fourthArg;
            WorkspaceContext tableContext = ContextUtils.getContextForPath(getWorkspaceStatus(), tableRefPath);
            // No context found for path
            if(tableContext==null) {
                print(CompletionConstants.MESSAGE_INDENT,Messages.getString("BuiltInShellCommand.locationArg_noContextWithThisName", tableRefPath)); //$NON-NLS-1$
                return false;
            } else {
                KomodoType refType = null;
                try {
                    refType = KomodoType.getKomodoType(tableContext.getType());
                } catch (Exception ex) {
                    // Do nothing
                }
                if(refType ==null || refType!=KomodoType.TABLE) {
                    print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CreateCommand.FKTableRefPath_NotATable", tableRefPath)); //$NON-NLS-1$
                    return false;
                }
            }
        }
        
        return true;
    }
    
    /**
     * Create an object of the supplied type and name, at the specified context
     * @param objType the object type
     * @param objName the object name
     * @param context the context
     * @param fourthArg fourth arg - required for some types.
     * @throws Exception the exception
     */
    private void create(String objType, String objName, WorkspaceContext context, String fourthArg) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceManager wkspManager = wsStatus.getCurrentContext().getWorkspaceManager();
        final UnitOfWork uow = wsStatus.getTransaction();
        KomodoObject parent = context.getKomodoObj();

        KomodoType kType = KomodoType.getKomodoType(objType);
        switch (kType) {
            case STATEMENT_OPTION: {
                String optionValue = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_StatementOptionValue")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(StandardDdlLexicon.VALUE, optionValue));
            } break;
            case VDB_ENTRY: {
                String entryPath = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_EntryPath")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(VdbLexicon.Entry.PATH, entryPath));
            } break;
            case VDB_TRANSLATOR: {
                String transType = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_TranslatorType")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(VdbLexicon.Translator.TYPE, transType));
            } break;
            case VDB: {
                String filePath = optionalArgument(2, Messages.getString("CreateCommand.DefaultVdb_VdbFilePath")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(VdbLexicon.Vdb.ORIGINAL_FILE, filePath));
            } break;
            case MODEL: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.modelNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(VdbLexicon.Model.MODEL, name));
            } break;
            case TABLE: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.tableNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.TABLE_STATEMENT, name));
            } break;
            case VIEW: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.viewNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.VIEW_STATEMENT, name));
            } break;
            case COLUMN: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.columnNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, name));
            } break;
            case FOREIGN_KEY: {
                String tableRefPath = requiredArgument(2, Messages.getString("CreateCommand.InvalidArgMsg_FKTableRefPath")); //$NON-NLS-1$

                FindWorkspaceNodeVisitor visitor = new FindWorkspaceNodeVisitor(tableRefPath);
                visitor.visit(wsStatus.getWorkspaceContext());
                WorkspaceContext otherTableContext = visitor.getNodeContext();

                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT, otherTableContext.getKomodoObj()));
            } break;
            case PRIMARY_KEY: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.primaryKeyNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.Constraint.TABLE_ELEMENT, name));
            } break;
            case STORED_PROCEDURE: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.procedureNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT, name));
            } break;
            case PARAMETER: {
                String name = requiredArgument(1, Messages.getString("CreateCommand.parameterNameRequired")); //$NON-NLS-1$
                wkspManager.create(uow, parent, objName, kType, new RelationalProperty(TeiidDdlLexicon.CreateProcedure.PARAMETER, name));
            } break;
            
            case UNKNOWN:
                throw new Exception(Messages.getString("CreateCommand.notValidType", objType)); //$NON-NLS-1$
            default:
                wkspManager.create(uow, parent, objName, kType);
        }
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        if (getArguments().isEmpty()) {
        	// Get list of valid child types for current context
        	KomodoObject kObj = getWorkspaceStatus().getCurrentContext().getKomodoObj();
        	KomodoType[] validTypes = kObj.getChildTypes();
        	List<String> validChildren = new ArrayList<String>(validTypes.length);
        	for(KomodoType kType : validTypes) {
        		validChildren.add(kType.getType());
        	}
        	
            // --------------------------------------------------------------
            // No arg - offer subcommands
            // --------------------------------------------------------------
            if(lastArgument==null) {
                candidates.addAll(validChildren);
                // --------------------------------------------------------------
                // One arg - determine the completion options for it.
                // --------------------------------------------------------------
            } else {
                for (String item : validChildren) {
                    if (item.toUpperCase().startsWith(lastArgument.toUpperCase())) {
                        candidates.add(item);
                    }
                }
            }
            return 0;
        } else if (getArguments().size()==1) {
            if(lastArgument==null) {
                candidates.add("objName"); //$NON-NLS-1$
            }
            return 0;
        } else if (getArguments().size()==2) {
            // The arg is expected to be a path
            updateTabCompleteCandidatesForPath(candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument);

            // Do not put space after it - may want to append more to the path
            return CompletionConstants.NO_APPEND_SEPARATOR;
        }
        return -1;
    }
    
    /**
     * Determine if the type requires the context path arg
     * @param objType the type name
     * @return 'true' if the path is required, 'false' if not.
     */
    private boolean requiresContextPath(String objType) {
        KomodoType kType = KomodoType.getKomodoType(objType);
        if(kType == KomodoType.FOREIGN_KEY) {
            return true;
        }
        
        return false;
    }

    /**
     * Determine if the type requires the fourth command line arg
     * @param objType the type name
     * @return 'true' if the fourth arg is required, 'false' if not.
     */
    private boolean requiresFourthArg(String objType) {
        KomodoType kType = KomodoType.getKomodoType(objType);
        if(kType == KomodoType.FOREIGN_KEY) {
            return true;
        }
        
        return false;
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage(int indent) {
        // Print out the objType-specific usage (if possible)
        if(getArguments()!=null && getArguments().size()>=1) {
            String objType = getArguments().get(0);
            if(requiresFourthArg(objType)) {
                KomodoType kType = KomodoType.getKomodoType(objType);
                if(kType==KomodoType.FOREIGN_KEY) {
                    print(indent,Messages.getString(getClass().getSimpleName() + ".fkUsage")); //$NON-NLS-1$
                } else {
                    print(indent,Messages.getString(getClass().getSimpleName() + ".usage")); //$NON-NLS-1$
                }
            } else {
                print(indent,Messages.getString(getClass().getSimpleName() + ".usage")); //$NON-NLS-1$
            }
        } else {
            print(indent,Messages.getString(getClass().getSimpleName() + ".usage")); //$NON-NLS-1$
        }
    }

}
