/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.teiid.query.resolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.metadata.QueryNode;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.language.SQLConstants;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.metadata.TempMetadataID.Type;
import org.teiid.query.metadata.TempMetadataStore;
import org.teiid.query.parser.TCQueryParser;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.ProcedureContainer;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.TriggerActionImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.validator.DefaultUpdateValidator.UpdateInfo;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;


public abstract class ProcedureContainerResolver extends CommandResolver {

    /**
     * @param queryResolver
     */
    public ProcedureContainerResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    public abstract void resolveProceduralCommand(CommandImpl command,
                                                  TempMetadataAdapter metadata) throws Exception;

    /**
     * Expand a command by finding and attaching all subcommands to the command.  If
     * some initial resolution must be done for this to be accomplished, that is ok, 
     * but it should be kept to a minimum.
     * @param procCcommand The command to expand
     * @param metadata Metadata access
     * 
     * @throws Exception
     */
    public CommandImpl expandCommand(ProcedureContainer procCommand, QueryMetadataInterface metadata)
    throws Exception {
        
        // Resolve group so we can tell whether it is an update procedure
        GroupSymbolImpl group = procCommand.getGroup();

        CommandImpl subCommand = null;
        
        String plan = getPlan(metadata, procCommand);
        
        if (plan == null) {
            return null;
        }
        
        TCQueryParser parser = getQueryResolver().getQueryParser();
        try {
            subCommand = parser.parseProcedure(plan, !(procCommand instanceof StoredProcedureImpl));
        } catch(Exception e) {
             throw new TeiidClientException(e, Messages.gs(Messages.TEIID.TEIID30060, group, procCommand.getClass().getSimpleName()));
        }
        
        return subCommand;
    }

    /** 
     * For a given resolver, this returns the unparsed command.
     * 
     * @param metadata
     * @param group
     * @return
     * @throws Exception
     * @throws Exception
     */
    protected abstract String getPlan(QueryMetadataInterface metadata,
                           GroupSymbolImpl group) throws Exception;
        
	private static void addChanging(TeiidClientParser parser, TempMetadataStore discoveredMetadata,
			GroupContextImpl externalGroups, List<ElementSymbolImpl> elements) {
		List<ElementSymbolImpl> changingElements = new ArrayList<ElementSymbolImpl>(elements.size());
        for(int i=0; i<elements.size(); i++) {
            ElementSymbolImpl virtualElmnt = elements.get(i);
            ElementSymbolImpl changeElement = virtualElmnt.clone();
            changeElement.setType(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass());
            changingElements.add(changeElement);
        }

        addScalarGroup(parser, ProcedureReservedWords.CHANGING, discoveredMetadata, externalGroups, changingElements, false);
	}
        
    /** 
     * @see org.teiid.query.resolver.CommandResolver#resolveCommand(org.teiid.query.sql.lang.CommandImpl, org.teiid.query.metadata.TempMetadataAdapter, boolean)
     */
    public void resolveCommand(CommandImpl command, TempMetadataAdapter metadata, boolean resolveNullLiterals) 
        throws Exception {
        
        ProcedureContainer procCommand = (ProcedureContainer)command;
        
        resolveGroup(metadata, procCommand);
        
        resolveProceduralCommand(procCommand, metadata);
        
        //getPlan(metadata, procCommand);
    }

	private String getPlan(QueryMetadataInterface metadata, ProcedureContainer procCommand)
			throws Exception {
		if(!procCommand.getGroup().isTempTable() && metadata.isVirtualGroup(procCommand.getGroup().getMetadataID())) {
            String plan = getPlan(metadata, procCommand.getGroup());
            if (plan == null && !metadata.isProcedure(procCommand.getGroup().getMetadataID())) {
            	int type = procCommand.getType();
            	//force validation
            	getUpdateInfo(procCommand.getGroup(), metadata, type, true);
            }
            return plan;
        }
		return null;
	}
	
	public UpdateInfo getUpdateInfo(GroupSymbolImpl group, QueryMetadataInterface metadata, int type, boolean validate) throws Exception {
		UpdateInfo info = getUpdateInfo(group, metadata);
		
		if (info == null) {
			return null;
		}
    	if (validate || group.getTeiidVersion().isLessThan(Version.TEIID_8_0.get())) {
    		String error = validateUpdateInfo(group, type, info);
    		if (error != null) {
    			throw new QueryResolverException(error);
    		}
    	}
    	return info;
	}

	public static String validateUpdateInfo(GroupSymbolImpl group, int type, UpdateInfo info) {
		String error = info.getDeleteValidationError();
		String name = "Delete"; //$NON-NLS-1$
		if (type == Command.TYPE_UPDATE) {
			error = info.getUpdateValidationError();
			name = "Update"; //$NON-NLS-1$
		} else if (type == Command.TYPE_INSERT) {
			error = info.getInsertValidationError();
			name = "Insert"; //$NON-NLS-1$
		}
		if (error != null) {
			return Messages.gs(Messages.TEIID.TEIID30061, group, name, error);
		}
		return null;
	}

	public UpdateInfo getUpdateInfo(GroupSymbolImpl group,
			QueryMetadataInterface metadata) throws Exception {
		if (!getQueryResolver().isView(group, metadata)) {
			return null;
		}
		try {
			return getQueryResolver().resolveView(group, metadata.getVirtualPlan(group.getMetadataID()), SQLConstants.Reserved.SELECT, metadata).getUpdateInfo();
		} catch (Exception e) {
			 throw new QueryResolverException(e);
		}
	}
	
    /** 
     * @param metadata
     * @param procCommand
     * @throws Exception
     * @throws Exception
     */
    protected void resolveGroup(TempMetadataAdapter metadata,
                              ProcedureContainer procCommand) throws Exception {
        // Resolve group so we can tell whether it is an update procedure
        GroupSymbolImpl group = procCommand.getGroup();
        ResolverUtil.resolveGroup(group, metadata);
        if (!group.isTempTable()) {
        	procCommand.setUpdateInfo(getUpdateInfo(group, metadata, procCommand.getType(), false));
        }
    }

    public static GroupSymbolImpl addScalarGroup(TeiidClientParser teiidParser, String name, TempMetadataStore metadata, GroupContextImpl externalGroups, List<? extends BaseExpression> symbols) {
    	return addScalarGroup(teiidParser, name, metadata, externalGroups, symbols, true);
    }
    
	public static GroupSymbolImpl addScalarGroup(TeiidClientParser teiidParser, String name, TempMetadataStore metadata, GroupContextImpl externalGroups, List<? extends BaseExpression> symbols, boolean updatable) {
		boolean[] updateArray = new boolean[symbols.size()];
		if (updatable) {
			Arrays.fill(updateArray, true);
		}
		return addScalarGroup(teiidParser, name, metadata, externalGroups, symbols, updateArray);
	}
	
	public static GroupSymbolImpl addScalarGroup(TeiidClientParser teiidParser, String name, TempMetadataStore metadata, GroupContextImpl externalGroups, List<? extends BaseExpression> symbols, boolean[] updatable) {
	    GroupSymbolImpl variables = teiidParser.createASTNode(ASTNodes.GROUP_SYMBOL);
		variables.setName(name);
	    externalGroups.addGroup(variables);
	    TempMetadataID tid = metadata.addTempGroup(name, symbols);
	    tid.setMetadataType(Type.SCALAR);
	    int i = 0;
	    for (TempMetadataID cid : tid.getElements()) {
			cid.setMetadataType(Type.SCALAR);
			cid.setUpdatable(updatable[i++]);
		}
	    variables.setMetadataID(tid);
	    return variables;
	}
	
	/**
	 * Set the appropriate "external" metadata for the given command
	 * @param queryResolver
	 * @param currentCommand 
	 * @param container 
	 * @param type 
	 * @param metadata 
	 * @param inferProcedureResultSetColumns 
	 * @throws Exception 
	 */
	public static void findChildCommandMetadata(TCQueryResolver queryResolver, CommandImpl currentCommand,
			GroupSymbolImpl container, int type, QueryMetadataInterface metadata, boolean inferProcedureResultSetColumns)
			throws Exception {
	    TeiidClientParser parser = queryResolver.getQueryParser().getTeiidParser();
		//find the childMetadata using a clean metadata store
	    TempMetadataStore childMetadata = new TempMetadataStore();
	    TempMetadataAdapter tma = new TempMetadataAdapter(metadata, childMetadata);
	    GroupContextImpl externalGroups = new GroupContextImpl();

		if (currentCommand instanceof TriggerActionImpl) {
			TriggerActionImpl ta = (TriggerActionImpl)currentCommand;
			ta.setView(container);
		    //TODO: it seems easier to just inline the handling here rather than have each of the resolvers check for trigger actions
		    List<ElementSymbolImpl> viewElements = ResolverUtil.resolveElementsInGroup(ta.getView(), metadata);
		    if (type == Command.TYPE_UPDATE || type == Command.TYPE_INSERT) {
		    	addChanging(parser, tma.getMetadataStore(), externalGroups, viewElements);
		    	addScalarGroup(parser, SQLConstants.Reserved.NEW, tma.getMetadataStore(), externalGroups, viewElements, false);
		    }
		    if (type == Command.TYPE_UPDATE || type == Command.TYPE_DELETE) {
		    	addScalarGroup(parser, SQLConstants.Reserved.OLD, tma.getMetadataStore(), externalGroups, viewElements, false);
		    }
		} else if (currentCommand instanceof CreateUpdateProcedureCommandImpl) {
            CreateUpdateProcedureCommandImpl cupc = (CreateUpdateProcedureCommandImpl)currentCommand;
            cupc.setVirtualGroup(container);

            if (type == Command.TYPE_STORED_PROCEDURE) {
                StoredProcedureInfo<SPParameter, QueryNode> info = metadata.getStoredProcedureInfoForProcedure(container.getCanonicalName());
                // Create temporary metadata that defines a group based on either the stored proc
                // name or the stored query name - this will be used later during planning
                String procName = info.getProcedureCallableName();
                
                // Look through parameters to find input elements - these become child metadata
                List<ElementSymbolImpl> tempElements = new ArrayList<ElementSymbolImpl>(info.getParameters().size());
                boolean[] updatable = new boolean[info.getParameters().size()];
                int i = 0;
                for (SPParameter param : info.getParameters()) {
                    if(param.getParameterType() != SPParameter.ParameterInfo.RESULT_SET.index()) {
                        ElementSymbolImpl symbol = (ElementSymbolImpl) param.getParameterSymbol();
                        tempElements.add(symbol);
                        updatable[i++] = param.getParameterType() != SPParameter.ParameterInfo.IN.index();
                    }
                }

                addScalarGroup(parser, procName, childMetadata, externalGroups, tempElements, updatable);
            } else if (type != Command.TYPE_DELETE) {
                createInputChangingMetadata(parser, childMetadata, tma, container, externalGroups);
            }
		} else if (currentCommand instanceof CreateProcedureCommandImpl) {
			CreateProcedureCommandImpl cupc = (CreateProcedureCommandImpl)currentCommand;
			cupc.setVirtualGroup(container);

			if (type == Command.TYPE_STORED_PROCEDURE) {
				StoredProcedureInfo<SPParameter, QueryNode> info = metadata.getStoredProcedureInfoForProcedure(container.getName());
		        // Create temporary metadata that defines a group based on either the stored proc
		        // name or the stored query name - this will be used later during planning
		        String procName = info.getProcedureCallableName();

		        // Look through parameters to find input elements - these become child metadata
		        List<ElementSymbolImpl> tempElements = new ArrayList<ElementSymbolImpl>(info.getParameters().size());
		        boolean[] updatable = new boolean[info.getParameters().size()];
		        int i = 0;
		        List<ElementSymbolImpl> rsColumns = Collections.emptyList();
		        for (SPParameter param : info.getParameters()) {
		            if(param.getParameterType() != SPParameter.ParameterInfo.RESULT_SET.index()) {
		                ElementSymbolImpl symbol = (ElementSymbolImpl) param.getParameterSymbol();
		                tempElements.add(symbol);
		                updatable[i++] = param.getParameterType() != SPParameter.ParameterInfo.IN.index();  
		                if (param.getParameterType() == SPParameter.ParameterInfo.RETURN_VALUE.index()) {
		                	cupc.setReturnVariable(symbol);
		                }
		            } else {
		            	rsColumns = param.getResultSetColumns();
		            }
		        }
		        if (inferProcedureResultSetColumns) {
		        	rsColumns = null;
		        }
		        GroupSymbolImpl gs = addScalarGroup(parser, procName, childMetadata, externalGroups, tempElements, updatable);
		        if (cupc.getReturnVariable() != null) {
		        	ResolverVisitorImpl visitor = new ResolverVisitorImpl(parser.getVersion());
		        	visitor.resolveLanguageObject(cupc.getReturnVariable(), Arrays.asList(gs), metadata);
		        }
		        cupc.setResultSetColumns(rsColumns);
		        //the relational planner will override this with the appropriate value
		        cupc.setProjectedSymbols(rsColumns);
			} else {
    			cupc.setUpdateType(type);
			}
		}

	    queryResolver.setChildMetadata(currentCommand, childMetadata, externalGroups);
	}

	@Removed(Version.TEIID_8_0)
    private static void createInputChangingMetadata(TeiidClientParser teiidParser, TempMetadataStore discoveredMetadata, QueryMetadataInterface metadata, GroupSymbolImpl group, GroupContextImpl externalGroups)
        throws Exception {
        //Look up elements for the virtual group
        List<ElementSymbolImpl> elements = ResolverUtil.resolveElementsInGroup(group, metadata);

        // Create the INPUT variables
        List<ElementSymbolImpl> inputElments = new ArrayList<ElementSymbolImpl>(elements.size());
        for (int i = 0; i < elements.size(); i++) {
            ElementSymbolImpl virtualElmnt = elements.get(i);
            ElementSymbolImpl inputElement = virtualElmnt.clone();
            inputElments.add(inputElement);
        }

        addScalarGroup(teiidParser, ProcedureReservedWords.INPUT, discoveredMetadata, externalGroups, inputElments, false);
        addScalarGroup(teiidParser, ProcedureReservedWords.INPUTS, discoveredMetadata, externalGroups, inputElments, false);

        // Switch type to be boolean for all CHANGING variables
        addChanging(teiidParser, discoveredMetadata, externalGroups, elements);
    }
}
