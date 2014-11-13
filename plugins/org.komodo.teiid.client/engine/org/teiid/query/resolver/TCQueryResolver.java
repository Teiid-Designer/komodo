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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.komodo.spi.query.QueryResolver;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.metadata.QueryNode;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.api.exception.query.QueryValidatorException;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.mapping.relational.TCQueryNode;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.metadata.TempMetadataStore;
import org.teiid.query.parser.TCQueryParser;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.resolver.command.AlterResolver;
import org.teiid.query.resolver.command.DeleteResolver;
import org.teiid.query.resolver.command.DynamicCommandResolver;
import org.teiid.query.resolver.command.ExecResolver;
import org.teiid.query.resolver.command.InsertResolver;
import org.teiid.query.resolver.command.SetQueryResolver;
import org.teiid.query.resolver.command.SimpleQueryResolver;
import org.teiid.query.resolver.command.TempTableResolver;
import org.teiid.query.resolver.command.UpdateProcedureResolver;
import org.teiid.query.resolver.command.UpdateResolver;
import org.teiid.query.resolver.command.XMLQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.ProcedureContainer;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.navigator.DeepPostOrderNavigator;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.visitor.ExpressionMappingVisitor;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.query.validator.AbstractValidationVisitor;
import org.teiid.query.validator.DefaultUpdateValidator;
import org.teiid.query.validator.DefaultUpdateValidator.UpdateInfo;
import org.teiid.query.validator.DefaultUpdateValidator.UpdateType;
import org.teiid.query.validator.ValidationVisitorImpl;
import org.teiid.query.validator.DefaultValidator;
import org.teiid.query.validator.ValidatorFailure;
import org.teiid.query.validator.ValidatorReport;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;


/**
 * <P>The QueryResolver is used between Parsing and QueryValidation. The SQL queries,
 * inserts, updates and deletes are parsed and converted into objects. The language
 * objects have variable names which resolved to fully qualified names using metadata
 * information. The resolver is also used in transforming the values in language
 * objects to their variable types defined in metadata.
 */
public class TCQueryResolver implements QueryResolver<CommandImpl, GroupSymbolImpl, BaseExpression> {

    private final String BINDING_GROUP = "INPUTS"; //$NON-NLS-1$
	private final CommandResolver simpleQueryResolver;
    private final CommandResolver setQueryResolver;
    private final CommandResolver xmlQueryResolver;
    private final ProcedureContainerResolver execResolver;
    private final ProcedureContainerResolver insertResolver;
    private final ProcedureContainerResolver updateResolver;
    private final ProcedureContainerResolver deleteResolver;
    private final CommandResolver updateProcedureResolver;
    private final CommandResolver dynamicCommandResolver;
    private final CommandResolver tempTableResolver;
    private final CommandResolver alterResolver;

    /*
     * The parser that preceded the resolution
     */
    private final TCQueryParser parser;

    /**
     * @param parser
     */
    public TCQueryResolver(TCQueryParser parser) {
        this.parser = parser;

        simpleQueryResolver = new SimpleQueryResolver(this);
        setQueryResolver = new SetQueryResolver(this);
        xmlQueryResolver = new XMLQueryResolver(this);
        execResolver = new ExecResolver(this);
        insertResolver = new InsertResolver(this);
        updateResolver = new UpdateResolver(this);
        deleteResolver = new DeleteResolver(this);
        updateProcedureResolver = new UpdateProcedureResolver(this);
        dynamicCommandResolver = new DynamicCommandResolver(this);
        tempTableResolver = new TempTableResolver(this);
        alterResolver = new AlterResolver(this);
    }

    /**
     * @param teiidVersion
     */
    public TCQueryResolver(TeiidVersion teiidVersion) {
        this(new TCQueryParser(teiidVersion));
    }

    /**
     * @return the query parser
     */
    public TCQueryParser getQueryParser() {
        return parser;
    }

    /**
     * @return teiid parser
     */
    public TeiidClientParser getTeiidParser() {
        return parser.getTeiidParser();
    }

    /**
     * @return parser teiid version
     */
    public TeiidVersion getTeiidVersion() {
        return getTeiidParser().getVersion();
    }

    protected boolean isTeiidVersionOrGreater(Version teiidVersion) {
        TeiidVersion minVersion = getTeiidVersion().getMinimumVersion();
        return minVersion.equals(teiidVersion.get()) || minVersion.isGreaterThan(teiidVersion.get());
    }

    protected boolean isTeiid8OrGreater() {
        return isTeiidVersionOrGreater(Version.TEIID_8_0);
    }

    protected boolean isTeiid87OrGreater() {
        return isTeiidVersionOrGreater(Version.TEIID_8_7);
    }

    public CommandImpl expandCommand(ProcedureContainer proc, QueryMetadataInterface metadata) throws Exception {
        ProcedureContainerResolver cr = (ProcedureContainerResolver)chooseResolver(proc, metadata);
        CommandImpl command = cr.expandCommand(proc, metadata);
        if (command == null) {
            return null;
        }

        if (command instanceof CreateUpdateProcedureCommandImpl) {
            CreateUpdateProcedureCommandImpl cupCommand = (CreateUpdateProcedureCommandImpl)command;
            cupCommand.setUserCommand(proc);
            //if the subcommand is virtual stored procedure, it must have the same
            //projected symbol as its parent.
            if(!cupCommand.isUpdateProcedure()){
                cupCommand.setProjectedSymbols(proc.getProjectedSymbols());
            } 
        }

        resolveCommand(command, proc.getGroup(), proc.getType(), metadata.getDesignTimeMetadata(), false);
        return command;
    }

	/**
	 * This implements an algorithm to resolve all the symbols created by the
	 * parser into real metadata IDs
	 * 
	 * @param command
	 *            Command the SQL command we are running (Select, Update,
	 *            Insert, Delete)
	 * @param metadata
	 *            IQueryMetadataInterface the metadata
	 * @return store of metadata ids representing the resolution of all symbols
	 * @throws Exception
	 */
	public TempMetadataStore resolveCommand(CommandImpl command, QueryMetadataInterface metadata) throws Exception {
		return resolveCommand(command, metadata, true);
	}

	/**
	 * Resolve a command in a given type container and type context.
	 * @param currentCommand
	 * @param container 
	 * @param type The {@link CommandImpl} type
	 * @param metadata 
	 * @param inferProcedureResultSetColumns if true and the currentCommand is a procedure definition, then resolving will set the getResultSetColumns on the command to what is discoverable in the procedure body.
	 * @return metadata object store
	 * @throws Exception 
	 */
    public TempMetadataStore resolveCommand(CommandImpl currentCommand, GroupSymbolImpl container, int type, QueryMetadataInterface metadata, boolean inferProcedureResultSetColumns) throws Exception {
    	ResolverUtil.resolveGroup(container, metadata);
    	switch (type) {
	    case Command.TYPE_QUERY:
	    	ResolverUtil.resolveGroup(container, metadata);
	        QueryNode queryNode = metadata.getVirtualPlan(container.getMetadataID());
            
	        return resolveWithBindingMetadata(currentCommand, metadata, queryNode, false);
    	case Command.TYPE_INSERT:
    	case Command.TYPE_UPDATE:
    	case Command.TYPE_DELETE:
    	case Command.TYPE_STORED_PROCEDURE:
    		ProcedureContainerResolver.findChildCommandMetadata(this, currentCommand, container, type, metadata, inferProcedureResultSetColumns);
    	}
    	return resolveCommand(currentCommand, metadata, false);
    }

    @Override
    public void resolveCommand(CommandImpl command, GroupSymbolImpl gSymbol, int teiidCommandType, QueryMetadataInterface metadata)
        throws Exception {
        resolveCommand(command, gSymbol, teiidCommandType, metadata, true);
    }

    @Override
    public void postResolveCommand(CommandImpl command, GroupSymbolImpl gSymbol, int commandType,
                                   QueryMetadataInterface metadata, List<BaseExpression> projectedSymbols) {

        if (command instanceof CreateUpdateProcedureCommandImpl) {

            /**
             * This was added to designer to avoid a validation failure, see TEIIDDES-624
             */
            CreateUpdateProcedureCommandImpl updateCommand = (CreateUpdateProcedureCommandImpl) command;

            if (updateCommand.getResultsCommand() instanceof DynamicCommandImpl) {
                DynamicCommandImpl dynamicCommand = (DynamicCommandImpl) updateCommand.getResultsCommand();

                if (dynamicCommand.isAsClauseSet()) {
                    updateCommand.setProjectedSymbols(projectedSymbols);
                }
            }
        }
    }

	/**
	 * Bindings are a poor mans input parameters.  They are represented in legacy metadata
	 * by ElementSymbols and placed positionally into the command or by alias symbols
	 * and matched by names.  After resolving bindings will be replaced with their
	 * referenced symbols (input names will not be used) and those symbols will
	 * be marked as external references.
	 * @param currentCommand 
	 * @param metadata 
	 * @param queryNode 
	 * @param replaceBindings 
	 * @return metadata object store
	 * @throws Exception 
	 */
	public TempMetadataStore resolveWithBindingMetadata(CommandImpl currentCommand,
			QueryMetadataInterface metadata, QueryNode queryNode, boolean replaceBindings)
			throws Exception {
		Map<ElementSymbolImpl, ElementSymbolImpl> symbolMap = null;
		TeiidClientParser teiidParser = parser.getTeiidParser();

		if (queryNode.getBindings() != null && queryNode.getBindings().size() > 0) {
			symbolMap = new HashMap<ElementSymbolImpl, ElementSymbolImpl>();

		    // Create ElementSymbols for each InputParameter
		    final List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>(queryNode.getBindings().size());
		    boolean positional = true;
		    for (BaseExpression ses : parseBindings(queryNode)) {
		    	String name = SymbolImpl.getShortName(ses);
		    	if (ses instanceof AliasSymbolImpl) {
		    		ses = ((AliasSymbolImpl)ses).getSymbol();
		    		positional = false;
		    	}
		    	ElementSymbolImpl elementSymbol = (ElementSymbolImpl)ses;
		    	ResolverVisitorImpl visitor = new ResolverVisitorImpl(getTeiidVersion());
		    	visitor.resolveLanguageObject(elementSymbol, metadata);
		    	elementSymbol.setIsExternalReference(true);
		    	if (!positional) {
		    	    if (isTeiid87OrGreater()) {
		    	        ElementSymbolImpl inputSymbol = teiidParser.createASTNode(ASTNodes.ELEMENT_SYMBOL);
		    	        inputSymbol.setName("INPUT" + SymbolImpl.SEPARATOR + name); //$NON-NLS-1$
		    	        inputSymbol.setType(elementSymbol.getType());
		    	        symbolMap.put(inputSymbol, elementSymbol.clone());
		    	    }
		    	    
		    	    ElementSymbolImpl keySymbol = teiidParser.createASTNode(ASTNodes.ELEMENT_SYMBOL);
		    	    keySymbol.setName(BINDING_GROUP + SymbolImpl.SEPARATOR + name);
		    		symbolMap.put(keySymbol, elementSymbol.clone());
		    		elementSymbol.setShortName(name);
		    	}
		        elements.add(elementSymbol);
		    }
		    if (positional) {
		    	ExpressionMappingVisitor emv = new ExpressionMappingVisitor(getTeiidVersion(), null) {
		    		@Override
		    		public BaseExpression replaceExpression(BaseExpression element) {
			    		if (!(element instanceof ReferenceImpl)) {
			    			return element;
			    		}
			    		ReferenceImpl ref = (ReferenceImpl)element;
			    		if (!ref.isPositional()) {
			    			return ref;
			    		}
			    		return elements.get(ref.getIndex()).clone();
		    		}
		    	};
		    	DeepPostOrderNavigator.doVisit(currentCommand, emv);
		    } else {
		        TempMetadataStore rootExternalStore = new TempMetadataStore();
		        
		        GroupContextImpl externalGroups = new GroupContextImpl();
		        
		        ProcedureContainerResolver.addScalarGroup(parser.getTeiidParser(), "INPUT", rootExternalStore, externalGroups, elements); //$NON-NLS-1$
		        ProcedureContainerResolver.addScalarGroup(parser.getTeiidParser(), BINDING_GROUP, rootExternalStore, externalGroups, elements);
		        setChildMetadata(currentCommand, rootExternalStore, externalGroups);
		    }
		}
		TempMetadataStore result = resolveCommand(currentCommand, metadata, false);
		if (replaceBindings && symbolMap != null && !symbolMap.isEmpty()) {
			ExpressionMappingVisitor emv = new ExpressionMappingVisitor(getTeiidVersion(), symbolMap);
			DeepPostOrderNavigator.doVisit(currentCommand, emv);
		}
		return result;
	}

	/**
	 * Bindings are a poor mans input parameters.  They are represented in legacy metadata
	 * by ElementSymbols and placed positionally into the command or by alias symbols
	 * and matched by names.
	 * @param planNode
	 * @return
	 * @throws Exception
	 */
    public List<BaseExpression> parseBindings(QueryNode planNode) throws Exception {
        Collection<String> bindingsCol = planNode.getBindings();
        if (bindingsCol == null) {
            return Collections.emptyList();
        }
        
        List<BaseExpression> parsedBindings = new ArrayList<BaseExpression>(bindingsCol.size());
        for (Iterator<String> bindings=bindingsCol.iterator(); bindings.hasNext();) {
            try {
                BaseExpression binding = parser.parseSelectExpression(bindings.next());
                parsedBindings.add(binding);
            } catch (Exception err) {
                 throw new TeiidClientException(err, Messages.getString(Messages.TEIID.TEIID30063));
            }
        }
        return parsedBindings;
    }

    public TempMetadataStore resolveCommand(CommandImpl currentCommand, QueryMetadataInterface metadata, boolean resolveNullLiterals)
        throws Exception {

//        TODO
//		LogManager.logTrace(org.teiid.logging.LogConstants.CTX_QUERY_RESOLVER, new Object[]{"Resolving command", currentCommand}); //$NON-NLS-1$
        
        TempMetadataAdapter resolverMetadata = null;
        try {
        	TempMetadataStore discoveredMetadata = currentCommand.getTemporaryMetadata();
            if(discoveredMetadata == null) {
            	discoveredMetadata = new TempMetadataStore();
                currentCommand.setTemporaryMetadata(discoveredMetadata);
            }
            
            resolverMetadata = new TempMetadataAdapter(metadata, discoveredMetadata);
            
            // Resolve external groups for command
            Collection<GroupSymbolImpl> externalGroups = currentCommand.getAllExternalGroups();
            for (GroupSymbolImpl extGroup : externalGroups) {
                Object metadataID = extGroup.getMetadataID();
                //make sure that the group is resolved and that it is pointing to the appropriate temp group
                //TODO: this is mainly for XML resolving since it sends external groups in unresolved
                if (metadataID == null || (!(extGroup.getMetadataID() instanceof TempMetadataID) && discoveredMetadata.getTempGroupID(extGroup.getName()) != null)) {
                    metadataID = resolverMetadata.getGroupID(extGroup.getName());
                    extGroup.setMetadataID(metadataID);
                }
            }

            CommandResolver resolver = chooseResolver(currentCommand, resolverMetadata);

            // Resolve this command
            resolver.resolveCommand(currentCommand, resolverMetadata, resolveNullLiterals);            
        } catch(Exception e) {
             throw new QueryResolverException(e);
        }

        // Flag that this command has been resolved.
        currentCommand.setIsResolved(true);
        
        return resolverMetadata.getMetadataStore();
    }

    /**
     * Method chooseResolver.
     * @param command
     * @param metadata
     * @return CommandResolver
     */
    private CommandResolver chooseResolver(CommandImpl command, QueryMetadataInterface metadata)
        throws Exception {

        switch(command.getType()) {
            case Command.TYPE_QUERY:
                if(command instanceof QueryImpl) {
                    if(isXMLQuery((QueryImpl)command, metadata)) {
                        return xmlQueryResolver;
                    }
                    return simpleQueryResolver;
                }
                return setQueryResolver;
            case Command.TYPE_INSERT:               return insertResolver;
            case Command.TYPE_UPDATE:               return updateResolver;
            case Command.TYPE_DELETE:               return deleteResolver;
            case Command.TYPE_STORED_PROCEDURE:     return execResolver;
            case Command.TYPE_TRIGGER_ACTION:		return updateProcedureResolver;
            case Command.TYPE_UPDATE_PROCEDURE:     return updateProcedureResolver;
//            case ICommand.TYPE_BATCHED_UPDATE:       return batchedUpdateResolver;
            case Command.TYPE_DYNAMIC:              return dynamicCommandResolver;
            case Command.TYPE_CREATE:               return tempTableResolver;
            case Command.TYPE_DROP:                 return tempTableResolver;
            case Command.TYPE_ALTER_PROC:           
            case Command.TYPE_ALTER_TRIGGER:        
            case Command.TYPE_ALTER_VIEW:           return alterResolver;
            default:
                throw new AssertionError("Unknown command type"); //$NON-NLS-1$
        }
    }

    /**
     * Check to verify if the query would return XML results.
     * @param query the query to check
     * @param metadata IQueryMetadataInterface the metadata
     * @return true if query is xml query, false otherwise
     * @throws Exception 
     */
    public boolean isXMLQuery(QueryImpl query, QueryMetadataInterface metadata)
     throws Exception {

        if (query.getWith() != null) {
        	return false;
        }

        // Check first group
        FromImpl from = query.getFrom();
        if(from == null){
            //select with no from
            return false;
        }
                
        if (from.getClauses().size() != 1) {
            return false;
        }
        
        FromClauseImpl clause = from.getClauses().get(0);
        
        if (!(clause instanceof UnaryFromClauseImpl)) {
            return false;
        }
        
        GroupSymbolImpl symbol = ((UnaryFromClauseImpl)clause).getGroup();
        
        ResolverUtil.resolveGroup(symbol, metadata);
                
        if (symbol.isProcedure()) {
            return false;
        }
        
        Object groupID = ((UnaryFromClauseImpl)clause).getGroup().getMetadataID();

        return metadata.isXMLGroup(groupID);
    }
    
    /**
     * Resolve just a criteria.  The criteria will be modified so nothing is returned.
     * @param criteria Criteria to resolve
     * @param metadata Metadata implementation
     * @throws Exception
     */
    public void resolveCriteria(CriteriaImpl criteria, QueryMetadataInterface metadata)
        throws Exception {
        ResolverVisitorImpl visitor = new ResolverVisitorImpl(getTeiidVersion());
        visitor.resolveLanguageObject(criteria, metadata);
    }

    public void setChildMetadata(CommandImpl subCommand, CommandImpl parent) {
    	TempMetadataStore childMetadata = parent.getTemporaryMetadata();
        GroupContextImpl parentContext = parent.getExternalGroupContexts();
        
        setChildMetadata(subCommand, childMetadata, parentContext);
    }
    
    public void setChildMetadata(CommandImpl subCommand, TempMetadataStore parentTempMetadata, GroupContextImpl parentContext) {
    	TempMetadataStore tempMetadata = subCommand.getTemporaryMetadata();
        if(tempMetadata == null) {
            subCommand.setTemporaryMetadata(parentTempMetadata.clone());
        } else {
            tempMetadata.getData().putAll(parentTempMetadata.getData());
        }
    
        subCommand.setExternalGroupContexts(parentContext);
    }
    
    public Map<ElementSymbolImpl, BaseExpression> getVariableValues(CommandImpl command, boolean changingOnly, QueryMetadataInterface metadata) throws Exception {
        
        CommandResolver resolver = chooseResolver(command, metadata);
        
        if (resolver instanceof VariableResolver) {
            return ((VariableResolver)resolver).getVariableValues(command, changingOnly, metadata);
        }
        
        return Collections.emptyMap();
    }
    
	public void resolveSubqueries(CommandImpl command,
			TempMetadataAdapter metadata, Collection<GroupSymbolImpl> externalGroups)
			throws Exception {
		for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(command)) {
            setChildMetadata(container.getCommand(), command);
            if (externalGroups != null) {
            	container.getCommand().pushNewResolvingContext(externalGroups);
            }
            resolveCommand(container.getCommand(), metadata.getMetadata(), false);
        }
	}

    public static void validateWithVisitor(AbstractValidationVisitor visitor, QueryMetadataInterface metadata, CommandImpl command)
        throws Exception {

        // Validate with visitor
        ValidatorReport report = DefaultValidator.validate(command, metadata, visitor);
        if (report.hasItems()) {
            ValidatorFailure firstFailure = report.getItems().iterator().next();
            throw new TeiidClientException(firstFailure.getMessage());
        }
    }

	public TCQueryNode resolveView(GroupSymbolImpl virtualGroup, QueryNode qnode,
			String cacheString, QueryMetadataInterface qmi) throws Exception {
		qmi = qmi.getDesignTimeMetadata();
		cacheString = "transformation/" + cacheString; //$NON-NLS-1$
		TCQueryNode cachedNode = (TCQueryNode)qmi.getFromMetadataCache(virtualGroup.getMetadataID(), cacheString);
        if (cachedNode == null) {
        	CommandImpl result = (CommandImpl) qnode.getCommand();
        	List<String> bindings = null;
            if (result == null) {
                try {
                	result = getQueryParser().parseCommand(qnode.getQuery());
                } catch(Exception e) {
                     throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30065, virtualGroup));
                }
                
                bindings = qnode.getBindings();
            } else {
            	result = result.clone();
            }
            if (bindings != null && !bindings.isEmpty()) {
            	resolveWithBindingMetadata(result, qmi, qnode, true);
            } else {
            	resolveCommand(result, qmi, false);
            }
	        validateWithVisitor(new ValidationVisitorImpl(getTeiidVersion()), qmi, result);

	        validateProjectedSymbols(virtualGroup, qmi, result);
            cachedNode = new TCQueryNode(qnode.getQuery());
            cachedNode.setCommand(result);
	        
			if(isView(virtualGroup, qmi)) {
		        String updatePlan = qmi.getUpdatePlan(virtualGroup.getMetadataID());
				String deletePlan = qmi.getDeletePlan(virtualGroup.getMetadataID());
				String insertPlan = qmi.getInsertPlan(virtualGroup.getMetadataID());
				//the elements must be against the view and not the alias
				if (virtualGroup.getDefinition() != null) {
					GroupSymbolImpl group = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
					group.setName(virtualGroup.getNonCorrelationName());
					group.setMetadataID(virtualGroup.getMetadataID());
					virtualGroup = group;
				}
	            List<ElementSymbolImpl> elements = ResolverUtil.resolveElementsInGroup(virtualGroup, qmi);
	    		DefaultUpdateValidator validator = new DefaultUpdateValidator(qmi, determineType(insertPlan), determineType(updatePlan), determineType(deletePlan));
				validator.validate(result, elements);
	    		UpdateInfo info = validator.getUpdateInfo();
	    		cachedNode.setUpdateInfo(info);
			}
	        qmi.addToMetadataCache(virtualGroup.getMetadataID(), cacheString, cachedNode);
        }
		return cachedNode;
	}

	public void validateProjectedSymbols(GroupSymbolImpl virtualGroup,
			QueryMetadataInterface qmi, CommandImpl result)
			throws QueryValidatorException, Exception {
		//ensure that null types match the view
		List<ElementSymbolImpl> symbols = ResolverUtil.resolveElementsInGroup(virtualGroup, qmi);
		List<BaseExpression> projectedSymbols = result.getProjectedSymbols();
		validateProjectedSymbols(virtualGroup, symbols, projectedSymbols);
	}

	public void validateProjectedSymbols(GroupSymbolImpl virtualGroup,
			List<? extends BaseExpression> symbols,
			List<? extends BaseExpression> projectedSymbols)
			throws QueryValidatorException {
		if (symbols.size() != projectedSymbols.size()) {
			 throw new QueryValidatorException(Messages.gs(Messages.TEIID.TEIID30066, virtualGroup, symbols.size(), projectedSymbols.size()));
		}
		DefaultDataTypeManager dataTypeManager = DefaultDataTypeManager.getInstance(getTeiidVersion());
		for (int i = 0; i < projectedSymbols.size(); i++) {
			BaseExpression projectedSymbol = projectedSymbols.get(i);
			
			ResolverUtil.setTypeIfNull(projectedSymbol, symbols.get(i).getType());
			
			if (projectedSymbol.getType() != symbols.get(i).getType()) {
                String symbolTypeName = dataTypeManager.getDataTypeName(symbols.get(i).getType());
			    String projSymbolTypeName = dataTypeManager.getDataTypeName(projectedSymbol.getType());
			    
				throw new QueryValidatorException(Messages.getString(Messages.QueryResolver.wrong_view_symbol_type, virtualGroup, i+1, symbolTypeName, projSymbolTypeName));
			}
		}
	}

	public boolean isView(GroupSymbolImpl virtualGroup,
			QueryMetadataInterface qmi) throws Exception {
		return !(virtualGroup.getMetadataID() instanceof TempMetadataID) && qmi.isVirtualGroup(virtualGroup.getMetadataID());// && qmi.isVirtualModel(qmi.getModelID(virtualGroup.getMetadataID()));
	}
	
	private UpdateType determineType(String plan) {
		UpdateType type = UpdateType.INHERENT;
		if (plan != null) {
		    type = UpdateType.INSTEAD_OF;
		}
		return type;
	}
	
}
