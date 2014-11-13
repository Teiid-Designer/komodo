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

package org.teiid.query.resolver.command;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.mapping.relational.TCQueryNode;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.metadata.TempMetadataStore;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.ProcedureContainerResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.ProcedureContainer;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;


/**
 */
public class ExecResolver extends ProcedureContainerResolver {
	
    /**
     * @param queryResolver
     */
    public ExecResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    /**
     * @param metadata
     * @param storedProcedureCommand
     * @param storedProcedureInfo
     * @param oldParams
     * @param namedParameters
     * @throws TeiidClientException
     * @throws TeiidComponentException
     * @throws QueryMetadataException
     */
    private void findCommand7Metadata(QueryMetadataInterface metadata, StoredProcedureImpl storedProcedureCommand, StoredProcedureInfo storedProcedureInfo, Collection<SPParameterImpl> oldParams, boolean namedParameters)
        throws Exception {
        // Cache original input parameter expressions.  Depending on whether
        // the procedure was parsed with named or unnamed parameters, the keys
        // for this map will either be the String names of the parameters or
        // the Integer indices, as entered in the user query
        Map<Object, BaseExpression> inputExpressions = new HashMap<Object, BaseExpression>();
        int adjustIndex = 0;
        for (SPParameterImpl param : oldParams) {
            if(param.getExpression() == null) {
                if (param.getParameterType() == SPParameterImpl.RESULT_SET) {
                    adjustIndex--;  //If this was already resolved, just pretend the result set param doesn't exist
                }
                continue;
            }
            if (namedParameters && param.getParameterType() != SPParameterImpl.RETURN_VALUE) {
                if (inputExpressions.put(param.getName().toUpperCase(), param.getExpression()) != null) {
                    throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30138, param.getName().toUpperCase()));
                }
            } else {
                inputExpressions.put(param.getIndex() + adjustIndex, param.getExpression());
            }
        }

        storedProcedureCommand.clearParameters();
        int origInputs = inputExpressions.size();
        /*
         * Take the values set from the stored procedure implementation, and match up with the
         * types of parameter it is from the metadata and then reset the newly joined parameters
         * into the stored procedure command.  If it is a result set get those columns and place
         * them into the stored procedure command as well.
         */
        List<SPParameterImpl> metadataParams = storedProcedureInfo.getParameters();
        List<SPParameterImpl> clonedMetadataParams = new ArrayList<SPParameterImpl>(metadataParams.size());
        int inputParams = 0;
        int outParams = 0;
        boolean hasReturnValue = false;
        for (SPParameterImpl metadataParameter : metadataParams) {
            if( (metadataParameter.getParameterType()==SPParameter.ParameterInfo.IN.index()) ||
                (metadataParameter.getParameterType()==SPParameter.ParameterInfo.INOUT.index())){

                inputParams++;
            } else if (metadataParameter.getParameterType() == SPParameter.ParameterInfo.OUT.index()) {
                outParams++;
            } else if (metadataParameter.getParameterType() == SPParameter.ParameterInfo.RETURN_VALUE.index()) {
                hasReturnValue = true;
            }
            SPParameterImpl clonedParam = metadataParameter.clone();
            clonedMetadataParams.add(clonedParam);
            storedProcedureCommand.addParameter(clonedParam);
        }
        
        if (storedProcedureCommand.isCalledWithReturn() && !hasReturnValue) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30139, storedProcedureCommand.getGroup()));
        }

        if(!namedParameters && (inputParams > inputExpressions.size())) {
            throw new TeiidClientException(Messages.getString(Messages.ERR.ERR_015_008_0007, inputParams, origInputs, storedProcedureCommand.getGroup()));
        }
        
        // Walk through the resolved parameters and set the expressions from the
        // input parameters
        int exprIndex = 1;
        HashSet<String> expected = new HashSet<String>();
        if (storedProcedureCommand.isCalledWithReturn() && hasReturnValue) {
            for (SPParameterImpl param : clonedMetadataParams) {
                if (param.getParameterType() == SPParameterImpl.RETURN_VALUE) {
                    BaseExpression expr = inputExpressions.remove(exprIndex++);
                    param.setExpression(expr);
                }
            }
        }
        for (SPParameterImpl param : clonedMetadataParams) {
            if(param.getParameterType() == SPParameterImpl.RESULT_SET || param.getParameterType() == SPParameterImpl.RETURN_VALUE) {
                continue;
            }
            if (namedParameters) {
                String nameKey = param.getParameterSymbol().getShortCanonicalName();
                BaseExpression expr = inputExpressions.remove(nameKey);
                // With named parameters, have to check on optional params and default values
                if (expr == null && param.getParameterType() != SPParameter.ParameterInfo.OUT.index()) {
                    expr = ResolverUtil.getDefault(param.getParameterSymbol(), metadata);
                    param.setUsingDefault(true);
                    expected.add(nameKey);
                } 
                param.setExpression(expr);                    
            } else {
                if(param.getParameterType() == SPParameterImpl.OUT) {
                    continue;
                }
                BaseExpression expr = inputExpressions.remove(exprIndex++);
                param.setExpression(expr);
            }
        }
        
        // Check for leftovers, i.e. params entered by user w/ wrong/unknown names
        if (!inputExpressions.isEmpty()) {
            if (namedParameters) {
                throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30141, inputExpressions.keySet(), expected));
            }
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID31113, inputParams, origInputs, storedProcedureCommand.getGroup().toString()));
        }
    }

    private void findCommand8Metadata(QueryMetadataInterface metadata, StoredProcedureImpl storedProcedureCommand, StoredProcedureInfo storedProcedureInfo, Collection<SPParameterImpl> oldParams, boolean namedParameters)
        throws Exception {

        // Cache original input parameter expressions.  Depending on whether
        // the procedure was parsed with named or unnamed parameters, the keys
        // for this map will either be the String names of the parameters or
        // the Integer indices, as entered in the user query
        Map<Integer, BaseExpression> positionalExpressions = new HashMap<Integer, BaseExpression>();
        Map<String, BaseExpression> namedExpressions = new TreeMap<String, BaseExpression>(String.CASE_INSENSITIVE_ORDER);
        int adjustIndex = 0;
        for (SPParameterImpl param : oldParams) {
            if(param.getExpression() == null) {
                if (param.getParameterType() == SPParameterImpl.RESULT_SET) {
                    adjustIndex--;  //If this was already resolved, just pretend the result set param doesn't exist
                }
                continue;
            }
            if (namedParameters && param.getParameterType() != SPParameterImpl.RETURN_VALUE) {
                if (namedExpressions.put(param.getParameterSymbol().getShortName(), param.getExpression()) != null) {
                     throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30138, param.getName()));
                }
            } else {
                positionalExpressions.put(param.getIndex() + adjustIndex, param.getExpression());
            }
        }

        storedProcedureCommand.clearParameters();
        int origInputs = positionalExpressions.size() + namedExpressions.size();
        /*
         * Take the values set from the stored procedure implementation, and match up with the
         * types of parameter it is from the metadata and then reset the newly joined parameters
         * into the stored procedure command.  If it is a result set get those columns and place
         * them into the stored procedure command as well.
         */
        List<SPParameterImpl> metadataParams = storedProcedureInfo.getParameters();
        List<SPParameterImpl> clonedMetadataParams = new ArrayList<SPParameterImpl>(metadataParams.size());
        int inputParams = 0;
        int optionalParams = 0;
        int outParams = 0;
        boolean hasReturnValue = false;
        boolean optional = false;
        boolean varargs = false;
        for (int i = 0; i < metadataParams.size(); i++) {
            SPParameterImpl metadataParameter = metadataParams.get(i);
            if( (metadataParameter.getParameterType()==SPParameter.ParameterInfo.IN.index()) ||
                (metadataParameter.getParameterType()==SPParameter.ParameterInfo.INOUT.index())){
                if (ResolverUtil.hasDefault(metadataParameter.getMetadataID(), metadata) || metadataParameter.isVarArg()) {
                    optional = true;
                    optionalParams++;
                } else {
                    inputParams++;
                    if (optional) {
                        optional = false;
                        inputParams += optionalParams;
                        optionalParams = 0;
                    }
                }
                if (metadataParameter.isVarArg()) {
                    varargs = true;
                }
            } else if (metadataParameter.getParameterType() == SPParameter.ParameterInfo.OUT.index()) {
                outParams++;
                /*
                 * TODO: it would consistent to do the following, but it is a breaking change for procedures that have intermixed out params with in.
                 * we may need to revisit this later
                 */
                //optional = true;
                //optionalParams++;
            } else if (metadataParameter.getParameterType() == SPParameter.ParameterInfo.RETURN_VALUE.index()) {
                hasReturnValue = true;
            }
            SPParameterImpl clonedParam = metadataParameter.clone();
            clonedMetadataParams.add(clonedParam);
            storedProcedureCommand.addParameter(clonedParam);
        }
        
        if (storedProcedureCommand.isCalledWithReturn() && !hasReturnValue) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30139, storedProcedureCommand.getGroup()));
        }

        if(!namedParameters && (inputParams > positionalExpressions.size()) ) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30140, inputParams, inputParams + optionalParams + (varargs?"+":""), origInputs, storedProcedureCommand.getGroup())); //$NON-NLS-1$ //$NON-NLS-2$
        }
        
        // Walk through the resolved parameters and set the expressions from the
        // input parameters
        int exprIndex = 1;
        HashSet<String> expected = new HashSet<String>();
        if (storedProcedureCommand.isCalledWithReturn() && hasReturnValue) {
            for (SPParameterImpl param : clonedMetadataParams) {
                if (param.getParameterType() == SPParameterImpl.RETURN_VALUE) {
                    BaseExpression expr = positionalExpressions.remove(exprIndex++);
                    param.setExpression(expr);
                    break;
                }
            }
        }
        for (SPParameterImpl param : clonedMetadataParams) {
            if(param.getParameterType() == SPParameterImpl.RESULT_SET || param.getParameterType() == SPParameterImpl.RETURN_VALUE) {
                continue;
            }
            if (namedParameters) {
                String nameKey = param.getParameterSymbol().getShortName();
                BaseExpression expr = namedExpressions.remove(nameKey);
                // With named parameters, have to check on optional params and default values
                if (expr == null) {
                    if (param.getParameterType() != SPParameter.ParameterInfo.OUT.index()) {
                        param.setUsingDefault(true);
                        expected.add(nameKey);
                        if (!param.isVarArg()) {
                            expr = ResolverUtil.getDefault(param.getParameterSymbol(), metadata);
                        } else {
                            //zero length array
                            List<BaseExpression> exprs = new ArrayList<BaseExpression>(0);
                            ArraySymbolImpl array = create(ASTNodes.ARRAY_SYMBOL); 
                            array.setExpressions(exprs);
                            array.setImplicit(true);
                            array.setType(param.getClassType());
                            expr = array;
                        }
                    }
                } 
                param.setExpression(expr);                    
            } else {
                BaseExpression expr = positionalExpressions.remove(exprIndex++);
                if(param.getParameterType() == SPParameterImpl.OUT) {
                    if (expr != null) {
                        boolean isRef = expr instanceof ReferenceImpl;
                        if (!isRef || exprIndex <= inputParams + 1) {
                            //for backwards compatibility, this should be treated instead as an input
                            exprIndex--;
                            positionalExpressions.put(exprIndex, expr);
                        } else if (isRef) {
                            //mimics the hack that was in PreparedStatementRequest.
                            ReferenceImpl ref = (ReferenceImpl)expr;
                            ref.setOptional(true); //may be an out
                            /*
                             * Note that there is a corner case here with out parameters intermixed with optional parameters
                             * there's not a good way around this.
                             */
                        }
                    }
                    continue;
                }
                if (expr == null) {
                    if (!param.isVarArg()) {
                        expr = ResolverUtil.getDefault(param.getParameterSymbol(), metadata);
                    }
                    param.setUsingDefault(true);
                } 
                if (param.isVarArg()) {
                    List<BaseExpression> exprs = new ArrayList<BaseExpression>(positionalExpressions.size() + 1);
                    if (expr != null) {
                        exprs.add(expr);
                    }
                    exprs.addAll(positionalExpressions.values());
                    positionalExpressions.clear();
                    ArraySymbolImpl array = create(ASTNodes.ARRAY_SYMBOL); 
                    array.setExpressions(exprs);
                    array.setImplicit(true);
                    expr = array;
                }
                param.setExpression(expr);
            }
        }
        
        // Check for leftovers, i.e. params entered by user w/ wrong/unknown names
        if (!namedExpressions.isEmpty()) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30141, namedExpressions.keySet(), expected));
        }
        if (!positionalExpressions.isEmpty()) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID31113, positionalExpressions.size(), origInputs, storedProcedureCommand.getGroup().toString()));
        }
    }

    /**
     * @see org.teiid.query.resolver.CommandResolver#findCommandMetadata(org.teiid.query.sql.lang.CommandImpl,
     * org.teiid.query.metadata.QueryMetadataInterface)
     */
    private void findCommandMetadata(CommandImpl command, TempMetadataStore discoveredMetadata, QueryMetadataInterface metadata)
        throws Exception {

        StoredProcedureImpl storedProcedureCommand = (StoredProcedureImpl) command;
        
        StoredProcedureInfo storedProcedureInfo = null;
        try {
        	storedProcedureInfo = metadata.getStoredProcedureInfoForProcedure(storedProcedureCommand.getProcedureName());
        } catch (Exception e) {
        	String[] parts = storedProcedureCommand.getProcedureName().split("\\.", 2); //$NON-NLS-1$
	    	if (parts.length > 1 && parts[0].equalsIgnoreCase(metadata.getVirtualDatabaseName())) {
	            try {
	            	storedProcedureInfo = metadata.getStoredProcedureInfoForProcedure(parts[1]);
	            	storedProcedureCommand.setProcedureName(parts[1]);
	            } catch(Exception e1) {
	            } 
	        }
	    	if (storedProcedureInfo == null) {
	    		throw e;
	    	}
        }

        storedProcedureCommand.setUpdateCount(storedProcedureInfo.getUpdateCount());
        storedProcedureCommand.setModelID(storedProcedureInfo.getModelID());
        storedProcedureCommand.setProcedureID(storedProcedureInfo.getProcedureID());
        storedProcedureCommand.setProcedureCallableName(storedProcedureInfo.getProcedureCallableName());

        // Get old parameters as they may have expressions set on them - collect
        // those expressions to copy later into the resolved parameters
        Collection<SPParameterImpl> oldParams = storedProcedureCommand.getParameters();

        boolean namedParameters = storedProcedureCommand.isDisplayNamedParameters();
        
        // If parameter count is zero, then for the purposes of this method treat that
        // as if named parameters were used.  Even though the StoredProcedure was not
        // parsed that way, the user may have entered no parameters with the intention
        // of relying on all default values of all optional parameters.
        if (oldParams.size() == 0 || (oldParams.size() == 1 && storedProcedureCommand.isCalledWithReturn())) {
        	storedProcedureCommand.setDisplayNamedParameters(true);
            namedParameters = true;
        }
        
        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get()))
            findCommand7Metadata(metadata, storedProcedureCommand, storedProcedureInfo, oldParams, namedParameters);
        else
            findCommand8Metadata(metadata, storedProcedureCommand, storedProcedureInfo, oldParams, namedParameters);
        
        // Create temporary metadata that defines a group based on either the stored proc
        // name or the stored query name - this will be used later during planning
        String procName = storedProcedureCommand.getProcedureName();
        List tempElements = storedProcedureCommand.getProjectedSymbols();
        boolean isVirtual = storedProcedureInfo.getQueryPlan() != null;
        discoveredMetadata.addTempGroup(procName, tempElements, isVirtual);

        // Resolve tempElements against new metadata
        GroupSymbolImpl procGroup = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
        procGroup.setName(storedProcedureInfo.getProcedureCallableName());
        procGroup.setProcedure(true);
        TempMetadataID tid = discoveredMetadata.getTempGroupID(procName);
        tid.setOriginalMetadataID(storedProcedureCommand.getProcedureID());
        procGroup.setMetadataID(tid);
        storedProcedureCommand.setGroup(procGroup);
    }
    
    /** 
     * @see org.teiid.query.resolver.ProcedureContainerResolver#resolveProceduralCommand(org.teiid.query.sql.lang.CommandImpl, org.teiid.query.metadata.TempMetadataAdapter)
     */
    @Override
    public void resolveProceduralCommand(CommandImpl command, TempMetadataAdapter metadata) 
        throws Exception {

        findCommandMetadata(command, metadata.getMetadataStore(), metadata);
        
        //Resolve expressions on input parameters
        StoredProcedureImpl storedProcedureCommand = (StoredProcedureImpl) command;
        GroupContextImpl externalGroups = storedProcedureCommand.getExternalGroupContexts();
        for (SPParameterImpl param : storedProcedureCommand.getParameters()) {
            BaseExpression expr = param.getExpression();
            if(expr == null) {
            	continue;
            }
            for (BaseSubqueryContainer<?> container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(expr)) {
                getQueryResolver().setChildMetadata(container.getCommand(), command);
                
                getQueryResolver().resolveCommand(container.getCommand(), metadata.getMetadata());
            }
            try {
            	ResolverVisitorImpl visitor = new ResolverVisitorImpl(expr.getTeiidVersion());
            	visitor.resolveLanguageObject(expr, null, externalGroups, metadata);
            } catch (Exception e) {
            	if (!checkForArray(param, expr)) {
            		throw e;
            	}
            	continue;
            }
            Class<?> paramType = param.getClassType();

            ResolverUtil.setDesiredType(expr, paramType, storedProcedureCommand);
            
            // Compare type of parameter expression against parameter type
            // and add implicit conversion if necessary
            Class<?> exprType = expr.getType();
            if(paramType == null || exprType == null) {
                 throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30143, storedProcedureCommand.getProcedureName(), param.getName()));
            }
            String tgtType = getDataTypeManager().getDataTypeName(paramType);
            String srcType = getDataTypeManager().getDataTypeName(exprType);
            BaseExpression result = null;
                            
            if (param.getParameterType() == SPParameterImpl.RETURN_VALUE || param.getParameterType() == SPParameterImpl.OUT) {
            	if (!ResolverUtil.canImplicitlyConvert(getTeiidVersion(), tgtType, srcType)) {
            		 throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30144, param.getParameterSymbol(), tgtType, srcType));
            	}
            } else {
                try {
                    result = ResolverUtil.convertExpression(expr, tgtType, metadata);
                } catch (Exception e) {
                     throw new TeiidClientException(e, Messages.gs(Messages.TEIID.TEIID30145, new Object[] { param.getParameterSymbol(), srcType, tgtType}));
                }                                                       
                param.setExpression(result);
            }
        }
    }

    /**
     * The param resolving always constructs an array, which is 
     * not appropriate if passing an array directly
     * @return 
     */
	private boolean checkForArray(SPParameterImpl param, BaseExpression expr) {
		if (!param.isVarArg() || !(expr instanceof ArraySymbolImpl)) {
			return false;
		}
		ArraySymbolImpl array = (ArraySymbolImpl)expr;
		if (array.getExpressions().size() == 1) {
			BaseExpression first = array.getExpressions().get(0);
			if (first.getType() != null && first.getType() == array.getType()) {
				param.setExpression(first);
				return true;
			} 
		}
		return false;
	}
    
    @Override
    protected void resolveGroup(TempMetadataAdapter metadata,
                                ProcedureContainer procCommand) throws Exception {
        //Do nothing
    }

    /** 
     * @throws Exception 
     * @see org.teiid.query.resolver.ProcedureContainerResolver#getPlan(org.teiid.query.metadata.QueryMetadataInterface, org.teiid.query.sql.symbol.GroupSymbolImpl)
     */
    @Override
    protected String getPlan(QueryMetadataInterface metadata,
                             GroupSymbolImpl group) throws Exception {
        StoredProcedureInfo<SPParameterImpl, TCQueryNode> storedProcedureInfo = metadata.getStoredProcedureInfoForProcedure(group.getName());
        
        //if there is a query plan associated with the procedure, get it.
        TCQueryNode plan = storedProcedureInfo.getQueryPlan();
        
        if (plan.getQuery() == null) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30146, group));
        }
        
        return plan.getQuery();
    }
}
