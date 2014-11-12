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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.language.SQLConstants;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.ProcedureContainerResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.VariableResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.GroupSymbolImpl;


/**
 * This class knows how to expand and resolve UDPATE commands.
 */
public class UpdateResolver extends ProcedureContainerResolver implements VariableResolver {

    /**
     * @param queryResolver
     */
    public UpdateResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    /** 
     * @see org.teiid.query.resolver.ProcedureContainerResolver#resolveProceduralCommand(org.teiid.query.sql.lang.CommandImpl, org.teiid.query.metadata.TempMetadataAdapter)
     */
    @Override
    public void resolveProceduralCommand(CommandImpl command, TempMetadataAdapter metadata) 
        throws Exception {

        //Cast to known type
        UpdateImpl update = (UpdateImpl) command;

        // Resolve elements and functions
        Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
        groups.add(update.getGroup());
        ResolverVisitorImpl visitor = new ResolverVisitorImpl(command.getTeiidVersion());
        for (SetClauseImpl clause : update.getChangeList().getClauses()) {
        	visitor.resolveLanguageObject(clause.getSymbol(), groups, null, metadata);
		}
        getQueryResolver().resolveSubqueries(command, metadata, groups);
        visitor.resolveLanguageObject(update, groups, update.getExternalGroupContexts(), metadata);
    }
    
    /** 
     * @param metadata
     * @param group
     * @return
     * @throws Exception
     * @throws Exception
     */
    @Override
    protected String getPlan(QueryMetadataInterface metadata,
                           GroupSymbolImpl group) throws Exception {
        return metadata.getUpdatePlan(group.getMetadataID());
    }

    /** 
     * @see org.teiid.query.resolver.VariableResolver#getVariableValues(CommandImpl, boolean, QueryMetadataInterface)
     */
    @Override
    public Map<ElementSymbolImpl, BaseExpression> getVariableValues(CommandImpl command, boolean changingOnly,
                                 QueryMetadataInterface metadata) throws Exception {
        Map<ElementSymbolImpl, BaseExpression> result = new HashMap<ElementSymbolImpl, BaseExpression>();
        
        UpdateImpl update = (UpdateImpl) command;
        
        Map<ElementSymbolImpl, BaseExpression> changing = update.getChangeList().getClauseMap();
        
        for (Entry<ElementSymbolImpl, BaseExpression> entry : changing.entrySet()) {
        	ElementSymbolImpl leftSymbol = entry.getKey().clone();
            leftSymbol.getGroupSymbol().setName(ProcedureReservedWords.CHANGING);
            leftSymbol.setType(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass());
            
            ConstantImpl constant = getTeiidParser().createASTNode(ASTNodes.CONSTANT);
            constant.setValue(Boolean.TRUE);
            result.put(leftSymbol, constant);
            if (!changingOnly) {
            	leftSymbol = entry.getKey().clone();
            	leftSymbol.getGroupSymbol().setName(SQLConstants.Reserved.NEW);
            	result.put(leftSymbol, entry.getValue());
            }
        }
        
        Collection<ElementSymbolImpl> insertElmnts = ResolverUtil.resolveElementsInGroup(update.getGroup(), metadata);

        insertElmnts.removeAll(changing.keySet());

        Iterator<ElementSymbolImpl> defaultIter = insertElmnts.iterator();
        while(defaultIter.hasNext()) {
            ElementSymbolImpl varSymbol = defaultIter.next().clone();
            varSymbol.getGroupSymbol().setName(ProcedureReservedWords.CHANGING);
            varSymbol.setType(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass());
            
            ConstantImpl constant = getTeiidParser().createASTNode(ASTNodes.CONSTANT);
            constant.setValue(Boolean.FALSE);
            result.put(varSymbol, constant);
        }
        
        return result;
    }

}
