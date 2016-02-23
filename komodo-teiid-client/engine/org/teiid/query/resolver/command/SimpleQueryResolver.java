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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.metadata.QueryMetadataInterface.SupportConstants;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.api.exception.query.UnresolvedSymbolDescription;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.CommandResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.lang.ArrayTableImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.TableFunctionReferenceImpl;
import org.teiid.query.sql.lang.TextColumnImpl;
import org.teiid.query.sql.lang.TextTableImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.WithQueryCommandImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.navigator.PostOrderNavigator;
import org.teiid.query.sql.navigator.PreOrPostOrderNavigator;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.visitor.ElementCollectorVisitorImpl;
import org.teiid.query.sql.visitor.ExpressionMappingVisitor;
import org.teiid.runtime.client.Messages;

/**
 *
 */
public class SimpleQueryResolver extends CommandResolver {

    /**
     * @param queryResolver
     */
    public SimpleQueryResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    /** 
     * @see org.teiid.query.resolver.CommandResolver#resolveCommand(org.teiid.query.sql.lang.CommandImpl, org.teiid.query.metadata.TempMetadataAdapter, boolean)
     */
    @Override
    public void resolveCommand(CommandImpl command, TempMetadataAdapter metadata, boolean resolveNullLiterals)
        throws Exception {

    	QueryImpl query = (QueryImpl) command;
    	
    	resolveWith(metadata, query);
        
        try {
            QueryResolverVisitor qrv = new QueryResolverVisitor(query, metadata);
            qrv.visit(query);
            ResolverVisitorImpl visitor = (ResolverVisitorImpl)qrv.getVisitor();
			visitor.throwException(true);
			if (visitor.hasUserDefinedAggregate() && getTeiidVersion().isGreaterThanOrEqualTo(Version.TEIID_8_6.get())) {
				ExpressionMappingVisitor emv = new ExpressionMappingVisitor(getTeiidVersion(), null) {
					@Override
                    public BaseExpression replaceExpression(BaseExpression element) {
						if (element instanceof FunctionImpl && !(element instanceof BaseAggregateSymbol) && ((FunctionImpl) element).isAggregate()) {
							FunctionImpl f = (FunctionImpl)element;
							BaseAggregateSymbol as = create(ASTNodes.AGGREGATE_SYMBOL);
							as.setName(f.getName());
							as.setDistinct(false);
							as.setArgs(f.getArgs());
							as.setType(f.getType());
							as.setFunctionDescriptor(f.getFunctionDescriptor());
							return as;
						}
						return element;
					}
				};
				PreOrPostOrderNavigator.doVisit(query, emv, PreOrPostOrderNavigator.POST_ORDER);
			}
        } catch (Exception e) {
            if (e.getCause() instanceof Exception) {
                throw (Exception)e.getCause();
            }
            throw e;
        }
                                       
        if (query.getLimit() != null) {
            ResolverUtil.resolveLimit(query.getLimit());
        }
        
        if (query.getOrderBy() != null) {
        	ResolverUtil.resolveOrderBy(query.getOrderBy(), query, metadata);
        }
        
        List<BaseExpression> symbols = query.getSelect().getProjectedSymbols();
        
        if (query.getInto() != null) {
            GroupSymbolImpl symbol = query.getInto().getGroup();
            ResolverUtil.resolveImplicitTempGroup(metadata, symbol, symbols);
        } else if (resolveNullLiterals) {
            ResolverUtil.resolveNullLiterals(symbols);
        }
    }

	/**
	 * @param metadata
	 * @param query
	 * @throws Exception
	 */
	public void resolveWith(TempMetadataAdapter metadata,
			QueryCommandImpl query) throws Exception {
		if (query.getWith() == null) {
			return;
		}
		LinkedHashSet<GroupSymbolImpl> discoveredGroups = new LinkedHashSet<GroupSymbolImpl>();
		for (WithQueryCommandImpl obj : query.getWith()) {
            QueryCommandImpl queryExpression = obj.getCommand();
            
            getQueryResolver().setChildMetadata(queryExpression, query);
            
            getQueryResolver().resolveCommand(queryExpression, metadata.getMetadata(), false);

            if (!discoveredGroups.add(obj.getGroupSymbol())) {
            	 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30101, obj.getGroupSymbol()));
            }
            List<? extends BaseExpression> projectedSymbols = obj.getCommand().getProjectedSymbols();
            if (obj.getColumns() != null && !obj.getColumns().isEmpty()) {
            	if (obj.getColumns().size() != projectedSymbols.size()) {
            		 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30102, obj.getGroupSymbol()));
            	}
            	Iterator<ElementSymbolImpl> iter = obj.getColumns().iterator();
            	for (BaseExpression singleElementSymbol : projectedSymbols) {
            		ElementSymbolImpl es = iter.next();
            		es.setType(singleElementSymbol.getType());
				}
            	projectedSymbols = obj.getColumns();
            } 
            TempMetadataID id = ResolverUtil.addTempGroup(metadata, obj.getGroupSymbol(), projectedSymbols, true);
            obj.getGroupSymbol().setMetadataID(metadata.getMetadataStore().getTempGroupID(obj.getGroupSymbol().getName()));
            obj.getGroupSymbol().setIsTempTable(true);
            List<GroupSymbolImpl> groups = Collections.singletonList(obj.getGroupSymbol());
            ResolverVisitorImpl visitor = new ResolverVisitorImpl(obj.getTeiidVersion());
            if (obj.getColumns() != null && !obj.getColumns().isEmpty()) {
	            for (BaseExpression singleElementSymbol : projectedSymbols) {
	                visitor.resolveLanguageObject(singleElementSymbol, groups, metadata);
				}
            }
            if (obj.getColumns() != null && !obj.getColumns().isEmpty()) {
            	Iterator<ElementSymbolImpl> iter = obj.getColumns().iterator();
                for (TempMetadataID colid : id.getElements()) {
            		ElementSymbolImpl es = iter.next();
            		es.setMetadataID(colid);
            		es.setGroupSymbol(obj.getGroupSymbol());
				}
            }
        }
	}

    private GroupSymbolImpl resolveAllInGroup(MultipleElementSymbolImpl allInGroupSymbol, Set<GroupSymbolImpl> groups, QueryMetadataInterface metadata) throws Exception {       
        String groupAlias = allInGroupSymbol.getGroup().getName();
        List<GroupSymbolImpl> groupSymbols = ResolverUtil.findMatchingGroups(groupAlias, groups, metadata);
        if(groupSymbols.isEmpty() || groupSymbols.size() > 1) {
            String msg = Messages.getString(groupSymbols.isEmpty() ? Messages.ERR.ERR_015_008_0047 : Messages.QueryResolver.ambiguous_all_in_group, allInGroupSymbol);
            QueryResolverException qre = new QueryResolverException(msg);
            qre.addUnresolvedSymbol(new UnresolvedSymbolDescription(allInGroupSymbol.toString(), msg));
            throw qre;
        }
        GroupSymbolImpl gs = allInGroupSymbol.getGroup();
        allInGroupSymbol.setGroup(groupSymbols.get(0).clone());
        return groupSymbols.get(0);
    }
    
    /**
     *
     */
    public class QueryResolverVisitor extends PostOrderNavigator {

        private LinkedHashSet<GroupSymbolImpl> currentGroups = new LinkedHashSet<GroupSymbolImpl>();
        private LinkedList<GroupSymbolImpl> discoveredGroups = new LinkedList<GroupSymbolImpl>();
        private List<GroupSymbolImpl> implicitGroups = new LinkedList<GroupSymbolImpl>();
        private TempMetadataAdapter metadata;
        private QueryImpl query;
        private boolean allowImplicit = true;
        
        /**
         * @param query
         * @param metadata
         */
        public QueryResolverVisitor(QueryImpl query, TempMetadataAdapter metadata) {
            super(new ResolverVisitorImpl(query.getTeiidVersion(), metadata, null, query.getExternalGroupContexts()));
            ResolverVisitorImpl visitor = (ResolverVisitorImpl)getVisitor();
            visitor.setGroups(currentGroups);
            this.query = query;
            this.metadata = metadata;
        }
        
        @Override
        protected void postVisitVisitor(BaseLanguageObject obj) {
            super.postVisitVisitor(obj);
            ResolverVisitorImpl visitor = (ResolverVisitorImpl)getVisitor();
            try {
				visitor.throwException(false);
			} catch (Exception e) {
				 throw new RuntimeException(e);
			}
        }
                
        /**
         * Resolving a Query requires a special ordering
         */
        @Override
        public void visit(QueryImpl obj) {
            visitNode(obj.getInto());
            visitNode(obj.getFrom());
            visitNode(obj.getCriteria());
            visitNode(obj.getGroupBy());
            visitNode(obj.getHaving());
            visitNode(obj.getSelect());        
            visitNode(obj.getLimit());
        }
        
        @Override
        public void visit(GroupSymbolImpl obj) {
            try {
                ResolverUtil.resolveGroup(obj, metadata);
            } catch (Exception err) {
                 throw new RuntimeException(err);
            }
        }
                        
        private void resolveSubQuery(BaseSubqueryContainer<?> obj, Collection<GroupSymbolImpl> externalGroups) {
            CommandImpl command = obj.getCommand();
            
            getQueryResolver().setChildMetadata(command, query);
            command.pushNewResolvingContext(externalGroups);
            
            try {
                getQueryResolver().resolveCommand(command, metadata.getMetadata(), false);
            } catch (Exception err) {
                 throw new RuntimeException(err);
            }
        }
        
        @Override
        public void visit(MultipleElementSymbolImpl obj) {
        	// Determine group that this symbol is for
            try {
                List<ElementSymbolImpl> elementSymbols = new ArrayList<ElementSymbolImpl>();
                Collection<GroupSymbolImpl> groups = currentGroups;
                if (obj.getGroup() != null) {
                	groups = Arrays.asList(resolveAllInGroup(obj, currentGroups, metadata));
                }
                for (GroupSymbolImpl group : groups) {
                    elementSymbols.addAll(resolveSelectableElements(group));
                }
                obj.setElementSymbols(elementSymbols);
            } catch (Exception err) {
                 throw new RuntimeException(err);
            } 
        }

        private List<ElementSymbolImpl> resolveSelectableElements(GroupSymbolImpl group) throws Exception {
            List<ElementSymbolImpl> elements = ResolverUtil.resolveElementsInGroup(group, metadata);
            
            List<ElementSymbolImpl> result = new ArrayList<ElementSymbolImpl>(elements.size());
   
            // Look for elements that are not selectable and remove them
            for (ElementSymbolImpl element : elements) {
                if(metadata.elementSupports(element.getMetadataID(), SupportConstants.Element.SELECT) && !metadata.isPseudo(element.getMetadataID())) {
                    element = element.clone();
                    element.setGroupSymbol(group);
                	result.add(element);
                }
            }
            return result;
        }
        
        @Override
        public void visit(ScalarSubqueryImpl obj) {
            resolveSubQuery(obj, this.currentGroups);
        }
        
        @Override
        public void visit(ExistsCriteriaImpl obj) {
            resolveSubQuery(obj, this.currentGroups);
        }
        
        @Override
        public void visit(SubqueryCompareCriteriaImpl obj) {
            visitNode(obj.getLeftExpression());
            resolveSubQuery(obj, this.currentGroups);
            postVisitVisitor(obj);
        }
        
        @Override
        public void visit(SubquerySetCriteriaImpl obj) {
            visitNode(obj.getExpression());
            resolveSubQuery(obj, this.currentGroups);
            postVisitVisitor(obj);
        }
        
        @Override
        public void visit(TextTableImpl obj) {
        	LinkedHashSet<GroupSymbolImpl> saved = preTableFunctionReference(obj);
        	this.visitNode(obj.getFile());
        	try {
				obj.setFile(ResolverUtil.convertExpression(obj.getFile(), DefaultDataTypeManager.DefaultDataTypes.CLOB.getId(), metadata));
			} catch (Exception e) {
				 throw new RuntimeException(e);
			}
			postTableFunctionReference(obj, saved);
            //set to fixed width if any column has width specified
            for (TextColumnImpl col : obj.getColumns()) {
				if (col.getWidth() != null) {
					obj.setFixedWidth(true);
					break;
				}
			}
        }
        
        @Override
        public void visit(ArrayTableImpl obj) {
        	LinkedHashSet<GroupSymbolImpl> saved = preTableFunctionReference(obj);
        	visitNode(obj.getArrayValue());
			postTableFunctionReference(obj, saved);
        }
        
        @Override
        public void visit(XMLTableImpl obj) {
        	LinkedHashSet<GroupSymbolImpl> saved = preTableFunctionReference(obj);
        	visitNodes(obj.getPassing());
			postTableFunctionReference(obj, saved);
			try {
	    		ResolverUtil.setDesiredType(obj.getPassing(), obj);
				obj.compileXqueryExpression();
				for (XMLColumnImpl column : obj.getColumns()) {
					if (column.getDefaultExpression() == null) {
						continue;
					}
					visitNode(column.getDefaultExpression());
					BaseExpression ex = ResolverUtil.convertExpression(column.getDefaultExpression(), getDataTypeManager().getDataTypeName(column.getSymbol().getType()), metadata);
					column.setDefaultExpression(ex);
				}
			} catch (Exception e) {
				 throw new RuntimeException(e);
			}
        }
        
        @Override
        public void visit(ObjectTableImpl obj) {
        	LinkedHashSet<GroupSymbolImpl> saved = preTableFunctionReference(obj);
        	visitNodes(obj.getPassing());
			postTableFunctionReference(obj, saved);
			try {
	    		ResolverUtil.setDesiredType(obj.getPassing(), obj, DefaultDataTypeManager.DefaultDataTypes.OBJECT.getTypeClass());
				for (ObjectColumnImpl column : obj.getColumns()) {
					if (column.getDefaultExpression() == null) {
						continue;
					}
					visitNode(column.getDefaultExpression());
					BaseExpression ex = ResolverUtil.convertExpression(column.getDefaultExpression(), getDataTypeManager().getDataTypeName(column.getSymbol().getType()), metadata);
					column.setDefaultExpression(ex);
				}
			} catch (Exception e) {
				 throw new RuntimeException(e);
			}
        }
        
        /**
		 * @param tfr  
         * @return set of group symbols
		 */
        public LinkedHashSet<GroupSymbolImpl> preTableFunctionReference(TableFunctionReferenceImpl tfr) {
        	LinkedHashSet<GroupSymbolImpl> saved = new LinkedHashSet<GroupSymbolImpl>(this.currentGroups);
        	if (allowImplicit) {
        		currentGroups.addAll(this.implicitGroups);
        	}
        	return saved;
        }

        /**
         * @param obj
         * @param saved
         */
        public void postTableFunctionReference(TableFunctionReferenceImpl obj, LinkedHashSet<GroupSymbolImpl> saved) {
			//we didn't create a true external context, so we manually mark external
			for (ElementSymbolImpl symbol : ElementCollectorVisitorImpl.getElements(obj, false)) {
				if (symbol.isExternalReference()) {
					continue;
				}
				if (implicitGroups.contains(symbol.getGroupSymbol())) {
					symbol.setIsExternalReference(true);
				}
			}
			if (allowImplicit) {
	        	this.currentGroups.clear();
	        	this.currentGroups.addAll(saved);
			}
            discoveredGroup(obj.getGroupSymbol());
            try {
                ResolverUtil.addTempGroup(metadata, obj.getGroupSymbol(), obj.getProjectedSymbols(), false);
            } catch (Exception err) {
                 throw new RuntimeException(err);
            }
            obj.getGroupSymbol().setMetadataID(metadata.getMetadataStore().getTempGroupID(obj.getGroupSymbol().getName()));
            //now resolve the projected symbols
            Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
            groups.add(obj.getGroupSymbol());
            ResolverVisitorImpl visitor = new ResolverVisitorImpl(obj.getTeiidVersion());
            for (ElementSymbolImpl symbol : obj.getProjectedSymbols()) {
                try {
					visitor.resolveLanguageObject(symbol, groups, null, metadata);
				} catch (Exception e) {
					 throw new RuntimeException(e);
				}				
			}
        }
        
        @Override
        public void visit(SubqueryFromClauseImpl obj) {
        	Collection<GroupSymbolImpl> externalGroups = this.currentGroups;
        	if (obj.isTable() && allowImplicit) {
        		externalGroups = new ArrayList<GroupSymbolImpl>(externalGroups);
        		externalGroups.addAll(this.implicitGroups);
        	}
            resolveSubQuery(obj, externalGroups);
            discoveredGroup(obj.getGroupSymbol());
            try {
                ResolverUtil.addTempGroup(metadata, obj.getGroupSymbol(), obj.getCommand().getProjectedSymbols(), false);
            } catch (Exception err) {
                 throw new RuntimeException(err);
            }
            obj.getGroupSymbol().setMetadataID(metadata.getMetadataStore().getTempGroupID(obj.getGroupSymbol().getName())); 
        }
                        
        @Override
        public void visit(UnaryFromClauseImpl obj) {
            GroupSymbolImpl group = obj.getGroup();
            visitNode(group);
            try {
	            if (!group.isProcedure() && metadata.isXMLGroup(group.getMetadataID())) {
	                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30112));
	            }
	            discoveredGroup(group);
	            if (group.isProcedure()) {
	                createProcRelational(obj);
	            }
            } catch(Exception e) {
                 throw new RuntimeException(e);
			}
        }
        
        private void discoveredGroup(GroupSymbolImpl group) {
        	discoveredGroups.add(group);
        	if (allowImplicit) {
        		implicitGroups.add(group);
        	}
        }

		private void createProcRelational(UnaryFromClauseImpl obj) throws Exception {
			GroupSymbolImpl group = obj.getGroup();
			String fullName = metadata.getFullName(group.getMetadataID());
			String queryName = group.getName();
			
			StoredProcedureInfo storedProcedureInfo = metadata.getStoredProcedureInfoForProcedure(fullName);

			StoredProcedureImpl storedProcedureCommand = getTeiidParser().createASTNode(ASTNodes.STORED_PROCEDURE);
			storedProcedureCommand.setProcedureRelational(true);
			storedProcedureCommand.setProcedureName(fullName);
			
			List<SPParameterImpl> metadataParams = storedProcedureInfo.getParameters();
			
			QueryImpl procQuery = getTeiidParser().createASTNode(ASTNodes.QUERY);
			FromImpl from = getTeiidParser().createASTNode(ASTNodes.FROM);
			SubqueryFromClauseImpl subqueryFromClause = getTeiidParser().createASTNode(ASTNodes.SUBQUERY_FROM_CLAUSE);
			subqueryFromClause.setName("X"); //$NON-NLS-1$
			subqueryFromClause.setCommand(storedProcedureCommand);
			from.addClause(subqueryFromClause);
			procQuery.setFrom(from);
			SelectImpl select = getTeiidParser().createASTNode(ASTNodes.SELECT);
			MultipleElementSymbolImpl mes = getTeiidParser().createASTNode(ASTNodes.MULTIPLE_ELEMENT_SYMBOL);
			mes.setName("X"); //$NON-NLS-1$
			select.addSymbol(mes);
			procQuery.setSelect(select);
			
			List<String> accessPatternElementNames = new LinkedList<String>();
			
			int paramIndex = 1;
			
			for (SPParameterImpl metadataParameter : metadataParams) {
			    SPParameterImpl clonedParam = metadataParameter.clone();
			    if (clonedParam.getParameterType()==SPParameter.ParameterInfo.IN.index() || metadataParameter.getParameterType()==SPParameter.ParameterInfo.INOUT.index()) {
			        ElementSymbolImpl paramSymbol = clonedParam.getParameterSymbol();
			        ReferenceImpl ref = getTeiidParser().createASTNode(ASTNodes.REFERENCE);
			        ref.setExpression(paramSymbol);
			        clonedParam.setExpression(ref);
			        clonedParam.setIndex(paramIndex++);
			        storedProcedureCommand.addParameter(clonedParam);
			        
			        String aliasName = paramSymbol.getShortName();
			        
			        if (metadataParameter.getParameterType()==SPParameter.ParameterInfo.INOUT.index()) {
			            aliasName += "_IN"; //$NON-NLS-1$
			        }

			        ExpressionSymbolImpl es = getTeiidParser().createASTNode(ASTNodes.EXPRESSION_SYMBOL);
			        es.setName(paramSymbol.getShortName());
			        es.setExpression(ref);
			        AliasSymbolImpl newSymbol = getTeiidParser().createASTNode(ASTNodes.ALIAS_SYMBOL);
			        newSymbol.setName(aliasName);
			        newSymbol.setSymbol(es);

			        select.addSymbol(newSymbol);
			        accessPatternElementNames.add(queryName + SymbolImpl.SEPARATOR + aliasName);
			    }
			}
			
			getQueryResolver().resolveCommand(procQuery, metadata.getMetadata());
			
			List<BaseExpression> projectedSymbols = procQuery.getProjectedSymbols();
			
			Set<String> foundNames = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
			
			for (BaseExpression ses : projectedSymbols) {
			    if (!foundNames.add(SymbolImpl.getShortName(ses))) {
			         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30114, fullName));
			    }
			}
			
			TempMetadataID id = metadata.getMetadataStore().getTempGroupID(queryName);

			if (id == null) {
			    metadata.getMetadataStore().addTempGroup(queryName, projectedSymbols, true);
			    
			    id = metadata.getMetadataStore().getTempGroupID(queryName);
			    id.setOriginalMetadataID(storedProcedureCommand.getProcedureID());
			    if (!accessPatternElementNames.isEmpty()) {
				    List<TempMetadataID> accessPatternIds = new LinkedList<TempMetadataID>();
				    
				    for (String name : accessPatternElementNames) {
				        accessPatternIds.add(metadata.getMetadataStore().getTempElementID(name));
				    }
				    
				    id.setAccessPatterns(Arrays.asList(new TempMetadataID("procedure access pattern", accessPatternIds))); //$NON-NLS-1$
			    }
			}
			
			group.setMetadataID(id);
			
		    obj.setExpandedCommand(procQuery);
		}
        
        /** 
         * @see org.teiid.query.sql.navigator.PreOrPostOrderNavigator#visit(org.teiid.query.sql.lang.IntoImpl)
         */
        @Override
        public void visit(IntoImpl obj) {
            if (!obj.getGroup().isImplicitTempGroupSymbol()) {
                super.visit(obj);
            }
        }

        @Override
        public void visit(JoinPredicateImpl obj) {
            assert currentGroups.isEmpty();
        	List<GroupSymbolImpl> tempImplicitGroups = new ArrayList<GroupSymbolImpl>(discoveredGroups);
        	discoveredGroups.clear();
            visitNode(obj.getLeftClause());
            List<GroupSymbolImpl> leftGroups = new ArrayList<GroupSymbolImpl>(discoveredGroups);
        	discoveredGroups.clear();
            visitNode(obj.getRightClause());
            discoveredGroups.addAll(leftGroups);
            addDiscoveredGroups();
            visitNodes(obj.getJoinCriteria());
            discoveredGroups.addAll(currentGroups);
            currentGroups.clear();
            discoveredGroups.addAll(tempImplicitGroups);
        }

		private void addDiscoveredGroups() {
			for (GroupSymbolImpl group : discoveredGroups) {
				if (!this.currentGroups.add(group)) {
	                String msg = Messages.getString(Messages.ERR.ERR_015_008_0046, group.getName());
	                QueryResolverException qre = new QueryResolverException(msg);
                    qre.addUnresolvedSymbol(new UnresolvedSymbolDescription(group.toString(), msg));
	                 throw new RuntimeException(qre);
	            }
			}
            discoveredGroups.clear();
		}
                
        @Override
        public void visit(FromImpl obj) {
            assert currentGroups.isEmpty();
            for (FromClauseImpl clause : obj.getClauses()) {
				checkImplicit(clause);
			}
            super.visit(obj);
            addDiscoveredGroups();
        }

		private void checkImplicit(FromClauseImpl clause) {
			if (clause instanceof JoinPredicateImpl) {
				JoinPredicateImpl jp = (JoinPredicateImpl)clause;
				if (JoinTypeTypes.JOIN_FULL_OUTER.equals(jp.getJoinType().getKind()) || JoinTypeTypes.JOIN_RIGHT_OUTER.equals(jp.getJoinType().getKind())) {
					allowImplicit = false;
					return;
				}
				checkImplicit(jp.getLeftClause());
				if (allowImplicit) {
					checkImplicit(jp.getRightClause());
				}
			}
		}
		
		@Override
		public void visit(LimitImpl obj) {
			super.visit(obj);
			if (obj.getOffset() != null) {
				ResolverUtil.setTypeIfNull(obj.getOffset(), DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
				try {
					obj.setOffset(ResolverUtil.convertExpression(obj.getOffset(), DefaultDataTypeManager.DefaultDataTypes.INTEGER.getId(), metadata));
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
			if (obj.getRowLimit() != null) {
                ResolverUtil.setTypeIfNull(obj.getRowLimit(), DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
                try {
                    obj.setRowLimit(ResolverUtil.convertExpression(obj.getRowLimit(), DefaultDataTypeManager.DefaultDataTypes.INTEGER.getId(), metadata));
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
		}
    }
}
