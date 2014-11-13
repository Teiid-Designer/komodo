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
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeMap;

import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.core.util.StringUtil;
import org.teiid.query.mapping.xml.MappingAttributeImpl;
import org.teiid.query.mapping.xml.MappingBaseNodeImpl;
import org.teiid.query.mapping.xml.MappingDocumentImpl;
import org.teiid.query.mapping.xml.MappingElementImpl;
import org.teiid.query.mapping.xml.MappingVisitor;
import org.teiid.query.mapping.xml.Navigator;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID.Type;
import org.teiid.query.metadata.TempMetadataStore;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.CommandResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.visitor.ElementCollectorVisitorImpl;
import org.teiid.query.sql.visitor.GroupCollectorVisitorImpl;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.runtime.client.Messages;


/**
 */
public class XMLQueryResolver extends CommandResolver {
	
	/**
     * @param queryResolver
     */
    public XMLQueryResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    private final class SubSelectVisitor extends MappingVisitor {
		private final List<ElementSymbolImpl> selectElems;
		private final ResolvingNode root;
		private final String mc;
		private String source;

		private SubSelectVisitor(List<ElementSymbolImpl> selectElems,
				ResolvingNode root, String mc) {
			this.selectElems = selectElems;
			this.root = root;
			this.mc = mc;
		}

		@Override
		public void visit(MappingBaseNodeImpl baseNode) {
			if (baseNode.getSource() != null && baseNode.getFullyQualifiedName().equalsIgnoreCase(mc)) {
				source = baseNode.getSource();
			}
		}

		@Override
		public void visit(MappingElementImpl element) {
			visit((MappingBaseNodeImpl)element);
			String nis = element.getNameInSource();
			getMappingClassColumn(nis, element.getFullyQualifiedName());
		}

		private void getMappingClassColumn(String nis, String fqn) {
			if (nis == null || source == null) {
				return;
			}
			String name = nis.substring(0, nis.lastIndexOf('.'));
			if (source.equalsIgnoreCase(name)) {
				selectElems.add(root.find(fqn));
			}
		}

		@Override
		public void visit(MappingAttributeImpl attribute) {
			getMappingClassColumn(attribute.getNameInSource(), attribute.getFullyQualifiedName());
		}
	}

	private final class ResolvingNode {
		ElementSymbolImpl elementSymbol;
		TreeMap<String, ResolvingNode> children = new TreeMap<String, ResolvingNode>(String.CASE_INSENSITIVE_ORDER);
		
		public void add(String name, ElementSymbolImpl symbol) {
			if (name == null) {
				this.elementSymbol = symbol;
				return;
			}
			int index = name.lastIndexOf('.');
			String childName = name;
			if (index >= 0) {
				childName = name.substring(0, index);
				name = name.substring(index + 1, name.length());
			} else {
				childName = null;
			}
			ResolvingNode child = children.get(name);
			if (child == null) {
				child = new ResolvingNode();
				children.put(name, child);
			}
			child.add(childName, symbol);
		}
		
		public <T extends Collection<ElementSymbolImpl>> T values(T values) {
			if (elementSymbol != null) {
				values.add(elementSymbol);
			}
			for (ResolvingNode node : children.values()) {
				node.values(values);
			}
			return values;
		}
		
		public ElementSymbolImpl find(String name) {
			int index = name.lastIndexOf('.');
			String part = name;
			if (index > 0) {
				part = name.substring(index + 1, name.length());
				name = name.substring(0, index);
			} else {
				name = null;
			}
			ResolvingNode r = children.get(part);
			if (r == null) {
				return null;
			}
			if (name == null) {
				return r.elementSymbol;
			}
			return r.find(name);
		}
		
		public void addAll(Collection<ElementSymbolImpl> elems) {
			for (ElementSymbolImpl es : elems) {
				this.add(es.getName(), es);
			}
		}
		
		public List<ElementSymbolImpl> values() {
			return values(new LinkedList<ElementSymbolImpl>());
		}
	}

	/**
     * @see org.teiid.query.resolver.CommandResolver#resolveCommand(org.teiid.query.sql.lang.CommandImpl, TempMetadataAdapter, boolean)
     */
	@Override
    public void resolveCommand(CommandImpl command, TempMetadataAdapter metadata, boolean resolveNullLiterals)
		throws Exception {
		resolveCommand((QueryImpl)command, null, metadata);
	}

	/**
	 * @param query
	 * @param docGroup
	 * @param metadata
	 * @throws Exception
	 */
	public void resolveCommand(QueryImpl query, GroupSymbolImpl docGroup, TempMetadataAdapter metadata)
	throws Exception {
		// set isXML flag
		query.setIsXML(docGroup == null);

		// get the group on this query
		Collection<GroupSymbolImpl> groups = GroupCollectorVisitorImpl.getGroups(query, true);
		GroupSymbolImpl group = groups.iterator().next();

		boolean subQuery = true;
		if (docGroup == null) {
			docGroup = group;
			subQuery = false;
		}
		
		if (subQuery && group.getDefinition() != null) {
			 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30129, group));
		}

		//external groups
        GroupContextImpl externalGroups = query.getExternalGroupContexts();

		// valid elements for select
        List<ElementSymbolImpl> validElems = ResolverUtil.resolveElementsInGroup(docGroup, metadata);
        final ResolvingNode root = new ResolvingNode();
        ResolvingNode selectRoot = root;
        if (subQuery) {
        	validElems = getElementsUnderNode(group.getMetadataID(), validElems, metadata);
        }
        root.addAll(validElems);
		if (subQuery) {
        	//the select can only be to the mapping class itself
        	MappingDocumentImpl doc = (MappingDocumentImpl) metadata.getMappingNode(docGroup.getMetadataID());
    		final String mc = group.getNonCorrelationName();
    		List<ElementSymbolImpl> selectElems = new LinkedList<ElementSymbolImpl>();
            doc.acceptVisitor(new Navigator(true, new SubSelectVisitor(selectElems, root, mc)));
			selectRoot = new ResolvingNode();
			selectRoot.addAll(selectElems);
        }
		
		resolveXMLSelect(subQuery, query, group, selectRoot, metadata);

		// valid elements for criteria and order by
		root.addAll(collectTempElements(group, metadata));

		CriteriaImpl crit = query.getCriteria();
		OrderByImpl orderBy = query.getOrderBy();
		
		if(crit != null) {
	        List<BaseSubqueryContainer> commands = ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(crit);
	        if (!commands.isEmpty()) {
	        	TempMetadataAdapter tma = new TempMetadataAdapter(metadata, new TempMetadataStore());
	        	if (!subQuery) {
	        		addPseudoSubqueryGroups(tma, group, docGroup);
	        	}
		        for (BaseSubqueryContainer<?> subCommand : commands) {
		            getQueryResolver().setChildMetadata(subCommand.getCommand(), query);
		            if (subCommand.getCommand() instanceof QueryImpl && getQueryResolver().isXMLQuery((QueryImpl)subCommand.getCommand(), tma)) {
		            	resolveCommand((QueryImpl)subCommand.getCommand(), docGroup, tma);
		            } else {
		            	getQueryResolver().resolveCommand(subCommand.getCommand(), metadata.getMetadata());
		            }
		        }
	        }

			resolveXMLCriteria(crit, externalGroups, root, metadata);
			// Resolve functions in current query
			ResolverVisitorImpl visitor = new ResolverVisitorImpl(crit.getTeiidVersion());
			visitor.resolveLanguageObject(crit, metadata);
		}

		// resolve any orderby specified on the query
		if(orderBy != null) {
			resolveXMLOrderBy(orderBy, externalGroups, root, metadata);
		}
        
        //we throw exceptions in these cases, since the clauses will not be resolved
        if (query.getGroupBy() != null) {
             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30130));
        }
        
        if (query.getHaving() != null) {
             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30131));
        }	
    }

	private void addPseudoSubqueryGroups(final TempMetadataAdapter metadata,
			GroupSymbolImpl group, GroupSymbolImpl docGroup)
			throws Exception {
		/*
		 * The next section of resolving logic adds in pseduo groups that can be used
		 * in subqueries
		 */
		MappingDocumentImpl doc = (MappingDocumentImpl) metadata.getMappingNode(docGroup.getMetadataID());
		
		final String prefix = group.getNonCorrelationName() + SymbolImpl.SEPARATOR;

        doc.acceptVisitor(new Navigator(true, new MappingVisitor() {
        	@Override
        	public void visit(MappingBaseNodeImpl baseNode) {
        		if (baseNode.getSource() == null) {
        			return;
        		}
        		if (StringUtil.startsWithIgnoreCase(baseNode.getFullyQualifiedName(), prefix)) {
        			try {
        			    GroupSymbolImpl gs = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
        			    gs.setName(baseNode.getFullyQualifiedName());
						ResolverUtil.addTempGroup(metadata, gs, Collections.EMPTY_LIST, false).setMetadataType(Type.XML);
					} catch (Exception e) {
						 throw new RuntimeException(e);
					}
        		}
        	}
        }));
	}

    /**
     * Method resolveXMLSelect.
     * @param select Select clause in user command
     * @param group GroupSymbol
     * @param externalGroups Collection of external groups
     * @param validElements Collection of valid elements
     * @param metadata QueryMetadataInterface the metadata(for resolving criteria on temp groups)
     * @throws Exception if resolving order by fails
     * @throws Exception if resolving fails
     * @throws Exception if resolving fails
     */
	void resolveXMLSelect(boolean subquery, QueryImpl query, GroupSymbolImpl group, ResolvingNode validElements, QueryMetadataInterface metadata)
		throws Exception {
        
        GroupContextImpl externalGroups = null;

		SelectImpl select = query.getSelect();
		// Allow SELECT DISTINCT, which is ignored.  It is meaningless except for
		// self-entity relation using relate() functionality

		List elements = select.getSymbols();
		for (int i = 0; i < elements.size(); i++) {
			BaseExpression ss = (BaseExpression) elements.get(i);

			if (ss instanceof ElementSymbolImpl) {
				// Here we make an assumption that: all elements named with "xml" must use qualified name
				// rather than a simple "xml" in order to distinguish it from "SELECT xml" and
				// "SELECT model.document.xml" case, both of whom stand for selecting the whole document.

				// Then "SELECT xml" or "SELECT model.document.xml" can only stand for one meaning with two cases:
				// 1) whole document
				// 2) whole document, root name = "xml", too

				// There are other cases of "xml", such as, element name = "xml",
				// but those are ok because those will be resolved later as normal elements
				ElementSymbolImpl es = (ElementSymbolImpl)ss;
				String symbolName = es.getName();
				if(!subquery && (symbolName.equalsIgnoreCase("xml") || symbolName.equalsIgnoreCase(group.getName() + ".xml"))) { //$NON-NLS-1$ //$NON-NLS-2$
					if(elements.size() != 1) {
						 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30133));
					}
					select.clearSymbols();
                    MultipleElementSymbolImpl all = getTeiidParser().createASTNode(ASTNodes.MULTIPLE_ELEMENT_SYMBOL);
                    all.setElementSymbols(validElements.values());
					select.addSymbol(all);
					query.setSelect(select);
					return;
				}
                // normal elements
				resolveElement(es, validElements, externalGroups, metadata);
			} else if (ss instanceof MultipleElementSymbolImpl) {
				// Resolve the element with "*" case. such as "A.*"
				// by stripping off the ".*" part,
                MultipleElementSymbolImpl all =  (MultipleElementSymbolImpl)ss;

                // Check for case where we have model.doc.*
                if(all.getGroup() == null || all.getGroup().getName().equalsIgnoreCase(group.getName())) {
                    all.setElementSymbols(validElements.values());
    				return;
                }
                // resovlve the node which is specified
                ElementSymbolImpl elementSymbol = getTeiidParser().createASTNode(ASTNodes.ELEMENT_SYMBOL); 
                elementSymbol.setName(all.getGroup().getName());
                resolveElement(elementSymbol, validElements, externalGroups, metadata);

                // now find all the elements under this node and set as elements.
                List<ElementSymbolImpl> elementsInNode = getElementsUnderNode(elementSymbol.getMetadataID(), validElements.values(), metadata);
                all.setElementSymbols(elementsInNode);
			} else if (ss instanceof ExpressionSymbolImpl) {
                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30134));
            } else if (ss instanceof AliasSymbolImpl) {
                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30135));
            }
            
		}
	}
        
    /**
     * @param group
     * @param metadata
     * @return collection of temporary elements
     * @throws Exception
     */
    public Collection<ElementSymbolImpl> collectTempElements(GroupSymbolImpl group, QueryMetadataInterface metadata)
        throws Exception {
    	ArrayList<ElementSymbolImpl> validElements = new ArrayList<ElementSymbolImpl>();
        // Create GroupSymbol for temp groups and add to groups
        Collection<?> tempGroups = metadata.getXMLTempGroups(group.getMetadataID());
        for (Object tempGroupID : tempGroups) {
            String name = metadata.getFullName(tempGroupID);
            GroupSymbolImpl tempGroup = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
            tempGroup.setName(name);
            tempGroup.setMetadataID(tempGroupID);

            validElements.addAll(ResolverUtil.resolveElementsInGroup(tempGroup, metadata));
        }
        return validElements;
    }

    /**
     * <p> Resolve the criteria specified on the XML query. The elements specified on the criteria should
     * be present on one of the mapping node objects passed to this method, or else be an element on a
     * temporary table at the root of the document model (if a temp table exists there).</p>
     * <p>A Exception will be thrown under the following circumstances:
     * <ol>
     * <li>the elements of the XML criteria cannot be resolved</li>
     * <li>the "@" attribute prefix is used to specify that the node is an attribute, but
     * a document node is found that is an element</li>
     * <li>an element is supplied in the criteria and is ambiguous (multiple
     * document nodes and/or root temp table elements exist which have that name)</li>
     * </ol></p>
     * <p>If an element is supplied in the criteria and is ambiguous (multiple document nodes and/or
     * root temp table elements of that name exist)
     * @param criteria The criteria object that should be resolved
     * @param externalGroups 
     * @param validElements
     * @param metadata QueryMetadataInterface the metadata(for resolving criteria on temp groups)
     * @throws Exception if any of the above fail conditions are met
     */
    public void resolveXMLCriteria(BaseLanguageObject criteria,GroupContextImpl externalGroups, ResolvingNode validElements, QueryMetadataInterface metadata)
        throws Exception {

        // Walk through each element in criteria and check against valid elements
        Collection<ElementSymbolImpl> critElems = ElementCollectorVisitorImpl.getElements(criteria, false);
        for (ElementSymbolImpl critElem : critElems) {
            if(! critElem.isExternalReference()) {
                resolveElement(critElem, validElements, externalGroups, metadata);
            }
        }
    }

    /**
     * Resolve OrderBy clause specified on the XML Query.
     * @param orderBy Order By clause in user command
     * @param group GroupSymbol
     * @param externalGroups Collection of external groups
     * @param validElements Collection of valid elements
     * @param metadata QueryMetadataInterface the metadata(for resolving criteria on temp groups)
     * @throws Exception if resolving order by fails
     * @throws Exception if resolving fails
     * @throws Exception if resolving fails
     */
    void resolveXMLOrderBy(OrderByImpl orderBy, GroupContextImpl externalGroups, ResolvingNode validElements, QueryMetadataInterface metadata)
        throws Exception {

        // Walk through each element in OrderBy clause and check against valid elements
        Collection<ElementSymbolImpl> orderElems = ElementCollectorVisitorImpl.getElements(orderBy, false);
        for (ElementSymbolImpl orderElem : orderElems) {
            resolveElement(orderElem, validElements, externalGroups, metadata);
        }
    }

	/**
	 * Resolve Element method.
	 * @param elem
	 * @param validElements
	 * @param externalGroups
	 * @param metadata
	 * @throws Exception
	 * @throws Exception
	 * @throws Exception
	 */
    void resolveElement(ElementSymbolImpl elem, ResolvingNode validElements, GroupContextImpl externalGroups, QueryMetadataInterface metadata)
        throws Exception {
        
        // Get exact matching name
        String partialName = elem.getName();
        String fullName = partialName;

        ResolvingNode current = validElements;
    	String part = partialName;
        for (int i = 0; partialName != null; i++) {
        	int index = partialName.lastIndexOf('.');
        	if (index < 0) {
        		part = partialName;
        		partialName = null;
        	} else {
        		part = partialName.substring(index + 1, partialName.length());
        		partialName = partialName.substring(0, index);
        	}
			current = current.children.get(part);
			if (current == null) {
				if (i == 0 && part.charAt(0) != '@') {
					//handle attribute case
					part = '@' + part;
					current = validElements.children.get(part);
					if (current != null) {
						continue;
					}
				}
				try {
	                ResolverVisitorImpl visitor = new ResolverVisitorImpl(elem.getTeiidVersion());
	                visitor.resolveLanguageObject(elem, Collections.EMPTY_LIST, externalGroups, metadata);
	                return;
	            } catch (Exception e) {
	                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30136, fullName));
	            }
			}
		}

        List<ElementSymbolImpl> partialMatches = current.values();

        if (partialMatches.size() != 1) {
        	// Found multiple matches
             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30137, fullName));
        } 

        ElementSymbolImpl exactMatch = partialMatches.get(0);
        String name = elem.getOutputName();
        // Resolve based on exact match
        elem.setShortName(exactMatch.getShortName());
        elem.setMetadataID(exactMatch.getMetadataID());
        elem.setType(exactMatch.getType());
        elem.setGroupSymbol(exactMatch.getGroupSymbol());
        if (metadata.useOutputName()) {
        	elem.setOutputName(name);
    	}
    }

    List<ElementSymbolImpl> getElementsUnderNode(Object mid, Collection<ElementSymbolImpl> validElements, QueryMetadataInterface metadata) 
        throws Exception {
        
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        String nodeName = metadata.getFullName(mid);
        for (ElementSymbolImpl validElement : validElements) {
            String qualifiedName = validElement.getName();
            if (StringUtil.startsWithIgnoreCase(qualifiedName, nodeName) && (qualifiedName.length() == nodeName.length() || qualifiedName.charAt(nodeName.length()) == '.')) {
                elements.add(validElement);
            }
        }
        return elements;
    }

}
