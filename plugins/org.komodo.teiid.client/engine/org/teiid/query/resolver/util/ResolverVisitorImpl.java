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

package org.teiid.query.resolver.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.sql.ResolverVisitor;
import org.komodo.spi.query.sql.symbol.ElementSymbol.DisplayMode;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.udf.FunctionLibrary;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.api.exception.query.UnresolvedSymbolDescription;
import org.teiid.core.CoreConstants;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.core.types.DefaultDataTypeManager.DefaultDataTypes;
import org.teiid.core.util.ArgCheck;
import org.teiid.core.util.StringUtil;
import org.teiid.query.function.TCFunctionDescriptor;
import org.teiid.query.function.DefaultFunctionLibrary;
import org.teiid.query.metadata.GroupInfo;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.ExpressionCriteriaImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.navigator.PostOrderNavigator;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;
import org.teiid.query.sql.symbol.v7.Aggregate7SymbolImpl;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;


public class ResolverVisitorImpl extends TCLanguageVisitorImpl
    implements ResolverVisitor<BaseLanguageObject, GroupSymbolImpl> {
    
    public static final String TEIID_PASS_THROUGH_TYPE = "teiid:pass-through-type"; //$NON-NLS-1$

	private static final String SYS_PREFIX = CoreConstants.SYSTEM_MODEL + '.';

	@Removed(Version.TEIID_8_5)
    private ThreadLocal<Boolean> determinePartialName = new ThreadLocal<Boolean>() {
    	@Override
        protected Boolean initialValue() {
    		return false;
    	}
    };

    private Collection<GroupSymbolImpl> groups;
    private GroupContextImpl externalContext;
    protected QueryMetadataInterface metadata;
    private Exception componentException;
    private Exception resolverException;
    private Map<FunctionImpl, Exception> unresolvedFunctions;
    private boolean findShortName;
    private List<ElementSymbolImpl> matches = new ArrayList<ElementSymbolImpl>(2);
    private List<GroupSymbolImpl> groupMatches = new ArrayList<GroupSymbolImpl>(2);
	@Since(Version.TEIID_8_6)
    private boolean hasUserDefinedAggregate;
    
    /**
     * Constructor for ResolverVisitor.
     * 
     * External groups are ordered from inner to outer most
     * @param teiidVersion
     */
    public ResolverVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    /**
     * Constructor for ResolverVisitor.
     *
     * @param teiidVersion
     * @param metadata
     * @param groups
     * @param externalContext
     */
    public ResolverVisitorImpl(TeiidVersion teiidVersion, QueryMetadataInterface metadata, Collection<GroupSymbolImpl> internalGroups, GroupContextImpl externalContext) {
        this(teiidVersion);
		this.groups = internalGroups;
        this.externalContext = externalContext;
        this.metadata = metadata;
        setFindShortName(metadata);
    }

    private void setFindShortName(QueryMetadataInterface metadata) {
        if (getTeiidVersion().isGreaterThanOrEqualTo(Version.TEIID_8_5.get()))
            this.findShortName = metadata.findShortName();
        else
            this.findShortName = determinePartialName.get();
    }
    
	/**
	 * @param groups
	 */
	public void setGroups(Collection<GroupSymbolImpl> groups) {
		this.groups = groups;
	}

    @Override
    public void visit(ElementSymbolImpl obj) {
        try {
            resolveElementSymbol(obj);
        } catch(Exception e) {
            handleException(handleUnresolvedElement(obj, e.getMessage()));
        }
    }

    private QueryResolverException handleUnresolvedElement(ElementSymbolImpl symbol, String description) {
    	UnresolvedSymbolDescription usd = new UnresolvedSymbolDescription(symbol.toString(), description);
    	QueryResolverException e = new QueryResolverException(usd.getDescription());
        e.setUnresolvedSymbols(Arrays.asList(usd));
        return e;
    }

    private void resolveElementSymbol(ElementSymbolImpl elementSymbol)
        throws Exception {

        // already resolved
        if(elementSymbol.getMetadataID() != null) {
        	return;
        }
        
        // look up group and element parts of the potentialID
        String groupContext = null;
        if (elementSymbol.getGroupSymbol() != null) {
        	groupContext = elementSymbol.getGroupSymbol().getName();
        }
        String elementShortName = elementSymbol.getShortName();
        if (groupContext != null) {
            groupContext = elementSymbol.getGroupSymbol().getName();
        	try {
				if (findShortName && internalResolveElementSymbol(elementSymbol, null, elementShortName, groupContext)) {
		    		elementSymbol.setDisplayMode(DisplayMode.SHORT_OUTPUT_NAME);
		    		return;
				}
			} catch (Exception e) {
				//ignore
			}
        }
        
        internalResolveElementSymbol(elementSymbol, groupContext, elementShortName, null);
   }

	private boolean internalResolveElementSymbol(ElementSymbolImpl elementSymbol,
			String groupContext, String shortCanonicalName, String expectedGroupContext)
			throws Exception {
		boolean isExternal = false;
        boolean groupMatched = false;
        
        GroupContextImpl root = null;
        
        if (groups != null || externalContext != null) {
            if (groups != null) {
                root = new GroupContextImpl(externalContext, groups);
            }
            if (root == null) {
                isExternal = true;
                root = externalContext;
            }
        } else {
            try {
                LinkedList<GroupSymbolImpl> matchedGroups = new LinkedList<GroupSymbolImpl>();
                
                if (groupContext != null) {
                    //assume that this is fully qualified
                    Object groupID = this.metadata.getGroupID(groupContext);
                    // No groups specified, so any group is valid
                    GroupSymbolImpl groupSymbol = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
                    groupSymbol.setName(groupContext);
                    groupSymbol.setMetadataID(groupID);
                    matchedGroups.add(groupSymbol);
                }
                
                root = new GroupContextImpl(null, matchedGroups);
            } catch(Exception e) {
                // ignore 
            }
        }
        
        matches.clear();
        groupMatches.clear();
        while (root != null) {
            Collection<GroupSymbolImpl> matchedGroups = ResolverUtil.findMatchingGroups(groupContext, root.getGroups(), metadata);
            if (matchedGroups != null && !matchedGroups.isEmpty()) {
                groupMatched = true;
                    
                resolveAgainstGroups(shortCanonicalName, matchedGroups);
                
                if (matches.size() > 1) {
            	    throw handleUnresolvedElement(elementSymbol, Messages.gs(Messages.TEIID.TEIID31117, elementSymbol, groupMatches));
                }
                
                if (matches.size() == 1) {
                    break;
                }
            }
            
            root = root.getParent();
            isExternal = true;
        }
        
        if (matches.isEmpty()) {
            if (groupMatched) {
                throw handleUnresolvedElement(elementSymbol, Messages.gs(Messages.TEIID.TEIID31118, elementSymbol)); 
            }
            throw handleUnresolvedElement(elementSymbol, Messages.gs(Messages.TEIID.TEIID31119, elementSymbol)); 
        }
        //copy the match information
        ElementSymbolImpl resolvedSymbol = matches.get(0);
        GroupSymbolImpl resolvedGroup = groupMatches.get(0);
        String oldName = elementSymbol.getOutputName();
        if (expectedGroupContext != null && !ResolverUtil.nameMatchesGroup(expectedGroupContext, resolvedGroup.getName())) {
        	return false;
        }
        elementSymbol.setIsExternalReference(isExternal);
        elementSymbol.setType(resolvedSymbol.getType());
        elementSymbol.setMetadataID(resolvedSymbol.getMetadataID());
        elementSymbol.setGroupSymbol(resolvedGroup);
        elementSymbol.setShortName(resolvedSymbol.getShortName());
        if (metadata.useOutputName()) {
        	elementSymbol.setOutputName(oldName);
        }
        return true;
	}
    
    private void resolveAgainstGroups(String elementShortName,
                                      Collection<GroupSymbolImpl> matchedGroups) throws Exception {
    	for (GroupSymbolImpl group : matchedGroups) {
            GroupInfo groupInfo = ResolverUtil.getGroupInfo(group, metadata);
            
            ElementSymbolImpl result = groupInfo.getSymbol(elementShortName);
            if (result != null) {
            	matches.add(result);
            	groupMatches.add(group);
            }
        }
    }
        
    @Override
    public void visit(BetweenCriteriaImpl obj) {
        try {
            resolveBetweenCriteria(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(CompareCriteriaImpl obj) {
        try {
            resolveCompareCriteria(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(MatchCriteriaImpl obj) {
        try {
            resolveMatchCriteria(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(SetCriteriaImpl obj) {
        try {
            resolveSetCriteria(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        try {
            obj.setLeftExpression(ResolverUtil.resolveSubqueryPredicateCriteria(obj.getLeftExpression(), obj, metadata));
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        try {
            obj.setExpression(ResolverUtil.resolveSubqueryPredicateCriteria(obj.getExpression(), obj, metadata));
        } catch(Exception e) {
            handleException(e);
        }
    }

    @Override
    public void visit(IsNullCriteriaImpl obj) {
        try {
        	setDesiredType(obj.getExpression(), DefaultDataTypes.OBJECT.getTypeClass(), obj);
        } catch(Exception e) {
            handleException(e);
        }
    }
    
    @Override
    public void visit(FunctionImpl obj) {
        try {
            resolveFunction(obj, (DefaultFunctionLibrary) this.metadata.getFunctionLibrary());
			if (obj.isAggregate() && isTeiidVersionOrGreater(Version.TEIID_8_6)) {
            	hasUserDefinedAggregate = true;
            }
        } catch(Exception e) {
            String msg = e.getMessage();
        	if (msg != null && (msg.contains(Messages.TEIID.TEIID30069.name()) || msg.contains(Messages.TEIID.TEIID30067.name()))) {
	        	if (unresolvedFunctions == null) {
	        		unresolvedFunctions = new LinkedHashMap<FunctionImpl, Exception>();
	        	}
	        	unresolvedFunctions.put(obj, e);
        	} else {
        		handleException(e);
        	}
        }
    }
    
    @Override
    public void visit(ArraySymbolImpl array) {
    	try {
	    	if (array.getComponentType() != null) {
	    		String type = getDataTypeManager().getDataTypeName(array.getComponentType());
	    		for (int i = 0; i < array.getExpressions().size(); i++) {
	    			BaseExpression expr = array.getExpressions().get(i);
	    			setDesiredType(expr, array.getComponentType(), array);
	    			if (array.getComponentType() != DefaultDataTypes.OBJECT.getTypeClass()) {
	    				array.getExpressions().set(i, ResolverUtil.convertExpression(expr, type, metadata));
	    			}
	    		}
	    	} else {
	    	    Class<?> type = null;
                for (int i = 0; i < array.getExpressions().size(); i++) {
                    BaseExpression expr = array.getExpressions().get(i);
                    if (type == null) {
                        type = expr.getType();
                    } else if (type != expr.getType()) {
                        type = DefaultDataTypeManager.DefaultDataTypes.OBJECT.getTypeClass();
                    }
                }
                if (type == null) {
                    type = DefaultDataTypeManager.DefaultDataTypes.OBJECT.getTypeClass();
                }
                array.setComponentType(type);
            }
    	} catch (Exception e) {
    		handleException(e);
    	}
    }

    @Override
    public void visit(CaseExpressionImpl obj) {
        try {
            resolveCaseExpression(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }
    
    @Override
    public void visit(SearchedCaseExpressionImpl obj) {
        try {
            resolveSearchedCaseExpression(obj);
        } catch(Exception e) {
            handleException(e);
        }
    }
    
    @Override
    public void visit(SetClauseImpl obj) {
    	String type = getDataTypeManager().getDataTypeName(obj.getSymbol().getType());
    	try {
    		setDesiredType(obj.getValue(), obj.getSymbol().getType(), obj);
            obj.setValue(ResolverUtil.convertExpression(obj.getValue(), type, metadata));                    
        } catch(Exception e) {
            handleException(new QueryResolverException(e, Messages.getString(Messages.QueryResolver.setClauseResolvingError, new Object[] {obj.getValue(), obj.getSymbol(), type})));
        } 
    }
    
    @Override
    public void visit(XMLSerializeImpl obj) {
    	try {
			obj.setExpression(ResolverUtil.convertExpression(obj.getExpression(), DefaultDataTypes.XML.getId(), metadata));
		} catch (Exception e) {
			handleException(new QueryResolverException(e, Messages.getString(Messages.QueryResolver.xmlSerializeResolvingError, obj)));
		}
    }
    
    @Override
    public void visit(XMLQueryImpl obj) {
    	try {
	    	ResolverUtil.setDesiredType(obj.getPassing(), obj);
			obj.compileXqueryExpression();
		} catch (Exception e) {
			handleException(e); 
		}
    }
    
    @Override
    public void visit(QueryStringImpl obj) {
    	try {
			obj.setPath(ResolverUtil.convertExpression(obj.getPath(), DefaultDataTypes.STRING.getId(), metadata));
			for (DerivedColumnImpl col : obj.getArgs()) {
				col.setExpression(ResolverUtil.convertExpression(col.getExpression(), DefaultDataTypes.STRING.getId(), metadata));
			}
		} catch (Exception e) {
			handleException(new QueryResolverException(e, Messages.getString(Messages.QueryResolver.xmlQueryResolvingError, obj)));
		}
    }
    
    @Override
    public void visit(ExpressionCriteriaImpl obj) {
		try {
			obj.setExpression(ResolverUtil.convertExpression(obj.getExpression(), DefaultDataTypes.BOOLEAN.getId(), metadata));
		} catch (Exception e) {
			handleException(e);
		}
    }
    
    @Override
    public void visit(ExceptionExpressionImpl obj) {
    	try {
    		if (obj.getErrorCode() != null) {
    			obj.setErrorCode(ResolverUtil.convertExpression(obj.getErrorCode(), DefaultDataTypes.INTEGER.getId(), metadata));
    		}
			obj.setMessage(ResolverUtil.convertExpression(obj.getMessage(), DefaultDataTypes.STRING.getId(), metadata));
			if (obj.getSqlState() != null) {
				obj.setSqlState(ResolverUtil.convertExpression(obj.getSqlState(), DefaultDataTypes.STRING.getId(), metadata));
			}
			checkException(obj.getParent());
		} catch (Exception e) {
			handleException(e);
		}
    }

	public static void checkException(BaseExpression obj)
			throws QueryResolverException {
		if (obj == null || obj instanceof ExceptionExpressionImpl) {
			return;
		}
		if (obj instanceof ElementSymbolImpl) {
			ElementSymbolImpl es = (ElementSymbolImpl)obj;
			if (!(es.getMetadataID() instanceof TempMetadataID)) {
				throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31120, obj));
			}
			TempMetadataID tid = (TempMetadataID)es.getMetadataID();
			if (tid.getType() != Exception.class) {
				throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31120, obj));
			}
		} else if (obj instanceof ConstantImpl) {
			ConstantImpl c = (ConstantImpl)obj;
			if (!(c.getValue() instanceof Exception)) {
				throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31120, obj));
			}
		} else {
			throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31120, obj));
		}
	}
    
    @Override
    public void visit(BaseAggregateSymbol obj) {
    	if (obj.getCondition() != null) {
			try {
				obj.setCondition(ResolverUtil.convertExpression(obj.getCondition(), DefaultDataTypes.BOOLEAN.getId(), metadata));
			} catch (Exception e) {
				handleException(e);
			}
    	}

    	if (obj instanceof Aggregate7SymbolImpl)
    	    return;

    	/* Following does not apply to 7.7.x aggregate symbols */

    	switch (obj.getAggregateFunction()) {
    	case USER_DEFINED:
    		visit((FunctionImpl)obj);
    		break;
    	case STRING_AGG:
    		try {
	    		if (obj.getArgs().length != 2) {
	    			throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31140, obj));
	    		}
	    		if (obj.getType() == null) {
					BaseExpression arg = obj.getArg(0);
					BaseExpression arg1 = obj.getArg(1);
					Class<?> type = null;
					if (isBinary(arg)) {
						setDesiredType(arg1, DefaultDataTypes.BLOB.getTypeClass(), obj);
						if (isBinary(arg1)) {
							type = DefaultDataTypes.BLOB.getTypeClass();
						}
					} else if (isCharacter(arg)) {
						setDesiredType(arg1, DefaultDataTypes.CLOB.getTypeClass(), obj);
						if (isCharacter(arg1)) {
							type = DefaultDataTypes.CLOB.getTypeClass();
						}
					} else if (arg.getType() == null) {
						if (isBinary(arg1)) {
							setDesiredType(arg, DefaultDataTypes.BLOB.getTypeClass(), obj);
							if (isBinary(arg)) {
								type = DefaultDataTypes.BLOB.getTypeClass();
							}
						} else if (isCharacter(arg1)) {
							setDesiredType(arg, DefaultDataTypes.CLOB.getTypeClass(), obj);
							if (isCharacter(arg)) {
								type = DefaultDataTypes.CLOB.getTypeClass();
							}
						}
					}
					if (type == null) {
						throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31141, obj));
					}
	    			obj.setType(type);
	    		}
    		} catch (Exception e) {
				handleException(e);
			}
    		break;
    	}
    }

	private boolean isCharacter(BaseExpression arg) {
		return arg.getType() == DefaultDataTypes.STRING.getTypeClass()
				|| arg.getType() == DefaultDataTypes.CLOB.getTypeClass();
	}

	private boolean isBinary(BaseExpression arg) {
		return arg.getType() == DefaultDataTypes.VARBINARY.getTypeClass()
				|| arg.getType() == DefaultDataTypes.BLOB.getTypeClass();
	}

    public Exception getComponentException() {
        return this.componentException;
    }

    public Exception getResolverException() {
        return this.resolverException;
    }

    void handleException(Exception e) {
        this.componentException = e;

        // Abort the validation process
        setAbort(true);
    }

	public void throwException(boolean includeUnresolvedFunctions)
			throws Exception {
		if(getComponentException() != null) {
            throw getComponentException();
        }

        if(getResolverException() != null) {
            throw getResolverException();
        }
        
        if (includeUnresolvedFunctions 
        		&& unresolvedFunctions != null && !unresolvedFunctions.isEmpty()) {
        	throw unresolvedFunctions.values().iterator().next();
        }
	}

	/**
	 * Resolve function such that all functions are resolved and type-safe.
	 */
	void resolveFunction(FunctionImpl function, DefaultFunctionLibrary library)
	    throws Exception {
	
	    // Check whether this function is already resolved
	    if(function.getFunctionDescriptor() != null) {
	        return;
	    }
	
	    // Look up types for all args
	    boolean hasArgWithoutType = false;
	    BaseExpression[] args = function.getArgs();
	    Class<?>[] types = new Class[args.length];
	    for(int i=0; i<args.length; i++) {
	        types[i] = args[i].getType();
	        if(types[i] == null) {
	        	if(!(args[i] instanceof ReferenceImpl)){
	                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30067, new Object[] {args[i], function}));
	        	}
	            hasArgWithoutType = true;
	        }
	    }
	
	    //special case handling for convert of an untyped reference
	    if (DefaultFunctionLibrary.isConvert(function) && hasArgWithoutType) {
	        ConstantImpl constant = (ConstantImpl)function.getArg(1);
	        Class<?> type = getDataTypeManager().getDataTypeClass((String)constant.getValue());
	
	        setDesiredType(function.getArg(0), type, function);
	        types[0] = type;
	        hasArgWithoutType = false;
	    }
	
	    // Attempt to get exact match of function for this signature
	    List<TCFunctionDescriptor> fds = findWithImplicitConversions(library, function, args, types, hasArgWithoutType);
	    
	    // Function did not resolve - determine reason and throw exception
	    if(fds.isEmpty()) {
	        if(!library.hasFunctionMethod(function.getName(), args.length)) {
	            // Unknown function form
	             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30068, function));
	        }
	        // Known function form - but without type information
	        if (hasArgWithoutType) {
	             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30069, function));
	        }
	        // Known function form - unable to find implicit conversions
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30070, function));
	    }
	    if (fds.size() > 1) {
            throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31150, function));
        }
        TCFunctionDescriptor fd = fds.get(0);
	    if (fd.getMethod().isVarArgs() 
	    		&& fd.getTypes().length == types.length 
	    		&& library.isVarArgArrayParam(fd.getMethod(), types, types.length - 1, fd.getTypes()[types.length - 1])) {
	    	fd = fd.clone();
	    	fd.setCalledWithVarArgArrayParam(true);
	    }
	    
	    if(fd.isSystemFunction(FunctionLibrary.FunctionName.CONVERT) || fd.isSystemFunction(FunctionLibrary.FunctionName.CAST)) {
	        String dataType = (String) ((ConstantImpl)args[1]).getValue();
	        Class<?> dataTypeClass = getDataTypeManager().getDataTypeClass(dataType);
	        fd = library.findTypedConversionFunction(args[0].getType(), dataTypeClass);
	
	        // Verify that the type conversion from src to type is even valid
	        Class<?> srcTypeClass = args[0].getType();
	        if(srcTypeClass != null && dataTypeClass != null &&
	           !srcTypeClass.equals(dataTypeClass) &&
	           !getDataTypeManager().isTransformable(srcTypeClass, dataTypeClass)) {
	
	             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30071, new Object[] {getDataTypeManager().getDataTypeName(srcTypeClass), dataType}));
	        }
	    } else if(fd.isSystemFunction(FunctionLibrary.FunctionName.LOOKUP)) {
			ResolverUtil.ResolvedLookup lookup = ResolverUtil.resolveLookup(function, metadata);
			fd = library.copyFunctionChangeReturnType(fd, lookup.getReturnElement().getType());
	    } else if (fd.isSystemFunction(FunctionLibrary.FunctionName.ARRAY_GET) && args[0].getType().isArray()) {
			if (args[0].getType().isArray()) {
	    		//hack to use typed array values
				fd = library.copyFunctionChangeReturnType(fd, args[0].getType().getComponentType());
	    	} else {
	    		if (function.getType() != null) {
	    			setDesiredType(args[0], function.getType(), function);
	    		}
	    		if (args[0].getType() != DefaultDataTypeManager.DefaultDataTypes.OBJECT.getTypeClass()) {
	    			throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31145, getDataTypeManager().getDataTypeName(args[0].getType()), function));
	    		}
	    	}
	    } else if (Boolean.valueOf(fd.getMethod().getProperty(TEIID_PASS_THROUGH_TYPE, false))) {
	    	//hack largely to support pg
	    	fd = library.copyFunctionChangeReturnType(fd, args[0].getType());
	    }
	
	    function.setFunctionDescriptor(fd);
	    function.setType(fd.getReturnType());
	    if (CoreConstants.SYSTEM_MODEL.equals(fd.getSchema()) && StringUtil.startsWithIgnoreCase(function.getName(), SYS_PREFIX)) {
	    	function.setName(function.getName().substring(SYS_PREFIX.length()));
	    }
	}

	/**
	 * Find possible matches based on implicit conversions of the arguments.
	 * NOTE: This method has the side-effect of explicitly inserting conversions into the function arguments,
	 * and thereby changing the structure of the function call.
	 * @param library
	 * @param function
	 * @param types
	 * @return
	 * @throws Exception 
	 *
	 */
	private List<TCFunctionDescriptor> findWithImplicitConversions(DefaultFunctionLibrary library, FunctionImpl function, BaseExpression[] args, Class<?>[] types, boolean hasArgWithoutType) throws Exception {
	    
	    // Try to find implicit conversion path to still perform this function
	    TCFunctionDescriptor[] conversions;
		try {
			conversions = library.determineNecessaryConversions(function.getName(), function.getType(), args, types, hasArgWithoutType);
		} catch (Exception e) {
			return Collections.emptyList();
		}
		Class<?>[] newSignature = types;
	    
	    if(conversions != null) {
		    newSignature = new Class[conversions.length];
		    // Insert new conversion functions as necessary, while building new signature
		    for(int i=0; i<conversions.length; i++) {
		        
		        Class<?> newType = types[i];
		        
		        if(conversions[i] != null) {
		            newType = conversions[i].getReturnType();
		            
		            setDesiredType(args[i], newType, function);
		                                
		            //only currently typed expressions need conversions
		            if (types[i] != null && newType != DefaultDataTypes.OBJECT.getTypeClass()) {
		                function.insertConversion(i, conversions[i]);
		            }
		        } 
		                    
		        newSignature[i] = newType;
		    }
	    }
	
	    // Now resolve using the new signature to get the function's descriptor
	    return library.findAllFunctions(function.getName(), newSignature);
	}

	/**
	 * Resolves criteria "a BETWEEN b AND c". If type conversions are necessary,
	 * this method attempts the following implicit conversions:
	 * <br/>
	 * <ol type="1" start="1">
	 *   <li>convert the lower and upper expressions to the criteria expression's type, or</li>
	 *   <li>convert the criteria and upper expressions to the lower expression's type, or</li>
	 *   <li>convert the criteria and lower expressions to the upper expression's type, or</li>
	 *   <li>convert all expressions to a common type to which all three expressions' types can be implicitly converted.</li>
	 * </ol>
	 * @param criteria
	 * @throws Exception
	 * @throws Exception 
	 * @throws Exception
	 */
	void resolveBetweenCriteria(BetweenCriteriaImpl criteria)
	    throws Exception {
	
	    BaseExpression exp = criteria.getExpression();
	    BaseExpression lower = criteria.getLowerExpression();
	    BaseExpression upper = criteria.getUpperExpression();
	
	    // invariants: none of the expressions is an aggregate symbol
	    setDesiredType(exp,
	                                   (lower.getType() == null)
	                                        ? upper.getType()
	                                        : lower.getType(), criteria);
	    // invariants: exp.getType() != null
	    setDesiredType(lower, exp.getType(), criteria);
	    setDesiredType(upper, exp.getType(), criteria);
	    // invariants: none of the types is null
	
	    String expTypeName = getDataTypeManager().getDataTypeName(exp.getType());
	    String lowerTypeName = getDataTypeManager().getDataTypeName(lower.getType());
	    String upperTypeName = getDataTypeManager().getDataTypeName(upper.getType());
	    if (exp.getType().equals(lower.getType()) && exp.getType().equals(upper.getType())) {
	        return;
	    }
	
	    String commonType = ResolverUtil.getCommonType(getTeiidVersion(), new String[] {expTypeName, lowerTypeName, upperTypeName});
	    if (commonType != null) {
	        criteria.setExpression(ResolverUtil.convertExpression(exp, expTypeName, commonType, metadata));
	        criteria.setLowerExpression(ResolverUtil.convertExpression(lower, lowerTypeName, commonType, metadata));
	        criteria.setUpperExpression(ResolverUtil.convertExpression(upper, upperTypeName, commonType, metadata));
	    } else {
	        // Couldn't find a common type to implicitly convert to
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30072, expTypeName, lowerTypeName, criteria));
	    }
	    // invariants: exp.getType() == lower.getType() == upper.getType()
	}

	void resolveCompareCriteria(CompareCriteriaImpl ccrit)
		throws Exception {
	
		BaseExpression leftExpression = ccrit.getLeftExpression();
		BaseExpression rightExpression = ccrit.getRightExpression();
	
		// Check typing between expressions
	    setDesiredType(leftExpression, rightExpression.getType(), ccrit);
	    setDesiredType(rightExpression, leftExpression.getType(), ccrit);
	
		if(leftExpression.getType().equals(rightExpression.getType()) ) {
			return;
		}
	
		// Try to apply an implicit conversion from one side to the other
		String leftTypeName = getDataTypeManager().getDataTypeName(leftExpression.getType());
		String rightTypeName = getDataTypeManager().getDataTypeName(rightExpression.getType());
	
	    // Special cases when right expression is a constant
	    if(rightExpression instanceof ConstantImpl) {
	        // Auto-convert constant string on right to expected type on left
	        try {
	            ccrit.setRightExpression(ResolverUtil.convertExpression(rightExpression, rightTypeName, leftTypeName, metadata));
	            return;
	        } catch (Exception qre) {
	            //ignore
	        }
	    } 
	    
	    // Special cases when left expression is a constant
	    if(leftExpression instanceof ConstantImpl) {
	        // Auto-convert constant string on left to expected type on right
	        try {
	            ccrit.setLeftExpression(ResolverUtil.convertExpression(leftExpression, leftTypeName, rightTypeName, metadata));
	            return;                                           
	        } catch (Exception qre) {
	            //ignore
	        }
	    }
	
	    // Try to apply a conversion generically
		
	    if(ResolverUtil.canImplicitlyConvert(getTeiidVersion(), leftTypeName, rightTypeName)) {
			ccrit.setLeftExpression(ResolverUtil.convertExpression(leftExpression, leftTypeName, rightTypeName, metadata) );
			return;
		}
	
		if(ResolverUtil.canImplicitlyConvert(getTeiidVersion(), rightTypeName, leftTypeName)) {
			ccrit.setRightExpression(ResolverUtil.convertExpression(rightExpression, rightTypeName, leftTypeName, metadata) );
			return;
	    }
	
		String commonType = ResolverUtil.getCommonType(getTeiidVersion(), new String[] {leftTypeName, rightTypeName});
		
		if (commonType == null) {
	        // Neither are aggs, but types can't be reconciled
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30072, new Object[] { leftTypeName, rightTypeName, ccrit }));
		}
		ccrit.setLeftExpression(ResolverUtil.convertExpression(leftExpression, leftTypeName, commonType, metadata) );
		ccrit.setRightExpression(ResolverUtil.convertExpression(rightExpression, rightTypeName, commonType, metadata) );
	}

	void resolveMatchCriteria(MatchCriteriaImpl mcrit)
	    throws Exception {
	
	    setDesiredType(mcrit.getLeftExpression(), mcrit.getRightExpression().getType(), mcrit);
	    mcrit.setLeftExpression(resolveMatchCriteriaExpression(mcrit, mcrit.getLeftExpression()));
	
	    setDesiredType(mcrit.getRightExpression(), mcrit.getLeftExpression().getType(), mcrit);
	    mcrit.setRightExpression(resolveMatchCriteriaExpression(mcrit, mcrit.getRightExpression()));
	}

	/**
	 * Checks one side of a LIKE Criteria; implicitly converts to a String or CLOB if necessary.
	 * @param mcrit the Match Criteria
	 * @param expr either left or right expression
	 * @return either 'expr' itself, or a new implicit type conversion wrapping expr
	 * @throws Exception if no implicit type conversion is available
	 */
	BaseExpression resolveMatchCriteriaExpression(MatchCriteriaImpl mcrit, BaseExpression expr)
	throws Exception {
	    // Check left expression == string or CLOB
	    String type = getDataTypeManager().getDataTypeName(expr.getType());
	    BaseExpression result = expr;
	    if(type != null) {
	        if (! type.equals(DefaultDataTypes.STRING) &&
	            ! type.equals(DefaultDataTypes.CLOB)) {
	                
	            if(ResolverUtil.canImplicitlyConvert(getTeiidVersion(), type, DefaultDataTypes.STRING.getId())) {
	
	                result = ResolverUtil.convertExpression(expr, type, DefaultDataTypes.STRING.getId(), metadata);
	                
	            } else if (ResolverUtil.canImplicitlyConvert(getTeiidVersion(), type, DefaultDataTypes.CLOB.getId())){
	                    
	                result = ResolverUtil.convertExpression(expr, type, DefaultDataTypes.CLOB.getId(), metadata);
	
	            } else {
	                 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30074, mcrit));
	            }
	        }
	    }
	    return result;
	}

	void resolveSetCriteria(SetCriteriaImpl scrit)
	    throws Exception {
	
	    // Check that each of the values are the same type as expression
	    Class<?> exprType = scrit.getExpression().getType();
	    if(exprType == null) {
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30075, scrit.getExpression()));
	    }
	
	    String exprTypeName = getDataTypeManager().getDataTypeName(exprType);
	    boolean changed = false;
	    List<BaseExpression> newVals = new ArrayList<BaseExpression>();
	
	    boolean convertLeft = false;
	    Class<?> setType = null;
	
	    Iterator valIter = scrit.getValues().iterator();
	    while(valIter.hasNext()) {
	        BaseExpression value = (BaseExpression) valIter.next();
	        setDesiredType(value, exprType, scrit);
	        if(! value.getType().equals(exprType)) {
	            // try to apply cast
	            String valTypeName = getDataTypeManager().getDataTypeName(value.getType());
	            if(ResolverUtil.canImplicitlyConvert(getTeiidVersion(), valTypeName, exprTypeName)) {
	                // Apply cast and replace current value
	                newVals.add(ResolverUtil.convertExpression(value, valTypeName, exprTypeName, metadata) );
	                changed = true;
	            } else {
	                convertLeft = true;
	                setType = value.getType();
	                break;
	            }
	        } else {
	            newVals.add(value);
	        }
	    }
	
	    // If no convert found for first element, check whether everything in the
	    // set is the same and the convert can be placed on the left side
	    if(convertLeft) {
	        // Is there a possible conversion from left to right?
	        String setTypeName = getDataTypeManager().getDataTypeName(setType);
	        if(ResolverUtil.canImplicitlyConvert(getTeiidVersion(), exprTypeName, setTypeName)) {
	            valIter = scrit.getValues().iterator();
	            while(valIter.hasNext()) {
	                BaseExpression value = (BaseExpression) valIter.next();
	                if(value.getType() == null) {
	                     throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30075, value));
	                } else if(! value.getType().equals(setType)) {
	                     throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30077, scrit));
	                }
	            }
	
	            // Convert left expression to type of values in the set
	            scrit.setExpression(ResolverUtil.convertExpression(scrit.getExpression(), exprTypeName, setTypeName, metadata));
	
	        } else {
	             throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30077, scrit));
	        }
	    }
	
	    if(changed) {
	        scrit.setValues(newVals);
	    }
	}

	void resolveCaseExpression(CaseExpressionImpl obj) throws Exception {
	    // If already resolved, do nothing
	    if (obj.getType() != null) {
	        return;
	    }
	    final int whenCount = obj.getWhenCount();
	    BaseExpression expr = obj.getExpression();
	
	    Class<?> whenType = null;
	    Class<?> thenType = null;
	    // Get the WHEN and THEN types, and get a candidate type for each (for the next step)
	    for (int i = 0; i < whenCount; i++) {
	        if (whenType == null) {
	            whenType = obj.getWhenExpression(i).getType();
	        }
	        if (thenType == null) {
	            thenType = obj.getThenExpression(i).getType();
	        }
	    }
	
	    BaseExpression elseExpr = obj.getElseExpression();
	    if (elseExpr != null) {
	        if (thenType == null) {
	            thenType = elseExpr.getType();
	        }
	    }
	    // Invariant: All the expressions contained in the obj are resolved (except References)
	
	    // 2. Attempt to set the target types of all contained expressions,
	    //    and collect their type names for the next step
	    ArrayList<String> whenTypeNames = new ArrayList<String>(whenCount + 1);
	    ArrayList<String> thenTypeNames = new ArrayList<String>(whenCount + 1);
	    setDesiredType(expr, whenType, obj);
	    // Add the expression's type to the WHEN types
	    whenTypeNames.add(getDataTypeManager().getDataTypeName(expr.getType()));
	    BaseExpression when = null;
	    BaseExpression then = null;
	    // Set the types of the WHEN and THEN parts
	    for (int i = 0; i < whenCount; i++) {
	        when = obj.getWhenExpression(i);
	        then = obj.getThenExpression(i);
	
	        setDesiredType(when, expr.getType(), obj);
	        setDesiredType(then, thenType, obj);
	
	        if (!whenTypeNames.contains(getDataTypeManager().getDataTypeName(when.getType()))) {
	            whenTypeNames.add(getDataTypeManager().getDataTypeName(when.getType()));
	        }
	        if (!thenTypeNames.contains(getDataTypeManager().getDataTypeName(then.getType()))) {
	            thenTypeNames.add(getDataTypeManager().getDataTypeName(then.getType()));
	        }
	    }
	    // Set the type of the else expression
	    if (elseExpr != null) {
	        setDesiredType(elseExpr, thenType, obj);
	        if (!thenTypeNames.contains(getDataTypeManager().getDataTypeName(elseExpr.getType()))) {
	            thenTypeNames.add(getDataTypeManager().getDataTypeName(elseExpr.getType()));
	        }
	    }
	
	    // Invariants: all the expressions' types are non-null
	
	    // 3. Perform implicit type conversions
	    String whenTypeName = ResolverUtil.getCommonType(getTeiidVersion(), whenTypeNames.toArray(new String[whenTypeNames.size()]));
	    if (whenTypeName == null) {
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30079, "WHEN", obj));//$NON-NLS-1$
	    }
	    String thenTypeName = ResolverUtil.getCommonType(getTeiidVersion(), thenTypeNames.toArray(new String[thenTypeNames.size()]));
	    if (thenTypeName == null) {
	         throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30079, "THEN/ELSE", obj));//$NON-NLS-1$
	    }
	    obj.setExpression(ResolverUtil.convertExpression(obj.getExpression(), whenTypeName, metadata));
	    ArrayList<BaseExpression> whens = new ArrayList<BaseExpression>(whenCount);
	    ArrayList<BaseExpression> thens = new ArrayList<BaseExpression>(whenCount);
	    for (int i = 0; i < whenCount; i++) {
	        whens.add(ResolverUtil.convertExpression(obj.getWhenExpression(i), whenTypeName, metadata));
	        thens.add(ResolverUtil.convertExpression(obj.getThenExpression(i), thenTypeName, metadata));
	    }
	    obj.setWhen(whens, thens);
	    if (elseExpr != null) {
	        obj.setElseExpression(ResolverUtil.convertExpression(elseExpr, thenTypeName, metadata));
	    }
	    // Set this CASE expression's type to the common THEN type, and we're done.
	    obj.setType(getDataTypeManager().getDataTypeClass(thenTypeName));
	}

	private void setDesiredType(BaseExpression obj, Class<?> type, BaseLanguageObject surrounding) throws Exception {
		ResolverUtil.setDesiredType(obj, type, surrounding);
		//second pass resolving for functions
		if (!(obj instanceof FunctionImpl)) {
			return;
		}
		if (unresolvedFunctions != null) {
			FunctionImpl f = (FunctionImpl)obj;
			if (f.getFunctionDescriptor() != null) {
				return;
			}
        	unresolvedFunctions.remove(obj);
			obj.acceptVisitor(this);
			Exception e = unresolvedFunctions.get(obj);
			if (e != null) {
				throw e;
			}
		}
	}

	void resolveSearchedCaseExpression(SearchedCaseExpressionImpl obj) throws Exception {
	    // If already resolved, do nothing
	    if (obj.getType() != null) {
	        return;
	    }
	    final int whenCount = obj.getWhenCount();
	    // 1. Call recursively to resolve any contained CASE expressions
	
	    Class<?> thenType = null;
	    // Get the WHEN and THEN types, and get a candidate type for each (for the next step)
	    for (int i = 0; i < whenCount; i++) {
	        if (thenType == null) {
	            thenType = obj.getThenExpression(i).getType();
	        }
	    }
	
	    BaseExpression elseExpr = obj.getElseExpression();
	    if (elseExpr != null) {
	        if (thenType == null) {
	            thenType = elseExpr.getType();
	        }
	    }
	    // Invariant: All the expressions contained in the obj are resolved (except References)
	
	    // 2. Attempt to set the target types of all contained expressions,
	    //    and collect their type names for the next step
	    ArrayList<String> thenTypeNames = new ArrayList<String>(whenCount + 1);
	    BaseExpression then = null;
	    // Set the types of the WHEN and THEN parts
	    for (int i = 0; i < whenCount; i++) {
	        then = obj.getThenExpression(i);
	        setDesiredType(then, thenType, obj);
            thenTypeNames.add(getDataTypeManager().getDataTypeName(then.getType()));
	    }
	    // Set the type of the else expression
	    if (elseExpr != null) {
	        setDesiredType(elseExpr, thenType, obj);
            thenTypeNames.add(getDataTypeManager().getDataTypeName(elseExpr.getType()));
	    }
	
	    // Invariants: all the expressions' types are non-null
	
	    // 3. Perform implicit type conversions
	    String thenTypeName = ResolverUtil.getCommonType(getTeiidVersion(), thenTypeNames.toArray(new String[thenTypeNames.size()]));
	    if (thenTypeName == null) {
	         throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30079, "THEN/ELSE", obj)); //$NON-NLS-1$
	    }
	    ArrayList<BaseExpression> thens = new ArrayList<BaseExpression>(whenCount);
	    for (int i = 0; i < whenCount; i++) {
	        thens.add(ResolverUtil.convertExpression(obj.getThenExpression(i), thenTypeName, metadata));
	    }
	    obj.setWhen(obj.getWhen(), thens);
	    if (elseExpr != null) {
	        obj.setElseExpression(ResolverUtil.convertExpression(elseExpr, thenTypeName, metadata));
	    }
	    // Set this CASE expression's type to the common THEN type, and we're done.
	    obj.setType(getDataTypeManager().getDataTypeClass(thenTypeName));
	}
	
    @Override
    public void resolveLanguageObject(BaseLanguageObject obj, QueryMetadataInterface metadata)
    throws Exception {
	    resolveLanguageObject(obj, null, metadata);
	}
	
	@Override
    public void resolveLanguageObject(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups, QueryMetadataInterface metadata)
	    throws Exception {
	    resolveLanguageObject(obj, groups, null, metadata);
	}
	
	/**
	 * @param obj
	 * @param groups
	 * @param externalContext
	 * @param metadata
	 * @throws Exception
	 */
	public void resolveLanguageObject(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups, GroupContextImpl externalContext, QueryMetadataInterface metadata)
	    throws Exception {
	
	    if(obj == null) {
	        return;
	    }

	    ArgCheck.isTrue(obj.getTeiidVersion().compareTo(getTeiidVersion()), "version of visitor should match version of object"); //$NON-NLS-1$

	    setGroups(groups);
        this.externalContext = externalContext;
        this.metadata = metadata;
        setFindShortName(metadata);

	    // Resolve elements, deal with errors
	    PostOrderNavigator.doVisit(obj, this);
	    this.throwException(true);
	}

	@Since(Version.TEIID_8_6)
	public boolean hasUserDefinedAggregate() {
		return hasUserDefinedAggregate;
	}

	@Deprecated
    @Override
    public void setProperty(String propertyName, Object value) {
        /* No longer required. To be removed on removal of deprecated client plugins */
    }
}
