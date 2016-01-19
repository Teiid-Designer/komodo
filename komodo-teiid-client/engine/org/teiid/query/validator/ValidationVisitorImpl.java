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

package org.teiid.query.validator;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.script.Compilable;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

import net.sf.saxon.om.Name11Checker;
import net.sf.saxon.om.QNameException;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.metadata.QueryNode;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.metadata.QueryMetadataInterface.SupportConstants;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.query.sql.lang.SetQuery.Operation;
import org.komodo.spi.query.sql.proc.CreateProcedureCommand;
import org.komodo.spi.query.sql.symbol.AggregateSymbol;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.udf.FunctionLibrary;
import org.teiid.api.exception.query.QueryValidatorException;
import org.teiid.core.types.ArrayImpl;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.language.SQLConstants;
import org.teiid.metadata.AggregateAttributes;
import org.teiid.metadata.Table;
import org.teiid.query.eval.Evaluator;
import org.teiid.query.function.FunctionMethods;
import org.teiid.query.function.source.XMLSystemFunctions;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.ProcedureContainerResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.AlterImpl;
import org.teiid.query.sql.lang.AlterProcedureImpl;
import org.teiid.query.sql.lang.AlterTriggerImpl;
import org.teiid.query.sql.lang.AlterViewImpl;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CreateImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.DropImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.Labeled;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NamespaceItem;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetClauseListImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.BaseTargetedCommand;
import org.teiid.query.sql.lang.TextColumnImpl;
import org.teiid.query.sql.lang.TextTableImpl;
import org.teiid.query.sql.lang.TranslateCriteriaImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.lang.WithQueryCommandImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.navigator.PreOrderNavigator;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl.BranchingMode;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.JSONObjectImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ReferenceImpl.Constraint;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.XMLAttributesImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;
import org.teiid.query.sql.visitor.AggregateSymbolCollectorVisitorImpl;
import org.teiid.query.sql.visitor.AggregateSymbolCollectorVisitorImpl.AggregateStopNavigator;
import org.teiid.query.sql.visitor.ElementCollectorVisitorImpl;
import org.teiid.query.sql.visitor.EvaluatableVisitor;
import org.teiid.query.sql.visitor.FunctionCollectorVisitorImpl;
import org.teiid.query.sql.visitor.GroupCollectorVisitorImpl;
import org.teiid.query.sql.visitor.GroupsUsedByElementsVisitorImpl;
import org.teiid.query.sql.visitor.SQLStringVisitorImpl;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.query.validator.DefaultUpdateValidator.UpdateInfo;
import org.teiid.query.xquery.saxon.SaxonXQueryExpression;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;
import org.teiid.translator.SourceSystemFunctions;

/**
 *
 */
public class ValidationVisitorImpl extends AbstractValidationVisitor {

    private static final class PositiveIntegerConstraint implements ReferenceImpl.Constraint {
    	
    	private Messages.ValidationVisitor msgKey;
    	
    	public PositiveIntegerConstraint(Messages.ValidationVisitor enumKey) {
    		this.msgKey = enumKey;
		}

		@Override
        public void validate(Object value) throws Exception {
			if (value == null || ((Integer)value).intValue() < 0) {
				 throw new TeiidClientException(Messages.getString(msgKey));
			}
		}
	}

    public static final Constraint LIMIT_CONSTRAINT = new PositiveIntegerConstraint(Messages.ValidationVisitor.badlimit2);

	// State during validation
    private boolean isXML = false;	// only used for Query commands
    
    private boolean inQuery;

	// update procedure being validated
	private CreateProcedureCommand<BlockImpl, GroupSymbolImpl, BaseExpression, TCLanguageVisitorImpl> createProc;

	private final DefaultDataTypeManager dataTypeManager;

	/**
     * @param teiidVersion
     */
    public ValidationVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
        dataTypeManager = DefaultDataTypeManager.getInstance(teiidVersion);
    }

	@Override
    public void reset() {
        super.reset();
        this.isXML = false;
        this.inQuery = false;

        if (getTeiidVersion().isGreaterThanOrEqualTo(Version.TEIID_8_0.get()))
            this.createProc = null;
    }

    // ############### Visitor methods for language objects ##################
    
//    public void visit(BatchedUpdateCommand obj) {
//        List<Command> commands = obj.getUpdateCommands();
//        Command command = null;
//        int type = 0;
//        for (int i = 0; i < commands.size(); i++) {
//            command = commands.get(i);
//            type = command.getType();
//            if (type != Command.TYPE_INSERT &&
//                type != Command.TYPE_UPDATE &&
//                type != Command.TYPE_DELETE &&
//                type != Command.TYPE_QUERY) { // SELECT INTO command
//                handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_batch_command),command);
//            } else if (type == Command.TYPE_QUERY) {
//                Into into = ((Query)command).getInto();
//                if (into == null) {
//                    handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_batch_command),command);
//                }
//            }
//        }
//    }

	@Override
    public void visit(DeleteImpl obj) {
    	validateNoXMLUpdates(obj);
        GroupSymbolImpl group = obj.getGroup();
        validateGroupSupportsUpdate(group);
        if (obj.getUpdateInfo() != null && obj.getUpdateInfo().isInherentDelete()) {
        	validateUpdate(obj, Command.TYPE_DELETE, obj.getUpdateInfo());
        }
    }

    @Override
    public void visit(GroupByImpl obj) {
    	// Get list of all group by IDs
        List<BaseExpression> groupBySymbols = obj.getSymbols();
        validateSortable(groupBySymbols);
        for (BaseExpression expr : groupBySymbols) {
            if (!ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(expr).isEmpty() || expr instanceof ConstantImpl || expr instanceof ReferenceImpl) {
            	handleValidationError(Messages.getString(Messages.ValidationVisitor.groupby_subquery, expr), expr);
            }
		}
    }
    
    @Override
    public void visit(GroupSymbolImpl obj) {
    	try {
			if (this.getMetadata().isScalarGroup(obj.getMetadataID())) {
			    handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_scalar_group_reference, obj),obj);    		
			}
		} catch (Exception e) {
			handleException(e);
		}
    }

    @Override
    public void visit(InsertImpl obj) {
        validateNoXMLUpdates(obj);
        validateGroupSupportsUpdate(obj.getGroup());
        validateInsert(obj);
        
        try {
			if (obj.isMerge()) {
				Collection keys = getMetadata().getUniqueKeysInGroup(obj.getGroup().getMetadataID());
				if (keys.isEmpty()) {
					handleValidationError(Messages.gs(Messages.TEIID.TEIID31132, obj.getGroup()), obj);
				} else {
					Set<Object> keyCols = new LinkedHashSet<Object>(getMetadata().getElementIDsInKey(keys.iterator().next()));
					for (ElementSymbolImpl es : obj.getVariables()) {
						keyCols.remove(es.getMetadataID());
					}
					if (!keyCols.isEmpty()) {
						handleValidationError(Messages.gs(Messages.TEIID.TEIID31133, obj.getGroup(), obj.getVariables()), obj);
					}
				}
			}
		} catch (Exception e1) {
			handleException(e1);
		}
        
        if (obj.getQueryExpression() != null) {
        	validateMultisourceInsert(obj.getGroup());
        }
        if (obj.getUpdateInfo() != null && obj.getUpdateInfo().isInherentInsert()) {
        	validateUpdate(obj, Command.TYPE_INSERT, obj.getUpdateInfo());
        	try {
				if (obj.getUpdateInfo().findInsertUpdateMapping(obj, false) == null) {
					handleValidationError(Messages.gs(Messages.TEIID.TEIID30376, obj.getVariables()), obj);
				}
			} catch (Exception e) {
				handleValidationError(e.getMessage(), obj);
			}
        }
    }

    @Override
    public void visit(OrderByItemImpl obj) {
    	validateSortable(obj.getSymbol());
    }
    
    @Override
    public void visit(QueryImpl obj) {
        validateHasProjectedSymbols(obj);
        if(isXMLCommand(obj)) {
            //no temp table (Select Into) allowed
            if(obj.getInto() != null){
                handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0069),obj);
            }

        	this.isXML = true;
	        validateXMLQuery(obj);
        } else {
        	this.inQuery = true;
            validateAggregates(obj);

            //if it is select with no from, should not have ScalarSubQuery
            if(obj.getSelect() != null && obj.getFrom() == null){
                if(!ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(obj.getSelect()).isEmpty()){
                    handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0067),obj);
                }
            }
            
            if (obj.getInto() != null) {
                validateSelectInto(obj);
            }                        
        }
    }
	
	@Override
    public void visit(SelectImpl obj) {
        validateSelectElements(obj);
        if(obj.isDistinct()) {
            validateSortable(obj.getProjectedSymbols());
        }
    }

	@Override
    public void visit(SubquerySetCriteriaImpl obj) {
		validateSubquery(obj);
		if (isNonComparable(obj.getExpression())) {
			handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0027, obj),obj);
    	}
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
        
		Collection<BaseExpression> projSymbols = obj.getCommand().getProjectedSymbols();

		//Subcommand should have one projected symbol (query with one expression
		//in SELECT or stored procedure execution that returns a single value).
		if(projSymbols.size() != 1) {
			handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0011),obj);
		}
	}
	
	@Override
	public void visit(XMLSerializeImpl obj) {
		if (obj.getEncoding() != null ) {
        	try {
				Charset.forName(obj.getEncoding());
        	} catch (IllegalArgumentException e) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_encoding, obj.getEncoding()), obj);
        	}
			if ((obj.getType() != DefaultDataTypeManager.DefaultDataTypes.BLOB.getTypeClass() && obj.getType() != DefaultDataTypeManager.DefaultDataTypes.VARBINARY.getTypeClass())) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.encoding_for_binary), obj);
			}
		}
	}

    @Override
    public void visit(SetQueryImpl obj) {
        validateHasProjectedSymbols(obj);
        validateSetQuery(obj);
    }
    
    @Override
    public void visit(UpdateImpl obj) {
        validateNoXMLUpdates(obj);
        validateGroupSupportsUpdate(obj.getGroup());
        validateUpdate(obj);
    }

    @Override
    public void visit(IntoImpl obj) {
        GroupSymbolImpl target = obj.getGroup();
        validateGroupSupportsUpdate(target);
        validateMultisourceInsert(obj.getGroup());
    }

	private void validateMultisourceInsert(GroupSymbolImpl group) {
		try {
			if (getMetadata().isMultiSource(getMetadata().getModelID(group.getMetadataID()))) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.multisource_insert, group), group);
			}
        } catch (Exception e) {
			handleException(e);
		}
	}

    @Override
    public void visit(FunctionImpl obj) {
    	if(FunctionLibrary.FunctionName.LOOKUP.equalsIgnoreCase(obj.getName())) {
    		try {
				ResolverUtil.ResolvedLookup resolvedLookup = ResolverUtil.resolveLookup(obj, getMetadata());
				if(ValidationVisitorImpl.isNonComparable(resolvedLookup.getKeyElement())) {
		            handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_lookup_key, resolvedLookup.getKeyElement()), resolvedLookup.getKeyElement());            
		        }
			} catch (Exception e) {
				handleException(e, obj);
			}
    	} else if (FunctionLibrary.FunctionName.CONTEXT.equalsIgnoreCase(obj.getName())) {
            if(!isXML) {
                // can't use this pseudo-function in non-XML queries
                handleValidationError(Messages.getString(Messages.ValidationVisitor.The_context_function_cannot_be_used_in_a_non_XML_command), obj);
            } else {
                if (!(obj.getArg(0) instanceof ElementSymbolImpl)){
                    handleValidationError(Messages.getString(Messages.ERR.ERR_015_004_0036), obj); 
                }
                
                for (Iterator<FunctionImpl> functions = FunctionCollectorVisitorImpl.getFunctions(obj.getArg(1), false).iterator(); functions.hasNext();) {
                    FunctionImpl function = functions.next();
                    
                    if (FunctionLibrary.FunctionName.CONTEXT.equalsIgnoreCase(function.getName())) {
                        handleValidationError(Messages.getString(Messages.ValidationVisitor.Context_function_nested), obj);
                    }
                }
            }
    	} else if (FunctionLibrary.FunctionName.ROWLIMIT.equalsIgnoreCase(obj.getName()) ||
    	            FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.equalsIgnoreCase(obj.getName())) {
            if(isXML) {
                if (!(obj.getArg(0) instanceof ElementSymbolImpl)) {
                    // Arg must be an element symbol
                    handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit2), obj);
                }
            } else {
                // can't use this pseudo-function in non-XML queries
                handleValidationError(Messages.getString(Messages.ValidationVisitor.The_rowlimit_function_cannot_be_used_in_a_non_XML_command), obj);
            }
        } else if(obj.getName().equalsIgnoreCase(SourceSystemFunctions.XPATHVALUE)) {
	        // Validate the xpath value is valid
	        if(obj.getArgs()[1] instanceof ConstantImpl) {
	            ConstantImpl xpathConst = (ConstantImpl) obj.getArgs()[1];
                try {
                    XMLSystemFunctions.validateXpath((String)xpathConst.getValue());
                } catch(Exception e) {
                	handleValidationError(Messages.getString(Messages.QueryResolver.invalid_xpath, e.getMessage()), obj);
                }
	        }
        } else if(obj.getName().equalsIgnoreCase(SourceSystemFunctions.TO_BYTES) || obj.getName().equalsIgnoreCase(SourceSystemFunctions.TO_CHARS)) {
        	try {
        		FunctionMethods.getCharset((String)((ConstantImpl)obj.getArg(1)).getValue());
        	} catch (IllegalArgumentException e) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_encoding, obj.getArg(1)), obj);
        	}
        } else if (obj.isAggregate()) {
        	handleValidationError(Messages.getString(Messages.ValidationVisitor.user_defined_aggregate_as_function, obj, obj.getName()), obj);
        } else if (FunctionLibrary.FunctionName.JSONARRAY.equalsIgnoreCase(obj.getName())) {
        	BaseExpression[] args = obj.getArgs();
        	for (BaseExpression expression : args) {
        		validateJSONValue(obj, expression);
			}
        }
    }

    // ############### Visitor methods for stored procedure lang objects ##################

    public void visit(AssignmentStatementImpl obj) {
    	
    	ElementSymbolImpl variable = obj.getVariable();

    	validateAssignment(obj, variable);
    }
    
    @Override
    public void visit(CommandStatementImpl obj) {
        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get()))
            visit7(obj);
        else
            visit8(obj);
    }
    
    private void visit7(CommandStatementImpl obj) {
    	if (obj.getCommand() instanceof StoredProcedureImpl) {
    		StoredProcedureImpl proc = (StoredProcedureImpl)obj.getCommand();
    		for (SPParameterImpl param : proc.getParameters()) {
				if ((param.getParameterType() == SPParameterImpl.RETURN_VALUE 
						|| param.getParameterType() == SPParameterImpl.OUT) && param.getExpression() instanceof ElementSymbolImpl) {
					validateAssignment(obj, (ElementSymbolImpl)param.getExpression());
				}
			}
    	}
    }
    
    @Override
    public void visit(StoredProcedureImpl obj) {
		for (SPParameterImpl param : obj.getInputParameters()) {
			try {
                if (!getMetadata().elementSupports(param.getMetadataID(), SupportConstants.Element.NULL) && EvaluatableVisitor.isFullyEvaluatable(param.getExpression(), true)) {
                    try {
	                    // If nextValue is an expression, evaluate it before checking for null
	                    Object evaluatedValue = Evaluator.assess(param.getExpression());
	                    if(evaluatedValue == null) {
	                        handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0055, param.getParameterSymbol()), param.getParameterSymbol());
	                    } else if (evaluatedValue instanceof ArrayImpl && getMetadata().isVariadic(param.getMetadataID())) {
	            			ArrayImpl av = (ArrayImpl)evaluatedValue;
	            			for (Object o : av.getValues()) {
	            				if (o == null) {
	            					handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0055, param.getParameterSymbol()), param.getParameterSymbol());
	            				}
	            			}
	            		}
	                } catch(Exception e) {
	                    //ignore for now, we don't have the context which could be the problem
	                }
	            }
            } catch (Exception e) {
            	handleException(e);
            }
		}
    }

	private void validateAssignment(BaseLanguageObject obj,
			ElementSymbolImpl variable) {
		String groupName = variable.getGroupSymbol().getCanonicalName();
		//This will actually get detected by the resolver, since we inject an automatic declaration.
    	if(groupName.equals(ProcedureReservedWords.CHANGING) || groupName.equals(ProcedureReservedWords.INPUTS)) {
			handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0012, ProcedureReservedWords.INPUTS, ProcedureReservedWords.CHANGING), obj);
		}
	}
    
    @Override
    public void visit(ScalarSubqueryImpl obj) {
    	validateSubquery(obj);
        Collection<BaseExpression> projSymbols = obj.getCommand().getProjectedSymbols();

        //Scalar subquery should have one projected symbol (query with one expression
        //in SELECT or stored procedure execution that returns a single value).
        if(projSymbols.size() != 1) {
        	handleValidationError(Messages.getString(Messages.ERR.ERR_015_008_0032, obj.getCommand()), obj.getCommand());
        }
    }

    /**
     * Validate that the command assigns a value to the ROWS_UPDATED variable 
     * @param obj
     *
     */
    @Removed(Version.TEIID_8_0)
    protected void validateContainsRowsUpdatedVariable(CreateUpdateProcedureCommandImpl obj) {
        final Collection<ElementSymbolImpl> assignVars = new ArrayList<ElementSymbolImpl>();
       // Use visitor to find assignment statements
        TCLanguageVisitorImpl visitor = new TCLanguageVisitorImpl(getTeiidVersion()) {
            @Override
            public void visit(AssignmentStatementImpl stmt) {
                assignVars.add(stmt.getVariable());
            }
        };
        PreOrderNavigator.doVisit(obj, visitor);
        boolean foundVar = false;
        for (ElementSymbolImpl variable : assignVars) {
            if(variable.getShortName().equalsIgnoreCase(ProcedureReservedWords.ROWS_UPDATED)) {
                foundVar = true;
                break;
            }
        }
        if(!foundVar) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0016, ProcedureReservedWords.ROWS_UPDATED), obj);
        }
    }

    @Override
	@Removed(Version.TEIID_8_0)
    public void visit(CreateUpdateProcedureCommandImpl obj) {
        if(!obj.isUpdateProcedure()){
            
            //check that the procedure does not contain references to itself
            if (GroupCollectorVisitorImpl.getGroups(obj,true).contains(obj.getVirtualGroup())) {
                handleValidationError(Messages.getString(Messages.ValidationVisitor.Procedure_has_group_self_reference),obj);
            }
            
            return;
        }

        // set the state to validate this procedure
        this.createProc = obj;
        validateContainsRowsUpdatedVariable(obj);
    }

    @Override
    public void visit(CreateProcedureCommandImpl obj) {
        //check that the procedure does not contain references to itself
    	if (obj.getUpdateType() == Command.TYPE_UNKNOWN) {
	        if (GroupCollectorVisitorImpl.getGroups(obj,true).contains(obj.getVirtualGroup())) {
	        	handleValidationError(Messages.getString(Messages.ValidationVisitor.Procedure_has_group_self_reference),obj);
	        }
	        if (obj.getResultSetColumns() != null) {
	        	//some unit tests bypass setting the columns
		        this.createProc = obj;
	        }
    	}
    }

    private boolean isUpdateProcedure() {
        if (this.createProc == null)
            return false;

        if (!(this.createProc instanceof CreateUpdateProcedureCommandImpl))
            return false;

        return ((CreateUpdateProcedureCommandImpl) this.createProc).isUpdateProcedure();
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(HasCriteriaImpl obj) {
        if (! isUpdateProcedure()) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0019), obj);
        }
    }
    
    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(TranslateCriteriaImpl obj) {
        if (! isUpdateProcedure()) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0019), obj);
        }
        if(obj.hasTranslations()) {
            Collection selectElmnts = null;
            if(obj.getSelector().hasElements()) {
                selectElmnts = obj.getSelector().getElements();
            }
            Iterator critIter = obj.getTranslations().iterator();
            while(critIter.hasNext()) {
                CompareCriteriaImpl transCrit = (CompareCriteriaImpl) critIter.next();
                Collection<ElementSymbolImpl> leftElmnts = ElementCollectorVisitorImpl.getElements(transCrit.getLeftExpression(), true);
                // there is always only one element
                ElementSymbolImpl leftExpr = leftElmnts.iterator().next();

                if(selectElmnts != null && !selectElmnts.contains(leftExpr)) {
                    handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0021), leftExpr);
                }
            }
        }

        // additional validation checks
        validateTranslateCriteria(obj);
    }

    @Override
    public void visit(CompoundCriteriaImpl obj) {
        // Validate use of 'rowlimit' or 'rowlimitexception' pseudo-function - each occurrence must be in a single
        // CompareCriteria which is entirely it's own conjunct (not OR'ed with anything else)
        if (isXML) {
            // Collect all occurrances of rowlimit and rowlimitexception functions
            List<FunctionImpl> rowLimitFunctions = new ArrayList<FunctionImpl>();
            FunctionCollectorVisitorImpl visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMIT.text());
            PreOrderNavigator.doVisit(obj, visitor); 
            visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.text());
            PreOrderNavigator.doVisit(obj, visitor);
            final int functionCount = rowLimitFunctions.size();
            if (functionCount > 0) {
                
                // Verify each use of rowlimit function is in a compare criteria that is 
                // entirely it's own conjunct
                Iterator<CriteriaImpl> conjunctIter = CriteriaImpl.separateCriteriaByAnd(obj).iterator();            
                
                int i = 0;
                while (conjunctIter.hasNext() && i<functionCount ) {
                    Object conjunct = conjunctIter.next();
                    if (conjunct instanceof CompareCriteriaImpl) {
                        CompareCriteriaImpl crit = (CompareCriteriaImpl)conjunct;
                        if ((rowLimitFunctions.contains(crit.getLeftExpression()) && !rowLimitFunctions.contains(crit.getRightExpression())) || 
                            (rowLimitFunctions.contains(crit.getRightExpression()) && !rowLimitFunctions.contains(crit.getLeftExpression()))) {
                        	i++;
                        }
                    }
                }
                if (i<functionCount) {
                    handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit3), obj);
                }
            }
        }
        
    }

    // ######################### Validation methods #########################

    /**
     * A valid translated expression is not an <code>AggregateSymbol</code> and
     * does not include elements not present on the groups of the command using
     * the translated criteria.
     */
    @Removed(Version.TEIID_8_0)
    protected void validateTranslateCriteria(TranslateCriteriaImpl obj) {
        if(this.currentCommand == null) {
            return;
        }
        if (! isUpdateProcedure()) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0019), obj);
        }

        CreateUpdateProcedureCommandImpl updateProc = (CreateUpdateProcedureCommandImpl) this.createProc;
        Map<ElementSymbolImpl, BaseExpression> symbolMap = updateProc.getSymbolMap();
        CommandImpl userCommand = updateProc.getUserCommand();
        // modeler validation
        if(userCommand == null) {
            return;
        }
        CriteriaImpl userCrit = null;
        int userCmdType = userCommand.getType();
        switch(userCmdType) {
            case Command.TYPE_DELETE:
                userCrit = ((DeleteImpl)userCommand).getCriteria();
                break;
            case Command.TYPE_UPDATE:
                userCrit = ((UpdateImpl)userCommand).getCriteria();
                break;
            default:
                break;
        }
        // nothing to validate if there is no user criteria
        if(userCrit == null) {
            return;
        }

        Collection<ElementSymbolImpl> transleElmnts = ElementCollectorVisitorImpl.getElements(obj, true);
        Collection<GroupSymbolImpl> groups = GroupCollectorVisitorImpl.getGroups(this.currentCommand, true);
        Operator selectType = obj.getSelector().getSelectorType();

        for (CriteriaImpl predCrit : CriteriaImpl.separateCriteriaByAnd(userCrit)) {
            if(selectType != Operator.NO_TYPE) {
                if(predCrit instanceof CompareCriteriaImpl) {
                    CompareCriteriaImpl ccCrit = (CompareCriteriaImpl) predCrit;
                    if(selectType.getIndex() != ccCrit.getOperator()) {
                        continue;
                    }
                } else if(predCrit instanceof MatchCriteriaImpl) {
                    if(selectType != Operator.LIKE) {
                        continue;
                    }
                } else if(predCrit instanceof IsNullCriteriaImpl) {
                    if(selectType != Operator.IS_NULL) {
                        continue;
                    }
                } else if(predCrit instanceof SetCriteriaImpl) {
                    if(selectType != Operator.IN) {
                        continue;
                    }
                } else if(predCrit instanceof BetweenCriteriaImpl) {
                    if(selectType != Operator.BETWEEN) {
                        continue;
                    }
                }
            }
            Iterator<ElementSymbolImpl> critEmlntIter = ElementCollectorVisitorImpl.getElements(predCrit, true).iterator();
            // collect all elements elements on the criteria map to
            while(critEmlntIter.hasNext()) {
                ElementSymbolImpl criteriaElement = critEmlntIter.next();
                if(transleElmnts.contains(criteriaElement)) {
                    BaseExpression mappedExpr = symbolMap.get(criteriaElement);
                    if(mappedExpr instanceof BaseAggregateSymbol) {
                        handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0022, criteriaElement), criteriaElement);
                    }

                    if (!groups.containsAll(GroupsUsedByElementsVisitorImpl.getGroups(mappedExpr))) {
                        handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0023, criteriaElement), criteriaElement);
                    }
                }
            }
        }
    }

    protected void validateSelectElements(SelectImpl obj) {
    	if(isXML) {
    		return;
    	}

        Collection<ElementSymbolImpl> elements = ElementCollectorVisitorImpl.getElements(obj, true);
        
        Collection<ElementSymbolImpl> cantSelect = validateElementsSupport(
            elements,
            SupportConstants.Element.SELECT );

		if(cantSelect != null) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0024, cantSelect), cantSelect);
		}
    }

    protected void validateHasProjectedSymbols(CommandImpl obj) {
        if(obj.getProjectedSymbols().size() == 0) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0025), obj);
        }
    }

    /**
     * Validate that no elements of type OBJECT are in a SELECT DISTINCT or
     * and ORDER BY.
     * @param symbols List of SingleElementSymbol
     */
    protected void validateSortable(List<? extends BaseExpression> symbols) {
    	for (BaseExpression expression : symbols) {
            validateSortable(expression);
        }
    }

	private void validateSortable(BaseExpression symbol) {
		if (isNonComparable(symbol)) {
		    handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0026, symbol), symbol);
		}
	}

    /**
     * @param symbol
     * @return whether symbol type is non-comparable
     */
    public static boolean isNonComparable(BaseExpression symbol) {
        DefaultDataTypeManager dataTypeManager = DefaultDataTypeManager.getInstance(symbol.getTeiidVersion());
        return dataTypeManager.isNonComparable(dataTypeManager.getDataTypeName(symbol.getType()));
    }

	/**
	 * This method can be used to validate Update commands cannot be
	 * executed against XML documents.
	 */
    protected void validateNoXMLUpdates(CommandImpl obj) {
     	if(isXMLCommand(obj)) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0029), obj);
     	}
    }

	/**
	 * This method can be used to validate commands used in the stored
	 * procedure languge cannot be executed against XML documents.
	 */
    protected void validateNoXMLProcedures(CommandImpl obj) {
     	if(isXMLCommand(obj)) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0030), obj);
     	}
    }

    private void validateXMLQuery(QueryImpl obj) {
        if(obj.getGroupBy() != null) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0031), obj);
        }
        if(obj.getHaving() != null) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0032), obj);
        }
        if(obj.getLimit() != null) {
            handleValidationError(Messages.getString(Messages.ValidationVisitor.limit_not_valid_for_xml), obj);
        }
        if (obj.getOrderBy() != null) {
        	OrderByImpl orderBy = obj.getOrderBy();
        	for (OrderByItemImpl item : orderBy.getOrderByItems()) {
				if (!(item.getSymbol() instanceof ElementSymbolImpl)) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.orderby_expression_xml), obj);
				}
			}
         }
    }
    
    protected void validateGroupSupportsUpdate(GroupSymbolImpl groupSymbol) {
    	try {
	    	if(! getMetadata().groupSupports(groupSymbol.getMetadataID(), SupportConstants.Group.UPDATE)) {
	            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0033, 
	                                                     SQLStringVisitorImpl.getSQLString(groupSymbol)), groupSymbol);
	        }
	    } catch (Exception e) {
	        handleException(e, groupSymbol);
	    }
    }
    
    protected void validateSetQuery(SetQueryImpl query) {
        // Walk through sub queries - validate each one separately and
        // also check the columns of each for comparability
        for (QueryCommandImpl subQuery : query.getQueryCommands()) {
            if(isXMLCommand(subQuery)) {
                handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0034), query);
            }
            if (subQuery instanceof QueryImpl && ((QueryImpl)subQuery).getInto() != null) {
            	handleValidationError(Messages.getString(Messages.ValidationVisitor.union_insert), query);
            }
        }
        
        if (!query.isAll() || query.getOperation() == Operation.EXCEPT || query.getOperation() == Operation.INTERSECT) {
            validateSortable(query.getProjectedSymbols());
        }
        
        if (query.isAll() && (query.getOperation() == Operation.EXCEPT || query.getOperation() == Operation.INTERSECT)) {
            handleValidationError(Messages.getString(Messages.ValidationVisitor.excpet_intersect_all), query);
        }
    }

    private void validateAggregates(QueryImpl query) {
        SelectImpl select = query.getSelect();
        GroupByImpl groupBy = query.getGroupBy();
        CriteriaImpl having = query.getHaving();
        validateNoAggsInClause(groupBy);
        List<GroupSymbolImpl> correlationGroups = null;
        validateNoAggsInClause(query.getCriteria());
        if (query.getFrom() == null) {
        	validateNoAggsInClause(select);
        	validateNoAggsInClause(query.getOrderBy());
        } else {
        	validateNoAggsInClause(query.getFrom());
        	correlationGroups = query.getFrom().getGroups();
        }
        
        Set<BaseExpression> groupSymbols = null;
        boolean hasAgg = false;
        if (groupBy != null) {
            groupSymbols = new HashSet<BaseExpression>(groupBy.getSymbols());
            hasAgg = true;
        }
        LinkedHashSet<BaseExpression> invalid = new LinkedHashSet<BaseExpression>();
        LinkedHashSet<BaseExpression> invalidWindowFunctions = new LinkedHashSet<BaseExpression>();
        LinkedList<BaseAggregateSymbol> aggs = new LinkedList<BaseAggregateSymbol>();
        if (having != null) {
            validateCorrelatedReferences(query, correlationGroups, groupSymbols, having, invalid);
        	AggregateSymbolCollectorVisitorImpl.getAggregates(having, aggs, invalid, null, invalidWindowFunctions, groupSymbols);
        	hasAgg = true;
        }
        for (BaseExpression symbol : select.getProjectedSymbols()) {
        	if (hasAgg || !aggs.isEmpty()) {
        		validateCorrelatedReferences(query, correlationGroups, groupSymbols, symbol, invalid);
        	}
        	AggregateSymbolCollectorVisitorImpl.getAggregates(symbol, aggs, invalid, null, null, groupSymbols);                                            
        }
        if ((!aggs.isEmpty() || hasAgg) && !invalid.isEmpty()) {
    		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0037, invalid), invalid);
        }
        if (!invalidWindowFunctions.isEmpty()) {
        	handleValidationError(Messages.getString(Messages.TeiidParser.window_only_top_level, invalidWindowFunctions), invalidWindowFunctions);
        }
    }

    /**
     * This validation is more convoluted than needed since it is being run before rewrite/planning.
     * Ideally we would already have correlated references set on the subqueries.
     */
	private void validateCorrelatedReferences(QueryImpl query,
			final List<GroupSymbolImpl> correlationGroups, final Set<BaseExpression> groupingSymbols, BaseLanguageObject object, LinkedHashSet<BaseExpression> invalid) {
		if (query.getFrom() == null) {
			return;
		}
		ElementCollectorVisitorImpl ecv = new ElementCollectorVisitorImpl(getTeiidVersion(), invalid) {
			@Override
            public void visit(ElementSymbolImpl obj) {
				if (obj.isExternalReference() && correlationGroups.contains(obj.getGroupSymbol())
						 && (groupingSymbols == null || !groupingSymbols.contains(obj))) {
					super.visit(obj);
				}
			}
		};
		AggregateStopNavigator asn = new AggregateStopNavigator(ecv);
		object.acceptVisitor(asn);
	}

	private void validateNoAggsInClause(BaseLanguageObject clause) {
		if (clause == null) {
        	return;
        }
		LinkedHashSet<BaseExpression> aggs = new LinkedHashSet<BaseExpression>();
		AggregateSymbolCollectorVisitorImpl.getAggregates(clause, aggs, null, null, aggs, null);
		if (!aggs.isEmpty()) {
			handleValidationError(Messages.getString(Messages.TeiidParser.Aggregate_only_top_level, aggs), aggs);
		}
	}
    
    protected void validateInsert(InsertImpl obj) {
        Collection<ElementSymbolImpl> vars = obj.getVariables();
        Iterator<ElementSymbolImpl> varIter = vars.iterator();
        Collection values = obj.getValues();
        Iterator valIter = values.iterator();
        GroupSymbolImpl insertGroup = obj.getGroup();
        try {
            boolean multiSource = getMetadata().isMultiSource(getMetadata().getModelID(insertGroup.getMetadataID()));
            // Validate that all elements in variable list are updatable
        	for (ElementSymbolImpl insertElem : vars) {
                if(! getMetadata().elementSupports(insertElem.getMetadataID(), SupportConstants.Element.UPDATE)) {
                    handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0052, insertElem), insertElem);
                }
                if (multiSource && getMetadata().isMultiSourceElement(insertElem.getMetadataID())) {
                	multiSource = false;
                }
            }
        	if (multiSource) {
        		validateMultisourceInsert(insertGroup);
        	}

            // Get elements in the group.
    		Collection<ElementSymbolImpl> insertElmnts = new LinkedList<ElementSymbolImpl>(ResolverUtil.resolveElementsInGroup(insertGroup, getMetadata()));

    		// remove all elements specified in insert to get the ignored elements
    		insertElmnts.removeAll(vars);

    		for (ElementSymbolImpl nextElmnt : insertElmnts) {
				if(!getMetadata().elementSupports(nextElmnt.getMetadataID(), SupportConstants.Element.DEFAULT_VALUE) &&
					!getMetadata().elementSupports(nextElmnt.getMetadataID(), SupportConstants.Element.NULL) &&
                    !getMetadata().elementSupports(nextElmnt.getMetadataID(), SupportConstants.Element.AUTO_INCREMENT)) {
		                handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0053, new Object[] {insertGroup, nextElmnt}), nextElmnt);
				}
			}

            //check to see if the elements support nulls in metadata,
            // if any of the value present in the insert are null
            while(valIter.hasNext() && varIter.hasNext()) {
                BaseExpression nextValue = (BaseExpression) valIter.next();
                ElementSymbolImpl nextVar = varIter.next();
                if (EvaluatableVisitor.isFullyEvaluatable(nextValue, true)) {
                    try {
                        // If nextValue is an expression, evaluate it before checking for null
                        Object evaluatedValue = Evaluator.assess(nextValue);
                        if(evaluatedValue == null && ! getMetadata().elementSupports(nextVar.getMetadataID(), SupportConstants.Element.NULL)) {
                            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0055, nextVar), nextVar);
                        }
                    } catch(Exception e) {
                        //ignore for now, we don't have the context which could be the problem
                    }
                }
            }// end of while
        } catch(Exception e) {
            handleException(e, obj);
        } 
    }
    
    protected void validateSetClauseList(SetClauseListImpl list) {
    	Set<ElementSymbolImpl> dups = new HashSet<ElementSymbolImpl>();
	    HashSet<ElementSymbolImpl> changeVars = new HashSet<ElementSymbolImpl>();
	    for (SetClauseImpl clause : list.getClauses()) {
	    	ElementSymbolImpl elementID = clause.getSymbol();
	        if (!changeVars.add(elementID)) {
	        	dups.add(elementID);
	        }
		}
	    if(!dups.isEmpty()) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0062, dups), dups);
	    }
    }
    
    protected void validateUpdate(UpdateImpl update) {
        try {
            UpdateInfo info = update.getUpdateInfo();

            // list of elements that are being updated
		    for (SetClauseImpl entry : update.getChangeList().getClauses()) {
        	    ElementSymbolImpl elementID = entry.getSymbol();

                // Check that left side element is updatable
                if(! getMetadata().elementSupports(elementID.getMetadataID(), SupportConstants.Element.UPDATE)) {
                    handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0059, elementID), elementID);
                }
                
                Object metadataID = elementID.getMetadataID();
                if (getMetadata().isMultiSourceElement(metadataID)){
                	handleValidationError(Messages.getString(Messages.ValidationVisitor.multi_source_update_not_allowed, elementID), elementID);
                }

			    // Check that right expression is a constant and is non-null
                BaseExpression value = entry.getValue();    
                if (EvaluatableVisitor.isFullyEvaluatable(value, true)) {
                    try {
                        ConstantImpl c = getTeiidParser().createASTNode(ASTNodes.CONSTANT);
                        c.setValue(Evaluator.assess(value));
                        value = c;
                    } catch (Exception err) {
                    }
                }
                
                if(value instanceof ConstantImpl) {
    			    // If value is null, check that element supports this as a nullable column
                    if(((ConstantImpl)value).getValue() == null && ! getMetadata().elementSupports(elementID.getMetadataID(), SupportConstants.Element.NULL)) {
                        handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0060, SQLStringVisitorImpl.getSQLString(elementID)), elementID);
                    }// end of if
                } 
		    }
            if (info != null && info.isInherentUpdate()) {
            	validateUpdate(update, Command.TYPE_UPDATE, info);
            	Set<ElementSymbolImpl> updateCols = update.getChangeList().getClauseMap().keySet();
            	if (!info.hasValidUpdateMapping(updateCols)) {
            		handleValidationError(Messages.gs(Messages.TEIID.TEIID30376, updateCols), update);
            	}
            }
        } catch(Exception e) {
            handleException(e, update);
        }
        
        validateSetClauseList(update.getChangeList());
    }

	private void validateUpdate(BaseTargetedCommand update, int type, UpdateInfo info) {
		String error = ProcedureContainerResolver.validateUpdateInfo(update.getGroup(), type, info);
		if (error != null) {
			handleValidationError(error, update.getGroup());
		}
	}
    
    /**
     * Validates SELECT INTO queries.
     * @param query
     *
     */
    protected void validateSelectInto(QueryImpl query) {
        List<BaseExpression> symbols = query.getSelect().getProjectedSymbols();
        GroupSymbolImpl intoGroup = query.getInto().getGroup();
        validateInto(query, symbols, intoGroup);
    }

    private void validateInto(BaseLanguageObject query,
                                List<BaseExpression> symbols,
                                GroupSymbolImpl intoGroup) {
        try {
            List elementIDs = getMetadata().getElementIDsInGroupID(intoGroup.getMetadataID());
            
            // Check if there are too many elements in the SELECT clause
            if (symbols.size() != elementIDs.size()) {
                handleValidationError(Messages.getString(Messages.ValidationVisitor.select_into_wrong_elements, new Object[] {new Integer(elementIDs.size()), new Integer(symbols.size())}), query);
                return;
            }

            for (int symbolNum = 0; symbolNum < symbols.size(); symbolNum++) {
                BaseExpression symbol = symbols.get(symbolNum);
                Object elementID = elementIDs.get(symbolNum);
                // Check if supports updates
                if (!getMetadata().elementSupports(elementID, SupportConstants.Element.UPDATE)) {
                    handleValidationError(Messages.getString(Messages.ValidationVisitor.element_updates_not_allowed, getMetadata().getFullName(elementID)), intoGroup);
                }

                Class<?> symbolType = symbol.getType();
                String symbolTypeName = dataTypeManager.getDataTypeName(symbolType);
                String targetTypeName = getMetadata().getElementType(elementID);
                if (symbolTypeName.equals(targetTypeName)) {
                    continue;
                }
                if (!dataTypeManager.isImplicitConversion(symbolTypeName, targetTypeName)) { // If there's no implicit conversion between the two
                    Object[] params = new Object [] {symbolTypeName, targetTypeName, new Integer(symbolNum + 1), query};
                    handleValidationError(Messages.getString(Messages.ValidationVisitor.select_into_no_implicit_conversion, params), query);
                    continue;
                }
            }
        } catch (Exception e) {
            handleException(e, query);
        } 
    }
    
    private void validateRowLimitFunctionNotInInvalidCriteria(CriteriaImpl obj) {
        // Collect all occurrances of rowlimit and rowlimitexception functions
        List<FunctionImpl> rowLimitFunctions = new ArrayList<FunctionImpl>();
        FunctionCollectorVisitorImpl visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMIT.text());
        PreOrderNavigator.doVisit(obj, visitor);      
        visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.text());
        PreOrderNavigator.doVisit(obj, visitor); 
        if (rowLimitFunctions.size() > 0) {
            handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit3), obj);
        }
    }
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.BetweenCriteriaImpl)
     *
     */
    @Override
    public void visit(BetweenCriteriaImpl obj) {
    	if (isNonComparable(obj.getExpression())) {
    		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0027, obj),obj);    		
    	}
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }

    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.IsNullCriteriaImpl)
     *
     */
    @Override
    public void visit(IsNullCriteriaImpl obj) {
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }

    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.MatchCriteriaImpl)
     *
     */
    @Override
    public void visit(MatchCriteriaImpl obj) {
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }

    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.NotCriteriaImpl)
     *
     */
    @Override
    public void visit(NotCriteriaImpl obj) {
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }

    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.SetCriteriaImpl)
     *
     */
    @Override
    public void visit(SetCriteriaImpl obj) {
    	if (isNonComparable(obj.getExpression())) {
    		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0027, obj),obj);    		
    	}
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }

    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl)
     *
     */
    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
    	validateSubquery(obj);
    	if (isNonComparable(obj.getLeftExpression())) {
    		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0027, obj),obj);    		
    	}
        this.validateRowLimitFunctionNotInInvalidCriteria(obj);
    }
    
    @Override
    public void visit(OptionImpl obj) {
        List<String> dep = obj.getDependentGroups();
        List<String> notDep = obj.getNotDependentGroups();
        if (dep != null && !dep.isEmpty()
            && notDep != null && !notDep.isEmpty()) {
            String groupName = null;
            String notDepGroup = null;
            for (Iterator<String> i = dep.iterator(); i.hasNext();) {
                groupName = i.next();
                for (Iterator<String> j = notDep.iterator(); j.hasNext();) {
                    notDepGroup = j.next();
                    if (notDepGroup.equalsIgnoreCase(groupName)) {
                        handleValidationError(Messages.getString(Messages.ValidationVisitor.group_in_both_dep, groupName), obj);
                        return;
                    }
                }
            }
        }
    }
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.DynamicCommandImpl)
     */
    @Override
    public void visit(DynamicCommandImpl obj) {
        if (obj.getIntoGroup() != null) {
            validateInto(obj, obj.getAsColumns(), obj.getIntoGroup());
        }
        if (obj.getUsing() != null) {
        	validateSetClauseList(obj.getUsing());
        }
    }
    
    @Override
    public void visit(CreateImpl obj) {
    	if (!obj.getPrimaryKey().isEmpty()) {
    		validateSortable(obj.getPrimaryKey());
    	}
    	if (obj.getTableMetadata() != null) {
    		Table t = obj.getTableMetadata();
    		if (!t.getForeignKeys().isEmpty()) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.temp_fk, obj.getTable()), obj);
    		}
    	}
    }
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.DropImpl)
     */
    @Override
    public void visit(DropImpl drop) {
        if (!drop.getTable().isTempTable()) {
            handleValidationError(Messages.getString(Messages.ValidationVisitor.drop_of_nontemptable, drop.getTable()), drop);
        }
        try {
			if (getMetadata().isVirtualGroup(drop.getTable().getMetadataID())) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.drop_of_globaltemptable, drop.getTable()), drop);
			}
		} catch (Exception e) {
			handleException(e);
		}
    }
    
    @Override
    public void visit(CompareCriteriaImpl obj) {
    	if (isNonComparable(obj.getLeftExpression())) {
    		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0027, obj),obj);    		
    	}
    	
        // Validate use of 'rowlimit' and 'rowlimitexception' pseudo-functions - they cannot be nested within another
        // function, and their operands must be a nonnegative integers

        // Collect all occurrences of rowlimit function
        List rowLimitFunctions = new ArrayList();
        FunctionCollectorVisitorImpl visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMIT.text());
        PreOrderNavigator.doVisit(obj, visitor);   
        visitor = new FunctionCollectorVisitorImpl(getTeiidVersion(), rowLimitFunctions, FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.text());
        PreOrderNavigator.doVisit(obj, visitor);            
        final int functionCount = rowLimitFunctions.size();
        if (functionCount > 0) {
            FunctionImpl function = null;
            BaseExpression expr = null;
            if (obj.getLeftExpression() instanceof FunctionImpl) {
                FunctionImpl leftExpr = (FunctionImpl)obj.getLeftExpression();
                
                if (FunctionLibrary.FunctionName.ROWLIMIT.equalsIgnoreCase(leftExpr.getName()) ||
                    FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.equalsIgnoreCase(leftExpr.getName())) {
                    function = leftExpr;
                    expr = obj.getRightExpression();
                }
            } 
            if (function == null && obj.getRightExpression() instanceof FunctionImpl) {
                FunctionImpl rightExpr = (FunctionImpl)obj.getRightExpression();
                
                if (FunctionLibrary.FunctionName.ROWLIMIT.equalsIgnoreCase(rightExpr.getName()) ||
                FunctionLibrary.FunctionName.ROWLIMITEXCEPTION.equalsIgnoreCase(rightExpr.getName())) {
                    function = rightExpr;
                    expr = obj.getLeftExpression();
                }
            }
            if (function == null) {
                // must be nested, which is invalid
                handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit0), obj);
            } else {
                if (expr instanceof ConstantImpl) {
                    ConstantImpl constant = (ConstantImpl)expr;
                    if (constant.getValue() instanceof Integer) {
                        Integer integer = (Integer)constant.getValue();
                        if (integer.intValue() < 0) {
                            handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit1), obj);
                        }
                    } else {
                        handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit1), obj);
                    }
                } else if (expr instanceof ReferenceImpl) {
                	((ReferenceImpl)expr).setConstraint(new PositiveIntegerConstraint(Messages.ValidationVisitor.rowlimit1));
                } else {
                    handleValidationError(Messages.getString(Messages.ValidationVisitor.rowlimit1), obj);
                }
            }                 
        }
    }
    
    @Override
    public void visit(LimitImpl obj) {
        validateLimitExpression(obj, obj.getOffset());
        validateLimitExpression(obj, obj.getRowLimit());
    }

	private void validateLimitExpression(LimitImpl obj, BaseExpression limitExpr) {
		if (limitExpr != null) {
	        if (limitExpr instanceof ConstantImpl) {
	            Integer limit = (Integer)((ConstantImpl)limitExpr).getValue();
	            if (limit.intValue() < 0) {
	                handleValidationError(Messages.getString(Messages.ValidationVisitor.badlimit2), obj);
	            }
	        } else if (limitExpr instanceof ReferenceImpl) {
	        	((ReferenceImpl)limitExpr).setConstraint(LIMIT_CONSTRAINT); 
	        } else if (!EvaluatableVisitor.willBecomeConstant(limitExpr)) {
	        	handleValidationError(Messages.getString(Messages.ValidationVisitor.badlimit1), obj);
	        }
        }
	}
    
    @Override
    public void visit(XMLForestImpl obj) {
    	validateDerivedColumnNames(obj, obj.getArgs());
    	for (DerivedColumnImpl dc : obj.getArgs()) {
			if (dc.getAlias() == null) {
				continue;
			}
			validateQName(obj, dc.getAlias());
			validateXMLContentTypes(dc.getExpression(), obj);
		}
    }
    
    @Override
	@Since(Version.TEIID_8_0)
    public void visit(JSONObjectImpl obj) {
    	for (DerivedColumnImpl dc : obj.getArgs()) {
    		validateJSONValue(obj, dc.getExpression());
		}
    }
    
    @Override
    public void visit(BaseWindowFunction windowFunction) {
    	BaseAggregateSymbol.Type type = windowFunction.getFunction().getAggregateFunction();
    	switch (type) {
    	case RANK:
    	case DENSE_RANK:
    	case ROW_NUMBER:
    		if (windowFunction.getWindowSpecification().getOrderBy() == null) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.ranking_requires_order_by, windowFunction), windowFunction);
    		}
    		break;
    	case TEXTAGG:
    	case ARRAY_AGG:
    	case JSONARRAY_AGG:
    	case XMLAGG:
    	case STRING_AGG:
    		if (windowFunction.getWindowSpecification().getOrderBy() != null) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.window_order_by, windowFunction), windowFunction);
            }
    		break;
    	default:
    	    break;
    	}
    	validateNoSubqueriesOrOuterReferences(windowFunction);
        if (windowFunction.getFunction().getOrderBy() != null || (windowFunction.getFunction().isDistinct() && windowFunction.getWindowSpecification().getOrderBy() != null)) {
        	handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0042, new Object[] {windowFunction.getFunction(), windowFunction}), windowFunction);
        }
        if (windowFunction.getWindowSpecification().getPartition() != null) {
        	validateSortable(windowFunction.getWindowSpecification().getPartition());
        }
    }
    
    @Override
    public void visit(BaseAggregateSymbol obj) {
    	if (!inQuery) {
    		handleValidationError(Messages.getString(Messages.TeiidParser.Aggregate_only_top_level, obj), obj);
    		return;
    	}
    	if (obj.getAggregateFunction() == AggregateSymbol.Type.USER_DEFINED) {
    		AggregateAttributes aa = obj.getFunctionDescriptor().getMethod().getAggregateAttributes();
    		if (!aa.allowsDistinct() && obj.isDistinct()) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.uda_not_allowed, "DISTINCT", obj), obj); //$NON-NLS-1$
    		}
    		if (!aa.allowsOrderBy() && obj.getOrderBy() != null) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.uda_not_allowed, "ORDER BY", obj), obj); //$NON-NLS-1$
    		}
    		if (aa.isAnalytic() && !obj.isWindowed()) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.uda_analytic, obj), obj);  
    		}
    	}
    	if (obj.getCondition() != null) {
    		BaseExpression condition = obj.getCondition();
    		validateNoSubqueriesOrOuterReferences(condition);
    	}

        BaseExpression aggExp = null;
        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get())) {
            aggExp = obj.getExpression();
            validateNoNestedAggs(aggExp);
        } else {
            BaseExpression[] aggExps = obj.getArgs();
            for (BaseExpression expression : aggExps) {
                validateNoNestedAggs(expression);
            }
            if (aggExps.length > 0)
                aggExp = aggExps[0];
        }

        validateNoNestedAggs(obj.getOrderBy());
        validateNoNestedAggs(obj.getCondition());
        
        // Verify data type of aggregate expression
        AggregateSymbol.Type aggregateFunction = obj.getAggregateFunction();
        if((aggregateFunction == AggregateSymbol.Type.SUM || aggregateFunction == AggregateSymbol.Type.AVG) && obj.getType() == null) {
            handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0041, new Object[] {aggregateFunction, obj}), obj);
        } else if (obj.getType() != DefaultDataTypeManager.DefaultDataTypes.NULL.getTypeClass()) {
        	if (aggregateFunction == AggregateSymbol.Type.XMLAGG && aggExp.getType() != DefaultDataTypeManager.DefaultDataTypes.XML.getTypeClass()) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.non_xml, new Object[] {aggregateFunction, obj}), obj);
        	} else if (obj.isBoolean() && aggExp.getType() != DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass()) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.non_boolean, new Object[] {aggregateFunction, obj}), obj);
        	} else if (aggregateFunction == AggregateSymbol.Type.JSONARRAY_AGG) {
				validateJSONValue(obj, aggExp);
        	}
        }
        if((obj.isDistinct() ||
            aggregateFunction == AggregateSymbol.Type.MIN ||
            aggregateFunction == AggregateSymbol.Type.MAX) &&
            dataTypeManager.isNonComparable(dataTypeManager.getDataTypeName(aggExp.getType()))) {
    		handleValidationError(Messages.getString(Messages.ValidationVisitor.non_comparable, new Object[] {aggregateFunction, obj}), obj);
        }
        if(obj.isEnhancedNumeric()) {
        	if (!Number.class.isAssignableFrom(aggExp.getType())) {
        		handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0041, new Object[] {aggregateFunction, obj}), obj);
        	}
        	if (obj.isDistinct()) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_distinct, new Object[] {aggregateFunction, obj}), obj);
        	}
        }
    	if (obj.getAggregateFunction() != AggregateSymbol.Type.TEXTAGG) {
    		return;
    	}
    	TextLineImpl tl = (TextLineImpl)aggExp;
    	if (tl.isIncludeHeader()) {
    		validateDerivedColumnNames(obj, tl.getExpressions());
    	}
    	for (DerivedColumnImpl dc : tl.getExpressions()) {
			validateXMLContentTypes(dc.getExpression(), obj);
		}
    	validateTextOptions(obj, tl.getDelimiter(), tl.getQuote());
    	if (tl.getEncoding() != null) {
    		try {
    			Charset.forName(tl.getEncoding());
    		} catch (IllegalArgumentException e) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_encoding, tl.getEncoding()), obj);
    		}
    	}
    }

	@Since(Version.TEIID_8_0)
	private void validateJSONValue(BaseLanguageObject obj, BaseExpression expr) {
		if (expr.getType() != DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass()
		    && !dataTypeManager.isTransformable(
		                                               expr.getType(),
		                                               DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass())) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_json_value, expr, obj), obj);
		}
	}

	private void validateNoSubqueriesOrOuterReferences(BaseExpression expr) {
		if (!ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(expr).isEmpty()) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.filter_subquery, expr), expr);
		}
		for (ElementSymbolImpl es : ElementCollectorVisitorImpl.getElements(expr, false)) {
			if (es.isExternalReference()) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.filter_subquery, es), es);
			}
		}
	}
    
	private void validateNoNestedAggs(BaseLanguageObject aggExp) {
		// Check for any nested aggregates (which are not allowed)
        if(aggExp != null) {
        	HashSet<BaseExpression> nestedAggs = new LinkedHashSet<BaseExpression>();
            AggregateSymbolCollectorVisitorImpl.getAggregates(aggExp, nestedAggs, null, null, nestedAggs, null);
            if(!nestedAggs.isEmpty()) {
                handleValidationError(Messages.getString(Messages.ERR.ERR_015_012_0039, nestedAggs), nestedAggs);
            }
        }
	}
    
	private String[] validateQName(BaseLanguageObject obj, String name) {
		try {
			return Name11Checker.getInstance().getQNameParts(name);
		} catch (QNameException e) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_invalid_qname, name), obj);
		}
		return null;
	}

	private void validateDerivedColumnNames(BaseLanguageObject obj, List<DerivedColumnImpl> cols) {
		for (DerivedColumnImpl dc : cols) {
    		if (dc.getAlias() == null && !(dc.getExpression() instanceof ElementSymbolImpl)) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.expression_requires_name), obj);
        	} 
		}
	}
    
    @Override
    public void visit(XMLAttributesImpl obj) {
    	validateDerivedColumnNames(obj, obj.getArgs());
    	for (DerivedColumnImpl dc : obj.getArgs()) {
			if (dc.getAlias() == null) {
				continue;
			}
			if ("xmlns".equals(dc.getAlias())) { //$NON-NLS-1$
				handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_attributes_reserved), obj);
			}
			String[] parts = validateQName(obj, dc.getAlias());
			if (parts == null) {
				continue;
			}
			if ("xmlns".equals(parts[0])) { //$NON-NLS-1$
				handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_attributes_reserved, dc.getAlias()), obj);
			}
		}
    }
    
    @Override
    public void visit(XMLElementImpl obj) {
    	for (BaseExpression expression : obj.getContent()) {
    		validateXMLContentTypes(expression, obj);
    	}
    	validateQName(obj, obj.getName());
    }
    
    /**
     * @param expression
     * @param parent
     */
    public void validateXMLContentTypes(BaseExpression expression, BaseLanguageObject parent) {
		if (expression.getType() == DefaultDataTypeManager.DefaultDataTypes.OBJECT.getTypeClass() || expression.getType() == DefaultDataTypeManager.DefaultDataTypes.BLOB.getTypeClass()) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_content_type, expression), parent);
		}
    }
    
    @Override
    public void visit(QueryStringImpl obj) {
    	validateDerivedColumnNames(obj, obj.getArgs());
    }
    
    @Override
    public void visit(XMLTableImpl obj) {
    	List<DerivedColumnImpl> passing = obj.getPassing();
    	validatePassing(obj, obj.getXQueryExpression(), passing);
    	boolean hasOrdinal = false;
    	for (XMLColumnImpl xc : obj.getColumns()) {
			if (!xc.isOrdinal()) {
				if (xc.getDefaultExpression() != null && !EvaluatableVisitor.isFullyEvaluatable(xc.getDefaultExpression(), false)) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_default, xc.getDefaultExpression()), obj);
				}
				continue;
			}
			if (hasOrdinal) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.one_ordinal), obj);
				break;
			}
			hasOrdinal = true;
		}
    }
    
    @Override
	@Since(Version.TEIID_8_0)
    public void visit(ObjectTableImpl obj) {
    	List<DerivedColumnImpl> passing = obj.getPassing();
    	TreeSet<String> names = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
    	for (DerivedColumnImpl dc : passing) {
    		if (dc.getAlias() == null) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.context_item_not_allowed), obj);
        	} else if (!names.add(dc.getAlias())) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.duplicate_passing, dc.getAlias()), obj);
        	}
		}
    	Compilable scriptCompiler = null;
    	try {
			ScriptEngine engine = this.getMetadata().getScriptEngine(obj.getScriptingLanguage());
			obj.setScriptEngine(engine);
			if (engine instanceof Compilable) {
				scriptCompiler = (Compilable)engine;
				engine.put(ScriptEngine.FILENAME, SQLConstants.NonReserved.OBJECTTABLE);
				obj.setCompiledScript(scriptCompiler.compile(obj.getRowScript()));
			}
		} catch (Exception e) {
			handleValidationError(e.getMessage(), obj);
		}

    	for (ObjectColumnImpl xc : obj.getColumns()) {
    		if (scriptCompiler != null) {
    			try {
					xc.setCompiledScript(scriptCompiler.compile(xc.getPath()));
				} catch (ScriptException e) {
					handleValidationError(Messages.gs(Messages.TEIID.TEIID31110, xc.getPath(), e.getMessage()), obj); //$NON-NLS
				}
    		}
			if (xc.getDefaultExpression() != null && !EvaluatableVisitor.isFullyEvaluatable(xc.getDefaultExpression(), false)) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_default, xc.getDefaultExpression()), obj);
			}
		}
    }
    
    @Override
    public void visit(XMLQueryImpl obj) {
    	validatePassing(obj, obj.getXQueryExpression(), obj.getPassing());
    }

	private void validatePassing(BaseLanguageObject obj, SaxonXQueryExpression xqe, List<DerivedColumnImpl> passing) {
		boolean context = false;
    	boolean hadError = false;
    	TreeSet<String> names = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
    	for (DerivedColumnImpl dc : passing) {
    		if (dc.getAlias() == null) {
    			Class<?> type = dc.getExpression().getType();
    			if (type != DefaultDataTypeManager.DefaultDataTypes.XML.getTypeClass()) {
    				handleValidationError(Messages.getString(Messages.ValidationVisitor.context_item_type), obj);
    			}
    			if (context && !hadError) {
    				handleValidationError(Messages.getString(Messages.ValidationVisitor.passing_requires_name), obj);
    				hadError = true;
    			}
    			context = true;
        	} else { 
        		validateXMLContentTypes(dc.getExpression(), obj);
        		if (!names.add(dc.getAlias())) {
        			handleValidationError(Messages.getString(Messages.ValidationVisitor.duplicate_passing, dc.getAlias()), obj);
        		}
        	}
		}
    	if (xqe.usesContextItem() && !context) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.context_required), obj);    		
    	}
	}
    
    @Override
    public void visit(XMLNamespacesImpl obj) {
    	boolean hasDefault = false;
    	for (NamespaceItem item : obj.getNamespaceItems()) {
			if (item.getPrefix() != null) {
				if (item.getPrefix().equals("xml") || item.getPrefix().equals("xmlns")) { //$NON-NLS-1$ //$NON-NLS-2$
					handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_namespaces_reserved), obj);
				} else if (!Name11Checker.getInstance().isValidNCName(item.getPrefix())) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_namespaces_invalid, item.getPrefix()), obj);
				}
				if (item.getUri().length() == 0) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_namespaces_null_uri), obj);
				}
				continue;
			}
			if (hasDefault) {
				handleValidationError(Messages.getString(Messages.ValidationVisitor.xml_namespaces), obj);
				break;
			}
			hasDefault = true;
		}
    }
    
    @Override
    public void visit(TextTableImpl obj) {
    	boolean widthSet = false;
    	Character delimiter = null;
    	Character quote = null;
    	boolean usingSelector = false;
    	for (TextColumnImpl column : obj.getColumns()) {
    	    if (column.isOrdinal())
                continue;

			if (column.getWidth() != null) {
				widthSet = true;
				if (column.getWidth() < 0) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_negative), obj);
				}
			} else if (widthSet) {
    			handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_invalid_width), obj);
			}
			if (column.getSelector() != null) {
				usingSelector = true;
				if (obj.getSelector() != null && obj.getSelector().equals(column.getSelector())) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_selector_required), obj);
				}
			}
        	if (column.getPosition() != null && column.getPosition() < 0) {
	    		handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_negative), obj);
	    	}
		}
    	if (widthSet) {
    		if (obj.getDelimiter() != null || obj.getHeader() != null || obj.getQuote() != null || usingSelector) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_width), obj);
    		}
    	} else {
        	if (obj.getHeader() != null && obj.getHeader() < 0) {
	    		handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_negative), obj);
	    	}
        	if (!obj.isUsingRowDelimiter()) {
        		handleValidationError(Messages.getString(Messages.ValidationVisitor.fixed_option), obj);
        	}
    		delimiter = obj.getDelimiter();
    		quote = obj.getQuote();
			validateTextOptions(obj, delimiter, quote);
    	}
    	if (obj.getSkip() != null && obj.getSkip() < 0) {
    		handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_negative), obj);
    	}
    	if (usingSelector && obj.getSelector() == null) {
    		handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_selector_required), obj);
    	}
    }

	private void validateTextOptions(BaseLanguageObject obj, Character delimiter,
			Character quote) {
		if (quote == null) {
			quote = '"';
		} 
		if (delimiter == null) {
			delimiter = ',';
		}
		
		if (quote.equals(delimiter)) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_delimiter), obj);
		}
		if (quote.equals('\n') || delimiter.equals('\n')) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.text_table_newline), obj);
		}
	}
    
    @Override
    public void visit(XMLParseImpl obj) {
    	if (obj.getExpression().getType() != DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass() && 
    			obj.getExpression().getType() != DefaultDataTypeManager.DefaultDataTypes.CLOB.getTypeClass() &&
    			obj.getExpression().getType() != DefaultDataTypeManager.DefaultDataTypes.BLOB.getTypeClass()) {
    		handleValidationError(Messages.getString(Messages.ValidationVisitor.xmlparse_type), obj);
    	}
    }
    
    @Override
    public void visit(ExistsCriteriaImpl obj) {
    	validateSubquery(obj);
    }
    
    @Override
    public void visit(SubqueryFromClauseImpl obj) {
    	validateSubquery(obj);
    }
    
    @Override
    public void visit(LoopStatementImpl obj) {
    	validateSubquery(obj);
    }
    
    @Override
    public void visit(WithQueryCommandImpl obj) {
    	validateSubquery(obj);
    }
    
    @Override
    public void visit(AlterViewImpl obj) {
    	try {
    	    TCQueryResolver queryResolver = new TCQueryResolver(getTeiidVersion());
            queryResolver.validateProjectedSymbols(obj.getTarget(), getMetadata(), obj.getDefinition());
			DefaultValidator.validate(obj.getDefinition(), getMetadata(), this);
			validateAlterTarget(obj);
		} catch (QueryValidatorException e) {
            handleValidationError(e.getMessage(), obj.getDefinition());
        } catch (Exception e) {
            handleException(e);
        }
    }

	private void validateAlterTarget(AlterImpl<?> obj) {
		if (getMetadata().getImportedModels().contains(obj.getTarget().getSchema())) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_alter, obj.getTarget()), obj.getTarget());
		}
	}

    @Override
    public void visit(AlterProcedureImpl obj) {
    	GroupSymbolImpl gs = obj.getTarget();
    	validateAlterTarget(obj);
    	try {
	    	if (!gs.isProcedure() || !getMetadata().isVirtualModel(getMetadata().getModelID(gs.getMetadataID()))) {
	    		handleValidationError(Messages.getString(Messages.ValidationVisitor.not_a_procedure, gs), gs);
	    		return;
	    	}
	    	DefaultValidator.validate(obj.getDefinition(), getMetadata(), this);
	    	TCQueryResolver queryResolver = new TCQueryResolver(getTeiidVersion());
	    	StoredProcedureInfo<SPParameter, QueryNode> info = getMetadata().getStoredProcedureInfoForProcedure(gs.getName());
	    	for (SPParameter param : info.getParameters()) {
	    		if (param.getParameterType() == SPParameterImpl.RESULT_SET) {
	    	    	queryResolver.validateProjectedSymbols(gs, param.getResultSetColumns(), obj.getDefinition().getProjectedSymbols());
	    	    	break;
	    		}
	    	}
    	} catch (QueryValidatorException e) {
            CommandImpl command = obj.getDefinition();
            if (command instanceof CreateUpdateProcedureCommandImpl)
                handleValidationError(e.getMessage(), ((CreateUpdateProcedureCommandImpl) command).getBlock());
            else if (command instanceof CreateProcedureCommandImpl)
                handleValidationError(e.getMessage(), ((CreateProcedureCommandImpl) command).getBlock());
        } catch (Exception e) {
            handleException(e);
        }
    }
    
    @Override
    public void visit(BlockImpl obj) {
    	if (obj.getLabel() == null) {
    		return;
    	}
		for (BaseLanguageObject lo : stack) {
			if (lo instanceof Labeled) {
				Labeled labeled = (Labeled)lo;
	    		if (obj.getLabel().equalsIgnoreCase(labeled.getLabel())) {
	    			handleValidationError(Messages.getString(Messages.ValidationVisitor.duplicate_block_label, obj.getLabel()), obj);
	    		}
			}
		}
    }

    private void visit8(CommandStatementImpl obj) {
    	if (this.createProc == null || this.createProc.getResultSetColumns().isEmpty() || !obj.isReturnable() || !obj.getCommand().returnsResultSet()) {
    		return;
    	}
		List<? extends BaseExpression> symbols = obj.getCommand().getResultSetColumns();
		if (symbols == null && obj.getCommand() instanceof DynamicCommandImpl) {
			DynamicCommandImpl cmd = (DynamicCommandImpl)obj.getCommand();
			cmd.setAsColumns(this.createProc.getResultSetColumns());
			return;
		}
		try {
		    TCQueryResolver queryResolver = new TCQueryResolver(getTeiidVersion());
            queryResolver.validateProjectedSymbols(createProc.getVirtualGroup(), createProc.getResultSetColumns(), symbols);
		} catch (Exception e) {
			handleValidationError(Messages.gs(Messages.TEIID.TEIID31121, createProc.getVirtualGroup(), obj, e.getMessage()), obj);
		}
    }
    
    @Override
    public void visit(BranchingStatementImpl obj) {
		boolean matchedLabel = false;
		boolean inLoop = false;
		for (BaseLanguageObject lo : stack) {
			if (lo instanceof LoopStatementImpl || lo instanceof WhileStatementImpl) {
				inLoop = true;
				if (obj.getLabel() == null) {
					break;
				}
				matchedLabel |= obj.getLabel().equalsIgnoreCase(((Labeled)lo).getLabel());
			} else if (obj.getLabel() != null && lo instanceof BlockImpl && obj.getLabel().equalsIgnoreCase(((BlockImpl)lo).getLabel())) {
				matchedLabel = true;
				if (obj.getMode() != BranchingMode.LEAVE) {
					handleValidationError(Messages.getString(Messages.ValidationVisitor.invalid_label, obj.getLabel()), obj);
				}
			}
		}
		if (obj.getMode() != BranchingMode.LEAVE && !inLoop) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.no_loop), obj);
		}
		if (obj.getLabel() != null && !matchedLabel) {
			handleValidationError(Messages.getString(Messages.ValidationVisitor.unknown_block_label, obj.getLabel()), obj);
		}
    }
    
    @Override
    public void visit(AlterTriggerImpl obj) {
    	validateAlterTarget(obj);
    	validateGroupSupportsUpdate(obj.getTarget());
		try {
			if (obj.getDefinition() != null) {
				DefaultValidator.validate(obj.getDefinition(), getMetadata(), this);
			}			
		} catch (Exception e) {
			handleException(e);
		}
    }

    //TODO: it may be simpler to catch this in the parser
    private void validateSubquery(BaseSubqueryContainer<?> subQuery) {
    	if (subQuery.getCommand() instanceof QueryImpl && ((QueryImpl)subQuery.getCommand()).getInto() != null) {
        	handleValidationError(Messages.getString(Messages.ValidationVisitor.subquery_insert), subQuery.getCommand());
        }
    }
    
}
