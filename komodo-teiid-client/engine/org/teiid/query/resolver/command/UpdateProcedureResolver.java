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
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.metadata.QueryMetadataInterface.SupportConstants;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.query.sql.proc.CreateProcedureCommand;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.api.exception.query.UnresolvedSymbolDescription;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.language.SQLConstants;
import org.teiid.language.SQLConstants.NonReserved;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.metadata.TempMetadataStore;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.CommandResolver;
import org.teiid.query.resolver.ProcedureContainerResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitorImpl;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.GroupContextImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.BaseExpressionStatement;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.ReturnStatementImpl;
import org.teiid.query.sql.proc.StatementImpl;
import org.teiid.query.sql.proc.StatementImpl.StatementType;
import org.teiid.query.sql.proc.TriggerActionImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.util.SymbolMap;
import org.teiid.query.sql.visitor.ResolveVirtualGroupCriteriaVisitor;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.runtime.client.Messages;

/**
 */
public class UpdateProcedureResolver extends CommandResolver {

    private final List<ElementSymbolImpl> exceptionGroup;

    private DefaultDataTypeManager dataTypeManager;

    /**
     * @param queryResolver
     */
    public UpdateProcedureResolver(TCQueryResolver queryResolver) {
        super(queryResolver);

        ElementSymbolImpl es1 = create(ASTNodes.ELEMENT_SYMBOL);
        es1.setName("STATE"); //$NON-NLS-1$
        es1.setType(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());

        ElementSymbolImpl es2 = create(ASTNodes.ELEMENT_SYMBOL);
        es2.setName("ERRORCODE"); //$NON-NLS-1$
        es2.setType(DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());

        ElementSymbolImpl es3 = create(ASTNodes.ELEMENT_SYMBOL);
        es3.setName("TEIIDCODE"); //$NON-NLS-1$
        es3.setType(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());

        ElementSymbolImpl es4 = create(ASTNodes.ELEMENT_SYMBOL);
        es4.setName(NonReserved.EXCEPTION);
        es4.setType(Exception.class);

        ElementSymbolImpl es5 = create(ASTNodes.ELEMENT_SYMBOL);
        es5.setName(NonReserved.CHAIN);
        es5.setType(Exception.class);

        exceptionGroup = Arrays.asList(es1, es2, es3, es4, es5);
    }

    /**
     * @return the dataTypeManager
     */
    public DefaultDataTypeManager getDataTypeManager() {
        if (dataTypeManager == null)
            dataTypeManager = DefaultDataTypeManager.getInstance(getTeiidVersion());

        return this.dataTypeManager;
    }

    private void resolveCommand(TriggerActionImpl ta, TempMetadataAdapter metadata, boolean resolveNullLiterals) throws Exception {

        CreateProcedureCommand<BlockImpl, GroupSymbolImpl, BaseExpression, TCLanguageVisitorImpl> cmd;
        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get())) {
            cmd = create(ASTNodes.CREATE_UPDATE_PROCEDURE_COMMAND);
        } else {
            cmd = create(ASTNodes.CREATE_PROCEDURE_COMMAND);
            //TODO: this is not generally correct - we should update the api to set the appropriate type
            ((CreateProcedureCommandImpl)cmd).setUpdateType(Command.TYPE_INSERT);
        }

        cmd.setBlock(ta.getBlock());
        cmd.setVirtualGroup(ta.getView());

        resolveBlock(cmd, ta.getBlock(), ta.getExternalGroupContexts(), metadata);
    }

    @Removed(Version.TEIID_8_0)
    @Deprecated
    private void resolveVirtualGroupElements(CreateUpdateProcedureCommandImpl procCommand, QueryMetadataInterface metadata)
        throws Exception {

        // virtual group on procedure
        GroupSymbolImpl virtualGroup = procCommand.getVirtualGroup();

        if (!metadata.isVirtualGroup(virtualGroup.getMetadataID())) {
            //if this is a compensating procedure, just return
            return;
        }

        ResolveVirtualGroupCriteriaVisitor.resolveCriteria(procCommand, virtualGroup, metadata);

        // get a symbol map between virtual elements and the elements that define
        // then in the query transformation, this info is used in evaluating/validating
        // has criteria/translate criteria clauses
        CommandImpl transformCmd;
        try {
            TCQueryResolver queryResolver = new TCQueryResolver(getTeiidVersion());
            transformCmd = queryResolver.resolveView(virtualGroup,
                                                     metadata.getVirtualPlan(virtualGroup.getMetadataID()),
                                                     SQLConstants.Reserved.SELECT,
                                                     metadata).getCommand();
        } catch (Exception e) {
            throw new QueryResolverException(e, e.getMessage());
        }

        List<BaseExpression> cloned = new ArrayList<BaseExpression>();
        for (BaseExpression item : transformCmd.getProjectedSymbols()) {
            cloned.add(item.clone());
        }

        Map<ElementSymbolImpl, BaseExpression> symbolMap = SymbolMap.createSymbolMap(virtualGroup, cloned, metadata).asMap();
        procCommand.setSymbolMap(symbolMap);
    }

    @Removed(Version.TEIID_8_0)
    @Deprecated
    private void resolveCommand(CreateUpdateProcedureCommandImpl procCommand, TempMetadataAdapter metadata, boolean resolveNullLiterals)
        throws Exception {

        //by creating a new group context here it means that variables will resolve with a higher precedence than input/changing
        GroupContextImpl externalGroups = procCommand.getExternalGroupContexts();

        List<ElementSymbolImpl> symbols = new LinkedList<ElementSymbolImpl>();

        // virtual group elements in HAS and TRANSLATE criteria have to be resolved
        if (procCommand.isUpdateProcedure()) {
            resolveVirtualGroupElements(procCommand, metadata);

            //add the default variables
            String countVar = ProcedureReservedWords.VARIABLES + SymbolImpl.SEPARATOR + ProcedureReservedWords.ROWS_UPDATED;
            ElementSymbolImpl updateCount = create(ASTNodes.ELEMENT_SYMBOL);
            updateCount.setName(countVar);
            updateCount.setType(DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
            symbols.add(updateCount);
        }

        String countVar = ProcedureReservedWords.VARIABLES + SymbolImpl.SEPARATOR + ProcedureReservedWords.ROWCOUNT;
        ElementSymbolImpl updateCount = create(ASTNodes.ELEMENT_SYMBOL);
        updateCount.setName(countVar);
        updateCount.setType(DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
        symbols.add(updateCount);

        ProcedureContainerResolver.addScalarGroup(getTeiidParser(),
                                                  ProcedureReservedWords.VARIABLES,
                                                  metadata.getMetadataStore(),
                                                  externalGroups,
                                                  symbols);
        resolveBlock(procCommand, procCommand.getBlock(), externalGroups, metadata);
    }

    private void resolveCommand(CreateProcedureCommandImpl command, TempMetadataAdapter metadata, boolean resolveNullLiterals)
        throws Exception {

        //by creating a new group context here it means that variables will resolve with a higher precedence than input/changing
        GroupContextImpl externalGroups = command.getExternalGroupContexts();

        List<ElementSymbolImpl> symbols = new LinkedList<ElementSymbolImpl>();

        String countVar = ProcedureReservedWords.VARIABLES + SymbolImpl.SEPARATOR + ProcedureReservedWords.ROWCOUNT;
        ElementSymbolImpl updateCount = create(ASTNodes.ELEMENT_SYMBOL);
        updateCount.setName(countVar);
        updateCount.setType(DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
        symbols.add(updateCount);

        ProcedureContainerResolver.addScalarGroup(getTeiidParser(),
                                                  ProcedureReservedWords.VARIABLES,
                                                  metadata.getMetadataStore(),
                                                  externalGroups,
                                                  symbols);

        resolveBlock(command, command.getBlock(), externalGroups, metadata);
    }

    /**
     * @see org.teiid.query.resolver.CommandResolver#resolveCommand(org.teiid.query.sql.lang.CommandImpl, TempMetadataAdapter, boolean)
     */
    @Override
    public void resolveCommand(CommandImpl command, TempMetadataAdapter metadata, boolean resolveNullLiterals) throws Exception {

        if (command instanceof CreateProcedureCommandImpl)
            resolveCommand((CreateProcedureCommandImpl)command, metadata, resolveNullLiterals);
        else if (command instanceof CreateUpdateProcedureCommandImpl)
            resolveCommand((CreateUpdateProcedureCommandImpl)command, metadata, resolveNullLiterals);
        else if (command instanceof TriggerActionImpl)
            resolveCommand((TriggerActionImpl)command, metadata, resolveNullLiterals);
        else
            throw new IllegalArgumentException();
    }

    /**
     * @param command
     * @param block
     * @param originalExternalGroups
     * @param metadata
     * @throws Exception
     */
    public void resolveBlock(CreateProcedureCommand<BlockImpl, GroupSymbolImpl, BaseExpression, TCLanguageVisitorImpl> command, BlockImpl block, GroupContextImpl originalExternalGroups, TempMetadataAdapter metadata)
        throws Exception {

        //create a new variable and metadata context for this block so that discovered metadata is not visible else where
        TempMetadataStore store = metadata.getMetadataStore().clone();
        metadata = new TempMetadataAdapter(metadata.getMetadata(), store);
        GroupContextImpl externalGroups = new GroupContextImpl(originalExternalGroups, null);

        //create a new variables group for this block
        GroupSymbolImpl variables = ProcedureContainerResolver.addScalarGroup(getTeiidParser(),
                                                                          ProcedureReservedWords.VARIABLES,
                                                                          store,
                                                                          externalGroups,
                                                                          new LinkedList<BaseExpression>());

        for (StatementImpl statement : block.getStatements()) {
            resolveStatement(command, statement, externalGroups, variables, metadata);
        }

        if (block.getExceptionGroup() != null) {
            //create a new variable and metadata context for this block so that discovered metadata is not visible else where
            store = metadata.getMetadataStore().clone();
            metadata = new TempMetadataAdapter(metadata.getMetadata(), store);
            externalGroups = new GroupContextImpl(originalExternalGroups, null);

            //create a new variables group for this block
            variables = ProcedureContainerResolver.addScalarGroup(getTeiidParser(),
                                                                  ProcedureReservedWords.VARIABLES,
                                                                  store,
                                                                  externalGroups,
                                                                  new LinkedList<BaseExpression>());
            isValidGroup(metadata, block.getExceptionGroup());

            if (block.getExceptionStatements() != null) {
                ProcedureContainerResolver.addScalarGroup(getTeiidParser(),
                                                          block.getExceptionGroup(),
                                                          store,
                                                          externalGroups,
                                                          exceptionGroup,
                                                          false);
                for (StatementImpl statement : block.getExceptionStatements()) {
                    resolveStatement(command, statement, externalGroups, variables, metadata);
                }
            }
        }
    }

    @SuppressWarnings( "incomplete-switch" )
    @Removed(Version.TEIID_8_0)
    @Deprecated
    private void resolveStatement(CreateUpdateProcedureCommandImpl command, StatementImpl statement, GroupContextImpl externalGroups, GroupSymbolImpl variables, TempMetadataAdapter metadata)
        throws Exception {
        ResolverVisitorImpl visitor = new ResolverVisitorImpl(getTeiidVersion());

        switch (statement.getType()) {
            case TYPE_IF:
                IfStatementImpl ifStmt = (IfStatementImpl)statement;
                CriteriaImpl ifCrit = ifStmt.getCondition();
                for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(ifCrit)) {
                    resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                }
                visitor.resolveLanguageObject(ifCrit, null, externalGroups, metadata);
                resolveBlock(command, ifStmt.getIfBlock(), externalGroups, metadata);
                if (ifStmt.hasElseBlock()) {
                    resolveBlock(command, ifStmt.getElseBlock(), externalGroups, metadata);
                }
                break;
            case TYPE_COMMAND:
                CommandStatementImpl cmdStmt = (CommandStatementImpl)statement;
                CommandImpl subCommand = cmdStmt.getCommand();

                TempMetadataStore discoveredMetadata = resolveEmbeddedCommand(metadata, externalGroups, subCommand);

                if (subCommand instanceof StoredProcedureImpl) {
                    StoredProcedureImpl sp = (StoredProcedureImpl)subCommand;
                    for (SPParameterImpl param : sp.getParameters()) {
                        SPParameter.ParameterInfo paramType = SPParameter.ParameterInfo.valueOf(param.getParameterType());
                        switch (paramType) {
                            case OUT:
                            case RETURN_VALUE:
                                if (param.getExpression() != null && !isAssignable(metadata, param)) {
                                    throw new QueryResolverException(
                                                                     Messages.gs(Messages.TEIID.TEIID30121, param.getExpression()));
                                }
                                sp.setCallableStatement(true);
                                break;
                            case INOUT:
                                if (!isAssignable(metadata, param)) {
                                    continue;
                                }
                                sp.setCallableStatement(true);
                                break;
                        }
                    }
                }

                if (discoveredMetadata != null) {
                    metadata.getMetadataStore().getData().putAll(discoveredMetadata.getData());
                }

                //dynamic commands need to be updated as to their implicitly expected projected symbols 
                if (subCommand instanceof DynamicCommandImpl) {
                    DynamicCommandImpl dynCommand = (DynamicCommandImpl)subCommand;

                    if (dynCommand.getIntoGroup() == null && !command.isUpdateProcedure() && !dynCommand.isAsClauseSet()
                        && !command.getProjectedSymbols().isEmpty()) {
                        dynCommand.setAsColumns(command.getProjectedSymbols());
                    }
                }

                if (!command.isUpdateProcedure()) {
                    //don't bother using the metadata when it doesn't matter
                    if (command.getResultsCommand() != null && command.getResultsCommand().getType() == Command.TYPE_DYNAMIC) {
                        DynamicCommandImpl dynamicCommand = (DynamicCommandImpl)command.getResultsCommand();
                        if (!dynamicCommand.isAsClauseSet()) {
                            dynamicCommand.setAsColumns(Collections.EMPTY_LIST);
                        }
                    }

                    if (subCommand.returnsResultSet()) {
                        //this could be the last select statement, set the projected symbol
                        //on the virtual procedure command
                        command.setResultsCommand(subCommand);
                    }
                }

                break;
            case TYPE_ERROR:
            case TYPE_ASSIGNMENT:
            case TYPE_DECLARE:
                BaseExpressionStatement exprStmt = (BaseExpressionStatement)statement;
                //first resolve the value.  this ensures the value cannot use the variable being defined
                if (exprStmt.getExpression() != null) {
                    BaseExpression expr = exprStmt.getExpression();
                    for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(expr)) {
                        resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                    }
                    visitor.resolveLanguageObject(expr, null, externalGroups, metadata);
                }

                //second resolve the variable
                if (statement.getType() == StatementType.TYPE_DECLARE) {
                    collectDeclareVariable((DeclareStatementImpl)statement, variables, metadata, externalGroups);
                } else if (statement.getType() == StatementType.TYPE_ASSIGNMENT) {
                    AssignmentStatementImpl assStmt = (AssignmentStatementImpl)statement;
                    visitor.resolveLanguageObject(assStmt.getVariable(), null, externalGroups, metadata);
                    if (!metadata.elementSupports(assStmt.getVariable().getMetadataID(), SupportConstants.Element.UPDATE)) {
                        throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30121, assStmt.getVariable()));
                    }
                    //don't allow variable assignments to be external
                    assStmt.getVariable().setIsExternalReference(false);
                }

                //third ensure the type matches
                if (exprStmt.getExpression() != null) {
                    Class<?> varType = exprStmt.getExpectedType();
                    Class<?> exprType = exprStmt.getExpression().getType();
                    if (exprType == null) {
                        throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30123));
                    }
                    String varTypeName = getDataTypeManager().getDataTypeName(varType);
                    exprStmt.setExpression(ResolverUtil.convertExpression(exprStmt.getExpression(), varTypeName, metadata));
                }
                break;
            case TYPE_WHILE:
                WhileStatementImpl whileStmt = (WhileStatementImpl)statement;
                CriteriaImpl whileCrit = whileStmt.getCondition();
                for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(whileCrit)) {
                    resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                }
                visitor.resolveLanguageObject(whileCrit, null, externalGroups, metadata);
                resolveBlock(command, whileStmt.getBlock(), externalGroups, metadata);
                break;
            case TYPE_LOOP:
                LoopStatementImpl loopStmt = (LoopStatementImpl)statement;
                String groupName = loopStmt.getCursorName();

                if (metadata.getMetadataStore().getTempGroupID(groupName) != null) {
                    throw new QueryResolverException(Messages.getString(Messages.ERR.ERR_015_012_0065));
                }

                //check - cursor name should not start with #
                if (GroupSymbolImpl.isTempGroupName(loopStmt.getCursorName())) {
                    throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30125, loopStmt.getCursorName()));
                }
                CommandImpl cmd = loopStmt.getCommand();
                resolveEmbeddedCommand(metadata, externalGroups, cmd);
                List<BaseExpression> symbols = cmd.getProjectedSymbols();

                //add the loop cursor group into its own context
                TempMetadataStore store = metadata.getMetadataStore().clone();
                metadata = new TempMetadataAdapter(metadata.getMetadata(), store);
                externalGroups = new GroupContextImpl(externalGroups, null);

                ProcedureContainerResolver.addScalarGroup(getTeiidParser(), groupName, store, externalGroups, symbols, false);

                resolveBlock(command, loopStmt.getBlock(), externalGroups, metadata);
                break;
        }
    }

    @SuppressWarnings( "incomplete-switch" )
    private void resolveStatement(CreateProcedureCommandImpl command, StatementImpl statement, GroupContextImpl externalGroups, GroupSymbolImpl variables, TempMetadataAdapter metadata)
        throws Exception {
        ResolverVisitorImpl visitor = new ResolverVisitorImpl(getTeiidVersion());

        switch (statement.getType()) {
            case TYPE_IF:
                IfStatementImpl ifStmt = (IfStatementImpl)statement;
                CriteriaImpl ifCrit = ifStmt.getCondition();
                for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(ifCrit)) {
                    resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                }
                visitor.resolveLanguageObject(ifCrit, null, externalGroups, metadata);
                resolveBlock(command, ifStmt.getIfBlock(), externalGroups, metadata);
                if (ifStmt.hasElseBlock()) {
                    resolveBlock(command, ifStmt.getElseBlock(), externalGroups, metadata);
                }
                break;
            case TYPE_COMMAND:
                CommandStatementImpl cmdStmt = (CommandStatementImpl)statement;
                CommandImpl subCommand = cmdStmt.getCommand();

                TempMetadataStore discoveredMetadata = resolveEmbeddedCommand(metadata, externalGroups, subCommand);

                if (subCommand instanceof StoredProcedureImpl) {
                    StoredProcedureImpl sp = (StoredProcedureImpl)subCommand;
                    for (SPParameterImpl param : sp.getParameters()) {
                        SPParameter.ParameterInfo paramType = SPParameter.ParameterInfo.valueOf(param.getParameterType());
                        switch (paramType) {
                            case OUT:
                            case RETURN_VALUE:
                                if (param.getExpression() != null && !isAssignable(metadata, param)) {
                                    throw new QueryResolverException(
                                                                     Messages.gs(Messages.TEIID.TEIID30121, param.getExpression()));
                                }
                                sp.setCallableStatement(true);
                                break;
                            case INOUT:
                                if (!isAssignable(metadata, param)) {
                                    continue;
                                }
                                sp.setCallableStatement(true);
                                break;
                        }
                    }
                }

                if (discoveredMetadata != null) {
                    metadata.getMetadataStore().getData().putAll(discoveredMetadata.getData());
                }

                //dynamic commands need to be updated as to their implicitly expected projected symbols 
                if (subCommand instanceof DynamicCommandImpl) {
                    DynamicCommandImpl dynCommand = (DynamicCommandImpl)subCommand;

                    if (dynCommand.getIntoGroup() == null && !dynCommand.isAsClauseSet()) {
                        if ((command.getResultSetColumns() != null && command.getResultSetColumns().isEmpty())
                            || !cmdStmt.isReturnable() || command.getResultSetColumns() == null) {
                            //we're not interested in the resultset
                            dynCommand.setAsColumns(Collections.EMPTY_LIST);
                        } else {
                            //should match the procedure
                            dynCommand.setAsColumns(command.getResultSetColumns());
                        }
                    }
                }

                if (command.getResultSetColumns() == null && cmdStmt.isReturnable() && subCommand.returnsResultSet()
                    && subCommand.getResultSetColumns() != null && !subCommand.getResultSetColumns().isEmpty()) {
                    command.setResultSetColumns(subCommand.getResultSetColumns());
                	if (command.getProjectedSymbols().isEmpty()) {
                		command.setProjectedSymbols(subCommand.getResultSetColumns());
                	}
                }

                break;
            case TYPE_ERROR:
            case TYPE_ASSIGNMENT:
            case TYPE_DECLARE:
            case TYPE_RETURN:
                BaseExpressionStatement exprStmt = (BaseExpressionStatement)statement;
                //first resolve the value.  this ensures the value cannot use the variable being defined
                if (exprStmt.getExpression() != null) {
                    BaseExpression expr = exprStmt.getExpression();
                    for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(expr)) {
                        resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                    }
                    visitor.resolveLanguageObject(expr, null, externalGroups, metadata);
                }

                //second resolve the variable
                switch (statement.getType()) {
                    case TYPE_DECLARE:
                        collectDeclareVariable((DeclareStatementImpl)statement, variables, metadata, externalGroups);
                        break;
                    case TYPE_ASSIGNMENT:
                        AssignmentStatementImpl assStmt = (AssignmentStatementImpl)statement;
                        visitor.resolveLanguageObject(assStmt.getVariable(), null, externalGroups, metadata);
                        if (!metadata.elementSupports(assStmt.getVariable().getMetadataID(), SupportConstants.Element.UPDATE)) {
                            throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30121, assStmt.getVariable()));
                        }
                        //don't allow variable assignments to be external
                        assStmt.getVariable().setIsExternalReference(false);
                        break;
                    case TYPE_RETURN:
                        ReturnStatementImpl rs = (ReturnStatementImpl)statement;
                        if (rs.getExpression() != null) {
                            if (command.getReturnVariable() == null) {
                                throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID31125, rs));
                            }
                            rs.setVariable(command.getReturnVariable().clone());
                        }
                        //else - we don't currently require the use of return for backwards compatibility
                        break;
                }

                //third ensure the type matches
                if (exprStmt.getExpression() != null) {
                    Class<?> varType = exprStmt.getExpectedType();
                    Class<?> exprType = exprStmt.getExpression().getType();
                    if (exprType == null) {
                        throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30123));
                    }
                    String varTypeName = getDataTypeManager().getDataTypeName(varType);
                    exprStmt.setExpression(ResolverUtil.convertExpression(exprStmt.getExpression(), varTypeName, metadata));
                    if (statement.getType() == StatementType.TYPE_ERROR) {
                        ResolverVisitorImpl.checkException(exprStmt.getExpression());
                    }
                }
                break;
            case TYPE_WHILE:
                WhileStatementImpl whileStmt = (WhileStatementImpl)statement;
                CriteriaImpl whileCrit = whileStmt.getCondition();
                for (BaseSubqueryContainer container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(whileCrit)) {
                    resolveEmbeddedCommand(metadata, externalGroups, container.getCommand());
                }
                visitor.resolveLanguageObject(whileCrit, null, externalGroups, metadata);
                resolveBlock(command, whileStmt.getBlock(), externalGroups, metadata);
                break;
            case TYPE_LOOP:
                LoopStatementImpl loopStmt = (LoopStatementImpl)statement;
                String groupName = loopStmt.getCursorName();

                isValidGroup(metadata, groupName);
                CommandImpl cmd = loopStmt.getCommand();
                resolveEmbeddedCommand(metadata, externalGroups, cmd);
                List<BaseExpression> symbols = cmd.getProjectedSymbols();

                //add the loop cursor group into its own context
                TempMetadataStore store = metadata.getMetadataStore().clone();
                metadata = new TempMetadataAdapter(metadata.getMetadata(), store);
                externalGroups = new GroupContextImpl(externalGroups, null);

                ProcedureContainerResolver.addScalarGroup(getTeiidParser(), groupName, store, externalGroups, symbols, false);

                resolveBlock(command, loopStmt.getBlock(), externalGroups, metadata);
                break;
            case TYPE_COMPOUND:
                resolveBlock(command, (BlockImpl)statement, externalGroups, metadata);
                break;
        }
    }

    private void resolveStatement(CreateProcedureCommand command, StatementImpl statement, GroupContextImpl externalGroups, GroupSymbolImpl variables, TempMetadataAdapter metadata)
        throws Exception {

        if (command instanceof CreateProcedureCommandImpl)
            resolveStatement((CreateProcedureCommandImpl)command, statement, externalGroups, variables, metadata);
        else if (command instanceof CreateUpdateProcedureCommandImpl)
            resolveStatement((CreateUpdateProcedureCommandImpl)command, statement, externalGroups, variables, metadata);
        else
            throw new IllegalArgumentException();
    }

    private void isValidGroup(TempMetadataAdapter metadata, String groupName) throws Exception {
        if (metadata.getMetadataStore().getTempGroupID(groupName) != null) {
            throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30124, groupName));
        }

        //check - cursor name should not start with #
        if (GroupSymbolImpl.isTempGroupName(groupName)) {
            throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30125, groupName));
        }
    }

    private boolean isAssignable(TempMetadataAdapter metadata, SPParameterImpl param) throws Exception {
        if (!(param.getExpression() instanceof ElementSymbolImpl)) {
            return false;
        }
        ElementSymbolImpl symbol = (ElementSymbolImpl)param.getExpression();

        return metadata.elementSupports(symbol.getMetadataID(), SupportConstants.Element.UPDATE);
    }

    private TempMetadataStore resolveEmbeddedCommand(TempMetadataAdapter metadata, GroupContextImpl groupContext, CommandImpl cmd)
        throws Exception {
        getQueryResolver().setChildMetadata(cmd, metadata.getMetadataStore(), groupContext);

        return getQueryResolver().resolveCommand(cmd, metadata.getMetadata());
    }

    private void collectDeclareVariable(DeclareStatementImpl obj, GroupSymbolImpl variables, TempMetadataAdapter metadata, GroupContextImpl externalGroups)
        throws Exception {
        ElementSymbolImpl variable = obj.getVariable();
        String typeName = obj.getVariableType();
        GroupSymbolImpl gs = variable.getGroupSymbol();
        if (gs == null) {
            String outputName = variable.getShortName();
            gs = create(ASTNodes.GROUP_SYMBOL);
            gs.setName(ProcedureReservedWords.VARIABLES);
            variable.setGroupSymbol(gs);
            variable.setOutputName(outputName);
        } else {
            if (gs.getSchema() != null || !gs.getShortName().equalsIgnoreCase(ProcedureReservedWords.VARIABLES)) {
                handleUnresolvableDeclaration(variable,
                                              Messages.getString(Messages.ERR.ERR_015_010_0031, new Object[] {
                                                  ProcedureReservedWords.VARIABLES, variable}));
            }
        }
        boolean exists = false;
        try {
            ResolverVisitorImpl visitor = new ResolverVisitorImpl(variable.getTeiidVersion());
            visitor.resolveLanguageObject(variable, null, externalGroups, metadata);
            exists = true;
        } catch (Exception e) {
            //ignore, not already defined
        }
        if (exists) {
            handleUnresolvableDeclaration(variable, Messages.getString(Messages.ERR.ERR_015_010_0032, variable.getOutputName()));
        }
        variable.setType(getDataTypeManager().getDataTypeClass(typeName));
        variable.setGroupSymbol(variables);
        TempMetadataID id = new TempMetadataID(
                                               variable.getName(),
                                               typeName.equalsIgnoreCase(SQLConstants.NonReserved.EXCEPTION) ? Exception.class : variable.getType());
        id.setUpdatable(true);
        variable.setMetadataID(id);
        //TODO: this will cause the variables group to loose it's cache of resolved symbols
        metadata.getMetadataStore().addElementToTempGroup(ProcedureReservedWords.VARIABLES, variable.clone());
    }

    private void handleUnresolvableDeclaration(ElementSymbolImpl variable, String description) throws QueryResolverException {
        UnresolvedSymbolDescription symbol = new UnresolvedSymbolDescription(variable.toString(), description);
        QueryResolverException e = new QueryResolverException(symbol.getDescription());
        e.setUnresolvedSymbols(Arrays.asList(new Object[] {symbol}));
        throw e;
    }

}
