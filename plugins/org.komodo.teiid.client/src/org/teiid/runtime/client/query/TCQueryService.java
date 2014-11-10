/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.teiid.runtime.client.query;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.spi.query.QueryFactory;
import org.komodo.spi.query.QueryParser;
import org.komodo.spi.query.QueryResolver;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.sql.ICommandCollectorVisitor;
import org.komodo.spi.query.sql.IElementCollectorVisitor;
import org.komodo.spi.query.sql.IFunctionCollectorVisitor;
import org.komodo.spi.query.sql.IGroupCollectorVisitor;
import org.komodo.spi.query.sql.IGroupsUsedByElementsVisitor;
import org.komodo.spi.query.sql.IPredicateCollectorVisitor;
import org.komodo.spi.query.sql.IReferenceCollectorVisitor;
import org.komodo.spi.query.sql.IResolverVisitor;
import org.komodo.spi.query.sql.ISQLStringVisitor;
import org.komodo.spi.query.sql.ISQLStringVisitorCallback;
import org.komodo.spi.query.sql.IValueIteratorProviderCollectorVisitor;
import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;
import org.komodo.spi.query.sql.symbol.ISymbol;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.udf.FunctionMethodDescriptor;
import org.komodo.spi.udf.FunctionParameterDescriptor;
import org.komodo.spi.udf.FunctionLibrary;
import org.komodo.spi.validator.IUpdateValidator;
import org.komodo.spi.validator.IValidator;
import org.komodo.spi.validator.IUpdateValidator.TransformUpdateType;
import org.komodo.spi.xml.IMappingDocumentFactory;
import org.teiid.core.types.JDBCSQLTypeInfo;
import org.teiid.language.SQLConstants;
import org.teiid.metadata.FunctionMethod;
import org.teiid.metadata.FunctionMethod.Determinism;
import org.teiid.metadata.FunctionParameter;
import org.teiid.query.function.TCFunctionDescriptor;
import org.teiid.query.function.DefaultFunctionLibrary;
import org.teiid.query.function.FunctionTree;
import org.teiid.query.function.SystemFunctionManager;
import org.teiid.query.function.UDFSource;
import org.teiid.query.parser.TCQueryParser;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.resolver.util.ResolverVisitor;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.Command;
import org.teiid.query.sql.symbol.GroupSymbol;
import org.teiid.query.sql.visitor.CallbackSQLStringVisitor;
import org.teiid.query.sql.visitor.CommandCollectorVisitor;
import org.teiid.query.sql.visitor.ElementCollectorVisitor;
import org.teiid.query.sql.visitor.FunctionCollectorVisitor;
import org.teiid.query.sql.visitor.GroupCollectorVisitor;
import org.teiid.query.sql.visitor.GroupsUsedByElementsVisitor;
import org.teiid.query.sql.visitor.SQLStringVisitor;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitor;
import org.teiid.query.validator.PredicateCollectorVisitor;
import org.teiid.query.validator.ReferenceCollectorVisitor;
import org.teiid.query.validator.UpdateValidator;
import org.teiid.query.validator.UpdateValidator.UpdateType;
import org.teiid.query.validator.Validator;
import org.teiid.runtime.client.proc.TCProcedureService;
import org.teiid.runtime.client.xml.MappingDocumentFactory;

/**
 *
 */
public class TCQueryService implements QueryService {

    private final TeiidVersion teiidVersion;

    private TCQueryParser queryParser;

    private final SystemFunctionManager systemFunctionManager;

    private SyntaxFactory factory;

    /**
     * @param teiidVersion
     */
    public TCQueryService(TeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
        systemFunctionManager = new SystemFunctionManager(teiidVersion, getClass().getClassLoader());
    }

    /**
     * @return a query parser applicable to the given teiid instance version
     */
    @Override
    public QueryParser getQueryParser() {
        if (queryParser == null) {
            queryParser = new TCQueryParser(teiidVersion);
        }

        return queryParser;
    }

    @Override
    public boolean isReservedWord(String word) {
        return SQLConstants.isReservedWord(teiidVersion, word);
    }

    @Override
    public boolean isProcedureReservedWord(String word) {
        return ProcedureReservedWords.isProcedureReservedWord(teiidVersion, word);
    }

    @Override
    public Set<String> getReservedWords() {
        return SQLConstants.getReservedWords(teiidVersion);
    }

    @Override
    public Set<String> getNonReservedWords() {
        return SQLConstants.getNonReservedWords(teiidVersion);
    }

    @Override
    public String getJDBCSQLTypeName(int jdbcType) {
        return JDBCSQLTypeInfo.getTypeName(jdbcType);
    }

    @Override
    public FunctionLibrary createFunctionLibrary() {
        return new DefaultFunctionLibrary(teiidVersion, systemFunctionManager.getSystemFunctions(), new FunctionTree[0]);
    }

    @Override
    public FunctionLibrary createFunctionLibrary(List<FunctionMethodDescriptor> functionMethodDescriptors) {

        // Dynamically return a function library for each call rather than cache it here.
        Map<String, FunctionTree> functionTrees = new HashMap<String, FunctionTree>();

        for (FunctionMethodDescriptor descriptor : functionMethodDescriptors) {

            List<FunctionParameter> inputParameters = new ArrayList<FunctionParameter>();
            for (FunctionParameterDescriptor paramDescriptor : descriptor.getInputParameters()) {
                inputParameters.add(new FunctionParameter(paramDescriptor.getName(), paramDescriptor.getType()));
            }

            FunctionParameter outputParameter = new FunctionParameter(descriptor.getOutputParameter().getName(),
                                                                      descriptor.getOutputParameter().getType());

            FunctionMethod fMethod = new FunctionMethod(descriptor.getName(), descriptor.getDescription(),
                                                        descriptor.getCategory(), descriptor.getInvocationClass(),
                                                        descriptor.getInvocationMethod(),
                                                        inputParameters.toArray(new FunctionParameter[0]), outputParameter);

            fMethod.setPushDown(descriptor.getPushDownLiteral());
            fMethod.setVarArgs(descriptor.isVariableArgs());
            if (descriptor.isDeterministic()) {
                fMethod.setDeterminism(Determinism.DETERMINISTIC);
            } else {
                fMethod.setDeterminism(Determinism.NONDETERMINISTIC);
            }

            FunctionTree tree = functionTrees.get(descriptor.getSchema());
            if (tree == null) {
                tree = new FunctionTree(teiidVersion, descriptor.getSchema(), new UDFSource(Collections.EMPTY_LIST, getClass().getClassLoader()), false);
                functionTrees.put(descriptor.getSchema(), tree);
            }

            TCFunctionDescriptor fd = tree.addFunction(descriptor.getSchema(), null, fMethod, false);
            fd.setMetadataID(descriptor.getMetadataID());
        }

        return new DefaultFunctionLibrary(teiidVersion, systemFunctionManager.getSystemFunctions(),
                                   functionTrees.values().toArray(new FunctionTree[0]));
    }

    @Override
    public QueryFactory createQueryFactory() {
        if (factory == null)
            factory = new SyntaxFactory(((TCQueryParser)getQueryParser()).getTeiidParser());

        return factory;
    }

    @Override
    public IMappingDocumentFactory getMappingDocumentFactory() {
        getQueryParser();
        return new MappingDocumentFactory(queryParser.getTeiidParser());
    }

    @Override
    public String getSymbolName(IExpression expression) {
        if (expression instanceof ISymbol) {
            return ((ISymbol)expression).getName();
        }

        return "expr"; //$NON-NLS-1$
    }

    @Override
    public String getSymbolShortName(String name) {
        int index = name.lastIndexOf(ISymbol.SEPARATOR);
        if (index >= 0) {
            return name.substring(index + 1);
        }
        return name;
    }

    @Override
    public String getSymbolShortName(IExpression expression) {
        if (expression instanceof ISymbol) {
            return ((ISymbol)expression).getShortName();
        }
        return "expr"; //$NON-NLS-1$
    }

    @Override
    public SQLStringVisitor getSQLStringVisitor() {
        return new SQLStringVisitor(teiidVersion);
    }

    @Override
    public ISQLStringVisitor getCallbackSQLStringVisitor(ISQLStringVisitorCallback visitorCallback) {
        return new CallbackSQLStringVisitor(teiidVersion, visitorCallback);
    }

    @Override
    public IGroupCollectorVisitor getGroupCollectorVisitor(boolean removeDuplicates) {
        return new GroupCollectorVisitor(teiidVersion, removeDuplicates);
    }

    @Override
    public IGroupsUsedByElementsVisitor getGroupsUsedByElementsVisitor() {
        return new GroupsUsedByElementsVisitor();
    }

    @Override
    public IElementCollectorVisitor getElementCollectorVisitor(boolean removeDuplicates) {
        return new ElementCollectorVisitor(teiidVersion, removeDuplicates);
    }

    @Override
    public ICommandCollectorVisitor getCommandCollectorVisitor() {
        return new CommandCollectorVisitor(teiidVersion);
    }

    @Override
    public IFunctionCollectorVisitor getFunctionCollectorVisitor(boolean removeDuplicates) {
        return new FunctionCollectorVisitor(teiidVersion, removeDuplicates);
    }

    @Override
    public IPredicateCollectorVisitor getPredicateCollectorVisitor() {
        return new PredicateCollectorVisitor(teiidVersion);
    }

    @Override
    public IReferenceCollectorVisitor getReferenceCollectorVisitor() {
        return new ReferenceCollectorVisitor(teiidVersion);
    }

    @Override
    public IValueIteratorProviderCollectorVisitor getValueIteratorProviderCollectorVisitor() {
        return new ValueIteratorProviderCollectorVisitor(teiidVersion);
    }

    @Override
    public IResolverVisitor getResolverVisitor() {
        return new ResolverVisitor(teiidVersion);
    }

    @Override
    public IValidator getValidator() {
        return new Validator();
    }

    @Override
    public IUpdateValidator getUpdateValidator(QueryMetadataInterface metadata, TransformUpdateType tInsertType, TransformUpdateType tUpdateType, TransformUpdateType tDeleteType) {

        UpdateType insertType = UpdateType.valueOf(tInsertType.name());
        UpdateType updateType = UpdateType.valueOf(tUpdateType.name());
        UpdateType deleteType = UpdateType.valueOf(tDeleteType.name());

        return new UpdateValidator(metadata, insertType, updateType, deleteType);
    }

    @Override
    public void resolveGroup(IGroupSymbol groupSymbol, QueryMetadataInterface metadata) throws Exception {
        ResolverUtil.resolveGroup((GroupSymbol)groupSymbol, metadata);
    }

    @Override
    public void fullyQualifyElements(ICommand command) {
        ResolverUtil.fullyQualifyElements((Command)command);
    }

    @Override
    public QueryResolver getQueryResolver() {
        getQueryParser();
        return new TCQueryResolver((TCQueryParser)getQueryParser());
    }

    @Override
    public TCProcedureService getProcedureService() {
        return new TCProcedureService(teiidVersion);
    }
}
