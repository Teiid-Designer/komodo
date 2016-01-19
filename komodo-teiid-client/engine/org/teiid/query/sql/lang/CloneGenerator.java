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
package org.teiid.query.sql.lang;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.teiid.core.util.StringUtil;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.proc.RaiseStatementImpl;
import org.teiid.query.sql.proc.ReturnStatementImpl;
import org.teiid.query.sql.proc.StatementImpl;
import org.teiid.query.sql.proc.TriggerActionImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.JSONObjectImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLAttributesImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;
import org.teiid.query.sql.symbol.v7.Aggregate7SymbolImpl;
import org.teiid.query.sql.symbol.v7.Window7FunctionImpl;
import org.teiid.query.sql.symbol.v8.Aggregate8SymbolImpl;
import org.teiid.query.sql.symbol.v8.Window8FunctionImpl;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class CloneGenerator {

    private static final String SEMI_COLON = ";";

    private static final String CLONE = "clone";

    private static final String RETURN = "return";

    private static final String CONSTRUCTOR_PARAMS = "(this.parser, this.id)";

    private static final String NEW = "new";

    private static final String EQUALS = "=";

    private static final String OVERRIDE = "@Override";

    private static final String TAB = "    ";

    private static final String OPEN_BRACE = "{";

    private static final String END_BRACE = "}";

    private static final String NEWLINE = "\n";

    private static final String PUBLIC = "public";

    private static final String SPACE = " ";

    private static final String CLONE_METHOD = "clone()";

    private static final String GET = "get";

    private static final String IS = "is";

    private static final String DOT = StringUtil.Constants.DOT;

    private static final String OPEN_BRACKET = "(";

    private static final String CLOSE_BRACKET = ")";

    private static final String IF = "if";

    private static final String NOT_NULL = "!= null";

    private static final String DIRECTORY = "/home/phantomjinx/programming/java/tdesigner/git/plugins/teiid/org.teiid.runtime.client/engine/";

    private static final String JAVA = ".java";
    
    private static final List<Class<?>> CLASS_LIST = new ArrayList<Class<?>>();

    private static final List<Class<?>> NON_CLONE_CLASSES = new ArrayList<Class<?>>();
    
    private final Map<Class<?>, File> classCache = new HashMap<Class<?>, File>();

    static {
        NON_CLONE_CLASSES.add(Boolean.class);
        NON_CLONE_CLASSES.add(String.class);
        NON_CLONE_CLASSES.add(Integer.class);
        NON_CLONE_CLASSES.add(Double.class);
        NON_CLONE_CLASSES.add(Float.class);
        NON_CLONE_CLASSES.add(SourceHintImpl.class);

        CLASS_LIST.add(Aggregate7SymbolImpl.class);
        CLASS_LIST.add(Aggregate8SymbolImpl.class);
        CLASS_LIST.add(AliasSymbolImpl.class);
        CLASS_LIST.add(AlterImpl.class);
        CLASS_LIST.add(ArrayTableImpl.class);
        CLASS_LIST.add(AssignmentStatementImpl.class);
        CLASS_LIST.add(BetweenCriteriaImpl.class);
        CLASS_LIST.add(BlockImpl.class);
        CLASS_LIST.add(BranchingStatementImpl.class);
        CLASS_LIST.add(CaseExpressionImpl.class);
        CLASS_LIST.add(CommandImpl.class);
        CLASS_LIST.add(CommandStatementImpl.class);
        CLASS_LIST.add(CompareCriteriaImpl.class);
        CLASS_LIST.add(CompoundCriteriaImpl.class);
        CLASS_LIST.add(ConstantImpl.class);
        CLASS_LIST.add(CreateProcedureCommandImpl.class);
        CLASS_LIST.add(CreateUpdateProcedureCommandImpl.class);
        CLASS_LIST.add(CriteriaImpl.class);
        CLASS_LIST.add(CriteriaSelectorImpl.class);
        CLASS_LIST.add(DeclareStatementImpl.class);
        CLASS_LIST.add(DeleteImpl.class);
        CLASS_LIST.add(DerivedColumnImpl.class);
        CLASS_LIST.add(DropImpl.class);
        CLASS_LIST.add(DynamicCommandImpl.class);
        CLASS_LIST.add(ElementSymbolImpl.class);
        CLASS_LIST.add(ExceptionExpressionImpl.class);
        CLASS_LIST.add(ExistsCriteriaImpl.class);
        CLASS_LIST.add(ExpressionCriteriaImpl.class);
        CLASS_LIST.add(ExpressionSymbolImpl.class);
        CLASS_LIST.add(FromClauseImpl.class);
        CLASS_LIST.add(FromImpl.class);
        CLASS_LIST.add(FunctionImpl.class);
        CLASS_LIST.add(GroupByImpl.class);
        CLASS_LIST.add(GroupSymbolImpl.class);
        CLASS_LIST.add(HasCriteriaImpl.class);
        CLASS_LIST.add(IfStatementImpl.class);
        CLASS_LIST.add(InsertImpl.class);
        CLASS_LIST.add(IntoImpl.class);
        CLASS_LIST.add(IsNullCriteriaImpl.class);
        CLASS_LIST.add(JoinPredicateImpl.class);
        CLASS_LIST.add(JoinTypeImpl.class);
        CLASS_LIST.add(JSONObjectImpl.class);
        CLASS_LIST.add(LimitImpl.class);
        CLASS_LIST.add(LoopStatementImpl.class);
        CLASS_LIST.add(MatchCriteriaImpl.class);
        CLASS_LIST.add(MultipleElementSymbolImpl.class);
        CLASS_LIST.add(NamespaceItem.class);
        CLASS_LIST.add(Node.class);
        CLASS_LIST.add(NotCriteriaImpl.class);
        CLASS_LIST.add(ObjectColumnImpl.class);
        CLASS_LIST.add(ObjectTableImpl.class);
        CLASS_LIST.add(OptionImpl.class);
        CLASS_LIST.add(OrderByItemImpl.class);
        CLASS_LIST.add(OrderByImpl.class);
        CLASS_LIST.add(ProjectedColumnImpl.class);
        CLASS_LIST.add(QueryCommandImpl.class);
        CLASS_LIST.add(QueryImpl.class);
        CLASS_LIST.add(QueryStringImpl.class);
        CLASS_LIST.add(RaiseErrorStatementImpl.class);
        CLASS_LIST.add(RaiseStatementImpl.class);
        CLASS_LIST.add(ReferenceImpl.class);
        CLASS_LIST.add(ReturnStatementImpl.class);
        CLASS_LIST.add(ScalarSubqueryImpl.class);
        CLASS_LIST.add(SearchedCaseExpressionImpl.class);
        CLASS_LIST.add(SelectImpl.class);
        CLASS_LIST.add(SetClauseImpl.class);
        CLASS_LIST.add(SetClauseListImpl.class);
        CLASS_LIST.add(SetCriteriaImpl.class);
        CLASS_LIST.add(SetQueryImpl.class);
        CLASS_LIST.add(SimpleNode.class);
        CLASS_LIST.add(StatementImpl.class);
        CLASS_LIST.add(StoredProcedureImpl.class);
        CLASS_LIST.add(SubqueryCompareCriteriaImpl.class);
        CLASS_LIST.add(SubqueryFromClauseImpl.class);
        CLASS_LIST.add(SubquerySetCriteriaImpl.class);
        CLASS_LIST.add(SymbolImpl.class);
        CLASS_LIST.add(TextColumnImpl.class);
        CLASS_LIST.add(TextLineImpl.class);
        CLASS_LIST.add(TextTableImpl.class);
        CLASS_LIST.add(TranslateCriteriaImpl.class);
        CLASS_LIST.add(TriggerActionImpl.class);
        CLASS_LIST.add(UnaryFromClauseImpl.class);
        CLASS_LIST.add(UpdateImpl.class);
        CLASS_LIST.add(Window7FunctionImpl.class);
        CLASS_LIST.add(Window8FunctionImpl.class);
        CLASS_LIST.add(WhileStatementImpl.class);
        CLASS_LIST.add(WindowSpecificationImpl.class);
        CLASS_LIST.add(WithQueryCommandImpl.class);
        CLASS_LIST.add(XMLAttributesImpl.class);
        CLASS_LIST.add(XMLColumnImpl.class);
        CLASS_LIST.add(XMLElementImpl.class);
        CLASS_LIST.add(XMLForestImpl.class);
        CLASS_LIST.add(XMLNamespacesImpl.class);
        CLASS_LIST.add(XMLParseImpl.class);
        CLASS_LIST.add(XMLQueryImpl.class);
        CLASS_LIST.add(XMLSerializeImpl.class);
        CLASS_LIST.add(XMLTableImpl.class);
        CLASS_LIST.add(CreateImpl.class);
    }

    private void cacheASTClasses() {

        for (Class<?> klazz : CLASS_LIST) {
            String packagePath = klazz.getPackage().getName();
            packagePath = packagePath.replaceAll("\\" + DOT, File.separator);

            StringBuffer fileName = new StringBuffer();
            fileName.append(packagePath);
            fileName.append(File.separator);
            
            if (klazz.getSimpleName().contains("7")) {
                fileName.append("v7");
                fileName.append(File.separator);
            } else if (klazz.getSimpleName().contains("8")) {
                fileName.append("v8");
                fileName.append(File.separator);
            }

            fileName.append(klazz.getSimpleName());
            fileName.append(JAVA);

            File jFile = new File(DIRECTORY, fileName.toString());
            if (! jFile.exists())
                throw new RuntimeException("The class file " + jFile.getAbsolutePath() + " does not exist");

            classCache.put(klazz, jFile);
        }
    }

    /**
     * @param key
     * @return
     */
    private StringBuffer createCloneMethod(Class<?> klazz) {
        StringBuffer  buffer = new StringBuffer(NEWLINE);
        String className = klazz.getSimpleName();

         buffer.append(TAB);
         buffer.append(OVERRIDE);
         buffer.append(NEWLINE);

         buffer.append(TAB);
         buffer.append(PUBLIC);
         buffer.append(SPACE);        
         buffer.append(className);
         buffer.append(SPACE);
         buffer.append(CLONE_METHOD);
         buffer.append(SPACE);
         buffer.append(OPEN_BRACE);
         buffer.append(NEWLINE);

         buffer.append(TAB);
         buffer.append(TAB);
         buffer.append(className);
         buffer.append(SPACE);
         buffer.append(CLONE);
         buffer.append(SPACE);
         buffer.append(EQUALS);
         buffer.append(SPACE);
         buffer.append(NEW);
         buffer.append(SPACE);
         buffer.append(className);
         buffer.append(CONSTRUCTOR_PARAMS);
         buffer.append(SEMI_COLON);
         buffer.append(NEWLINE);
         buffer.append(NEWLINE);
        
        for (Method method : klazz.getMethods()) {
            String methodName = method.getName();
            if (! methodName.startsWith("set"))
                continue;

            Class<?>[] params = method.getParameterTypes();

            String fieldName = methodName.substring(3);
            String getter = GET + fieldName;
            if (isBoolean(params[0]))
                getter = IS + fieldName;

            // Should only be 1 parameter
            boolean hasNonCloneParam = false;
            for (Class<?> nonCloneClass :  NON_CLONE_CLASSES) {
                if (nonCloneClass.getSimpleName().equalsIgnoreCase(params[0].getSimpleName())) {
                    hasNonCloneParam = true;
                    break;
                }
            }

            // check the param is not an enum
            if (! hasNonCloneParam && params[0].isEnum()) {
                hasNonCloneParam = true;
            }

            if (! isBoolean(params[0])) {
                buffer.append(TAB);
                buffer.append(TAB);
                buffer.append(IF);
                buffer.append(OPEN_BRACKET);
                buffer.append(getter);
                buffer.append(OPEN_BRACKET);
                buffer.append(CLOSE_BRACKET);
                buffer.append(SPACE);
                buffer.append(NOT_NULL);
                buffer.append(CLOSE_BRACKET);
                buffer.append(NEWLINE);
                buffer.append(TAB);
            }

            buffer.append(TAB);
            buffer.append(TAB);
            buffer.append(CLONE);
            buffer.append(DOT);
            buffer.append(methodName);
            buffer.append(OPEN_BRACKET);
            buffer.append(getter);
            buffer.append(OPEN_BRACKET);
            buffer.append(CLOSE_BRACKET);
            
            if(! hasNonCloneParam) {
                buffer.append(DOT);
                buffer.append(CLONE_METHOD);
            }

            buffer.append(CLOSE_BRACKET);
            buffer.append(SEMI_COLON);
            buffer.append(NEWLINE);
        }

        // Return statement
        buffer.append(NEWLINE);
        buffer.append(TAB);
        buffer.append(TAB);
        buffer.append(RETURN);
        buffer.append(SPACE);
        buffer.append(CLONE);
        buffer.append(SEMI_COLON);
        buffer.append(NEWLINE);
        
        buffer.append(TAB);
        buffer.append(END_BRACE);
        
        buffer.append(NEWLINE);
        buffer.append(NEWLINE);

        return  buffer;
    }

    private boolean isBoolean(Class<?> klazz) {
        return klazz.getSimpleName().equalsIgnoreCase(Boolean.class.getSimpleName());
    }

    private void writeMethod(StringBuffer method, File jFile) throws Exception {
        BufferedReader reader = new BufferedReader(new FileReader(jFile));
        BufferedWriter writer = null;
        StringBuffer content = new StringBuffer();

        try {
            String line = null;
            while ((line = reader.readLine()) != null) {
                content.append(line);
                content.append("\n");
            }

            int offset = content.lastIndexOf(END_BRACE);
            if (offset == -1)
                throw new RuntimeException("File " + jFile.getName() + " does not contain an accept comment!");

            content.insert(offset, method.toString());

            writer = new BufferedWriter(new FileWriter(jFile));
            writer.write(content.toString());

        } finally {
            reader.close();
            if (writer != null)
                writer.close();
        }
    }

    public void generate() throws Exception {
        cacheASTClasses();

        for (Entry<Class<?>, File> entry : classCache.entrySet()) {
            StringBuffer method = createCloneMethod(entry.getKey());
            writeMethod(method, entry.getValue());
        }
    }

    public static void main(String[] args) throws Exception {
        CloneGenerator cg = new CloneGenerator();
        cg.generate();
    }
}
