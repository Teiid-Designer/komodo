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
package org.teiid.query.generators;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.teiid.query.parser.AbstractTeiidClientParser;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.lang.AlterProcedure;
import org.teiid.query.sql.lang.Select;
import org.teiid.query.sql.lang.Teiid7ClientParserTreeConstants;
import org.teiid.query.sql.lang.Teiid8ClientParserTreeConstants;
import org.teiid.query.sql.lang.v7.Alter7Procedure;
import org.teiid.query.sql.lang.v8.Alter8Procedure;
import org.teiid.query.sql.proc.Block;
import org.teiid.query.sql.symbol.AggregateSymbol;
import org.teiid.query.sql.symbol.Symbol;
import org.teiid.query.sql.symbol.WindowFunction;
import org.teiid.query.sql.symbol.v7.Aggregate7Symbol;
import org.teiid.query.sql.symbol.v8.Aggregate8Symbol;
import org.teiid.runtime.client.Messages;

/**
 * Generator for creating node factory
 */
@SuppressWarnings( "nls" )
public class TeiidNodeFactoryGenerator implements GeneratorConstants {

    private static final String GENERATOR_HOME_DIR = DEMI_GEN_DIR + File.separator +
                                                            Utilities.convertPackageToDirPath(TeiidNodeFactoryGenerator.class.getPackage());

    private static final String FACTORY_DIR = GENERATOR_HOME_DIR + File.separator + DOT + DOT +
                                                                           File.separator + "parser";

    private static final String FACTORY_FILENAME = "TeiidNodeFactory.java";

    private static final String PREFIX = "JJT"; //$NON-NLS-1$
    private static final String NON_NLS = "//$NON-NLS-1$"; //$NON-NLS-1$
    private static final String PACKAGE_NAME = "org.teiid.query.sql.lang"; //$NON-NLS-1$
    private static final String TEIID_CLASS_PREFIX = "Teiid"; //$NON-NLS-1$
    private static final String TEIID_CLASS_POSTFIX = "ClientParser";
    private static final String NODENAME_FIELD = "jjtNodeName"; //$NON-NLS-1$
    private static final String VOID = "VOID"; //$NON-NLS-1$
    private static final String TEIID_PARSER_INTERFACE = TeiidClientParser.class.getSimpleName();
    private static final String TEIID_SEVEN_CONSTANT_CLASS = Teiid7ClientParserTreeConstants.class.getSimpleName();
    private static final String TEIID_EIGHT_CONSTANT_CLASS = Teiid8ClientParserTreeConstants.class.getSimpleName();

    /* Methods that should be excluded from creation */
    private static final String[] COMPONENT_METHOD_EXCLUSIONS = { };

    /* Classes that are interfaces and have version specific classes */
    private static final Class[] COMPONENT_INTERFACES = { AggregateSymbol.class,
                                                                                                       AlterProcedure.class,
                                                                                                       WindowFunction.class};

    private static final Map<String, String> AST_NODE_ANNOTATIONS = new HashMap<String, String>();

    static {
        AST_NODE_ANNOTATIONS.put("CreateUpdateProcedureCommand", "@Removed(Version.TEIID_8_0)"); //$NON-NLS-1$ //$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("CriteriaSelector", "@Removed(Version.TEIID_8_0)"); //$NON-NLS-1$ //$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("RaiseErrorStatement", "@Removed(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("TranslateCriteria", "@Removed(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("HasCriteria", "@Removed(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$

        AST_NODE_ANNOTATIONS.put("CreateProcedureCommand", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("ObjectTable", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("ReturnStatement", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("RaiseStatement", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("Array", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("ExceptionExpression", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
        AST_NODE_ANNOTATIONS.put("JSONObject", "@Since(Version.TEIID_8_0)");  //$NON-NLS-1$//$NON-NLS-2$
    }

    /**
     * Writer for the generation of the modeshape cnd file
     */
    private final BufferedWriter factoryWriter;

    /**
     * @throws Exception
     */
    public TeiidNodeFactoryGenerator() throws Exception {
        File factoryFile = new File(FACTORY_DIR, FACTORY_FILENAME);
        if (factoryFile.exists())
            factoryFile.delete();

        factoryWriter = new BufferedWriter(new FileWriter(factoryFile));
    }

    /**
     * Adds the given token to the cnd writer
     *
     * @param token
     * @throws Exception
     */
    private void write(String token) throws Exception {
        factoryWriter.write(token);
    }

    private String createIsTeiidParserMethodName(int serverVersion) {
        return "is" + TEIID_CLASS_PREFIX + serverVersion+ TEIID_CLASS_POSTFIX;
    }

    private String createTeiidVersionParserClass(int serverVersion) {
        return TEIID_CLASS_PREFIX + serverVersion + TEIID_CLASS_POSTFIX;
    }

    private String createTeiidConstantClass(int serverVersion) {
        switch (serverVersion) {
            case 7:
                return TEIID_SEVEN_CONSTANT_CLASS;
            case 8:
                return TEIID_EIGHT_CONSTANT_CLASS;
            default:
                throw new UnsupportedOperationException();
        }
    }
    
    private String createImports(int[] serverVersions) throws Exception {
        StringBuffer buf = new StringBuffer();
        String imp = "import";

        Package teiidParserPkg = TeiidClientParser.class.getPackage();
        for (int serverVersion : serverVersions) {
            buf.append(imp + SPACE + teiidParserPkg.getName() + DOT)
                 .append("v" + serverVersion + DOT + createTeiidVersionParserClass(serverVersion))
                 .append(SEMI_COLON + NEW_LINE);
        }

        Class<?>[] klazzes = { TeiidClientParser.class, Messages.class, 
                                             DefaultTeiidVersion.Version.class,
                                             Removed.class, Since.class };

        for (Class<?> klazz : klazzes) {
            buf.append(imp + SPACE + klazz.getCanonicalName() + SEMI_COLON + NEW_LINE);
        }

        Package[] SQL_PACKAGES = {
            Select.class.getPackage(), // lang package
            Block.class.getPackage(), // proc package
            Symbol.class.getPackage(), //symbol package
            Alter7Procedure.class.getPackage(), // lang.v7 package
            Alter8Procedure.class.getPackage(), // lang.v8 package
            Aggregate7Symbol.class.getPackage(), // symbol.v7 package
            Aggregate8Symbol.class.getPackage() // symbol.v8 package
        };

        for (Package p : SQL_PACKAGES) {
            buf.append(imp + SPACE + p.getName() + DOT + STAR + SEMI_COLON + NEW_LINE);
        }

        buf.append(NEW_LINE);

        return buf.toString();
    }

    private String createClassDeclaration(int[] serverVersions) throws Exception {
        StringBuffer buf = new StringBuffer();

        buf.append("/**" + NEW_LINE)
            .append(" * Factory for creating parser nodes" + NEW_LINE)
            .append(" */" + NEW_LINE)
            .append(PUBLIC + SPACE + CLASS + SPACE + "TeiidNodeFactory" + SPACE + OPEN_BRACE + NEW_LINE)
            .append(NEW_LINE);

        // instance field
        buf.append(TAB + PRIVATE + SPACE + STATIC + SPACE + "TeiidNodeFactory instance" + SEMI_COLON + NEW_LINE)
             .append(NEW_LINE);

        // static getInstance method
        buf.append(NEW_LINE)
             .append(TAB + "/**" + NEW_LINE)
             .append(TAB + " * @return singleton instance" + NEW_LINE)
             .append(TAB + " */" + NEW_LINE)
             .append(TAB + PUBLIC + SPACE + STATIC + SPACE + "TeiidNodeFactory getInstance() " + OPEN_BRACE + NEW_LINE)
             .append(TAB + TAB + "if (instance == null) instance = new TeiidNodeFactory();" + NEW_LINE)
             .append(TAB + TAB + "return instance;" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        // static isTeiidXParser method
        for (int serverVersion : serverVersions) {
            buf.append(NEW_LINE)
                 .append(TAB + PRIVATE + SPACE + STATIC + SPACE + "boolean " + createIsTeiidParserMethodName(serverVersion) + "(" + TEIID_PARSER_INTERFACE + " teiidParser) " + OPEN_BRACE + NEW_LINE) 
                 .append(TAB + TAB + "return teiidParser instanceof " + createTeiidVersionParserClass(serverVersion) + SEMI_COLON + NEW_LINE)
                 .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);
        }

        // static jjcreate method
        buf.append(TAB + "/**" + NEW_LINE)
             .append(TAB + " * Method used by the generated parsers for constructing nodes" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @param teiidParser" + NEW_LINE)
             .append(TAB + " * @param nodeType" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @return created language object" + NEW_LINE)
             .append(TAB + " */" + NEW_LINE)
             .append(TAB + "public static LanguageObject jjtCreate(" + TEIID_PARSER_INTERFACE + " teiidParser, int nodeType) " + OPEN_BRACE + NEW_LINE)
             .append(TAB + TAB + "return getInstance().create(teiidParser, nodeType);" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        // create method for server versions
        buf.append(TAB + "/**" + NEW_LINE)
             .append(TAB + " * Create a parser node for the given node type" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @param teiidParser" + NEW_LINE)
             .append(TAB + " * @param nodeType" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @return node applicable to the given parser" + NEW_LINE)
             .append(TAB + " */" + NEW_LINE)
             .append(TAB + "public <T extends LanguageObject> T create(" + TEIID_PARSER_INTERFACE + " teiidParser, int nodeType) " + OPEN_BRACE + NEW_LINE);

        for (int i = 0; i < serverVersions.length; ++i) {
            int serverVersion = serverVersions[i];
            buf.append(TAB + TAB);

            if (i > 0)
                buf.append("else ");

            buf.append("if (" + createIsTeiidParserMethodName(serverVersion) + "(teiidParser))" + NEW_LINE)
                 .append(TAB + TAB + TAB + "return create((" + createTeiidVersionParserClass(serverVersion) + ") teiidParser, nodeType);" + NEW_LINE);
        }

        buf.append(TAB +TAB + "throw new IllegalArgumentException(")
             .append("Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, ")
             .append("teiidParser.getVersion()));" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);
        
        // create method
        buf.append(TAB + "/**" + NEW_LINE)
            .append(TAB + " * Create a parser node for the node with the given common node name" + NEW_LINE)
            .append(TAB + " *" + NEW_LINE)
            .append(TAB + " * @param teiidParser" + NEW_LINE)
            .append(TAB + " * @param nodeType" + NEW_LINE)
            .append(TAB + " *" + NEW_LINE)
            .append(TAB + " * @return node applicable to the given parser" + NEW_LINE)
            .append(TAB + " */" + NEW_LINE)
            .append(TAB + "public <T extends LanguageObject> T create(" + TEIID_PARSER_INTERFACE + " teiidParser, ASTNodes nodeType) " + OPEN_BRACE + NEW_LINE)
            .append(NEW_LINE);

        for (int i = 0; i < serverVersions.length; ++i) {
            int serverVersion = serverVersions[i];

            buf.append(TAB + TAB);
            if (i > 0)
                buf.append("else" + SPACE);

            String constantClass = createTeiidConstantClass(serverVersion);
            buf.append("if (" + createIsTeiidParserMethodName(serverVersion) + "(teiidParser)) " + OPEN_BRACE + NEW_LINE)
                 .append(TAB +TAB + TAB + "for (int i = 0; i < " + constantClass)
                 .append(DOT + "jjtNodeName.length; ++i) ")
                 .append(OPEN_BRACE + NEW_LINE)
                 .append(TAB + TAB + TAB + TAB)
                 .append("String constantName = " + constantClass)
                 .append(DOT + "jjtNodeName[i];" + NEW_LINE)
                 .append(TAB + TAB + TAB + TAB + TAB)
                 .append("if (! constantName.equalsIgnoreCase(nodeType.getName()))" + NEW_LINE)
                 .append(TAB + TAB + TAB + TAB + TAB + TAB +"continue;" + NEW_LINE)
                 .append(NEW_LINE)
                 .append(TAB + TAB + TAB + TAB + "return create(teiidParser, i);" + NEW_LINE)
                 .append(TAB + TAB + TAB + CLOSE_BRACE + NEW_LINE)
                 .append(TAB + TAB + CLOSE_BRACE + NEW_LINE)
                 .append(NEW_LINE);
        }

        buf.append(TAB + TAB + "throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType.getName(), teiidParser.getVersion()));" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        return buf.toString();
    }

    private Map<String, String> indexNodeNames(Class<?> constantClass) throws NoSuchFieldException, IllegalAccessException {
        /*
         * Find the jjtNodeName declarations and index the
         * values in a map with their lower case names as keys.
         * Use this to convert the constants into their respective
         * camel case method names.
         */
        Field nodeNameField = constantClass.getField(NODENAME_FIELD);
        Object nodeNameObj = nodeNameField.get(null);
        String[] nodeNameFields = (String[]) nodeNameObj;
        Map<String, String> nodeNameMap = new HashMap<String, String>();
        for (String nodeName : nodeNameFields) {
            nodeNameMap.put(nodeName.toLowerCase(), nodeName);
        }

        return nodeNameMap;
    }

    private String createASTNodesEnumDeclaration() {
        StringBuffer buffer = new StringBuffer();

        buffer.append(TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * Names of AST nodes to allow creation outside of the parsers" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @generated" +  NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " */" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + "public enum ASTNodes {" + NEW_LINE); //$NON-NLS-1$

        return buffer.toString();
    }

    private String createASTNodeEnumValue(String typeName) {
        StringBuffer buffer = new StringBuffer();

        buffer.append(TAB + TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + " * " + typeName + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + " * @generated" +  NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + " */" + NEW_LINE); //$NON-NLS-1$

        String annotation = AST_NODE_ANNOTATIONS.get(typeName);
        if (annotation != null)
            buffer.append(TAB + TAB + annotation + NEW_LINE);

        buffer.append(TAB + TAB);
        for (int i = 0; i < typeName.length(); ++i) {
            Character c = typeName.charAt(i);

            // Avoid issues with sequences of capitals such as XMLSerialise
            Character c1 = null;
            if ((i + 1) < typeName.length())
                c1 = typeName.charAt(i + 1);

            if (i > 0 && Character.isUpperCase(c) && ! (c1 != null && Character.isUpperCase(c1)))
                buffer.append(UNDERSCORE);

            buffer.append(Character.toUpperCase(c));
        }
        buffer.append(OPEN_BRACKET + SPEECH_MARK);
        buffer.append(typeName);
        buffer.append(SPEECH_MARK + CLOSE_BRACKET);
        buffer.append(COMMA + SPACE + NON_NLS + NEW_LINE + NEW_LINE);

        return buffer.toString();
    }

    private String createASTNodesEnumMethods() {
        StringBuffer buffer = new StringBuffer();

        buffer.append(TAB + TAB + "private String name" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$
        buffer.append(NEW_LINE);
        buffer.append(TAB + TAB + "ASTNodes(String name) {" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + TAB + "this.name = name" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + "}" + NEW_LINE); //$NON-NLS-1$
        buffer.append(NEW_LINE);
        buffer.append(TAB + TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + " * @return Name of this common node" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + " */" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + "public String getName() {" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + TAB + "return name" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + TAB + "}" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + "}" + NEW_LINE); //$NON-NLS-1$

        return buffer.toString();
    }

    private String createMethodDeclaration(int serverVersion) {
        StringBuffer buffer = new StringBuffer();

        buffer.append("/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" * Create a version " + serverVersion + " teiid parser node for the given node type." + NEW_LINE);  //$NON-NLS-1$//$NON-NLS-2$
        buffer.append(" *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" * @generated" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" * @param teiidParser" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" * @param nodeType" + NEW_LINE); //$NON-NLS-1$
        buffer.append(" * @return version " +serverVersion + " teiid parser node" + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        buffer.append(" */" + NEW_LINE); //$NON-NLS-1$
        buffer.append("private <T extends LanguageObject> T create(" + createTeiidVersionParserClass(serverVersion) + " teiidParser, int nodeType) {" + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$

        return buffer.toString();
    }

    private String createSwitchCase(String astIdentifier, String typeName, String constantClassName) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(TAB + TAB + TAB + "case " + constantClassName + DOT + astIdentifier + ":" + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        buffer.append(TAB + TAB + TAB + TAB + "return (T) create" + typeName + "(teiidParser, nodeType)" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        
        return buffer.toString();
    }

    private String createComponentMethod(String typeName, int[] serverVersions) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @generated" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param teiidParser" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param nodeType" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @return" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " */" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + "private " + typeName + " create" + typeName + "(" + TEIID_PARSER_INTERFACE + " teiidParser, int nodeType) {" + NEW_LINE);
        
        boolean isInterface = false;
        for (Class<?> iface : COMPONENT_INTERFACES) {
            if (iface.getSimpleName().equals(typeName)) {
                isInterface = true;
                break;
            }
        }

        if (isInterface) {
            for (int i = 0; i < serverVersions.length; ++i) {
                int serverVersion = serverVersions[i];

                //
                // Implementation of interface type will have
                // the server version in the middle of it
                //
                StringBuffer typeSVName = new StringBuffer();
                for (int index = 0; index < typeName.length(); ++index) {
                    char c = typeName.charAt(index);
                    if (index > 0) {
                        if (Character.isUpperCase(c))
                            typeSVName.append(serverVersion);
                    }

                    typeSVName.append(c);
                }

                buffer.append(TAB + TAB);

                if (i > 0)
                    buffer.append("else ");

                buffer.append("if (" + createIsTeiidParserMethodName(serverVersion) + "(teiidParser))" + NEW_LINE);
                buffer.append(TAB + TAB + TAB + "return new " + typeSVName.toString())
                         .append(OPEN_BRACKET + OPEN_BRACKET)
                         .append(createTeiidVersionParserClass(serverVersion) + CLOSE_BRACKET + SPACE)
                         .append("teiidParser, nodeType" + CLOSE_BRACKET)
                         .append(SEMI_COLON + NEW_LINE); 
            }

            buffer.append(TAB + TAB + "throw new IllegalArgumentException" + OPEN_BRACKET)
                     .append("Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, ")
                     .append("teiidParser.getVersion()));")
                     .append(NEW_LINE + NEW_LINE);

        } else {
            buffer.append(TAB + TAB + TAB + "return new " + typeName + "(teiidParser, nodeType)" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        }

        buffer.append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE); 

        return buffer.toString();
    }
    
    private void generate(int... serverVersions) throws Exception {
        try {
            Set<String> componentMethods = new LinkedHashSet<String>();
            Set<String> astNodesEnum = new LinkedHashSet<String>();
            StringBuffer createMethodBuffer = new StringBuffer();
            List<String> componentMethodExclusions = Arrays.asList(COMPONENT_METHOD_EXCLUSIONS);

            astNodesEnum.add(createASTNodesEnumDeclaration());

            for (int serverVersion : serverVersions) {
                String constantClassName = createTeiidConstantClass(serverVersion);
                Class<?> constantClass = Class.forName(PACKAGE_NAME + DOT + constantClassName);

                /* Index node names against their camelcase equivalents */
                Map<String, String> nodeNameIndex = indexNodeNames(constantClass);

                createMethodBuffer.append(createMethodDeclaration(serverVersion));

                // Create switch statement
                createMethodBuffer.append(TAB + TAB + "switch (nodeType) {" + NEW_LINE); //$NON-NLS-1$

                for (Field field : constantClass.getFields()) {
                    String fieldName = field.getName();
                    if (!fieldName.startsWith(PREFIX))
                        continue;

                    if (fieldName.equalsIgnoreCase(PREFIX + VOID))
                        continue;

                    String astName = fieldName.substring(PREFIX.length());
                    String typeName = nodeNameIndex.get(astName.toLowerCase());

                    // Append to main create's switch statement
                    createMethodBuffer.append(createSwitchCase(fieldName, typeName, constantClassName));

                    // Create component method if not already created
                    if (!componentMethodExclusions.contains(typeName))
                        componentMethods.add(createComponentMethod(typeName, serverVersions));

                    // Create AST Node enum if not already created
                    astNodesEnum.add(createASTNodeEnumValue(typeName));
                }

                // Complete switch statement
                createMethodBuffer.append(TAB + TAB + "default:" + NEW_LINE); //$NON-NLS-1$
                createMethodBuffer.append(TAB + TAB + TAB);
                createMethodBuffer.append("throw new IllegalArgumentException(" //$NON-NLS-1$
                                          + "Messages.getString(Messages.TeiidParser.invalidNodeType, " //$NON-NLS-1$
                                          + "nodeType, teiidParser.getVersion()))"); //$NON-NLS-1$
                createMethodBuffer.append(SEMI_COLON + NEW_LINE);
                createMethodBuffer.append(TAB + TAB + "}" + NEW_LINE); //$NON-NLS-1$
                createMethodBuffer.append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);
            }

            // Replace the last enum value's comma with a semi-colon
            Iterator<String> iter = astNodesEnum.iterator();
            String lastValue = null;
            while (iter.hasNext()) {
                lastValue = iter.next();
            }
            astNodesEnum.remove(lastValue);
            lastValue = lastValue.replace(COMMA, SEMI_COLON);
            astNodesEnum.add(lastValue);

            // Write licence
            write(LICENSE);

            // Write package
            Package p = AbstractTeiidClientParser.class.getPackage();
            write("package " + p.getName() + SEMI_COLON + NEW_LINE + NEW_LINE);

            // Write imports
            write(createImports(serverVersions));

            // Write class declaration
            write(createClassDeclaration(serverVersions));

            // Complete AST Node Enum
            astNodesEnum.add(createASTNodesEnumMethods());

            // Write ASTNodes enum
            for (String value : astNodesEnum) {
                write(value);
            }

            // Write component methods
            for (String method : componentMethods) {
                write(method);
            }

            // Write create statement
            write(createMethodBuffer.toString());

            // Write closing class brace
            write(NEW_LINE + CLOSE_BRACE);

        } finally {
            factoryWriter.close();
        }
    }
    
    /**
     * Execute to auto-generate the factory methods based on the
     * TeiidnTreeParserConstants interfaces.
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        TeiidNodeFactoryGenerator factory = new TeiidNodeFactoryGenerator();
        factory.generate(7, 8);
    }
}