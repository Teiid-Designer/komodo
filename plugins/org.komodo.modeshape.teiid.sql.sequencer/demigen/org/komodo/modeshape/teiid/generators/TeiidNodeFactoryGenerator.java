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
package org.komodo.modeshape.teiid.generators;

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
import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.parser.AbstractTeiidParser;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.symbol.Symbol;

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
    private static final String PACKAGE_NAME = "org.komodo.modeshape.teiid.sql.lang"; //$NON-NLS-1$
    private static final String CONSTANT_CLASS_PREFIX = "Teiid"; //$NON-NLS-1$
    private static final String CONSTANT_CLASS_POSTFIX = "ParserTreeConstants"; //$NON-NLS-1$
    private static final String NODENAME_FIELD = "jjtNodeName"; //$NON-NLS-1$
    private static final String VOID = "VOID"; //$NON-NLS-1$
    
    /* Methods that should be excluded from creation */
    private static final String[] COMPONENT_METHOD_EXCLUSIONS = { };

    private static final Map<String, String> AST_NODE_ANNOTATIONS = new HashMap<String, String>();

    /**
     * Writer for the generation of the modeshape cnd file
     */
    private final BufferedWriter factoryWriter;

    /**
     * Create new instance
     *
     * @throws Exception if generator fails
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

    private String createImports() throws Exception {
        StringBuffer buf = new StringBuffer();
        String imp = "import";

        Class<?>[] klazzes = { ITeiidParser.class,
                                             Messages.class
        };

        for (Class<?> klazz : klazzes) {
            buf.append(imp + SPACE + klazz.getCanonicalName() + SEMI_COLON + NEW_LINE);
        }

        Package[] SQL_PACKAGES = {
            ASTNode.class.getPackage(), // lang package
            Block.class.getPackage(), // proc package
            Symbol.class.getPackage() //symbol package
        };

        for (Package p : SQL_PACKAGES) {
            buf.append(imp + SPACE + p.getName() + DOT + STAR + SEMI_COLON + NEW_LINE);
        }

        buf.append(NEW_LINE);

        return buf.toString();
    }

    private String createClassDeclaration() throws Exception {
        StringBuffer buf = new StringBuffer();

        buf.append("/**" + NEW_LINE)
            .append(" * Factory used to generate parser nodes" + NEW_LINE)
            .append(" */" + NEW_LINE)
            .append(PUBLIC + SPACE + CLASS + SPACE + "TeiidNodeFactory" + SPACE + OPEN_BRACE + NEW_LINE)
            .append(NEW_LINE);

        // instance field
        buf.append(TAB + PRIVATE + SPACE + STATIC + SPACE + "TeiidNodeFactory instance" + SEMI_COLON + NEW_LINE)
             .append(NEW_LINE);

        // static getInstance method
        buf.append(TAB + "/**" + NEW_LINE)
             .append(TAB + " * @return Singleton instance of this factory" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @generated" + NEW_LINE)
             .append(TAB +" */" + NEW_LINE)
             .append(TAB + PUBLIC + SPACE + STATIC + SPACE + "TeiidNodeFactory getInstance() " + OPEN_BRACE + NEW_LINE)
             .append(TAB + TAB + "if (instance == null) instance = new TeiidNodeFactory();" + NEW_LINE)
             .append(TAB + TAB + "return instance;" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        // static jjcreate method
        buf.append(TAB + "/**" + NEW_LINE)
             .append(TAB + " * Method used by the generated parsers for constructing nodes" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @param teiidParser parent parser" + NEW_LINE)
             .append(TAB + " * @param nodeType type of node" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @return created language object" + NEW_LINE)
             .append(TAB + " *" + NEW_LINE)
             .append(TAB + " * @generated" + NEW_LINE)
             .append(TAB + " */" + NEW_LINE)
             .append(TAB + "public static LanguageObject jjtCreate(ITeiidParser teiidParser, int nodeType) " + OPEN_BRACE + NEW_LINE)
             .append(TAB + TAB + "return getInstance().create(teiidParser, nodeType);" + NEW_LINE)
             .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        // create method
        buf.append(TAB + "/**" + NEW_LINE)
            .append(TAB + " * Create a parser node for the node with the given common node name" + NEW_LINE)
            .append(TAB + " *" + NEW_LINE)
            .append(TAB + " * @param teiidParser parent parser" + NEW_LINE)
            .append(TAB + " * @param nodeType type of node" + NEW_LINE)
            .append(TAB + " *" + NEW_LINE)
            .append(TAB + " * @return node applicable to the given parser" + NEW_LINE)
            .append(TAB + " *" + NEW_LINE)
            .append(TAB + " * @generated" + NEW_LINE)
            .append(TAB + " */" + NEW_LINE)
            .append(TAB + "public <T extends LanguageObject> T create(ITeiidParser teiidParser, ASTNodes nodeType) " + OPEN_BRACE + NEW_LINE)
            .append(NEW_LINE)
            .append(TAB + TAB + "for (int i = 0; i < TeiidParserTreeConstants.jjtNodeName.length; ++i) " + OPEN_BRACE + NEW_LINE)
            .append(TAB + TAB + TAB + "String constantName = TeiidParserTreeConstants.jjtNodeName[i];" + NEW_LINE)
            .append(TAB + TAB + TAB + TAB + "if (! constantName.equalsIgnoreCase(nodeType.getName()))" + NEW_LINE)
            .append(TAB + TAB + TAB + TAB + TAB +"continue;" + NEW_LINE)
            .append(NEW_LINE)
            .append(TAB + TAB + TAB + "return create(teiidParser, i);" + NEW_LINE)
            .append(TAB + TAB + CLOSE_BRACE)
            .append(NEW_LINE)
            .append(TAB + TAB + "throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType.getName(), teiidParser.getVersion()));" + NEW_LINE)
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

    private String createMethodDeclaration() {
        StringBuffer buffer = new StringBuffer();

        buffer.append(TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * Create a teiid parser node for the given node type." + NEW_LINE);  //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @generated" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param teiidParser" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param nodeType" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @return teiid parser node" + NEW_LINE); //$NON-NLS-1$ 
        buffer.append(TAB + " */" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + "protected <T extends LanguageObject> T create(ITeiidParser teiidParser, int nodeType) {" + NEW_LINE); //$NON-NLS-1$ 

        return buffer.toString();
    }

    private String createSwitchCase(String astIdentifier, String typeName, String constantClassName) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(TAB + TAB + TAB + "case " + constantClassName + DOT + astIdentifier + ":" + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        buffer.append(TAB + TAB + TAB + TAB + "return (T) create" + typeName + "(teiidParser, nodeType)" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        
        return buffer.toString();
    }

    private String createComponentMethod(String typeName) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(TAB + "/**" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @generated" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " *" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param teiidParser" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @param nodeType" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " * @return" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + " */" + NEW_LINE); //$NON-NLS-1$
        buffer.append(TAB + "private " + typeName + " create" + typeName + "(ITeiidParser teiidParser, int nodeType) {" + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        buffer.append(TAB + TAB + "return new " + typeName + "(teiidParser, nodeType)" + SEMI_COLON + NEW_LINE); //$NON-NLS-1$ //$NON-NLS-2$
        buffer.append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE); 

        return buffer.toString();
    }
    
    private void generate() throws Exception {
        try {
            Set<String> componentMethods = new LinkedHashSet<String>();
            Set<String> astNodesEnum = new LinkedHashSet<String>();
            StringBuffer createMethodBuffer = new StringBuffer();
            List<String> componentMethodExclusions = Arrays.asList(COMPONENT_METHOD_EXCLUSIONS);

            astNodesEnum.add(createASTNodesEnumDeclaration());

            String constantClassName = CONSTANT_CLASS_PREFIX + CONSTANT_CLASS_POSTFIX;
            Class<?> constantClass = Class.forName(PACKAGE_NAME + DOT + constantClassName);

            /* Index node names against their camelcase equivalents */
            Map<String, String> nodeNameIndex = indexNodeNames(constantClass);

            createMethodBuffer.append(createMethodDeclaration());

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
                    componentMethods.add(createComponentMethod(typeName));

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
            Package p = AbstractTeiidParser.class.getPackage();
            write("package " + p.getName() + SEMI_COLON + NEW_LINE + NEW_LINE);

            // Write imports
            write(createImports());

            // Write class declaration
            write(createClassDeclaration());

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
     * @param args (not required)
     * @throws Exception if generator fails
     */
    public static void main(String[] args) throws Exception {
        TeiidNodeFactoryGenerator factory = new TeiidNodeFactoryGenerator();
        factory.generate();
    }
}