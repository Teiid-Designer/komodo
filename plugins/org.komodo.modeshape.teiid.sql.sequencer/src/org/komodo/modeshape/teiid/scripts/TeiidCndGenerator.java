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
package org.komodo.modeshape.teiid.scripts;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.komodo.modeshape.teiid.scripts.CTree.CNode;
import org.komodo.modeshape.teiid.scripts.CTree.INode;
import org.komodo.modeshape.teiid.scripts.CTree.Node;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.LanguageObject;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.symbol.Symbol;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.ArgCheck;

/**
 *
 */
@SuppressWarnings( "nls" )
public class TeiidCndGenerator implements StringConstants {

    private static final String EXEC_HOME = DOT;

    private static final String SRC_DIR = EXEC_HOME + File.separator + SRC;
    
    private static final String GENERATOR_HOME_SRC_DIR = SRC_DIR + File.separator + convertPackageToDirPath(TeiidCndGenerator.class.getPackage());

    public static void main(String[] args) throws Exception {
        TeiidCndGenerator ccf = new TeiidCndGenerator(chooseTargetDirectory());
        ccf.generate();
    }

    /**
     * @param pkg
     */
    private static String convertPackageToDirPath(Package pkg) {
        return pkg.getName().replaceAll(DOUBLE_BACK_SLASH + DOT, File.separator);
    }

    private static File chooseTargetDirectory() {
        File genDir = new File(GENERATOR_HOME_SRC_DIR);
        File cndDir = new File(genDir.getParentFile(), "cnd");
        cndDir.mkdirs();
        if (! cndDir.exists())
            throw new RuntimeException("Cannot create target directory");

        return cndDir;
    }

    private static final String LICENSE = "" +
    "/*" + NEW_LINE +
    " * JBoss, Home of Professional Open Source." + NEW_LINE +
    " * See the COPYRIGHT.txt file distributed with this work for information" + NEW_LINE +
    " * regarding copyright ownership.  Some portions may be licensed" + NEW_LINE +
    " * to Red Hat, Inc. under one or more contributor license agreements." + NEW_LINE +
    " * " + NEW_LINE +
    " * This library is free software; you can redistribute it and/or" + NEW_LINE +
    " * modify it under the terms of the GNU Lesser General Public" + NEW_LINE +
    " * License as published by the Free Software Foundation; either" + NEW_LINE +
    " * version 2.1 of the License, or (at your option) any later version." + NEW_LINE +
    " * " + NEW_LINE +
    " * This library is distributed in the hope that it will be useful," + NEW_LINE +
    " * but WITHOUT ANY WARRANTY; without even the implied warranty of" + NEW_LINE +
    " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU" + NEW_LINE +
    " * Lesser General Public License for more details." + NEW_LINE +
    " * " + NEW_LINE +
    " * You should have received a copy of the GNU Lesser General Public" + NEW_LINE +
    " * License along with this library; if not, write to the Free Software" + NEW_LINE +
    " * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA" + NEW_LINE +
    " * 02110-1301 USA." + NEW_LINE +
    " */" + NEW_LINE;

    private static final Package[] SQL_PACKAGES = {
        ASTNode.class.getPackage(), // lang package
        Block.class.getPackage(), // proc package
        Symbol.class.getPackage() //symbol package
    };

    private static final String TSQL_PREFIX = "tsql";

    private static final String TEIID_SQL_CND = "TeiidSQL.cnd";

    private static final String[] MODESHAPE_NAMESPACES = {
        "<jcr='http://www.jcp.org/jcr/1.0'>",
        "<nt='http://www.jcp.org/jcr/nt/1.0'>",
        "<mix='http://www.jcp.org/jcr/mix/1.0'>",
        "<" + TSQL_PREFIX + "='http://www.teiid.org/sql/1.0'>"
    };

    /**
     * Prefix for setter methods
     */
    private static final String SET = "set";

    private static final String STRING = "STRING";
    private static final String BINARY = "BINARY";
    private static final String LONG = "LONG";
    private static final String DOUBLE = "DOUBLE";
    private static final String DATE = "DATE";
    private static final String BOOLEAN = "BOOLEAN";
    private static final String NAME = "NAME";
    private static final String PATH = "PATH";
    private static final String REFERENCE = "REFERENCE";
    private static final String WEAKREFERENCE = "WEAKREFERENCE";
    private static final String URI = "URI";
    private static final String DECIMAL = "DECIMAL";
    
    private static final String[] MODESHAPE_BASIC_TYPES = new String[] {
        STRING, BINARY, LONG, DOUBLE,DATE, BOOLEAN, NAME,
        PATH, REFERENCE, WEAKREFERENCE, URI, DECIMAL
    };

    private static final Map<String, String> PROPERTY_TYPE_MAP = new HashMap<String, String>();

    static {
        for (String basicPropType : MODESHAPE_BASIC_TYPES) {
            PROPERTY_TYPE_MAP.put(basicPropType, basicPropType);
        }

        PROPERTY_TYPE_MAP.put("INT", LONG);
        PROPERTY_TYPE_MAP.put("INTEGER", LONG);
        PROPERTY_TYPE_MAP.put("CHARACTER", STRING);
        PROPERTY_TYPE_MAP.put("CHAR", STRING);
    }

    // Modeshape attributes for properties and children
    private enum Attributes {
        PRIMARY(true),
        AUTOCREATED(true),
        MANDATORY(true),
        PROTECTED(true),
        MULTIPLE(true),
        COPY(false),
        VERSION(false),
        INITIALIZE(false),
        COMPUTE(false),
        IGNORE(false),
        ABORT(false),
        SNS(true);

        private boolean lowerCase;

        private Attributes(boolean lowerCase) {
            this.lowerCase = lowerCase;
        }

        public boolean isLowerCase() {
            return lowerCase;
        }
    }
    private final File cndTargetFile;

    private final BufferedWriter cndWriter;

    /**
     * @param targetDirectory
     */
    public TeiidCndGenerator(File targetDirectory) throws Exception {
        ArgCheck.isNotNull(targetDirectory);
        ArgCheck.isTrue(targetDirectory.isDirectory(), "Parent directory not directory!");
        ArgCheck.isTrue(targetDirectory.canWrite(), "Parent directory not writeable!");

        cndTargetFile = new File(targetDirectory, TEIID_SQL_CND);
        if (cndTargetFile.exists())
            cndTargetFile.delete();

        cndWriter = new BufferedWriter(new FileWriter(cndTargetFile));
    }

    /**
     * @param name
     * @return
     */
    private String toLowerCamelCase(String name) {
        return name.substring(0, 1).toLowerCase() + name.substring(1);
    }

    private String capitalize(String name) {
        return name.substring(0, 1).toUpperCase() + name.substring(1);
    }

    private void write(String token) throws Exception {
        cndWriter.write(token);
    }

    private void newLine() throws Exception {
        write(NEW_LINE);
    }

    private void writeLicense() throws Exception {
        write(LICENSE);
    }

    private void writeSection1Comment(String comment) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append("//------------------------------------------------------------------------------");
        buf.append(NEW_LINE);
        
        buf.append("// ");
        for (int i = 0; i < comment.length(); ++i) {
            buf.append(comment.charAt(i));
            if (i < comment.length() - 1)
                buf.append(SPACE);
        }
        buf.append(NEW_LINE);

        buf.append("//------------------------------------------------------------------------------");
        buf.append(NEW_LINE);
 
        write(buf.toString());
    }

    private void writeSection2Comment(String comment) throws Exception {
        write("//==================================================");
        newLine();
        write("//  " + comment);
        newLine();
        write("//==================================================");
        newLine();
    }

    private void writeNamespaces() throws Exception {
        writeSection1Comment("NAMESPACES");
        for (String nm : MODESHAPE_NAMESPACES) {
            write(nm);
            newLine();
        }
    }

    private boolean isRootClass(Class<?> objClass) {
        if (objClass == ASTNode.class)
            return true;

        return false;
    }

    private boolean assignable(Class<?> objClass, Class<?> fromClass) {
        return fromClass.isAssignableFrom(objClass);
    }

    private boolean isInterface(Class<?> objClass) {
        return objClass.isInterface();
    }

    private boolean isEnum(Class<?> objClass) {
        return objClass.isEnum();
    }

    private void writeNodeName(Class<?> data) throws Exception {
        write(TSQL_PREFIX);
        write(COLON);
        String name = data.getSimpleName();
        name = toLowerCamelCase(name);
        write(name);
    }

    private Iterator<CTree.Node> createParentIterator(CTree.Node node) {
        List<CTree.Node> parents = new ArrayList<CTree.Node>(node.getParents());

        // Don't need the ASTNode listed in the CND file
        if (parents.contains(node.getTree().getRoot())) {
            parents.remove(node.getTree().getRoot());
        }

        Iterator<CTree.Node> parentIter = parents.iterator();
        return parentIter;
    }

    private void writeInheritsFrom() throws Exception {
        write(TAB);
        write(CLOSE_ANGLE_BRACKET);
        write(TAB);
    }

    private void writeParentList(Iterator<? extends CTree.Node> parentIter) throws Exception {
        while(parentIter.hasNext()) {
            CTree.Node parentNode = parentIter.next();
            writeNodeName(parentNode.klazz());

            if (parentIter.hasNext()) {
                write(COMMA);
                write(SPACE);
            }
        }
    }

    private void writeNodeDeclaration(CTree.Node node) throws Exception {
        write(OPEN_SQUARE_BRACKET);
        writeNodeName(node.klazz());
        write(CLOSE_SQUARE_BRACKET);

        Iterator<CTree.Node> parentIter = createParentIterator(node);
        if (parentIter.hasNext()) {
            // Write out parent node types
            writeInheritsFrom();

            writeParentList(parentIter);
        }
    }

    private void writeMixinDeclaration() throws Exception {
        // All node types are mixin
        write(SPACE + "mixin");
    }

    private void writeAbstractDeclaration(Class<?> klazz) throws Exception {
        if (Modifier.isAbstract(klazz.getModifiers()))
            write(SPACE + "abstract");
    }

    private boolean isSetter(Method method) {
        return method.getName().startsWith(SET);
    }

    private String getSetterName(Method method) {
        String name = method.getName();
        name = name.substring(SET.length());
        name = toLowerCamelCase(name);
        return name;
    }

    private void appendAttributes(StringBuffer buf, List<Attributes> attributes) {
        if (attributes == null || attributes.isEmpty())
            return;

        buf.append(SPACE);

        Iterator<Attributes> iterator = attributes.iterator();
        while(iterator.hasNext()) {
            Attributes attr = iterator.next();
            buf.append(attr.isLowerCase() ? attr.name().toLowerCase() : attr.name());
            if (iterator.hasNext()) {
                buf.append(COMMA);
                buf.append(SPACE);
            }
        }
    }

    private void appendConstraints(StringBuffer buf, List<String> constraints) {
        if (constraints == null || constraints.isEmpty())
            return;

        buf.append(SPACE);
        buf.append(OPEN_ANGLE_BRACKET);
        buf.append(SPACE);

        Iterator<String> iterator = constraints.iterator();
        while(iterator.hasNext()) {
            String constraint = iterator.next();
            buf.append(SPEECH_MARK);
            buf.append(constraint);
            buf.append(SPEECH_MARK);
            if (iterator.hasNext()) {
                buf.append(COMMA);
                buf.append(SPACE);
            }
        }
    }

    private boolean isBasicProperty(Class<?> aspectClass) {
        return PROPERTY_TYPE_MAP.containsKey(aspectClass.getSimpleName().toUpperCase());
    }

    private Class<?> findGenericClass(Class<?> parameterClass, Method method) throws Exception {
        // Get the generic type information for this method parameter
        try {
            Type genericType = null;
            for (Type type : method.getGenericParameterTypes()) {
                if (! (type instanceof ParameterizedType))
                    continue;

                ParameterizedType pType = (ParameterizedType) type;
                if (pType.getRawType().equals(parameterClass)) {
                    genericType = pType.getActualTypeArguments()[0];
                    break;
                }
            }

            if (genericType == null)
                throw new Exception("Failed to find the generic type of the given parameter class " + parameterClass);

            String gTypeName = genericType.toString();
            if (gTypeName.startsWith(CLASS + SPACE))
                gTypeName = gTypeName.substring((CLASS + SPACE).length());
            else if (gTypeName.startsWith(INTERFACE + SPACE))
                gTypeName = gTypeName.substring((INTERFACE + SPACE).length());

            String extendsPrefix = "? extends ";
            if (gTypeName.startsWith(extendsPrefix))
                gTypeName = gTypeName.substring(extendsPrefix.length());

            return Class.forName(gTypeName);
        } catch (ClassCastException e) {
            String msg = "Failed to obtain generic class from parameter " + parameterClass + "!";
            throw new Exception(msg, e);
        }
    }

    private String formProperty(String propName, String propertyType, boolean multiple, List<String> constraints) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(TAB);
        buf.append(MINUS);
        buf.append(SPACE);
        buf.append(TSQL_PREFIX);
        buf.append(COLON);
        buf.append(propName);
        buf.append(SPACE);
        buf.append(OPEN_BRACKET);
        buf.append(propertyType.toLowerCase());
        buf.append(CLOSE_BRACKET);

        List<Attributes> attributes = new ArrayList<Attributes>();
        if (multiple)
            attributes.add(Attributes.MULTIPLE);
            
        appendAttributes(buf, attributes);
        appendConstraints(buf, constraints);

        buf.append(NEW_LINE);
        return buf.toString();
    }

    private String formChildNode(String childName, String childType, boolean multiple) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(TAB);
        buf.append(PLUS);
        buf.append(SPACE);
        buf.append(TSQL_PREFIX);
        buf.append(COLON);
        buf.append(childName);
        buf.append(SPACE);
        buf.append(OPEN_BRACKET);
        buf.append(TSQL_PREFIX);
        buf.append(COLON);
        buf.append(toLowerCamelCase(childType));
        buf.append(CLOSE_BRACKET);

        List<Attributes> attributes = new ArrayList<Attributes>();
        if (multiple)
            attributes.add(Attributes.SNS);

        appendAttributes(buf, attributes);

        buf.append(NEW_LINE);
        return buf.toString();        
    }

    private String formAspect(Class<?> parameterClass, Method method, Node classNode, boolean multiple) throws Exception {
        String aspectName = getSetterName(method);
        String aspectType = parameterClass.getSimpleName();
        boolean isRegistered = classNode.getTree().containsClass(parameterClass);
        boolean isSPIInterface = classNode.getTree().isSPIInterface(parameterClass);

        if (isSPIInterface)
            return null; // Not interested in these methods

        if (isBasicProperty(parameterClass))
            // Basic property like STRING, or DOUBLE
            return formProperty(aspectName, PROPERTY_TYPE_MAP.get(aspectType.toUpperCase()), multiple, null);
        else if (isRegistered)
            // A LanguageObject child
            return formChildNode(aspectName, aspectType, multiple);
        else if (parameterClass == Class.class)
            // A Class property
            return formProperty(aspectName + capitalize(CLASS), STRING, multiple, null);
        else if (parameterClass == Object.class) {
            // Need to handle these setObject methods but the actual values will need to be serialised or something tbd.
            return formProperty(aspectName, BINARY, multiple, null);
        }
        else if (parameterClass.isArray())
            // An Array
            return formAspect(parameterClass.getComponentType(), method, classNode, true);
        else if (parameterClass == Collection.class || parameterClass == List.class) {
            // A Collection or List
            parameterClass = findGenericClass(parameterClass, method);
            return formAspect(parameterClass, method, classNode, true);
        } else if (parameterClass.isEnum()) {
            // An Enum
            List<String> constraints = new ArrayList<String>();
            Object[] enumConstants = parameterClass.getEnumConstants();
            for (Object c : enumConstants) {
                constraints.add(c.toString());
            }
            return formProperty(aspectName, STRING, multiple, constraints);
        }
        else
            System.out.println("The class " + classNode.klazz().getSimpleName() + " has the setter method " + method.getName() + " with a parameter type '" + aspectType + "' is not supported");

        return null;
    }

    private String formAspect(Class<?> parameterClass, Method method, Node classNode) throws Exception {
        return formAspect(parameterClass, method, classNode, false);
    }

    /**
     * Search all the API setter methods for attribute / properties
     *
     * STRING, BINARY, LONG, DOUBLE, DATE, BOOLEAN, NAME,
     * PATH, REFERENCE, WEAKREFERENCE, URI, and DECIMAL
     *
     * @param node
     */
    private void writeAspects(Node node) throws Exception {
        Class<?> nodeClass = node.klazz();
        StringBuffer propBuf = new StringBuffer();
        StringBuffer childBuf = new StringBuffer();

        Map<String, Method> dedupedMethods = new HashMap<String, Method>();
        for (Method method : nodeClass.getDeclaredMethods()) {
            if (! isSetter(method))
                continue;

            Class<?>[] parameterTypes = method.getParameterTypes();
            if (parameterTypes.length > 1)
                throw new Exception("The class " + node.klazz().getSimpleName() + " has the setter method '" + method.getName() + "' with multiple parameters");
            else if (parameterTypes.length == 0)
                throw new Exception("The class " + node.klazz().getSimpleName() + " has the setter method '" + method.getName() + "' with no parameters");

            Method deMethod = dedupedMethods.get(method.getName());
            if (deMethod != null) {
                // Already have a method of this name so need to determine
                // which is narrower as this tends to be produced by generics in
                // the parameters
                Class<?> deParamType = deMethod.getParameterTypes()[0];
                if (deParamType.isAssignableFrom(parameterTypes[0])) {
                    // deMethod is super of method so use method
                    dedupedMethods.put(method.getName(), method);
                }
            } else {
                dedupedMethods.put(method.getName(), method);
            }
        }

        for (Method method : dedupedMethods.values()) {
            Class<?>[] parameterTypes = method.getParameterTypes();
            String aspect = formAspect(parameterTypes[0], method, node);
            if (aspect == null)
                continue;

            if (aspect.startsWith(TAB + MINUS))
                propBuf.append(aspect);
            else
                childBuf.append(aspect);
        }

        write(propBuf.toString());
        write(childBuf.toString());
    }

    private void writeInterfaceNode(CTree.INode iNode) throws Exception {
        writeNodeDeclaration(iNode);

        writeMixinDeclaration();

        newLine();
        newLine();

        // Write the children out
        for (CTree.INode child : iNode.getChildren()) {
            writeInterfaceNode(child);
        }
    }

    private void writeClassNode(CTree.CNode classNode) throws Exception {    
        writeNodeDeclaration(classNode);

        List<INode> classInterfaces = classNode.getClassInterfaces();

        // Class nodes with a parent of the root node will not display the interfaces
        // associated with the root node so find those interfaces and add them in
        // unless an interface has already been added which extends those root interfaces
        CNode rootNode = classNode.getTree().getRoot();
        if (rootNode.equals(classNode.getParent())) {
            List<INode> rootInterfaces = rootNode.getClassInterfaces();
            for (INode rootIface : rootInterfaces) {
                boolean addIt = true;
                if (classInterfaces.contains(rootIface)) {
                    addIt = false;
                    continue; // Already got it!
                }

                for (INode classIface : classInterfaces) {
                    if (rootIface.isAssignable(classIface)) {
                        addIt = false;
                        break;
                    }
                }

                if (addIt)
                    classInterfaces.add(rootIface);
            }
        }
        
        
        if (! classInterfaces.isEmpty()) {
            // Have we already written the inherits from declaration with parents?
            Iterator<Node> parentIter = createParentIterator(classNode);
            if (! parentIter.hasNext()) {
                // No parents written as yet so write the inherits from declaration
                writeInheritsFrom();
            } else {
                write(COMMA + SPACE);
            }

            writeParentList(classInterfaces.iterator());
        }

        writeMixinDeclaration();

        writeAbstractDeclaration(classNode.klazz());

        newLine();

        // Write properties and child fields of this node
        writeAspects(classNode);

        newLine();

        // Write the children out
        for (CTree.CNode child : classNode.getChildren()) {
            writeClassNode(child);
        }
    }

    private void writeClassTree() throws Exception {
        CTree tree = new CTree(ASTNode.class);

        for (Package sqlPkg : SQL_PACKAGES) {
            File pkgDir = new File(SRC_DIR, convertPackageToDirPath(sqlPkg));

            if (! pkgDir.exists())
                throw new RuntimeException("The package directory " + pkgDir + " does not exist!");

            for (String srcFile : pkgDir.list()) {
                String javaClass = srcFile.substring(0, srcFile.indexOf(DOT));
                Class<?> objClass = Class.forName(sqlPkg.getName() + DOT + javaClass);

                if (isEnum(objClass))
                    continue; // Ignore enums

                if (! assignable(objClass, LanguageObject.class))
                    continue; // Must be at least a language object

                if (isRootClass(objClass)) {
                    continue; // Don't need to catalogue this
                }

                if (isInterface(objClass))
                    continue; // Interface taken care of by the class tree

                if (! assignable(objClass, ASTNode.class))
                    continue; // Ignore any framework classes

                tree.add(objClass);
            }
        }

        System.out.println(tree.toString());

        if (! tree.getInterfaceNodes().isEmpty()) {
            writeSection2Comment("Interfaces");
            for (INode iNode : tree.getInterfaceNodes()) {
                writeInterfaceNode(iNode);
            }
        }

        writeSection2Comment("Classes");
        for (CNode cNode : tree.getRoot().getChildren()) {
            writeClassNode(cNode);
        }
    }

    /**
     * 
     */
    public void generate() throws Exception {
        try {
            writeLicense();
            newLine();

            writeNamespaces();
            newLine();

            writeSection1Comment("NODETYPES");
            writeClassTree();
            newLine();

        } finally {
            cndWriter.close();
        }
    }

}
