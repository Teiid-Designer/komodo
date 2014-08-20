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
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.komodo.modeshape.teiid.scripts.CTree.CNode;
import org.komodo.modeshape.teiid.scripts.CTree.CTreeCallback;
import org.komodo.modeshape.teiid.scripts.CTree.INode;
import org.komodo.modeshape.teiid.scripts.CTree.Node;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator;
import org.komodo.modeshape.teiid.sql.lang.LanguageObject;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.symbol.Symbol;
import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.value.Reference;

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
    
    private static final String TSQL_NAMESPACE = "http://www.teiid.org/sql/1.0";

    private static final String TEIID_SQL_CND = "TeiidSql.cnd";

    private static final String TEIID_SQL_LEXICON = "TeiidSqlLexicon";

    private static final String[] MODESHAPE_NAMESPACES = {
        "<jcr='http://www.jcp.org/jcr/1.0'>",
        "<nt='http://www.jcp.org/jcr/nt/1.0'>",
        "<mix='http://www.jcp.org/jcr/mix/1.0'>",
        "<" + TSQL_PREFIX + "='" + TSQL_NAMESPACE + "'>"
    };

    private static final String[] IGNORED_METHOD_NAMES = {
        "getTeiidParser"
    };

    private static final List<String> IGNORED_METHOD_LIST = Arrays.asList(IGNORED_METHOD_NAMES);

    private static final String GET = "get";

    private static final String SET = "set";

    private enum ModeshapeType {
        STRING("STRING", String.class),
        BINARY("BINARY", Object.class),
        LONG("LONG", Long.class),
        DOUBLE("DOUBLE", Double.class),
        DATE("DATE", Date.class),
        BOOLEAN("BOOLEAN", Boolean.class),
        NAME("NAME", String.class),
        PATH("PATH", String.class),
        REFERENCE("REFERENCE", Reference.class),
        WEAKREFERENCE("WEAKREFERENCE", WeakReference.class),
        URI("URI", URI.class),
        DECIMAL("DECIMAL", Float.class),
        INT("LONG", Long.class),
        INTEGER("LONG", Long.class),
        CHARACTER("STRING", String.class),
        CHAR("STRING", String.class);

        private String name;
        private Class<?> type;

        private ModeshapeType(String name, Class<?> type) {
            this.name = name;
            this.type = type;
        }

        public String getName() {
            return this.name;
        }

        public Class<?> getTypeClass() {
            return this.type;
        }

        public static ModeshapeType get(Class<?> klazz) {
            for (ModeshapeType mst : ModeshapeType.values()) {
                if (mst.name().equals(klazz.getSimpleName().toUpperCase()))
                    return mst;
            }
            return null;
        }
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
    
    private abstract class Aspect {

        private String name;
        private boolean multiple;
        private Version sinceVersion;
        private Version removedVersion;

        public Aspect(String name, boolean multiple) {
            this.name = name;
            this.multiple = multiple;
        }

        public String getName() {
            return this.name;
        }

        public boolean isMultiple() {
            return this.multiple;
        }

        public abstract String getTypeName();

        public abstract Class<?> getTypeClass();

        public Version getSinceVersion() {
            return this.sinceVersion;
        }

        public void setSinceVersion(Version sinceVersion) {
            this.sinceVersion = sinceVersion;
        }

        public Version getRemovedVersion() {
            return this.removedVersion;
        }

        public void setRemovedVersion(Version removedVersion) {
            this.removedVersion = removedVersion;
        }
    }

    private class ChildAspect extends Aspect {

        private Class<?> klazz;

        public ChildAspect(String name, Class<?> klazz, boolean multiple) {
            super(name, multiple);
            this.klazz = klazz;
        }

        @Override
        public String getTypeName() {
            return klazz.getSimpleName();
        }

        @Override
        public Class<?> getTypeClass() {
            return klazz;
        }
    }

    private class PropertyAspect extends Aspect {

        private List<String> constraints;
        private ModeshapeType mType;

        public PropertyAspect(String name, ModeshapeType mType, boolean multiple) {
            super(name, multiple);
            this.mType = mType;
        }

        @Override
        public String getTypeName() {
            return mType.getName();
        }

        @Override
        public Class<?> getTypeClass() {
            return mType.getTypeClass();
        }

        public List<String> getConstraints() {
            return this.constraints;
        }

        public void setConstraints(List<String> constraints) {
            this.constraints = new ArrayList<String>();
            this.constraints.addAll(constraints);
        }
        
    }

    private final BufferedWriter cndWriter;

    private final BufferedWriter lexiconWriter;

    /**
     * @param targetDirectory
     */
    public TeiidCndGenerator(File targetDirectory) throws Exception {
        ArgCheck.isNotNull(targetDirectory);
        ArgCheck.isTrue(targetDirectory.isDirectory(), "Parent directory not directory!");
        ArgCheck.isTrue(targetDirectory.canWrite(), "Parent directory not writeable!");

        File cndTargetFile = new File(targetDirectory, TEIID_SQL_CND);
        if (cndTargetFile.exists())
            cndTargetFile.delete();

        cndWriter = new BufferedWriter(new FileWriter(cndTargetFile));

        File lexiconTargetFile = new File(targetDirectory, TEIID_SQL_LEXICON + DOT + JAVA);
        if (lexiconTargetFile.exists())
            lexiconTargetFile.delete();

        lexiconWriter = new BufferedWriter(new FileWriter(lexiconTargetFile));
    }

    private String toLowerCamelCase(String name) {
        return name.substring(0, 1).toLowerCase() + name.substring(1);
    }

    private String camelCaseToUnderscores(String name) {
        StringBuffer buf = new StringBuffer();

        for (int i = 0; i < name.length(); ++i) {
            Character c = name.charAt(i);

            if (i > 0 && Character.isUpperCase(c)) {
                Character c1 = null;

                if ((i + 1) < name.length()) {
                    c1 = name.charAt(i + 1);
                }

                if (c1 != null && ! Character.isUpperCase(c1))
                    buf.append(UNDERSCORE);
            }

            buf.append(Character.toUpperCase(c));
        }

        return buf.toString();
    }

    private String capitalize(String name) {
        return name.substring(0, 1).toUpperCase() + name.substring(1);
    }

    private void cnd(String token) throws Exception {
        cndWriter.write(token);
    }

    private void lex(String token) throws Exception {
        lexiconWriter.write(token);
    }

    private void lexPackage() throws Exception {
        File genDir = new File(GENERATOR_HOME_SRC_DIR);
        File cndDir = new File(genDir.getParentFile(), "cnd");
        String lexPackage = cndDir.getAbsolutePath();
        lexPackage = lexPackage.substring(lexPackage.indexOf(SRC_DIR) + SRC_DIR.length() + 1);
        lexPackage = lexPackage.replaceAll(File.separator, DOT);

        lex("package " + lexPackage + SEMI_COLON);
        lex(NEW_LINE);
        lex(NEW_LINE);

        lex("import " + Field.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        lex("import " + HashMap.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        lex("import " + Map.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        lex("import " + ASTNode.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        lex("import " + StringConstants.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        lex("import " + Version.class.getCanonicalName() + SEMI_COLON + NEW_LINE);
        for (Package sqlPkg : SQL_PACKAGES) {
            lex("import " + sqlPkg.getName() + DOT + STAR + SEMI_COLON + NEW_LINE);
        }
        lex(NEW_LINE);
        lex(NEW_LINE);
    }

    private void lexClassDeclaration() throws Exception {
        lex("@SuppressWarnings( { \"javadoc\", \"nls\" })" + NEW_LINE);
        lex(PUBLIC + SPACE + CLASS + SPACE + TEIID_SQL_LEXICON + " implements StringConstants" + SPACE + OPEN_BRACE);
        lex(NEW_LINE);
        lex(NEW_LINE);
    }

    private void lexIndexClasses(CTree tree) throws Exception {
        lex(NEW_LINE);
        lex(TAB + PRIVATE + SPACE + STATIC + " Map<String, Class<?>> astIndex = new HashMap<String, Class<?>>();" + NEW_LINE);
        lex(NEW_LINE);

        lex(TAB + STATIC + SPACE + OPEN_BRACE + NEW_LINE);

        CTreeCallback callback = new CTreeCallback() {
            @Override
            public void run(Node node) throws Exception {
                String name = node.klazz().getSimpleName();
                lex(TAB + TAB + "astIndex.put(" + name + ".class.getSimpleName()" + COMMA + SPACE + name + ".class);" + NEW_LINE);
            }
        };

        tree.execute(callback);

        lex(TAB + CLOSE_BRACE + NEW_LINE);
        lex(NEW_LINE);

        lex(TAB + PUBLIC + SPACE + STATIC + " String getTypeId(Class<? extends ASTNode> astNodeClass) {" + NEW_LINE);
        lex(TAB + TAB + "try {" + NEW_LINE);
        lex(TAB + TAB + TAB + "Class<?> astClass = astIndex.get(astNodeClass.getSimpleName());" + NEW_LINE);
        lex(TAB + TAB + TAB + "Field idField = astClass.getField(\"ID\");" + NEW_LINE);
        lex(TAB + TAB + TAB + "Object idValue = idField.get(null);" + NEW_LINE);
        lex(TAB + TAB + TAB + "return idValue.toString();" + NEW_LINE);
        lex(TAB + TAB + "} catch (Exception ex) {" + NEW_LINE);
        lex(TAB + TAB + TAB + "throw new RuntimeException(ex);" + NEW_LINE);
        lex(TAB + TAB + CLOSE_BRACE + NEW_LINE);
        lex(TAB + CLOSE_BRACE + NEW_LINE);
    }

private void cndSection1Comment(String comment) throws Exception {
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
 
        cnd(buf.toString());
    }

    private void cndSection2Comment(String comment) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append("//==================================================");
        buf.append(NEW_LINE);
        buf.append("//  " + comment);
        buf.append(NEW_LINE);
        buf.append("//==================================================");
        buf.append(NEW_LINE);

        cnd(buf.toString());
    }

    private void writeNamespaces() throws Exception {
        cndSection1Comment("NAMESPACES");
        for (String nm : MODESHAPE_NAMESPACES) {
            cnd(nm);
            cnd(NEW_LINE);
        }
        cnd(NEW_LINE);

        StringBuffer buf = new StringBuffer();
        buf.append(TAB).append(INTERFACE).append(" Namespace ").append(OPEN_BRACE).append(NEW_LINE);

        buf.append(TAB).append(TAB).append(PUBLIC).append(SPACE).append(STATIC).append(SPACE).append(FINAL)
             .append(SPACE).append("String PREFIX = ").append(SPEECH_MARK).append(TSQL_PREFIX)
             .append(SPEECH_MARK).append(SEMI_COLON).append(NEW_LINE);

        buf.append(TAB).append(TAB).append(PUBLIC).append(SPACE).append(STATIC).append(SPACE).append(FINAL)
             .append(SPACE).append("String URI = ").append(SPEECH_MARK).append(TSQL_NAMESPACE)
             .append(SPEECH_MARK).append(SEMI_COLON).append(NEW_LINE);
             
        buf.append(TAB).append(CLOSE_BRACE).append(NEW_LINE).append(NEW_LINE);

        lex(buf.toString());
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

    private String nodeName(Class<?> data) throws Exception {
        StringBuffer buf = new StringBuffer();
        String name = data.getSimpleName();
        name = toLowerCamelCase(name);

        buf.append(TSQL_PREFIX).append(COLON).append(name);
        return buf.toString();
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
        cnd(TAB);
        cnd(CLOSE_ANGLE_BRACKET);
        cnd(TAB);

        lex(" extends ");
    }

    private void writeParentList(Iterator<? extends CTree.Node> parentIter) throws Exception {
        while(parentIter.hasNext()) {
            CTree.Node parentNode = parentIter.next();
            cnd(nodeName(parentNode.klazz()));
            lex(parentNode.klazz().getSimpleName());

            if (parentIter.hasNext()) {
                cnd(COMMA);
                cnd(SPACE);
                lex(COMMA);
                lex(SPACE);
            }
        }
    }

    private void writeNodeDeclaration(CTree.Node node) throws Exception {
        cnd(OPEN_SQUARE_BRACKET);
        cnd(nodeName(node.klazz()));
        cnd(CLOSE_SQUARE_BRACKET);

        lex(TAB + "/**" + NEW_LINE);
        lex(TAB + " * " + nodeName(node.klazz()) + NEW_LINE);
        lex(TAB + " */" + NEW_LINE);
        lex(TAB + PUBLIC + SPACE + INTERFACE + SPACE + node.klazz().getSimpleName());
        
        Iterator<CTree.Node> parentIter = createParentIterator(node);
        if (parentIter.hasNext()) {
            // Write out parent node types
            writeInheritsFrom();

            writeParentList(parentIter);
        }
    }

    private void writeMixinDeclaration() throws Exception {
        // All node types are mixin
        cnd(SPACE + "mixin");
    }

    private void writeAbstractDeclaration(Class<?> klazz) throws Exception {
        boolean isAbstract = false;

        if (Modifier.isAbstract(klazz.getModifiers())) {
            cnd(SPACE + "abstract");
            isAbstract = true;
        }

        lex(NEW_LINE);
        lex(TAB + TAB + "boolean IS_ABSTRACT = " + Boolean.toString(isAbstract) + SEMI_COLON + NEW_LINE);
        lex(NEW_LINE);
    }

    private boolean isGetter(Method method) {
        return method.getName().startsWith(GET);
    }

    private boolean isSetter(Method method) {
        return method.getName().startsWith(SET);
    }

    private String extractRootName(Method method) {
        String name = method.getName();
        name = name.substring(SET.length()); // Set and Get are the same length so this works for either!
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

    private void lexTeiidVersions(Aspect aspect, String varName) throws Exception {
        StringBuffer buf = new StringBuffer();

        if (aspect.getSinceVersion() != null) {
            buf.append(TAB).append(TAB)
                        .append("Version ").append(varName).append("_SINCE_VERSION").append(" = ")
                        .append("Version").append(DOT).append(aspect.getSinceVersion().name())
                        .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);
        }

        if (aspect.getRemovedVersion() != null) {
            buf.append(TAB).append(TAB)
                        .append("Version ").append(varName).append("_REMOVED_VERSION").append(" = ")
                        .append("Version").append(DOT).append(aspect.getRemovedVersion().name())
                        .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);
        }

        lex(buf.toString());
    }

    private void writeProperty(PropertyAspect aspect) throws Exception {
        String prefixName = TSQL_PREFIX + COLON + aspect.getName();
        
        StringBuffer cndBuf = new StringBuffer();
        cndBuf.append(TAB);
        cndBuf.append(MINUS);
        cndBuf.append(SPACE);
        cndBuf.append(prefixName);
        cndBuf.append(SPACE);
        cndBuf.append(OPEN_BRACKET);
        cndBuf.append(aspect.getTypeName().toLowerCase());
        cndBuf.append(CLOSE_BRACKET);

        List<Attributes> attributes = new ArrayList<Attributes>();
        if (aspect.isMultiple())
            attributes.add(Attributes.MULTIPLE);
            
        appendAttributes(cndBuf, attributes);

        if (aspect.getConstraints() != null && ! aspect.getConstraints().isEmpty()) {
            cndBuf.append(SPACE);
            cndBuf.append(OPEN_ANGLE_BRACKET);
            cndBuf.append(SPACE);
            appendConstraints(cndBuf, aspect.getConstraints());
        }

        cndBuf.append(NEW_LINE);
        cnd(cndBuf.toString());

        String varName = camelCaseToUnderscores(aspect.getName()).toUpperCase();
        StringBuffer lexBuf = new StringBuffer();

        lexBuf.append(TAB + TAB + "/**" + NEW_LINE)
                 .append(TAB + TAB + " * " + varName + " Property" + NEW_LINE)
                 .append(TAB + TAB + " */" + NEW_LINE);
        
        lexBuf.append(TAB).append(TAB)
                 .append("String ").append(varName).append("_PROP_NAME")
                 .append(" = Namespace.PREFIX ").append(PLUS).append(SPACE).append("COLON").append(SPACE)
                 .append(PLUS).append(SPACE).append(SPEECH_MARK).append(aspect.getName()).append(SPEECH_MARK)
                 .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);
                 
        lexBuf.append(TAB).append(TAB)
                  .append("Class<?> ").append(varName).append("_PROP_TYPE").append(" = ")
                  .append(SPACE).append(aspect.getTypeClass().getSimpleName()).append(DOT).append(CLASS)
                  .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);

        lexBuf.append(TAB).append(TAB)
                  .append("boolean ").append(varName).append("_PROP_MULTIPLE").append(" = ")
                  .append(Boolean.toString(aspect.isMultiple())).append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);

        if (aspect.getConstraints() != null && ! aspect.getConstraints().isEmpty()) {
            lexBuf.append(TAB).append(TAB)
                      .append("String[] ").append(varName).append("_PROP_CONSTRAINTS").append(" = ")
                      .append(OPEN_BRACE).append(SPACE);

            appendConstraints(lexBuf, aspect.getConstraints());
                      
            lexBuf.append(SPACE).append(CLOSE_BRACE).append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);
        }

        lex(lexBuf.toString());

        lexTeiidVersions(aspect, varName + "_PROP");
    }

    private void writeChildNode(ChildAspect aspect) throws Exception {
        StringBuffer cndBuf = new StringBuffer();
        cndBuf.append(TAB);
        cndBuf.append(PLUS);
        cndBuf.append(SPACE);
        cndBuf.append(TSQL_PREFIX);
        cndBuf.append(COLON);
        cndBuf.append(aspect.getName());
        cndBuf.append(SPACE);
        cndBuf.append(OPEN_BRACKET);
        cndBuf.append(TSQL_PREFIX);
        cndBuf.append(COLON);
        cndBuf.append(toLowerCamelCase(aspect.getTypeName()));
        cndBuf.append(CLOSE_BRACKET);

        List<Attributes> attributes = new ArrayList<Attributes>();
        if (aspect.isMultiple())
            attributes.add(Attributes.SNS);

        appendAttributes(cndBuf, attributes);

        cndBuf.append(NEW_LINE);
        cnd(cndBuf.toString());

        String varName = camelCaseToUnderscores(aspect.getName()).toUpperCase();
        StringBuffer lexBuf = new StringBuffer();

        lexBuf.append(TAB + TAB + "/**" + NEW_LINE)
                 .append(TAB + TAB + " * " + varName + " Reference" + NEW_LINE)
                 .append(TAB + TAB + " */" + NEW_LINE);

        lexBuf.append(TAB).append(TAB)
                 .append("String ").append(varName).append("_REF_NAME")
                 .append(" = Namespace.PREFIX ").append(PLUS).append(SPACE).append("COLON").append(SPACE)
                 .append(PLUS).append(SPACE).append(SPEECH_MARK).append(aspect.getName()).append(SPEECH_MARK)
                 .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);
        
        lexBuf.append(TAB).append(TAB)
                 .append("Class<?> ").append(varName).append("_REF_TYPE").append(" = ")
                 .append(SPACE).append(aspect.getTypeClass().getSimpleName()).append(DOT).append(CLASS)
                 .append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);

        lexBuf.append(TAB).append(TAB)
                 .append("boolean ").append(varName).append("_REF_MULTIPLE").append(" = ")
                 .append(Boolean.toString(aspect.isMultiple())).append(SEMI_COLON).append(NEW_LINE).append(NEW_LINE);

        lex(lexBuf.toString());

        lexTeiidVersions(aspect, varName + "_REF");
    }

    private Aspect createAspect(Class<?> parameterClass, Method method, Node classNode, boolean multiple) throws Exception {
        String aspectName = extractRootName(method);
        String aspectType = parameterClass.getSimpleName();
        ModeshapeType mType = ModeshapeType.get(parameterClass);
        boolean isRegistered = classNode.getTree().containsClass(parameterClass);
        boolean isSPIInterface = classNode.getTree().isSPILanguageInterface(parameterClass);

        if (isSPIInterface)
            return null; // Not interested in these methods

        Aspect aspect = null;
        if (mType != null)
            // Basic property like STRING, or DOUBLE
            aspect = new PropertyAspect(aspectName, mType, multiple);
        else if (isRegistered)
            // A LanguageObject child
            aspect = new ChildAspect(aspectName, parameterClass, multiple);
        else if (parameterClass == Class.class)
            // A Class property
            aspect = new PropertyAspect(aspectName + capitalize(CLASS), ModeshapeType.STRING, multiple);
        else if (parameterClass == Object.class) {
            // Need to handle these setObject methods but the actual values will need to be serialised or something tbd.
            aspect = new PropertyAspect(aspectName, ModeshapeType.BINARY, multiple);
        }
        else if (parameterClass.isArray())
            // An Array
            aspect = createAspect(parameterClass.getComponentType(), method, classNode, true);
        else if (parameterClass == Collection.class || parameterClass == List.class) {
            // A Collection or List
            parameterClass = findGenericClass(parameterClass, method);
            aspect = createAspect(parameterClass, method, classNode, true);
        } else if (parameterClass.isEnum()) {
            // An Enum
            List<String> constraints = new ArrayList<String>();

            // CriteriaOperator.Operator enum is exceptional in that its toString()
            // does not include the extra != from the NotEquals and since this is a
            // constraint we don't really want to NOT include it!!
            if (parameterClass.equals(CriteriaOperator.Operator.class)) {
                for (CriteriaOperator.Operator op : CriteriaOperator.Operator.values()) {
                    for (String symbol : op.getSymbols()) {
                        constraints.add(symbol);
                    }
                }
            } else {
                Object[] enumConstants = parameterClass.getEnumConstants();
                if (enumConstants != null) {
                    for (Object c : enumConstants) {
                        constraints.add(c.toString());
                    }
                }
            }

            PropertyAspect propAspect = new PropertyAspect(aspectName, ModeshapeType.STRING, multiple);
            propAspect.setConstraints(constraints);
            aspect = propAspect;
        }
        else
            System.out.println("The class " + classNode.klazz().getSimpleName() + " has the setter method " + method.getName() + " with a parameter type '" + aspectType + "' is not supported");

        if (aspect != null) {
            aspect.setSinceVersion(AnnotationUtils.getSinceVersion(method, Since.class));
            aspect.setRemovedVersion(AnnotationUtils.getRemovedVersion(method, Removed.class));
        }

        return aspect;
    }

    private Aspect createAspect(Class<?> parameterClass, Method method, Node classNode) throws Exception {
        return createAspect(parameterClass, method, classNode, false);
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
        List<PropertyAspect> propertyAspects = new ArrayList<PropertyAspect>();
        List<ChildAspect> childAspects = new ArrayList<ChildAspect>();

        Map<String, Method> dedupedMethods = new HashMap<String, Method>();
        for (Method method : nodeClass.getDeclaredMethods()) {

            if (nodeClass.isInterface() && ! isGetter(method))
                continue; // extract getter from interfaces
            else if (! nodeClass.isInterface() && ! isSetter(method))
                continue; // extract setters from classes

            Method overriddenMethod = findOverriddenMethod(node, method);
            // overriddenMethod should be highest possible declaration
            if (overriddenMethod != null)
                continue; // No need to create an aspect for this method as we should use the parent declaration

            if (IGNORED_METHOD_LIST.contains(method.getName()))
                continue;

            if (isGetter(method)) {
                dedupedMethods.put(method.getName(), method);
            } else if (isSetter(method)) {
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
                    ModeshapeType deMType = ModeshapeType.get(deParamType);
                    ModeshapeType mType = ModeshapeType.get(parameterTypes[0]);

                    if (deParamType.isAssignableFrom(parameterTypes[0])) {
                        // deMethod is super of method so use method
                        dedupedMethods.put(method.getName(), method);
                    } else if (parameterTypes[0].isAssignableFrom(deParamType)) {
                        // method is super of deMethod so already using deMethod
                        continue;
                    } else if (mType != null && deMType == null) {
                        // method has a modeshape type parameter, prefer that
                        dedupedMethods.put(method.getName(), method);
                    } else if (deMType != null && mType == null) {
                        // deMethod has a modeshape type parameter, prefer that
                        continue;
                    } else {
                        // Methods have same name but totally unrelated so flag this up
                        System.err.println(nodeClass + " contains more than one " + method.getName() + ". Not sure which to use!");
                    }
                } else {
                    dedupedMethods.put(method.getName(), method);
                }
            }
        }

        for (Method method : dedupedMethods.values()) {
            Class<?> parameterClass = null;
            if (isGetter(method)) {
                parameterClass = method.getReturnType();
            } else {
                Class<?>[] parameterTypes = method.getParameterTypes();
                parameterClass  = parameterTypes[0];
            }

            Aspect aspect = createAspect(parameterClass, method, node);
            
            if (aspect instanceof PropertyAspect)
                propertyAspects.add((PropertyAspect) aspect);
            else if (aspect instanceof ChildAspect)
                childAspects.add((ChildAspect) aspect);
        }

        for (PropertyAspect aspect : propertyAspects) {
            writeProperty(aspect);
        }

        for (ChildAspect aspect : childAspects) {
            writeChildNode(aspect);
        }
    }

    private Method findOverriddenMethod(Node node, Method method) throws Exception {
        Class<?> klazz = node.klazz();
        String rootMethodName = extractRootName(method);

        if (ASTNode.class.equals(klazz))
            return null; // Try and short-circuit for performance

        Method overriddenMethod = null;

        Class<?> superClass = klazz.getSuperclass();
        while (superClass != null) {
            for (Method dmethod : superClass.getDeclaredMethods()) {
                if (dmethod.getName().equals(rootMethodName)) {
                    overriddenMethod = dmethod;
                    break;
                }
            }

            superClass = superClass.getSuperclass();
        }

        if (overriddenMethod != null)
            return overriddenMethod;

        Class<?>[] interfaces = klazz.getInterfaces();
        if (interfaces == null)
            return overriddenMethod;

        for (Class<?> iface : interfaces) {
            if (node.getTree().isSPILanguageInterface(iface))
                continue;

            for (Method dmethod : iface.getDeclaredMethods()) {
                // Need to compare setters with getters for the interfaces
                if (extractRootName(dmethod).equals(rootMethodName)) {
                    overriddenMethod = dmethod;
                    break;
                }
            }
        }

        return overriddenMethod;
    }

    private void lexId(Node node) throws Exception {
        
        String name = node.klazz().getSimpleName();
        name = toLowerCamelCase(name);
        
        lex(NEW_LINE);
        
        StringBuffer buf = new StringBuffer();
        buf.append(TAB).append(TAB)
            .append("String ID = Namespace.PREFIX ").append(PLUS).append(SPACE).append("COLON ").append(PLUS).append(SPACE)
            .append(SPEECH_MARK).append(name).append(SPEECH_MARK).append(SEMI_COLON);
        lex(buf.toString());
        lex(NEW_LINE);
    }

    private void writeInterfaceNode(CTree.INode iNode) throws Exception {
        writeNodeDeclaration(iNode);

        writeMixinDeclaration();

        cnd(NEW_LINE);

        lex(SPACE + OPEN_BRACE + NEW_LINE);
        lexId(iNode);
        lex(NEW_LINE);

        // Write properties and child fields of this node
        writeAspects(iNode);

        cnd(NEW_LINE);
        lex(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

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
                cnd(COMMA + SPACE);
                lex(COMMA + SPACE);
            }

            writeParentList(classInterfaces.iterator());
        }

        writeMixinDeclaration();

        // Open brace of the interface
        lex(SPACE + OPEN_BRACE + NEW_LINE);

        lexId(classNode);

        writeAbstractDeclaration(classNode.klazz());

        cnd(NEW_LINE);

        // Write properties and child fields of this node
        writeAspects(classNode);

        cnd(NEW_LINE);
        lex(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

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

            List<String> classList = Arrays.asList(pkgDir.list());
            Collections.sort(classList);

            for (String srcFile : classList) {
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
            cndSection2Comment("Derived from Language Object Interfaces");
            for (INode iNode : tree.getInterfaceNodes()) {
                writeInterfaceNode(iNode);
            }
        }

        cndSection2Comment("Derived from Language Object Classes");
        for (CNode cNode : tree.getRoot().getChildren()) {
            writeClassNode(cNode);
        }

        lexIndexClasses(tree);
    }

    /**
     * 
     */
    public void generate() throws Exception {
        try {
            cnd(LICENSE);
            cnd(NEW_LINE);

            lex(LICENSE);
            lex(NEW_LINE);

            lexPackage();
            lexClassDeclaration();

            writeNamespaces();

            cndSection1Comment("NODETYPES");
            writeClassTree();
            cnd(NEW_LINE);

            lex(CLOSE_BRACE);

        } finally {
            cndWriter.close();
            lexiconWriter.close();
        }
    }

}
