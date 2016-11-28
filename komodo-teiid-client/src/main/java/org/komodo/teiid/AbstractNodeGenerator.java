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
package org.komodo.teiid;

import static org.modeshape.jcr.api.JcrConstants.NT_UNSTRUCTURED;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.StatementType;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.spi.type.DataTypeManager.DataTypeName;
import org.modeshape.jcr.api.Session;

public abstract class AbstractNodeGenerator<T> implements StringConstants {

    /**
     * @param value the value being lower camel-cased
     * @return camelCase version of the value but also ensures that values such as
     *                  SPParameter become spParameter
     */
    protected static String toLowerCamelCase(final String value) {
        if (value == null)
            return null;

        char c[] = value.toCharArray();
        if (c.length == 0)
            return null;

        for (int i = 0; i < c.length; ++i) {
            Character curr = c[i];
            Character next = null;

            if ((i + 1) < c.length)
                next = c[i + 1];

            if (next == null)
                break;

            if (Character.isUpperCase(curr) && Character.isUpperCase(next))
                c[i] = Character.toLowerCase(curr);

            if (i == 0)
                c[i] = Character.toLowerCase(curr);

            if (!Character.isUpperCase(next))
                break;
        }

        return new String(c);
    }

    protected class IndexKey {

        private String parentPath;

        private T obj;

        public IndexKey(T obj, Node parent) throws RepositoryException {
            this.obj = obj;
            this.parentPath = parent == null ? FORWARD_SLASH : parent.getPath();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((obj == null) ? 0 : obj.hashCode());
            result = prime * result + ((parentPath == null) ? 0 : parentPath.hashCode());
            return result;
        }

        @SuppressWarnings( "unchecked" )
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            IndexKey other = (IndexKey)obj;
            if (this.obj == null) {
                if (other.obj != null)
                    return false;
            } else if (!this.obj.equals(other.obj))
                return false;
            if (parentPath == null) {
                if (other.parentPath != null)
                    return false;
            } else if (!parentPath.equals(other.parentPath))
                return false;
            return true;
        }
    }

    /**
     * @param obj
     * @param node
     * @return a new IndexKey
     * @throws RepositoryException
     */
    protected IndexKey createKey(T obj, Node node) throws RepositoryException {
        return new IndexKey(obj, node);
    }

    protected class Context {

        private Node oldParent;

        private String oldReference;

        public Context(Node newParent, String newReference) {
            switchParent(newParent);
            switchReference(newReference);
        }

        private void switchParent(Node newParent) {
            oldParent = getParentNode();
            setParentNode(newParent);
        }

        private void switchReference(String newReference) {
            oldReference = getReference();
            setReference(newReference);
        }

        public void reset() {
            setParentNode(oldParent);
            setReference(oldReference);
        }
    }

    protected Context localContext(Node parent, String reference) {
        return new Context(parent, reference);
    }

    /*
     * Makes the generic class available to test against at runtime
     */
    private final Class<T> subjectClass;

    private final Node rootNode;

    private final DataTypeManager dataTypeManager;

    private final TeiidVersion version;

    private Map<IndexKey, Node> objectIndex;

    private Exception error = null;

    //==========================
    //
    // Context fields
    //
    //==========================
    // The parent node which the converted visited object will be added
    private Node parentNode;
    // The reference name under which the node will be added
    private String reference;

    public AbstractNodeGenerator(Class<T> subjectClass, Node parentNode, DataTypeManager dataTypeManager, TeiidVersion version) {
        this.subjectClass = subjectClass;
        this.rootNode = parentNode;
        this.dataTypeManager = dataTypeManager;
        this.version = version;
        this.parentNode = this.rootNode;
    }

    public TeiidVersion getVersion() {
        return version;
    }

    public DataTypeManager getDataTypeManager() {
        return dataTypeManager;
    }

    public boolean errorOccurred() {
        return this.error != null;
    }

    public Exception getError() {
        return this.error;
    }

    public void setError(Exception error) {
        this.error = error;
    }

    public Node getParentNode() {
        return parentNode;
    }

    public void setParentNode(Node parentNode) {
        this.parentNode = parentNode;
    }

    public String getReference() {
        return reference;
    }

    public void setReference(String reference) {
        this.reference = reference;
    }

    protected String resolveCurrentPath(String jcrName) throws Exception {
        final Session session = (Session)rootNode.getSession();
        // if first character is a '{' then the name is prefixed by the namespace URL
        if ((jcrName.charAt(0) == OPEN_BRACE.charAt(0)) && (jcrName.indexOf(CLOSE_BRACE.charAt(0)) != -1)) {
            final int index = jcrName.indexOf(CLOSE_BRACE.charAt(0));
            String localName = jcrName.substring(index + 1);
            localName = session.encode(localName);

            jcrName = jcrName.substring(0, (index + 1)) + localName;
        } else {
            jcrName = session.encode(jcrName);
        }

        return jcrName;
    }

    protected void index(T obj, Node node) throws RepositoryException {
        if (objectIndex == null)
            objectIndex = new HashMap<>();

        IndexKey key = createKey(obj, getParentNode());
        objectIndex.put(key, node);
    }

    protected Node node(T obj) throws RepositoryException {
        if (objectIndex == null)
            return null;

        IndexKey key = createKey(obj, getParentNode());
        return objectIndex.get(key);
    }

    protected Node create(Object obj) throws Exception {
        String jcrName = TeiidSqlLexicon.Namespace.PREFIX + COLON + toLowerCamelCase(obj.getClass().getSimpleName());
        String pathComponent = this.reference;
        if (this.reference == null)
            pathComponent = jcrName;

        Node node = null;
        String relativePath = resolveCurrentPath(pathComponent);

        node = parentNode.addNode(relativePath, NT_UNSTRUCTURED);

        node.addMixin(jcrName);

        // Add the teiid version to the sequence node
        node.setProperty(TeiidSqlLexicon.LanguageObject.TEIID_VERSION_PROP_NAME, getVersion().toString());
        return node;
    }

    protected Node createTreeObject(T obj) throws Exception {
        Node node = node(obj);
        if (node != null)
            return node;

        node = create((Object)obj);

        // Index the node against the object so can be found again
        index(obj, node);
        return node;
    }

    protected Node transform(T obj) throws Exception {
        Node node = node(obj);
        if (node != null)
            return node;

        node = create((Object)obj);

        // Index the node against the object so can be found again
        index(obj, node);
        return node;
    }

    protected List<Value> convertToPropertyValues(Object objectValue, ValueFactory valueFactory) throws RepositoryException {
        List<Value> result = new ArrayList<Value>();
        if (objectValue instanceof Collection) {
            Collection<?> objects = (Collection<?>)objectValue;
            for (Object childObjectValue : objects) {
                List<Value> childValues = convertToPropertyValues(childObjectValue, valueFactory);
                result.addAll(childValues);
            }
        } else if (objectValue instanceof Boolean) {
            result.add(valueFactory.createValue((Boolean)objectValue));
        } else if (objectValue instanceof Integer) {
            result.add(valueFactory.createValue((Integer)objectValue));
        } else if (objectValue instanceof Long) {
            result.add(valueFactory.createValue((Long)objectValue));
        } else if (objectValue instanceof Double) {
            result.add(valueFactory.createValue((Double)objectValue));
        } else if (objectValue instanceof Float) {
            result.add(valueFactory.createValue((Float)objectValue));
        } else if (subjectClass.isInstance(objectValue)) {
            result.add(valueFactory.createValue(node(subjectClass.cast(objectValue))));
        } else {
            result.add(valueFactory.createValue(objectValue.toString()));
        }
        return result;
    }

    protected void setProperty(Node node, String name, Object value) throws Exception {
        ValueFactory valueFactory = node.getSession().getValueFactory();

        if (value == null) {
            node.setProperty(name, (Value)null);
            return;
        }

        List<Value> valuesList = convertToPropertyValues(value, valueFactory);
        if (valuesList.size() == 1) {
            node.setProperty(name, valuesList.get(0));
        } else {
            node.setProperty(name, valuesList.toArray(new Value[0]));
        }
    }

    protected void setDataTypeProperty(Node node, String reference, Class<?> typeClass) throws Exception {
        DataTypeName dataTypeName = getDataTypeManager().retrieveDataTypeName(typeClass);
        setProperty(node, reference, dataTypeName.name());
    }

    protected void setStatementTypeProperty(Node node, int type) throws Exception {
        StatementType statementType = StatementType.findStatementType(type);
        if (statementType == null)
            return;

        setProperty(node, TeiidSqlLexicon.Statement.TYPE_PROP_NAME, statementType.name());
    }

    protected void visitObject(Node node, String reference, T obj) {
        Context context = localContext(node, reference);
        visitObject(obj);
        context.reset();
    }

    protected void visitObjects(Node node, String reference, T[] objs) {
        if (objs == null || objs.length == 0)
            return;

        Context context = localContext(node, reference);

        for (T obj : objs)
            visitObject(obj);

        context.reset();
    }

    protected void visitObjects(Node node, String reference, Collection<? extends T> objs) {
        if (objs == null || objs.size() == 0)
            return;

        Context context = localContext(node, reference);

        for (T obj : objs)
            visitObject(obj);

        context.reset();
    }

    /**
     * @param obj
     */
    public abstract void visitObject(T obj);
}
