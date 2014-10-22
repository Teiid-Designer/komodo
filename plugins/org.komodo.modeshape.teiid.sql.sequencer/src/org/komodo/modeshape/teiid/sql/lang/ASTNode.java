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
package org.komodo.modeshape.teiid.sql.lang;

import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;
import static org.modeshape.jcr.api.JcrConstants.NT_UNSTRUCTURED;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.type.IDataTypeManagerService;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;
import org.komodo.utils.ArgCheck;
import org.modeshape.common.util.CheckArg;
import org.modeshape.jcr.api.JcrConstants;

/**
 * Utility object class designed to facilitate constructing an AST or Abstract Syntax Tree representing nodes and properties that
 * are compatible with ModeShape graph component structure.
 */
public abstract class ASTNode extends SimpleNode implements LanguageObject, StringConstants, Cloneable {

    /**
     * 
     */
    public static final String MIXIN = "mixin"; //$NON-NLS-1$

    /**
     * 
     */
    public static final String IDENTIFIER = "identifier"; //$NON-NLS-1$

    /**
     * 
     */
    public static final String VALUE = "value"; //$NON-NLS-1$

    private class ChildNodeIterator implements Iterator<ASTNode> {

        private Iterator<Object> childrenIterator;

        private Iterator<ASTNode> currentIterator;

        public ChildNodeIterator() {
            this.childrenIterator = children.values().iterator();
        }

        @Override
        public boolean hasNext() {
            return currentIterator != null ? currentIterator.hasNext() : childrenIterator.hasNext();
        }

        @Override
        public ASTNode next() {
            if (currentIterator != null && currentIterator.hasNext()) {
                ASTNode next = currentIterator.next();
                if (! currentIterator.hasNext())
                    currentIterator = null;

                return next;
            }

            Object nextObject = childrenIterator.next();
            if (nextObject instanceof ASTNode) {
                currentIterator = null;
                return (ASTNode) nextObject;
            }
            else if (nextObject instanceof List) {
                currentIterator = ((List)nextObject).iterator();
                return currentIterator.next();
            }

            throw new NoSuchElementException();
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private ASTNode parent;
    private String astIdentifier;

    private final Map<String, Object> properties = new HashMap<String, Object>();
    private final Map<String, Object> children = new HashMap<String, Object>();

    // Cached by getAbsolutePath() - do not use directly
    private String absolutePath = null;

    private javax.jcr.Node sequencedNode;

    /**
     * @param parser teiid parser
     * @param nodeTypeIndex node type id
     */
    public ASTNode(ITeiidParser parser, int nodeTypeIndex) {
        super(parser, nodeTypeIndex);

        String lexiconType = TeiidSqlLexicon.getTypeId(getClass());
        ArgCheck.isNotNull(lexiconType);
        setProperty(JCR_MIXIN_TYPES, lexiconType);
        setProperty(JCR_PRIMARY_TYPE, NT_UNSTRUCTURED);
        setAstIdentifier(lexiconType);
    }

    /**
     * @return the sequencedNode
     */
    public javax.jcr.Node getSequencedNode() {
        return this.sequencedNode;
    }

    /**
     * @param sequencedNode the sequencedNode to set
     */
    public void setSequencedNode(javax.jcr.Node sequencedNode) {
        this.sequencedNode = sequencedNode;
    }

    /**
     * @return data type service
     */
    public IDataTypeManagerService getDataTypeService() {
        return getTeiidParser().getDataTypeService();
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    /**
     * @param mixin the mixin being added (cannot be <code>null</code> or empty)
     * @return <code>true</code> if mixin was added
     */
    public boolean addMixin(final String mixin) {
        CheckArg.isNotEmpty(mixin, MIXIN);
        final List<String> mixins = getMixins();

        if (!mixins.contains(mixin)) {
            if (mixins.add(mixin)) {
                setProperty(JcrConstants.JCR_MIXIN_TYPES, mixins);
            }
        }
        return false;
    }

    /**
     * @param mixin the mixin to look for (cannot be <code>null</code> or empty)
     * @return <code>true</code> if the node has the specified mixin
     */
    public boolean hasMixin(final String mixin) {
        CheckArg.isNotEmpty(mixin, MIXIN);
        return getMixins().contains(mixin);
    }

    /**
     * Get the ast identifier of the node.
     * 
     * @return the node's ast identifier; never null
     */
    public String astIdentifier() {
        return astIdentifier;
    }

    /**
     * @param identifier
     */
    protected void setAstIdentifier(String identifier) {
        this.astIdentifier = identifier;
    }

    /**
     * @return primary type
     */
    public String getPrimaryType() {
        return (String)properties.get(JcrConstants.JCR_PRIMARY_TYPE);
    }

    private void recalculatePath() {
        this.absolutePath = null;
    }

    /**
     * Get the current path of this node
     * 
     * @return the path of this node; never null
     */
    public String getAbsolutePath() {
        if (absolutePath == null) {
            StringBuilder pathBuilder = new StringBuilder(FORWARD_SLASH).append(this.astIdentifier());
            ASTNode parent = this.parent;
            while (parent != null) {
                pathBuilder.insert(0, FORWARD_SLASH + parent.astIdentifier());
                parent = parent.parent;
            }
            absolutePath = pathBuilder.toString();
        }

        return absolutePath;
    }

    /**
     * Get the property with the supplied name.
     * 
     * @param name the property name; never null
     * @return the property, or null if no such property exists on the node
     */
    public Object getProperty(String name) {
        return properties.get(name);
    }

    /**
     * Get the multi property values with the supplied name.
     * If we know that the required property is definitely multi-valued
     * then this avoids having to cast the return Object to a collection 
     * 
     * @param name the property name; never null
     * @return the collection of property values, or null if no such property exists on the node
     */
    public Collection<Object> getProperties(String name) {
        Object property = properties.get(name);
        if (property == null)
            return null;
        else if (property instanceof Collection) {
            Collection<Object> properties = (Collection<Object>) property;
            return properties;
        }

        return Collections.singleton(property);
    }

    /**
     * Set the property with the given name to the supplied value. Any existing property with the same name will be replaced.
     * 
     * @param identifier the name of the property; may not be null
     * @param value the value of the property; may not be null
     * @return this node, for method chaining purposes
     */
    public ASTNode setProperty(String identifier, Object value) {
        CheckArg.isNotNull(identifier, IDENTIFIER);
        if (value == null)
            properties.remove(identifier);

        properties.put(identifier, value);
        return this;
    }

    /**
     * Add the value at the given property identifier
     *
     * @param identifier property identifier
     * @param value property value
     * @return this ASTNode
     */
    public ASTNode addProperty(String identifier, Object value) {
        CheckArg.isNotNull(identifier, IDENTIFIER);
        CheckArg.isNotNull(value, VALUE);

        Object existingValue = properties.get(identifier);
        if (existingValue instanceof Collection) {
            Collection<Object> properties = (Collection<Object>) existingValue;
            properties.add(value);
            this.properties.put(identifier, properties);
        } else if (existingValue != null) {
            Collection<Object> values = new ArrayList<Object>();
            values.add(existingValue);
            values.add(value);
            properties.put(identifier, values);
        } else {
            properties.put(identifier, value);
        }

        return this;
    }

    /**
     * Set the property with the given name to the supplied values. If there is at least one value, the new property will replace
     * any existing property with the same name. This method does nothing if zero values are supplied.
     * 
     * @param identifier the name of the property; may not be null
     * @param values the values of the property
     * @return this node, for method chaining purposes
     */
    public ASTNode setProperty(String identifier, Object... values) {
        CheckArg.isNotNull(identifier, IDENTIFIER);
        CheckArg.isNotNull(values, VALUE);
        if (values.length != 0) {
            properties.put(identifier, Arrays.asList(values));
        }
        return this;
    }

    /**
     * Remove and return the property with the supplied name.
     * 
     * @param name the property name; may not be null
     * @return the list of values of the property that was removed, or null if there was no such property
     */
    public Object removeProperty(String name) {
        return properties.remove(name);
    }

    /**
     * Return the list of property names for this node.
     * 
     * @return the list of strings.
     */
    public List<String> getPropertyNames() {
        return new ArrayList<String>(properties.keySet());
    }

    private void appendChild(ASTNode child) {
        String key = child.astIdentifier();

        Object value = this.children.get(key);
        if (value instanceof ASTNode) {
            List<Object> values = new ArrayList<Object>();
            values.add(value);
            values.add(child);
            this.children.put(key, values);
        } else if (value instanceof List) {
            List<Object> values = (List<Object>) value;
            values.add(child);
            this.children.put(key, values);
        } else {
            // Neither a list or ASTNode so directly add the map
            this.children.put(key, child);
        }
    }

    /**
     * @return all mixin properties
     */
    @SuppressWarnings( "unchecked" )
    public List<String> getMixins() {
        Object mixinValues = getProperty(JcrConstants.JCR_MIXIN_TYPES);
        List<String> result = new ArrayList<String>();
        if (mixinValues instanceof Collection) {
            result.addAll((Collection<? extends String>)mixinValues);
        } else if (mixinValues != null) {
            result.add(mixinValues.toString());
        }
        return result;
    }

    /**
     * Get the parent of this node.
     * 
     * @return the parent node, or null if this node has no parent
     */
    public ASTNode getParent() {
        return parent;
    }

    /**
     * Set the parent for this node. If this node already has a parent, this method will remove this node from the current parent.
     * If the supplied parent is not null, then this node will be added to the supplied parent's children.
     * 
     * @param parent the new parent, or null if this node is to have no parent
     */
    public void setParent(ASTNode parent) {
        removeFromParent();
        if (parent != null) {
            this.parent = parent;
            this.parent.appendChild(this);

            this.recalculatePath();
        }
    }

    /**
     * Remove this node from its parent, and return the node that used to be the parent of this node. Note that this method
     * removes the entire subgraph under this node.
     * 
     * @return the node that was the parent of this node, or null if this node had no parent
     */
    public ASTNode removeFromParent() {
        ASTNode result = this.parent;
        if (this.parent != null) {
            // Remove this node from its current parent ...
            parent.removeChild(this);
            this.parent = null;
            this.recalculatePath();
        }
        return result;
    }

    /**
     * Get the number of child nodes.
     * 
     * @return the number of children; never negative
     */
    public int getChildCount() {
        return this.children.size();
    }

    /**
     * @param astIdentifier the identifier of the child being requested (cannot be <code>null</code> or empty)
     * @return a collection of children with the specified name (never <code>null</code> but can be empty)
     */
    public List<ASTNode> childrenWithIdentifier(final String astIdentifier) {
        CheckArg.isNotEmpty(astIdentifier, IDENTIFIER);

        if (this.children.isEmpty()) {
            return Collections.emptyList();
        }

        final List<ASTNode> matches = new ArrayList<ASTNode>();

        for (Map.Entry<String, Object> childEntry : this.children.entrySet()) {
            if (! childEntry.getKey().equals(astIdentifier))
                continue;

            Object value = childEntry.getValue();
            if (value instanceof ASTNode)
                matches.add((ASTNode) value);
            else if (value instanceof List)
                matches.addAll((Collection<? extends ASTNode>) value);

        }

        return Collections.unmodifiableList(matches);
    }

    /**
     * Utility method to obtain a {@link ASTNode} child of a parent {@link ASTNode} with the given string identifier and node type.
     *
     * @param identifier identifier to find
     * @param referenceTypeClass type class of applicable children
     * @return child matching identifier and reference type or null
     */
    public <T> T getChildforIdentifierAndRefType(String identifier, Class<T> referenceTypeClass) {
        CheckArg.isNotNull(identifier, "identifier"); //$NON-NLS-1$

        Object value = children.get(identifier);
        if (value instanceof ASTNode && referenceTypeClass.isInstance(value))
            return (T) value;
        else if (value instanceof List) {
            List<ASTNode> values = (List<ASTNode>) value;
            return referenceTypeClass.isInstance(values.get(0)) ? (T) values.get(0) : null;
        }

        return null;
    }

    /**
     * Utility method to obtain the children of a parent {@link ASTNode} with the given string identifier and node type.
     *
     * @param identifier identifier to find
     * @param referenceTypeClass type class of applicable children
     * @return children matching identifier and reference type or empty list
     */
    public <T> List<T> getChildrenforIdentifierAndRefType(String identifier, Class<T> referenceTypeClass) {
        CheckArg.isNotNull(identifier, "identifier"); //$NON-NLS-1$
        Object value = this.children.get(identifier);

        List<T> children = new ArrayList<T>();
        if (value instanceof ASTNode && referenceTypeClass.isInstance(value))
            children.add((T) value);
        else if (value instanceof List) {
            List<ASTNode> values = (List<ASTNode>) value;
            for (ASTNode astNode : values) {
                if (referenceTypeClass.isInstance(astNode))
                    children.add((T) astNode);
            }
        }

        return Collections.unmodifiableList(children);
    }

    /**
     * Add the supplied node to the end of the list of children.
     * 
     * @param child the node that should be added as the last child; may not be null
     */
    private void addLastChild(ASTNode child) {
        assert child != null;
        this.appendChild(child);
        child.removeFromParent();
        child.parent = this;
        child.recalculatePath();
    }

    /**
     * Convenience method for appending a child LanguageObject
     *
     * @param referenceName name for identifier
     * @param languageObject child to add
     */
    public void addLastChild(String referenceName, LanguageObject languageObject) {
        if (languageObject == null)
            languageObject = new NullNode(getTeiidParser());

        ASTNode childNode = (ASTNode)languageObject;
        childNode.setAstIdentifier(referenceName);
        addLastChild(childNode);
    }

    /**
     * Adds the given language object as the singleton child at the given reference.
     * Note: this will remove any existing child set against this reference
     *
     * @param referenceName name for identifier
     * @param languageObject child to add
     */
    public void setChild(String referenceName, LanguageObject languageObject) {
        removeChildren(referenceName);

        if (languageObject == null)
            return;

        ArgCheck.isInstanceOf(ASTNode.class, languageObject);
        addLastChild(referenceName, languageObject);
    }
    
    /**
     * Add the collection of language objects at the given reference
     * Note: this removes any existing children at this reference
     *
     * @param referenceName name for identifier
     * @param languageObjects children to add
     */
    public void setChildren(String referenceName, Collection<? extends LanguageObject> languageObjects) {
        removeChildren(referenceName);

        if (languageObjects == null)
            return;

        for (LanguageObject object : languageObjects) {
            addLastChild(referenceName, object);
        }
    }

    /**
     * Remove the children with the given ast identifier
     *
     * @param astIdentifier identifier of children to remove
     */
    public void removeChildren(String astIdentifier) {
        List<ASTNode> childrenWithIdentifier = this.childrenWithIdentifier(astIdentifier);
        for (ASTNode child : childrenWithIdentifier) {
            removeChild(child);
        }
    }

    /**
     * Remove the node from this node.
     * 
     * @param child the child node; may not be null
     * @return true if the child was removed from this node, or false if the supplied node was not a child of this node
     */
    public boolean removeChild(ASTNode child) {
        String key = child.astIdentifier();
        boolean result = false;

        Object value = this.children.remove(key);

        if (value instanceof ASTNode) {
            result = true;
        } else if (value instanceof List) {
            // if value is a list then we need to remove the child
            // and put the list back
            List<ASTNode> values = (List<ASTNode>) value;
            result = values.remove(child);
            this.children.put(key, values);
        }

        if (result) {
            child.parent = null;
            child.recalculatePath();
        }

        return result;
    }

    /**
     * Get the unmodifiable list of child nodes. This list will immediately reflect any changes made to the children (via other
     * methods), but this list cannot be used to add or remove children.
     * 
     * @return the list of children, which immediately reflects changes but which cannot be modified directly; never null
     */
    public Iterator<ASTNode> getChildren() {
        return new ChildNodeIterator();
    }

    /**
     * @param mixin the mixin to match children with (cannot be <code>null</code> or empty)
     * @return the children having the specified mixin (never <code>null</code>)
     */
    public List<ASTNode> getChildren(final String mixin) {
        final List<ASTNode> result = new ArrayList<ASTNode>();

        Iterator<ASTNode> nodeIter = getChildren();
        while(nodeIter.hasNext()) {
            ASTNode kid = nodeIter.next();
            if (kid.getMixins().contains(mixin)) {
                result.add(kid);
            }
        }
        return result;
    }

    /**
     * Determine whether the supplied plan is equivalent to this plan.
     * 
     * @param other the other plan to compare with this instance
     * @return true if the two plans are equivalent, or false otherwise
     */
    public boolean isSameAs(ASTNode other) {
        if (other == null) {
            return false;
        }
        if (!this.astIdentifier.equals(other.astIdentifier)) {
            return false;
        }
        if (!this.properties.equals(other.properties)) {
            return false;
        }
        if (this.getChildCount() != other.getChildCount()) {
            return false;
        }
        Iterator<ASTNode> thisChildren = this.getChildren();
        Iterator<ASTNode> thatChildren = other.getChildren();
        while (thisChildren.hasNext() && thatChildren.hasNext()) {
            if (!thisChildren.next().isSameAs(thatChildren.next())) {
                return false;
            }
        }
        return true;
    }

    protected Class<?> convertTypeClassPropertyToClass(String propertyName) {
        Object property = getProperty(propertyName);
        if (property == null)
            return null;

        DataTypeName dataTypeName = DataTypeName.findDataTypeName(property.toString());
        return getDataTypeService().getDefaultDataClass(dataTypeName);
    }

    /**
     * {@inheritDoc}     * <p>
     * This class returns a new clone of the plan tree rooted at this node. However, the top node of the resulting plan tree (that
     * is, the node returned from this method) has no parent.
     * </p>
     */
    @Override
    public abstract ASTNode clone();
//        return cloneWithoutNewParent();
//    }

//    protected ASTNode cloneWithoutNewParent() {
//        ASTNode result = new ASTNode(getTeiidParser(), getId());
//        result.properties.putAll(this.properties);
//        // Clone the children ...
//        for (ASTNode child : children) {
//            ASTNode childClone = child.cloneWithoutNewParent();
//            // The child has no parent, so add the child to the new result ...
//            result.addLastChild(childClone);
//        }
//        return result;
//    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(getAbsolutePath());
        stringBuilder.append(OPEN_SQUARE_BRACKET);
        for (Iterator<String> propertyIt = properties.keySet().iterator(); propertyIt.hasNext();) {
            String propertyName = propertyIt.next();
            stringBuilder.append(propertyName).append(COLON).append(properties.get(propertyName));
            if (propertyIt.hasNext()) {
                {
                    stringBuilder.append(COMMA);
                    stringBuilder.append(SPACE);
                }
            }
        }
        stringBuilder.append(CLOSE_SQUARE_BRACKET);
        return stringBuilder.toString();
    }
}
