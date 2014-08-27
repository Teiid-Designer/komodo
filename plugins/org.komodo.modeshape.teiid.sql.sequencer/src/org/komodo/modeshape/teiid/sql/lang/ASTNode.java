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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
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
public abstract class ASTNode extends SimpleNode implements LanguageObject, StringConstants, Iterable<ASTNode>, Cloneable {

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

    private ASTNode parent;
    private String astIdentifier;

    private final Map<String, Object> properties = new HashMap<String, Object>();
    private final LinkedList<ASTNode> children = new LinkedList<ASTNode>();
    private final List<ASTNode> childrenView = Collections.unmodifiableList(children);

    /**
     * @param parser
     * @param nodeTypeIndex
     */
    public ASTNode(TeiidParser parser, int nodeTypeIndex) {
        super(parser, nodeTypeIndex);

        String lexiconType = TeiidSqlLexicon.getTypeId(getClass());
        ArgCheck.isNotNull(lexiconType);
        setProperty(JCR_MIXIN_TYPES, lexiconType);
        setProperty(JCR_PRIMARY_TYPE, NT_UNSTRUCTURED);
        setAstIdentifier(lexiconType);
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

    /**
     * Get the current same-name-sibling index.
     * 
     * @return the SNS index, or 1 if this is the first sibling with the same name
     */
    public int getSameNameSiblingIndex() {
        int snsIndex = 1;
        if (this.parent == null) {
            return snsIndex;
        } // Go through all the children ...
        for (ASTNode sibling : this.parent.getChildren()) {
            if (sibling == this) {
                break;
            }
            if (sibling.astIdentifier().equals(this.astIdentifier)) {
                ++snsIndex;
            }
        }
        return snsIndex;
    }

    /**
     * Get the current path of this node
     * 
     * @return the path of this node; never null
     */
    public String getAbsolutePath() {
        StringBuilder pathBuilder = new StringBuilder(FORWARD_SLASH).append(this.astIdentifier());
        ASTNode parent = this.parent;
        while (parent != null) {
            pathBuilder.insert(0, FORWARD_SLASH + parent.astIdentifier());
            parent = parent.parent;
        }
        return pathBuilder.toString();
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
     * @param identifier
     * @param value
     * @return this ASTNode
     */
    public ASTNode addProperty(String identifier, Object value) {
        CheckArg.isNotNull(identifier, IDENTIFIER);
        CheckArg.isNotNull(value, VALUE);

        Object property = properties.get(identifier);
        if (property instanceof Collection) {
            Collection<Object> properties = (Collection<Object>) property;
            properties.add(value);
            this.properties.put(identifier, properties);
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
            this.parent.children.add(this);
        }
    }

    /**
     * Insert the supplied node into the plan node tree immediately above this node. If this node has a parent when this method is
     * called, the new parent essentially takes the place of this node within the list of children of the old parent. This method
     * does nothing if the supplied new parent is null.
     * <p>
     * For example, consider a plan node tree before this method is called:
     * 
     * <pre>
     *        A
     *      / | \
     *     /  |  \
     *    B   C   D
     * </pre>
     * 
     * Then after this method is called with <code>c.insertAsParent(e)</code>, the resulting plan node tree will be:
     * 
     * <pre>
     *        A
     *      / | \
     *     /  |  \
     *    B   E   D
     *        |
     *        |
     *        C
     * </pre>
     * 
     * </p>
     * <p>
     * Also note that the node on which this method is called ('C' in the example above) will always be added as the
     * last child to the new parent. This allows the new parent to already have children before
     * this method is called.
     * </p>
     * 
     * @param newParent the new parent; method does nothing if this is null
     */
    public void insertAsParent(ASTNode newParent) {
        if (newParent == null) {
            return;
        }
        newParent.removeFromParent();
        if (this.parent != null) {
            this.parent.replaceChild(this, newParent);
        }
        newParent.addLastChild(this);
    }

    /**
     * Remove this node from its parent, and return the node that used to be the parent of this node. Note that this method
     * removes the entire subgraph under this node.
     * 
     * @return the node that was the parent of this node, or null if this node had no parent
     * @see #extractChild(ASTNode)
     * @see #extractFromParent()
     */
    public ASTNode removeFromParent() {
        ASTNode result = this.parent;
        if (this.parent != null) {
            // Remove this node from its current parent ...
            parent.removeChild(this);
            this.parent = null;
        }
        return result;
    }

    /**
     * Replace the supplied child with another node. If the replacement is already a child of this node, this method effectively
     * swaps the position of the child and replacement nodes.
     * 
     * @param child the node that is already a child and that is to be replaced; may not be null and must be a child
     * @param replacement the node that is to replace the 'child' node; may not be null
     * @return true if the child was successfully replaced
     */
    public boolean replaceChild(ASTNode child, ASTNode replacement) {
        assert child != null;
        assert replacement != null;
        if (child.parent == this) {
            int i = this.children.indexOf(child);
            if (replacement.parent == this) {
                // Swapping the positions ...
                int j = this.children.indexOf(replacement);
                this.children.set(i, replacement);
                this.children.set(j, child);
                return true;
            } // The replacement is not yet a child ...
            this.children.set(i, replacement);
            replacement.removeFromParent();
            replacement.parent = this;
            child.parent = null;
            return true;
        }
        return false;
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
     * Get the first child.
     * 
     * @return the first child, or null if there are no children
     */
    public ASTNode getFirstChild() {
        return this.children.isEmpty() ? null : this.children.getFirst();
    }

    /**
     * Get the last child.
     * 
     * @return the last child, or null if there are no children
     */
    public ASTNode getLastChild() {
        return this.children.isEmpty() ? null : this.children.getLast();
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

        for (final ASTNode kid : this.children) {
            if (astIdentifier.equals(kid.astIdentifier())) {
                matches.add(kid);
            }
        }
        return Collections.unmodifiableList(matches);
    }

    /**
     * Get the child at the supplied index.
     * 
     * @param index the index
     * @return the child, or null if there are no children
     * @throws IndexOutOfBoundsException if the index is not valid given the number of children
     */
    public ASTNode getChild(int index) {
        return this.children.isEmpty() ? null : this.children.get(index);
    }

    /**
     * Utility method to obtain the children of a given node that match the given type
     *
     * @param nodeType the type property of the target child node; may not be null
     * @return the list of typed nodes (may be empty)
     */
    public List<ASTNode> getChildrenForType(String nodeType ) {
        CheckArg.isNotNull(nodeType, "nodeType"); //$NON-NLS-1$

        List<ASTNode> childrenOfType = new ArrayList<ASTNode>();
        for (ASTNode child : getChildren()) {
            if (child.hasMixin(nodeType)) {
                childrenOfType.add(child);
            }
        }

        return Collections.unmodifiableList(childrenOfType);
    }

    /**
     * Utility method to obtain a {@link ASTNode} child of a parent {@link ASTNode} with the given string identifier and node type.
     *
     * @param identifier
     * @param referenceTypeClass
     * @return child matching identifier and reference type or null
     */
    public <T> T getChildforIdentifierAndRefType(String identifier, Class<T> referenceTypeClass) {
        CheckArg.isNotNull(identifier, "identifier"); //$NON-NLS-1$

        for (ASTNode child : getChildren()) {
            if (identifier.equalsIgnoreCase(child.astIdentifier())) {
                if (referenceTypeClass.isInstance(child))
                    return (T) child;
            }
        }
        return null;
    }

    /**
     * Utility method to obtain the children of a parent {@link ASTNode} with the given string identifier and node type.
     *
     * @param identifier
     * @param referenceTypeClass
     * @return children matching identifier and reference type or empty list
     */
    public <T> List<T> getChildrenforIdentifierAndRefType(String identifier, Class<T> referenceTypeClass) {
        CheckArg.isNotNull(identifier, "identifier"); //$NON-NLS-1$
        List<T> children = new ArrayList<T>();
        
        for (ASTNode child : getChildren()) {
            if (identifier.equalsIgnoreCase(child.astIdentifier())) {
                if (referenceTypeClass.isInstance(child))
                    children.add((T) child);
            }
        }

        return Collections.unmodifiableList(children);
    }

    /**
     * Utility method to obtain a {@link ASTNode} child of a parent {@link ASTNode} with the given string identifier and node type.
     *
     * @param identifier the identifier property of the node; may not be null
     * @param nodeType the type property of the target child node; may not be null
     * @return the matched child (may be null)
     */
    public ASTNode getChildforIdentifierAndType(String identifier, String nodeType) {
        CheckArg.isNotNull(identifier, "identifier"); //$NON-NLS-1$
        CheckArg.isNotNull(nodeType, "nodeType"); //$NON-NLS-1$

        for (ASTNode child : getChildren()) {
            if (child.hasMixin(nodeType)) {
                if (identifier.equalsIgnoreCase(child.astIdentifier())) {
                    return child;
                }
            }
        }
        return null;
    }

    /**
     * Add the supplied node to the front of the list of children.
     * 
     * @param child the node that should be added as the first child; may not be null
     */
    public void addFirstChild(ASTNode child) {
        assert child != null;
        this.children.addFirst(child);
        child.removeFromParent();
        child.parent = this;
    }

    /**
     * Add the supplied node to the end of the list of children.
     * 
     * @param child the node that should be added as the last child; may not be null
     */
    private void addLastChild(ASTNode child) {
        assert child != null;
        this.children.addLast(child);
        child.removeFromParent();
        child.parent = this;
    }

    /**
     * Convenience method for appending a child LanguageObject
     *
     * @param referenceName
     * @param languageObject
     */
    public void addLastChild(String referenceName, LanguageObject languageObject) {
        if (languageObject == null)
            return;

        ArgCheck.isInstanceOf(ASTNode.class, languageObject);

        ASTNode childNode = (ASTNode)languageObject;
        childNode.setAstIdentifier(referenceName);
        addLastChild(childNode);
    }

    /**
     * Adds the given language object as the singleton child at the given reference.
     * Note: this will remove any existing child set against this reference
     *
     * @param referenceName
     * @param languageObject
     */
    public void setChild(String referenceName, LanguageObject languageObject) {
        if (languageObject == null)
            return;

        ArgCheck.isInstanceOf(ASTNode.class, languageObject);

        removeChildren(referenceName);
        addLastChild(referenceName, languageObject);
    }
    
    /**
     * Add the collection of language objects at the given reference
     * Note: this removes any existing children at this reference
     *
     * @param referenceName
     * @param languageObjects
     */
    public void setChildren(String referenceName, Collection<? extends LanguageObject> languageObjects) {
        if (languageObjects == null)
            return;

        removeChildren(referenceName);

        for (LanguageObject object : languageObjects) {
            addLastChild(referenceName, object);
        }
    }

    /**
     * Add the supplied nodes at the end of the list of children.
     * 
     * @param otherChildren the children to add; may not be null
     */
    public void addChildren(Iterable<ASTNode> otherChildren) {
        assert otherChildren != null;
        for (ASTNode planNode : otherChildren) {
            this.addLastChild(planNode);
        }
    }

    /**
     * Add the supplied nodes at the end of the list of children.
     * 
     * @param first the first child to add
     * @param second the second child to add
     */
    public void addChildren(ASTNode first, ASTNode second) {
        if (first != null) {
            this.addLastChild(first);
        }
        if (second != null) {
            this.addLastChild(second);
        }
    }

    /**
     * Add the supplied nodes at the end of the list of children.
     * 
     * @param first the first child to add
     * @param second the second child to add
     * @param third the third child to add
     */
    public void addChildren(ASTNode first, ASTNode second, ASTNode third) {
        if (first != null) {
            this.addLastChild(first);
        }
        if (second != null) {
            this.addLastChild(second);
        }
        if (third != null) {
            this.addLastChild(third);
        }
    }

    /**
     * Remove the children with the given ast identifier
     *
     * @param astIdentifier
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
        boolean result = this.children.remove(child);
        if (result) {
            child.parent = null;
        }
        return result;
    }

    /**
     * Remove the child node from this node, and replace that child with its first child (if there is one).
     * 
     * @param child the child to be extracted; may not be null and must have at most 1 child
     * @see #extractFromParent()
     */
    public void extractChild(ASTNode child) {
        if (child.getChildCount() == 0) {
            removeChild(child);
        } else {
            ASTNode grandChild = child.getFirstChild();
            replaceChild(child, grandChild);
        }
    }

    /**
     * Extract this node from its parent, but replace this node with its child (if there is one).
     * 
     * @see #extractChild(ASTNode)
     */
    public void extractFromParent() {
        this.parent.extractChild(this);
    }

    /**
     * Get the unmodifiable list of child nodes. This list will immediately reflect any changes made to the children (via other
     * methods), but this list cannot be used to add or remove children.
     * 
     * @return the list of children, which immediately reflects changes but which cannot be modified directly; never null
     */
    public List<ASTNode> getChildren() {
        return childrenView;
    }

    /**
     * @param mixin the mixin to match children with (cannot be <code>null</code> or empty)
     * @return the children having the specified mixin (never <code>null</code>)
     */
    public List<ASTNode> getChildren(final String mixin) {
        final List<ASTNode> result = new ArrayList<ASTNode>();

        for (final ASTNode kid : getChildren()) {
            if (kid.getMixins().contains(mixin)) {
                result.add(kid);
            }
        }
        return result;
    }

    /**
     * {@inheritDoc}     * <p>
     * This iterator is immutable.
     * </p>
     * 
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<ASTNode> iterator() {
        return childrenView.iterator();
    }

    /**
     * Remove all children from this node. All nodes immediately become orphaned. The resulting list will be mutable.
     * 
     * @return a copy of all the of the children that were removed (and which have no parent); never null
     */
    public List<ASTNode> removeAllChildren() {
        if (this.children.isEmpty()) {
            return new ArrayList<ASTNode>(0);
        }
        List<ASTNode> copyOfChildren = new ArrayList<ASTNode>(this.children);
        for (Iterator<ASTNode> childIter = this.children.iterator(); childIter.hasNext();) {
            ASTNode child = childIter.next();
            childIter.remove();
            child.parent = null;
        }
        return copyOfChildren;
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
        Iterator<ASTNode> thisChildren = this.getChildren().iterator();
        Iterator<ASTNode> thatChildren = other.getChildren().iterator();
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
