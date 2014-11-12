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

package org.teiid.query.mapping.xml;

import java.util.ArrayList;
import java.util.List;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.xml.MappingBaseNode;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.runtime.client.Messages;



/** 
 * This is base class to define all nodes except the attribute. However, this quite not
 * enough to define a Element node. Specially designed for sequence, choice and all node 
 * types
 */
public abstract class MappingBaseNodeImpl extends MappingNodeImpl implements MappingBaseNode<MappingNodeImpl> {
    // An ID on the recursive parent as to who the recursive child node is?  
    String recursionId;
    
    protected MappingBaseNodeImpl(TeiidClientParser teiidParser) {
        super(teiidParser);
    }
    
    @Override
    public void addChildNode(MappingNodeImpl childNode) {
        if (childNode instanceof MappingAllNodeImpl)
            addAllNode((MappingAllNodeImpl) childNode);
        else if (childNode instanceof MappingChoiceNodeImpl)
            addChoiceNode((MappingChoiceNodeImpl) childNode);
        else if (childNode instanceof MappingCriteriaNodeImpl)
            addCriteriaNode((MappingCriteriaNodeImpl) childNode);
        else if (childNode instanceof MappingElementImpl)
            addChildElement((MappingElementImpl) childNode);
        else if (childNode instanceof MappingSequenceNodeImpl)
            addSequenceNode((MappingSequenceNodeImpl) childNode);
        else if (childNode instanceof MappingSourceNodeImpl)
            addSourceNode((MappingSourceNodeImpl) childNode);
        else
            throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID30457, childNode));
    }

    public void setMinOccurrs(int cardinality) {
        setProperty(MappingNodeConstants.Properties.CARDINALITY_MIN_BOUND, new Integer(cardinality));
    }

    public void setMaxOccurrs(int cardinality) {
        setProperty(MappingNodeConstants.Properties.CARDINALITY_MAX_BOUND, new Integer(cardinality));
    }    
         
    public void setSource(String source) {
        if (source != null) {
            setProperty(MappingNodeConstants.Properties.RESULT_SET_NAME, source);
        }
        else {
            removeProperty(MappingNodeConstants.Properties.RESULT_SET_NAME);
        }
    }
         
    public String getSource() {
        return (String)getProperty(MappingNodeConstants.Properties.RESULT_SET_NAME);
    }     
    
    public void addChildElement(MappingElementImpl elem) {
        if (elem.isRecursive()) {
            MappingRecursiveElementImpl recursiveElement = (MappingRecursiveElementImpl)elem;
            MappingBaseNodeImpl recursiveRoot = getRecursiveRootNode(recursiveElement);
			String mappingClass = recursiveElement.getMappingClass();

			/* The upper case of the class is used in Teiid 7 mappings */
			TeiidVersion minVersion = getTeiidVersion().getMinimumVersion();
			if (minVersion.isLessThan(Version.TEIID_8_0.get()))
				mappingClass = mappingClass.toUpperCase();

            recursiveRoot.setRootRecursiveNode(true, mappingClass);
            addChild(elem);
        }
        else {
            addChild(elem);
        }
    }
    
    private MappingBaseNodeImpl getRecursiveRootNode(MappingRecursiveElementImpl elem) {
        if (hasSource(elem.getMappingClass())) {
            return this;
        }
        MappingBaseNodeImpl parent = this.getParentNode();
        if (parent != null) {
            return parent.getRecursiveRootNode(elem);
        }
         throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID30457, elem));
    }
    
    /**
     * Any node with its parent node of Source Node, is like property on node itself, 
     * as all the source nodes will have atmost one child. 
     * @param source
     * @return
     */
    private boolean hasSource(String source) {
        return source.equals(getSource());
    }
    
    public void addChoiceNode(MappingChoiceNodeImpl elem) {
        addChild(elem);
    }
    
    public void addSequenceNode(MappingSequenceNodeImpl elem) {
        addChild(elem);
    }
    
    public void addAllNode(MappingAllNodeImpl elem) {
        addChild(elem);
    }     
    
    public void addSourceNode(MappingSourceNodeImpl elem) {
        addChild(elem);
    }     
    
    public void addCriteriaNode(MappingCriteriaNodeImpl node) {
        addChild(node);     
    }
    
    public MappingBaseNodeImpl getParentNode() {
        if (getParent() instanceof MappingBaseNodeImpl) {
            return (MappingBaseNodeImpl)getParent();
        }
        return null;
    }
    
    public String getName() {
        // if we decide to give the choice/seq/all nodes names then
        // we need to change the logic in the "getFullName()"
        return null;
    }
    
	@Removed(Version.TEIID_8_0)
    public String getCanonicalName() {
        return getFullyQualifiedName().toUpperCase();
    }

    public void removeChildNode(MappingBaseNodeImpl toRemove) {
        getChildren().remove(toRemove);
    }
        
    public int getMinOccurence() {
        Integer occur = (Integer)getProperty(MappingNodeConstants.Properties.CARDINALITY_MIN_BOUND);
        if (occur != null) {
            return occur.intValue();
        }
        return 1;
    }
    
    public int getMaxOccurence() {
        Integer occur = (Integer)getProperty(MappingNodeConstants.Properties.CARDINALITY_MAX_BOUND);
        if (occur != null) {
            return occur.intValue();
        }
        return 1;
    }
                    
    /**
     * specify the element is a recursive root
     * @param root
     */
    void setRootRecursiveNode(boolean root, String recursionId) {
        setProperty(MappingNodeConstants.Properties.IS_RECURSIVE_ROOT, Boolean.valueOf(root));
        this.recursionId = recursionId;
    }
        
    public String getRecursionId() {
        return this.recursionId;
    }
    
    public boolean isRootRecursiveNode() {
        return Boolean.TRUE.equals(getProperty(MappingNodeConstants.Properties.IS_RECURSIVE_ROOT));        
    }
    
    /**
     * Get the document node of this node.
     * @return
     */
    public MappingDocumentImpl getDocument() {
        if (isDocumentNode()) {
            return (MappingDocumentImpl)this;
        }
        return getParentNode().getDocument();
    }
    
    public boolean isDocumentNode() {
        return false;
    }
    
    /**
     * A tag root node is the first visual node (Element to be specific) in the document tree
     * which is the root element in the output xml document. 
     * @return true if 
     */
    public boolean isTagRoot() {
        return false;
    }
    
    public List<String> getStagingTables() {
        return (List<String>)getProperty(MappingNodeConstants.Properties.TEMP_GROUP_NAMES);
    }
    
    public void setStagingTables(List<String> tables) {
        if (tables != null) {
            setProperty(MappingNodeConstants.Properties.TEMP_GROUP_NAMES, tables);
        }
        else {
            removeProperty(MappingNodeConstants.Properties.TEMP_GROUP_NAMES);
        }
    }
    
    public void addStagingTable(String tablename) {
        if (tablename == null) {
            return;
        }
        List<String> tables = getStagingTables();
        if (tables == null || tables.isEmpty()) {
            tables = new ArrayList<String>();
        }
        tables.add(tablename);
        setProperty(MappingNodeConstants.Properties.TEMP_GROUP_NAMES, tables);
    }  
    
    @Override
    public MappingNodeImpl clone() {
    	MappingBaseNodeImpl clone = (MappingBaseNodeImpl) super.clone();
    	List<String> staging = getStagingTables();
    	if (getStagingTables() != null && staging != MappingNodeConstants.Defaults.DEFAULT_VALUES.get(MappingNodeConstants.Properties.TEMP_GROUP_NAMES)) {
    		clone.setStagingTables(new ArrayList<String>(staging));
    	}
    	return clone;
    }
    
}
