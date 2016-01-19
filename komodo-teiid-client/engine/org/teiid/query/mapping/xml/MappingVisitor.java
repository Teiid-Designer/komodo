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

import java.util.Iterator;
import java.util.List;


/** 
 * A Visitor framework for navigating the Mapping Nodes
 */
public class MappingVisitor {
    private boolean abort = false;

    protected void setAbort(boolean abort) {
        this.abort = abort;
    }
    
    protected boolean shouldAbort() {
        return abort;
    }    
    
    /**
	 * @param node  
	 */
    public void visit(MappingNodeImpl node) {}
    
    public void visit(MappingDocumentImpl doc) {
        visit((MappingNodeImpl)doc);
    }
    public void visit(MappingElementImpl element) {
        visit((MappingBaseNodeImpl)element);
    }
    public void visit(MappingAttributeImpl attribute) {
        visit((MappingNodeImpl)attribute);
    }
    public void visit(MappingBaseNodeImpl baseNode) {
        visit((MappingNodeImpl)baseNode);
    }
    public void visit(MappingChoiceNodeImpl choice) {
        visit((MappingBaseNodeImpl)choice);
    }
    public void visit(MappingSequenceNodeImpl sequence) {
        visit((MappingBaseNodeImpl)sequence);
    }
    public void visit(MappingAllNodeImpl all) {
        visit((MappingBaseNodeImpl)all);
    }
    public void visit(MappingCommentNodeImpl comment) {
        visit((MappingNodeImpl)comment);
    }
    public void visit(MappingCriteriaNodeImpl node) {
        visit((MappingBaseNodeImpl)node);
    }
    public void visit(MappingRecursiveElementImpl element) {
        visit((MappingElementImpl)element);
    }
    public void visit(MappingSourceNodeImpl element) {
        visit((MappingBaseNodeImpl)element);
    }
    /** 
     * @param element
     */
    protected void walkChildNodes(MappingNodeImpl element) {

        List<MappingNodeImpl> children = element.getNodeChildren();
        for(Iterator<MappingNodeImpl> i=children.iterator(); i.hasNext();) {
            
            if (shouldAbort()) {
                break;
            }
            
            MappingNodeImpl node = i.next();            
            node.acceptVisitor(this);
        }
    }    
    
    /** 
     * @param element
     */
    protected void walkAttributes(MappingElementImpl element) {
        List attributes = element.getAttributes();
        for(Iterator i=attributes.iterator(); i.hasNext();) {
            if (shouldAbort()) {
                break;
            }            
            visit((MappingAttributeImpl)i.next());
        }
    }     
}
