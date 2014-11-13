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

package org.teiid.query.sql.visitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import org.komodo.spi.query.sql.GroupCollectorVisitor;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.navigator.DeepPreOrderNavigator;
import org.teiid.query.sql.navigator.PreOrderNavigator;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.runtime.client.Messages;


/**
 * <p>This visitor class will traverse a language object tree and collect all group
 * symbol references it finds.  It uses a collection to collect the groups in so
 * different collections will give you different collection properties - for instance,
 * using a Set will remove duplicates.</p>
 *
 * <p>The easiest way to use this visitor is to call the static methods which create
 * the visitor (and possibly the collection), run the visitor, and get the collection.
 * The public visit() methods should NOT be called directly.</p>
 */
public class GroupCollectorVisitorImpl extends TCLanguageVisitorImpl
    implements GroupCollectorVisitor<BaseLanguageObject, GroupSymbolImpl> {

    private Collection<GroupSymbolImpl> groups;

    private boolean isIntoClauseGroup;
       
    // In some cases, set a flag to ignore groups created by a subquery from clause
    private boolean ignoreInlineViewGroups = false;
    private Collection<GroupSymbolImpl> inlineViewGroups;    // groups defined by a SubqueryFromClause

    /**
     * Construct a new visitor with a default returning collection
     * @param teiidVersion 
     * 
     * @param removeDuplicates 
     */
    public GroupCollectorVisitorImpl(TeiidVersion teiidVersion, boolean removeDuplicates) {
        this(teiidVersion, removeDuplicates ? new HashSet<GroupSymbolImpl>() : new ArrayList<GroupSymbolImpl>());
    }
    
    /**
     * Construct a new visitor with the specified collection, which should
     * be non-null.
     * @param teiidVersion 
     * @param groups Collection to use for groups
     * @throws IllegalArgumentException If groups is null
     */
	public GroupCollectorVisitorImpl(TeiidVersion teiidVersion, Collection<GroupSymbolImpl> groups) {
	    super(teiidVersion);
        if(groups == null) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0023));
        }
        this.groups = groups;
    }

    /**
     * Get the groups collected by the visitor.  This should best be called
     * after the visitor has been run on the language object tree.
     * @return Collection of {@link org.teiid.query.sql.symbol.GroupSymbolImpl}
     */
    public Collection<GroupSymbolImpl> getGroups() {
        return this.groups;
    }
    
    public Collection<GroupSymbolImpl> getInlineViewGroups() {
        return this.inlineViewGroups;
    }
    
    public void setIgnoreInlineViewGroups(boolean ignoreInlineViewGroups) {
        this.ignoreInlineViewGroups = ignoreInlineViewGroups;
    }
    
    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    public void visit(GroupSymbolImpl obj) {
        if(this.isIntoClauseGroup){
            if (!obj.isTempGroupSymbol()) {
                // This is a physical group. Collect it.
                this.groups.add(obj);
            }
            this.isIntoClauseGroup = false;
        }else{
            this.groups.add(obj);
        }
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    public void visit(StoredProcedureImpl obj) {
        this.groups.add(obj.getGroup());
    }

    public void visit(IntoImpl obj) {
        this.isIntoClauseGroup = true;
    }
    
    
    public void visit(SubqueryFromClauseImpl obj) {
        if(this.ignoreInlineViewGroups) {
            if(this.inlineViewGroups == null) { 
                this.inlineViewGroups = new ArrayList<GroupSymbolImpl>();
            }
            this.inlineViewGroups.add(obj.getGroupSymbol());
        }
    }
    
    /**
     * Helper to quickly get the groups from obj in the groups collection
     * @param obj Language object
     * @param elements Collection to collect groups in
     */
    public static void getGroups(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups) {
        GroupCollectorVisitorImpl visitor = new GroupCollectorVisitorImpl(obj.getTeiidVersion(), groups);
        PreOrderNavigator.doVisit(obj, visitor);
    }

    /**
     * Helper to quickly get the groups from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate groups will be
     * filtered out.
     * @param obj Language object
     * @param removeDuplicates True to remove duplicates
     * @return Collection of {@link org.teiid.query.sql.symbol.GroupSymbolImpl}
     */
    public static Collection<GroupSymbolImpl> getGroups(BaseLanguageObject obj, boolean removeDuplicates) {
        Collection<GroupSymbolImpl> groups = null;
        if(removeDuplicates) {
            groups = new HashSet<GroupSymbolImpl>();
        } else {
            groups = new ArrayList<GroupSymbolImpl>();
        }
        GroupCollectorVisitorImpl visitor = new GroupCollectorVisitorImpl(obj.getTeiidVersion(), groups);
        PreOrderNavigator.doVisit(obj, visitor);
        return groups;
    }
    
    /**
     * Helper to quickly get the groups from obj in the groups collection
     * @param obj Language object
     * @param elements Collection to collect groups in
     */
    public static void getGroupsIgnoreInlineViews(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups) {
        GroupCollectorVisitorImpl visitor = new GroupCollectorVisitorImpl(obj.getTeiidVersion(), groups);
        visitor.setIgnoreInlineViewGroups(true);
        DeepPreOrderNavigator.doVisit(obj, visitor);  
        
        if(visitor.getInlineViewGroups() != null) {
            groups.removeAll(visitor.getInlineViewGroups());
        }
    }

    /**
     * Helper to quickly get the groups from obj in a collection.  The 
     * removeDuplicates flag affects whether duplicate groups will be 
     * filtered out.
     * @param obj Language object
     * @param removeDuplicates True to remove duplicates
     * @return Collection of {@link org.teiid.query.sql.symbol.GroupSymbolImpl}
     */
    public static Collection<GroupSymbolImpl> getGroupsIgnoreInlineViews(BaseLanguageObject obj, boolean removeDuplicates) {
        Collection<GroupSymbolImpl> groups = null;
        if(removeDuplicates) { 
            groups = new HashSet<GroupSymbolImpl>();
        } else {
            groups = new ArrayList<GroupSymbolImpl>();
        }    
        GroupCollectorVisitorImpl visitor = new GroupCollectorVisitorImpl(obj.getTeiidVersion(), groups);
        visitor.setIgnoreInlineViewGroups(true);
        DeepPreOrderNavigator.doVisit(obj, visitor);
        
        if(visitor.getInlineViewGroups() != null) {
            groups.removeAll(visitor.getInlineViewGroups());
        }

        return groups;
    }
    
    @Override
    public Collection<GroupSymbolImpl> findGroups(BaseLanguageObject obj) {
        PreOrderNavigator.doVisit(obj, this);
        return groups;
    }
    
    @Override
    public Collection<GroupSymbolImpl> findGroupsIgnoreInlineViews(BaseLanguageObject obj) {
        setIgnoreInlineViewGroups(true);
        DeepPreOrderNavigator.doVisit(obj, this);  
        
        if(getInlineViewGroups() != null) {
            groups.removeAll(getInlineViewGroups());
        }
        
        return groups;
    }

}
