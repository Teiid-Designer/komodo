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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.komodo.spi.query.sql.GroupsUsedByElementsVisitor;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;


/**
 * groups used by elements visitor
 */
public class GroupsUsedByElementsVisitorImpl
    implements GroupsUsedByElementsVisitor<BaseLanguageObject, GroupSymbolImpl> {

    /**
     * Helper to quickly get the groups from obj in the elements collection
     * @param obj Language object
     * @param groups Collection to collect groups in
     */
    public static final void getGroups(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups) {
        Collection<ElementSymbolImpl> elements = ElementCollectorVisitorImpl.getElements(obj, true);

        for (ElementSymbolImpl elementSymbol : elements) {
        	if (elementSymbol.getGroupSymbol() != null) {
        		groups.add(elementSymbol.getGroupSymbol());  
        	}
        }
    }

    /**
     * Helper to quickly get the groups from obj in a collection.  Duplicates
     * are removed.
     * @param obj Language object
     * @return Collection of {@link org.teiid.query.sql.symbol.GroupSymbolImpl}
     */
    public static final Set<GroupSymbolImpl> getGroups(BaseLanguageObject obj) {
        Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
        getGroups(obj, groups);
        return groups;
    }
    
    /**
     * @param objects
     * @return set of group symbols
     */
    public static Set<GroupSymbolImpl> getGroups(Collection<? extends BaseLanguageObject> objects) {
        Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
        getGroups(objects, groups);
        return groups;
    }

    /**
     * @param objects
     * @param groups
     */
    public static void getGroups(Collection<? extends BaseLanguageObject> objects, Set<GroupSymbolImpl> groups) {
        // Get groups from elements     
        for (BaseLanguageObject languageObject : objects) {
            if (languageObject instanceof ElementSymbolImpl) {
                ElementSymbolImpl elem = (ElementSymbolImpl) languageObject;
                groups.add(elem.getGroupSymbol());
            } else {
                GroupsUsedByElementsVisitorImpl.getGroups(languageObject, groups);
            }
        }
    }
    
    /**
     * @param objects
     * @param groups
     */
    public void findGroups(Collection<? extends BaseLanguageObject> objects, Set<GroupSymbolImpl> groups) {
        // Get groups from elements     
        for (BaseLanguageObject languageObject : objects) {
            if (languageObject instanceof ElementSymbolImpl) {
                ElementSymbolImpl elem = (ElementSymbolImpl) languageObject;
                groups.add(elem.getGroupSymbol());
            } else {
                GroupsUsedByElementsVisitorImpl.getGroups(languageObject, groups);
            }
        }
    }
    
    /**
     * @param obj
     * @param groups
     */
    public void findGroups(BaseLanguageObject obj, Collection<GroupSymbolImpl> groups) {
        Collection<ElementSymbolImpl> elements = ElementCollectorVisitorImpl.getElements(obj, true);

        for (ElementSymbolImpl elementSymbol : elements) {
            if (elementSymbol.getGroupSymbol() != null) {
                groups.add(elementSymbol.getGroupSymbol());  
            }
        }
    }
    
    @Override
    public Set<GroupSymbolImpl> findGroups(BaseLanguageObject obj) {
        Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
        findGroups(obj, groups);
        return groups;
    }

    @Override
    public <T extends BaseLanguageObject> Set<GroupSymbolImpl> findGroups(Collection<T> objects) {
        Set<GroupSymbolImpl> groups = new HashSet<GroupSymbolImpl>();
        findGroups(objects, groups);
        return groups;
    }


}
