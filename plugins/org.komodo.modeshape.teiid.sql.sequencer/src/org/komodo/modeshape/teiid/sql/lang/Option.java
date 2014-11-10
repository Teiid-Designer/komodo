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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants;
import org.komodo.spi.query.sql.lang.IOption;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class Option extends ASTNode implements IOption<SQLanguageVisitorImpl>, TeiidSQLConstants.Reserved {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public Option(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Collection<String> getDependentGroups() {
        Collection<Object> properties = getProperties(TeiidSqlLexicon.Option.DEPENDENT_GROUPS_PROP_NAME);
        if (properties == null)
            return null;

        List<String> dps = new ArrayList<String>();
        for (Object o : properties)
            dps.add(o.toString());

        return dps;
    }

    public void addDependentGroup(String groupId) {
        addProperty(TeiidSqlLexicon.Option.DEPENDENT_GROUPS_PROP_NAME, groupId);
        /*
         * group options should remain in sync with the collection of dependency groupIds
         */
        MakeDep groupOption = getTeiidParser().createASTNode(ASTNodes.MAKE_DEP);
        addGroupOption(groupOption);
    }

    public void setDependentGroups(Collection<String> groupIds) {
        setProperty(TeiidSqlLexicon.Option.DEPENDENT_GROUPS_PROP_NAME, groupIds);
        /*
         * group options should remain in sync with the collection of dependency groupIds
         */
        for (int i = 0; i < groupIds.size(); ++i) {
            MakeDep groupOption = getTeiidParser().createASTNode(ASTNodes.MAKE_DEP);
            addGroupOption(groupOption);
        }
    }

    /*
     * group options should remain in sync with the collection of dependency groupIds
     */
    private void addGroupOption(MakeDep groupOption) {
        addLastChild(TeiidSqlLexicon.Option.DEPENDENT_GROUP_OPTIONS_REF_NAME, groupOption);
    }

    public void setDependentGroupOptions(Collection<MakeDep> groupOptions) {
        setChildren(TeiidSqlLexicon.Option.DEPENDENT_GROUP_OPTIONS_REF_NAME, groupOptions);
    }

    @Override
    public Collection<String> getNotDependentGroups() {
        Collection<Object> properties = getProperties(TeiidSqlLexicon.Option.NOT_DEPENDENT_GROUPS_PROP_NAME);
        if (properties == null)
            return null;

        List<String> dps = new ArrayList<String>();
        for (Object o : properties)
            dps.add(o.toString());

        return dps;
    }

    public void addNotDependentGroup(String groupId) {
        addProperty(TeiidSqlLexicon.Option.NOT_DEPENDENT_GROUPS_PROP_NAME, groupId);
    }

    public void setNotDependentGroups(Collection<String> groupIds) {
        setProperty(TeiidSqlLexicon.Option.NOT_DEPENDENT_GROUPS_PROP_NAME, groupIds);  
    }

    @Override
    public boolean isNoCache() {
        Object property = getProperty(TeiidSqlLexicon.Option.NO_CACHE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setNoCache(boolean noCache) {
        setProperty(TeiidSqlLexicon.Option.NO_CACHE_PROP_NAME, noCache);
    }

    @Override
    public Collection<String> getNoCacheGroups() {
        Collection<Object> properties = getProperties(TeiidSqlLexicon.Option.NO_CACHE_GROUPS_PROP_NAME);
        if (properties == null)
            return null;

        List<String> dps = new ArrayList<String>();
        for (Object o : properties)
            dps.add(o.toString());

        return dps;        
    }

    public void addNoCacheGroup(String groupId) {
        addProperty(TeiidSqlLexicon.Option.NO_CACHE_GROUPS_PROP_NAME, groupId);
    }

    public void setNoCacheGroups(Collection<String> groupIds) {
        setProperty(TeiidSqlLexicon.Option.NO_CACHE_GROUPS_PROP_NAME, groupIds);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getDependentGroups() == null) ? 0 : this.getDependentGroups().hashCode());
        result = prime * result + ((this.getNotDependentGroups() == null) ? 0 : this.getNotDependentGroups().hashCode());
        result = prime * result + (this.isNoCache() ? 1231 : 1237);
        result = prime * result + ((this.getNoCacheGroups() == null) ? 0 : this.getNoCacheGroups().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Option other = (Option)obj;
        if (this.getDependentGroups() == null) {
            if (other.getDependentGroups() != null) return false;
        } else if (!this.getDependentGroups().equals(other.getDependentGroups())) return false;
        if (this.getNotDependentGroups() == null) {
            if (other.getNotDependentGroups() != null) return false;
        } else if (!this.getNotDependentGroups().equals(other.getNotDependentGroups())) return false;
        if (this.isNoCache() != other.isNoCache()) return false;
        if (this.getNoCacheGroups() == null) {
            if (other.getNoCacheGroups() != null) return false;
        } else if (!this.getNoCacheGroups().equals(other.getNoCacheGroups())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public Option clone() {
        Option clone = new Option(this.getTeiidParser(), this.getId());

        clone.setNoCache(isNoCache());

        if (this.getDependentGroups() != null) {
            for (String group : getDependentGroups()) {
                clone.addDependentGroup(group);
            }
        }

        if (getNotDependentGroups() != null) {
            for (String group : getNotDependentGroups()) {
                clone.addNotDependentGroup(group);
            }
        }

        if (getNoCacheGroups() != null) {
            for (String group : getNoCacheGroups()) {
                clone.addNoCacheGroup(group);
            }
        }

        return clone;
    }

}
