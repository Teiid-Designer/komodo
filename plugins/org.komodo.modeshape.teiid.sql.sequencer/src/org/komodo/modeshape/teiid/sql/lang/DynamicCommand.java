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
import java.util.Collections;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IDynamicCommand;

/**
 * Dynamic Command object
 */
public class DynamicCommand extends Command implements IDynamicCommand<Expression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public DynamicCommand(TeiidSeqParser p, int id) {
        super(p, id);
        setType(TYPE_DYNAMIC);
    }

    /** 
     * @return Returns the columns.
     */
    public List<ElementSymbol> getAsColumns() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.DynamicCommand.AS_COLUMNS_REF_NAME, ElementSymbol.class);
    }

    /** 
     * @param columns The columns to set.
     */
    public void setAsColumns(List<ElementSymbol> columns) {
        setChildren(TeiidSqlLexicon.DynamicCommand.AS_COLUMNS_REF_NAME, columns);
    }
    
    /** 
     * @return Returns the intoGroup.
     */
    public GroupSymbol getIntoGroup() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.DynamicCommand.INTO_GROUP_REF_NAME, GroupSymbol.class);
    }
    
    /** 
     * @param intoGroup The intoGroup to set.
     */
    public void setIntoGroup(GroupSymbol intoGroup) {
        setChild(TeiidSqlLexicon.DynamicCommand.INTO_GROUP_REF_NAME, intoGroup);
    }
    
    /** 
     * @return Returns the sql.
     */
    public Expression getSql() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.DynamicCommand.SQL_REF_NAME, Expression.class);
    }
    
    /** 
     * @param sql The sql to set.
     */
    public void setSql(Expression sql) {
        setChild(TeiidSqlLexicon.DynamicCommand.SQL_REF_NAME, sql);
    }
            
    /** 
     * @return Returns the using.
     */
    public SetClauseList getUsing() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.DynamicCommand.USING_REF_NAME, SetClauseList.class);
    }
    
    /** 
     * @param using The using to set.
     */
    public void setUsing(SetClauseList using) {
        setChild(TeiidSqlLexicon.DynamicCommand.USING_REF_NAME, using);
    }

    /** 
     * @return Returns the asClauseSet.
     */
    public boolean isAsClauseSet() {
        Object property = getProperty(TeiidSqlLexicon.DynamicCommand.AS_CLAUSE_SET_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /** 
     * @param asClauseSet The asClauseSet to set.
     */
    public void setAsClauseSet(boolean asClauseSet) {
        setProperty(TeiidSqlLexicon.DynamicCommand.AS_CLAUSE_SET_PROP_NAME, asClauseSet);
    }

    /**
     * @return updating model count
     */
    public int getUpdatingModelCount() {
        Object property = getProperty(TeiidSqlLexicon.DynamicCommand.UPDATING_MODEL_COUNT_PROP_NAME);
        return property == null ? 0 : Integer.parseInt(property.toString());
    }

    /**
     * @param count value
     */
    public void setUpdatingModelCount(int count) {
        if (count < 0) {
            count = 0;
        } else if (count > 2) {
            count = 2;
        }

        setProperty(TeiidSqlLexicon.DynamicCommand.UPDATING_MODEL_COUNT_PROP_NAME, count);
    }

    @Override
    public boolean returnsResultSet() {
        return getIntoGroup() == null;
    }

    /** 
     * Once past resolving, an EMPTY set of project columns indicates that the
     * project columns of the actual command do not need to be checked during
     * processing.
     */
    @Override
    public List<Expression> getProjectedSymbols() {
        if (getIntoGroup() != null) {
            return getUpdateCommandSymbol();
        }
        
        if (getAsColumns() != null) {
            List<Expression> ps = new ArrayList<Expression>();
            for (ElementSymbol es : getAsColumns()) {
                ps.add(es);
            }
            return ps;
        }
        
        return Collections.EMPTY_LIST;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isAsClauseSet() ? 1231 : 1237);
        result = prime * result + ((this.getAsColumns() == null) ? 0 : this.getAsColumns().hashCode());
        result = prime * result + ((this.getIntoGroup() == null) ? 0 : this.getIntoGroup().hashCode());
        result = prime * result + ((this.getSql() == null) ? 0 : this.getSql().hashCode());
        result = prime * result + this.getUpdatingModelCount();
        result = prime * result + ((this.getUsing() == null) ? 0 : this.getUsing().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        DynamicCommand other = (DynamicCommand)obj;
        if (this.isAsClauseSet() != other.isAsClauseSet())
            return false;
        if (this.getAsColumns() == null) {
            if (other.getAsColumns() != null)
                return false;
        } else if (!this.getAsColumns().equals(other.getAsColumns()))
            return false;
        if (this.getIntoGroup() == null) {
            if (other.getIntoGroup() != null)
                return false;
        } else if (!this.getIntoGroup().equals(other.getIntoGroup()))
            return false;
        if (this.getSql() == null) {
            if (other.getSql() != null)
                return false;
        } else if (!this.getSql().equals(other.getSql()))
            return false;
        if (this.getUpdatingModelCount() != other.getUpdatingModelCount())
            return false;
        if (this.getUsing() == null) {
            if (other.getUsing() != null)
                return false;
        } else if (!this.getUsing().equals(other.getUsing()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public DynamicCommand clone() {
        DynamicCommand clone = new DynamicCommand(this.getTeiidParser(), this.getId());

        if (getAsColumns() != null && !getAsColumns().isEmpty())
            clone.setAsColumns(cloneList(getAsColumns()));
        if (getIntoGroup() != null)
            clone.setIntoGroup(getIntoGroup().clone());
        if (getSql() != null)
            clone.setSql(getSql().clone());
        if (getUsing() != null)
            clone.setUsing(getUsing().clone());
        clone.setAsClauseSet(isAsClauseSet());
        clone.setUpdatingModelCount(getUpdatingModelCount());
        if (getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if (getOption() != null)
            clone.setOption(getOption().clone());

        copyMetadataState(clone);

        return clone;
    }

}
