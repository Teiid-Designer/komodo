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

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.DerivedColumn;
import org.komodo.spi.query.sql.lang.IObjectTable;

public class ObjectTable extends TableFunctionReference implements IObjectTable<LanguageVisitor> {

    public ObjectTable(TeiidParser p, int id) {
        super(p, id);
    }

    public String getRowScript() {
        Object property = getProperty(TeiidSqlLexicon.ObjectTable.ROW_SCRIPT_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setRowScript(String rowScript) {
        setProperty(TeiidSqlLexicon.ObjectTable.ROW_SCRIPT_PROP_NAME, rowScript);
    }

    public List<DerivedColumn> getPassing() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.ObjectTable.PASSING_REF_NAME, DerivedColumn.class);
    }

    public void setPassing(List<DerivedColumn> passingValues) {
        setChildren(TeiidSqlLexicon.ObjectTable.PASSING_REF_NAME, passingValues);
    }

    @Override
    public List<ObjectColumn> getColumns() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.ObjectTable.COLUMNS_REF_NAME, ObjectColumn.class);
    }

    public void setColumns(List<ObjectColumn> columns) {
    }

    public String getScriptingLanguage() {
        Object property = getProperty(TeiidSqlLexicon.ObjectTable.SCRIPTING_LANGUAGE_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setScriptingLanguage(String lang) {
        setProperty(TeiidSqlLexicon.ObjectTable.SCRIPTING_LANGUAGE_PROP_NAME, lang);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getColumns() == null) ? 0 : this.getColumns().hashCode());
        result = prime * result + ((this.getPassing() == null) ? 0 : this.getPassing().hashCode());
        result = prime * result + ((this.getRowScript() == null) ? 0 : this.getRowScript().hashCode());
        result = prime * result + ((this.getScriptingLanguage() == null) ? 0 : this.getScriptingLanguage().hashCode());
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
        ObjectTable other = (ObjectTable)obj;
        if (this.getColumns() == null) {
            if (other.getColumns() != null)
                return false;
        } else if (!this.getColumns().equals(other.getColumns()))
            return false;
        if (this.getPassing() == null) {
            if (other.getPassing() != null)
                return false;
        } else if (!this.getPassing().equals(other.getPassing()))
            return false;
        if (this.getRowScript() == null) {
            if (other.getRowScript() != null)
                return false;
        } else if (!this.getRowScript().equals(other.getRowScript()))
            return false;
        if (this.getScriptingLanguage() == null) {
            if (other.getScriptingLanguage() != null)
                return false;
        } else if (!this.getScriptingLanguage().equals(other.getScriptingLanguage()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ObjectTable clone() {
        ObjectTable clone = new ObjectTable(this.getTeiidParser(), this.getId());

        if(getColumns() != null)
            clone.setColumns(cloneList(getColumns()));
        if(getScriptingLanguage() != null)
            clone.setScriptingLanguage(getScriptingLanguage());
        if(getPassing() != null)
            clone.setPassing(cloneList(getPassing()));
        if(getRowScript() != null)
            clone.setRowScript(getRowScript());
        if(getName() != null)
            clone.setName(getName());
        clone.setOptional(isOptional());
        clone.setMakeInd(isMakeInd());
        clone.setNoUnnest(isNoUnnest());
        clone.setMakeDep(isMakeDep());
        clone.setMakeNotDep(isMakeNotDep());
        clone.setPreserve(isPreserve());

        return clone;
    }

}
