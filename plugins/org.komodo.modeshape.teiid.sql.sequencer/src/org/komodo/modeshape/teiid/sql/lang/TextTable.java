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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ITextTable;

/**
 *
 */
public class TextTable extends TableFunctionReference implements ITextTable<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public TextTable(TeiidSeqParser p, int id) {
        super(p, id);
        setUsingRowDelimiter(true);
    }

    public Expression getFile() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.TextTable.FILE_REF_NAME, Expression.class);
    }

    public void setFile(Expression file) {
        setChild(TeiidSqlLexicon.TextTable.FILE_REF_NAME, file);
    }

    @Override
    public List<TextColumn> getColumns() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.TextTable.COLUMNS_REF_NAME, TextColumn.class);
    }

    public void setColumns(List<TextColumn> columns) {
        setChildren(TeiidSqlLexicon.TextTable.COLUMNS_REF_NAME, columns);
    }

    public Character getDelimiter() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.DELIMITER_PROP_NAME);
        return property == null ? null : property.toString().charAt(0);
    }

    public void setDelimiter(Character delimiter) {
        setProperty(TeiidSqlLexicon.TextTable.DELIMITER_PROP_NAME, delimiter);
    }

    public boolean isEscape() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.ESCAPE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setEscape(boolean escape) {
        setProperty(TeiidSqlLexicon.TextTable.ESCAPE_PROP_NAME, escape);
    }

    public Integer getHeader() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.HEADER_PROP_NAME);
        return property == null ? null : Integer.parseInt(property.toString());
    }

    public void setHeader(Integer header) {
        setProperty(TeiidSqlLexicon.TextTable.HEADER_PROP_NAME, header);
    }

    public Integer getSkip() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.SKIP_PROP_NAME);
        return property == null ? null : Integer.parseInt(property.toString());
    }

    public void setSkip(Integer skip) {
        setProperty(TeiidSqlLexicon.TextTable.SKIP_PROP_NAME, skip);
    }

    public Character getQuote() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.QUOTE_PROP_NAME);
        return property == null ? null : property.toString().charAt(0);
    }

    public void setQuote(Character quote) {
        setProperty(TeiidSqlLexicon.TextTable.QUOTE_PROP_NAME, quote);
    }

    public boolean isUsingRowDelimiter() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.USING_ROW_DELIMITER_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setUsingRowDelimiter(boolean useRowDelimiter) {
        setProperty(TeiidSqlLexicon.TextTable.USING_ROW_DELIMITER_PROP_NAME, useRowDelimiter);
    }

    public String getSelector() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.SELECTOR_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setSelector(String selector) {
        setProperty(TeiidSqlLexicon.TextTable.SELECTOR_PROP_NAME, selector);
    }

    public boolean isFixedWidth() {
        Object property = getProperty(TeiidSqlLexicon.TextTable.FIXED_WIDTH_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setFixedWidth(boolean fixedWidth) {
        setProperty(TeiidSqlLexicon.TextTable.FIXED_WIDTH_PROP_NAME, fixedWidth);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getColumns() == null) ? 0 : this.getColumns().hashCode());
        result = prime * result + ((this.getDelimiter() == null) ? 0 : this.getDelimiter().hashCode());
        result = prime * result + (this.isEscape() ? 1231 : 1237);
        result = prime * result + ((this.getFile() == null) ? 0 : this.getFile().hashCode());
        result = prime * result + (this.isFixedWidth() ? 1231 : 1237);
        result = prime * result + ((this.getHeader() == null) ? 0 : this.getHeader().hashCode());
        result = prime * result + ((this.getQuote() == null) ? 0 : this.getQuote().hashCode());
        result = prime * result + ((this.getSelector() == null) ? 0 : this.getSelector().hashCode());
        result = prime * result + ((this.getSkip() == null) ? 0 : this.getSkip().hashCode());
        result = prime * result + (this.isUsingRowDelimiter() ? 1231 : 1237);
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
        TextTable other = (TextTable)obj;
        if (this.getColumns() == null) {
            if (other.getColumns() != null)
                return false;
        } else if (!this.getColumns().equals(other.getColumns()))
            return false;
        if (this.getDelimiter() == null) {
            if (other.getDelimiter() != null)
                return false;
        } else if (!this.getDelimiter().equals(other.getDelimiter()))
            return false;
        if (this.isEscape() != other.isEscape())
            return false;
        if (this.getFile() == null) {
            if (other.getFile() != null)
                return false;
        } else if (!this.getFile().equals(other.getFile()))
            return false;
        if (this.isFixedWidth() != other.isFixedWidth())
            return false;
        if (this.getHeader() == null) {
            if (other.getHeader() != null)
                return false;
        } else if (!this.getHeader().equals(other.getHeader()))
            return false;
        if (this.getQuote() == null) {
            if (other.getQuote() != null)
                return false;
        } else if (!this.getQuote().equals(other.getQuote()))
            return false;
        if (this.getSelector() == null) {
            if (other.getSelector() != null)
                return false;
        } else if (!this.getSelector().equals(other.getSelector()))
            return false;
        if (this.getSkip() == null) {
            if (other.getSkip() != null)
                return false;
        } else if (!this.getSkip().equals(other.getSkip()))
            return false;
        if (this.isUsingRowDelimiter() != other.isUsingRowDelimiter())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public TextTable clone() {
        TextTable clone = new TextTable(this.getTeiidParser(), this.getId());

        if (getColumns() != null)
            clone.setColumns(cloneList(getColumns()));
        if (getSelector() != null)
            clone.setSelector(getSelector());
        if (getDelimiter() != null)
            clone.setDelimiter(getDelimiter());
        if (getQuote() != null)
            clone.setQuote(getQuote());
        if (getFile() != null)
            clone.setFile(getFile().clone());
        clone.setEscape(isEscape());
        if (getHeader() != null)
            clone.setHeader(getHeader());
        if (getSkip() != null)
            clone.setSkip(getSkip());
        clone.setUsingRowDelimiter(isUsingRowDelimiter());
        clone.setFixedWidth(isFixedWidth());
        if (getName() != null)
            clone.setName(getName());
        clone.setOptional(isOptional());
        clone.setMakeInd(isMakeInd());
        clone.setNoUnnest(isNoUnnest());
        if (getMakeDependency() != null)
            clone.setMakeDependency(getMakeDependency().clone());
        clone.setMakeNotDep(isMakeNotDep());
        clone.setPreserve(isPreserve());

        return clone;
    }

}
