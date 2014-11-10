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

package org.komodo.modeshape.teiid.sql.symbol;

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.ITextLine;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class TextLine extends ASTNode implements Expression, ITextLine<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public TextLine(TeiidSeqParser p, int id) {
        super(p, id);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5.get()))
            setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, DataTypeName.STRING_ARRAY.name());
        else
            setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, DataTypeName.STRING.name());
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    public Character getDelimiter() {
        Object property = getProperty(TeiidSqlLexicon.TextLine.DELIMITER_PROP_NAME);
        return property == null ? null : property.toString().charAt(0);
    }

    public void setDelimiter(Character delimiter) {
        setProperty(TeiidSqlLexicon.TextLine.DELIMITER_PROP_NAME, delimiter);
    }

    public Character getQuote() {
        Object property = getProperty(TeiidSqlLexicon.TextLine.QUOTE_PROP_NAME);
        return property == null ? null : property.toString().charAt(0);
    }

    public void setQuote(Character quote) {
        setProperty(TeiidSqlLexicon.TextLine.QUOTE_PROP_NAME, quote);
    }

    public boolean isIncludeHeader() {
        Object property = getProperty(TeiidSqlLexicon.TextLine.INCLUDE_HEADER_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setIncludeHeader(boolean header) {
        setProperty(TeiidSqlLexicon.TextLine.INCLUDE_HEADER_PROP_NAME, header);
    }

    public List<DerivedColumn> getExpressions() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.TextLine.EXPRESSIONS_REF_NAME, DerivedColumn.class);
    }

    public void setExpressions(List<DerivedColumn> expressions) {
        setChildren(TeiidSqlLexicon.TextLine.EXPRESSIONS_REF_NAME, expressions);
    }

    public String getEncoding() {
        Object property = getProperty(TeiidSqlLexicon.TextLine.ENCODING_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setEncoding(String encoding) {
        setProperty(TeiidSqlLexicon.TextLine.ENCODING_PROP_NAME, encoding);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getDelimiter() == null) ? 0 : this.getDelimiter().hashCode());
        result = prime * result + ((this.getEncoding() == null) ? 0 : this.getEncoding().hashCode());
        result = prime * result + ((this.getExpressions() == null) ? 0 : this.getExpressions().hashCode());
        result = prime * result + (this.isIncludeHeader() ? 1231 : 1237);
        result = prime * result + ((this.getQuote() == null) ? 0 : this.getQuote().hashCode());
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
        TextLine other = (TextLine)obj;
        if (this.getDelimiter() == null) {
            if (other.getDelimiter() != null)
                return false;
        } else if (!this.getDelimiter().equals(other.getDelimiter()))
            return false;
        if (this.getEncoding() == null) {
            if (other.getEncoding() != null)
                return false;
        } else if (!this.getEncoding().equals(other.getEncoding()))
            return false;
        if (this.getExpressions() == null) {
            if (other.getExpressions() != null)
                return false;
        } else if (!this.getExpressions().equals(other.getExpressions()))
            return false;
        if (this.isIncludeHeader() != other.isIncludeHeader())
            return false;
        if (this.getQuote() == null) {
            if (other.getQuote() != null)
                return false;
        } else if (!this.getQuote().equals(other.getQuote()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public TextLine clone() {
        TextLine clone = new TextLine(this.getTeiidParser(), this.getId());

        if (getDelimiter() != null)
            clone.setDelimiter(getDelimiter());
        if (getEncoding() != null)
            clone.setEncoding(getEncoding());
        if (getQuote() != null)
            clone.setQuote(getQuote());
        clone.setIncludeHeader(isIncludeHeader());
        if (getExpressions() != null)
            clone.setExpressions(cloneList(getExpressions()));

        return clone;
    }

}
