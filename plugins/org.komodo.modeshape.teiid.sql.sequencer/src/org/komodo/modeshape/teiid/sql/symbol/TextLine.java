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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.ITextLine;

public class TextLine extends ASTNode implements Expression, ITextLine<LanguageVisitor> {

    public TextLine(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * @return
     */
    public Character getDelimiter() {
        return null;
    }

    /**
     * @param delimiter
     */
    public void setDelimiter(Character delimiter) {
    }

    /**
     * @return
     */
    public Character getQuote() {
        return null;
    }

    /**
     * @param quote
     */
    public void setQuote(Character quote) {
    }

    /**
     * @return
     */
    public boolean isIncludeHeader() {
        return false;
    }

    /**
     * @param header
     */
    public void setIncludeHeader(boolean header) {
    }

    /**
     * @return
     */
    public List<DerivedColumn> getExpressions() {
        return null;
    }

    /**
     * @param expressions
     */
    public void setExpressions(List<DerivedColumn> expressions) {
    }

    /**
     * @return
     */
    public String getEncoding() {
        return null;
    }

    /**
     * @param encoding
     */
    public void setEncoding(String encoding) {
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
    public void acceptVisitor(LanguageVisitor visitor) {
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
