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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.spi.query.sql.lang.ITextColumn;

public class TextColumn extends ProjectedColumn implements ITextColumn<LanguageVisitor> {

    public TextColumn(TeiidParser p, int id) {
        super(p, id);
    }

    public String getName() {
        throw new UnsupportedOperationException();
    }

    public void setName(String name) {
    }

    /**
     * @return
     */
    public boolean isOrdinal() {
        return false;
    }

    /**
     * @param b
     */
    public void setOrdinal(boolean b) {
    }

    /**
     * @return
     */
    public Integer getWidth() {
        return null;
    }

    /**
     * @param width
     */
    public void setWidth(Integer width) {
    }

    /**
     * @return
     */
    public boolean isNoTrim() {
        return false;
    }

    /**
     * @param noTrim
     */
    public void setNoTrim(boolean noTrim) {
    }

    /**
     * @return
     */
    public String getSelector() {
        return null;
    }

    /**
     * @param selector
     */
    public void setSelector(String selector) {
    }

    /**
     * @return
     */
    public Integer getPosition() {
        return null;
    }

    /**
     * @param position
     */
    public void setPosition(Integer position) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isNoTrim() ? 1231 : 1237);
        result = prime * result + ((this.getPosition() == null) ? 0 : this.getPosition().hashCode());
        result = prime * result + ((this.getSelector() == null) ? 0 : this.getSelector().hashCode());
        result = prime * result + ((this.getWidth() == null) ? 0 : this.getWidth().hashCode());
        result = prime * result + (this.isOrdinal() ? 1231 : 1237);
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
        TextColumn other = (TextColumn)obj;
        if (this.isNoTrim() != other.isNoTrim())
            return false;
        if (this.getPosition() == null) {
            if (other.getPosition() != null)
                return false;
        } else if (!this.getPosition().equals(other.getPosition()))
            return false;
        if (this.getSelector() == null) {
            if (other.getSelector() != null)
                return false;
        } else if (!this.getSelector().equals(other.getSelector()))
            return false;
        if (this.getWidth() == null) {
            if (other.getWidth() != null)
                return false;
        } else if (!this.getWidth().equals(other.getWidth()))
            return false;
        if (this.isOrdinal() != other.isOrdinal())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public TextColumn clone() {
        TextColumn clone = new TextColumn(this.getTeiidParser(), this.getId());

        if (getSelector() != null)
            clone.setSelector(getSelector());
        if (getWidth() != null)
            clone.setWidth(getWidth());
        clone.setNoTrim(isNoTrim());
        if (getPosition() != null)
            clone.setPosition(getPosition());
        if (getName() != null)
            clone.setName(getName());
        if (getType() != null)
            clone.setType(getType());
        clone.setOrdinal(isOrdinal());

        return clone;
    }

}
