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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.spi.query.sql.lang.IJoinType;

/**
 *
 */
public class JoinType extends ASTNode implements IJoinType<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public JoinType(TeiidSeqParser p, int id) {
        super(p, id);
    }

    /**
     * @return kind
     */
    public Types getKind() {
        Object property = getProperty(TeiidSqlLexicon.JoinType.KIND_PROP_NAME);
        return property == null ? Types.JOIN_CROSS : Types.findType(property.toString());
    }

    /**
     * @param kind value
     */
    public void setKind(Types kind) {
        setProperty(TeiidSqlLexicon.JoinType.KIND_PROP_NAME, kind.name());
    }

    @Override
    public int getTypeCode() {
        Types kind = getKind();
        if (kind == null)
            return Types.JOIN_CROSS.getTypeCode();

        return kind.getTypeCode();
    }

    @Override
    public boolean isOuter() {
        Types kind = getKind();
        if (kind == null)
            return Types.JOIN_CROSS.isOuter();

        return kind.isOuter();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getKind() == null) ? 0 : this.getKind().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        JoinType other = (JoinType)obj;
        if (this.getKind() != other.getKind()) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public JoinType clone() {
        JoinType clone = new JoinType(this.getTeiidParser(), this.getId());

        if (getKind() != null)
            clone.setKind(getKind());

        return clone;
    }

}
