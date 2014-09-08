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

package org.komodo.modeshape.teiid.sql.proc;

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.proc.ITriggerAction;

public class TriggerAction extends Command implements ITriggerAction<Expression, LanguageVisitor> {

    public TriggerAction(TeiidParser p, int id) {
        super(p, id);
        setType(TYPE_TRIGGER_ACTION);
    }

    public Block getBlock() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.TriggerAction.BLOCK_REF_NAME, Block.class);
    }

    public void setBlock(Block block) {
        setChild(TeiidSqlLexicon.TriggerAction.BLOCK_REF_NAME, block);
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getBlock() == null) ? 0 : this.getBlock().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        TriggerAction other = (TriggerAction)obj;
        if (this.getBlock() == null) {
            if (other.getBlock() != null) return false;
        } else if (!this.getBlock().equals(other.getBlock())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public TriggerAction clone() {
        TriggerAction clone = new TriggerAction(this.getTeiidParser(), this.getId());

        if(getBlock() != null)
            clone.setBlock(getBlock().clone());
        if(getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if(getOption() != null)
            clone.setOption(getOption().clone());

        return clone;
    }

}
