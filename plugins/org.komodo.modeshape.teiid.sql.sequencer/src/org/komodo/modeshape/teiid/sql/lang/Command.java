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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ICommand;

/**
 *
 */
public abstract class Command extends ASTNode implements ICommand<Expression, LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public Command(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return true if command has been resolved
     */
    @Override
    public boolean isResolved() {
        return false;
    }

    /**
     * This command is intended to only be used by the QueryResolver.
     * @param isResolved whether this command is resolved or not
     */
    public void setIsResolved(boolean isResolved) {
    }

    /**
     * @return null if unknown, empty if results are not returned, or the resultset columns
     */
    @Override
    public List<? extends Expression> getResultSetColumns() {
        return null;
    }

    /**
     * Get the option clause for the query.
     * @return option clause
     */
    @Override
    public Option getOption() {
        return null;
    }

    /**
     * Set the option clause for the query.
     * @param option New option clause
     */
    public void setOption(Option option) {
    }

    /**
     * @return the sourceHint
     */
    public SourceHint getSourceHint() {
        return null;
    }

    /**
     * @param sourceHint the sourceHint to set
     */
    public void setSourceHint(SourceHint sourceHint) {
    }

    /**
     * @return whether this object returns a result set
     */
    public boolean returnsResultSet() {
        return false;
    }

    /**
     * @return singleton update symbol which is lazily created
     */
    public List<Expression> getUpdateCommandSymbol() {
        return null;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isResolved() ? 1231 : 1237);
        result = prime * result + ((this.getOption() == null) ? 0 : this.getOption().hashCode());
        result = prime * result + ((this.getSourceHint() == null) ? 0 : this.getSourceHint().hashCode());
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
        Command other = (Command)obj;
        if (this.isResolved() != other.isResolved())
            return false;
        if (this.getOption() == null) {
            if (other.getOption() != null)
                return false;
        } else if (!this.getOption().equals(other.getOption()))
            return false;
        if (this.getSourceHint() == null) {
            if (other.getSourceHint() != null)
                return false;
        } else if (!this.getSourceHint().equals(other.getSourceHint()))
            return false;
        return true;
    }

    /**
     * @param clone
     */
    protected void copyMetadataState(Command clone) {
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public abstract Command clone();
}
