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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.proc.TriggerAction;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.IAlterTrigger;

/**
 *
 */
public class AlterTrigger extends Alter<TriggerAction> implements IAlterTrigger<Expression, LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public AlterTrigger(ITeiidParser p, int id) {
        super(p, id);
        setType(TYPE_ALTER_TRIGGER);
    }

    /**
     * @return the event
     */
    public TriggerEvent getEvent() {
        Object property = getProperty(TeiidSqlLexicon.AlterTrigger.EVENT_PROP_NAME);
        return TriggerEvent.findTriggerEvent(property.toString());
    }

    /**
     * @param event the event to set
     */
    public void setEvent(TriggerEvent event) {
        setProperty(TeiidSqlLexicon.AlterTrigger.EVENT_PROP_NAME, event.name());
    }

    /**
     * @return the create
     */
    public boolean isCreate() {
        Object property = getProperty(TeiidSqlLexicon.AlterTrigger.CREATE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param create the create to set
     */
    public void setCreate(boolean create) {
        setProperty(TeiidSqlLexicon.AlterTrigger.CREATE_PROP_NAME, create);
    }

    /**
     * @return the enabled
     */
    public Boolean getEnabled() {
        Object property = getProperty(TeiidSqlLexicon.AlterTrigger.ENABLED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param enabled the enabled to set
     */
    public void setEnabled(Boolean enabled) {
        setProperty(TeiidSqlLexicon.AlterTrigger.ENABLED_PROP_NAME, enabled);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isCreate() ? 1231 : 1237);
        result = prime * result + ((this.getEnabled() == null) ? 0 : this.getEnabled().hashCode());
        result = prime * result + ((this.getEvent() == null) ? 0 : this.getEvent().hashCode());
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
        AlterTrigger other = (AlterTrigger)obj;
        if (this.isCreate() != other.isCreate())
            return false;
        if (this.getEnabled() == null) {
            if (other.getEnabled() != null)
                return false;
        } else if (!this.getEnabled().equals(other.getEnabled()))
            return false;
        if (this.getEvent() != other.getEvent())
            return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public AlterTrigger clone() {
        AlterTrigger clone = new AlterTrigger(this.getTeiidParser(), this.getId());

        if (getDefinition() != null)
            clone.setDefinition(getDefinition().clone());
        if (getEvent() != null)
            clone.setEvent(getEvent());
        clone.setCreate(isCreate());
        clone.setEnabled(getEnabled());
        if (getTarget() != null)
            clone.setTarget(getTarget().clone());
        if (getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if (getOption() != null)
            clone.setOption(getOption().clone());

        copyMetadataState(clone);

        return clone;
    }

}
