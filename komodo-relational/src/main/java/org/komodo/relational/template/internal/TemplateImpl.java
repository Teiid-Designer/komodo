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
package org.komodo.relational.template.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Implementation of template instance model
 */
public class TemplateImpl extends RelationalObjectImpl implements Template, EventManager {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public TemplateImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Template.IDENTIFIER;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid id property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getId( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property prop = getRawProperty( transaction, JcrLexicon.UUID.getString() );
        final String result = prop.getStringValue( transaction );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public boolean isJdbc(UnitOfWork uow) throws KException {
        List<TemplateEntry> entries = getEntries(uow, Template.CONN_FACTORY_CLASS_KEY);
        return entries.isEmpty();
    }

    @Override
    public TemplateEntry addEntry( final UnitOfWork transaction, final String entryName ) throws KException {
        return RelationalModelFactory.createTemplateEntry( transaction, getRepository(), this, entryName );
    }

    @Override
    public List<TemplateEntry> getEntries(final UnitOfWork transaction, String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final List<TemplateEntry> result = new ArrayList<TemplateEntry>();
        for (final KomodoObject kobject : getChildrenOfType(transaction, DataVirtLexicon.TemplateEntry.NODE_TYPE, namePatterns)) {
            TemplateEntry entry = new TemplateEntryImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(entry);
        }

        if (result.isEmpty()) {
            return Collections.emptyList();
        }

        return result;
    }

    @Override
    public boolean addListener(ExecutionConfigurationListener listener) {
        return false;
    }

    @Override
    public void permitListeners(boolean enable) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners(ExecutionConfigurationEvent event) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener(ExecutionConfigurationListener listener) {
        return false;
    }
}
