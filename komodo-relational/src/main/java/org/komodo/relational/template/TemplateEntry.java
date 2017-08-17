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
package org.komodo.relational.template;

import java.util.Collection;
import java.util.Properties;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.template.internal.TemplateEntryImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A model of a template instance
 */
public interface TemplateEntry extends RelationalObject {

    String CUSTOM_PREFIX = "custom-";

    /**
     * The type identifier.
     */
    int TYPE_ID = TemplateEntry.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.TEMPLATE_ENTRY;

    /**
     * An empty array of templates.
     */
    TemplateEntry[] NO_TEMPLATE_ENTRIES = new TemplateEntry[0];

    /**
     * The resolver of a {@link TemplateEntry}.
     */
    TypeResolver< TemplateEntry > RESOLVER = new TypeResolver< TemplateEntry >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< TemplateEntryImpl > owningClass() {
            return TemplateEntryImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, DataVirtLexicon.TemplateEntry.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public TemplateEntry resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == TemplateEntry.TYPE_ID ) {
                return ( TemplateEntry )kobject;
            }
            return new TemplateEntryImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return id of this template
     * @throws KException
     */
    String getId(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the description
     * @throws KException
     */
    String getDescription(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param description the description
     * @throws KException
     */
    void setDescription(UnitOfWork transaction, String description) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the displayName
     * @throws KException
     */
    String getDisplayName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param displayName the display name
     * @throws KException
     */
    void setDisplayName(UnitOfWork transaction, String displayName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the collection of allowed values
     * @throws KException
     */
    Collection<Object> getAllowedValues(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param allowedValues the collection of allowed values
     * @throws KException
     */
    void setAllowedValues(UnitOfWork transaction, Collection<Object> allowedValues) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the category
     * @throws KException
     */
    String getCategory(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param category the category
     * @throws KException
     */
    void setCategory(UnitOfWork transaction, String category) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the default value
     * @throws KException
     */
    Object getDefaultValue(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param defaultValue the default value
     * @throws KException
     */
    void setDefaultValue(UnitOfWork transaction, Object defaultValue) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the type class name
     * @throws KException
     */
    String getTypeClassName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param propertyTypeClassName the type class name
     * @throws KException
     */
    void setTypeClassName(UnitOfWork transaction, String typeClassName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return whether entry is constrained to allowed values
     * @throws KException
     */
    boolean isConstrainedToAllowedValues(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param constrainedToAllowedValues is the property constrained to allowed values
     * @throws KException
     */
    void setConstrainedToAllowedValues(UnitOfWork transaction, boolean constrainedToAllowedValues) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return whether entry is advanced
     * @throws KException
     */
    boolean isAdvanced(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param advanced is advanced property
     * @throws KException
     */
    void setAdvanced(UnitOfWork transaction, boolean advanced) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return whether entry is masked
     * @throws KException
     */
    boolean isMasked(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param masked is value masked
     * @throws KException
     */
    void setMasked(UnitOfWork transaction, boolean masked) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return whether entry is modifiable
     * @throws KException
     */
    boolean isModifiable(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param modifiable is value modifiable
     * @throws KException
     */
    void setModifiable(UnitOfWork transaction, boolean modifiable) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return whether entry is required
     * @throws KException
     */
    boolean isRequired(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param required is property required
     * @throws KException
     */
    void setRequired(UnitOfWork transaction, boolean required) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return custom properties
     * @throws KException
     */
    Properties getCustomProperties(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param key the property name
     * @param value the property value
     * @throws KException
     */
    void addCustomProperty(UnitOfWork transaction, String key, String value) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @param properties the property values
     * @throws KException
     */
    void setCustomProperties(UnitOfWork transaction, Properties properties) throws KException;

}
