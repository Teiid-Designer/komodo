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
package org.komodo.relational.dataservice.internal;

import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.DataServiceEntry;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.dataservice.DataServiceEntry.PublishPolicy;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

public abstract class AbstractDataServiceEntryImpl<T extends Exportable & RelationalObject> extends RelationalObjectImpl
    implements DataServiceEntry<T> {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    public AbstractDataServiceEntryImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export(final UnitOfWork transaction, final Properties properties) throws KException {
        final T resource = getReference(transaction);

        if (resource == null) {
            if (getPublishPolicy(transaction) != PublishPolicy.NEVER) {
                throw new KException(Messages.getString(Relational.EXPORT_FAILED_NO_CONTENT, getAbsolutePath()));
            }

            return NO_CONTENT;
        }

        return resource.export(transaction, properties);
    }

    @Override
    public DocumentType getDocumentType(final UnitOfWork transaction) throws KException {
        final T ref = getReference(transaction);

        if (ref == null) {
            return DocumentType.UNKNOWN;
        }

        return ref.getDocumentType(transaction);
    }

    @Override
    public String getEntryPath(final UnitOfWork transaction) throws KException {
        if (hasProperty(transaction, DataVirtLexicon.ResourceEntry.PATH)) {
            return getProperty(transaction, DataVirtLexicon.ResourceEntry.PATH).getStringValue(transaction);
        }

        final T file = getReference(transaction);
        String folder = getArchiveFolder();

        if (StringUtils.isBlank(folder)) {
            if (folder == null) {
                folder = StringConstants.EMPTY_STRING;
            }
        } else if (!folder.endsWith(StringConstants.FORWARD_SLASH)) {
            folder += StringConstants.FORWARD_SLASH;
        }

        if (file != null) {
            return (folder + file.getDocumentType(transaction).fileName(file.getName(transaction)));
        }

        return (folder + getName(transaction));
    }

    @Override
    public PublishPolicy getPublishPolicy(final UnitOfWork transaction) throws KException {
        if (hasProperty(transaction, DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY)) {
            final String value = getProperty(transaction,
                                             DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY).getStringValue(transaction);
            return PublishPolicy.valueOf(value);
        }

        return PublishPolicy.DEFAULT;
    }

    @Override
    public void setEntryPath(final UnitOfWork transaction, final String newEntryPath) throws KException {
        setProperty(transaction, DataVirtLexicon.DataServiceEntry.PATH, newEntryPath);
    }

    @Override
    public void setPublishPolicy(final UnitOfWork transaction, final PublishPolicy newPublishPolicy) throws KException {
        String value = ((newPublishPolicy == null) ? null : newPublishPolicy.name());
        setProperty(transaction, DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY, value);
    }

    @Override
    public void setReference(final UnitOfWork transaction, final T reference) throws KException {
        String refId = null;

        if (reference != null) {
            Property uuidProperty = reference.getRawProperty(transaction, JcrLexicon.UUID.getString());
            if (uuidProperty == null) {
                String msg = Messages.getString(Messages.Relational.NO_UUID_PROPERTY, reference.getName(transaction));
                throw new KException(msg);
            }

            refId = uuidProperty.getStringValue(transaction);
        }

        setProperty(transaction, DataVirtLexicon.DataServiceEntry.SOURCE_RESOURCE, refId);
    }
}
