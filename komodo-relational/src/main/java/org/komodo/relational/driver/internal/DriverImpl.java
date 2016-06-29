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
package org.komodo.relational.driver.internal;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.driver.Driver;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.FileUtils;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrNtLexicon;

/**
 * Implementation of driver instance model
 */
public class DriverImpl extends RelationalObjectImpl implements Driver {

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
    public DriverImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Driver.IDENTIFIER;
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.driver.Driver#getContent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public InputStream getContent( final UnitOfWork uow ) throws KException {
        if (! hasChild(uow, JcrLexicon.CONTENT.getString()))
            return null;

        KomodoObject fileNode = getChild(uow, JcrLexicon.CONTENT.getString(), JcrNtLexicon.UNSTRUCTURED.getString());
        Property property = fileNode.getProperty(uow, JcrLexicon.DATA.getString());
        return property.getBinaryValue(uow);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.driver.Driver#setDriverName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setContent( final UnitOfWork uow, final byte[] content ) throws KException {
        KomodoObject fileNode = null;

        if (! hasChild(uow, JcrLexicon.CONTENT.getString()))
            fileNode = addChild(uow, JcrLexicon.CONTENT.getString(), null);
        else
            fileNode = getChild(uow, JcrLexicon.CONTENT.getString());

        ByteArrayInputStream stream = new ByteArrayInputStream(content);
        fileNode.setProperty(uow, JcrLexicon.DATA.getString(), stream);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export(UnitOfWork transaction, Properties exportProperties) throws KException {
        InputStream stream = getContent(transaction);

        byte[] contents = null;;
        try {
            contents = FileUtils.write(stream);
        } catch (IOException e) {
            handleError(e);
        }

        return contents;
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
        String name = getName(transaction);
        return DocumentType.createDocumentType(name);
    }
}
