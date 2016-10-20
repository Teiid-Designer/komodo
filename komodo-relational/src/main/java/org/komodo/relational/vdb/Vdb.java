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
package org.komodo.relational.vdb;

import java.util.Properties;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.Model;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;

/**
 * Represents a virtual database manifest.
 */
public interface Vdb extends Exportable, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Vdb.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB;

    /**
     * Represents a VDB XML manifest file.
     */
    public interface VdbManifest extends Exportable {

        /**
         * @return the manifest as an XML document (never <code>null</code>)
         * @throws KException
         *         if an error occurs
         */
        Document asDocument() throws KException;

    }

    /**
     * The default value indicating if this VDB is a preview VDB. Value is {@value} .
     */
    boolean DEFAULT_PREVIEW = false;

    /**
     * The default version number. Value is {@value} .
     */
    int DEFAULT_VERSION = 1;

    /**
     * An empty array of VDBs.
     */
    Vdb[] NO_VDBS = new Vdb[0];

    /**
     * The resolver of a {@link Vdb}.
     */
    TypeResolver< Vdb > RESOLVER = new TypeResolver< Vdb >() {

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
        public Class< VdbImpl > owningClass() {
            return VdbImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Vdb resolve( final UnitOfWork transaction,
                            final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Vdb.TYPE_ID ) {
                return ( Vdb )kobject;
            }

            return new VdbImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataRoleName
     *        the name of the data role being added (cannot be empty)
     * @return the new data role (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    DataRole addDataRole( final UnitOfWork transaction,
                          final String dataRoleName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param entryName
     *        the name of the entry being added (cannot be empty)
     * @param entryPath
     *        the path of the entry (cannot be empty)
     * @return the new entry (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Entry addEntry( final UnitOfWork transaction,
                    final String entryName,
                    final String entryPath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param vdbName
     *        the name of the VDB being imported (cannot be empty)
     * @return the new VDB import (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbImport addImport( final UnitOfWork transaction,
                         final String vdbName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelName
     *        the name of the VDB being imported (cannot be empty)
     * @return the new VDB import (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Model addModel( final UnitOfWork transaction,
                    final String modelName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param translatorName
     *        the name of the translator being added (cannot be empty)
     * @param translatorType
     *        the type of translator (cannot be empty)
     * @return the new translator (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Translator addTranslator( final UnitOfWork transaction,
                              final String translatorName,
                              final String translatorType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param properties (can be <code>null</code> or empty)
     * @return the VDB XML manifest representing the current state of the VDB (never null)
     * @throws KException
     *         if an error occurs
     */
    VdbManifest createManifest( final UnitOfWork transaction, Properties properties ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the allowed languages (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAllowedLanguages( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the authentication type (can be empty)
     * @throws KException
     */
    String getAuthenticationType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>connection type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getConnectionType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the data roles (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    DataRole[] getDataRoles( final UnitOfWork transaction,
                             final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Entry[] getEntries( final UnitOfWork transaction,
                        final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the GSS pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getGssPattern( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB models (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Model[] getModels( final UnitOfWork transaction,
                       final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB imports (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbImport[] getImports( final UnitOfWork transaction,
                            final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>original file path</code> property (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getOriginalFilePath( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the password pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getPasswordPattern( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the query timeout or -1 if not set
     * @throws KException
     *         if an error occurs
     */
    int getQueryTimeout( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the security domain (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getSecurityDomain( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the translators (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Translator[] getTranslators( final UnitOfWork transaction,
                                 final String... namePatterns ) throws KException;

    /**
     * A name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Teiid VDB name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getVdbName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VERSION
     */
    int getVersion( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if a preview VDB
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_PREVIEW
     */
    boolean isPreview( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataRoleToRemove
     *        the name of the data role being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeDataRole( final UnitOfWork transaction,
                         final String dataRoleToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param entryToRemove
     *        the name of the entry being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeEntry( final UnitOfWork transaction,
                      final String entryToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param importToRemove
     *        the name of the VDB import being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeImport( final UnitOfWork transaction,
                       final String importToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelToRemove
     *        the name of the model being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeModel( final UnitOfWork transaction,
                      final String modelToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param translatorToRemove
     *        the name of the translator being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeTranslator( final UnitOfWork transaction,
                           final String translatorToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowedLanguages
     *        the new allowed languages (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAllowedLanguages( final UnitOfWork transaction,
                              final String newAllowedLanguages ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAuthenticationType
     *        the new authentication type (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAuthenticationType( final UnitOfWork transaction,
                                final String newAuthenticationType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newConnectionType
     *        the new value of the <code>connection type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setConnectionType( final UnitOfWork transaction,
                            final String newConnectionType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newGssPattern
     *        the new GSS pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setGssPattern( final UnitOfWork transaction,
                        final String newGssPattern ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOriginalFilePath
     *        the new value of the <code>original file path</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOriginalFilePath( final UnitOfWork transaction,
                              final String newOriginalFilePath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPasswordPattern
     *        the new password pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setPasswordPattern( final UnitOfWork transaction,
                             final String newPasswordPattern ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPreview
     *        the new value for the <code>preview</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_PREVIEW
     */
    void setPreview( final UnitOfWork transaction,
                     final boolean newPreview ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newQueryTimeout
     *        the new query timeout or a negative number to delete the current value
     * @throws KException
     *         if an error occurs
     */
    void setQueryTimeout( final UnitOfWork transaction,
                          final int newQueryTimeout ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newSecurityDomain
     *        the new security domain (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setSecurityDomain( final UnitOfWork transaction,
                            final String newSecurityDomain ) throws KException;

    /**
     * Sets the name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVdbName
     *        the new value of the <code>Teiid VDB name</code> property
     * @throws KException
     *         if an error occurs
     */
    void setVdbName( final UnitOfWork transaction,
                     final String newVdbName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVersion
     *        the new value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VERSION
     */
    void setVersion( final UnitOfWork transaction,
                     final int newVersion ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param teiid 
     *        the Teiid instance
     * @return the deployment status of this vdb to the given teiid
     */
    DeployStatus deploy(UnitOfWork uow, Teiid teiid);

}
