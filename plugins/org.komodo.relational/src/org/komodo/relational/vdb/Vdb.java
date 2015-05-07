/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import java.util.Properties;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.model.Model;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
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
     * @return the value of the <code>connection type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getConnectionType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the data roles (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    DataRole[] getDataRoles( final UnitOfWork transaction ) throws KException;

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
     * @return the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Entry[] getEntries( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the VDB models (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Model[] getModels( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the VDB imports (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbImport[] getImports( final UnitOfWork transaction ) throws KException;

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
     * @return the translators (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Translator[] getTranslators( final UnitOfWork transaction ) throws KException;

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
     * @param newPreview
     *        the new value for the <code>preview</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_PREVIEW
     */
    void setPreview( final UnitOfWork transaction,
                     final boolean newPreview ) throws KException;

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

}
