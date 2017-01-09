/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.ui;

import java.util.List;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.utils.PropertyProvider;
import org.komodo.spi.utils.TextFormat;

/**
 * Provides display representations for {@link KomodoObject}s.
 */
public interface KomodoObjectLabelProvider {

    /**
     * Display settings.
     */
    interface Settings {

        /**
         * Set to <code>true</code> if property namespace prefixes should be shown. If property does not exist, the value should
         * default to <code>true</code>.
         */
        String SHOW_PROP_NAME_PREFIX = "showPropNamePrefix"; //$NON-NLS-1$

    }

    /**
     * @return the path to the workspace
     */
    String getWorkspacePath();

    /**
     * Set the path to the workspace using the username available from the transaction
     *
     * @param transaction
     */
    void setWorkspacePath(UnitOfWork transaction);

    /**
     * @param path
     * @return if the given path is the workspace
     */
    boolean isWorkspacePath(String path);

    /**
     * If necessary, removes the namespace prefix from the name.
     *
     * @param transaction
     *        the transaction (never <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param kobject
     *        the repository path whose display name is being requested (cannot be <code>null</code>)
     * @param format
     *        the text formatting (can be <code>null</code> if no formatting will be done)
     * @return the display text (can be empty if unable to provide text)
     */
    String getDisplayName( final UnitOfWork transaction,
                           final KomodoObject kobject,
                           final TextFormat format );

    /**
     * @param transaction
     *        the transaction (never <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param kobject
     *        the object whose whose repository display path is being requested (cannot be <code>null</code>)
     * @param format
     *        the text formatting (can be <code>null</code> if no formatting will be done)
     * @return the display path (can be empty if unable to provide text)
     */
    String getDisplayPath( final UnitOfWork transaction,
                           final KomodoObject kobject,
                           final TextFormat format );

    /**
     * @param transaction
     *        the transaction (never <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repositoryAbsolutePath
     *        the path of the object whose whose repository display path is being requested (cannot be empty)
     * @param format
     *        the text formatting (can be <code>null</code> if no formatting will be done)
     * @return the display path (can be empty if unable to provide text)
     */
    String getDisplayPath( final UnitOfWork transaction,
                           final String repositoryAbsolutePath,
                           final TextFormat format );

    /**
     * @return a unique identifier for this label provider (never empty)
     */
    String getId();

    /**
     * @param transaction
     *        the transaction (never <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param displayPath
     *        the display path whose repository path is being requested (cannot be empty)
     * @return the absolute, fully qualified repository path (can be empty if unable to provide a path)
     */
    String getPath( final UnitOfWork transaction,
                    final String displayPath );

    /**
     * Get the type display string for a KomodoObject
     *
     * @param transaction
     *        the transaction (never <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param kobject
     *        the KomodoObject (never <code>null</code>)
     * @param format
     *        the text formatting (can be <code>null</code> if no formatting will be done)
     * @return the type display string (<code>null</code> if unable to provide type display)
     */
    String getTypeDisplay( final UnitOfWork transaction,
                           final KomodoObject kobject,
                           final TextFormat format );

    /**
     * @param propertyProvider
     *        the property provider (cannot be <code>null</code>)
     */
    void setPropertyProvider( final PropertyProvider propertyProvider );

    /**
     * This method will be called by the Komodo shell framework and should not be called.
     *
     * @param repository
     *        the repository whose paths and display paths will be provided (never <code>null</code>)
     * @param transaction
     *        the transaction holding the prospective user
     * @throws KException if error occurs 
     */
    void setRepository( final Repository repository, UnitOfWork transaction ) throws KException;

    /**
     * @return Returns a node names which should be omitted from display path (Never <code>null</code>. Returns empty list if not
     *         grouping nodes are available)
     */
    List< String > skippedPathSegmentNames();

}
