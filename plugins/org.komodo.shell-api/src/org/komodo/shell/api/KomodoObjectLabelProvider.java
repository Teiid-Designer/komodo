/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.api;

import static org.komodo.spi.constants.StringConstants.FORWARD_SLASH;

import java.util.List;

import org.komodo.core.KomodoLexicon;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * Provides display representations for {@link KomodoObject}s.
 */
public interface KomodoObjectLabelProvider {

    /**
     * The Komodo root repository absolute path. Value is {@value}.
     */
    String ROOT_PATH = RepositoryImpl.KOMODO_ROOT;

    /**
     * The Komodo root repository absolute path with a slash suffix. Value is {@value}.
     */
    String ROOT_SLASH_PATH = ( ROOT_PATH + FORWARD_SLASH );

    /**
     * The Komodo root display name. Value is {@value}.
     */
    String ROOT_DISPLAY_NAME = FORWARD_SLASH;

    /**
     * The Komodo root display path. Value is {@value}.
     */
    String ROOT_DISPLAY_PATH = FORWARD_SLASH;

    /**
     * The Komodo environment area absolute path. Value is {@value}.
     */
    String ENV_PATH = RepositoryImpl.ENV_ROOT;

    /**
     * The Komodo environment area absolute path with a slash suffix. Value is {@value}.
     */
    String ENV_SLASH_PATH = ( ENV_PATH + FORWARD_SLASH );

    /**
     * The Komodo environment area display name. Value is {@value}.
     */
    String ENV_DISPLAY_NAME = KomodoLexicon.Environment.UNQUALIFIED_NAME;

    /**
     * The Komodo environment area display path. Value is {@value}.
     */
    String ENV_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + ENV_DISPLAY_NAME );

    /**
     * The Komodo library area absolute path. Value is {@value}.
     */
    String LIB_PATH = RepositoryImpl.LIBRARY_ROOT;

    /**
     * The Komodo library area absolute path with a slash suffix. Value is {@value}.
     */
    String LIB_SLASH_PATH = ( LIB_PATH + FORWARD_SLASH );

    /**
     * The Komodo library area display name. Value is {@value}.
     */
    String LIB_DISPLAY_NAME = KomodoLexicon.Library.UNQUALIFIED_NAME;

    /**
     * The Komodo library area display path. Value is {@value}.
     */
    String LIB_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + LIB_DISPLAY_NAME );

    /**
     * The Komodo workspace area absolute path. Value is {@value}.
     */
    String WORKSPACE_PATH = RepositoryImpl.WORKSPACE_ROOT;

    /**
     * The Komodo workspace area absolute path with a slash suffix. Value is {@value}.
     */
    String WORKSPACE_SLASH_PATH = ( WORKSPACE_PATH + FORWARD_SLASH );

    /**
     * The Komodo workspace area display name. Value is {@value}.
     */
    String WORKSPACE_DISPLAY_NAME = KomodoLexicon.Workspace.UNQUALIFIED_NAME;

    /**
     * The Komodo workspace area display path. Value is {@value}.
     */
    String WORKSPACE_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + WORKSPACE_DISPLAY_NAME );

    /**
     * If necessary, removes the namespace prefix from the name.
     *
     * @param kobject
     *        the repository path whose display name is being requested (cannot be <code>null</code>)
     * @return the display text (can be empty if unable to provide text)
     */
    String getDisplayName( final KomodoObject kobject );

    /**
     * @param kobject
     *        the object whose whose repository display path is being requested (cannot be <code>null</code>)
     * @return the display path (can be empty if unable to provide text)
     */
    String getDisplayPath( final KomodoObject kobject );

    /**
     * @param repositoryAbsolutePath
     *        the path of the object whose whose repository display path is being requested (cannot be empty)
     * @return the display path (can be empty if unable to provide text)
     */
    String getDisplayPath( final String repositoryAbsolutePath );

    /**
     * @return a unique identifier for this label provider (never empty)
     */
    String getId();

    /**
     * @param displayPath
     *        the display path whose repository path is being requested (cannot be empty)
     * @return the absolute, fully qualified repository path (can be empty if unable to provide a path)
     */
    String getPath( final String displayPath );

    /**
     * This method will be called by the Komodo shell framework and should not be called.
     *
     * @param repository
     *        the repository whose paths and display paths will be provided (never <code>null</code>)
     */
    void setRepository( final Repository repository );

    /**
     * This method will be called by the Komodo shell framework and should not be called.
     *
     * @param status
     *        the workspace status (never <code>null</code>)
     */
    void setWorkspaceStatus( final WorkspaceStatus status );
    
	 /**
     * @return Returns a node names which should be omitted from display path
     *         (Never <code>null</code>. Returns empty list if not grouping nodes are available)
     */
	List<String> skippedPathSegmentNames();
	
    /**
     * Get the type display string for a KomodoObject
     * @param uow the transaction (never <code>null</code>)
     * @param kObj the KomodoObject (never <code>null</code>)
     * @return the type display string (never returns <code>null</code>)
     */
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj );

}
