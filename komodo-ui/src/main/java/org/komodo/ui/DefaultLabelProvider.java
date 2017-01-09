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
package org.komodo.ui;

import static org.komodo.spi.constants.StringConstants.COLON;
import static org.komodo.spi.constants.StringConstants.EMPTY_STRING;
import static org.komodo.spi.constants.StringConstants.FORWARD_SLASH;

import java.util.ArrayList;
import java.util.List;

import org.komodo.core.KomodoLexicon;
import org.komodo.repository.Messages;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.ui.KomodoObjectLabelProvider;
import org.komodo.spi.utils.PropertyProvider;
import org.komodo.spi.utils.TextFormat;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;

/**
 * The default path label provider: removes the namespace prefix from path segments when providing display text; and (2) uses
 * first matching child when providing path.
 */
public class DefaultLabelProvider implements KomodoObjectLabelProvider {

    /**
     * The Komodo root repository absolute path. Value is {@value}.
     */
    public static final String ROOT_PATH = RepositoryImpl.KOMODO_ROOT;

    /**
     * The Komodo root repository absolute path with a slash suffix. Value is {@value}.
     */
    public static final String ROOT_SLASH_PATH = ( ROOT_PATH + FORWARD_SLASH );

    /**
     * The Komodo root display name. Value is {@value}.
     */
    public static final String ROOT_DISPLAY_NAME = FORWARD_SLASH;

    /**
     * The Komodo root display path. Value is {@value}.
     */
    public static final String ROOT_DISPLAY_PATH = FORWARD_SLASH;

    /**
     * The Komodo environment area absolute path. Value is {@value}.
     */
    public static final String ENV_PATH = RepositoryImpl.ENV_ROOT;

    /**
     * The Komodo environment area absolute path with a slash suffix. Value is {@value}.
     */
    public static final String ENV_SLASH_PATH = ( ENV_PATH + FORWARD_SLASH );

    /**
     * The Komodo library area absolute path. Value is {@value}.
     */
    public static final String LIB_PATH = RepositoryImpl.LIBRARY_ROOT;

    /**
     * The Komodo library area absolute path with a slash suffix. Value is {@value}.
     */
    public static final String LIB_SLASH_PATH = ( LIB_PATH + FORWARD_SLASH );

    /**
     * The Komodo environment area display name. Value is {@value}.
     */
    public static final String ENV_DISPLAY_NAME = KomodoLexicon.Environment.UNQUALIFIED_NAME;

    /**
     * The Komodo environment area display path. Value is {@value}.
     */
    public static final String ENV_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + ENV_DISPLAY_NAME );

    /**
     * The Komodo library area display name. Value is {@value}.
     */
    public static final String LIB_DISPLAY_NAME = KomodoLexicon.Library.UNQUALIFIED_NAME;

    /**
     * The Komodo library area display path. Value is {@value}.
     */
    public static final String LIB_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + LIB_DISPLAY_NAME );

//    /**
//     * The Komodo workspace area display name. Value is {@value}.
//     */
//    public static final String WORKSPACE_DISPLAY_NAME = KomodoLexicon.Workspace.UNQUALIFIED_NAME;
//
//    /**
//     * The Komodo workspace area display path. Value is {@value}.
//     */
//    public static final String WORKSPACE_DISPLAY_PATH = ( ROOT_DISPLAY_PATH + WORKSPACE_DISPLAY_NAME );

	private static List<String> GROUPING_NODES=new ArrayList<String>(); //default label provider has currently no grouping nodes
    protected static final String TKO_PREFIX = "tko:"; //$NON-NLS-1$

    /**
     * @param kobject
     *        the object being tested (cannot be <code>null</code>)
     * @return <code>true</code> if the type should be shown to the user
     */
    public static boolean shouldShowType( final UnitOfWork transaction, final KomodoObject kobject ) throws KException {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        final String path = kobject.getAbsolutePath();
        if (ROOT_PATH.equals( path ) || ENV_PATH.equals( path ) || LIB_PATH.equals( path ))
            return false;

        Descriptor type = kobject.getPrimaryType(transaction);
        return ! KomodoLexicon.Workspace.NODE_TYPE.equals(type.getName())
                        && ! KomodoLexicon.Home.NODE_TYPE.equals(type.getName());
    }

    protected PropertyProvider propertyProvider;
    protected Repository repository;

    private String workspacePath;

    /**
     * The Komodo workspace area display name. Value is {@value}.
     */
    public static String workspaceDisplayName(UnitOfWork transaction) {
        if (transaction == null || RepositoryImpl.isSystemTx(transaction))
            return KomodoLexicon.Workspace.UNQUALIFIED_NAME;

        return KomodoLexicon.Workspace.UNQUALIFIED_NAME + FORWARD_SLASH + transaction.getUserName();
    }

    /**
     * The Komodo workspace area display path. Value is {@value}.
     */
    public static String workspaceDisplayPath(UnitOfWork transaction) {
        return ROOT_DISPLAY_PATH + workspaceDisplayName(transaction);
    }

    @Override
    public String getWorkspacePath() {
        return workspacePath;
    }

    @Override
    public void setWorkspacePath(UnitOfWork transaction) {
        this.workspacePath = RepositoryImpl.komodoWorkspacePath(transaction);
    }

    public String getWorkspaceSlashPath() {
        return workspacePath + FORWARD_SLASH;
    }

    @Override
    public boolean isWorkspacePath(String path) {
        return getWorkspacePath().equals(path) || getWorkspaceSlashPath().equals(path);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getDisplayName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getDisplayName( final UnitOfWork transaction,
                                  final KomodoObject kobject,
                                  final TextFormat format ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        final String path = kobject.getAbsolutePath();

        // Check whether it is workspace, library or environment
        String areaObjectName=getNameForAreaObject(path, transaction);

        if(areaObjectName != null){
        	return areaObjectName;
        }

        String lastSegment = null;
        try {
			lastSegment = kobject.getName( transaction );
		} catch (KException ex) {
            KLog.getLogger()
                .error( I18n.bind( Messages.getString( Messages.Komodo.UNABLE_TO_OBTAIN_NAME, kobject.getAbsolutePath() ) ), ex );
		}

        if(lastSegment == null || skippedPathSegmentNames().contains( lastSegment )) {
            return null;
        }

        if ( isShowingPropertyNamePrefixes() ) {
            final int index = lastSegment.indexOf( COLON );

            if ( index == -1 ) {
                return lastSegment;
            }

            return lastSegment.substring( index + 1 );
        }

        return lastSegment;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getDisplayPath(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getDisplayPath( final UnitOfWork transaction,
                                  final KomodoObject kobject,
                                  final TextFormat format ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        return getDisplayPath( transaction, kobject.getAbsolutePath(), format );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getDisplayPath(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getDisplayPath( final UnitOfWork transaction,
                                  final String repositoryAbsolutePath,
                                  final TextFormat format ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( repositoryAbsolutePath, "repositoryAbsolutePath" ); //$NON-NLS-1$

        if ( !repositoryAbsolutePath.startsWith( ROOT_PATH ) ) {
            return null;
        }

        // /tko:komodo
        if ( ROOT_PATH.equals( repositoryAbsolutePath ) || ROOT_SLASH_PATH.equals( repositoryAbsolutePath ) ) {
            return ROOT_DISPLAY_PATH;
        }

        // /tko:komodo/workspace/${USER}
        if ( getWorkspacePath().equals( repositoryAbsolutePath ) || getWorkspaceSlashPath().equals( repositoryAbsolutePath ) ) {
            return workspaceDisplayPath(transaction);
        }

        // /tko:komodo/library
        if ( LIB_PATH.equals( repositoryAbsolutePath ) || LIB_SLASH_PATH.equals( repositoryAbsolutePath ) ) {
            return LIB_DISPLAY_PATH;
        }

        // /tko:komodo/environment
        if ( ENV_PATH.equals( repositoryAbsolutePath ) || ENV_SLASH_PATH.equals( repositoryAbsolutePath ) ) {
            return ENV_DISPLAY_PATH;
        }

        final String relativePath = repositoryAbsolutePath.substring( ROOT_PATH.length() );
        final StringBuilder displayPath = new StringBuilder( ROOT_DISPLAY_PATH );
        boolean firstTime = true;

        for ( final String segment : relativePath.split( FORWARD_SLASH ) ) {
            if ( EMPTY_STRING.equals( segment ) ) {
                continue;
            }

            final boolean skip = skippedPathSegmentNames().contains( segment );

            if ( !firstTime && !skip ) {
                displayPath.append( FORWARD_SLASH );
            } else {
                firstTime = false;
            }

            if ( !skip ) {
                if(segment.startsWith(TKO_PREFIX)) {
                    displayPath.append(segment.substring(TKO_PREFIX.length()));
                } else {
                    displayPath.append( segment );
                }
            }
        }

        if ( repositoryAbsolutePath.endsWith( FORWARD_SLASH ) ) {
            displayPath.append( FORWARD_SLASH );
        }

        return displayPath.toString();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getId()
     */
    @Override
    public String getId() {
        return DefaultLabelProvider.class.getName();
    }

    private String checkWorkspacePath(final UnitOfWork transaction, String displayPath) {
        String wkspPragma = workspaceDisplayName(null) + FORWARD_SLASH;
        String userName = transaction.getUserName();

        if (RepositoryImpl.isSystemTx(transaction))
            return displayPath; // no user name so nothing to do

        int wkspIndex = displayPath.indexOf(wkspPragma);
        if (wkspIndex == -1)
            return displayPath; // no workspace so nothing to do
            
        int userIndex = displayPath.indexOf(wkspPragma + userName);
        if (userIndex != -1)
            return displayPath; // already contains workspace/user so nothing to do

        //
        // So displayPath contains workspace/ but does not yet contain workspace/$USER
        //
        return displayPath.replace(wkspPragma, wkspPragma + userName + FORWARD_SLASH);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getPath(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public String getPath( final UnitOfWork transaction, String displayPath ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( displayPath, "displayPath" ); //$NON-NLS-1$

        if ( !displayPath.startsWith( ROOT_DISPLAY_PATH ) ) {
            return null;
        }

        if ( ROOT_DISPLAY_PATH.equals( displayPath ) ) {
            return ROOT_PATH;
        }

        displayPath = checkWorkspacePath(transaction, displayPath);

        KomodoObject kobject = null;

        try {
            KomodoObject parent = new ObjectImpl( this.repository, ROOT_PATH, 0 );

            // for each segment make sure a child exists and add in prefix if necessary
            for ( final String segment : displayPath.split( FORWARD_SLASH ) ) {
                if ( EMPTY_STRING.equals( segment ) || StringConstants.DOT.equals(segment) ) {
                    continue;
                }
                // Process '..' in display path
                if (StringConstants.DOT_DOT.equals(segment)) {
                    KomodoObject theParent = parent.getParent( transaction );
                    if(theParent!=null) {
                        if(skippedPathSegmentNames().contains(theParent.getName( transaction ))) {
                            parent = theParent.getParent( transaction );
                        } else {
                            parent = theParent;
                        }
                        continue;
                    }
                }

                if ( isShowingPropertyNamePrefixes() ) {
                    if ( parent.hasChild( transaction, segment ) ) {
                        kobject = parent.getChild( transaction, segment );
                        parent = kobject;
                    } else {
                        return null; // no child with that name
                    }
                } else {
                    // loop through children and take first one with local name match.  skip grouping nodes
                    boolean childFound = false;
                    for ( final KomodoObject kid : parent.getChildren( transaction ) ) {
                        final String name = kid.getName( transaction );
                        final int index = name.indexOf( StringConstants.COLON );

                        if ( index == -1 ) {
                            if ( segment.equals( name ) ) {
                                kobject = kid;
                                parent = kobject;
                                childFound = true;
                                break;
                            }
                        } else if (skippedPathSegmentNames().contains( name )) {
                            KomodoObject[] children = kid.getChildren( transaction, segment );
                            if(children.length>0) {
                                kobject = children[0];
                                parent = kobject;
                                childFound = true;
                                break;
                            }
                        } else {
                            if ( segment.equals( name.substring( index + 1 ) ) ) {
                                kobject = kid;
                                parent = kobject;
                                childFound = true;
                                break;
                            }
                        }
                    }
                    if(!childFound) return null;
                }
            }

            if ( kobject == null ) {
                return null;
            }

            return kobject.getAbsolutePath();
        } catch ( final Exception e ) {
            return null;
        }
    }

    /**
     * @return <code>true</code> if the property name prefixes should be shown
     */
    protected boolean isShowingPropertyNamePrefixes() {
        final Object value = this.propertyProvider.getProperty( KomodoObjectLabelProvider.Settings.SHOW_PROP_NAME_PREFIX );
        return ( ( value == null ) ? true : new Boolean( value.toString() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#setPropertyProvider(org.komodo.spi.utils.PropertyProvider)
     */
    @Override
    public void setPropertyProvider( final PropertyProvider newPropertyProvider ) {
        this.propertyProvider = newPropertyProvider;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#setRepository(org.komodo.spi.repository.Repository)
     */
    @Override
    public void setRepository( final Repository repository, UnitOfWork transaction ) throws KException {
        assert( repository != null );
        this.repository = repository;

        //
        // Ensures that the user workspace is created
        //
        this.repository.komodoWorkspace(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see  org.komodo.spi.ui.KomodoObjectLabelProvider#skippedPathSegmentNames()
     */
	@Override
	public List<String> skippedPathSegmentNames(){
		return GROUPING_NODES;
	}

	/**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
	@Override
	public String getTypeDisplay( final UnitOfWork transaction,
	                              final KomodoObject kobject,
	                              final TextFormat format ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        String areaObjectName=getNameForAreaObject(kobject.getAbsolutePath(), transaction);

		if(areaObjectName != null){
			return areaObjectName;
		}

		if (ObjectImpl.class.equals(kobject.getClass())) {
			return KomodoObject.class.getSimpleName();
		}

		return kobject.getClass().getSimpleName();
	}
	/**
	 *
	 * @param path Absolute path to object
	 * @param transaction the transaction
	 * @return Name of the area object (Workspace, Library,..) or null if the path doesn't lead to area object
	 */
	protected String getNameForAreaObject(String path, UnitOfWork transaction){
        // /tko:komodo
        if ( ROOT_PATH.equals( path ) || ROOT_SLASH_PATH.equals( path ) ) {
            return ROOT_DISPLAY_NAME;
        }

        // /tko:komodo/workspace/${USER}
        if ( getWorkspacePath().equals( path ) || getWorkspaceSlashPath().equals( path ) ) {
            return workspaceDisplayName(transaction);
        }

        // /tko:komodo/library
        if ( LIB_PATH.equals( path ) || LIB_SLASH_PATH.equals( path ) ) {
            return LIB_DISPLAY_NAME;
        }

        // /tko:komodo/environment
        if ( ENV_PATH.equals( path ) || ENV_SLASH_PATH.equals( path ) ) {
            return ENV_DISPLAY_NAME;
        }

        return null;
	}

}
