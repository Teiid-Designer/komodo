/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell;

import static org.komodo.spi.constants.StringConstants.COLON;
import static org.komodo.spi.constants.StringConstants.EMPTY_STRING;
import static org.komodo.spi.constants.StringConstants.FORWARD_SLASH;

import java.util.ArrayList;
import java.util.List;

import org.komodo.repository.ObjectImpl;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;

/**
 * The default path label provider: removes the namespace prefix from path segments when providing display text; and (2) uses
 * first matching child when providing path.
 */
public class DefaultLabelProvider implements KomodoObjectLabelProvider {

	private static List<String> GROUPING_NODES=new ArrayList<String>(); //default label provider has currently no grouping nodes
    protected static final String TKO_PREFIX = "tko:"; //$NON-NLS-1$

	/**
     * @param kobject
     *        the object being tested (cannot be <code>null</code>)
     * @return <code>true</code> if the type should be shown to the user
     */
    protected static boolean shouldShowType( final KomodoObject kobject ) {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        final String path = kobject.getAbsolutePath();
        return ( !ROOT_PATH.equals( path )
                 && !ENV_PATH.equals( path )
                 && !LIB_PATH.equals( path )
                 && !WORKSPACE_PATH.equals( path ) );
    }


    protected Repository repository;
    protected WorkspaceStatus status;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getDisplayName(org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getDisplayName( final KomodoObject kobject ) {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        final String path = kobject.getAbsolutePath();

        // Check whether it is workspace, library or environment
        String areaObjectName=getNameForAreaObject(path);

        if(areaObjectName != null){
        	return areaObjectName;
        }

        String lastSegment = null;
        try {
			lastSegment = kobject.getName(status.getTransaction());
		} catch (KException ex) {
			KLog.getLogger().error( I18n.bind( ShellI18n.internalError) , ex );
		}

        if(lastSegment == null || skippedPathSegmentNames().contains( lastSegment )) {
            return null;
        }

        if ( this.status.isShowingPropertyNamePrefixes() ) {
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
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getDisplayPath(org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getDisplayPath( final KomodoObject kobject ) {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        return getDisplayPath( kobject.getAbsolutePath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getDisplayPath(java.lang.String)
     */
    @Override
    public String getDisplayPath( final String repositoryAbsolutePath ) {
        ArgCheck.isNotEmpty( repositoryAbsolutePath, "repositoryAbsolutePath" ); //$NON-NLS-1$

        if ( !repositoryAbsolutePath.startsWith( ROOT_PATH ) ) {
            return null;
        }

        // /tko:komodo
        if ( ROOT_PATH.equals( repositoryAbsolutePath ) || ROOT_SLASH_PATH.equals( repositoryAbsolutePath ) ) {
            return ROOT_DISPLAY_PATH;
        }

        // /tko:komodo/workspace
        if ( WORKSPACE_PATH.equals( repositoryAbsolutePath ) || WORKSPACE_SLASH_PATH.equals( repositoryAbsolutePath ) ) {
            return WORKSPACE_DISPLAY_PATH;
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
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getId()
     */
    @Override
    public String getId() {
        return DefaultLabelProvider.class.getName();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getPath(java.lang.String)
     */
    @Override
    public String getPath( final String displayPath ) {
        ArgCheck.isNotEmpty( displayPath, "displayPath" ); //$NON-NLS-1$

        if ( !displayPath.startsWith( ROOT_DISPLAY_PATH ) ) {
            return null;
        }

        if ( ROOT_DISPLAY_PATH.equals( displayPath ) ) {
            return ROOT_PATH;
        }

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
                    KomodoObject theParent = parent.getParent(this.status.getTransaction());
                    if(theParent!=null) {
                        if(skippedPathSegmentNames().contains(theParent.getName(this.status.getTransaction()))) {
                            parent = theParent.getParent(this.status.getTransaction());
                        } else {
                            parent = theParent;
                        }
                        continue;
                    }
                }

                if ( this.status.isShowingPropertyNamePrefixes() ) {
                    if ( parent.hasChild( this.status.getTransaction(), segment ) ) {
                        kobject = parent.getChild( this.status.getTransaction(), segment );
                        parent = kobject;
                    } else {
                        return null; // no child with that name
                    }
                } else {
                    // loop through children and take first one with local name match.  skip grouping nodes
                    boolean childFound = false;
                    for ( final KomodoObject kid : parent.getChildren( this.status.getTransaction() ) ) {
                        final String name = kid.getName( this.status.getTransaction() );
                        final int index = name.indexOf( StringConstants.COLON );

                        if ( index == -1 ) {
                            if ( segment.equals( name ) ) {
                                kobject = kid;
                                parent = kobject;
                                childFound = true;
                                break;
                            }
                        } else if (skippedPathSegmentNames().contains( name )) {
                            KomodoObject[] children = kid.getChildren(this.status.getTransaction(), segment);
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
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#setRepository(org.komodo.spi.repository.Repository)
     */
    @Override
    public void setRepository( final Repository repository ) {
        assert( repository != null );
        this.repository = repository;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#setWorkspaceStatus(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void setWorkspaceStatus( final WorkspaceStatus status ) {
        assert( status != null );
        this.status = status;
    }

    /**
     * {@inheritDoc}
     *
     * @see  org.komodo.shell.api.KomodoObjectLabelProvider#skippedPathSegmentNames()
     */
	@Override
	public List<String> skippedPathSegmentNames(){
		return GROUPING_NODES;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.komodo.shell.api.KomodoObjectLabelProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
	 *      org.komodo.spi.repository.KomodoObject)
	 */
	@Override
	public String getTypeDisplay(UnitOfWork uow, KomodoObject kobject) {

		String areaObjectName=getNameForAreaObject(kobject.getAbsolutePath());
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
	 * @return Name of the area object (Workspace, Library,..) or null if the path doesn't lead to area object
	 */
	private String getNameForAreaObject(String path){
        // /tko:komodo
        if ( ROOT_PATH.equals( path ) || ROOT_SLASH_PATH.equals( path ) ) {
            return ROOT_DISPLAY_NAME;
        }

        // /tko:komodo/workspace
        if ( WORKSPACE_PATH.equals( path ) || WORKSPACE_SLASH_PATH.equals( path ) ) {
            return WORKSPACE_DISPLAY_NAME;
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
