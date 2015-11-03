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
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * The default path label provider: removes the namespace prefix from path segments when providing display text; and (2) uses
 * first matching child when providing path.
 */
public class DefaultLabelProvider implements KomodoObjectLabelProvider {

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

        final String[] segments = path.split( FORWARD_SLASH );
        final String lastSegment = segments[ segments.length - 1 ];

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
            if ( !StringUtils.isBlank( getPath( repositoryAbsolutePath ) ) ) {
                return repositoryAbsolutePath;
            }

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
            if ( EMPTY_STRING.equals( segment ) || ROOT_PATH.equals( segment ) ) {
                continue;
            }

            if ( firstTime ) {
                firstTime = false;
            } else {
                displayPath.append( FORWARD_SLASH );
            }

            if ( this.status.isShowingPropertyNamePrefixes() ) {
                displayPath.append( segment );
            } else {
                final int index = segment.indexOf( StringConstants.COLON );

                if ( index == -1 ) {
                    displayPath.append( segment );
                } else {
                    displayPath.append( segment.substring( index + 1 ) );
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
                        parent = theParent;
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
                    // loop through children and take first one with local name match
                    for ( final KomodoObject kid : parent.getChildren( this.status.getTransaction() ) ) {
                        final String name = kid.getName( this.status.getTransaction() );
                        final int index = name.indexOf( StringConstants.COLON );

                        if ( index == -1 ) {
                            if ( segment.equals( name ) ) {
                                kobject = kid;
                                parent = kobject;
                                break;
                            }
                        } else {
                            if ( segment.equals( name.substring( index + 1 ) ) ) {
                                kobject = kid;
                                parent = kobject;
                                break;
                            }
                        }
                    }
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

}
