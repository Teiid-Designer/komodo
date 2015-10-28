/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.spi.constants.StringConstants.EMPTY_STRING;
import static org.komodo.spi.constants.StringConstants.FORWARD_SLASH;
import java.util.Arrays;
import java.util.List;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.DefaultLabelProvider;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A label provider for relational objects.
 */
public class RelationalLabelProvider extends DefaultLabelProvider {

    private static final String TKO_PREFIX = "tko:"; //$NON-NLS-1$
    
    /**
     * A collection of grouping node names that should be removed from the display paths.
     */
    private static final List< String > GROUPING_NODES = Arrays.asList( new String[] { VdbLexicon.DataRole.PERMISSIONS,
                                                                                       VdbLexicon.DataRole.Permission.CONDITIONS,
                                                                                       VdbLexicon.DataRole.Permission.MASKS,
                                                                                       VdbLexicon.Vdb.DATA_ROLES,
                                                                                       VdbLexicon.Vdb.TRANSLATORS,
                                                                                       VdbLexicon.Vdb.SOURCES,
                                                                                       VdbLexicon.Vdb.ENTRIES,
                                                                                       VdbLexicon.Vdb.IMPORT_VDBS} );


    /**
     * Constructs a command provider for workspace shell commands.
     */
    public RelationalLabelProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#getDisplayName(org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getDisplayName( final KomodoObject kobject ) {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        // Null returned if grouping node
        final Repository.UnitOfWork uow = this.status.getTransaction();
        String nodeName = null;
        try {
            nodeName = kobject.getName(uow);
        } catch (KException ex) {
            // node exception
        }
        if(GROUPING_NODES.contains( nodeName )) {
            return null;
        }
        return super.getDisplayName(kobject);
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

            final boolean skip = GROUPING_NODES.contains( segment );

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
                        if(GROUPING_NODES.contains(theParent.getName(this.status.getTransaction()))) {
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
                        } else if (GROUPING_NODES.contains( name )) {
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
    
}
