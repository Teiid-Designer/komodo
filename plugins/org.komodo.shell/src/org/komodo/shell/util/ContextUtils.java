/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.util;

import java.util.Arrays;
import java.util.List;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;


/**
 * Helper methods for working with paths
 */
public class ContextUtils implements StringConstants {

	/**
	 * A collection of grouping node names that should be removed from the display paths.
	 */
    private static final List< String > GROUPING_NODES = Arrays.asList( new String[] { VdbLexicon.DataRole.PERMISSIONS,
                                                                                      VdbLexicon.Vdb.DATA_ROLES,
                                                                                      VdbLexicon.Vdb.TRANSLATORS } );

    /**
     * @param path
     *        the path whose display path is being requested (cannot be empty)
     * @return the display path (never empty)
     */
    public static String convertPathToDisplayPath( String path ) {
        if ( StringUtils.isBlank( path ) || path.equals(FORWARD_SLASH) ) {
            return path;
        }

        final StringBuilder displayPath = new StringBuilder();

        if ( path.startsWith( FORWARD_SLASH ) ) {
            displayPath.append( FORWARD_SLASH );
        }

        boolean firstTime = true;

        for ( final String segment : getPathSegments( path ) ) {
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
                displayPath.append( segment );
            }
        }

        if ( path.endsWith( FORWARD_SLASH ) ) {
            displayPath.append( FORWARD_SLASH );
        }

        return displayPath.toString();
    }

	/**
	 * Get the workspace context for the specified path.  The path can be either an absolute path, or relative
	 * to the current context.  The path is separated by the PATH_SEPARATOR, for example 'table1/col1'.
	 * @param workspaceStatus the workspace status
	 * @param path the supplied path
	 * @return the context at the specified path, null if not found.
	 */
	public static KomodoObject getContextForPath(WorkspaceStatus workspaceStatus, String path) {
	    path = convertPathToDisplayPath( path );

	    if(StringUtils.isBlank(path)) return workspaceStatus.getCurrentContext();

        // check path for cd into root options
        if ( path.equals( FORWARD_SLASH ) ) {
            return workspaceStatus.getRootContext();
        }

        KomodoObject resultObject = null;
		try {
            // Location supplied as absolute path
            if(ContextUtils.isAbsolutePath(path)) {
            	resultObject = ContextUtils.getContextForAbsolutePath(workspaceStatus, workspaceStatus.getRootContext(), path);
            // Location supplied as relative path
            } else {
            	resultObject = ContextUtils.getRelativeContext(workspaceStatus, workspaceStatus.getCurrentContext(), path);
            }
        } catch (KException ex) {
            // Failed to locate the object
        }

		if(resultObject!=null) {
		    try {
                final KomodoObject resolved = workspaceStatus.resolve( resultObject );

                if ( resolved != null ) {
                    return resolved;
                }
            } catch (KException ex) {
            }
		}
		return resultObject;
	}

	/**
	 * Get the context relative to the current context at the specified path.  The path could be multi-level,
	 * separated by the PATH_SEPARATOR, for example 'table1/col1'.  or it could be single path.
	 * @param currentContext the current context
	 * @param relativePath the supplied child path relative to the context
	 * @return the context at the specified path, null if not found.
	 */
	private static KomodoObject getRelativeContext(WorkspaceStatus wsStatus, KomodoObject currentContext, String relativePath) throws KException {
	    KomodoObject resultContext = null;
	    if(relativePath==null) return resultContext;

		// Get the path segments
		String[] pathSegments = ContextUtils.getPathSegments(relativePath);

		// Walk thru path as far as possible, getting child contexts
		for(String segment : pathSegments) {
		    KomodoObject theContext = getContext(wsStatus,currentContext,segment);

			// Child context found, update and keep going
			if(theContext!=null) {
				currentContext = theContext;
				resultContext = theContext;
			// Child context not found, quit and return null
			} else {
				resultContext = null;
				break;
			}
		}

		return resultContext;
	}

	/**
	 * Get a context child (or parent) of the current context, based on provided segment name.  Returns null if a child of supplied name
	 * does not exist.  The supplied segment can only represent a change of one level above or below the currentContext.
	 * @param currentContext the current context
	 * @param segmentName the name of the context to find
	 * @return the child context
	 */
	private static KomodoObject getContext(final WorkspaceStatus wsStatus, KomodoObject currentContext, String segmentName) throws KException {
		// Special navigation cases
		if(segmentName.equals("..")) { //$NON-NLS-1$
		    if (wsStatus.getRootContext().equals( currentContext )) {
		        return null;
		    }

		    return currentContext.getParent(wsStatus.getTransaction());
		} else if(segmentName.equals(".")) { //$NON-NLS-1$
			return currentContext;
		}
		KomodoObject childContext = null;
		try {
			for(KomodoObject theContext : currentContext.getChildren(wsStatus.getTransaction())) {
			    final String contextName = wsStatus.getLabelProvider().getDisplayName( theContext );
				if(contextName.equals(segmentName)) {
					childContext = theContext;
					break;
				}
			}
		} catch (Exception e) {
			childContext = null;
		}
		return childContext;
	}

    /**
     * Determine if the supplied context has multiple children that start with the segmentName
     * @param currentContext the current context
     * @param segmentName the name of the context to find
     * @return 'true' if multiple matching, 'false' if not.
     */
    private static boolean hasMultipleChildrenStartingWith(final WorkspaceStatus wsStatus, KomodoObject currentContext, String segmentName) {
        int nMatching = 0;
        try {
            for(KomodoObject theContext : currentContext.getChildren(wsStatus.getTransaction())) {
                final String contextName = wsStatus.getLabelProvider().getDisplayName( theContext );
                if(contextName.startsWith(segmentName)) {
                    nMatching++;
                    if(nMatching>1) break;
                }
            }
        } catch (Exception e) {
        }
        return nMatching>1;
    }

    /**
	 * Get the context at the specified absolute path.  If there is no such context, null is returned.
	 * @param rootContext the root context
	 * @param absolutePath the absolute path
	 * @return the context at the specified absolute path, null if not found.
	 */
	private static KomodoObject getContextForAbsolutePath(WorkspaceStatus wsStatus, KomodoObject rootContext, String absolutePath) throws KException {
        final String path = wsStatus.getLabelProvider().getDisplayPath( absolutePath );

        if ( !StringUtils.isBlank( path ) ) {
            absolutePath = path;
        }

	    KomodoObject resultContext = null;

		if(isAbsolutePath(absolutePath)) {
			String relativePath = ContextUtils.convertAbsolutePathToRootRelative(wsStatus,absolutePath);
			resultContext = ContextUtils.getRelativeContext(wsStatus, rootContext, relativePath);
		}

		return resultContext;
	}

	/**
	 * Get the deepest matching context along the supplied relative path.  If there is no such context, the current context is returned.
	 * @param wsStatus the WorkspaceStatus
	 * @param currentContext the current context
	 * @param relativePath the path relative to current context
	 * @return the context at the deepest matching segment in the specified relative path, if none match - current context is returned.
	 * @throws KException the exception
	 */
	public static KomodoObject getDeepestMatchingContextRelative(WorkspaceStatus wsStatus, KomodoObject currentContext, String relativePath) throws KException {
	    KomodoObject resultContext = currentContext;

		if(!StringUtils.isEmpty(relativePath)) {
			String[] segments = getPathSegments(relativePath);
			int nSegments = segments.length;
			for(int i=0; i<nSegments; i++) {
			    String segment = segments[i];
			    if(ContextUtils.hasMultipleChildrenStartingWith(wsStatus,currentContext,segment)) {
			        resultContext = currentContext;
			        break;
			    } else {
			        KomodoObject theContext = ContextUtils.getContext(wsStatus,currentContext, segment);

			        if(theContext==null) {
			            resultContext = currentContext;
			            break;
			        } else {
			            currentContext = theContext;
			            resultContext = theContext;
			        }
			    }
			}
		}
		return resultContext;
	}

	/**
	 * Determines if the supplied path is absolute ( starts with / )
	 * @param path the supplied path
	 * @return true if path is absolute, false if not
	 */
	public static boolean isAbsolutePath(String path) {
		if(path.startsWith(FORWARD_SLASH)) {
			return true;
		}
		return false;
	}

	/**
	 * convert the supplied absolute path to a path relative to the workspace context
	 * @param wsStatus the WorkspaceContext
	 * @param absolutePath the supplied absolute path
	 * @return the path relative to the root context
	 */
	public static String convertAbsolutePathToRootRelative(WorkspaceStatus wsStatus, String absolutePath) {
        final String path = wsStatus.getLabelProvider().getDisplayPath( absolutePath );

        if ( !StringUtils.isBlank( path ) ) {
            absolutePath = path;
        }

        KomodoObject rootContext = wsStatus.getRootContext();
        String absRootPath = KomodoObjectUtils.getFullName( wsStatus, rootContext );

        if(absolutePath.startsWith(absRootPath)) {
			return absolutePath.substring( absRootPath.length() );
		}
		return null;
	}

	/**
	 * convert the supplied absolute path to a path relative to the supplied context
	 * @param wsStatus the WorkspaceContext
	 * @param context the context
	 * @param absolutePath the supplied absolute path
	 * @return the path relative to the root context
	 */
    public static String convertAbsolutePathToRelative( WorkspaceStatus wsStatus,
                                                        KomodoObject context,
                                                        final String absolutePath ) {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( absolutePath, "absolutePath" ); //$NON-NLS-1$

        final String absContextPath = KomodoObjectUtils.getFullName( wsStatus, context );
        final String displayPath = wsStatus.getLabelProvider().getDisplayPath( absolutePath );
        String path = ( StringUtils.isBlank( displayPath ) ? absolutePath : displayPath );

        if ( path.startsWith( absContextPath ) ) {
            String relativePath = path.substring( absContextPath.length() );

            if ( !StringUtils.isEmpty( relativePath ) ) {
                if ( relativePath.startsWith( FORWARD_SLASH ) ) {
                    relativePath = relativePath.substring( 1 );
                }
            }

            return relativePath;
        }

        return null;
    }

	/**
	 * Breaks the path apart into its segments, using the path separator
	 * @param path the supplied path
	 * @return the array of path segments
	 */
	public static String[] getPathSegments(String path) {
		return path.split(FORWARD_SLASH);
	}

	/**
	 * Builds a path from the specified segments, starting at the root and including nLevels
	 * @param pathSegments the array of segments
	 * @param nLevels number of levels to include
	 * @return the path
	 */
	public static String getPath(String[] pathSegments, int nLevels) {
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<nLevels; i++) {
			if(i!=0) {
				sb.append(FORWARD_SLASH);
			}
			sb.append(pathSegments[i]);
		}
		return sb.toString();
	}

}
