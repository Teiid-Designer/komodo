/*
 * Copyright 2013 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell;

import java.io.File;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.ui.DefaultLabelProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * Abstract base class for all built-in shell commands.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use different Messages class
 *
 */
public abstract class BuiltInShellCommand implements ShellCommand, StringConstants {

    private final String name;
    private WorkspaceStatus wsStatus;
    private Arguments arguments;
    private Writer writer;

    //private final StringNameValidator nameValidator = new StringNameValidator();
    private final String[] aliases;

    /**
     * Constructs a command.
     *
     * @param workspaceStatus
     *        the workspace status (cannot be <code>null</code>)
     * @param names
     *        the command name and then any aliases (cannot be <code>null</code>, empty, or have a <code>null</code> first
     *        element)
     */
    public BuiltInShellCommand(final WorkspaceStatus workspaceStatus,
                               final String... names ) {

        ArgCheck.isNotEmpty( names, "names" ); //$NON-NLS-1$
        ArgCheck.isNotNull( workspaceStatus, "workspaceStatus" ); //$NON-NLS-1$
        this.name = names[0];
        this.wsStatus = workspaceStatus;
        this.arguments = new Arguments();

        // save aliases if necessary
        if ( names.length == 1 ) {
            this.aliases = StringConstants.EMPTY_ARRAY;
        } else {
            this.aliases = new String[ names.length - 1 ];
            boolean firstTime = true;
            int i = 0;

            for ( final String alias : names ) {
                if (firstTime) {
                    firstTime = false;
                    continue;
                }

                this.aliases[i++] = alias;
            }
        }
    }

    /**
     * @return the command result (cannot be <code>null</code>)
     */
    protected abstract CommandResult doExecute();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public final CommandResult execute() {
        // make sure there aren't too many args
        if ( ( getMaxArgCount() != -1 ) && ( getArguments().size() > getMaxArgCount() ) ) {
            return new CommandResultImpl( false, I18n.bind( ShellI18n.tooManyArgs, this ), null );
        }

        // Make sure command is valid for the context
        if ( !isValidForCurrentContext() ) {
            return new CommandResultImpl( false, I18n.bind( ShellI18n.invalidCommandForContext ), null );
        }

        // execute command
        final CommandResult result = doExecute();

        // return result
        assert (result != null);
        return result;
    }

    /**
     * @return the maximum number of arguments allowed or -1 if there is no maximum
     */
    protected abstract int getMaxArgCount();

    /**
     * @see org.komodo.shell.api.ShellCommand#getName()
     */
    @Override
    public final String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#getAliases()
     */
    @Override
    public final String[] getAliases() {
        return this.aliases;
    }

    protected UnitOfWork getTransaction() {
        return getWorkspaceStatus().getTransaction();
    }

    @Override
    public WorkspaceStatus getWorkspaceStatus() {
        return this.wsStatus;
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#setArguments(Arguments)
     */
    @Override
    public void setArguments( final Arguments newArguments ) {
        this.arguments = ( ( newArguments == null ) ? new Arguments() : newArguments );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#getArguments()
     */
    @Override
    public Arguments getArguments() {
        return this.arguments;
    }

    /**
     * Returns the argument at the given index.  Throws an exception if the argument
     * does not exist.
     * @param argIndex
     * @param message
     * @throws InvalidCommandArgumentException
     */
    protected String requiredArgument(int argIndex, String message) throws InvalidCommandArgumentException {
        if (getArguments().size() <= argIndex) {
            throw new InvalidCommandArgumentException(argIndex, message);
        }
        return getArguments().get(argIndex);
    }

    /**
     * Returns the optional argument at the given index.  Returns null if the argument
     * does not exist.
     * @param argIndex
     */
    protected String optionalArgument(int argIndex) {
        return optionalArgument(argIndex, null);
    }

    /**
     * Returns the optional argument at the given index.  Returns the given default value if
     * the argument does not exist.
     * @param argIndex
     * @param defaultValue
     */
    protected String optionalArgument(int argIndex, String defaultValue) {
        if (getArguments().size() <= argIndex) {
            return defaultValue;
        }
        return getArguments().get(argIndex);
    }

    /**
     * @param startIndex
     *        the argument index to start the processing
     * @return an array of the arguments starting at the specified index (never <code>null</code> but can be empty)
     */
    protected String[] processOptionalArguments( final int startIndex ) {
        final int numArgs = getArguments().size();

        if ( ( numArgs == 0 ) || startIndex >= numArgs ) {
            return EMPTY_ARRAY;
        }

        final List< String > optionalArgs = new ArrayList< >();
        int i = startIndex;

        while ( optionalArgument( i ) != null ) {
            optionalArgs.add( optionalArgument( i++ ) );
        }

        return optionalArgs.toArray( new String[ optionalArgs.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#setWriter(java.io.Writer)
     */
    @Override
    public void setWriter(Writer output) {
        this.writer = output;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#getWriter()
     */
    @Override
    public Writer getWriter() {
        return this.writer;
    }

	protected KomodoObject getContext() {
	    return getWorkspaceStatus().getCurrentContext();
	}

	protected boolean isShowingPropertyNamePrefixes() {
	    return getWorkspaceStatus().isShowingPropertyNamePrefixes();
	}

	/**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.helpUsageHeading ) );
        printHelpUsage( 2 * indent );
    }

    private void printAliases( final int indent ) {
        final int twoIndents = ( 2 * indent );
        final String[] aliases = getAliases();

        if ( aliases.length == 0 ) {
            print( twoIndents, I18n.bind( ShellI18n.helpNoAliases ) );
        } else {
            final StringBuilder builder = new StringBuilder();
            boolean firstTime = true;

            for ( final String alias : aliases ) {
                if ( firstTime ) {
                    firstTime = false;
                } else {
                    builder.append( ", " ); //$NON-NLS-1$
                }

                builder.append( alias );
            }

            print( twoIndents, builder.toString() );
        }
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        // description
        print( indent, I18n.bind( ShellI18n.helpDescriptionHeading ) );
        printHelpDescription( indent );
        print();

        // aliases
        print( indent, I18n.bind( ShellI18n.helpAliasesHeading ) );
        printAliases( indent );
        print();

        // usage
        printUsage( indent );
        print();

        // examples
        print( indent, I18n.bind( ShellI18n.helpExamplesHeading ) );
        printHelpExamples( indent );
    }

    protected abstract void printHelpDescription( final int indent );

    protected abstract void printHelpExamples( final int indent );

    protected abstract void printHelpUsage( final int indent );

    protected void print() {
        print( 0, StringConstants.EMPTY_STRING );
    }

    protected void print( final int indent,
                          final String formattedMessage,
                          final Object... params ) {
        PrintUtils.print( getWriter(), indent, formattedMessage, params );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final Arguments args = getArguments();
        final StringBuffer buff = new StringBuffer( getName() );

        for ( int i = 0, size = args.size(); i < size; ++i ) {
            buff.append(' ').append(args.get( i ) );
        }

        return buff.toString();
    }

	/**
	 * Validates that the supplied path argument is a readable file.
	 * @param filePathArg the path to test
	 * @return "OK" if the path is valid readable file, other error message if not
	 */
	public String validateReadableFileArg(String filePathArg) {
		String filePath = filePathArg.trim();

        // Check the fileName arg validity
        File theFile = new File(filePath);
        if(!theFile.exists()) {
            return I18n.bind(ShellI18n.fileNotFound, filePath);
        } else if(!theFile.isFile()) {
            return I18n.bind(ShellI18n.fileArgNotAFile, filePath);
        } else if(!theFile.canRead()) {
            return I18n.bind(ShellI18n.fileCannotRead, filePath);
        }

		return CompletionConstants.OK;
	}

	/**
	 * Validates whether the supplied path is valid.  If the path is relative this takes into account the
	 * current context.  If valid 'Ok' is returned, otherwise the appropriate error message.
	 * @param displayPath the path to test, never null.
	 * @return result message - "ok" if the path is valid, "error message" if not.
	 */
	public String validatePath(String displayPath) {
        ArgCheck.isNotNull(displayPath, "displayPath"); //$NON-NLS-1$

        displayPath = displayPath.trim();
		if(displayPath.trim().length()==0) {
		    return I18n.bind(ShellI18n.locationArgEmpty);
		}

		// If supplied path doesnt start with FORWARD_SLASH, it should be relative to current context
		String entireDisplayPath = displayPath;
		if(!displayPath.startsWith(FORWARD_SLASH)) {
		    if(KomodoObjectUtils.isRoot(getContext())) {
		        entireDisplayPath = FORWARD_SLASH + displayPath;
		    } else {
		        entireDisplayPath = getWorkspaceStatus().getCurrentContextDisplayPath( null )+FORWARD_SLASH+displayPath;
		    }
		}

		// Try to locate the object at the specified path
        KomodoObject newContext = null;
        String repoPath = getWorkspaceStatus().getCurrentContextLabelProvider().getPath(getTransaction(), entireDisplayPath);
        if(!StringUtils.isBlank(repoPath)) {
            // TODO: probably need to add getFromLibrary method similar to getFromWorkspace instead of the below...
            // /tko:komodo/library
            if ( DefaultLabelProvider.LIB_PATH.equals( repoPath ) || DefaultLabelProvider.LIB_SLASH_PATH.equals( repoPath ) ) {
                return CompletionConstants.OK;
            }

            // /tko:komodo/environment
            if ( DefaultLabelProvider.ENV_PATH.equals( repoPath ) || DefaultLabelProvider.ENV_SLASH_PATH.equals( repoPath ) ) {
                return CompletionConstants.OK;
            }

            //
            try {
                newContext = wsStatus.getRootContext().getRepository().getFromWorkspace(getTransaction(), repoPath);
            } catch (KException ex) {
                return I18n.bind(ShellI18n.locationArgNoContextWithThisName, displayPath);
            }
        }

		if(newContext==null) {
		    return I18n.bind(ShellI18n.locationArgNoContextWithThisName, displayPath);
		}
		return CompletionConstants.OK;
	}

	/**
	 * Validate whether the supplied propName is valid for the supplied context.  If invalid, a message is printed out.
	 * @param context the context
	 * @param propName the property name
	 * @return 'true' if valid, 'false' if not.
	 * @throws Exception exception if problem getting the value.
	 */
//	public boolean validatePropertyName(KomodoObject context, String propName) throws Exception {
//		final boolean exists = KomodoObjectUtils.getProperties(getWorkspaceStatus(),context).contains( propName );
//
//        if ( !exists ) {
//            print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.propertyArg_noPropertyWithThisName, propName));
//			return false;
//		}
//		return true;
//	}

    /**
     * Updates the candidates for tab completion, given the currentContext and path
     * @param candidates the candidates list
     * @param currentContext the current context
     * @param includeGoUp if 'true' the '..' option is included
     * @param lastArgument the last arg
     * @throws Exception the exception
     */
    public void updateTabCompleteCandidatesForPath(List<CharSequence> candidates, KomodoObject currentContext, boolean includeGoUp, String lastArgument) throws Exception {
    	// List of potentials completions
    	List<String> potentialsList = new ArrayList<String>();
    	// Only offer '..' if below the root
        if ( ( currentContext.getParent( getTransaction() ) != null ) && includeGoUp ) {
    		potentialsList.add(StringConstants.DOT_DOT);
    	}

    	// --------------------------------------------------------------
    	// No arg - offer children relative current context.
    	// --------------------------------------------------------------
    	if(lastArgument==null) {
    	    KomodoObject[] children = currentContext.getChildren( getTransaction() );
    		for(KomodoObject wsContext : children) {
                final String contextName = this.wsStatus.getCurrentContextLabelProvider().getDisplayName( getTransaction(),
                                                                                                          wsContext,
                                                                                                          null );
    			potentialsList.add(contextName+FORWARD_SLASH);
    		}
    		candidates.addAll(potentialsList);
    		// --------------------------------------------------------------
    		// One arg - determine the completion options for it.
    		// --------------------------------------------------------------
    	} else {
    		// --------------------------------------------
    		// Absolute Path Arg handling
    		// --------------------------------------------
    		if( lastArgument.startsWith(FORWARD_SLASH) ) {
    		    String relativePath = lastArgument.substring(FORWARD_SLASH.length());
                ContextPathPair contextPathPair = getMatchingContextAndPathRelative(getWorkspaceStatus(), getWorkspaceStatus().getRootContext(), relativePath);
                KomodoObject deepestMatchingContext = contextPathPair.getContext();

    		    // Get children of deepest context match to form potentialsList
    		    KomodoObject[] children = deepestMatchingContext.getChildren(getTransaction());
    		    if(children.length != 0) {
    		        // Get all children as potentials
    		        for(KomodoObject childContext : children) {
    		            final String absolutePath = this.wsStatus.getDisplayPath(childContext, null);
    		            potentialsList.add(absolutePath+FORWARD_SLASH);
    		        }
    		    } else {
    		        final String absolutePath = this.wsStatus.getDisplayPath(deepestMatchingContext, null);
    		        potentialsList.add(absolutePath+FORWARD_SLASH);
    		    }
    		    updateCandidates(candidates, potentialsList, lastArgument);
    		// -------------------------------------------
    		// Relative Path Arg handling
    		// -------------------------------------------
    		} else {
    			// Deepest matching context for relative path
    		    ContextPathPair contextPathPair = getMatchingContextAndPathRelative(getWorkspaceStatus(), currentContext, lastArgument);
    		    KomodoObject deepestMatchingContext = contextPathPair.getContext();
    		    String deepestMatchingPath = contextPathPair.getPath();

    			// Get children of deepest context match to form potentialsList
    		    KomodoObject[] children = deepestMatchingContext.getChildren( getTransaction() );
    			if(children.length!=0) {
    				// Get all children as potentials
    				for(KomodoObject childContext : children) {
                        final String absolutePath = this.wsStatus.getDisplayPath(childContext, null);
    					String relativePath = convertAbsoluteDisplayPathToRelative(deepestMatchingContext, absolutePath);
    					if(!StringUtils.isBlank(deepestMatchingPath) && lastArgument.startsWith(deepestMatchingPath)) {
    					    potentialsList.add(deepestMatchingPath+FORWARD_SLASH+relativePath+FORWARD_SLASH);
    					} else {
    					    potentialsList.add(relativePath+FORWARD_SLASH);
    					}
    				}
    			} else {
                    final String absolutePath = this.wsStatus.getDisplayPath(deepestMatchingContext, null);
    				String relativePath = convertAbsoluteDisplayPathToRelative(deepestMatchingContext, absolutePath);
                    if(!StringUtils.isBlank(deepestMatchingPath) && lastArgument.startsWith(deepestMatchingPath)) {
                        potentialsList.add(deepestMatchingPath+FORWARD_SLASH+relativePath+FORWARD_SLASH);
                    } else {
                        potentialsList.add(relativePath+FORWARD_SLASH);
                    }
    			}
    			updateCandidates(candidates, potentialsList, lastArgument);
    		}

    	}
    }

    /**
     * Get the deepest matching context along the supplied relative path.  If there is no such context, the current context is returned.
     * @param wsStatus the WorkspaceStatus
     * @param currentContext the current context
     * @param relativeDisplayPath the path relative to current context
     * @return the context at the deepest matching segment in the specified relative path, if none match - current context is returned.
     * @throws KException the exception
     */
    protected ContextPathPair getMatchingContextAndPathRelative(WorkspaceStatus wsStatus, KomodoObject currentContext, String relativeDisplayPath) throws KException {
        ContextPathPair result = new ContextPathPair();

        String currentPath = EMPTY_STRING;
        if(!StringUtils.isEmpty(relativeDisplayPath)) {
            String[] segments = relativeDisplayPath.split(FORWARD_SLASH);
            int nSegments = segments.length;
            for(int i=0; i<nSegments; i++) {
                String segment = segments[i];
                currentPath = currentPath + segment;
                if(hasMultipleChildrenStartingWith(wsStatus,currentContext,segment)) {
                    result.setContext(currentContext);
                    int segLength = segment.length();
                    boolean hasFwdSlash = currentPath.contains(FORWARD_SLASH);
                    String prevPath = hasFwdSlash ? currentPath.substring(0, currentPath.length()-segLength-1) : currentPath.substring(0, currentPath.length()-segLength);
                    result.setPath(prevPath);
                    break;
                } else {
                    String currContextPath = wsStatus.getCurrentContextLabelProvider().getDisplayPath( getTransaction(),
                                                                                                       currentContext,
                                                                                                       null );
                    String displayPath = currContextPath + FORWARD_SLASH + segment;
                    KomodoObject theContext = wsStatus.getContextForDisplayPath(displayPath);

                    if(theContext==null) {
                        result.setContext(currentContext);
                        int segLength = segment.length();
                        boolean hasFwdSlash = currentPath.contains(FORWARD_SLASH);
                        String prevPath = hasFwdSlash ? currentPath.substring(0, currentPath.length()-segLength-1) : currentPath.substring(0, currentPath.length()-segLength);
                        result.setPath(prevPath);
                        break;
                    } else {
                        currentContext = theContext;
                        result.setContext(currentContext);
                        result.setPath(currentPath);
                        currentPath = currentPath + FORWARD_SLASH;  // Add separator prior to next iteration
                    }
                }
            }
        } else {
            result.setContext(currentContext);
            result.setPath(EMPTY_STRING);
        }
        return result;
    }

    private class ContextPathPair {
        private KomodoObject context;
        private String path;

        public void setContext(KomodoObject context) {
            this.context = context;
        }
        public void setPath(String path) {
            this.path = path;
        }
        public KomodoObject getContext() {
            return this.context;
        }
        public String getPath() {

            return this.path;
        }
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
                final String contextName = wsStatus.getCurrentContextLabelProvider().getDisplayName( wsStatus.getTransaction(),
                                                                                                     theContext,
                                                                                                     null );
                if(contextName.startsWith(segmentName)) {
                    nMatching++;
                    if(nMatching>1) break;
                }
            }
        } catch (Exception e) {
            // Handle exception
        }
        return nMatching>1;
    }

    /**
     * convert the supplied absolute path to a path relative to the supplied context
     * @param wsStatus the WorkspaceContext
     * @param context the context
     * @param absolutePath the supplied absolute path
     * @return the path relative to the root context
     */
    private String convertAbsoluteDisplayPathToRelative( KomodoObject context,
                                                         final String absolutePath ) {
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( absolutePath, "absolutePath" ); //$NON-NLS-1$

        final String absContextPath = wsStatus.getDisplayPath(context, null);
        final String displayPath = wsStatus.getCurrentContextLabelProvider().getDisplayPath( getTransaction(), absolutePath, null );
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
     * Updates the candidates for tab completion, given the context and property Arg
     * @param candidates the candidates list
     * @param context the context
     * @param propArg the propName for completion
     * @throws Exception the exception
     */
    public void updateTabCompleteCandidatesForProperty(List<CharSequence> candidates, KomodoObject context, String propArg) throws Exception {
		// List of potentials completions
		List<String> potentials = null;

		// Context properties
		final List<String> propNames = KomodoObjectUtils.getProperties(getWorkspaceStatus(),context);  // All properties

        if ( isShowingPropertyNamePrefixes() ) {
            potentials = propNames;
        } else {
            potentials = new ArrayList<>( propNames.size() );

            // strip off prefix
            for ( final String name : propNames ) {
                potentials.add( KomodoObjectUtils.removePrefix( name ) );
            }
        }

        Collections.sort( potentials );

        if ( StringUtils.isEmpty( propArg ) ) {
            candidates.addAll( potentials );
        } else {
            updateCandidates( candidates, potentials, propArg );
        }
    }

    protected void updateCandidatesForBooleanProperty( final String lastArgument,
                                                       final List< CharSequence > candidates ) {
        if ( StringUtils.isBlank( lastArgument ) || KomodoObjectUtils.TRUE_STRING.startsWith( lastArgument ) ) {
            candidates.add( Boolean.TRUE.toString() );
        }

        if ( StringUtils.isBlank( lastArgument ) || KomodoObjectUtils.FALSE_STRING.startsWith( lastArgument ) ) {
            candidates.add( Boolean.FALSE.toString() );
        }
    }

    /**
     * Adds the valid items from the completionList to the candidates.  They are added to the candidates if they start
     * with 'lastArg'
     * @param candidates the candidates
     * @param completionList possibilities before filtering based on last arg
     * @param lastArg the commandline arg
     */
    private void updateCandidates(List<CharSequence> candidates, List<String> completionList, String lastArg) {
        updateCandidates( candidates, completionList, lastArg, true );
    }

    private void updateCandidates( final List< CharSequence > candidates,
                                   final List< String > completionList,
                                   final String lastArg,
                                   final boolean caseSensitive ) {
        for ( final String item : completionList ) {
            if (caseSensitive) {
                if ( item.startsWith( lastArg )) {
                    candidates.add( item );
                }
            } else if ( item.toUpperCase().startsWith( lastArg.toUpperCase() ) ) {
                candidates.add( item );
            }
        }
    }

}
