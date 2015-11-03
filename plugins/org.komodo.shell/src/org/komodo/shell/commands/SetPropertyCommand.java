/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import java.util.List;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;

/**
 * SetPropertyCommand - sets property value on a KomodoObject.
 */
public class SetPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-property"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // Not valid in root, workspace, library or environment
        if( KomodoObjectUtils.isRoot(getContext()) || KomodoObjectUtils.isRootChild(getContext()) ) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            // property name and value are required
            String propNameArg = requiredArgument(0, Messages.getString(Messages.SHELL.InvalidArgMsg_PropertyName));
            String propValueArg = requiredArgument(1, Messages.getString(Messages.SHELL.InvalidArgMsg_PropertyValue));
            // path is optional.  if path is not included, current context is assumed.
            String pathArg = optionalArgument(2);

            // Validate the location Path if supplied
            if(!StringUtils.isEmpty(pathArg)) {
                String validationMsg = validatePath(pathArg);
                if(!validationMsg.equals(CompletionConstants.OK)) {
                    return new CommandResultImpl(false, validationMsg, null);
                }
            }

            // Get the context for object.  otherwise use current context
            final KomodoObject context = StringUtils.isEmpty( pathArg ) ? getContext() : getWorkspaceStatus().getContextForDisplayPath(pathArg);

            // Validate the type is valid for the context
            if (!validateProperty(propNameArg,context)) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.SetPropertyCommand.InvalidPropName, propNameArg ),
                                              null );
            }

            // Validate the property value
            if (!validatePropertyValue(propNameArg,propValueArg,context)) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.SetPropertyCommand.InvalidPropValue, propValueArg ),
                                              null );
            }

            // Set the property
            setProperty(context,propNameArg, propValueArg);
            return new CommandResultImpl( Messages.getString( Messages.SetPropertyCommand.PropertySet, propNameArg ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 3;
    }

    @Override
    public boolean validatePropertyValue( final String propName,
                                          final String propValue,
                                          final KomodoObject context ) {
        if ( !StringUtils.isBlank( propValue ) ) {
            try {
                // make sure multi-valued property value parses
                if ( isMultiValuedProperty( context, propName ) ) {
                    final String[] multiValues = parseMultiValues( propValue );
                    return ( multiValues.length != 0 );
                }
            } catch ( final Exception e ) {
                return false;
            }
        }

        // let super validate
        return super.validatePropertyValue( propName, propValue, context );
    }

    private void setProperty( final KomodoObject context,
                              final String name,
                              final String propValue ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );

        if ( !StringUtils.isBlank( propValue ) && isMultiValuedProperty( context, propertyName ) ) {
            final String[] values = parseMultiValues( propValue );
            context.setProperty( getTransaction(), propertyName, ( Object[] )values );
        } else {
            context.setProperty( getTransaction(), propertyName, propValue );
        }
    }

    private boolean isMultiValuedProperty( final KomodoObject context,
                                           final String name ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );
        final PropertyDescriptor descriptor = context.getPropertyDescriptor( getTransaction(),
                                                                                            propertyName );
        return ( ( descriptor == null ) ? false : descriptor.isMultiple() );
    }

    private String concatMultiValues( final KomodoObject context,
                                      final String name ) throws Exception {
        // TODO need to account for escaped values
        assert isMultiValuedProperty( context, name );

        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );
        final Property property = context.getProperty( getTransaction(), propertyName );
        final Repository.UnitOfWork uow = getTransaction();
        final StringBuilder result = new StringBuilder();
        boolean quoted = false;
        boolean firstTime = true;

        for ( final Object value : property.getValues( getTransaction() ) ) {
            if ( !firstTime ) {
                result.append( ',' );
            } else {
                firstTime = false;
            }

            final Type type = property.getDescriptor( uow ).getType();
            final boolean propIsReference = ( ( Type.REFERENCE == type ) || ( Type.WEAKREFERENCE == type ) );
            String valueAsText = null;

            if ( propIsReference ) {
                final String path = RepositoryTools.findPathOfReference( uow, property.getRepository(), value.toString() );
                valueAsText = ( StringUtils.isBlank( path ) ? value.toString()
                                                            : getWorkspaceStatus().getLabelProvider().getDisplayPath( path ) );
            } else {
                valueAsText = value.toString();

                if ( ( valueAsText.indexOf( ' ' ) != -1 ) && !quoted ) {
                    quoted = true;
                    result.insert( 0, '"' );
                }
            }

            result.append( valueAsText );
        }

        if ( quoted ) {
            result.append( '"' );
        }

        return result.toString();
    }

    private String[] parseMultiValues( final String valuesString ) {
        // TODO need to account for escaped values
        assert !StringUtils.isBlank( valuesString );
        String multiValues = null;

        // strip off leading and trailing quotes if necessary
        if ( valuesString.startsWith( "\"" ) && valuesString.endsWith( "\"" ) ) { //$NON-NLS-1$ //$NON-NLS-2$
            if ( valuesString.length() == 2 ) {
                return StringConstants.EMPTY_ARRAY;
            }

            multiValues = valuesString.substring( 1, valuesString.length() - 1 );
        } else {
            multiValues = valuesString;
        }

        return multiValues.split( "," ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            updateTabCompleteCandidatesForProperty( candidates, getContext(), lastArgument );

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        } else if (getArguments().size() == 1) {
            final KomodoObject context = getContext();
            String propArg = getArguments().get(0);
            final String propName = isShowingPropertyNamePrefixes() ? propArg : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, propArg );

            if ( isMultiValuedProperty( context, propName ) ) {
                if ( context.hasProperty( getTransaction(), propName ) ) {
                    // concat current multi-values as a string
                    final String value = concatMultiValues( context, propName );
                    candidates.add( value );
                }
            } else {
                provideCandidates( context, propName, lastArgument, candidates );
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        return -1;
    }

    private void provideCandidates( final KomodoObject context,
                                    final String name,
                                    final String lastArgument,
                                    final List< CharSequence > candidates ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );
        final PropertyDescriptor descriptor = context.getPropertyDescriptor( getTransaction(), propertyName );

        if ( descriptor == null ) {
            return;
        }

        String[] possibleValues = StringConstants.EMPTY_ARRAY;

        if ( Type.BOOLEAN == descriptor.getType() ) {
            possibleValues = new String[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() };
        } else {
            final Object[] defaultValues = descriptor.getDefaultValues();

            if ( defaultValues.length != 0 ) {
                possibleValues = new String[ defaultValues.length ];
                int i = 0;

                for ( final Object defaultValue : defaultValues ) {
                    possibleValues[ i++ ] = defaultValue.toString();
                }
            }
        }

        if ( possibleValues.length != 0 ) {
            final boolean hasLastArgument = !StringUtils.isBlank( lastArgument );

            for ( final String value : possibleValues ) {
                if ( ( hasLastArgument && value.startsWith( lastArgument ) ) || !hasLastArgument ) {
                    candidates.add( value );
                }
            }
        }
    }

}
