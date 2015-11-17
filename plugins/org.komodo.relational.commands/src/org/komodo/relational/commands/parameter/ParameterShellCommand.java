/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.parameter;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Parameter;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Parameter parameter}-related shell commands.
 */
abstract class ParameterShellCommand extends RelationalShellCommand {

    protected static final String DATATYPE_NAME = "datatypeName"; //$NON-NLS-1$
    protected static final String DEFAULT_VALUE = "defaultValue"; //$NON-NLS-1$
    protected static final String DIRECTION = "parameterType"; //$NON-NLS-1$
    protected static final String LENGTH = "datatypeLength"; //$NON-NLS-1$
    protected static final String NULLABLE = "nullable"; //$NON-NLS-1$
    protected static final String PRECISION = "datatypePrecision"; //$NON-NLS-1$
    protected static final String RESULT = "result"; //$NON-NLS-1$
    protected static final String SCALE = "datatypeScale"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DATATYPE_NAME, DEFAULT_VALUE, DIRECTION,
                                                                                    LENGTH, NULLABLE, PRECISION, RESULT,
                                                                                    SCALE } );

    protected ParameterShellCommand( final String name,
                                     final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Parameter getParameter() throws Exception {
        assert getContext() instanceof Parameter;
        return Parameter.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Parameter.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
