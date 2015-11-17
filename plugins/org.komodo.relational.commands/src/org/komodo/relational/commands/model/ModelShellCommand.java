/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Model model}-related shell commands.
 */
abstract class ModelShellCommand extends RelationalShellCommand {

    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String METADATA_TYPE = "metadataType"; //$NON-NLS-1$
    protected static final String MODEL_TYPE = "modelType"; //$NON-NLS-1$
    protected static final String VISIBLE = "visible"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, METADATA_TYPE, MODEL_TYPE,
                                                                                    VISIBLE } );

    protected ModelShellCommand( final String name,
                                 final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Model getModel() throws Exception {
        assert getContext() instanceof Model;
        return Model.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Model.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

}
