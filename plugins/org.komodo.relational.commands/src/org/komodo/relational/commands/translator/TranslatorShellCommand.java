/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.translator;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Translator;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for {@link Translator translator}-related shell commands.
 */
abstract class TranslatorShellCommand extends RelationalShellCommand {

    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String TYPE = "type"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, TYPE } );

    protected TranslatorShellCommand( final String name,
                                      final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Translator getTranslator() throws Exception {
        assert getContext() instanceof Translator;
        return Translator.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Translator.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
