/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import java.io.InputStream;
import org.komodo.spi.repository.validation.ValidationRulesProvider;


/**
 * A validation rules provider for relational objects.
 */
public class RelationalValidationRulesProvider implements ValidationRulesProvider {

    private static final String VALIDATION_RULES_FILE_NAME = "relationalValidationRulesDefault.xml"; //$NON-NLS-1$

    /**
     * Constructs a rules provider for relational objects.
     */
    public RelationalValidationRulesProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.ValidationRulesProvider#provideRules()
     */
    @Override
    public InputStream provideRules() throws Exception {
        return getClass().getClassLoader().getResourceAsStream( VALIDATION_RULES_FILE_NAME );
    }

}
