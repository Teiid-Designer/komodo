/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository.validation;

import java.io.InputStream;
import org.komodo.spi.repository.ValidationManager;

/**
 * A provider of {@link Rule validation rules}.
 */
public interface ValidationRulesProvider {

    /**
     * The provided stream will be closed by the {@link ValidationManager validation manager}.
     *
     * @return the stream of the XML file containing the rule definitions (cannot be <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    InputStream provideRules() throws Exception;

}
