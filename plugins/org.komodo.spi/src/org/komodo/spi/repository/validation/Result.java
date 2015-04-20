/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository.validation;

import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.KomodoObject;

/**
 * A result of evaluating a {@link Rule validation rule}.
 */
public interface Result extends Outcome {

    /**
     * @return the path of the {@link KomodoObject} that was validated (never <code>null</code>)
     */
    String getPath();

    /**
     * @return the identifier of the rule whose evaluation produced this result (never <code>null</code>)
     */
    String getRuleId();

    /**
     * @return the date and time this result was created
     */
    long getTimestamp();

}
